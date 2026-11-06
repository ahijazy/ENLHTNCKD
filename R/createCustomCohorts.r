# CRITICAL SAFETY CHECK: Prevent deleting source cohorts
# carefull here, this deletes elements in the table with if they have the same 
# cohort definition id as prevalentCohortId


# ==============================================================================
# Merge multiple cohorts into a single new cohort
# For each subject, cohort_start_date = MIN(start dates across the given cohorts)
#                    cohort_end_date   = MAX(end dates across the given cohorts)
# ==============================================================================

mergeCohorts <- function(connection,
                         cohortDatabaseSchema,
                         cohortTable,
                         cohortIds,
                         newCohortId) 
{
  dbms <- DatabaseConnector::dbms(connection)
  if (length(cohortIds) < 2) 
  {
    stop("cohortIds must contain at least 2 cohort ids to merge.")
  }
  # 1) Remove any existing rows for the new cohort id (idempotent re-run)
  deleteSql <- "
DELETE FROM @cohort_database_schema.@cohort_table
WHERE cohort_definition_id = @new_cohort_id;
"
 
  renderedDelete <- SqlRender::render(
    deleteSql,
    cohort_database_schema = cohortDatabaseSchema,
    cohort_table = cohortTable,
    new_cohort_id = as.integer(newCohortId)
  )
  translatedDelete <- SqlRender::translate(renderedDelete, targetDialect = dbms)
  ParallelLogger::logInfo("Removing existing rows for new_cohort_id = ", newCohortId)
  DatabaseConnector::executeSql(connection, translatedDelete)
 
  # 2) Insert merged rows: earliest start date, latest end date per subject
  insertSql <- "

INSERT INTO @cohort_database_schema.@cohort_table
  (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
SELECT
  @new_cohort_id AS cohort_definition_id,
  subject_id,
  MIN(cohort_start_date) AS cohort_start_date,
  MAX(cohort_end_date)   AS cohort_end_date
FROM @cohort_database_schema.@cohort_table
WHERE cohort_definition_id IN (@cohort_ids)
GROUP BY subject_id;"
 
  renderedInsert <- SqlRender::render(
    insertSql,
    cohort_database_schema = cohortDatabaseSchema,
    cohort_table = cohortTable,
    new_cohort_id = as.integer(newCohortId),
    cohort_ids = as.integer(cohortIds)
  )
  translatedInsert <- SqlRender::translate(renderedInsert, targetDialect = dbms)
  ParallelLogger::logInfo(
    "Merging cohorts [", paste(cohortIds, collapse = ", "), "] into new_cohort_id = ", newCohortId
  )
  DatabaseConnector::executeSql(connection, translatedInsert)
  invisible(newCohortId)                                         
}

# ==============================================================================
# Add incident population to a new cohort based on two source cohorts (CKD and HTN)
# ==============================================================================

 addIncidentPopulationtoCohortTable <- function(connection, 
                                               cohortDatabaseSchema,
                                               cohortTable,
                                               incidentCohortId,
                                               CKDCohortId,
                                               HTNCohortId,
                                               indexDate) 
{
  dbms <- DatabaseConnector::dbms(connection)
  sql_incident_population <- "
  DELETE FROM @cohort_database_schema.@cohort_table
  WHERE cohort_definition_id = @incident_cohort_id;
  
  WITH CKD_patients AS (
      SELECT 
          subject_id,
          cohort_start_date AS CKD_start_date,
          cohort_end_date AS CKD_end_date
      FROM @cohort_database_schema.@cohort_table
      WHERE cohort_definition_id = @ckd_cohort_id
  ),
  HTN_patients AS (
      SELECT 
          subject_id,
          cohort_start_date AS HTN_start_date,
          cohort_end_date AS HTN_end_date
      FROM @cohort_database_schema.@cohort_table
      WHERE cohort_definition_id = @htn_cohort_id
  ),
  Comorbid_patients AS (
      SELECT 
          c.subject_id,
          -- The first date the patient has both (MAX of the two start dates)
          CASE 
              WHEN c.CKD_start_date > h.HTN_start_date THEN c.CKD_start_date 
              ELSE h.HTN_start_date 
          END AS cohort_start_date,
          -- The cohort end date (MAX of the two end dates)
          CASE 
              WHEN c.CKD_end_date > h.HTN_end_date THEN c.CKD_end_date 
              ELSE h.HTN_end_date 
          END AS cohort_end_date
      FROM CKD_patients c
      INNER JOIN HTN_patients h ON c.subject_id = h.subject_id
  )
  INSERT INTO @cohort_database_schema.@cohort_table (
      cohort_definition_id,
      subject_id,
      cohort_start_date,
      cohort_end_date
  )
  SELECT 
      @incident_cohort_id AS cohort_definition_id,
      subject_id, 
      cohort_start_date,
      cohort_end_date
  FROM Comorbid_patients
  -- Filter: The patient achieved both conditions ON or AFTER the specified index date
  WHERE cohort_start_date >= CAST('@index_date' AS DATE);
  "

  # 1. Swap R variables into the SQL template placeholders
  renderedSql <- SqlRender::render(sql = sql_incident_population, 
                                   cohort_database_schema = cohortDatabaseSchema,
                                   cohort_table = cohortTable,
                                   ckd_cohort_id = CKDCohortId,
                                   htn_cohort_id = HTNCohortId,
                                   incident_cohort_id = incidentCohortId,
                                   index_date = indexDate)
  
  # 2. Universal Dialect Translation (reads database metadata to detect connection type)
  translatedSql <- SqlRender::translate(renderedSql, targetDialect =  dbms)
  ParallelLogger::logInfo("Populating cohort table with Incident Cohort ", incidentCohortId)
  # 3. Execute translated SQL query
  DatabaseConnector::executeSql(connection, translatedSql)

  ParallelLogger::logInfo("Successfully populated cohort table for Incident Cohort: ", incidentCohortId)
}

# ==============================================================================
# Add prevalent population to a new cohort based on two source cohorts (CKD and HTN)
# ==============================================================================
addPrevalentPopulationtoCohortTable <- function(connection, 
                                                cohortDatabaseSchema,
                                                cohortTable,
                                                prevalentCohortId,
                                                CKDCohortId,
                                                HTNCohortId,
                                                indexDate) 
{dbms <- DatabaseConnector::dbms(connection)
  # Safety check
  if (prevalentCohortId == CKDCohortId || prevalentCohortId == HTNCohortId) {
    stop(paste0(
      "CRITICAL ERROR: 'prevalentCohortId' (", prevalentCohortId, ") ",
      "cannot match source 'CKDCohortId' or 'HTNCohortId'."
    ))
  }

  sql_prevalent_population <- "
  DELETE FROM @cohort_database_schema.@cohort_table
  WHERE cohort_definition_id = @prevalent_cohort_id;
  
  WITH CKD_patients AS (
      SELECT 
          subject_id,
          cohort_start_date AS CKD_start_date,
          cohort_end_date AS CKD_end_date
      FROM @cohort_database_schema.@cohort_table
      WHERE cohort_definition_id = @ckd_cohort_id
  ),
  HTN_patients AS (
      SELECT 
          subject_id,
          cohort_start_date AS HTN_start_date,
          cohort_end_date AS HTN_end_date
      FROM @cohort_database_schema.@cohort_table
      WHERE cohort_definition_id = @htn_cohort_id
  ),
  Comorbid_patients AS (
      SELECT 
          c.subject_id,
          -- The first date the patient has both (MAX of the two start dates)
          CASE 
              WHEN c.CKD_start_date > h.HTN_start_date THEN c.CKD_start_date 
              ELSE h.HTN_start_date 
          END AS cohort_start_date,
          -- The cohort end date (MAX of the two end dates)
          CASE 
              WHEN c.CKD_end_date > h.HTN_end_date THEN c.CKD_end_date 
              ELSE h.HTN_end_date 
          END AS cohort_end_date
      FROM CKD_patients c
      INNER JOIN HTN_patients h ON c.subject_id = h.subject_id
  )
  INSERT INTO @cohort_database_schema.@cohort_table (
      cohort_definition_id,
      subject_id,
      cohort_start_date,
      cohort_end_date
  )
  SELECT 
      @prevalent_cohort_id AS cohort_definition_id,
      subject_id, 
      cohort_start_date,
      cohort_end_date
  FROM Comorbid_patients
  -- Filter: The patient achieved both conditions before the specified index date
  WHERE cohort_start_date < CAST('@index_date' AS DATE);
  "

  renderedSql <- SqlRender::render(sql = sql_prevalent_population, 
                                   cohort_database_schema = cohortDatabaseSchema,
                                   cohort_table = cohortTable,
                                   ckd_cohort_id = CKDCohortId,
                                   htn_cohort_id = HTNCohortId,
                                   prevalent_cohort_id = prevalentCohortId,
                                   index_date = indexDate)
  
  translatedSql <- SqlRender::translate(renderedSql, targetDialect = dbms)
  ParallelLogger::logInfo("Populating cohort table with Prevalent Cohort ", prevalentCohortId)

  DatabaseConnector::executeSql(connection, translatedSql)
  ParallelLogger::logInfo("Successfully populated cohort table for Prevalent Cohort: ", prevalentCohortId)
}

# ==============================================================================
# wrapper function to merge the CKD condition and measurement cohorts into a single new cohort,same for HTN
# then create the incident and prevalent cohorts based on the merged CKD and HTN cohorts
# ==============================================================================
CreateTargetPopulations=function(connection, 
                                 cohortDatabaseSchema,
                                 cohortTable) 
{
  # Merge CKD cohorts
  mergedCKDCohortId <- mergeCohorts(connection, 
                                    cohortDatabaseSchema, 
                                    cohortTable, 
                                    c(396,398), 
                                    newCohortId = 9001)  # Example new cohort ID for merged CKD
  
  # Merge HTN cohorts
  mergedHTNCohortId <- mergeCohorts(connection, 
                                    cohortDatabaseSchema, 
                                    cohortTable, 
                                    c(366, 403), 
                                    newCohortId = 9002)  # Example new cohort ID for merged HTN
  
  # Create Incident Cohort
  addIncidentPopulationtoCohortTable(connection, 
                                     cohortDatabaseSchema, 
                                     cohortTable, 
                                     incidentCohortId = 9003, 
                                     CKDCohortId = 9001, 
                                     HTNCohortId = 9002, 
                                     indexDate = '20180101')
  
  # Create Prevalent Cohort
  addPrevalentPopulationtoCohortTable(connection, 
                                      cohortDatabaseSchema, 
                                      cohortTable, 
                                      prevalentCohortId = 9004, 
                                      CKDCohortId = 9001, 
                                      HTNCohortId = 9002, 
                                      indexDate = '20230101')
}



# ==============================================================================
# intersectTwoCohorts
# Inserts a new cohort (newCohortId) containing every date interval in which a
# subject belongs to BOTH cohortIdA and cohortIdB at the same time.
#
# Overlap for each pair of episodes (a from A, b from B) of the same subject:
# start = GREATEST(a.start, b.start)
# end   = LEAST(a.end, b.end)
# Only pairs that actually overlap are inserted.
# ==============================================================================

intersectTwoCohorts <- function(connection,
                                cohortDatabaseSchema,
                                cohortTable = "cohort",
                                cohortIdA,
                                cohortIdB,
                                newCohortId) 
{
    deleteSql <- "
    DELETE FROM @cohort_database_schema.@cohort_table
    WHERE cohort_definition_id = @new_cohort_id;"

    insertSql <- "
    INSERT INTO @cohort_database_schema.@cohort_table
    (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
    SELECT
    @new_cohort_id,
    a.subject_id,
    GREATEST(a.cohort_start_date, b.cohort_start_date),
    LEAST(a.cohort_end_date, b.cohort_end_date)
    FROM @cohort_database_schema.@cohort_table a
    JOIN @cohort_database_schema.@cohort_table b
    ON a.subject_id = b.subject_id
    WHERE a.cohort_definition_id = @cohort_id_a
    AND b.cohort_definition_id = @cohort_id_b
    AND a.cohort_start_date <= b.cohort_end_date
    AND b.cohort_start_date <= a.cohort_end_date;"

    params <- list(
        cohort_database_schema = cohortDatabaseSchema,
        cohort_table          = cohortTable,
        new_cohort_id         = as.integer(newCohortId),
        cohort_id_a           = as.integer(cohortIdA),
        cohort_id_b           = as.integer(cohortIdB)
    )
    deleteRendered <- do.call(SqlRender::render, c(list(deleteSql), params))
    insertRendered <- do.call(SqlRender::render, c(list(insertSql), params))
    DatabaseConnector::executeSql(connection, SqlRender::translate(deleteRendered, targetDialect = dbms))
    DatabaseConnector::executeSql(connection, SqlRender::translate(insertRendered, targetDialect = dbms))
    ParallelLogger::logInfo("Intersected cohort ", cohortIdA, " with cohort ", cohortIdB,
                            " into new_cohort_id = ", newCohortId)
    invisible(newCohortId)
}
 
# ==============================================================================
# addFirstEventFlags
# ==============================================================================
# For a target cohort of comorbid patients (all have BOTH conditions A and B),
# determines which condition came first and creates:
#
# Baseline flags (one-hot, using SEPARATE synthetic cohort ids):
#   baseline_<aFirstBaselineId>   : 1 if condition A came first
#   baseline_<bFirstBaselineId>   : 1 if condition B came first
#   baseline_<sameDayBaselineId>  : 1 if both on same day
#

# Prior outcome columns (using the CONDITION cohort ids):
#   prior_outcome_<conditionACohortId> : 1 if A came first or same day
#   prior_outcome_<conditionBCohortId> : 1 if B came first or same day
#

# Outcome columns (using the CONDITION cohort ids):
#   outcome_<conditionBCohortId>      : 1 when A first (B is the outcome)
#   outcome_days_<conditionBCohortId> : days from A to B
#   outcome_<conditionACohortId>      : 1 when B first (A is the outcome)
#   outcome_days_<conditionACohortId> : days from B to A
#

# Parameters:
#   connection              : DatabaseConnector connection object
#   cohortDatabaseSchema    : schema holding the cohort and target tables
#   cohortTable             : name of the cohort table (default "cohort")
#   targetId                : target cohort id (forms target_<id>)
#   conditionACohortId      : cohort_definition_id for condition A (e.g. HTN)
#   conditionBCohortId      : cohort_definition_id for condition B (e.g. CKD)
#   aFirstBaselineId        : synthetic cohort id for "A first" baseline flag
#   bFirstBaselineId        : synthetic cohort id for "B first" baseline flag
#   sameDayBaselineId       : synthetic cohort id for "same day" baseline flag
# ==============================================================================

addFirstEventFlags <- function(connection,
                                cohortDatabaseSchema,
                                cohortTable = "cohort",
                                targetId,
                                conditionACohortId,
                                conditionBCohortId,
                                aFirstBaselineId,
                                bFirstBaselineId,
                                sameDayBaselineId)

{
  dbms <- DatabaseConnector::dbms(connection)
  targetTableName <- paste0("target_", targetId)
   # --- Column names -----------------------------------------------------------
  # Baseline flags (synthetic ids)
  aFirstCol  <- paste0("baseline_", aFirstBaselineId)
  bFirstCol  <- paste0("baseline_", bFirstBaselineId)
  sameDayCol <- paste0("baseline_", sameDayBaselineId)
  # Prior outcome (condition ids)
  priorACol <- paste0("prior_outcome_", conditionACohortId)
  priorBCol <- paste0("prior_outcome_", conditionBCohortId)
  # Outcome flag and days (condition ids)
  outACol  <- paste0("outcome_", conditionACohortId)
  daysACol <- paste0("outcome_days_", conditionACohortId)
  outBCol  <- paste0("outcome_", conditionBCohortId)
  daysBCol <- paste0("outcome_days_", conditionBCohortId)
  # --- Step 1: Add columns ---------------------------------------------------

  # Baseline flags (default 0)
  alterTableAddColumn(connection, cohortDatabaseSchema, targetTableName, aFirstCol, "INT", 0)
  alterTableAddColumn(connection, cohortDatabaseSchema, targetTableName, bFirstCol, "INT", 0)
  alterTableAddColumn(connection, cohortDatabaseSchema, targetTableName, sameDayCol, "INT", 0)
 
  # Prior outcome (default 0)
  alterTableAddColumn(connection, cohortDatabaseSchema, targetTableName, priorACol, "INT", 0)
  alterTableAddColumn(connection, cohortDatabaseSchema, targetTableName, priorBCol, "INT", 0)
 
  # Outcome flag (default 0) and days (default NULL)
  alterTableAddColumn(connection, cohortDatabaseSchema, targetTableName, outACol, "INT", 0)
  alterTableAddColumn(connection, cohortDatabaseSchema, targetTableName, daysACol, "INT", NULL)
  alterTableAddColumn(connection, cohortDatabaseSchema, targetTableName, outBCol, "INT", 0)
  alterTableAddColumn(connection, cohortDatabaseSchema, targetTableName, daysBCol, "INT", NULL)
 
  # --- Step 2: Single UPDATE -------------------------------------------------
  sql <- "
  UPDATE @cohort_database_schema.@target_table_name AS t
  SET
      -- Baseline flags: which came first (synthetic ids)
      @a_first_col   = m.a_first,
      @b_first_col   = m.b_first,
      @same_day_col  = m.same_day,
       -- Prior outcome: the first condition IS the prior (condition ids)

      -- A is prior when A came first or same day
      @prior_a_col   = CASE WHEN m.a_first = 1 OR m.same_day = 1 THEN 1 ELSE 0 END,

      -- B is prior when B came first or same day
      @prior_b_col   = CASE WHEN m.b_first = 1 OR m.same_day = 1 THEN 1 ELSE 0 END,
 
      -- Outcome: the SECOND condition is the outcome (condition ids)
      -- B is outcome when A came first
      @out_b_col     = m.a_first,
      @days_b_col    = CASE WHEN m.a_first = 1 THEN m.tte_a_to_b ELSE NULL END,

      -- A is outcome when B came first
      @out_a_col     = m.b_first,
      @days_a_col    = CASE WHEN m.b_first = 1 THEN m.tte_b_to_a ELSE NULL END
 
  FROM (
    SELECT
      t.subject_id,
      -- A came first
      CASE WHEN MIN(CASE WHEN c.cohort_definition_id = @condition_a_id
                         THEN c.cohort_start_date ELSE NULL END)
        < MIN(CASE WHEN c.cohort_definition_id = @condition_b_id
                         THEN c.cohort_start_date ELSE NULL END)
                         THEN 1 ELSE 0 END AS a_first,
 
      -- B came first
      CASE WHEN MIN(CASE WHEN c.cohort_definition_id = @condition_b_id
                         THEN c.cohort_start_date ELSE NULL END)
        < MIN(CASE WHEN c.cohort_definition_id = @condition_a_id
                        THEN c.cohort_start_date ELSE NULL END)
                        THEN 1 ELSE 0 END AS b_first,
 
      -- Same day
      CASE WHEN MIN(CASE WHEN c.cohort_definition_id = @condition_a_id
                         THEN c.cohort_start_date ELSE NULL END)
                = MIN(CASE WHEN c.cohort_definition_id = @condition_b_id
                           THEN c.cohort_start_date ELSE NULL END)
           THEN 1 ELSE 0 END AS same_day,
 
      -- TTE from A to B
        (MIN(CASE WHEN c.cohort_definition_id = @condition_b_id
                THEN c.cohort_start_date ELSE NULL END)
       - MIN(CASE WHEN c.cohort_definition_id = @condition_a_id
                  THEN c.cohort_start_date ELSE NULL END)) AS tte_a_to_b,
 
      -- TTE from B to A
        (MIN(CASE WHEN c.cohort_definition_id = @condition_a_id
                THEN c.cohort_start_date ELSE NULL END)
       - MIN(CASE WHEN c.cohort_definition_id = @condition_b_id
                  THEN c.cohort_start_date ELSE NULL END)) AS tte_b_to_a
 
    FROM @cohort_database_schema.@target_table_name t
    JOIN @cohort_database_schema.@cohort_table c
      ON t.subject_id = c.subject_id
      AND c.cohort_definition_id IN (@condition_a_id, @condition_b_id)
    GROUP BY t.subject_id
  ) m
  WHERE t.subject_id = m.subject_id;"
 
  renderedSql <- SqlRender::render(
    sql,
    cohort_database_schema = cohortDatabaseSchema,
    target_table_name      = targetTableName,
    cohort_table           = cohortTable,
    condition_a_id         = as.integer(conditionACohortId),
    condition_b_id         = as.integer(conditionBCohortId),
    a_first_col            = aFirstCol,
    b_first_col            = bFirstCol,
    same_day_col           = sameDayCol,
    prior_a_col            = priorACol,
    prior_b_col            = priorBCol,
    out_a_col              = outACol,
    days_a_col             = daysACol,
    out_b_col              = outBCol,
    days_b_col             = daysBCol
)

  translatedSql <- SqlRender::translate(renderedSql, targetDialect = dbms)
  ParallelLogger::logInfo("Adding first-event flags and outcome TTE for conditions ",
                          conditionACohortId, " vs ", conditionBCohortId,
                          " on ", targetTableName)

  DatabaseConnector::executeSql(connection, translatedSql)
  invisible(TRUE)
}
 

# ==============================================================================
# addStageAtBaseline
# ==============================================================================
# Determines the stage at baseline by going back to the raw covariate table
# (baseline_target_<targetId>) and finding the stage record closest to index
# (day_offset closest to 0, but <= 0). If multiple stages on the same date,
# picks the most severe (highest rank).
#
# stageDefinitions: data.frame with cohortName, cohortId, rank
# Creates one column named by stageColumnName containing the cohortId of the
# selected stage, or NULL if no stage record exists before baseline.
# ==============================================================================
addStageAtBaseline <- function(connection,
                                cohortDatabaseSchema,
                                targetId,
                                stageDefinitions,
                                stageColumnName,
                                columnPrefix = "baseline")
{
  dbms <- DatabaseConnector::dbms(connection)
  required <- c("cohortName", "cohortId", "rank")
  if (!all(required %in% colnames(stageDefinitions))) {
    stop("stageDefinitions must contain columns: ", paste(required, collapse = ", "))
  }
 
  if (nrow(stageDefinitions) == 0) {
    ParallelLogger::logWarn("stageDefinitions is empty; skipping.")
    return(invisible(NULL))
  }
 
  targetTableName <- paste0("target_", targetId)
  covariateTable  <- paste0(columnPrefix, "_target_", targetId)
 
  # --- Add column -------------------------------------------------------------
  alterTableAddColumn(connection, cohortDatabaseSchema, targetTableName,
                      stageColumnName, "INT")
 
  # --- Build rank mapping CASE ------------------------------------------------
  # Maps each covariate_id to its rank inside SQL
  rankClauses <- character(nrow(stageDefinitions))
  for (i in seq_len(nrow(stageDefinitions))) {
    rankClauses[i] <- SqlRender::render(
      "WHEN @cohort_id THEN @rank",
      cohort_id = as.integer(stageDefinitions$cohortId[i]),
      rank      = as.integer(stageDefinitions$rank[i])
    )
  }
 
  stageIdList <- paste(as.integer(stageDefinitions$cohortId), collapse = ", ")
 
  # --- Single UPDATE ----------------------------------------------------------
  # 1. Filter covariate table to stage ids with day_offset <= 0 (at or before index)
  # 2. Map each covariate_id to its rank
  # 3. ROW_NUMBER partitioned by subject, ordered by:
  #    - day_offset DESC (most recent first, closest to index)
  #    - rank DESC (most severe first, for ties on same date)
  # 4. Pick rn = 1 per subject
  sql <- "
  UPDATE @cohort_database_schema.@target_table_name AS t
  SET @stage_column = m.covariate_id
  FROM (
    SELECT subject_id, covariate_id
    FROM (
      SELECT
        c.subject_id,
        c.covariate_id,
        ROW_NUMBER() OVER (
          PARTITION BY c.subject_id
          ORDER BY
            c.day_offset DESC,
            CASE c.covariate_id
              @rank_clauses
            END DESC
        ) AS rn
      FROM @cohort_database_schema.@covariate_table c
      WHERE c.covariate_id IN (@stage_id_list)
        AND c.day_offset <= 0
    ) ranked
    WHERE rn = 1
  ) m
  WHERE t.subject_id = m.subject_id;"
 
  renderedSql <- SqlRender::render(
    sql,
    cohort_database_schema = cohortDatabaseSchema,
    target_table_name      = targetTableName,
    covariate_table        = covariateTable,
    stage_column           = stageColumnName,
    rank_clauses           = paste(rankClauses, collapse = "\n              "),
    stage_id_list          = stageIdList
  )
 
  translatedSql <- SqlRender::translate(renderedSql, targetDialect = dbms)
 
  ParallelLogger::logInfo("Adding ", stageColumnName, " on ", targetTableName,
                          " from ", nrow(stageDefinitions), " stage definitions",
                          " (closest to index, most severe on ties)")
  DatabaseConnector::executeSql(connection, translatedSql)
 
  invisible(TRUE)
}


# ==============================================================================
# ==============================================================================+
# ==============================================================================
# addStageStrataColumns
# ==============================================================================
# Reads an existing stage column (e.g. ckd_stage_at_baseline) on
# target_<targetId> and creates one-hot encoded strata_<cohortId> columns.
#
# For each cohortId in stageDefinitions:
#   strata_<cohortId> = 1 if the stage column equals that cohortId
#   strata_<cohortId> = 0 otherwise (including NULL)
#
# Intended to run AFTER addStageAtBaseline, which populates the stage column.
# ==============================================================================
addStageStrataColumns <- function(connection,
                                  cohortDatabaseSchema,
                                  targetId,
                                  stageDefinitions,
                                  sourceColumn,
                                  columnPrefix = "strata")
{
  dbms <- DatabaseConnector::dbms(connection)
  if (!("cohortId" %in% colnames(stageDefinitions))) {
    stop("stageDefinitions must contain a cohortId column.")
  }
 
  if (nrow(stageDefinitions) == 0) {
    ParallelLogger::logWarn("stageDefinitions is empty; skipping.")
    return(invisible(NULL))
  }
 
  targetTableName <- paste0("target_", targetId)
  cohortIds <- as.integer(stageDefinitions$cohortId)
 
  # --- Step 1: Add columns (default 0) ----------------------------------------
  for (cid in cohortIds) {
    alterTableAddColumn(connection, cohortDatabaseSchema, targetTableName,
                        paste0(columnPrefix, "_", cid), "INT", 0)
  }
 
  # --- Step 2: Build SET clauses ----------------------------------------------
  setClauses <- character(length(cohortIds))
  for (i in seq_along(cohortIds)) {
    setClauses[i] <- SqlRender::render(
      "@col_name = CASE WHEN @source_column = @cohort_id THEN 1 ELSE 0 END",
      col_name      = paste0(columnPrefix, "_", cohortIds[i]),
      source_column = sourceColumn,
      cohort_id     = cohortIds[i]
    )
  }
 
  # --- Step 3: Single UPDATE --------------------------------------------------
  sql <- "
  UPDATE @cohort_database_schema.@target_table_name
  SET @set_clauses;"
 
  renderedSql <- SqlRender::render(
    sql,
    cohort_database_schema = cohortDatabaseSchema,
    target_table_name      = targetTableName,
    set_clauses            = paste(setClauses, collapse = ",\n    ")
  )
 
  translatedSql <- SqlRender::translate(renderedSql, targetDialect = dbms)
 
  ParallelLogger::logInfo("Adding ", length(cohortIds), " one-hot strata columns from ",
                          sourceColumn, " on ", targetTableName)
  DatabaseConnector::executeSql(connection, translatedSql)
 
  invisible(TRUE)
}

# ==============================================================================
# ==============================================================================
# ==============================================================================
# ==============================================================================
# addMeasurementChangeOutcome
# ==============================================================================

# For each subject in target_<targetId>, checks whether a measurement changed
# by a given threshold from its baseline value. Supports two directions:

#   direction = "decline"  : follow-up <= baseline * (1 - changePercent/100)
#                            e.g. eGFR 50% decline: follow-up <= baseline * 0.50
#
#   direction = "increase" : follow-up >= baseline * (1 + changePercent/100)
#                            e.g. uACR doubling: follow-up >= baseline * 2.00
#
# Baseline is read from the target table column <baselineColumnPrefix>_<measurementCohortId>.
# Follow-up values are read from measurements_target_<targetId>.
#
# Creates two columns on target_<targetId>:
#   <columnPrefix>_<changeSuffix>_<measurementCohortId>      : 1 if change occurred, 0 otherwise
#   <columnPrefix>_<changeSuffix>_days_<measurementCohortId> : day_offset of first qualifying change
#                                                              NULL if no change
#
# Parameters:
#   connection              : DatabaseConnector connection object
#   cohortDatabaseSchema    : schema holding the target and measurement tables
#   targetId                : target cohort id (forms target_<id>)
#   measurementCohortId     : cohort id of the measurement
#   changePercent           : percentage change threshold (e.g. 50 for 50% decline, 100 for doubling)
#   direction               : "decline" or "increase"
#   changeSuffix            : suffix for column names (e.g. "decline", "doubling")
#   windowStart             : earliest day_offset to consider (default 1)
#   windowEnd               : latest day_offset to consider (default 9999)
#   columnPrefix            : prefix for created columns (default "meas")
#   baselineColumnPrefix    : prefix of the baseline measurement column on target (default "meas")
# ==============================================================================

addMeasurementChangeOutcome <- function(connection,
                                        cohortDatabaseSchema,
                                        targetId,
                                        measurementCohortId,
                                        changePercent,
                                        direction            = "decline",
                                        changeSuffix         = NULL,
                                        windowStart          = 1,
                                        windowEnd            = 9999,
                                        columnPrefix         = "meas",
                                        baselineColumnPrefix = "meas")

{

  # --- Validate direction -----------------------------------------------------
  direction <- tolower(direction)
  if (!direction %in% c("decline", "increase")) {
    stop("direction must be 'decline' or 'increase'.")
  }
 
  # --- Compute threshold multiplier and comparison operator -------------------
  if (direction == "decline") {
    # e.g. 50% decline: follow-up <= baseline * 0.50
    thresholdMultiplier <- (100 - changePercent) / 100
    operator <- "<="
  } else {
    # e.g. 100% increase (doubling): follow-up >= baseline * 2.00

    thresholdMultiplier <- (100 + changePercent) / 100

    operator <- ">="

  }
 
  # --- Default suffix if not provided -----------------------------------------

  if (is.null(changeSuffix)) {

    changeSuffix <- direction

  }
 
  # --- Column names -----------------------------------------------------------

  targetTableName      <- paste0("target_", targetId)

  measurementTableName <- paste0("measurements_target_", targetId)

  baselineCol          <- paste0(baselineColumnPrefix, "_", measurementCohortId)

  changeCol            <- paste0(columnPrefix, "_", changeSuffix, "_", measurementCohortId)

  changeDaysCol        <- paste0(columnPrefix, "_", changeSuffix, "_days_", measurementCohortId)
 
  # --- Step 1: Add columns ---------------------------------------------------

  alterTableAddColumn(connection, cohortDatabaseSchema, targetTableName,

                      changeCol, "INT", 0)

  alterTableAddColumn(connection, cohortDatabaseSchema, targetTableName,

                      changeDaysCol, "INT")
 
  # --- Step 2: Find the first follow-up measurement that qualifies -----------

  sql <- "

  UPDATE @cohort_database_schema.@target_table_name AS t

  SET @change_col      = CASE WHEN m.first_change_day IS NOT NULL THEN 1 ELSE 0 END,

      @change_days_col = m.first_change_day

  FROM (

    SELECT subject_id, day_offset AS first_change_day

    FROM (

      SELECT

        fu.subject_id,

        fu.day_offset,

        ROW_NUMBER() OVER (

          PARTITION BY fu.subject_id

          ORDER BY fu.day_offset ASC

        ) AS rn

      FROM @cohort_database_schema.@measurement_table_name fu

      JOIN @cohort_database_schema.@target_table_name t

        ON fu.subject_id = t.subject_id

      WHERE fu.cohort_id = @measurement_cohort_id

        AND fu.value_as_number IS NOT NULL

        AND t.@baseline_col IS NOT NULL

        AND fu.day_offset BETWEEN @window_start AND @window_end

        AND fu.value_as_number @operator t.@baseline_col * @threshold_multiplier

    ) ranked

    WHERE rn = 1

  ) m

  WHERE t.subject_id = m.subject_id;"
 
  renderedSql <- SqlRender::render(

    sql,

    cohort_database_schema = cohortDatabaseSchema,

    target_table_name      = targetTableName,

    measurement_table_name = measurementTableName,

    baseline_col           = baselineCol,

    change_col             = changeCol,

    change_days_col        = changeDaysCol,

    measurement_cohort_id  = as.integer(measurementCohortId),

    window_start           = as.integer(windowStart),

    window_end             = as.integer(windowEnd),

    operator               = operator,

    threshold_multiplier   = thresholdMultiplier

  )
 
  translatedSql <- SqlRender::translate(renderedSql, targetDialect = dbms)
 
  ParallelLogger::logInfo("Adding ", changePercent, "% ", direction, " columns (",

                          changeCol, ") for measurement ", measurementCohortId,

                          " on ", targetTableName,

                          " (window [", windowStart, ", ", windowEnd, "])")

  DatabaseConnector::executeSql(connection, translatedSql)
 
  invisible(TRUE)

}
 