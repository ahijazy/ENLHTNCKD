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
                         newCohortId,
                         dbms = "postgresql") 
{
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
  translatedSql <- SqlRender::translate(renderedSql, targetDialect = attr(connection, "dbms"))
  
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
{
  # Safety check
  if (prevalentCohortId == CKDCohortId || prevalentCohortId == HTNCohortId) {
    stop(paste0(
      "CRITICAL ERROR: 'prevalentCohortId' (", prevalentCohortId, ") ",
      "cannot match source 'CKDCohortId' or 'HTNCohortId'."
    ))
  }

  # Standard OHDSI T-SQL Template (compiles to all major dialects)
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

  # 1. Swap R variables into the SQL template placeholders
  renderedSql <- SqlRender::render(sql = sql_prevalent_population, 
                                   cohort_database_schema = cohortDatabaseSchema,
                                   cohort_table = cohortTable,
                                   ckd_cohort_id = CKDCohortId,
                                   htn_cohort_id = HTNCohortId,
                                   prevalent_cohort_id = prevalentCohortId,
                                   index_date = indexDate)
  
  # 2. Universal Dialect Translation (reads database metadata to detect connection type)
  translatedSql <- SqlRender::translate(renderedSql, targetDialect = attr(connection, "dbms"))
  
  ParallelLogger::logInfo("Populating cohort table with Prevalent Cohort ", prevalentCohortId)
  
  # 3. Execute translated SQL query 
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