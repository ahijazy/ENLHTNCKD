# ==============================================================================
# addAllEventColumns
# ==============================================================================
# Generic bulk-update function that adds and populates event-related columns on
# target_<targetId> from a precomputed covariate table (either outcomes or
# treatments).
#
# Two modes controlled by isTreatment:
#
#   isTreatment = FALSE (outcomes, point-in-time events)
#   -------------------------------------------------------
#   Columns per cohortId:
#     prior_<prefix>_<cid>     : 1 if any event occurred before index (day_offset < 0)
#     <prefix>_<cid>           : 1 if any event occurred in [windowStart, windowEnd]
#     <prefix>_days_<cid>      : days to first event in window; falls back to
#                                days to end_of_continuous_observation if no event
#
#   isTreatment = TRUE (treatments, interval-based episodes)
#   -------------------------------------------------------
#   Columns per cohortId:
#     prior_treatment_<cid>                   : 1 if any episode started before windowStart
#     treatment_status_at_baseline_<cid>      : 1 if any episode overlaps the index date
#                                               (day_offset <= 0 AND day_offset_end >= 0)
#     treatment_initiated_at_baseline_<cid>   : 1 if any episode starts exactly on index
#                                               (day_offset = 0)
#     time_to_treatment_initiation_<cid>      : days to first post-baseline start within
#                                               [1, windowEnd]; forced NULL when
#                                               treatment_status_at_baseline = 1 (already
#                                               on treatment, so initiation is meaningless)
#
# The function:
#   1. Adds all columns (flags default to 0, days/tti default to NULL)
#   2. Builds per-cohortId CASE WHEN clauses via SqlRender::render
#   3. Executes a single UPDATE ... FROM with a LEFT JOIN to the covariate\
#      table, so subjects with no matching events keep their defaults
#

# Parameters:
#   connection           : DatabaseConnector connection object
#   cohortDatabaseSchema : schema holding target and covariate tables
#   targetId             : integer target cohort id (forms table name target_<id>)
#   outcomeDefinitions   : data.frame with cohortId, windowStart, windowEnd
#   columnPrefix         : string prefix for outcome columns (ignored when isTreatment)
#   outcomeTablePrefix   : string prefix for the covariate table name
#   isTreatment          : logical; FALSE = point-in-time outcomes, TRUE = interval treatments
# ==============================================================================

addAllEventColumns <- function(connection,
                                cohortDatabaseSchema,
                                targetId,
                                outcomeDefinitions,
                                columnPrefix       = "outcome",
                                outcomeTablePrefix = "outcomes_target_",
                                isTreatment        = FALSE)

{
   dbms = DatabaseConnector::dbms(connection)
  # --- Input validation -------------------------------------------------------
  required <- c("cohortId", "windowStart", "windowEnd")
  if (!all(required %in% colnames(outcomeDefinitions))) {
    stop("outcomeDefinitions must contain columns: ", paste(required, collapse = ", "))
  }
  # Coerce to integer; invalid values become NA
  outcomeDefinitions$cohortId    <- suppressWarnings(as.integer(outcomeDefinitions$cohortId))
  outcomeDefinitions$windowStart <- suppressWarnings(as.integer(outcomeDefinitions$windowStart))
  outcomeDefinitions$windowEnd   <- suppressWarnings(as.integer(outcomeDefinitions$windowEnd))
 
  # Drop rows with NA or invalid window

  bad <- is.na(outcomeDefinitions$cohortId) |
         is.na(outcomeDefinitions$windowStart) |
         is.na(outcomeDefinitions$windowEnd) |
         outcomeDefinitions$windowStart > outcomeDefinitions$windowEnd

  if (any(bad)) {
    ParallelLogger::logWarn("Dropping ", sum(bad), " invalid definition row(s) for target_", targetId)
    outcomeDefinitions <- outcomeDefinitions[!bad, , drop = FALSE]
  }
 
  # Nothing left to process
  if (nrow(outcomeDefinitions) == 0) {
    ParallelLogger::logWarn("No valid definitions for target_", targetId, "; skipping.")
    return(invisible(NULL))
  }

  # --- Table names ------------------------------------------------------------
  tableName    <- paste0("target_", targetId)
  outcomeTable <- paste0(outcomeTablePrefix, targetId)
 
  # ==========================================================================
  # Step 1: Add columns
  # ==========================================================================

  for (i in seq_len(nrow(outcomeDefinitions))) {
    cid <- outcomeDefinitions$cohortId[i]
    if (!isTreatment) 
    {
      # Outcome mode: prior flag, post flag, days to event
      alterTableAddColumn(connection, cohortDatabaseSchema, tableName,
                          paste0("prior_", columnPrefix, "_", cid), "INT", 0)

      alterTableAddColumn(connection, cohortDatabaseSchema, tableName,
                          paste0(columnPrefix, "_", cid), "INT", 0)

      alterTableAddColumn(connection, cohortDatabaseSchema, tableName,
                          paste0(columnPrefix, "_days_", cid), "INT")
 
    } else
    {
      # Treatment mode: prior, status at baseline, initiated, time to initiation
      alterTableAddColumn(connection, cohortDatabaseSchema, tableName,
                          paste0("prior_treatment_", cid), "INT", 0)

      alterTableAddColumn(connection, cohortDatabaseSchema, tableName,
                          paste0("treatment_status_at_baseline_", cid), "INT", 0)

      alterTableAddColumn(connection, cohortDatabaseSchema, tableName,
                          paste0("treatment_initiated_at_baseline_", cid), "INT", 0)

      alterTableAddColumn(connection, cohortDatabaseSchema, tableName,
                          paste0("time_to_treatment_initiation_", cid), "INT")
    }
  }
 
  # ==========================================================================
  # Step 2: Build per-cohortId CASE clauses
  # ==========================================================================

  caseClauses <- vapply(seq_len(nrow(outcomeDefinitions)), function(i) {

    cid <- outcomeDefinitions$cohortId[i]
    ws  <- outcomeDefinitions$windowStart[i]
    we  <- outcomeDefinitions$windowEnd[i]

    if (!isTreatment) 
    {
      # ---- Outcome mode (point-in-time) ------------------------------------
      #   prior  : any event before index
      #   post   : any event started in [windowStart, windowEnd]
      #   days   : day_offset of first event in window; falls back to
      #            observation end if no event in window

      priorCol <- paste0("prior_", columnPrefix, "_", cid)
      postCol  <- paste0(columnPrefix, "_", cid)
      daysCol  <- paste0(columnPrefix, "_days_", cid)
 
      SqlRender::render(
        "MAX(CASE WHEN o.covariate_id = @cid AND o.day_offset < @ws
                  THEN 1 ELSE 0 END) AS @prior_col,
         MAX(CASE WHEN o.covariate_id = @cid
                   AND o.day_offset BETWEEN @ws AND @we
                  THEN 1 ELSE 0 END) AS @post_col,
         MIN(COALESCE(
           CASE WHEN o.covariate_id = @cid
                  AND o.day_offset BETWEEN @ws AND @we
                THEN o.day_offset ELSE NULL END,
           DATEDIFF(day, t.target_start_date, t.end_of_continuous_observation)
         )) AS @days_col",

        cid = cid, ws = ws, we = we, prior_col = priorCol, post_col = postCol, days_col = daysCol)
    } 
    else
    {
      # ---- Treatment mode (interval-based) ---------------------------------
      #   prior   : any episode started before the window
      #   status  : any episode overlaps index date (day 0)
      #             i.e. started on/before index AND ended on/after index
      #   init    : any episode starts exactly on index (day_offset = 0)
      #   tti     : first post-baseline start in [1, windowEnd]

      priorCol  <- paste0("prior_treatment_", cid)
      statusCol <- paste0("treatment_status_at_baseline_", cid)
      initCol   <- paste0("treatment_initiated_at_baseline_", cid)
      ttiCol    <- paste0("time_to_treatment_initiation_", cid)
    
      SqlRender::render(
      "MAX(CASE WHEN o.covariate_id = @cid AND o.day_offset < @ws
                THEN 1 ELSE 0 END) AS @prior_col,

      MAX(CASE WHEN o.covariate_id = @cid
                AND o.day_offset <= @we AND o.day_offset_end >= @ws
                THEN 1 ELSE 0 END) AS @status_col,

      MAX(CASE WHEN o.covariate_id = @cid
                AND o.day_offset BETWEEN @ws AND @we
                THEN 1 ELSE 0 END) AS @init_col,

      MIN(CASE WHEN o.covariate_id = @cid
                AND o.day_offset > @we
                THEN o.day_offset ELSE NULL END) AS @tti_col",

      cid = cid, ws = ws, we = we, prior_col = priorCol, status_col = statusCol,
      init_col = initCol, tti_col = ttiCol
    )
    }
  }, character(1))

  # ==========================================================================
  # Step 3: Build SET clauses
  # ==========================================================================
  setClauses <- vapply(seq_len(nrow(outcomeDefinitions)), function(i) {
    cid <- outcomeDefinitions$cohortId[i]
    if (!isTreatment)
    {
      # Outcome mode: straightforward copy from subquery
      priorCol <- paste0("prior_", columnPrefix, "_", cid)
      postCol  <- paste0(columnPrefix, "_", cid)
      daysCol  <- paste0(columnPrefix, "_days_", cid)

      SqlRender::render(

        "@prior_col = m.@prior_col,
         @post_col = m.@post_col,
         @days_col = m.@days_col",

        prior_col = priorCol, post_col = postCol, days_col = daysCol
      )
 
    } else
    {
      # Treatment mode: copy flags directly; force tti to NULL when already
      # on treatment at baseline (status = 1) because "time to initiation"
      # is only meaningful for subjects not yet on treatment

      priorCol  <- paste0("prior_treatment_", cid)
      statusCol <- paste0("treatment_status_at_baseline_", cid)
      initCol   <- paste0("treatment_initiated_at_baseline_", cid)
      ttiCol    <- paste0("time_to_treatment_initiation_", cid)
 
      SqlRender::render(
        "@prior_col = m.@prior_col,
         @status_col = m.@status_col,
         @init_col = m.@init_col,
         @tti_col = CASE WHEN m.@status_col = 0 THEN m.@tti_col ELSE NULL END",

        prior_col = priorCol, status_col = statusCol,
        init_col = initCol, tti_col = ttiCol
      )
    }
  }, character(1))
 
  # ==========================================================================
  # Step 4: Build and execute single UPDATE ... FROM
  # ==========================================================================

  # Combine all cohort ids for the IN clause so the LEFT JOIN is selective
  cohortIdList <- paste(outcomeDefinitions$cohortId, collapse = ", ")
 
  # The subquery aggregates per subject across all covariate rows, computing
  # every CASE clause at once (one pass). LEFT JOIN ensures subjects with no
  # matching event still appear in the result (keeping defaults).

  sql <- "
  UPDATE @cohort_database_schema.@target_table_name AS t
  SET @set_clauses
  FROM (
    SELECT t.subject_id, t.target_start_date, t.end_of_continuous_observation,
           @case_clauses
    FROM @cohort_database_schema.@target_table_name t
    LEFT JOIN @cohort_database_schema.@outcome_table o
      ON t.subject_id = o.subject_id
      AND o.covariate_id IN (@cohort_id_list)
    GROUP BY t.subject_id, t.target_start_date, t.end_of_continuous_observation
  ) m
  WHERE t.subject_id = m.subject_id;"
 
  renderedSql <- SqlRender::render(sql,
                                   cohort_database_schema = cohortDatabaseSchema,
                                   target_table_name = tableName,
                                   outcome_table = outcomeTable,
                                   cohort_id_list = cohortIdList,
                                   set_clauses = paste(setClauses, collapse = ",\n    "),
                                   case_clauses = paste(caseClauses, collapse = ",\n    "))
 
  translatedSql <- SqlRender::translate(renderedSql, targetDialect = dbms)
 
  label <- if (isTreatment) "treatment baseline" else columnPrefix
  ParallelLogger::logInfo("Bulk-populating '", label, "' columns for ", nrow(outcomeDefinitions),
                          " cohorts on ", cohortDatabaseSchema, ".", tableName,
                          " from ", outcomeTable)
  DatabaseConnector::executeSql(connection, translatedSql)

  invisible(TRUE)

}
 
# ==============================================================================
# addAllOutcomes (thin wrapper for outcome mode)
# ==============================================================================
# Adds prior/flag/days columns for point-in-time outcomes.
# Reads from outcomes_target_<targetId>.
# ==============================================================================

addAllOutcomes <- function(connection,
                           cohortDatabaseSchema,
                           targetId,
                           outcomeDefinitions)
 {
   dbms = DatabaseConnector::dbms(connection)

  addAllEventColumns(
    connection           = connection,
    cohortDatabaseSchema = cohortDatabaseSchema,
    targetId             = targetId,
    outcomeDefinitions   = outcomeDefinitions,
    columnPrefix         = "outcome",
    outcomeTablePrefix   = "outcomes_target_",
    isTreatment          = FALSE
  )
}
 

# ==============================================================================
# addAllTreatmentColumns (thin wrapper for treatment mode)
# ==============================================================================

# Adds prior/status_at_baseline/initiated/time_to_initiation columns for
# interval-based treatment episodes.
# Reads from treatments_target_<targetId>.
# ==============================================================================

addAllTreatmentColumns <- function(connection,
                                   cohortDatabaseSchema,
                                   targetId,
                                  treatmentDefinitions) 
{
  dbms = DatabaseConnector::dbms(connection)
  addAllEventColumns(

    connection           = connection,
    cohortDatabaseSchema = cohortDatabaseSchema,
    targetId             = targetId,
    outcomeDefinitions   = treatmentDefinitions,
    columnPrefix         = "treatment",
    outcomeTablePrefix   = "treatments_target_",
    isTreatment          = TRUE
  )

}
 