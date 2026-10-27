# ==============================================================================
# addDemographicRangeStrata: Add demographic range strata to the target table
# these are value based with a lower and upper bound
# ==============================================================================
addDemographicRangeStrata <- function(connection,
                                      cohortDatabaseSchema,
                                      targetId,
                                      strata, #dataframe
                                      targetColumn,
                                      strataColumnName) {

  targetTableName <- paste0("target_", targetId)

  alterTableAddColumn(connection,
                      schemaName   = cohortDatabaseSchema,
                      tableName    = targetTableName,
                      columnName   = strataColumnName,
                      type         = "BIGINT",
                      defaultValue = NULL)

# from the utilities
  whenClauses <- buildRangeCaseWhenClauses(cohortList  = strata,
                                           valueColumn = targetColumn)

  sql <- paste0(
    "UPDATE @cohort_database_schema.@target_table_name\n",
    "SET @strata_column = CASE\n  ",
    paste(whenClauses, collapse = "\n  "),
    "\nEND;"
  )
  sql_rendered <- SqlRender::render(
    sql,
    cohort_database_schema = cohortDatabaseSchema,
    target_table_name      = targetTableName,
    strata_column          = strataColumnName
  )
  sql_translated <- SqlRender::translate(sql_rendered, targetDialect = attr(connection, "dbms"))

  ParallelLogger::logInfo("Assigning ", strataColumnName, " on target_", targetId,
                          " across ", nrow(strata), " ", targetColumn, " bands")
  DatabaseConnector::executeSql(connection, sql_translated)
  ParallelLogger::logInfo("Successfully assigned ", strataColumnName, " on target_", targetId)
}
 
# ==============================================================================
# Runners, add age and index strata and age bins to the target table
# assign demographic range strata columns to target_<id>
# Each reads a settings CSV of [lowerBound, upperBound) bands and assigns the
# matching band cohortId into a strata column on the target table.
# ==============================================================================

# ==============================================================================
# add age strata to the target table
# ==============================================================================

addAgeStrataToTarget <- function(connection,
                                 cohortDatabaseSchema,
                                 targetId,
                                 strata) 
{
  #check the column names: they need to match cohortName	lowerBound	upperBound	cohortId	targetId
  required <- c("cohortName", "lowerBound", "upperBound", "cohortId")  # targetId optional
  if (!all(required %in% colnames(strata))) {
    stop("Strata data frame must contain columns: ", paste(required, collapse = ", "))
  }
  if (nrow(strata) == 0) {
    warning("No age strata rows available for targetId=", targetId, ". Skipping addAgeStrataToTarget.")
    return(invisible(NULL))
  }
  ParallelLogger::logInfo("adding age strata on target_", targetId)
  addDemographicRangeStrata(connection,
                            cohortDatabaseSchema,
                            targetId,
                            strata           = strata,
                            targetColumn     = "age",
                            strataColumnName = "ageStrata")
  return(invisible(NULL))
}
# ==============================================================================
# add the index-year strata to the target table
# ==============================================================================

addIndexStrataToTarget <- function(connection,
                                   cohortDatabaseSchema,
                                   targetId,
                                   strata) 
{
  #check the column names: they need to match cohortName	lowerBound	upperBound	cohortId	targetId
  required <- c("cohortName", "lowerBound", "upperBound", "cohortId")  # targetId optional
  if (!all(required %in% colnames(strata))) {
    stop("Strata data frame must contain columns: ", paste(required, collapse = ", "))
  }
  if (nrow(strata) == 0) {
    warning("No index strata rows available for targetId=", targetId, ". Skipping addIndexStrataToTarget.")
    return(invisible(NULL))
  }
  ParallelLogger::logInfo("adding index strata on target_", targetId)
  addDemographicRangeStrata(connection,
                            cohortDatabaseSchema,
                            targetId,
                            strata           = strata,
                            targetColumn     = "index_year",
                            strataColumnName = "indexStrata")
}
 
# ==============================================================================
# add the age bins to the target table
# NOTE: these are used for the charachterization of the target cohort not as strata for the analysis
# ==============================================================================

addAgeBinsToTarget <- function(connection,
                               cohortDatabaseSchema,
                               targetId,
                               strata) 
{#check the column names: they need to match cohortName	lowerBound	upperBound	cohortId	targetId
  required <- c("cohortName", "lowerBound", "upperBound", "cohortId")  # targetId optional
    if (!all(required %in% colnames(strata))) {
    stop("Strata data frame must contain columns: ", paste(required, collapse = ", "))
  }
  if (nrow(strata) == 0) {
    warning("No age bins rows available for targetId=", targetId, ". Skipping addAgeBinsToTarget.")
    return(invisible(NULL))
  }
  ParallelLogger::logInfo("adding age bins on target_", targetId)
addDemographicRangeStrata(connection,
                            cohortDatabaseSchema,
                            targetId,
                            strata           = strata,
                            targetColumn     = "age",
                            strataColumnName = "ageBins")
}

# ==============================================================================
# addGenderAgeStrata
# Adds a GenderByAge column to target_<id> and populates it by concatenating
# the existing gender_concept_id and age-strata column already on the table.
# ==============================================================================
addGenderAgeStrata <- function(connection,
                               cohortDatabaseSchema,
                               targetId,
                               ageStrataColumn  = "ageStrata",
                               strataColumnName = "GenderByAge") {

  targetTableName <- paste0("target_", targetId)

  alterTableAddColumn(connection,
                      schemaName   = cohortDatabaseSchema,
                      tableName    = targetTableName,
                      columnName   = strataColumnName,
                      type         = "BIGINT",
                      defaultValue = NULL)

  sql <- "
  UPDATE @cohort_database_schema.@target_table_name
  SET @strata_column = CAST(CONCAT(gender_concept_id, @age_strata_column) AS BIGINT)
  WHERE gender_concept_id IS NOT NULL
    AND @age_strata_column IS NOT NULL;"

  sql_rendered <- SqlRender::render(
    sql,
    cohort_database_schema = cohortDatabaseSchema,
    target_table_name      = targetTableName,
    strata_column          = strataColumnName,
    age_strata_column      = ageStrataColumn
  )
  sql_translated <- SqlRender::translate(sql_rendered, targetDialect = attr(connection, "dbms"))

  ParallelLogger::logInfo("Assigning ", strataColumnName, " on ", targetTableName)
  DatabaseConnector::executeSql(connection, sql_translated)
  ParallelLogger::logInfo("Successfully assigned ", strataColumnName, " on ", targetTableName)
}
 
# ==============================================================================
# ==============================================================================
# Bulk-populate binary flag columns on target_<id> from a precomputed covariate
# table (e.g. baseline_target_<id>), which already carries covariate_id,
# subject_id, and day_offset.
#
# For each cohort definition (cohortId + windowStart/windowEnd relative to the
# index date), adds an INT 0/1 column <columnPrefix>_<cohortId> and sets it to 1
# when the subject has that covariate with day_offset in [windowStart, windowEnd].
#
# Window convention: day_offset = DATEDIFF(day, target_start_date,
# covariate_start_date); negative = before index. windowStart <= windowEnd.
# ==============================================================================
# ==============================================================================

addBinaryColumnsToTargetTableBulk <- function(connection,
                                              cohortDatabaseSchema,
                                              targetId,
                                              strata,   # data.frame: cohortId, windowStart, windowEnd
                                              columnPrefix,
                                              covariateTable)      # e.g. "baseline_target_123"
{

  # Coerce to integer and drop rows with NA id/window
  strata$cohortId    <- suppressWarnings(as.integer(strata$cohortId))
  strata$windowStart <- suppressWarnings(as.integer(strata$windowStart))
  strata$windowEnd   <- suppressWarnings(as.integer(strata$windowEnd))

  bad <- is.na(strata$cohortId) |
         is.na(strata$windowStart) |
         is.na(strata$windowEnd)
  # warning for the user if any rows are dropped
  if (any(bad)) {
    ParallelLogger::logWarn("Dropping ", sum(bad), " rows from '", columnPrefix,
                            "' definitions due to NA cohortId/windowStart/windowEnd")
  }
  strata <- strata[!bad, , drop = FALSE]

  if (nrow(strata) == 0) {
    ParallelLogger::logWarn("No valid '", columnPrefix, "' definitions after NA filter; skipping.")
    return(invisible(NULL))
  }

  tableName <- paste0("target_", targetId)
  colNames  <- paste0(columnPrefix, "_", strata$cohortId)

  # --- Step 1: add all flag columns (default 0) ------------------------------
  for (i in seq_len(nrow(strata))) {
    alterTableAddColumn(connection,
                        schemaName   = cohortDatabaseSchema,
                        tableName    = tableName,
                        columnName   = colNames[i],
                        type         = "INT",
                        defaultValue = 0)
  }

  # --- Step 2: one join, one pass — conditional aggregation over day_offset --
  caseTemplate <- "MAX(CASE WHEN c.covariate_id = @cohort_id
                                AND c.day_offset BETWEEN @window_start AND @window_end
                                THEN 1 ELSE 0 END) AS @col_name"

  caseClauses <- vapply(seq_len(nrow(strata)), function(i) {
    SqlRender::render(caseTemplate,
                      cohort_id    = strata$cohortId[i],
                      window_start = strata$windowStart[i],
                      window_end   = strata$windowEnd[i],
                      col_name     = colNames[i])
  }, character(1))

  setClauses <- vapply(colNames, function(cn) {
    SqlRender::render("@col_name = m.@col_name", col_name = cn)
  }, character(1))

  sql <- "
  UPDATE @cohort_database_schema.@target_table_name
  SET @set_clauses
  FROM (
    SELECT c.subject_id, @case_clauses
    FROM @cohort_database_schema.@covariate_table c
    GROUP BY c.subject_id
  ) m
  WHERE @cohort_database_schema.@target_table_name.subject_id = m.subject_id
    AND @cohort_database_schema.@target_table_name.target_id = @target_id;"

  renderedSql <- SqlRender::render(sql,
                                   cohort_database_schema = cohortDatabaseSchema,
                                   target_table_name      = tableName,
                                   covariate_table        = covariateTable,
                                   target_id              = targetId,
                                   set_clauses            = paste(setClauses, collapse = ", "),
                                   case_clauses           = paste(caseClauses, collapse = ", "))

  translatedSql <- SqlRender::translate(renderedSql, targetDialect = attr(connection, "dbms"))

  ParallelLogger::logInfo("Bulk-populating ", nrow(strata), " '", columnPrefix,
                          "' columns on ", cohortDatabaseSchema, ".", tableName,
                          " from ", covariateTable)

  DatabaseConnector::executeSql(connection, translatedSql)

  return(invisible(tableName))
}
# ==============================================================================
# wrapper for addBinaryColumnsToTargetTableBulk
# ==============================================================================
addBinaryColumnsWrapper <- function ( connection,
                                        cohortDatabaseSchema,
                                        targetId,
                                        binaryStrata,
                                        columnPrefix,
                                        covariatePrefix) 
{

  if (is.null(binaryStrata) || nrow(binaryStrata) == 0) {
    ParallelLogger::logWarn("No applicable '", columnPrefix,
                            "' definitions for target ", targetId, "; skipping.")
    return(invisible(NULL))
  }

  covariateTable <- paste0(covariatePrefix, "_target_", targetId)

  addBinaryColumnsToTargetTableBulk(
    connection           = connection,
    cohortDatabaseSchema = cohortDatabaseSchema,
    targetId             = targetId,
    strata               = binaryStrata,
    columnPrefix         = columnPrefix,
    covariateTable       = covariateTable)
}
# ==============================================================================
# Runner: binary strata
# ==============================================================================
addAllbinaryStrata <- function( connection,
                                        cohortDatabaseSchema,
                                        targetId,
                                        binaryStrata)
{
  addBinaryColumnsWrapper(
    connection           = connection,
    cohortDatabaseSchema = cohortDatabaseSchema,
    targetId             = targetId,
    binaryStrata         = binaryStrata,
    columnPrefix         = "strata",
    covariatePrefix      =  "baseline")   # covariate table = baseline_target_<id>
}


# ==============================================================================
# Runner: Baseline Covariates
# ==============================================================================
addAllBaselineCovariates <- function( connection,
                                        cohortDatabaseSchema,
                                        targetId,
                                        binaryStrata)
{
  addBinaryColumnsWrapper(
    connection           = connection,
    cohortDatabaseSchema = cohortDatabaseSchema,
    targetId             = targetId,
    binaryStrata         = binaryStrata,
    columnPrefix         = "baseline",
    covariatePrefix      =  "baseline")   # covariate table = baseline_target_<id>
}


# ==============================================================================
# Runner: Baseline Treatments
# ==============================================================================
addAllBaselineTreatments <- function( connection,
                                        cohortDatabaseSchema,
                                        targetId,
                                        binaryStrata )
{
  addBinaryColumnsWrapper(
    connection           = connection,
    cohortDatabaseSchema = cohortDatabaseSchema,
    targetId             = targetId,
    binaryStrata         = binaryStrata,
    columnPrefix         = "treatment",
    covariatePrefix      = "treatments")   # covariate table = baseline_target_<id>
}

# ============================================================================== 
# ==============================================================================
# Add measurements values and strata
# ==============================================================================
# ==============================================================================

 
# ==============================================================================
# get the sql for aggreation of the measurement values
# this can take min, max or closest to the index date
# closest prefers measurements prior to the index date, and then the closest after the index date
# ============================================================================== 

getMeasurementAggregationSql <- function(measurementCohortId,
                                        targetId,
                                        windowStart,
                                        windowEnd,
                                        aggregation,
                                        cohortDatabaseSchema) {

  aggregation <- tolower(aggregation)
  measurementTableName <- paste0("measurements_target_", targetId)

  if (aggregation %in% c("min", "max")) {

    sql <- "
      SELECT m.subject_id, @agg_fun(m.value_as_number) AS agg_value
      FROM @cohort_database_schema.@measurement_table_name m
      WHERE m.cohort_id = @cohort_id
        AND m.value_as_number IS NOT NULL
        AND m.day_offset BETWEEN @window_start AND @window_end
      GROUP BY m.subject_id"

    rendered <- SqlRender::render(
      sql,
      agg_fun = if (aggregation == "min") "MIN" else "MAX",
      cohort_database_schema = cohortDatabaseSchema,
      measurement_table_name = measurementTableName,
      cohort_id = as.integer(measurementCohortId),
      window_start = as.integer(windowStart),
      window_end = as.integer(windowEnd)
    )

    return(rendered)
  }

  if (aggregation == "closest") {
    sql <- "
      SELECT subject_id, value_as_number AS agg_value
      FROM (
        SELECT m.subject_id, m.value_as_number,
          ROW_NUMBER() OVER (
            PARTITION BY m.subject_id
            ORDER BY
              ABS(m.day_offset),                       -- closest to index
              CASE WHEN m.day_offset <= 0 THEN 0 ELSE 1 END, -- prefer on/before index
              m.measurement_date                       -- stable tie-break
          ) AS rn
        FROM @cohort_database_schema.@measurement_table_name m
        WHERE m.cohort_id = @cohort_id
          AND m.value_as_number IS NOT NULL
          AND m.day_offset BETWEEN @window_start AND @window_end
      ) ranked
      WHERE rn = 1"
    rendered <- SqlRender::render(
      sql,
      cohort_database_schema = cohortDatabaseSchema,
      measurement_table_name = measurementTableName,
      cohort_id = as.integer(measurementCohortId),
      window_start = as.integer(windowStart),
      window_end = as.integer(windowEnd)
    )

    return(rendered)
  }

  stop("Unknown aggregation '", aggregation, "'. Use 'min', 'max', or 'closest'.")
}
 # ==============================================================================
# addMeasurementValueColumn
# Adds and populates ONE measurement value column on target_<id> for a single
# measurement definition (one row). Column: <columnPrefix>_<cohortId>.
# Subjects without a qualifying baseline measurement remain NULL.
# Rows with missing/non-numeric cohortId, windowStart, or windowEnd are skipped
# with a warning rather than producing a malformed column name or broken SQL.
# ==============================================================================
addMeasurementValueColumn <- function(connection,
                                      cohortDatabaseSchema,
                                      targetId,
                                      measurementRow,
                                      columnPrefix = "meas",
                                      defaultAggregation = "closest") {

  cohortId    <- suppressWarnings(as.integer(measurementRow$cohortId))
  windowStart <- suppressWarnings(as.integer(measurementRow$windowStart))
  windowEnd   <- suppressWarnings(as.integer(measurementRow$windowEnd))

  if (is.na(cohortId) || is.na(windowStart) || is.na(windowEnd)) {
    ParallelLogger::logWarn("Skipping measurement row for target_", targetId,
                            ": invalid cohortId/windowStart/windowEnd (",
                            "cohortId=", measurementRow$cohortId,
                            ", windowStart=", measurementRow$windowStart,
                            ", windowEnd=", measurementRow$windowEnd, ")")
    return(invisible(NA_character_))
  }
  if (windowStart > windowEnd) {
    ParallelLogger::logWarn("Skipping measurement cohortId=", cohortId,
                            " on target_", targetId, ": windowStart > windowEnd")
    return(invisible(NA_character_))
  }

  aggregation <- tolower(measurementRow$aggregation)
  if (is.na(aggregation) || aggregation == "") {
    aggregation <- defaultAggregation
  }

  targetTableName <- paste0("target_", targetId)
  valueCol <- paste0(columnPrefix, "_", cohortId)

  # 1) Add the value column (NUMERIC; stays NULL if no qualifying measurement)
  alterTableAddColumn(
    connection,
    schemaName = cohortDatabaseSchema,
    tableName  = targetTableName,
    columnName = valueCol,
    type       = "NUMERIC",
    defaultValue = NULL
  )

  # 2) Aggregation subquery (rendered, dialect-translated once below)
  aggSubquery <- getMeasurementAggregationSql(
    measurementCohortId = cohortId,
    targetId            = targetId,
    windowStart         = windowStart,
    windowEnd           = windowEnd,
    aggregation         = aggregation,
    cohortDatabaseSchema = cohortDatabaseSchema
  )

  # 3) Update target table by subject_id (no need to filter on target_id in a per-target table)
  updateSql <- "
    UPDATE @cohort_database_schema.@target_table_name AS t
    SET @value_column = agg.agg_value
    FROM ( @agg_subquery ) AS agg
    WHERE t.subject_id = agg.subject_id;"

  renderedSql <- SqlRender::render(
    updateSql,
    cohort_database_schema = cohortDatabaseSchema,
    target_table_name      = targetTableName,
    value_column           = valueCol,
    agg_subquery           = aggSubquery)

  translatedSql <- SqlRender::translate(renderedSql, targetDialect = attr(connection, "dbms"))

  ParallelLogger::logInfo(paste0("Populated ", valueCol, " (", aggregation, ") on ", targetTableName))
  DatabaseConnector::executeSql(connection, translatedSql)

  invisible(valueCol)
}

# ==============================================================================
# addAllMeasurementValueColumns
# runner: add all measurements
# ==============================================================================
addAllMeasurementValueColumns <- function(connection,
                                          cohortDatabaseSchema,
                                          targetId,
                                          measurementsRef,
                                          columnPrefix = "meas",
                                          defaultAggregation = "closest") {

  createdCols <- character(nrow(measurementsRef))

  for (i in seq_len(nrow(measurementsRef))) {
    createdCols[i] <- addMeasurementValueColumn(
      connection = connection,
      cohortDatabaseSchema = cohortDatabaseSchema,
      targetId = targetId,
      measurementRow = measurementsRef[i, , drop = FALSE],
      columnPrefix = columnPrefix,
      defaultAggregation = defaultAggregation
    )
  }

  invisible(createdCols[!is.na(createdCols)])
}

 # ==============================================================================
# Bulk-Update Core Function for Outcomes
# Reads from the precomputed outcomes_target_<id> table (built by
# createCovariateTable(kind = "outcomes")), which already carries
# covariate_id (= outcome cohort_definition_id), subject_id, covariate_start_date,
# and day_offset relative to target_start_date — mirrors how
# addBinaryColumnsToTargetTableBulk reads baseline_target_<id>/treatments_target_<id>
# rather than re-joining the raw cohort table.
#
# Every subject gets outcome_days_<cid> populated:
#   - if the outcome occurs in [windowStart, windowEnd]: day_offset of that occurrence
#   - otherwise: days from target_start_date to end_of_continuous_observation
# Uses a LEFT JOIN so subjects with no matching outcome row at all still get a
# censored days value (an INNER JOIN would drop them from the update entirely).
# ==============================================================================
addAllOutcomes <-  function(connection,
                            cohortDatabaseSchema,
                            targetId,
                            outcomeDefinitions  # data.frame: cohortId, windowStart, windowEnd
                                              )
{
  required <- c("cohortId", "windowStart", "windowEnd")
  if (!all(required %in% colnames(outcomeDefinitions))) {
    stop("outcomeDefinitions must contain columns: ", paste(required, collapse = ", "))
  }

  outcomeDefinitions$cohortId    <- suppressWarnings(as.integer(outcomeDefinitions$cohortId))
  outcomeDefinitions$windowStart <- suppressWarnings(as.integer(outcomeDefinitions$windowStart))
  outcomeDefinitions$windowEnd   <- suppressWarnings(as.integer(outcomeDefinitions$windowEnd))

  bad <- is.na(outcomeDefinitions$cohortId) |
         is.na(outcomeDefinitions$windowStart) |
         is.na(outcomeDefinitions$windowEnd) |
         outcomeDefinitions$windowStart > outcomeDefinitions$windowEnd
  if (any(bad)) {
    ParallelLogger::logWarn("Dropping ", sum(bad), " invalid outcome definition row(s) ",
                            "(missing/non-numeric id or windowStart > windowEnd) for target_", targetId)
    outcomeDefinitions <- outcomeDefinitions[!bad, , drop = FALSE]
  }

  if (nrow(outcomeDefinitions) == 0) {
    ParallelLogger::logWarn("No valid outcome definitions for target_", targetId, "; skipping.")
    return(invisible(NULL))
  }

  tableName <- paste0("target_", targetId)
  outcomeTable <-  paste0("outcomes_target_", targetId)
  # Step 1: Add all columns first (prior/post default to 0, days stays NULL until matched)
  for (i in seq_len(nrow(outcomeDefinitions))) {
    cid <- outcomeDefinitions$cohortId[i]

    alterTableAddColumn(connection, schemaName = cohortDatabaseSchema, tableName = tableName,
                        columnName = paste0("prior_outcome_", cid), type = "INT", defaultValue = 0)

    alterTableAddColumn(connection, schemaName = cohortDatabaseSchema, tableName = tableName,
                        columnName = paste0("outcome_", cid), type = "INT", defaultValue = 0)

    alterTableAddColumn(connection, schemaName = cohortDatabaseSchema, tableName = tableName,
                        columnName = paste0("outcome_days_", cid), type = "INT")  # no default -> NULL
  }

  # Step 2: One join, one pass — compute prior flag, post flag, and days-to-event
  # (or days-to-censoring when no event) for every outcome, for every subject,
  # using the precomputed outcomes_target_<id> table's day_offset.
  priorTemplate <- "MAX(CASE WHEN o.covariate_id = @cohort_id AND o.day_offset < 0 THEN 1 ELSE 0 END) AS @prior_col"

  postTemplate <- "MAX(CASE WHEN o.covariate_id = @cohort_id AND o.day_offset BETWEEN @window_start AND @window_end THEN 1 ELSE 0 END) AS @post_col"

  # No matching outcome row in-window -> COALESCE falls through to censoring
  # (days to end_of_continuous_observation). With the LEFT JOIN, subjects with
  # no outcomes_target row at all for this cid have o.* = NULL on every group
  # row, so the CASE never matches and this always falls through correctly too.
  daysTemplate <- "MIN(COALESCE(
       CASE WHEN o.covariate_id = @cohort_id
              AND o.day_offset BETWEEN @window_start AND @window_end
            THEN o.day_offset
            ELSE NULL END,
       DATEDIFF(day, t.target_start_date, t.end_of_continuous_observation)
     )) AS @days_col"

  setTemplate <- "@prior_col = m.@prior_col, @post_col = m.@post_col, @days_col = m.@days_col"

  caseClauses <- vapply(seq_len(nrow(outcomeDefinitions)), function(i) {
    cid <- outcomeDefinitions$cohortId[i]
    ws  <- outcomeDefinitions$windowStart[i]
    we  <- outcomeDefinitions$windowEnd[i]

    priorCol <- paste0("prior_outcome_", cid)
    postCol  <- paste0("outcome_", cid)
    daysCol  <- paste0("outcome_days_", cid)

    priorExpr <- SqlRender::render(priorTemplate, cohort_id = cid, prior_col = priorCol)
    postExpr  <- SqlRender::render(postTemplate, cohort_id = cid,
                                   window_start = ws, window_end = we, post_col = postCol)
    daysExpr  <- SqlRender::render(daysTemplate, cohort_id = cid,
                                   window_start = ws, window_end = we, days_col = daysCol)

    paste(priorExpr, postExpr, daysExpr, sep = ",\n    ")
  }, character(1))

  setClauses <- vapply(seq_len(nrow(outcomeDefinitions)), function(i) {
    cid <- outcomeDefinitions$cohortId[i]
    SqlRender::render(setTemplate,
                      prior_col = paste0("prior_outcome_", cid),
                      post_col  = paste0("outcome_", cid),
                      days_col  = paste0("outcome_days_", cid))
  }, character(1))

  cohortIdList <- paste(outcomeDefinitions$cohortId, collapse = ", ")

  sql <- "
  UPDATE @cohort_database_schema.@target_table_name t
  SET @set_clauses
  FROM (
    SELECT t.subject_id, t.end_of_continuous_observation, t.target_start_date, @case_clauses
    FROM @cohort_database_schema.@target_table_name t
    LEFT JOIN @cohort_database_schema.@outcome_table o
      ON t.subject_id = o.subject_id
      AND o.covariate_id IN (@cohort_id_list)
    WHERE t.target_id = @target_id
    GROUP BY t.subject_id, t.target_start_date, t.end_of_continuous_observation
  ) m
  WHERE t.subject_id = m.subject_id
    AND t.target_id = @target_id;"

  renderedSql <- SqlRender::render(sql,
                                   cohort_database_schema = cohortDatabaseSchema,
                                   target_table_name = tableName,
                                   outcome_table = outcomeTable,
                                   target_id = targetId,
                                   cohort_id_list = cohortIdList,
                                   set_clauses = paste(setClauses, collapse = ",\n    "),
                                   case_clauses = paste(caseClauses, collapse = ",\n    "))

  translatedSql <- SqlRender::translate(renderedSql, targetDialect = attr(connection, "dbms"))

  ParallelLogger::logInfo("Bulk-populating prior/post/days outcomes for ", nrow(outcomeDefinitions),
                          " outcome cohorts in one pass on table ", cohortDatabaseSchema, ".", tableName,
                          " from ", outcomeTable)
  DatabaseConnector::executeSql(connection, translatedSql)
}
 
 
 # ==============================================================================
# addAllTargetVariables
# Top-level orchestrator: adds every strata, binary-flag, measurement, and
# outcome variable to target_<id> in one call. Each stage is optional (pass
# NULL to skip) and isolated with tryCatch so a failure in one stage is logged
# and does not prevent the remaining stages from running.
# Returns a named list of per-stage status ("skipped" | "success" | "failed: <msg>").
# ==============================================================================
addAllTargetVariables <- function(connection,
                                  cohortDatabaseSchema,
                                  targetId,
                                  ageStrata           = NULL,  # data.frame or NULL to skip
                                  indexStrata         = NULL,
                                  ageBins             = NULL,
                                  addGenderByAge      = TRUE,   # requires ageStrata to have run
                                  addbinaryStrata     = NULL,   # data.frame or NULL to skip
                                  baselineCovariates  = NULL,
                                  baselineTreatments  = NULL,
                                  measurementsRef     = NULL,
                                  outcomes            = NULL)
{
  status <- list()

  runStage <- function(name, expr) {
    ParallelLogger::logInfo("== Starting stage: ", name, " (target_", targetId, ") ==")
    tryCatch({
      expr
      ParallelLogger::logInfo("== Completed stage: ", name, " ==")
      status[[name]] <<- "success"
    }, error = function(e) {
      ParallelLogger::logError("Stage '", name, "' failed on target_", targetId, ": ", conditionMessage(e))
      status[[name]] <<- paste0("failed: ", conditionMessage(e))
    })
  }

  skipStage <- function(name) {
    ParallelLogger::logInfo("Skipping stage: ", name, " (no data provided)")
    status[[name]] <<- "skipped"
  }

  # --- Demographic range strata ----------------------------------------------
  if (!is.null(ageStrata) && nrow(ageStrata) > 0) {
    runStage("ageStrata", {
      addAgeStrataToTarget(
        connection           = connection,
        cohortDatabaseSchema = cohortDatabaseSchema,
        targetId             = targetId,
        strata               = ageStrata
      )
    })
  } else {
    skipStage("ageStrata")
  }

  if (!is.null(indexStrata) && nrow(indexStrata) > 0) {
    runStage("indexStrata", {
      addIndexStrataToTarget(
        connection           = connection,
        cohortDatabaseSchema = cohortDatabaseSchema,
        targetId             = targetId,
        strata               = indexStrata
      )
    })
  } else {
    skipStage("indexStrata")
  }

  if (!is.null(ageBins) && nrow(ageBins) > 0) {
    runStage("ageBins", {
      addAgeBinsToTarget(
        connection           = connection,
        cohortDatabaseSchema = cohortDatabaseSchema,
        targetId             = targetId,
        strata               = ageBins
      )
    })
  } else {
    skipStage("ageBins")
  }

  # GenderByAge depends on ageStrata having actually populated the ageStrata
  # column, so only attempt it if that stage was requested and succeeded.
  if (isTRUE(addGenderByAge)) {
    ageStrataStatus <- if (is.null(status[["ageStrata"]])) "not requested" else status[["ageStrata"]]
    if (identical(ageStrataStatus, "success")) {
      runStage("genderByAge", {
        addGenderAgeStrata(
          connection           = connection,
          cohortDatabaseSchema = cohortDatabaseSchema,
          targetId             = targetId,
          ageStrataColumn       = "ageStrata",
          strataColumnName      = "GenderByAge"
        )
      })
    } else {
      ParallelLogger::logWarn("Skipping genderByAge: ageStrata stage did not succeed (status: ",
                              ageStrataStatus, ")")
      status[["genderByAge"]] <- paste0("skipped: ageStrata not available (", ageStrataStatus, ")")
    }
  } else {
    skipStage("genderByAge")
  }

  # --- Binary flag columns ----------------------------------------------------
  if( !is.null(addbinaryStrata) && nrow(addbinaryStrata) > 0) {
    runStage("binaryStrata", {
      addAllbinaryStrata(
        connection           = connection,
        cohortDatabaseSchema = cohortDatabaseSchema,
        targetId             = targetId,
        binaryStrata         = addbinaryStrata
      )
    })
  } else {
    skipStage("binaryStrata")
  }



  if (!is.null(baselineCovariates) && nrow(baselineCovariates) > 0) {
    runStage("baselineCovariates", {
      addAllBaselineCovariates(
        connection           = connection,
        cohortDatabaseSchema = cohortDatabaseSchema,
        targetId             = targetId,
        binaryStrata         = baselineCovariates
      )
    })
  } else {
    skipStage("baselineCovariates")
  }

  if (!is.null(baselineTreatments) && nrow(baselineTreatments) > 0) {
    runStage("baselineTreatments", {
      addAllBaselineTreatments(
        connection           = connection,
        cohortDatabaseSchema = cohortDatabaseSchema,
        targetId             = targetId,
        binaryStrata         = baselineTreatments
      )
    })
  } else {
    skipStage("baselineTreatments")
  }

  # --- Measurement values ------------------------------------------------------
  if (!is.null(measurementsRef) && nrow(measurementsRef) > 0) {
    runStage("measurements", {
      addAllMeasurementValueColumns(
        connection           = connection,
        cohortDatabaseSchema = cohortDatabaseSchema,
        targetId             = targetId,
        measurementsRef      = measurementsRef,
        columnPrefix         = "meas",
        defaultAggregation   = "closest"
      )
    })
  } else {
    skipStage("measurements")
  }

  # --- Outcomes -----------------------------------------------------------------
  if (!is.null(outcomes) && nrow(outcomes) > 0) {
    runStage("outcomes", {
      addAllOutcomes(
        connection           = connection,
        cohortDatabaseSchema = cohortDatabaseSchema,
        targetId             = targetId,
        outcomeDefinitions   = outcomes
      )
    })
  } else {
    skipStage("outcomes")
  }

  ParallelLogger::logInfo("== All stages complete for target_", targetId, " ==")
  for (stageName in names(status)) {
    ParallelLogger::logInfo("  ", stageName, ": ", status[[stageName]])
  }

  invisible(status)
}