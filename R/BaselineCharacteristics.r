library(data.table)

getBaselineCharacteristics <- function(connection,
                                       cohortDatabaseSchema,
                                       cohortDataStrataRow,
                                       characteristicsTableKind) {

  # characteristicsTableKind is either baseline, treatment, measurement

  targetId <- as.integer(cohortDataStrataRow$targetCohortId)
  targetTableName <- paste0("target_", targetId)

  characteristicsTableName <- paste0( characteristicsTableKind, "_", "target_", targetId)
  whereClause <- getStrataWhereClause(cohortDataStrataRow)
  characteristicsCohortId <- as.integer(cohortDataStrataRow$strataCohortId)
  ws <- as.integer(cohortDataStrataRow$windowStart)
  we <- as.integer(cohortDataStrataRow$windowEnd)

  sql <- "
    SELECT
      subject_id,
      MIN(day_offset) AS day_offset,
      FLOOR(MIN(day_offset) / 30.0) AS month_bin
    FROM @cohort_database_schema.@characteristics_table
    WHERE covariate_id = @characteristics_cohort_id
      AND day_offset >= @window_start
      AND day_offset <= @window_end
      AND subject_id IN (
        SELECT subject_id
        FROM @cohort_database_schema.@target_table_name
        WHERE @strata_condition
      )
    GROUP BY subject_id
  "

  renderedSql <- SqlRender::render(
    sql,
    cohort_database_schema = cohortDatabaseSchema,
    characteristics_table = characteristicsTableName,
    characteristics_cohort_id = characteristicsCohortId,
    target_table_name = targetTableName,
    strata_condition = whereClause,
    window_start = ws,
    window_end = we
  )

  translatedSql <- SqlRender::translate(
    renderedSql,
    targetDialect = attr(connection, "dbms")
  )

  result <- DatabaseConnector::querySql(
    connection,
    translatedSql
  )

  if (nrow(result) == 0) {
    ParallelLogger::logInfo(
      "No baseline characteristics found for cohortId=",
      targetId,
      " and strataCohortId=",
      characteristicsCohortId
    )
    return(data.frame())
  }

  data.table::setDT(result)
  data.table::setnames(result, tolower(names(result)))

  # Frequency of each month bin
  bins <- result[, .(frequency = .N), by = month_bin][order(month_bin)]

  # Quantiles of the day offset
  quantiles <- stats::quantile(
    result$day_offset,
    probs = c(0, 0.25, 0.5, 0.75, 1),
    na.rm = TRUE
  )

  # Return all characteristics as a named list
  list(
    cohortId = cohortDataStrataRow$cohortId,
    bins = bins,
    quantiles = quantiles,
    totalCount = nrow(result),
    mean = mean(result$day_offset, na.rm = TRUE),
    sd = sd(result$day_offset, na.rm = TRUE)
  )
}

df <- getBaselineCharacteristics(
  connection = connection,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortDataStrataRow = cohortdatastrataRow,
  characteristicsTableKind = "baseline"
)

getDemographics <- function(connection,
                            cohortDatabaseSchema,
                            cohortDataStrataRow) {

  # demographics are in the target table
  sql <- "
    SELECT age, index_year, gender_concept_id, race_concept_id,
           ethnicity_concept_id, agebins,
           start_of_continuous_observation, end_of_continuous_observation
    FROM @cohort_database_schema.@target_table_name
    WHERE @strata_condition
  "

  df <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    cohort_database_schema = cohortDatabaseSchema,
    target_table_name = paste0("target_", cohortDataStrataRow$targetCohortId),
    strata_condition = getStrataWhereClause(cohortDataStrataRow)
  )

  if (nrow(df) == 0) {
    ParallelLogger::logInfo(
      "No demographics found for cohortId=",
      cohortDataStrataRow$cohortId,
      " and strataCohortId=",
      cohortDataStrataRow$strataCohortId
    )
    return(data.frame())
  }

  data.table::setDT(df)
  data.table::setnames(df, tolower(names(df)))

  # helper: frequency table of one column, sorted by that column
  freqBy <- function(dt, col) {
    dt[, .(frequency = .N), by = col][order(get(col))]
  }

  list(
    cohortId = cohortDataStrataRow$cohortId,
    mean_age = mean(df$age, na.rm = TRUE),
    sd_age = sd(df$age, na.rm = TRUE),
    age_quantiles = stats::quantile(
      df$age,
      probs = c(0, 0.25, 0.5, 0.75, 1),
      na.rm = TRUE
    ),
    age_distribution = freqBy(df, "agebins"),
    gender_distribution = freqBy(df, "gender_concept_id"),
    race_distribution = freqBy(df, "race_concept_id"),
    ethnicity_distribution = freqBy(df, "ethnicity_concept_id")
  )
}

df <- getDemographics(
  connection = connection,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortDataStrataRow = cohortdatastrataRow
)