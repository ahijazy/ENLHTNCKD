# Joins a target table with cohort events (baseline, outcome, or treatment).
#
# firstEventOnly = TRUE  -> LEFT JOIN; one row per (subject, cohortId).
#                           Subjects with no event in the window return NULL
#                           covariate columns (= censored / absent).
#                           Use for: survival, time-to-event, baseline flags.
#
# firstEventOnly = FALSE -> INNER JOIN; one row per event occurrence with
#                           event_seq numbering recurrences.
#                           Subjects with no events are excluded.
#                           Use for: incidence rate, MCF (recurrent events).
#
# windowStart / windowEnd: days relative to target_start_date (e.g. -365 to 0
# for baseline lookback, 1 to 99999 for post-index outcomes).
getTargetCohortData <- function(connection,
                                cohortDatabaseSchema,
                                cohortTable,
                                targetId,
                                cohortIds,
                                windowStart,
                                windowEnd,
                                firstEventOnly = TRUE) {

  if (firstEventOnly) {
    sql <- "
    SELECT
      t.subject_id,
      t.target_id,
      t.target_start_date,
      t.target_end_date,
      t.age,
      t.index_year,
      t.gender_concept_id,
      t.end_of_continuous_observation,
      t.end_of_all_observation,
      c.cohort_definition_id AS covariate_cohort_id,
      c.cohort_start_date    AS covariate_start_date,
      c.cohort_end_date      AS covariate_end_date,
      DATEDIFF(day, t.target_start_date, c.cohort_start_date) AS days_from_index
    FROM @cohort_database_schema.target_@target_id t
    LEFT JOIN (
      SELECT
        subject_id,
        cohort_definition_id,
        cohort_start_date,
        cohort_end_date,
        ROW_NUMBER() OVER (
          PARTITION BY subject_id, cohort_definition_id
          ORDER BY cohort_start_date
        ) AS rn
      FROM @cohort_database_schema.@cohort_table
      WHERE cohort_definition_id IN (@cohort_ids)
    ) c
      ON  t.subject_id = c.subject_id
      AND c.rn = 1
      AND DATEDIFF(day, t.target_start_date, c.cohort_start_date)
          BETWEEN @window_start AND @window_end
    "
  } else {
    sql <- "
    SELECT
      t.subject_id,
      t.target_id,
      t.target_start_date,
      t.target_end_date,
      t.age,
      t.index_year,
      t.gender_concept_id,
      t.end_of_continuous_observation,
      t.end_of_all_observation,
      c.cohort_definition_id AS covariate_cohort_id,
      c.cohort_start_date    AS covariate_start_date,
      c.cohort_end_date      AS covariate_end_date,
      DATEDIFF(day, t.target_start_date, c.cohort_start_date) AS days_from_index,
      ROW_NUMBER() OVER (
        PARTITION BY t.subject_id, c.cohort_definition_id
        ORDER BY c.cohort_start_date
      ) AS event_seq
    FROM @cohort_database_schema.target_@target_id t
    JOIN @cohort_database_schema.@cohort_table c
      ON  t.subject_id = c.subject_id
      AND c.cohort_definition_id IN (@cohort_ids)
      AND DATEDIFF(day, t.target_start_date, c.cohort_start_date)
          BETWEEN @window_start AND @window_end
    "
  }

  renderedSql <- SqlRender::render(sql,
                                   cohort_database_schema = cohortDatabaseSchema,
                                   cohort_table           = cohortTable,
                                   target_id              = targetId,
                                   cohort_ids             = cohortIds,
                                   window_start           = windowStart,
                                   window_end             = windowEnd)

  translatedSql <- SqlRender::translate(renderedSql,
                                        targetDialect = attr(connection, "dbms"))

  ParallelLogger::logInfo("Fetching cohort data for target ", targetId,
                          " | cohorts: ", paste(cohortIds, collapse = ", "),
                          " | window: [", windowStart, ", ", windowEnd, "]",
                          " | firstEventOnly: ", firstEventOnly)

  DatabaseConnector::querySql(connection, translatedSql, snakeCaseToCamelCase = TRUE)
}
