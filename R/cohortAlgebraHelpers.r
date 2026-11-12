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
                                cohortTable,
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
  
 