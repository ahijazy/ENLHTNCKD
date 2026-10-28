# ==============================================================================
# Target table: pulls the target table from the cohort and adds demographics
# ==============================================================================
createTargetTable <- function(connection, 
                              cdmDatabaseSchema,
                              cohortDatabaseSchema,
                              cohortTable,
                              targetId) 
{
  sql <- "
  DROP TABLE IF EXISTS @cohort_database_schema.@target_table_name;
  
  CREATE TABLE @cohort_database_schema.@target_table_name AS
  WITH target_temp AS (
      SELECT
          c.cohort_definition_id AS target_id,
          c.subject_id,
          c.cohort_start_date AS target_start_date,
          c.cohort_end_date AS target_end_date,
          p.gender_concept_id,
          p.race_concept_id,        
          p.ethnicity_concept_id,   
          YEAR(c.cohort_start_date) - p.year_of_birth AS age
      FROM @cohort_database_schema.@cohort_table c
      JOIN @cdm_database_schema.person p ON c.subject_id = p.person_id
      WHERE c.cohort_definition_id = @target_id
  ),
  continuous_observation AS (
      SELECT
          tt.subject_id,
          tt.target_id,
          tt.target_start_date,
          tt.target_end_date,
          tt.age,
          tt.gender_concept_id,
          tt.race_concept_id,        
          tt.ethnicity_concept_id,   
          op.observation_period_start_date AS start_of_continuous_observation,
          op.observation_period_end_date AS end_of_continuous_observation,
          ROW_NUMBER() OVER (
              PARTITION BY tt.subject_id
              ORDER BY op.observation_period_start_date DESC
          ) AS rn
      FROM target_temp tt
      JOIN @cdm_database_schema.observation_period op
        ON tt.subject_id = op.person_id
       AND op.observation_period_start_date <= tt.target_start_date
       AND op.observation_period_end_date >= tt.target_start_date
  ),
  all_observation AS (
      SELECT
          op.person_id AS subject_id,
          MAX(op.observation_period_end_date) AS end_of_all_observation
      FROM @cdm_database_schema.observation_period op
      INNER JOIN target_temp tt ON op.person_id = tt.subject_id
      GROUP BY op.person_id
  )
  SELECT
      co.target_id,
      co.subject_id,
      co.target_start_date,
      co.target_end_date,
      co.age,
      YEAR(co.target_start_date) as index_year,
      co.gender_concept_id,
      co.race_concept_id,       
      co.ethnicity_concept_id,   
      co.start_of_continuous_observation,
      co.end_of_continuous_observation,
      ao.end_of_all_observation
  FROM continuous_observation co
  LEFT JOIN all_observation ao ON co.subject_id = ao.subject_id
  WHERE co.rn = 1;
  "

  targetTableName <- paste0("target_", targetId)

  renderedSql <- SqlRender::render(sql, 
                                   cohort_database_schema = cohortDatabaseSchema,
                                   cdm_database_schema = cdmDatabaseSchema,
                                   cohort_table = cohortTable,
                                   target_table_name = targetTableName,
                                   target_id = targetId)
  
  translatedSql <- SqlRender::translate(renderedSql, targetDialect = attr(connection, "dbms"))
  
  ParallelLogger::logInfo("Creating target table with demographics for cohort ", targetId)
  DatabaseConnector::executeSql(connection, translatedSql)
  ParallelLogger::logInfo("Successfully created target table: ", targetTableName)
  return(invisible(targetTableName))
}


# ==============================================================================
# upload the resolved concept set table to the schema
# ==============================================================================
createConceptSetTable <- function(connection,
                              cohortDatabaseSchema,
                              resolvedConceptSetsDF) 
{
ParallelLogger::logInfo("Uploading resolved concept sets to ", cohortDatabaseSchema, ".resolvedConceptSets")
DatabaseConnector::insertTable(connection = connection,
                              tableName = "resolvedConceptSets",
                              data = resolvedConceptSetsDF,
                              databaseSchema = cohortDatabaseSchema,
                              tempTable = FALSE,
                              createTable = TRUE,
                              dropTableIfExists = TRUE)
}


# ==============================================================================
# MeasurementTable:  comes from the resolved concept sets 
# the function gets the measurement table from the cdm schema for patient ids who are in the target table
# and concept ids who are in the resolved concept set table
# ==============================================================================
createMeasurementTable <- function(connection,
                                   cdmDatabaseSchema,
                                   cohortDatabaseSchema,
                                   targetId,
                                   packageName) 
{
    relevantIds <- NULL
    if(system.file("settings","measurements.csv", package = packageName)=="")
   {
     ParallelLogger::logWarn("No measurements.csv file found in package settings; skipping measurement table creation.")
    } else
    {
  measurementIds <- fetchApplicableCohortIds(targetIdToCheck = targetId,
                                             table = "measurements",
                                             packageName = packageName)
    relevantIds <- unique(union(measurementIds, relevantIds))
    }
    if(system.file("settings","measurementStrata.csv", package = packageName)=="")
    {
      ParallelLogger::logWarn("No measurementStrata.csv file found in package settings; skipping measurement strata addition.")
    } else
    {
    measurementStrataIds <- fetchApplicableCohortIds(targetIdToCheck = targetId,
                                                    table = "measurementStrata",
                                                    packageName = packageName)
    relevantIds <- unique(union(measurementStrataIds, relevantIds))
    }


  if (length(relevantIds) == 0) {
    ParallelLogger::logWarn("No relevant measurement concept sets for target ", targetId, "; skipping.")
    return(invisible(NULL))
  }

  sql <- "
DROP TABLE IF EXISTS @cohort_database_schema.measurements_target_@target_id;

CREATE TABLE @cohort_database_schema.measurements_target_@target_id AS
SELECT rcs.cohortId AS cohort_id,
       m.measurement_id,
       m.person_id AS subject_id,
       m.measurement_concept_id,
       m.measurement_date,
       m.operator_concept_id,
       m.value_as_number,
       m.value_as_concept_id,
       m.unit_concept_id,
       (m.measurement_date - t.target_start_date) AS day_offset
FROM @cdm_database_schema.measurement m
JOIN @cohort_database_schema.resolvedConceptSets rcs
  ON m.measurement_concept_id = rcs.conceptId
JOIN @cohort_database_schema.target_@target_id t
  ON m.person_id = t.subject_id
WHERE rcs.cohortId IN (@relevant_ids);
"

  renderedSql <- SqlRender::render(sql,
                                   cdm_database_schema = cdmDatabaseSchema,
                                   cohort_database_schema = cohortDatabaseSchema,
                                   target_id = targetId,
                                   relevant_ids = relevantIds)

  translatedSql <- SqlRender::translate(renderedSql, targetDialect = "postgresql")

  ParallelLogger::logInfo("Creating measurement table for target ", targetId)
  DatabaseConnector::executeSql(connection, translatedSql)
  ParallelLogger::logInfo("Successfully created measurement table: measurements_target_", targetId)

  invisible(TRUE)
}
# ==============================================================================
 # create Baseline occurunce table
 # this selects the relevant baseline covariates for the target cohort 
 # and creates a table with subject_id, covariate_id, target_id, target_start_date, covariate_start_date,
 # days_from_covariate_to_target
 # NOTE TO SELf -- negative = covariate occurred BEFORE index
# ==============================================================================
createCovariateTable  <- function(connection,
                               cdmDatabaseSchema,
                               cohortDatabaseSchema,
                               cohortTable,
                               targetId,
                               kind,                 # "baseline" | "treatments" | "outcomes"
                               packageName) 
 {
  # get the relevan ids for the baseline covariates
  relevantIds=fetchApplicableCohortIds(targetIdToCheck = targetId,
                                           table = kind,
                                           packageName = packageName)
  if (length(relevantIds) == 0) {
  ParallelLogger::logWarn("No ", kind, " for target ", targetId, "; skipping.")
  return(invisible(NULL))
  }
  if (length(targetId) != 1 || is.na(targetId)) {
    stop("`targetId` must be a single, non-NA value.")
  }

  tableName <- paste0(kind, "_target_", targetId)
  sql="
  DROP TABLE IF EXISTS @cohort_database_schema.@tableName;
  CREATE TABLE @cohort_database_schema.@tableName AS
  SELECT t.target_id,
         b.cohort_definition_id as covariate_id,
         b.subject_id,
         t.target_start_date,
         b.cohort_start_date as covariate_start_date,
         b.cohort_end_date as covariate_end_date,
         DATEDIFF(day, t.target_start_date, b.cohort_start_date) as day_offset
  FROM @cohort_database_schema.@cohort_table b
  JOIN @cohort_database_schema.target_@target_id t
    ON b.subject_id = t.subject_id
  WHERE b.cohort_definition_id in (@relevant_ids);"  
  sql_rendered <- SqlRender::render(sql,
                                   cohort_database_schema = cohortDatabaseSchema,
                                   tableName = tableName,
                                   cohort_table = cohortTable,
                                   target_id = targetId,
                                   relevant_ids = paste(relevantIds, collapse = ","))
  sql_translated <- SqlRender::translate(sql_rendered, targetDialect = attr(connection, "dbms"))
  ParallelLogger::logInfo("Creating ", kind, " table for target ", targetId)
  DatabaseConnector::executeSql(connection, sql_translated)
  ParallelLogger::logInfo("Successfully created ", kind, " table: ", kind, "_target_", targetId)
 }
# ==============================================================================
 #Runner: Build baseline, treatments and outcome tables for a given targetId
# ==============================================================================
createCovariateTables<- function(connection,
                               cdmDatabaseSchema,
                               cohortDatabaseSchema,
                               cohortTable,
                               targetId,
                               packageName) 
 {
  createCovariateTable(connection, cdmDatabaseSchema, cohortDatabaseSchema, cohortTable, targetId, kind = "baseline", packageName)
  createCovariateTable(connection, cdmDatabaseSchema, cohortDatabaseSchema, cohortTable, targetId, kind = "treatments", packageName)
  createCovariateTable(connection, cdmDatabaseSchema, cohortDatabaseSchema, cohortTable, targetId, kind = "outcomes", packageName)
 }

# ==============================================================================
 #Runner: build targets and covariate tables for a all targets in the target table
# ==============================================================================
createDataTables <- function(connection,
                       cdmDatabaseSchema,
                       cohortDatabaseSchema,
                       cohortTable,
                       packageName) 
 {
  resolvedConceptSetsDF= read.csv(system.file( "conceptsets","resolvedConceptSets.csv", package = packageName), stringsAsFactors = FALSE)
  createConceptSetTable (connection, cohortDatabaseSchema, resolvedConceptSetsDF =resolvedConceptSetsDF)
   # get the target ids from the target table
   targetPath <- file.path(system.file("settings", package = packageName), "targets.csv")
   targets=read.csv(targetPath, stringsAsFactors = FALSE)
   targetIds=unique(targets$cohortId)
  for (targetId in targetIds) {
    createTargetTable(connection, cdmDatabaseSchema, cohortDatabaseSchema, cohortTable, targetId)
    createCovariateTables(connection, cdmDatabaseSchema, cohortDatabaseSchema, cohortTable, targetId, packageName)
    createMeasurementTable(connection, cdmDatabaseSchema, cohortDatabaseSchema, targetId, packageName)
  }
 }
