

createCohorts <- function(connection,
                           cdmDatabaseSchema,
                           vocabularyDatabaseSchema = cdmDatabaseSchema,
                           cohortDatabaseSchema,
                           cohortTable,
                           oracleTempSchema = NULL,
                           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                           packageName = "ENLHTNCKD",
                           outputFolder)
  
  {
    
  if (!is.null(oracleTempSchema) && oracleTempSchema != "") {
    warning("The 'oracleTempSchema' argument is deprecated. Use 'tempEmulationSchema' instead.")
    tempEmulationSchema <- oracleTempSchema
  }
  
  # Create study cohort table structure:
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "CreateCohortTable.sql",
                                           packageName = packageName,
                                           dbms = attr(connection, "dbms"),
                                           tempEmulationSchema = tempEmulationSchema,
                                           cohort_database_schema = cohortDatabaseSchema,
                                           cohort_table = cohortTable)
  DatabaseConnector::executeSql(connection, sql, progressBar = FALSE, reportOverallTime = FALSE)
  
  

  # Instantiate cohorts:
  pathToCsv <- system.file("settings", "CohortsToCreate.csv", package = packageName)
   cohortsToCreate <- readr::read_csv(pathToCsv, col_types = readr::cols())
  for (i in 1:nrow(cohortsToCreate)) {
    writeLines(paste("Creating cohort:", cohortsToCreate$cohortName[i]))
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename = paste0(cohortsToCreate$cohortId [i], ".sql"),
                                             packageName = packageName,
                                             dbms = attr(connection, "dbms"),
                                             tempEmulationSchema = tempEmulationSchema,
                                             cdm_database_schema = cdmDatabaseSchema,
                                             vocabulary_database_schema = vocabularyDatabaseSchema,
                                             target_database_schema = cohortDatabaseSchema,
                                             target_cohort_table = cohortTable,
                                             target_cohort_id = cohortsToCreate$cohortId[i])
    DatabaseConnector::executeSql(connection, sql,progressBar = TRUE, reportOverallTime = TRUE)
  }
  
}
