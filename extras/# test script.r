createCohorts (connection=conn,
                           cdmDatabaseSchema=cdmDatabaseSchema,
                           vocabularyDatabaseSchema = cdmDatabaseSchema,
                           cohortDatabaseSchema=cohortDatabaseSchema,
                           cohortTable=cohortTable,
                           oracleTempSchema = NULL,
                           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                             outputFolder='C:/git_clone/output',
                           packageName='ENLHTNCKD')

CreateTargetPopulations(connection, 
                        cohortDatabaseSchema,
                        cohortTable) 
                        