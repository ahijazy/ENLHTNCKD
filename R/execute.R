#execute Function
executeStudy=function(cdmDatabaseSchema,
                     vocabularyDatabaseSchema = cdmDatabaseSchema,
                     cohortDatabaseSchema,
                     cohortTable,
                     oracleTempSchema = NULL,
                     tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                     outputFolder)
{
    .createCohorts (connection,
                           cdmDatabaseSchema,
                           vocabularyDatabaseSchema = cdmDatabaseSchema,
                           cohortDatabaseSchema,
                           cohortTable,
                           oracleTempSchema = NULL,
                           tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                           outputFolder)
    
}