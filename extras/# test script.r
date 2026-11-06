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

createDataTables (connection,
                       cdmDatabaseSchema,
                       cohortDatabaseSchema,
                       cohortTable,
                        packageName='ENLHTNCKD') 
createTargetTable (connection, 
                              cdmDatabaseSchema,
                              cohortDatabaseSchema,
                              cohortTable,
                              targetId=9003) 
                              


addFirstEventFlags  (connection,
                                cohortDatabaseSchema,
                                cohortTable ,
                                targetId=9003,
                                conditionACohortId=9001, #CKD
                                conditionBCohortId=9002, #HTN
                                aFirstBaselineId=6002, #CKD first
                                bFirstBaselineId=6001,#HTN first 
                                sameDayBaselineId=6003 #same day
                                )
#==============================================================================


addStageAtBaseline  (connection,
                                cohortDatabaseSchema,
                                targetId=9003,
                                stageDefinitions,
                                stageColumnName='CKD_stage',
                                columnPrefix = "baseline")
# get the CKD stage 
ckd_stage=DatabaseConnector::querySql(conn,'select subject_id,ckd_stage from OHDSI.SCRATCH_ENLIGHTN_CKD.target_9003')
df=DatabaseConnector::querySql(conn,'select * from OHDSI.SCRATCH_ENLIGHTN_CKD.enlhtnckd_cohorts where subject_id=1214404187 and cohort_definition_id in (420,419,417,416,418,415,593)')
df=DatabaseConnector::querySql(conn,'select * from OHDSI.SCRATCH_ENLIGHTN_CKD.enlhtnckd_cohorts where subject_id=1214404187 and cohort_definition_id =9003')

addStageStrataColumns  (connection,
                                  cohortDatabaseSchema,
                                  targetId=9003,
                                  stageDefinitions,
                                  sourceColumn='CKD_stage',
                                  columnPrefix = "strata")

