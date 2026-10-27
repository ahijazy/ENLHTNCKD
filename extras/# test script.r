# test script
# source connection details
source('c:/connection/ods_synpuf/connection.txt')
# source webapi connection
#source('c:/connection/webapi_connection.r')

# first create cohortstrataref
targets=read.csv('inst/settings/targets.csv')
strata       = read.csv('inst/settings/strata.csv')
ageStrata    = read.csv('inst/settings/ageStrata.csv')
ageBins      = read.csv('inst/settings/ageBins.csv')
indexStrata  = read.csv('inst/settings/indexStrata.csv')
measurements = read.csv('inst/settings/measurements.csv')
measurementStrata =read.csv('inst/settings/measurementStrata.csv')
strataRef=read.csv('inst/settings/cohortStrataRef.csv')
baseline =read.csv('inst/settings/baseline.csv')
baselineIntervals =read.csv('inst/settings/baselineIntervals.csv')
treatments =read.csv('inst/settings/treatments.csv')
# check the insertion functions
source('inst/settings/scripts/insertCohortsAndConceptSets.r')
#insertCohortsandConceptsInPackage(instFolder = 'inst', baseUrl , packageName = 'ENLHTNCKD')

#create binary strata first
createBinaryStrataRef (targets,strata)
createGenderBasedStrata(targets)
stratifyGenderByAge(targets, ageStrata)
# now create all the strata references
 

createAllStrataRef(targets=targets,
                               strata       = strata,
                               ageStrata    = ageStrata,
                               indexStrata  = indexStrata,
                               measurementStrata = measurementStrata,
                               stratifyByCohort      = TRUE,
                               stratifyByGender      = TRUE,
                               stratifyByAge         = TRUE,
                               statifyAgebyGender    = TRUE,
                               stratifyByIndex       = TRUE,
                               stratifyByMeasurement = TRUE
                               )
checkCohortStrataRef('cohortStrataRef')
# then test the upload of the tables
 
createTargetTable(connection, 
                              cdmDatabaseSchema,
                              cohortDatabaseSchema,
                              cohortTable,
                              targetId=1222)
 

addAllBinaryStrata (connection,
                               cohortDatabaseSchema,
                               cohortTable  ,
                               targetId=1222)
 
 
addAgeStrataToTarget(connection,
                            cohortDatabaseSchema,
                            targetId=1222)
addindexStrataToTarget(connection,
                            cohortDatabaseSchema,
                            targetId=1222)

createMeasurementTable(connection,
                               cdmDatabaseSchema,
                               cohortDatabaseSchema, 
                               targetId=1222)
addIncidentPopulationtoCohortTable(connection, 
                                   cohortDatabaseSchema,
                                   cohortTable,
                                   incidentCohortId=9001,
                                   CKDCohortId=1222,
                                   HTNCohortId=1223,
                                   indexDate='1/1/2000')
addPrevalentPopulationtoCohortTable(connection, 
                                   cohortDatabaseSchema,
                                   cohortTable,
                                   prevalentCohortId=9002,
                                   CKDCohortId=1222,
                                   HTNCohortId=1223,
                                   indexDate='1/1/2010')                      
 
# see the measurement table
df_HTN=DatabaseConnector::querySql(connection, paste0("select * from ",cohortDatabaseSchema,".",cohortTable," where cohort_definition_id=1222"))
df_CKD=DatabaseConnector::querySql(connection, paste0("select * from ",cohortDatabaseSchema,".",cohortTable," where cohort_definition_id=1223"))
df_incident=DatabaseConnector::querySql(connection, paste0("select * from ",cohortDatabaseSchema,".",cohortTable," where cohort_definition_id=9001"))
df_prevalent=DatabaseConnector::querySql(connection, paste0("select * from ",cohortDatabaseSchema,".",cohortTable," where cohort_definition_id=9002"))

createTargetTable    (connection, 
                              cdmDatabaseSchema,
                              cohortDatabaseSchema,
                              cohortTable,
                              targetId=9001) 


addAllBinaryStrata(connection,
                               cohortDatabaseSchema,
                               cohortTable ,
                               targetId=9001)
addAllbaselineCovariates(connection,
                               cohortDatabaseSchema,
                               cohortTable ,
                               targetId=9001)
addAllBaselineTreatments(connection,
                               cohortDatabaseSchema,
                               cohortTable ,
                               targetId=9001)
addAllOutcomes  (connection,
                           cohortDatabaseSchema,
                           cohortTable  ,
                           targetId=9001) 


source('c:/connection/ods_synpuf/connection.txt')
getTotalCohortCounts(connection,
                cohortDatabaseSchema,
                cohortTable,
                outputFolder)

getBinaryStrataCounts(connection,
                cohortDatabaseSchema,
                cohortTable,
                targetId=9001,
                strataRef)

getAgeStrataCounts (connection,
                cohortDatabaseSchema,
                cohortTable,
                targetId=9001,
                strataRef)

getIndexStrataCounts(connection,
                cohortDatabaseSchema,
                cohortTable,
                targetId=9001,
                strataRef)

getGenderAgeStrataCounts(connection,
                cohortDatabaseSchema,
                cohortTable,
                targetId=9001,
                strataRef)
df_prevalent=DatabaseConnector::querySql(connection, paste0("select * from ",cohortDatabaseSchema,".target_9001"))

getAllStrataCountsForTarget  (connection,
                cohortDatabaseSchema,
                cohortTable,
                targetId=9001,
                strataRef)

df=querySql(connection, paste0("select * from ",cohortDatabaseSchema,".target_9001"))


# test for cohort 9001
testData <- 
 getBaselineCharacteristics(connection,
                cohortDatabaseSchema,
                cohortTable = "cohort",
                targetId=9001,
                baseline,           # data.frame: cohortId, cohortName
                baselineIntervals,  # data.frame: windowStart, windowEnd
                strataRef,
                occurrenceType = c("all", "earliest", "latest"),
                bandwidth = "nrd0",
                n = 512)

                # test for cohort 9001
testData <- 
 getBaselineCharacteristics(connection,
                cohortDatabaseSchema,
                cohortTable = "cohort",
                targetId=9001,
                baseline,           # data.frame: cohortId, cohortName
                baselineIntervals,  # data.frame: windowStart, windowEnd
                strataRef,
                occurrenceType = c("all", "earliest", "latest"),
                bandwidth = "nrd0",
                n = 512)

# test
getStrataCohortId(strataRef, targetId = 1223, strataId = 8507 ,strataType = "gender")
    source('c:/connection/ods_synpuf/connection.txt')

createTargetTable    (connection, 
                              cdmDatabaseSchema,
                              cohortDatabaseSchema,
                              cohortTable,
                              targetId=9001) 
createConceptSetTable(  connection,
                              cdmDatabaseSchema,
                              cohortDatabaseSchema,
                              resolvedConceptSetsDF) 
createMeasurementTable(connection,
                               cdmDatabaseSchema,
                               cohortDatabaseSchema, 
                               targetId=9001,
                               packageName = "ENLHTNCKD")
 
 createCovariateTables(connection,
                               cdmDatabaseSchema,
                               cohortDatabaseSchema, 
                               cohortTable,
                               targetId=9001,
                               packageName = "ENLHTNCKD")
addAgeStrataToTarget (connection,
                                 cohortDatabaseSchema,
                                 targetId=9001,
                                 strata=ageStrata) 
addIndexStrataToTarget (connection,
                                   cohortDatabaseSchema,
                                   targetId=9001,
                                   strata=indexStrata) 
addAgeBinsToTarget (connection,
                               cohortDatabaseSchema,
                               targetId=9001,
                               strata=ageBins) 

addGenderAgeStrata (connection,
                               cohortDatabaseSchema,
                               targetId=9001,
                               ageStrataColumn  = "ageStrata",
                               strataColumnName = "GenderByAge")
addAllBaselineCovariates ( connection,
                                        cohortDatabaseSchema,
                                        targetId=9001,
                                        binaryStrata=baseline)
addAllBaselineTreatments ( connection,
                                        cohortDatabaseSchema,
                                        targetId=9001,
                                        binaryStrata=treatments)
addAllOutcomes    (connection,
                           cohortDatabaseSchema,
                           cohortTable,
                           targetId=9001,
                           outcomes=outcomes) 
addAllTargetVariables (connection,
                                  cohortDatabaseSchema,
                                  targetId=9001,
                                  ageStrata           = ageStrata,  # data.frame or NULL to skip
                                  indexStrata         = indexStrata,
                                  ageBins             = ageBins,
                                  addGenderByAge      = TRUE,   # requires ageStrata to have run
                                  addbinaryStrata     = strata,  # data.frame or NULL to skip
                                  baselineCovariates  = baseline,
                                  baselineTreatments  = treatments,
                                  measurementsRef     = measurements,
                                  outcomes            = outcomes)

                                  # first create cohortstrataref
targets=read.csv('inst/settings/targets.csv')
strata       = read.csv('inst/settings/strata.csv')
ageStrata    = read.csv('inst/settings/ageStrata.csv')
ageBins      = read.csv('inst/settings/ageBins.csv')
indexStrata  = read.csv('inst/settings/indexStrata.csv')
measurements = read.csv('inst/settings/measurements.csv')
measurementStrata =read.csv('inst/settings/measurementStrata.csv')
strataRef=read.csv('inst/settings/cohortStrataRef.csv')
baseline =read.csv('inst/settings/baseline.csv')
baselineIntervals =read.csv('inst/settings/baselineIntervals.csv')
treatments =read.csv('inst/settings/treatments.csv')
outcomes =read.csv('inst/settings/outcomes.csv')