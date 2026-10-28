library(dplyr)
library(ROhdsiWebApi)
library(RJSONIO)
# ==============================================================================
 # read all cohorts in the csv files and add a role column to indicate which file they came from
# ==============================================================================
 readAllCohorts= function(settingsFolder) {
  files <- c("targets", "strata", "outcomes", "treatments", "baseline")
  dataList <- list()
  
  for (f in files) {
  filePath <- paste0(settingsFolder, '/', f, '.csv')
    if (file.exists(filePath)) {
      dataList[[f]] <- read.csv(filePath, stringsAsFactors = FALSE)
      dataList[[f]]$role <- f
    } else {
      warning("File ", f, ".csv not found in settings folder; skipping.")
    }
  }
  # filter out those with custom Sql

  # select the atlasId cohortId and the cohort name from all the files
    df <- lapply(dataList, function(df) df %>% select(cohortId, atlasId, cohortName, role ,needsCustomSql))
    df <- lapply(dataList, function(df) df %>% filter(needsCustomSql==0))
    # bind them together into one data frame
    df <- bind_rows(df)

  return(df)
}
# ==============================================================================
 # write cohort to create, concatenating the role values with a semicolon for cohorts 
 # that appear in multiple files
# ==============================================================================

writeCohortsToCreate <- function(settingsFolder)
 {
  cohorts=readAllCohorts(settingsFolder)
  # collapse cohorts with the same cohortId into one row, concatenating the role values with a semicolon
  cohorts <- cohorts %>%
    group_by(cohortId, atlasId, cohortName) %>%
    summarise(role = paste(role, collapse = ";"), .groups = "drop")
  write.csv(cohorts, file =  paste0(settingsFolder, '/CohortsToCreate.csv'), row.names = FALSE)
 return(cohorts)
  }


# ==============================================================================
 #  get the concept sets from the measurements, measurementStrata and conceptSets files,
 #  and write them to a single file
# ==============================================================================

  getConceptSets= function(settingsFolder)
  {
    files <- c("measurements", "measurementStrata",'conceptSets')
    conceptSets <- list()
    
    for (f in files) {
      filePath <- paste0(settingsFolder, '/', f, '.csv')
      if (file.exists(filePath)) {
        df <- read.csv(filePath, stringsAsFactors = FALSE)
        df$role <- f
        conceptSets[[f]] <- df %>% select(conceptSetId ,conceptSetName, role)  
      } else {
        warning("File ", f, ".csv not found in settings folder; skipping.")
      }
    }
    return(conceptSets)
  }

  # ==============================================================================
  #  add a cohort id column to the concept sets
  # check the concept set ids and give a unique cohort id for each concept set
  # overwrite the csv files with a csv adding the cohort id column
  # ==============================================================================

  createAndaddCohortIdsToConceptSets <- function(settingsFolder)
  {
    conceptSets=getConceptSets(settingsFolder)
    # bind them together into one data frame
    conceptSets <- bind_rows(conceptSets) %>%
      group_by(conceptSetId, conceptSetName) %>%
      summarise(role = paste(role, collapse = ";"), .groups = "drop")
    # add a cohort id column
    conceptSets$cohortId <- conceptSets$conceptSetId + 10000
    # write the csv files with the cohort id column added
    write.csv(conceptSets, file = paste0(settingsFolder, '/conceptSetsToCreate.csv'), row.names = FALSE)
    for (f in c("measurements", "measurementStrata",'conceptSets')) {
      filePath <- paste0(settingsFolder, '/', f, '.csv')
      if (file.exists(filePath)) {
        df <- read.csv(filePath, stringsAsFactors = FALSE)
        if ("cohortId" %in% colnames(df))
        {
          df <- df %>% select(-cohortId)
        }
        df <- merge(df, conceptSets[, c("conceptSetId", "cohortId")], by = "conceptSetId", all.x = TRUE)
        write.csv(df, file = filePath, row.names = FALSE)
      } else {
        warning("File ", f, ".csv not found in settings folder; skipping.")
      }
    }
  }
 
 
# ==============================================================================
 # resolve the concept sets using the webapi and write them to a csv file
 # this creates a csv file with all the concept ids for each concept set
 # which can be used to create the concept set tables in the database
# ==============================================================================
resolveconceptsets=function(baseUrl, instFolder)
{
  conceptSets=read.csv(paste0(instFolder, '/settings/conceptSetsToCreate.csv'), stringsAsFactors = FALSE)
  resolvedConceptSets <- data.frame()
  for (i in 1:nrow(conceptSets)) {
    JSON = ROhdsiWebApi::getConceptSetDefinition(conceptSetId = conceptSets$conceptSetId[i], baseUrl = baseUrl)
    resolved= ROhdsiWebApi::resolveConceptSet(conceptSetDefinition = JSON, baseUrl = EPAMbaseUrl)

      resolvedConceptSets <- rbind(resolvedConceptSets, 
                                  data.frame(
                                  cohortId = conceptSets$ cohortId[i],
                                  conceptSetId = conceptSets$conceptSetId[i],
                                  conceptId=resolved))
    }
    write.csv(resolvedConceptSets, file =  paste0(instFolder, '/conceptsets/resolvedConceptSets.csv'), row.names = FALSE)
  message("Resolved concept sets written to ", paste0(instFolder, '/conceptsets/resolvedConceptSets.csv'))
}
# ==============================================================================
# Runner: insert cohorts and concept sets in package, and resolve the concept sets
# ==============================================================================

insertCohortsandConceptsInPackage=function(instFolder, baseUrl, packageName)
{

  # first add the cohorts
  ROhdsiWebApi::insertCohortDefinitionSetInPackage(fileName = paste0(instFolder,
                                                   '/settings/CohortsToCreate.csv'),
                                                   baseUrl = baseUrl,
                                                   packageName = packageName )


  # for the concept sets, this has to be done in a for loop, as the function only takes one concept set at a time
  createAndaddCohortIdsToConceptSets (settingsFolder= paste0(instFolder, '/settings'))
  conceptSets=read.csv(paste0(instFolder, '/settings/ConceptSetsToCreate.csv'), stringsAsFactors = FALSE)
  # create conceptsets folder under the settings folder 
  dirPath <- paste0(instFolder, '/conceptsets')
  if (!dir.exists(dirPath)) {
    dir.create(dirPath, recursive = TRUE, showWarnings = FALSE)
  }
  
   for (i in 1:nrow(conceptSets)) {
    JSON = ROhdsiWebApi::getConceptSetDefinition(conceptSetId = conceptSets$conceptSetId[i], baseUrl = baseUrl)
    message(sprintf("Writing JSON for concept set %s:",  conceptSets$conceptSetId[i]))
    # display a log message 
    write(RJSONIO::toJSON(JSON), file = file.path(dirPath, paste0(conceptSets$conceptSetId[i], '.json')))
  }
  resolveconceptsets(baseUrl, instFolder)
}
 
#test
insertCohortsandConceptsInPackage(instFolder = 'inst', baseUrl , packageName = 'ENLHTNCKD')
