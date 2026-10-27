readSettingsTable <- function(tableName, packageName) {
     tablePath <- system.file("settings", paste0(tableName, ".csv"), package = packageName)
     if (tablePath == "") {
       stop("Could not find '", tableName, ".csv' in package '", packageName, "'.")
     }
     read.csv(tablePath, stringsAsFactors = FALSE, na.strings = c("", "NA"))
   }
  
   
#utilities
# ==============================================================================
# helper: builds when Then clauses for range bounded strata
# ==============================================================================
buildRangeCaseWhenClauses <- function(cohortList,
                                      valueColumn ,#name of the colomn with the values are in e.g. age, index years
                                      bandIdColumn = "cohortId") # bandId corresponds to the value to fill in i.e. 1,2,3 (strata or cohortid)
{

  LOWER_SENTINEL <- -9999
  UPPER_SENTINEL <- 9999
  vapply(seq_len(nrow(cohortList)), function(i) {

    lb <- cohortList$lowerBound[i]
    ub <- cohortList$upperBound[i]
    conds <- character(0)

    if (lb > LOWER_SENTINEL)
    {
      conds <- c(conds, paste0(valueColumn, " >= ", lb))
    }

    if (ub < UPPER_SENTINEL)
    {
      conds <- c(conds, paste0(valueColumn, " < ", ub))
    }

    cond <- if (length(conds)) paste(conds, collapse = " AND ") else "1 = 1"

    paste0("WHEN ", cond, " THEN ", cohortList[[bandIdColumn]][i])

  }, character(1))
}
# ==============================================================================
# helper: fetch the cohort ids that are applicable to a target from one of the tables
# strata, baseline, treatments, outcomes, measurements, measurementStrata
# ==============================================================================
filterTableToApplicableTarget <- function(targetIdToCheck, table, packageName) {
  tablePath <- system.file("settings", paste0(table, ".csv"), package = packageName)
  if (tablePath == "") {
    stop("Could not find '", table, ".csv' in package '", packageName, "'.")
  }
  tableData <- read.csv(tablePath, stringsAsFactors = FALSE, na.strings = c("", "NA"))

  tableData[is.na(tableData$targetId) | tableData$targetId == targetIdToCheck, , drop = FALSE]
}

fetchApplicableCohortIds <- function(targetIdToCheck, table, packageName) {
  filtered <- filterTableToApplicableTarget(targetIdToCheck, table, packageName)
  unique(filtered$cohortId)
}
# ==============================================================================
# helper:  Drop a table
# ==============================================================================
dropTable <- function(connection, schemaName, tableName) {
  sql_drop_table <- "
  DROP TABLE @schema_name.@table_name;"
  
  renderedSql <- SqlRender::render(sql_drop_table, 
                                   schema_name = schemaName,
                                   table_name = tableName)
  
  translatedSql <- SqlRender::translate(renderedSql, targetDialect = attr(connection, "dbms"))
  
  ParallelLogger::logInfo("Dropping table '", schemaName, ".", tableName, "'")
  DatabaseConnector::executeSql(connection, translatedSql)
  
  return(invisible(TRUE))
}
# ==============================================================================
# helper:  adds a colomn to a table
# ==============================================================================

alterTableAddColumn <- function(connection, 
                                schemaName,
                                tableName,
                                columnName,
                                type = "INT",
                                defaultValue = NULL) 
{
  sql_add_column <- "
  ALTER TABLE @schema_name.@table_name
  ADD @column_name @type @default_value;"
  
  defaultClause <- ifelse(is.null(defaultValue), "", paste("DEFAULT", defaultValue))
  
  renderedSql <- SqlRender::render(sql_add_column, 
                                   schema_name = schemaName,
                                   table_name = tableName,
                                   column_name = columnName,
                                   type = type,
                                   default_value = defaultClause)
  
  translatedSql <- SqlRender::translate(renderedSql, targetDialect = attr(connection, "dbms"))
  
  ParallelLogger::logInfo("Adding column '", columnName, "' to table '", schemaName, ".", tableName, "'")
  DatabaseConnector::executeSql(connection, translatedSql)
  
  return(invisible(TRUE))
}

# ==============================================================================
# Get the subject_ids belonging to a given stratum, by querying the target
# table directly. Result can be merged with baseline/outcome/measurement
# tables on subject_id.
# ==============================================================================

getSubjectsInStrata=function(connection,
                cohortDatabaseSchema,
                targetId,
                strataRow,
                strataRef)
{
    tableName <- paste0("target_", targetId)
    condition <- buildStrataCondition(strataRow, strataRef)

    sql <- '
    SELECT subject_id
    FROM @cohort_database_schema.@target_table_name t
    WHERE t.target_id = @target_id
      AND @where_condition;'

    renderedSql <- SqlRender::render(sql,
                                     cohort_database_schema = cohortDatabaseSchema,
                                     target_table_name = tableName,
                                     target_id = targetId,
                                     where_condition = condition)
    translatedSql <- SqlRender::translate(renderedSql, targetDialect = attr(connection, "dbms"))
    ParallelLogger::logInfo("Getting subjects in stratum ", strataRow$cohortId, " for target ", targetId, "...")
    result <- DatabaseConnector::querySql(connection, translatedSql)
    names(result) <- tolower(names(result))
    result$subject_id
}

# ==============================================================================
# Helper: build a SQL select clause for a given strata type to 
# select the relevant columns
# ==============================================================================


# ==============================================================================
# Helper: build a SQL WHERE condition for a single strataRef row, based on its type
# ==============================================================================
buildStrataCondition <- function(strataRow, strataRef) {
    if (strataRow$strataType == "binary") {
        val <- ifelse(strataRow$value == 1, 1, 0)
        return(sprintf("t.strata_%d = %d", strataRow$strataCohortId, val))
    }
    if (strataRow$strataType == "age") {
        return(sprintf("t.ageStrata = %s", strataRow$strataCohortId))
    }
    if (strataRow$strataType == "index") {
        return(sprintf("t.indexStrata = %s", strataRow$strataCohortId))
    }
    if (strataRow$strataType == "gender") {
        return(sprintf("t.gender_concept_id = %d", strataRow$value))
    }
    if (strataRow$strataType == "gender_age") {
        ageStrataId <- getStrataCohortId(strataRef,
                                         targetId    = strataRow$targetCohortId,
                                         strataId    = strataRow$value,
                                         lowerBound  = strataRow$lowerBound,
                                         upperBound  = strataRow$upperBound)
        # ageStrataId above resolves to the gender_age row itself (same lowerBound/
        # upperBound/value uniquely identify it) — what we actually need is the
        # underlying age-only stratum's id, so look it up by type instead:
        ageLookup <- strataRef[strataRef$strataType == "age" &
                               strataRef$targetCohortId == strataRow$targetCohortId &
                               strataRef$lowerBound == strataRow$lowerBound &
                               strataRef$upperBound == strataRow$upperBound, ]
        if (nrow(ageLookup) == 0) {
            stop("No matching age strata found for gender_age row with cohortId ", strataRow$cohortId)
        }
        ageStrataId <- ageLookup$strataCohortId[1]
        return(sprintf("t.gender_concept_id = %d AND t.ageStrata = %s", strataRow$value, ageStrataId))
    }
    if (strataRow$strataType == "measurement") {
        return(sprintf("t.measurementStrata = %s", strataRow$strataCohortId))
    }
    stop("Unsupported strataType: ", strataRow$strataType)
}

#


# ==============================================================================
# Look up a strata cohortId from strataRef given its identifying attributes.
# Any parameter left as NA is ignored as a matching criterion — pass only
# what's relevant for the strata type you're looking up.
# ==============================================================================

getStrataCohortId=function(strataRef,
                targetId,
                strataId,
                windowStart = NA,
                windowEnd   = NA,
                lowerBound  = NA,
                upperBound  = NA,
                value       = NA,
                strataType  = NA)
{
    # matches a strataRef column against an input value, treating NA input as
    # "ignore this filter" rather than requiring an exact NA==NA match
    matchField <- function(col, val) {
        if (is.na(val)) return(rep(TRUE, length(col)))
        ifelse(is.na(col), FALSE, col == val)
    }

    matches <- strataRef$targetCohortId == targetId &
               strataRef$strataCohortId == strataId &
               matchField(strataRef$windowStart, windowStart) &
               matchField(strataRef$windowEnd,   windowEnd) &
               matchField(strataRef$lowerBound,  lowerBound) &
               matchField(strataRef$upperBound,  upperBound) &
               matchField(strataRef$value,       value) &
               matchField(strataRef$strataType,  strataType)

    result <- strataRef$cohortId[matches]

    if (length(result) == 0) {
        stop("No matching strata cohortId found for targetId=", targetId,
             ", strataId=", strataId,
             ", windowStart=", windowStart, ", windowEnd=", windowEnd,
             ", lowerBound=", lowerBound, ", upperBound=", upperBound,
             ", value=", value)
    }
    if (length(result) > 1) {
        warning("Multiple matching strata cohortIds found (", length(result),
                ") for the given criteria — returning the first. Consider passing ",
                "more specific bounds/windows/value to disambiguate.")
    }

    result[1]
}
