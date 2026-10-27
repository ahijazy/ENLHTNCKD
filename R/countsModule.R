# ==============================================================================
# Total cohort counts in the database [cohort table]
# ==============================================================================
getTotalCohortCounts=function(connection,
                cohortDatabaseSchema,
                cohortTable,
                outputFolder)
{
    sql='
    SELECT cohort_definition_id, COUNT(*) AS count
    FROM @cohort_database_schema.@cohort_table
    GROUP BY cohort_definition_id'
    sql <- SqlRender::render(sql,
                           cohort_database_schema = cohortDatabaseSchema,
                           cohort_table = cohortTable)
    translatedSql <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"))
    ParallelLogger::logInfo("Getting total cohort counts from cohort table...")
    counts <- DatabaseConnector::querySql(connection, translatedSql)
    write.csv(counts, file.path(outputFolder, "totalCohortCounts.csv"), row.names = FALSE)
    return(counts)
}

 
 
 # ==============================================================================
# Get patient counts by iterating over cohortStrataRef
#
# Each row of cohortStrataRef defines ONE patient subset.
# We construct the corresponding WHERE condition and count the patients
# directly from target_<targetId>.
#
# ==============================================================================


# ------------------------------------------------------------------------------
# Build the SQL WHERE condition for one cohortStrataRef row
# ------------------------------------------------------------------------------

getStrataWhereClause <- function(strataRow) {

  strataType <- as.character(strataRow$strataType)

  # --------------------------------------------------------------------------
  # Binary strata
  # 1 for with and 0 without
  # --------------------------------------------------------------------------

  if (strataType == "binary") {

    columnName <- paste0(
      "strata_",
      as.integer(strataRow$strataCohortId)
    )

    value <- as.integer(strataRow$value)

    return(
      paste0(columnName, " = ", value)
    )
  }
  # --------------------------------------------------------------------------
  # Gender
  # value contains the OMOP gender concept ID.
  # --------------------------------------------------------------------------

  if (strataType == "gender") {

    value <- as.integer(strataRow$value)

    return(
      paste0("gender_concept_id = ", value)
    )
  }
  # --------------------------------------------------------------------------
  # Age
  # ageStrata contains the cohort ID of the age band.
  # --------------------------------------------------------------------------
  if (strataType == "age") {

    ageStrataId <- as.integer(strataRow$strataCohortId)

    return(
      paste0("ageStrata = ", ageStrataId)
    )
  }
  # --------------------------------------------------------------------------
  # Index year
  # indexStrata contains the cohort ID of the index-year band.
  # --------------------------------------------------------------------------

  if (strataType == "index") {

    indexStrataId <- as.integer(strataRow$strataCohortId)

    return(
      paste0("indexStrata = ", indexStrataId)
    )
  }
  # --------------------------------------------------------------------------
  # Gender x age
  #
  # GenderByAge was populated as:
  #   CONCAT(gender_concept_id, ageStrata)
  # and the reference stores the resulting value.
  # --------------------------------------------------------------------------

  if (strataType == "gender_age") {

    value <- as.numeric(strataRow$value)

    return(
      paste0("GenderByAge = ", value)
    )
  }
  # --------------------------------------------------------------------------
  # measurements to be added here later
  # --------------------------------------------------------------------------

  stop(
    "Unknown strataType: ",
    strataType
  )
}

# ------------------------------------------------------------------------------
# Get patients and/or count for ONE strata reference row
# ------------------------------------------------------------------------------

getCountsForStrata <- function(connection,
                                 cohortDatabaseSchema,
                                 strataRow ) {

  targetId <- as.integer(strataRow$targetCohortId)

  targetTableName <- paste0(
    "target_",
    targetId
  )

  whereClause <- getStrataWhereClause(strataRow)
  # --------------------------------------------------------------------------
  # Count patients
  # --------------------------------------------------------------------------
    sql <- "
      SELECT COUNT(*) AS count
      FROM @cohort_database_schema.@target_table_name
      WHERE target_id = @target_id
        AND @strata_condition
    "

    renderedSql <- SqlRender::render(
      sql,

      cohort_database_schema = cohortDatabaseSchema,
      target_table_name = targetTableName,
      target_id = targetId,
      strata_condition = whereClause
    )

    translatedSql <- SqlRender::translate(
      renderedSql,
      targetDialect = attr(connection, "dbms")
    )

    result <- DatabaseConnector::querySql(
      connection,
      translatedSql
    )

    names(result) <- tolower(names(result))

    return(as.numeric(result$count[1]))
}


getCohortStrataCounts <- function(connection,
                                  cohortDatabaseSchema,
                                  cohortStrataRef,
                                  outputFolder) {

  if (is.null(cohortStrataRef) || nrow(cohortStrataRef) == 0) {
     return(data.frame())
  }

  results <- data.frame(
    cohortId = integer(),
    cohortName = character(),
    count = integer(),
    stringsAsFactors = FALSE
  )


  for (i in seq_len(nrow(cohortStrataRef))) {

    strataRow <- cohortStrataRef[i, , drop = FALSE]

    ParallelLogger::logInfo(
      "Getting count for strata ", paste0( strataRow$cohortId, " (", strataRow$cohortName, ")"   ))

      count <- getCountsForStrata(
        connection = connection,
        cohortDatabaseSchema = cohortDatabaseSchema,
        strataRow = strataRow
      )
    
      results[i,] <- data.frame(
        cohortId = strataRow$cohortId,
        cohortName = strataRow$cohortName,
        count = count,
        stringsAsFactors = FALSE
      )
    }  
write.csv(results, file.path(outputFolder, "cohortStrataCounts.csv"), row.names = FALSE)
return(results)
}

