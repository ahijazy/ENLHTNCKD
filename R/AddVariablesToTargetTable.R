#==============================================================================
# Top-level orchestrator: adds every strata, binary-flag, measurement,
# stage, outcome, and treatment variable to target_<targetId> in one call.
#
# Each stage is optional (pass NULL to skip) and isolated with tryCatch so
# a failure in one stage is logged but does not prevent the remaining stages
# from running.
#
# Stages run in this order:
#   1. Age strata
#   2. Index strata
#   3. Age bins
#   4. Gender x Age strata (depends on ageStrata succeeding)
#   5. Binary strata (from baseline covariate table)
#   6. Baseline covariates (from baseline covariate table)
#   7. Baseline treatments (from treatments covariate table)
#   8. Stage at baseline (from one-hot baseline columns)
#   9. Stage strata (one-hot from stage column, depends on stage succeeding)
#  10. Measurements
#  11. Outcomes
#  12. Treatments
#
# Returns a named list of per-stage status:
#   "skipped" | "success" | "failed: <message>"
#
#==============================================================================
addAllTargetVariables <- function(connection,
                                  cohortDatabaseSchema,
                                  cohortTable,
                                  targetId,
                                  # demographic range strata
                                  ageStrata           = NULL,
                                  indexStrata         = NULL,
                                  ageBins             = NULL,
                                  addGenderByAge      = TRUE,
                                  # binary flags
                                  binaryStrata        = NULL,
                                  baselineCovariates  = NULL,
                                  baselineTreatments  = NULL,
                                  # stage at baseline
                                  stageDefinitions    = NULL,
                                  stageColumnName     = "ckd_stage_at_baseline",
                                  stageColumnPrefix   = "baseline",
                                  stageStrataPrefix   = "strata",
                                  # measurements
                                  measurementsRef     = NULL,
                                  # outcomes and treatments
                                  outcomes            = NULL,
                                  treatments          = NULL
                                  )
{
  dbms <- DatabaseConnector::dbms(connection)
  status <- list()
 
  # --- Helpers ----------------------------------------------------------------
  # Runs a stage and logs it. No tryCatch: errors propagate and stop everything.
  runStage <- function(name, expr) {
    ParallelLogger::logInfo("== Starting stage: ", name, " (target_", targetId, ") ==")
    expr
    ParallelLogger::logInfo("== Completed stage: ", name, " ==")
    status[[name]] <<- "success"
  }
 
  skipStage <- function(name) {
    ParallelLogger::logInfo("Skipping stage: ", name, " (no data provided)")
    status[[name]] <<- "skipped"
  }
 
  hasData <- function(df) !is.null(df) && is.data.frame(df) && nrow(df) > 0
 
  # === 1. Age strata ==========================================================
  if (hasData(ageStrata)) {
    runStage("ageStrata", {
      addAgeStrataToTarget(
        connection           = connection,
        cohortDatabaseSchema = cohortDatabaseSchema,
        targetId             = targetId,
        strata               = ageStrata
      )
    })
  } else {
    skipStage("ageStrata")
  }
 
  # === 2. Index strata ========================================================
  if (hasData(indexStrata)) {
    runStage("indexStrata", {
      addIndexStrataToTarget(
        connection           = connection,
        cohortDatabaseSchema = cohortDatabaseSchema,
        targetId             = targetId,
        strata               = indexStrata
      )
    })
  } else {
    skipStage("indexStrata")
  }
 
  # === 3. Age bins ============================================================
  if (hasData(ageBins)) {
    runStage("ageBins", {
      addAgeBinsToTarget(
        connection           = connection,
        cohortDatabaseSchema = cohortDatabaseSchema,
        targetId             = targetId,
        strata               = ageBins
      )
    })
  } else {
    skipStage("ageBins")
  }
 
  # === 4. Gender x Age ========================================================
  if (isTRUE(addGenderByAge)) {
    if (identical(status[["ageStrata"]], "success")) {
      runStage("genderByAge", {
        addGenderAgeStrata(
          connection           = connection,
          cohortDatabaseSchema = cohortDatabaseSchema,
          targetId             = targetId,
          ageStrataColumn      = "ageStrata",
          strataColumnName     = "GenderByAge"
        )
      })
    } else {
      stop("Cannot run genderByAge: ageStrata stage was skipped or not provided.")
    }
  } else {
    skipStage("genderByAge")
  }
 
  # === 5. Binary strata =======================================================
  if (hasData(binaryStrata)) {
    runStage("binaryStrata", {
      addAllbinaryStrata(
        connection           = connection,
        cohortDatabaseSchema = cohortDatabaseSchema,
        targetId             = targetId,
        binaryStrata         = binaryStrata
      )
    })
  } else {
    skipStage("binaryStrata")
  }
 
  # === 6. Baseline covariates =================================================
  if (hasData(baselineCovariates)) {
    runStage("baselineCovariates", {
      addAllBaselineCovariates(
        connection           = connection,
        cohortDatabaseSchema = cohortDatabaseSchema,
        targetId             = targetId,
        binaryStrata         = baselineCovariates
      )
    })
  } else {
    skipStage("baselineCovariates")
  }
 
  # === 7. Baseline treatments =================================================
  if (hasData(baselineTreatments)) {
    runStage("baselineTreatments", {
      addAllBaselineTreatments(
        connection           = connection,
        cohortDatabaseSchema = cohortDatabaseSchema,
        targetId             = targetId,
        binaryStrata         = baselineTreatments
      )
    })
  } else {
    skipStage("baselineTreatments")
  }
 
  # === 8. Stage at baseline ===================================================
  if (hasData(stageDefinitions)) {
    runStage("stageAtBaseline", {
      addStageAtBaseline(
        connection           = connection,
        cohortDatabaseSchema = cohortDatabaseSchema,
        targetId             = targetId,
        stageDefinitions     = stageDefinitions,
        stageColumnName      = stageColumnName,
        columnPrefix         = stageColumnPrefix,
        dbms                 = dbms
      )
    })
  } else {
    skipStage("stageAtBaseline")
  }
 
  # === 9. Stage strata (one-hot from stage column) ============================
  if (hasData(stageDefinitions)) {
    if (identical(status[["stageAtBaseline"]], "success")) {
      runStage("stageStrata", {
        addStrataFromColumn(
          connection           = connection,
          cohortDatabaseSchema = cohortDatabaseSchema,
          targetId             = targetId,
          sourceColumn         = stageColumnName,
          definitions          = stageDefinitions,
          columnPrefix         = stageStrataPrefix,
          dbms                 = dbms
        )
      })
    } else {
      stop("Cannot run stageStrata: stageAtBaseline stage did not succeed.")
    }
  } else {
    skipStage("stageStrata")
  }
 
  # === 10. Measurements =======================================================
  if (hasData(measurementsRef)) {
    runStage("measurements", {
      addAllMeasurementValueColumns(
        connection           = connection,
        cohortDatabaseSchema = cohortDatabaseSchema,
        targetId             = targetId,
        measurementsRef      = measurementsRef,
        columnPrefix         = "meas",
        defaultAggregation   = "closest"
      )
    })
  } else {
    skipStage("measurements")
  }
 
  # === 11. Outcomes ============================================================
  if (hasData(outcomes)) {
    runStage("outcomes", {
      addAllOutcomes(
        connection           = connection,
        cohortDatabaseSchema = cohortDatabaseSchema,
        targetId             = targetId,
        outcomeDefinitions   = outcomes,
        dbms                 = dbms
      )
    })
  } else {
    skipStage("outcomes")
  }
 
  # === 12. Treatments ==========================================================
  if (hasData(treatments)) {
    runStage("treatments", {
      addAllTreatmentColumns(
        connection           = connection,
        cohortDatabaseSchema = cohortDatabaseSchema,
        targetId             = targetId,
        treatmentDefinitions = treatments,
        dbms                 = dbms
      )
    })
  } else {
    skipStage("treatments")
  }
 
  # === Summary ================================================================
  ParallelLogger::logInfo("== All stages complete for target_", targetId, " ==")
  for (stageName in names(status)) {
    ParallelLogger::logInfo("  ", stageName, ": ", status[[stageName]])
  }
 
  invisible(status)
}