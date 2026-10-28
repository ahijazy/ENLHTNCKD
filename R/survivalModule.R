#survival analysis
# generate KM tables and survival curves
# cox models for outcomes?

getSurvivalData=function(
  connection,
  cohortDatabaseSchema,
  cohortDataStrataRow,
  outcomeId)
 {
  targetId <- as.integer(cohortDataStrataRow$targetCohortId)
  targetTableName <- paste0("target_", targetId)
  outcomeId=as.integer(outcomeId)
  priorOutomeColumn=paste0("prior_outcome_",outcomeId)
  outcomeIndicatorColumn=paste0("outcome_",outcomeId)
  daysToOutcomeColumn=paste0("outcome_days_",outcomeId)
  
  sql="
    SELECT subject_id,@prior_outcome_column AS prior_outcome,
           @outcome_indicator_column AS outcome_indicator,
           @days_to_outcome_column AS days_to_outcome    
    FROM @cohort_database_schema.@target_table_name
    WHERE @strata_condition
    "
    df=DatabaseConnector::renderTranslateQuerySql(
      connection,
      sql,
      cohort_database_schema = cohortDatabaseSchema,
      target_table_name = targetTableName,
      strata_condition = getStrataWhereClause(cohortDataStrataRow),
      prior_outcome_column=priorOutomeColumn,
      outcome_indicator_column=outcomeIndicatorColumn,
      days_to_outcome_column=daysToOutcomeColumn
    )
    fitted_surv=survival::survfit(survival::Surv(days_to_outcome,outcome_indicator)~1,data=df)
    return(df)
 }

#