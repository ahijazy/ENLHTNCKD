# CRITICAL SAFETY CHECK: Prevent deleting source cohorts
# carefull here, this deletes elements in the table with if they have the same 
# cohort definition id as prevalentCohortId
 addIncidentPopulationtoCohortTable <- function(connection, 
                                               cohortDatabaseSchema,
                                               cohortTable,
                                               incidentCohortId,
                                               CKDCohortId,
                                               HTNCohortId,
                                               indexDate) 
{

  sql_incident_population <- "
  DELETE FROM @cohort_database_schema.@cohort_table
  WHERE cohort_definition_id = @incident_cohort_id;
  
  WITH CKD_patients AS (
      SELECT 
          subject_id,
          cohort_start_date AS CKD_start_date,
          cohort_end_date AS CKD_end_date
      FROM @cohort_database_schema.@cohort_table
      WHERE cohort_definition_id = @ckd_cohort_id
  ),
  HTN_patients AS (
      SELECT 
          subject_id,
          cohort_start_date AS HTN_start_date,
          cohort_end_date AS HTN_end_date
      FROM @cohort_database_schema.@cohort_table
      WHERE cohort_definition_id = @htn_cohort_id
  ),
  Comorbid_patients AS (
      SELECT 
          c.subject_id,
          -- The first date the patient has both (MAX of the two start dates)
          CASE 
              WHEN c.CKD_start_date > h.HTN_start_date THEN c.CKD_start_date 
              ELSE h.HTN_start_date 
          END AS cohort_start_date,
          -- The cohort end date (MAX of the two end dates)
          CASE 
              WHEN c.CKD_end_date > h.HTN_end_date THEN c.CKD_end_date 
              ELSE h.HTN_end_date 
          END AS cohort_end_date
      FROM CKD_patients c
      INNER JOIN HTN_patients h ON c.subject_id = h.subject_id
  )
  INSERT INTO @cohort_database_schema.@cohort_table (
      cohort_definition_id,
      subject_id,
      cohort_start_date,
      cohort_end_date
  )
  SELECT 
      @incident_cohort_id AS cohort_definition_id,
      subject_id, 
      cohort_start_date,
      cohort_end_date
  FROM Comorbid_patients
  -- Filter: The patient achieved both conditions ON or AFTER the specified index date
  WHERE cohort_start_date >= CAST('@index_date' AS DATE);
  "

  # 1. Swap R variables into the SQL template placeholders
  renderedSql <- SqlRender::render(sql = sql_incident_population, 
                                   cohort_database_schema = cohortDatabaseSchema,
                                   cohort_table = cohortTable,
                                   ckd_cohort_id = CKDCohortId,
                                   htn_cohort_id = HTNCohortId,
                                   incident_cohort_id = incidentCohortId,
                                   index_date = indexDate)
  
  # 2. Universal Dialect Translation (reads database metadata to detect connection type)
  translatedSql <- SqlRender::translate(renderedSql, targetDialect = attr(connection, "dbms"))
  
  ParallelLogger::logInfo("Populating cohort table with Incident Cohort ", incidentCohortId)
  
  # 3. Execute translated SQL query
  DatabaseConnector::executeSql(connection, translatedSql)
  
  ParallelLogger::logInfo("Successfully populated cohort table for Incident Cohort: ", incidentCohortId)
}


# CRITICAL SAFETY CHECK: Prevent deleting source cohorts
# carefull here, this deletes elements in the table with if they have the same 
# cohort definition id as incidentCohortId
addPrevalentPopulationtoCohortTable <- function(connection, 
                                                cohortDatabaseSchema,
                                                cohortTable,
                                                prevalentCohortId,
                                                CKDCohortId,
                                                HTNCohortId,
                                                indexDate) 
{
  # Safety check
  if (prevalentCohortId == CKDCohortId || prevalentCohortId == HTNCohortId) {
    stop(paste0(
      "CRITICAL ERROR: 'prevalentCohortId' (", prevalentCohortId, ") ",
      "cannot match source 'CKDCohortId' or 'HTNCohortId'."
    ))
  }

  # Standard OHDSI T-SQL Template (compiles to all major dialects)
  sql_prevalent_population <- "
  DELETE FROM @cohort_database_schema.@cohort_table
  WHERE cohort_definition_id = @prevalent_cohort_id;
  
  WITH CKD_patients AS (
      SELECT 
          subject_id,
          cohort_start_date AS CKD_start_date,
          cohort_end_date AS CKD_end_date
      FROM @cohort_database_schema.@cohort_table
      WHERE cohort_definition_id = @ckd_cohort_id
  ),
  HTN_patients AS (
      SELECT 
          subject_id,
          cohort_start_date AS HTN_start_date,
          cohort_end_date AS HTN_end_date
      FROM @cohort_database_schema.@cohort_table
      WHERE cohort_definition_id = @htn_cohort_id
  ),
  Comorbid_patients AS (
      SELECT 
          c.subject_id,
          -- The first date the patient has both (MAX of the two start dates)
          CASE 
              WHEN c.CKD_start_date > h.HTN_start_date THEN c.CKD_start_date 
              ELSE h.HTN_start_date 
          END AS cohort_start_date,
          -- The cohort end date (MAX of the two end dates)
          CASE 
              WHEN c.CKD_end_date > h.HTN_end_date THEN c.CKD_end_date 
              ELSE h.HTN_end_date 
          END AS cohort_end_date
      FROM CKD_patients c
      INNER JOIN HTN_patients h ON c.subject_id = h.subject_id
  )
  INSERT INTO @cohort_database_schema.@cohort_table (
      cohort_definition_id,
      subject_id,
      cohort_start_date,
      cohort_end_date
  )
  SELECT 
      @prevalent_cohort_id AS cohort_definition_id,
      subject_id, 
      cohort_start_date,
      cohort_end_date
  FROM Comorbid_patients
  -- Filter: The patient achieved both conditions before the specified index date
  WHERE cohort_start_date < CAST('@index_date' AS DATE);
  "

  # 1. Swap R variables into the SQL template placeholders
  renderedSql <- SqlRender::render(sql = sql_prevalent_population, 
                                   cohort_database_schema = cohortDatabaseSchema,
                                   cohort_table = cohortTable,
                                   ckd_cohort_id = CKDCohortId,
                                   htn_cohort_id = HTNCohortId,
                                   prevalent_cohort_id = prevalentCohortId,
                                   index_date = indexDate)
  
  # 2. Universal Dialect Translation (reads database metadata to detect connection type)
  translatedSql <- SqlRender::translate(renderedSql, targetDialect = attr(connection, "dbms"))
  
  ParallelLogger::logInfo("Populating cohort table with Prevalent Cohort ", prevalentCohortId)
  
  # 3. Execute translated SQL query 
  DatabaseConnector::executeSql(connection, translatedSql)
  
  ParallelLogger::logInfo("Successfully populated cohort table for Prevalent Cohort: ", prevalentCohortId)
}
