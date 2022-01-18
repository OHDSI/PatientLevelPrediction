# @file AdditionalCovariates.R
#
# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of PatientLevelPrediction
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


#' Extracts covariates based on cohorts
#'
#' @details
#' The user specifies a cohort and time period and then a covariate is constructed whether they are in the
#' cohort during the time periods relative to target population cohort index
#'
#' @param connection  The database connection
#' @param oracleTempSchema  The temp schema if using oracle
#' @param cdmDatabaseSchema  The schema of the OMOP CDM data
#' @param cdmVersion  version of the OMOP CDM data
#' @param cohortTable  the table name that contains the target population cohort
#' @param rowIdField  string representing the unique identifier in the target population cohort
#' @param aggregated  whether the covariate should be aggregated
#' @param cohortId  cohort id for the target population cohort
#' @param covariateSettings  settings for the covariate cohorts and time periods
#'
#' @return
#' The models will now be in the package
#'
#' @export
getCohortCovariateData <- function(
  connection,
  oracleTempSchema = NULL,
  cdmDatabaseSchema,
  cdmVersion = "5",
  cohortTable = "#cohort_person",
  rowIdField = "row_id",
  aggregated,
  cohortId,
  covariateSettings
  ){
  
  # Some SQL to construct the covariate:
  sql <- paste(
    "select a.@row_id_field AS row_id, @covariate_id AS covariate_id,",
    "{@lnAgeInteraction}?{LOG(max(YEAR(a.cohort_start_date)-p.year_of_birth))}:{",
    "{@ageInteraction}?{max(YEAR(a.cohort_start_date)-p.year_of_birth)}:{",
    "{@countval}?{count(distinct b.cohort_start_date)}:{max(1)}",
    "}} as covariate_value",
    "from @cohort_temp_table a inner join @covariate_cohort_schema.@covariate_cohort_table b",
    " on a.subject_id = b.subject_id and ",
    " b.cohort_start_date <= dateadd(day, @endDay, a.cohort_start_date) and ",
    " b.cohort_end_date >= dateadd(day, @startDay, a.cohort_start_date) ",
    "{@ageInteraction | @lnAgeInteraction}?{inner join @cdm_database_schema.person p on p.person_id=a.subject_id}",
    "where b.cohort_definition_id = @covariate_cohort_id
    group by a.@row_id_field "
  )
  
  sql <- SqlRender::render(
    sql,
    covariate_cohort_schema = covariateSettings$cohortDatabaseSchema,
    covariate_cohort_table = covariateSettings$cohortTable,
    covariate_cohort_id = covariateSettings$cohortId,
    cohort_temp_table = cohortTable,
    row_id_field = rowIdField,
    startDay = covariateSettings$startDay,
    covariate_id = covariateSettings$covariateId,
    endDay = covariateSettings$endDay,
    countval = covariateSettings$count,
    ageInteraction = covariateSettings$ageInteraction,
    lnAgeInteraction = covariateSettings$lnAgeInteraction,
    cdm_database_schema = cdmDatabaseSchema
    )
  
  sql <- SqlRender::translate(
    sql = sql, 
    targetDialect = attr(connection, "dbms"),
    tempEmulationSchema = oracleTempSchema
    )
  
  # Retrieve the covariate:
  covariates <- DatabaseConnector::querySql(connection, sql)
  
  # Convert colum names to camelCase:
  colnames(covariates) <- SqlRender::snakeCaseToCamelCase(colnames(covariates))
  # Construct covariate reference:
  sql <- "select @covariate_id as covariate_id, '@concept_set' as covariate_name,
  @analysis_id as analysis_id, -1 as concept_id"
  sql <- SqlRender::render(
    sql = sql, 
    covariate_id = covariateSettings$covariateId,
    analysis_id = covariateSettings$analysisId,
    concept_set = paste('Cohort_covariate during day',
      covariateSettings$startDay,
      'through',
      covariateSettings$endDay,
      'days relative to index:',
      ifelse(covariateSettings$count, 'Number of', ''),
      covariateSettings$covariateName,
      ifelse(covariateSettings$ageInteraction, ' X Age', ''),
      ifelse(covariateSettings$lnAgeInteraction, ' X ln(Age)', '')
    )
  )
  
  sql <- SqlRender::translate(
    sql = sql, 
    targetDialect = attr(connection, "dbms"), 
    tempEmulationSchema =  oracleTempSchema
    )
  
  # Retrieve the covariateRef:
  covariateRef  <- DatabaseConnector::querySql(connection, sql)
  colnames(covariateRef) <- SqlRender::snakeCaseToCamelCase(colnames(covariateRef))
  
  analysisRef <- data.frame(
    analysisId = covariateSettings$analysisId,
    analysisName = "cohort covariate",
    domainId = "cohort covariate",
    startDay = 0,
    endDay = 0,
    isBinary = "Y",
    missingMeansZero = "Y"
    )
  
  metaData <- list(sql = sql, call = match.call())
  result <- Andromeda::andromeda(
    covariates = covariates,
    covariateRef = covariateRef,
    analysisRef = analysisRef
    )
  
  attr(result, "metaData") <- metaData
  class(result) <- "CovariateData"	
  return(result)
}


#' Extracts covariates based on cohorts
#'
#' @details
#' The user specifies a cohort and time period and then a covariate is constructed whether they are in the
#' cohort during the time periods relative to target population cohort index
#'
#' @param cohortName  Name for the cohort
#' @param settingId   A unique id for the covariate time and 
#' @param cohortDatabaseSchema  The schema of the database with the cohort
#' @param cohortTable  the table name that contains the covariate cohort
#' @param cohortId  cohort id for the covariate cohort
#' @param startDay  The number of days prior to index to start observing the cohort
#' @param endDay  The number of days prior to index to stop observing the cohort
#' @param count   If FALSE the covariate value is binary (1 means cohort occurred between index+startDay and index+endDay, 0 means it did not)
#'                If TRUE then the covariate value is the number of unique cohort_start_dates between index+startDay and index+endDay 
#' @param ageInteraction  If TRUE multiple covariate value by the patient's age in years      
#' @param lnAgeInteraction  If TRUE multiple covariate value by the log of the patient's age in years 
#' @param analysisId  The analysisId for the covariate          
#'
#' @return
#' An object of class covariateSettings specifying how to create the cohort covariate with the covariateId
#'  cohortId x 100000 + settingId x 1000 + analysisId
#'
#' @export
createCohortCovariateSettings <- function(
  cohortName, 
  settingId,
  cohortDatabaseSchema, 
  cohortTable, 
  cohortId,
  startDay = -30, 
  endDay = 0, 
  count = F, 
  ageInteraction = F, 
  lnAgeInteraction = F,
  analysisId = 456
  ){
  
  if( settingId > 100 || settingId < 0){
    stop('settingId must be between 1 and 100')
  }
  
  covariateSettings <- list(
    covariateName = cohortName, 
    covariateId = cohortId*100000+settingId*1000+analysisId,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTable,
    cohortId = cohortId,
    startDay = startDay,
    endDay = endDay,
    count = count,
    ageInteraction = ageInteraction,
    lnAgeInteraction = lnAgeInteraction,
    analysisId = analysisId
    )
  
  attr(covariateSettings, "fun") <- "PatientLevelPrediction::getCohortCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}