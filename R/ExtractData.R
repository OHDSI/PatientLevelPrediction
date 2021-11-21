# @file ExtractData.R
#
# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of CohortMethod
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

#' Get the patient level prediction data from the server
#' @description
#' This function executes a large set of SQL statements against the database in OMOP CDM format to
#' extract the data needed to perform the analysis.
#'
#' @details
#' Based on the arguments, the at risk cohort data is retrieved, as well as outcomes
#' occurring in these subjects. The at risk cohort is identified  through
#' user-defined cohorts in a cohort table either inside the CDM instance or in a separate schema.
#' Similarly, outcomes are identified 
#' through user-defined cohorts in a cohort table either inside the CDM instance or in a separate
#' schema. Covariates are automatically extracted from the appropriate tables within the CDM.
#' If you wish to exclude concepts from covariates you will need to
#' manually add the concept_ids and descendants to the \code{excludedCovariateConceptIds} of the
#' \code{covariateSettings} argument.
#'
#' @param connectionDetails            An R object of type\cr\code{connectionDetails} created using the
#'                                     function \code{createConnectionDetails} in the
#'                                     \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema            The name of the database schema that contains the OMOP CDM
#'                                     instance.  Requires read permissions to this database. On SQL
#'                                     Server, this should specifiy both the database and the schema,
#'                                     so for example 'cdm_instance.dbo'.
#' @param oracleTempSchema             For Oracle only: the name of the database schema where you want
#'                                     all temporary tables to be managed. Requires create/insert
#'                                     permissions to this database.
#' @param cohortId                     A unique identifier to define the at risk cohort. CohortId is
#'                                     used to select the cohort_concept_id in the cohort-like table.
#' @param outcomeIds                   A list of cohort_definition_ids used to define outcomes (-999 mean no outcome gets downloaded).
#' @param studyStartDate               A calendar date specifying the minimum date that a cohort index
#'                                     date can appear. Date format is 'yyyymmdd'.
#' @param studyEndDate                 A calendar date specifying the maximum date that a cohort index
#'                                     date can appear. Date format is 'yyyymmdd'. Important: the study
#'                                     end data is also used to truncate risk windows, meaning no outcomes
#'                                     beyond the study end date will be considered.
#' @param cohortDatabaseSchema         The name of the database schema that is the location where the
#'                                     cohort data used to define the at risk cohort is available.
#'                                     Requires read permissions to this database.
#' @param cohortTable                  The tablename that contains the at risk cohort.  cohortTable has
#'                                     format of COHORT table: cohort_concept_id, SUBJECT_ID,
#'                                     COHORT_START_DATE, COHORT_END_DATE.
#' @param outcomeDatabaseSchema            The name of the database schema that is the location where
#'                                         the data used to define the outcome cohorts is available. 
#'                                         Requires read permissions to this database.
#' @param outcomeTable                     The tablename that contains the outcome cohorts. Expectation is
#'                                         outcomeTable has format of COHORT table:
#'                                         COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE,
#'                                         COHORT_END_DATE.
#' @param cdmVersion                   Define the OMOP CDM version used: currently support "4", "5" and "6".
#' @param firstExposureOnly            Should only the first exposure per subject be included? Note that
#'                                     this is typically done in the \code{createStudyPopulation} function,
#'                                     but can already be done here for efficiency reasons.
#' @param washoutPeriod                The mininum required continuous observation time prior to index
#'                                     date for a person to be included in the at risk cohort. Note that
#'                                     this is typically done in the \code{createStudyPopulation} function,
#'                                     but can already be done here for efficiency reasons.
#' @param sampleSize                   If not NULL, only this number of people will be sampled from the target population (Default NULL)
#' 
#' @param covariateSettings            An object of type \code{covariateSettings} as created using the
#'                                     \code{createCovariateSettings} function in the
#'                                     \code{FeatureExtraction} package.
#'
#' @return
#' Returns an object of type \code{plpData}, containing information on the cohorts, their
#' outcomes, and baseline covariates. Information about multiple outcomes can be captured at once for
#' efficiency reasons. This object is a list with the following components: \describe{
#' \item{outcomes}{A data frame listing the outcomes per person, including the time to event, and
#' the outcome id. Outcomes are not yet filtered based on risk window, since this is done at
#' a later stage.} \item{cohorts}{A data frame listing the persons in each cohort, listing their
#' exposure status as well as the time to the end of the observation period and time to the end of the
#' cohort (usually the end of the exposure era).} \item{covariates}{An ffdf object listing the
#' baseline covariates per person in the two cohorts. This is done using a sparse representation:
#' covariates with a value of 0 are omitted to save space.} \item{covariateRef}{An ffdf object describing the covariates that have been extracted.}
#' \item{metaData}{A list of objects with information on how the cohortMethodData object was
#' constructed.} } The generic \code{()} and \code{summary()} functions have been implemented for this object.
#'
#' @export
getPlpData <- function(connectionDetails,
                       cdmDatabaseSchema,
                       oracleTempSchema = cdmDatabaseSchema,
                       cohortId,
                       outcomeIds,
                       studyStartDate = "",
                       studyEndDate = "",
                       cohortDatabaseSchema = cdmDatabaseSchema,
                       cohortTable = "cohort",
                       outcomeDatabaseSchema = cdmDatabaseSchema,
                       outcomeTable = "cohort",
                       cdmVersion = "5",
                       firstExposureOnly = FALSE,
                       washoutPeriod = 0,
                       sampleSize = NULL,
                       covariateSettings) {
  if (studyStartDate != "" && regexpr("^[12][0-9]{3}[01][0-9][0-3][0-9]$", studyStartDate) == -1)
    stop("Study start date must have format YYYYMMDD")
  if (studyEndDate != "" && regexpr("^[12][0-9]{3}[01][0-9][0-3][0-9]$", studyEndDate) == -1)
    stop("Study end date must have format YYYYMMDD")
  if(!is.null(sampleSize)){
    if(!class(sampleSize) %in% c('numeric', 'integer'))
      stop("sampleSize must be numeric")
  }
  
  if(is.null(cohortId))
    stop('User must input cohortId')
  if(length(cohortId)>1)
    stop('Currently only supports one cohortId at a time')
  if(is.null(outcomeIds))
    stop('User must input outcomeIds')
  #ToDo: add other checks the inputs are valid
  
  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  dbms <- connectionDetails$dbms
  
  writeLines("\nConstructing the at risk cohort")
  if(!is.null(sampleSize))  writeLines(paste("\n Sampling ",sampleSize, " people"))
  renderedSql <- SqlRender::loadRenderTranslateSql("CreateCohorts.sql",
                                                   packageName = "PatientLevelPrediction",
                                                   dbms = dbms,
                                                   oracleTempSchema = oracleTempSchema,
                                                   cdm_database_schema = cdmDatabaseSchema,
                                                   cohort_database_schema = cohortDatabaseSchema,
                                                   cohort_table = cohortTable,
                                                   cdm_version = cdmVersion,
                                                   cohort_id = cohortId,
                                                   study_start_date = studyStartDate,
                                                   study_end_date = studyEndDate,
                                                   first_only = firstExposureOnly,
                                                   washout_period = washoutPeriod,
                                                   use_sample = !is.null(sampleSize),
                                                   sample_number=sampleSize
  )
  DatabaseConnector::executeSql(connection, renderedSql)
  
  writeLines("Fetching cohorts from server")
  start <- Sys.time()
  cohortSql <- SqlRender::loadRenderTranslateSql("GetCohorts.sql",
                                                 packageName = "PatientLevelPrediction",
                                                 dbms = dbms,
                                                 oracleTempSchema = oracleTempSchema,
                                                 cdm_version = cdmVersion)
  cohorts <- DatabaseConnector::querySql(connection, cohortSql)
  colnames(cohorts) <- SqlRender::snakeCaseToCamelCase(colnames(cohorts))
  metaData.cohort <- list(cohortId = cohortId,
                          studyStartDate = studyStartDate,
                          studyEndDate = studyEndDate)
  
  if(nrow(cohorts)==0)
    stop('Target population is empty')
  
  delta <- Sys.time() - start
  writeLines(paste("Loading cohorts took", signif(delta, 3), attr(delta, "units")))
  
  #covariateSettings$useCovariateCohortIdIs1 <- TRUE
  covariateData <- FeatureExtraction::getDbCovariateData(connection = connection,
                                                         oracleTempSchema = oracleTempSchema,
                                                         cdmDatabaseSchema = cdmDatabaseSchema,
                                                         cdmVersion = cdmVersion,
                                                         cohortTable = "#cohort_person",
                                                         cohortTableIsTemp = TRUE,
                                                         rowIdField = "row_id",
                                                         covariateSettings = covariateSettings)
  # add indexes for tidyCov + covariate summary
  Andromeda::createIndex(covariateData$covariates, c('rowId'),
                         indexName = 'covariates_rowId')
  Andromeda::createIndex(covariateData$covariates, c('covariateId'),
                         indexName = 'covariates_covariateId')
  Andromeda::createIndex(covariateData$covariates, c('covariateId', 'covariateValue'),
                         indexName = 'covariates_covariateId_value')
  
  if(max(outcomeIds)!=-999){
    writeLines("Fetching outcomes from server")
    start <- Sys.time()
    outcomeSql <- SqlRender::loadRenderTranslateSql("GetOutcomes.sql",
                                                    packageName = "PatientLevelPrediction",
                                                    dbms = dbms,
                                                    oracleTempSchema = oracleTempSchema,
                                                    cdm_database_schema = cdmDatabaseSchema,
                                                    outcome_database_schema = outcomeDatabaseSchema,
                                                    outcome_table = outcomeTable,
                                                    outcome_ids = outcomeIds,
                                                    cdm_version = cdmVersion)
    outcomes <- DatabaseConnector::querySql(connection, outcomeSql)
    colnames(outcomes) <- SqlRender::snakeCaseToCamelCase(colnames(outcomes))
    metaData.outcome <- data.frame(outcomeIds =outcomeIds)
    attr(outcomes, "metaData") <- metaData.outcome
    if(nrow(outcomes)==0)
      stop('No Outcomes')
    
    metaData.cohort$attrition <- getCounts2(cohorts,outcomes, "Original cohorts")
    attr(cohorts, "metaData") <- metaData.cohort
    
    delta <- Sys.time() - start
    writeLines(paste("Loading outcomes took", signif(delta, 3), attr(delta, "units")))
  } else {
    outcomes <- NULL
  }
  
  
  
  
  # Remove temp tables:
  renderedSql <- SqlRender::loadRenderTranslateSql("RemoveCohortTempTables.sql",
                                                   packageName = "PatientLevelPrediction",
                                                   dbms = dbms,
                                                   oracleTempSchema = oracleTempSchema)
  DatabaseConnector::executeSql(connection, renderedSql, progressBar = FALSE, reportOverallTime = FALSE)
  #DatabaseConnector::disconnect(connection)
  
  metaData <- covariateData$metaData
  metaData$call <- match.call()
  metaData$call$connectionDetails = NULL
  metaData$call$connection = NULL
  metaData$call$cdmDatabaseSchema = cdmDatabaseSchema
  metaData$call$oracleTempSchema = oracleTempSchema
  metaData$call$cohortId = cohortId
  metaData$call$outcomeIds = outcomeIds
  metaData$call$studyStartDate = studyStartDate
  metaData$call$studyEndDate = studyEndDate
  metaData$call$cohortDatabaseSchema = cohortDatabaseSchema
  metaData$call$cohortTable = cohortTable
  metaData$call$outcomeDatabaseSchema = outcomeDatabaseSchema
  metaData$call$outcomeTable = outcomeTable
  metaData$call$cdmVersion = cdmVersion
  metaData$call$firstExposureOnly = firstExposureOnly
  metaData$call$washoutPeriod = washoutPeriod
  metaData$call$covariateSettings= covariateSettings
  metaData$call$sampleSize = sampleSize
  
  # create the temporal settings (if temporal use)
  timeReference <- NULL
  if(!is.null(covariateSettings$temporal)){
    if(covariateSettings$temporal){
      # make sure time days populated
      if(length(covariateSettings$temporalStartDays)>0){
        timeReference = data.frame(timeId=1:length(covariateSettings$temporalStartDays),
                                   startDay = covariateSettings$temporalStartDays, 
                                   endDay = covariateSettings$temporalEndDays)
      }
    }}
  
  
  result <- list(cohorts = cohorts,
                 outcomes = outcomes,
                 covariateData = covariateData,
                 timeRef = timeReference,
                 metaData = metaData)
  
  class(result) <- "plpData"
  return(result)
}


#' @export
print.plpData <- function(x, ...) {
  writeLines("plpData object")
  writeLines("")
  writeLines(paste("At risk concept ID:", attr(x$cohorts, "metaData")$cohortId))
  writeLines(paste("Outcome concept ID(s):", paste(attr(x$outcomes, "metaData")$outcomeIds, collapse = ",")))
}

#' @method summary plpData
#' @export
summary.plpData <- function(object,...){
  people <- length(unique(object$cohorts$subjectId))
  outcomeCounts <- data.frame(outcomeId = attr(object$outcomes, "metaData")$outcomeIds,
                              eventCount = 0,
                              personCount = 0)
  for (i in 1:nrow(outcomeCounts)) {
    outcomeCounts$eventCount[i] <- sum(object$outcomes$outcomeId == attr(object$outcomes, "metaData")$outcomeIds[i])
    outcomeCounts$personCount[i] <- length(unique(object$outcomes$rowId[object$outcomes$outcomeId == attr(object$outcomes, "metaData")$outcomeIds[i]]))
  }
  
  covDetails <- FeatureExtraction::summary(object$covariateData)
  result <- list(metaData = append(append(object$metaData, attr(object$cohorts, "metaData")), attr(object$outcomes, "metaData")),
                 people = people,
                 outcomeCounts = outcomeCounts,
                 covariateCount = covDetails$covariateCount,
                 covariateValueCount = covDetails$covariateValueCount)
  class(result) <- "summary.plpData"
  return(result)
}

#' @export
print.summary.plpData <- function(x, ...) {
  writeLines("plpData object summary")
  writeLines("")
  writeLines(paste("At risk cohort concept ID:", x$metaData$cohortId))
  writeLines(paste("Outcome concept ID(s):", x$metaData$outcomeIds, collapse = ","))
  writeLines("")
  writeLines(paste("People:", paste(x$people)))
  writeLines("")
  writeLines("Outcome counts:")
  outcomeCounts <- x$outcomeCounts
  rownames(outcomeCounts) <- outcomeCounts$outcomeId
  outcomeCounts$outcomeId <- NULL
  colnames(outcomeCounts) <- c("Event count", "Person count")
  stats::printCoefmat(outcomeCounts)
  writeLines("")
  writeLines("Covariates:")
  writeLines(paste("Number of covariates:", x$covariateCount))
  writeLines(paste("Number of non-zero covariate values:", x$covariateValueCount))
}