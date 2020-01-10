# @file PlpSaveLoad.R
#
# Copyright 2019 Observational Health Data Sciences and Informatics
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
#' @param cohortId                     A unique identifier to define the at risk cohort.  If
#'                                     cohortTable = DRUG_ERA, cohortId is a CONCEPT_ID and all
#'                                     descendant concepts within that CONCEPT_ID will be used to
#'                                     define the cohort.  If cohortTable <> DRUG_ERA, cohortId is
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
#'                                     If cohortTable = DRUG_ERA, cohortDatabaseSchema is not used
#'                                     by assumed to be cdmSchema.  Requires read permissions to this
#'                                     database.
#' @param cohortTable                  The tablename that contains the at risk cohort.  If
#'                                     cohortTable <> DRUG_ERA, then expectation is cohortTable has
#'                                     format of COHORT table: cohort_concept_id, SUBJECT_ID,
#'                                     COHORT_START_DATE, COHORT_END_DATE.
#' @param outcomeDatabaseSchema            The name of the database schema that is the location where
#'                                         the data used to define the outcome cohorts is available. If
#'                                         cohortTable = CONDITION_ERA, exposureDatabaseSchema is not
#'                                         used by assumed to be cdmSchema.  Requires read permissions
#'                                         to this database.
#' @param outcomeTable                     The tablename that contains the outcome cohorts.  If
#'                                         outcomeTable <> CONDITION_OCCURRENCE, then expectation is
#'                                         outcomeTable has format of COHORT table:
#'                                         COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE,
#'                                         COHORT_END_DATE.
#' @param cdmVersion                   Define the OMOP CDM version used: currently support "4" and "5".
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
#' @param excludeDrugsFromCovariates   A redundant option                                     
#' @param baseUrl                      If extracting cohorts from atlas enter atlas url to extract cohort creation details
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
                       covariateSettings,
                       excludeDrugsFromCovariates = FALSE,
                       baseUrl = NULL) {
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
  DatabaseConnector::disconnect(connection)
  
  metaData <- covariateData$metaData
  metaData$call <- match.call()
  metaData$call$connectionDetails = connectionDetails
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
        timeReference = ff::as.ffdf(data.frame(timeId=1:length(covariateSettings$temporalStartDays),
                                               startDay = covariateSettings$temporalStartDays, 
                                               endDay = covariateSettings$temporalEndDays))
      }
    }}
  
  
  # code to extract cohort if selected
  if(!is.null(baseUrl)){
    cohortCode <- saveCirceDefinition(cohortId, 'Target Cohort', baseUrl)
    outcomeCode <- list()
    length(outcomeCode) <- length(outcomeIds)
    i <- 0
    for(outcomeId in outcomeIds){
      i <- i+1
      outcomeCode[[i]] <- saveCirceDefinition(outcomeId, paste0('Outcome ',i, ' Cohort'), baseUrl)
    }
    metaData$cohortCreate <- list(targetCohort = cohortCode ,
                         outcomeCohorts = outcomeCode)
  }

  result <- list(cohorts = cohorts,
                 outcomes = outcomes,
                 covariates = covariateData$covariates,
                 covariateRef = covariateData$covariateRef,
                 timeRef = timeReference,#covariateData$covariatesContinuous,
                 analysisRef = covariateData$analysisRef,
                 metaData = metaData)
  
  class(result) <- "plpData"
  return(result)
}




#' Get the covaridate data for a cohort table
#' @description
#' This function executes some SQL to extract covaraite data for a cohort table
#'
#' @details
#'
#' @param connection                   Can also use an existing connection rather than the connectionDetails
#' @param cdmDatabaseSchema            The name of the database schema that contains the OMOP CDM
#'                                     instance.  Requires read permissions to this database. On SQL
#'                                     Server, this should specifiy both the database and the schema,
#'                                     so for example 'cdm_instance.dbo'.
#' @param oracleTempSchema             For Oracle only: the name of the database schema where you want
#'                                     all temporary tables to be managed. Requires create/insert
#'                                     permissions to this database.
#' @param cohortTable                  The temp table containing the cohort of people
#' @param cdmVersion                   The version of the CDM (default 5)
#' @param covariateSettings            An object of type \code{covariateSettings} as created using the
#'                                     \code{createCovariateSettings} function in the
#'                                     \code{FeatureExtraction} package.
#'
#' @return
#' Returns the covariates for the people in the temp table
#' @export
getCovariateData <- function(connection,
                       cdmDatabaseSchema,
                       oracleTempSchema = cdmDatabaseSchema,
                       cohortTable = "#cohort_person",
                       cdmVersion = 5,
                       covariateSettings) {
  
  if(missing(connection)){
    stop('Need to enter an existing connection')
  } 
  
  writeLines("Extracting covariate data")
  covariateData <- FeatureExtraction::getDbCovariateData(connection = connection,
                                                         oracleTempSchema = oracleTempSchema,
                                                         cdmDatabaseSchema = cdmDatabaseSchema,
                                                         cdmVersion = cdmVersion,
                                                         cohortTable = cohortTable,
                                                         cohortTableIsTemp = TRUE,
                                                         rowIdField = "row_id",
                                                         covariateSettings = covariateSettings)
  return(covariateData$covariates)
}


#' Save the cohort data to folder
#'
#' @description
#' \code{savePlpData} saves an object of type plpData to folder.
#'
#' @param plpData   An object of type \code{plpData} as generated using
#'                           \code{getDbPlpData}.
#' @param file               The name of the folder where the data will be written. The folder should
#'                           not yet exist.
#' @param envir              The environment for to evaluate variables when saving
#' @param overwrite          Whether to force overwrite an existing file
#' @details
#' The data will be written to a set of files in the folder specified by the user.
#'
#' @examples
#' # todo
#'
#' @export
savePlpData <- function(plpData, file, envir=NULL, overwrite=F) {
  if (missing(plpData))
    stop("Must specify plpData")
  if (missing(file))
    stop("Must specify file")
  if (!class(plpData) %in% c("plpData","plpData.libsvm"  ))
    stop("Data not of class plpData")
  
  # save the actual values in the metaData
  # TODO - only do this if exists in parent or environ
  if(is.null(plpData$metaData$call$sampleSize)){  # fixed a bug when sampleSize is NULL
    plpData$metaData$call$sampleSize <- 'NULL'
  }
  for(i in 2:length(plpData$metaData$call)){
    if(!is.null(plpData$metaData$call[[i]]))
      plpData$metaData$call[[i]] <- eval(plpData$metaData$call[[i]], envir = envir)
  }
  
  if('ffdf'%in%class(plpData$covariates)){
    covariates <- plpData$covariates
    covariateRef <- plpData$covariateRef
    
    if(!is.null(plpData$analysisRef)){
      if(!is.null(plpData$timeRef)){
        analysisRef <- plpData$analysisRef
        timeRef <- plpData$timeRef
        ffbase::save.ffdf(covariates, covariateRef,analysisRef,timeRef, dir = file, clone = TRUE, overwrite = overwrite)
      } else {
        analysisRef <- plpData$analysisRef
        ffbase::save.ffdf(covariates, covariateRef,analysisRef, dir = file, clone = TRUE, overwrite = overwrite)
      }
    } else {
      ffbase::save.ffdf(covariates, covariateRef, dir = file, clone = TRUE, overwrite = overwrite)
    }
    
  } else{
    covariateRef <- plpData$covariateRef
    analysisRef <- plpData$analysisRef
    timeRef <- plpData$timeRef
    ffbase::save.ffdf(covariateRef,analysisRef,timeRef, dir = file, clone = TRUE, overwrite = overwrite)
    saveRDS(plpData$covariates, file = file.path(file, "covariates.rds"))
  }
  saveRDS(plpData$cohorts, file = file.path(file, "cohorts.rds"))
  saveRDS(plpData$outcomes, file = file.path(file, "outcomes.rds"))
  saveRDS(plpData$metaData, file = file.path(file, "metaData.rds"))
}

#' Load the cohort data from a folder
#'
#' @description
#' \code{loadPlpData} loads an object of type plpData from a folder in the file
#' system.
#'
#' @param file       The name of the folder containing the data.
#' @param readOnly   If true, the data is opened read only.
#'
#' @details
#' The data will be written to a set of files in the folder specified by the user.
#'
#' @return
#' An object of class plpData.
#'
#' @examples
#' # todo
#'
#' @export
loadPlpData <- function(file, readOnly = TRUE) {
  if (!file.exists(file))
    stop(paste("Cannot find folder", file))
  if (!file.info(file)$isdir)
    stop(paste("Not a folder", file))
  
  temp <- setwd(file)
  absolutePath <- setwd(temp)
  
  if(!file.exists(file.path(file, "covariates.rds"))){
  e <- new.env()
  ffbase::load.ffdf(absolutePath, e)
  result <- list(covariates = get("covariates", envir = e),
                 covariateRef = get("covariateRef", envir = e),
                 timeRef = get0("timeRef", envir = e,ifnotfound = NULL),
                 analysisRef = get0("analysisRef", envir = e,ifnotfound = NULL),
                 cohorts = readRDS(file.path(file, "cohorts.rds")),
                 outcomes = readRDS(file.path(file, "outcomes.rds")),
                 metaData = readRDS(file.path(file, "metaData.rds")))
  # Open all ffdfs to prevent annoying messages later:
  open(result$covariates, readonly = readOnly)
  open(result$covariateRef, readonly = readOnly)
  if(!is.null(result$timeRef)){open(result$timeRef, readonly = readOnly)}
  if(!is.null(result$analysisRef)){open(result$analysisRef, readonly = readOnly)}
  class(result) <- "plpData"
  } else{
    e <- new.env()
    ffbase::load.ffdf(absolutePath, e)
    result <- list(covariates = readRDS(file.path(file, "covariates.rds")),
                   covariateRef = get("covariateRef", envir = e),
                   timeRef = get0("timeRef", envir = e,ifnotfound = NULL),
                   analysisRef = get0("analysisRef", envir = e,ifnotfound = NULL),
                   cohorts = readRDS(file.path(file, "cohorts.rds")),
                   outcomes = readRDS(file.path(file, "outcomes.rds")),
                   metaData = readRDS(file.path(file, "metaData.rds")))
    # Open all ffdfs to prevent annoying messages later:
    open(result$covariateRef, readonly = readOnly)
    if(!is.null(result$timeRef)){open(result$timeRef, readonly = readOnly)}
    if(!is.null(result$analysisRef)){open(result$analysisRef, readonly = readOnly)}
    class(result) <- "plpData.libsvm"
  }

  rm(e)
  return(result)
}

#' @export
print.plpData <- function(x, ...) {
  writeLines("plpData object")
  writeLines("")
  writeLines(paste("At risk concept ID:", attr(x$cohorts, "metaData")$cohortId))
  writeLines(paste("Outcome concept ID(s):", paste(attr(x$outcomes, "metaData")$outcomeIds, collapse = ",")))
}

#' @export
summary.plpData <- function(object, ...) {
  people <- length(unique(object$cohorts$subjectId))
  outcomeCounts <- data.frame(outcomeId = attr(object$outcomes, "metaData")$outcomeIds,
                              eventCount = 0,
                              personCount = 0)
  for (i in 1:nrow(outcomeCounts)) {
    outcomeCounts$eventCount[i] <- sum(object$outcomes$outcomeId == attr(object$outcomes, "metaData")$outcomeIds[i])
    outcomeCounts$personCount[i] <- length(unique(object$outcomes$rowId[object$outcomes$outcomeId == attr(object$outcomes, "metaData")$outcomeIds[i]]))
  }
  result <- list(metaData = append(append(object$metaData, attr(object$cohorts, "metaData")), attr(object$outcomes, "metaData")),
                 people = people,
                 outcomeCounts = outcomeCounts,
                 covariateCount = nrow(object$covariateRef),
                 covariateValueCount = nrow(object$covariates))
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
  printCoefmat(outcomeCounts)
  writeLines("")
  writeLines("Covariates:")
  writeLines(paste("Number of covariates:", x$covariateCount))
  writeLines(paste("Number of non-zero covariate values:", x$covariateValueCount))
}

#' Extract covariate names
#'
#' @description
#' Extracts covariate names using a regular-expression.
#'
#' @details
#' This function extracts covariate names that match a regular-expression for a
#' \code{plpData} or \code{covariateData} object.
#'
#' @param object    An R object of type \code{plpData} or \code{covariateData}.
#' @param pattern   A regular expression with which to name covariate names
#'
#' @return
#' Returns a \code{data.frame} containing information about covariates that match a regular
#' expression.  This \code{data.frame} has the following columns: \describe{
#' \item{covariateId}{Numerical identifier for use in model fitting using these covariates}
#' \item{covariateName}{Text identifier} \item{analysisId}{Analysis identifier} \item{conceptId}{OMOP
#' common data model concept identifier, or 0} }
#'
#' @export
grepCovariateNames <- function(pattern, object) {
  if (is.null(object$covariateRef)) {
    stop("object does not contain a covariateRef")
  }
  select <- ffbase::ffwhich(object$covariateRef, grepl(pattern, covariateName))
  if (is.null(select)) {
    data.frame(covariateId = numeric(0),
               covariateName = character(0),
               analysisID = numeric(0),
               conceptId = numeric(0))
  } else {
    ff::as.ram(object$covariateRef[select, ])
  }
}

#' Insert a population into a database
#'
#' @details
#' Inserts a population table into a database. The table in the database will have the same structure as the
#' 'cohort' table in the Common Data Model.
#'
#' @param population                   Either an object of type \code{plpData} or a population object generated by functions
#'                                     like \code{createStudyPopulation}.
#' @param cohortIds                    The IDs to be used for the treated and comparator cohort, respectively.
#' @param connectionDetails            An R object of type\cr\code{connectionDetails} created using the
#'                                     function \code{createConnectionDetails} in the
#'                                     \code{DatabaseConnector} package.
#' @param cohortDatabaseSchema         The name of the database schema where the data will be written.
#'                                     Requires write permissions to this database. On SQL
#'                                     Server, this should specifiy both the database and the schema,
#'                                     so for example 'cdm_instance.dbo'.
#' @param cohortTable                  The name of the table in the database schema  where the data will be written.
#' @param createTable                  Should a new table be created? If not, the data will be inserted into an existing
#'                                     table.
#' @param dropTableIfExists            If \code{createTable = TRUE} and the table already exists it will be overwritten.
#' @param cdmVersion                   Define the OMOP CDM version used: currently support "4" and "5".
#'
#' @export
insertDbPopulation <- function(population,
                               cohortIds = 1,
                               connectionDetails,
                               cohortDatabaseSchema,
                               cohortTable = "cohort",
                               createTable = FALSE,
                               dropTableIfExists = TRUE,
                               cdmVersion = "5") {
  if (methods::is(population, "plpData")) {
    population = population$cohorts
  }
  ids <- population$cohort
  population <- population[, c("subjectId", "cohortStartDate")]
  if (cdmVersion == "4"){
    population$cohortConceptId <- ids
  } else {
    population$cohortDefinitionId <- ids
  }
  population$cohortEndDate <- NA
  colnames(population) <- SqlRender::camelCaseToSnakeCase(colnames(population))
  connection <- DatabaseConnector::connect(connectionDetails)
  writeLines(paste("Writing", nrow(population), "rows to", paste(cohortDatabaseSchema, cohortTable, sep = ".")))
  start <- Sys.time()
  if (!createTable) {
    if (cdmVersion == "4") {
      sql <- "DELETE FROM @table WHERE cohort_concept_id IN (@cohort_ids);"
    } else {
      sql <- "DELETE FROM @table WHERE cohort_definition_id IN (@cohort_ids);"
    }
    sql <- SqlRender::renderSql(sql,
                                table = paste(cohortDatabaseSchema, cohortTable, sep = "."),
                                cohort_ids = cohortIds)$sql
    sql <- SqlRender::translateSql(sql, targetDialect = dbms)$sql
    DatabaseConnector::executeSql(connection = connection, sql = sql, progressBar = FALSE, reportOverallTime = FALSE)
  }
  DatabaseConnector::insertTable(connection = connection,
                                 tableName = paste(cohortDatabaseSchema, cohortTable, sep = "."),
                                 data = population,
                                 dropTableIfExists = dropTableIfExists,
                                 createTable = createTable,
                                 tempTable = FALSE,
                                 oracleTempSchema = NULL)
  DatabaseConnector::disconnect(connection)
  delta <- Sys.time() - start
  writeLines(paste("Inserting rows took", signif(delta, 3), attr(delta, "units")))
  invisible(TRUE)
}


#' Saves the plp model
#'
#' @details
#' Saves the plp model to a user specificed folder
#'
#' @param plpModel                   A trained classifier returned by running \code{runPlp()$model}
#' @param dirPath                  A location to save the model to
#'
#' @export
savePlpModel <- function(plpModel, dirPath){
  if (missing(plpModel))
    stop("Must specify plpModel")
  if (missing(dirPath))
    stop("Must specify directory path")
  if (class(plpModel) != "plpModel")
    stop("Not a plpModel")
  
  if(!dir.exists(dirPath)) dir.create(dirPath)
  
  
  # If model is saved on hard drive move it...
  #============================================================
  moveFile <- moveHdModel(plpModel, dirPath )
  if(!moveFile){
    ParallelLogger::logError('Moving model files error')
  }
  #============================================================
    

  # if deep (keras) then save hdfs
  if(attr(plpModel, 'type')%in%c('deep', 'deepMulti','deepEnsemble')){
    
    if(attr(plpModel, 'type')=='deepEnsemble'){
      tryCatch(
        {#saveRDS(plpModel, file = file.path(dirPath,  "deepEnsemble_model.rds"))
          for (i in seq(plpModel$modelSettings$modelParameters$numberOfEnsembleNetwork)){
          model<-keras::serialize_model(plpModel$model[[i]], include_optimizer = TRUE)
          keras::save_model_hdf5(model, filepath = file.path(dirPath, "keras_model",i))
        }},error=function(e) NULL
      )
    }
    if(attr(plpModel, 'type')=='deep'){
      keras::save_model_hdf5(plpModel$model, filepath = file.path(dirPath, "keras_model"))
    }
    if(attr(plpModel, 'type')=='deepMulti'){
      saveRDS(attr(plpModel, 'inputs'), file = file.path(dirPath,  "inputs_attr.rds"))
    }
  } else {  
  saveRDS(plpModel$model, file = file.path(dirPath, "model.rds"))
  }
  saveRDS(plpModel$predict, file = file.path(dirPath, "transform.rds"))
  saveRDS(plpModel$index, file = file.path(dirPath, "index.rds"))
  saveRDS(plpModel$trainCVAuc, file = file.path(dirPath, "trainCVAuc.rds"))
  saveRDS(plpModel$hyperParamSearch, file = file.path(dirPath, "hyperParamSearch.rds"))
  saveRDS(plpModel$modelSettings, file = file.path(dirPath,  "modelSettings.rds"))
  saveRDS(plpModel$metaData, file = file.path(dirPath, "metaData.rds"))
  saveRDS(plpModel$populationSettings, file = file.path(dirPath, "populationSettings.rds"))
  saveRDS(plpModel$trainingTime, file = file.path(dirPath,  "trainingTime.rds"))
  saveRDS(plpModel$varImp, file = file.path(dirPath,  "varImp.rds"))
  saveRDS(plpModel$dense, file = file.path(dirPath,  "dense.rds"))
  saveRDS(plpModel$cohortId, file = file.path(dirPath,  "cohortId.rds"))
  saveRDS(plpModel$outcomeId, file = file.path(dirPath,  "outcomeId.rds"))
  saveRDS(plpModel$analysisId, file = file.path(dirPath,  "analysisId.rds"))
  #if(!is.null(plpModel$covariateMap))
  saveRDS(plpModel$covariateMap, file = file.path(dirPath,  "covariateMap.rds"))
  
  attributes <- list(type=attr(plpModel, 'type'), predictionType=attr(plpModel, 'predictionType') )
  saveRDS(attributes, file = file.path(dirPath,  "attributes.rds"))
  
  
}

moveHdModel <- function(plpModel, dirPath ){
  #==================================================================
  # if python then move pickle
  #==================================================================
  if(attr(plpModel, 'type') %in% c('pythonOld','pythonReticulate', 'pythonAuto') ){
    if(!dir.exists(file.path(dirPath,'python_model')))
      dir.create(file.path(dirPath,'python_model'))
    for(file in dir(plpModel$model)){   #DOES THIS CORRECTLY TRANSFER AUTOENCODER BITS?
      file.copy(file.path(plpModel$model,file), 
                file.path(dirPath,'python_model'), overwrite=TRUE,  recursive = FALSE,
                copy.mode = TRUE, copy.date = FALSE)
    }
  }
  
  #==================================================================
  # if sagemaker then move pickle
  #==================================================================
  if(attr(plpModel, 'type') =='sagemaker'){
    if(!dir.exists(file.path(dirPath,'sagemaker_model')))
      dir.create(file.path(dirPath,'sagemaker_model'))
    for(file in dir(plpModel$model$loc)){
      file.copy(file.path(plpModel$model$loc,file), 
                file.path(dirPath,'sagemaker_model'), overwrite=TRUE,  recursive = FALSE,
                copy.mode = TRUE, copy.date = FALSE)
    }
  }
  
  #==================================================================
  # if knn then move model
  #==================================================================
  if(attr(plpModel, 'type') =='knn'){
    if(!dir.exists(file.path(dirPath,'knn_model')))
      dir.create(file.path(dirPath,'knn_model'))
    for(file in dir(plpModel$model)){
      file.copy(file.path(plpModel$model,file), 
                file.path(dirPath,'knn_model'), overwrite=TRUE,  recursive = FALSE,
                copy.mode = TRUE, copy.date = FALSE)
    }
  }
  
  return(TRUE)
}

#' loads the plp model
#'
#' @details
#' Loads a plp model that was saved using \code{savePlpModel()}
#'
#' @param dirPath                  The location of the model
#'
#' @export
loadPlpModel <- function(dirPath) {
  if (!file.exists(dirPath))
    stop(paste("Cannot find folder", dirPath))
  if (!file.info(dirPath)$isdir)
    stop(paste("Not a folder", dirPath))
  
  hyperParamSearch <- tryCatch(readRDS(file.path(dirPath, "hyperParamSearch.rds")),
                               error=function(e) NULL)
  # add in these as they got dropped
  outcomeId <- tryCatch(readRDS(file.path(dirPath, "outcomeId.rds")),
                        error=function(e) NULL)
  cohortId <- tryCatch(readRDS(file.path(dirPath, "cohortId.rds")),
                       error=function(e) NULL)  
  dense <- tryCatch(readRDS(file.path(dirPath, "dense.rds")),
                    error=function(e) NULL)  
  covariateMap <- tryCatch(readRDS(file.path(dirPath, "covariateMap.rds")),
                           error=function(e) NULL) 
  analysisId <- tryCatch(readRDS(file.path(dirPath, "analysisId.rds")),
                           error=function(e) NULL) 
  
  if(file.exists(file.path(dirPath, "keras_model"))){
    model <- keras::load_model_hdf5(file.path(dirPath, "keras_model"))
  } else {  
    model <- readRDS(file.path(dirPath, "model.rds"))
  }
  
  result <- list(model = model,
                 modelSettings = readRDS(file.path(dirPath, "modelSettings.rds")),
                 hyperParamSearch = hyperParamSearch,
                 trainCVAuc = readRDS(file.path(dirPath, "trainCVAuc.rds")),
                 metaData = readRDS(file.path(dirPath, "metaData.rds")),
                 populationSettings= readRDS(file.path(dirPath, "populationSettings.rds")),
                 outcomeId = outcomeId,
                 cohortId = cohortId,
                 varImp = readRDS(file.path(dirPath, "varImp.rds")),
                 trainingTime = readRDS(file.path(dirPath, "trainingTime.rds")),
                 covariateMap =covariateMap,
                 predict = readRDS(file.path(dirPath, "transform.rds")),
                 index = readRDS(file.path(dirPath, "index.rds")),
                 dense = dense,
                 analysisId = analysisId)
  
  #attributes <- readRDS(file.path(dirPath, "attributes.rds"))
  attributes <- readRDS(file.path(dirPath, "attributes.rds"))
  attr(result, 'type') <- attributes$type
  attr(result, 'predictionType') <- attributes$predictionType
  class(result) <- "plpModel"
  
  # update the model location to the load dirPath
  result <- updateModelLocation(result, dirPath)
    
  return(result)
}

updateModelLocation  <- function(plpModel, dirPath){
  type <- attr(plpModel, 'type')
  # if python update the location
  if( type %in% c('pythonOld','pythonReticulate', 'pythonAuto')){
    plpModel$model <- file.path(dirPath,'python_model')
    plpModel$predict <- createTransform(plpModel)
  }
  if( type =='sagemaker'){
    plpModel$model$loc <- file.path(dirPath,'sagemaker_model')
    plpModel$predict <- createTransform(plpModel)
  }
  # if knn update the locaiton - TODO !!!!!!!!!!!!!!
  if( type =='knn'){
    plpModel$model <- file.path(dirPath,'knn_model')
    plpModel$predict <- createTransform(plpModel)
  }
  if( type =='deep' ){
    plpModel$predict <- createTransform(plpModel)
  }
  if( type =='deepEnsemble' ){
    plpModel$predict <- createTransform(plpModel)
  }
  if( type =='deepMulti'){
    attr(plpModel, 'inputs') <- tryCatch(readRDS(file.path(dirPath, "inputs_attr.rds")),
                                       error=function(e) NULL) 
    plpModel$predict <- createTransform(plpModel)
    
  }
  
  return(plpModel) 
}


#' Saves the prediction dataframe to RDS
#'
#' @details
#' Saves the prediction data frame returned by predict.R to an RDS file and returns the fileLocation where the prediction is saved
#'
#' @param prediction                   The prediciton data.frame
#' @param dirPath                     The directory to save the prediction RDS
#' @param fileName                    The name of the RDS file that will be saved in dirPath
#' 
#' @export
savePrediction <- function(prediction, dirPath, fileName='prediction.rds'){
  #TODO check inupts
  saveRDS(prediction, file=file.path(dirPath,fileName))
  
  return(file.path(dirPath,fileName))
}

#' Loads the prediciton dataframe to csv
#'
#' @details
#' Loads the prediciton  RDS file
#'
#' @param fileLocation                     The location with the saved prediction
#' 
#' @export
loadPrediction <- function(fileLocation){
  #TODO check inupts
  prediction <- readRDS(file=fileLocation)
  return(prediction)
}

#' Saves the result from runPlp into the location directory
#'
#' @details
#' Saves the result from runPlp into the location directory
#'
#' @param result                      The result of running runPlp()
#' @param dirPath                     The directory to save the csv
#' 
#' @export
savePlpResult <- function(result, dirPath){
  if (missing(result))
    stop("Must specify runPlp output")
  if (missing(dirPath))
    stop("Must specify directory location")
  #if (class(plpModel) != "plpModel")
  #  stop("Not a plpModel")
  
  if(!dir.exists(dirPath)) dir.create(dirPath, recursive = T)
  
  savePlpModel(result$model, dirPath=file.path(dirPath,'model') )
  saveRDS(result$analysisRef, file = file.path(dirPath, "analysisRef.rds"))
  saveRDS(result$inputSetting, file = file.path(dirPath, "inputSetting.rds"))
  saveRDS(result$executionSummary, file = file.path(dirPath, "executionSummary.rds"))
  saveRDS(result$prediction, file = file.path(dirPath, "prediction.rds"))
  saveRDS(result$performanceEvaluation, file = file.path(dirPath, "performanceEvaluation.rds"))
  #saveRDS(result$performanceEvaluationTrain, file = file.path(dirPath, "performanceEvaluationTrain.rds"))
  saveRDS(result$covariateSummary, file = file.path(dirPath, "covariateSummary.rds"))
  
  
}

#' Loads the evalaution dataframe
#'
#' @details
#' Loads the evaluation 
#'
#' @param dirPath                     The directory where the evaluation was saved
#' 
#' @export
loadPlpResult <- function(dirPath){
  if (!file.exists(dirPath))
    stop(paste("Cannot find folder", dirPath))
  if (!file.info(dirPath)$isdir)
    stop(paste("Not a folder", dirPath))
  
  
  result <- list(model = loadPlpModel(file.path(dirPath, "model")),
                 analysisRef = readRDS(file.path(dirPath, "analysisRef.rds")),
                 inputSetting = readRDS(file.path(dirPath, "inputSetting.rds")),
                 executionSummary = readRDS(file.path(dirPath, "executionSummary.rds")),
                 prediction = readRDS(file.path(dirPath, "prediction.rds")),
                 performanceEvaluation = readRDS(file.path(dirPath, "performanceEvaluation.rds")),
                 #performanceEvaluationTrain= readRDS(file.path(dirPath, "performanceEvaluationTrain.rds")),
                 covariateSummary = readRDS(file.path(dirPath, "covariateSummary.rds"))
  )
  class(result) <- "runPlp"
  
  return(result)
  
}

# MIGHT DELETE THIS:
# Insert cohort definitions into package
saveCirceDefinition <- function (definitionId, name = NULL,
                                            baseUrl = "https://enter_address")
{
  url <- paste(baseUrl, "cohortdefinition", definitionId, sep = "/")
  json <- RCurl::getURL(url, .opts = list(ssl.verifypeer=FALSE))
  parsedJson <- RJSONIO::fromJSON(json)
  if (is.null(name)) {
    name <- parsedJson$name
  }
  expression <- parsedJson$expression
  parsedExpression <- RJSONIO::fromJSON(parsedJson$expression)
  jsonBody <- RJSONIO::toJSON(list(expression = parsedExpression),
                              digits = 23)
  httpheader <- c(Accept = "application/json; charset=UTF-8",
                  `Content-Type` = "application/json")
  url <- paste(baseUrl, "cohortdefinition", "sql", sep = "/")
  cohortSqlJson <- RCurl::postForm(url, .opts = list(httpheader = httpheader,
                                                     postfields = jsonBody,
                                                     ssl.verifypeer=FALSE))
  sql <- RJSONIO::fromJSON(cohortSqlJson)

  return(list(json=expression,
              sql=sql))
}


