# @file ExtractData.R
#
# Copyright 2025 Observational Health Data Sciences and Informatics
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


#' createRestrictPlpDataSettings define extra restriction settings when calling getPlpData
#'
#' @description
#' This function creates the settings used to restrict the target cohort when calling getPlpData
#' @details
#' Users need to specify the extra restrictions to apply when downloading the target cohort
#'
#' @param studyStartDate               A calendar date specifying the minimum date that a cohort index
#'                                     date can appear. Date format is 'yyyymmdd'.
#' @param studyEndDate                 A calendar date specifying the maximum date that a cohort index
#'                                     date can appear. Date format is 'yyyymmdd'. Important: the study
#'                                     end data is also used to truncate risk windows, meaning no outcomes
#'                                     beyond the study end date will be considered.
#' @param firstExposureOnly            Should only the first exposure per subject be included? Note that
#'                                     this is typically done in the \code{createStudyPopulation} function,
#'                                     but can already be done here for efficiency reasons.
#' @param washoutPeriod                The mininum required continuous observation time prior to index
#'                                     date for a person to be included in the at risk cohort. Note that
#'                                     this is typically done in the \code{createStudyPopulation} function,
#'                                     but can already be done here for efficiency reasons.
#' @param sampleSize                       If not NULL, the number of people to sample from the target cohort
#'
#' @return
#' A setting object of class \code{restrictPlpDataSettings} containing a list of
#' the settings: \itemize{
#' \item{`studyStartDate`: A calendar date specifying the minimum date that a cohort index date can appear}
#' \item{`studyEndDate`: A calendar date specifying the maximum date that a cohort index date can appear}
#' \item{`firstExposureOnly`: Should only the first exposure per subject be included}
#' \item{`washoutPeriod`: The mininum required continuous observation time prior to index date for a person to be included in the at risk cohort}
#' \item{`sampleSize`: If not NULL, the number of people to sample from the target cohort}
#' }
#' @examples
#' # restrict to 2010, first exposure only, require washout period of 365 day
#' # and sample 1000 people
#' createRestrictPlpDataSettings(studyStartDate = "20100101", studyEndDate = "20101231", 
#' firstExposureOnly = TRUE, washoutPeriod = 365, sampleSize = 1000)
#' @export
createRestrictPlpDataSettings <- function(
    studyStartDate = "",
    studyEndDate = "",
    firstExposureOnly = FALSE,
    washoutPeriod = 0,
    sampleSize = NULL) {
  if (studyStartDate != "" && regexpr("^[12][0-9]{3}[01][0-9][0-3][0-9]$", studyStartDate) == -1) {
    stop("Study start date must have format YYYYMMDD")
  }
  if (studyEndDate != "" && regexpr("^[12][0-9]{3}[01][0-9][0-3][0-9]$", studyEndDate) == -1) {
    stop("Study end date must have format YYYYMMDD")
  }

  # add input checks
  checkIsClass(sampleSize, c("integer", "numeric", "NULL"))

  result <- list(
    studyStartDate = studyStartDate,
    studyEndDate = studyEndDate,
    firstExposureOnly = firstExposureOnly,
    washoutPeriod = washoutPeriod,
    sampleSize = sampleSize
  )

  class(result) <- "restrictPlpDataSettings"
  return(result)
}

#' Create a setting that holds the details about the cdmDatabase connection for data extraction
#'
#' @details
#' This function simply stores the settings for communicating with the cdmDatabase when extracting
#' the target cohort and outcomes
#'
#' @param connectionDetails              An R object of type \code{connectionDetails} created using the
#'                                       function \code{createConnectionDetails} in the
#'                                       \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema              The name of the database schema that contains the OMOP CDM
#'                                       instance. Requires read permissions to this database. On SQL
#'                                       Server, this should specifiy both the database and the schema,
#'                                       so for example 'cdm_instance.dbo'.
#' @param cdmDatabaseName                A string with the name of the database - this is used in the shiny app and when externally validating models to name the result list and to specify the folder name when saving validation results (defaults to cdmDatabaseSchema if not specified)
#' @param cdmDatabaseId                  A string with a unique identifier for the database and version - this is stored in the plp object for future reference and used by the shiny app (defaults to cdmDatabaseSchema if not specified)
#' @param tempEmulationSchema            For dmbs like Oracle only: the name of the database schema where you
#'                                       want all temporary tables to be managed. Requires
#'                                       create/insert permissions to this database.
#' @param cohortDatabaseSchema           The name of the database schema that is the location where the
#'                                       target cohorts are available.  Requires read
#'                                       permissions to this database.
#' @param cohortTable                    The tablename that contains the target cohorts.  Expectation is cohortTable
#'                                       has format of COHORT table: COHORT_DEFINITION_ID, SUBJECT_ID,
#'                                       COHORT_START_DATE, COHORT_END_DATE.
#' @param outcomeDatabaseSchema          The name of the database schema that is the location where the
#'                                       data used to define the outcome cohorts is available. Requires read permissions to
#'                                       this database.
#' @param outcomeTable                   The tablename that contains the outcome cohorts.  Expectation is
#'                                       outcomeTable has format of COHORT table: COHORT_DEFINITION_ID,
#'                                       SUBJECT_ID, COHORT_START_DATE, COHORT_END_DATE.
#' @param targetId                       An integer specifying the cohort id for the target cohort
#' @param outcomeIds                      A single integer or vector of integers specifying the cohort ids for the outcome cohorts
#' @param cdmVersion                     Define the OMOP CDM version used: currently support "4" and "5".
#' @param cohortId                       (depreciated: use targetId) old input for the target cohort id
#'
#' @return
#' A list with the the database specific settings: \itemize{
#' \item{`connectionDetails`: An R object of type \code{connectionDetails} created using the function \code{createConnectionDetails} in the \code{DatabaseConnector} package.}
#' \item{`cdmDatabaseSchema`: The name of the database schema that contains the OMOP CDM instance.}
#' \item{`cdmDatabaseName`: A string with the name of the database - this is used in the shiny app and when externally validating models to name the result list and to specify the folder name when saving validation results (defaults to cdmDatabaseSchema if not specified).}
#' \item{`cdmDatabaseId`: A string with a unique identifier for the database and version - this is stored in the plp object for future reference and used by the shiny app (defaults to cdmDatabaseSchema if not specified).}
#' \item{`tempEmulationSchema`: The name of a databae schema where you want all temporary tables to be managed. Requires create/insert permissions to this database.}
#' \item{`cohortDatabaseSchema`: The name of the database schema that is the location where the target cohorts are available. Requires read permissions to this schema.}
#' \item{`cohortTable`: The tablename that contains the target cohorts. Expectation is cohortTable has format of COHORT table: COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE, COHORT_END_DATE.}
#' \item{`outcomeDatabaseSchema`: The name of the database schema that is the location where the data used to define the outcome cohorts is available. Requires read permissions to this database.}
#' \item{`outcomeTable`: The tablename that contains the outcome cohorts. Expectation is outcomeTable has format of COHORT table: COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE, COHORT_END_DATE.}
#' \item{`targetId`: An integer specifying the cohort id for the target cohort}
#' \item{`outcomeIds`: A single integer or vector of integers specifying the cohort ids for the outcome cohorts}
#' \item{`cdmVersion`: Define the OMOP CDM version used: currently support "4" and "5".}
#' }
#' @examplesIf rlang::is_installed("Eunomia") && rlang::is_installed("curl") && curl::has_internet()
#' \donttest{ \dontshow{ # takes too long }
#' connectionDetails <- Eunomia::getEunomiaConnectionDetails()
#' # create the database details for Eunomia example database
#' createDatabaseDetails(
#'   connectionDetails = connectionDetails,
#'   cdmDatabaseSchema = "main",
#'   cdmDatabaseName = "main",
#'   cohortDatabaseSchema = "main",
#'   cohortTable = "cohort",
#'   outcomeDatabaseSchema = "main",
#'   outcomeTable = "cohort",
#'   targetId = 1, # users of celecoxib
#'   outcomeIds = 3, # GIbleed
#'   cdmVersion = 5)
#' }
#' @export
createDatabaseDetails <- function(
    connectionDetails,
    cdmDatabaseSchema,
    cdmDatabaseName,
    cdmDatabaseId, # added for strategus
    tempEmulationSchema = cdmDatabaseSchema,
    cohortDatabaseSchema = cdmDatabaseSchema,
    cohortTable = "cohort",
    outcomeDatabaseSchema = cohortDatabaseSchema,
    outcomeTable = cohortTable,
    targetId = NULL,
    outcomeIds = NULL,
    cdmVersion = 5,
    cohortId = NULL) {
  if (is.null(targetId)) {
    if (!is.null(cohortId)) {
      ParallelLogger::logWarn("cohortId has been depreciated.  Please use targetId.")
      targetId <- cohortId
    }
  }

  if (missing(cdmDatabaseName)) {
    ParallelLogger::logInfo("No cdm database name entered so using cdmDatabaseSchema")
    cdmDatabaseName <- removeInvalidString(cdmDatabaseSchema)
  }
  if (missing(cdmDatabaseId)) {
    ParallelLogger::logInfo("No cdm database id entered so using cdmDatabaseSchema - if cdmDatabaseSchema is the same for multiple different databases, please use cdmDatabaseId to specify a unique identifier for the database and version")
    cdmDatabaseId <- removeInvalidString(cdmDatabaseSchema)
  }

  if (length(cdmDatabaseId) == 0) {
    stop("cdmDatabaseId must be a string with length > 0")
  }

  # check to make sure cdmDatabaseId is not an int as that will cause issues
  if (!inherits(cdmDatabaseId, "character")) {
    ParallelLogger::logInfo("cdmDatabaseId is not a string - this will cause issues when inserting into a result database so casting it")
    cdmDatabaseId <- as.character(cdmDatabaseId)
  }

  result <- list(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cdmDatabaseName = cdmDatabaseName,
    cdmDatabaseId = cdmDatabaseId,
    tempEmulationSchema = tempEmulationSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTable,
    outcomeDatabaseSchema = outcomeDatabaseSchema,
    outcomeTable = outcomeTable,
    targetId = targetId,
    outcomeIds = outcomeIds,
    cdmVersion = cdmVersion
  )

  attr(result, "cdmDatabaseName") <- cdmDatabaseName
  class(result) <- "databaseDetails"
  return(result)
}


plpDataObjectDoc <- function() {
  paste0(r"(
 An object of type `plpData`, containing information on the cohorts, their
 outcomes, and baseline covariates. Information about multiple outcomes can be
 captured at once for efficiency reasons. This object is a list with the
 following components: \describe{ \item{outcomes}{A data frame listing the 
 outcomes per person, including the time to event, and the outcome id} 
 \item{cohorts}{A data frame listing the persons in each cohort, listing their
 exposure status as well as the time to the end of the observation period and 
 time to the end of the cohort} \item{covariateData}{An Andromeda object created 
 with the `FeatureExtraction` package. This object contains the following items: 
 \describe{ \item{covariates}{An Andromeda table listing the covariates per 
 person in the two cohorts. This is done using a sparse representation:
 covariates with a value of 0 are omitted to save space. Usually has three 
 columns, rowId, covariateId and covariateValue'.} \item{covariateRef}{An 
 Andromeda table describing the covariates that have been extracted.} 
 \item{AnalysisRef}{An Andromeda table with information about which analysisIds 
 from 'FeatureExtraction' were used.} }}})")
}
NULL

#' Extract the patient level prediction data from the server
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
#'
#' @param databaseDetails              The cdm database details created using \code{createDatabaseDetails()}
#' @param covariateSettings            An object of type \code{covariateSettings} or a list of such objects as created using the
#'                                     \code{createCovariateSettings} function in the
#'                                     \code{FeatureExtraction} package.
#' @param restrictPlpDataSettings Extra settings to apply to the target population while extracting data.
#' Created using \code{createRestrictPlpDataSettings()}. This is optional.
#'
#' @return 'r plpDataObjectDoc()`
#' @examplesIf rlang::is_installed("Eunomia") && rlang::is_installed("curl") && curl::has_internet()
#' \donttest{ \dontshow{ # takes too long }
#'  # use Eunomia database
#'  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
#'  Eunomia::createCohorts(connectionDetails)
#'  outcomeId <- 3 # GIbleed
#'  databaseDetails <- createDatabaseDetails(
#'    connectionDetails = connectionDetails,
#'    cdmDatabaseSchema = "main",
#'    cdmDatabaseName = "main",
#'    cohortDatabaseSchema = "main",
#'    cohortTable = "cohort",
#'    outcomeDatabaseSchema = "main",
#'    outcomeTable = "cohort",
#'    targetId = 1,
#'    outcomeIds = outcomeId,
#'    cdmVersion = 5
#'  )
#'
#'  covariateSettings <- FeatureExtraction::createCovariateSettings(
#'    useDemographicsAge = TRUE,
#'    useDemographicsGender = TRUE,
#'    useConditionOccurrenceAnyTimePrior = TRUE
#'  )
#'
#'  plpData <- getPlpData(
#'    databaseDetails = databaseDetails,
#'    covariateSettings = covariateSettings,
#'    restrictPlpDataSettings = createRestrictPlpDataSettings()
#'  )
#' }
#' @export
getPlpData <- function(
    databaseDetails,
    covariateSettings,
    restrictPlpDataSettings = NULL) {
  checkIsClass(databaseDetails, "databaseDetails")
  if (!inherits(covariateSettings, "list")) {
    checkIsClass(covariateSettings, "covariateSettings")
  } else {
    for (i in seq_along(covariateSettings)) {
      checkIsClass(covariateSettings[[i]], "covariateSettings")
    }
  }
  if (!is.null(restrictPlpDataSettings)) {
    checkIsClass(restrictPlpDataSettings, "restrictPlpDataSettings")
  }



  if (is.null(databaseDetails$targetId)) {
    stop("User must input targetId")
  }
  if (length(databaseDetails$targetId) > 1) {
    stop("Currently only supports one targetId at a time")
  }
  if (is.null(databaseDetails$outcomeIds)) {
    stop("User must input outcomeIds")
  }
  # ToDo: add other checks the inputs are valid

  connection <- DatabaseConnector::connect(databaseDetails$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  dbms <- databaseDetails$connectionDetails$dbms

  ParallelLogger::logTrace("\nConstructing the at risk cohort")
  if (!is.null(restrictPlpDataSettings$sampleSize)) writeLines(paste("\n Sampling ", restrictPlpDataSettings$sampleSize, " people"))

  pathToSql <- system.file(
    paste("sql/", "sql_server",
      sep = ""
    ),
    "CreateCohorts.sql",
    package = "PatientLevelPrediction"
  )

  renderedSql <- readChar(pathToSql, file.info(pathToSql)$size)
  renderArgs <- list(
    sql = renderedSql,
    cdm_database_schema = databaseDetails$cdmDatabaseSchema,
    cohort_database_schema = databaseDetails$cohortDatabaseSchema,
    cohort_table = databaseDetails$cohortTable,
    cdm_version = databaseDetails$cdmVersion,
    target_id = databaseDetails$targetId
  )

  if (!is.null(restrictPlpDataSettings)) {
    renderArgs$study_start_date <- restrictPlpDataSettings$studyStartDate
    renderArgs$study_end_date <- restrictPlpDataSettings$studyEndDate
    renderArgs$first_only <- restrictPlpDataSettings$firstExposureOnly
    renderArgs$washout_period <- restrictPlpDataSettings$washoutPeriod
    renderArgs$use_sample <- !is.null(restrictPlpDataSettings$sampleSize)
    renderArgs$sample_number <- restrictPlpDataSettings$sampleSize
  }

  renderedSql <- do.call(SqlRender::render, renderArgs)

  renderedSql <- SqlRender::translate(
    sql = renderedSql,
    targetDialect = dbms,
    tempEmulationSchema = databaseDetails$tempEmulationSchema
  )

  DatabaseConnector::executeSql(connection, renderedSql)

  ParallelLogger::logTrace("Fetching cohorts from server")
  start <- Sys.time()

  pathToSql <- system.file(
    paste("sql/", "sql_server",
      sep = ""
    ),
    "GetCohorts.sql",
    package = "PatientLevelPrediction"
  )

  cohortSql <- readChar(pathToSql, file.info(pathToSql)$size)

  cohortSql <- SqlRender::render(
    sql = cohortSql,
    cdm_version = databaseDetails$cdmVersion
  )

  cohortSql <- SqlRender::translate(
    sql = cohortSql,
    targetDialect = dbms,
    tempEmulationSchema = databaseDetails$tempEmulationSchema
  )
  cohorts <- DatabaseConnector::querySql(connection, cohortSql)
  colnames(cohorts) <- SqlRender::snakeCaseToCamelCase(colnames(cohorts))
  metaDataCohort <- list(targetId = databaseDetails$targetId)

  if (nrow(cohorts) == 0) {
    stop("Target population is empty")
  }

  delta <- Sys.time() - start
  ParallelLogger::logTrace(paste("Loading cohorts took", signif(delta, 3), attr(delta, "units")))

  covariateData <- FeatureExtraction::getDbCovariateData(
    connection = connection,
    tempEmulationSchema = databaseDetails$tempEmulationSchema,
    cdmDatabaseSchema = databaseDetails$cdmDatabaseSchema,
    cdmVersion = databaseDetails$cdmVersion,
    cohortTable = "#cohort_person",
    cohortTableIsTemp = TRUE,
    rowIdField = "row_id",
    covariateSettings = covariateSettings
  )
  if (inherits(covariateData, "RSQLiteConnection")) {
    # add indexes for tidyCov + covariate summary
    Andromeda::createIndex(covariateData$covariates, c("rowId"),
      indexName = "covariates_rowId"
    )
    Andromeda::createIndex(covariateData$covariates, c("covariateId"),
      indexName = "covariates_covariateId"
    )
    Andromeda::createIndex(covariateData$covariates, c("covariateId", "covariateValue"),
      indexName = "covariates_covariateId_value"
    )
  }

  if (max(databaseDetails$outcomeIds) != -999) {
    ParallelLogger::logTrace("Fetching outcomes from server")
    start <- Sys.time()

    pathToSql <- system.file(
      paste("sql/", "sql_server",
        sep = ""
      ),
      "GetOutcomes.sql",
      package = "PatientLevelPrediction"
    )

    outcomeSql <- readChar(pathToSql, file.info(pathToSql)$size)

    outcomeSql <- SqlRender::render(
      sql = outcomeSql,
      cdm_database_schema = databaseDetails$cdmDatabaseSchema,
      outcome_database_schema = databaseDetails$outcomeDatabaseSchema,
      outcome_table = databaseDetails$outcomeTable,
      outcome_ids = databaseDetails$outcomeIds,
      cdm_version = databaseDetails$cdmVersion
    )

    outcomeSql <- SqlRender::translate(
      sql = outcomeSql,
      targetDialect = dbms,
      tempEmulationSchema = databaseDetails$tempEmulationSchema
    )

    outcomes <- DatabaseConnector::querySql(connection, outcomeSql)
    colnames(outcomes) <- SqlRender::snakeCaseToCamelCase(colnames(outcomes))
    metaDataOutcome <- data.frame(outcomeIds = databaseDetails$outcomeIds)
    attr(outcomes, "metaData") <- metaDataOutcome
    if (nrow(outcomes) == 0) {
      stop("No Outcomes")
    }

    metaDataCohort$attrition <- getCounts2(cohorts, outcomes, "Original cohorts")
    attr(cohorts, "metaData") <- metaDataCohort

    delta <- Sys.time() - start
    ParallelLogger::logTrace(paste("Loading outcomes took", signif(delta, 3), attr(delta, "units")))
  } else {
    outcomes <- NULL
  }


  # Remove temp tables:
  pathToSql <- system.file(
    paste("sql/", "sql_server",
      sep = ""
    ),
    "RemoveCohortTempTables.sql",
    package = "PatientLevelPrediction"
  )

  removeSql <- readChar(pathToSql, file.info(pathToSql)$size)
  removeSql <- SqlRender::translate(
    sql = removeSql,
    targetDialect = dbms,
    tempEmulationSchema = databaseDetails$tempEmulationSchema
  )

  DatabaseConnector::executeSql(connection, removeSql, progressBar = FALSE, reportOverallTime = FALSE)

  metaData <- covariateData$metaData
  metaData$databaseDetails <- databaseDetails
  metaData$databaseDetails$connectionDetails <- NULL
  metaData$databaseDetails$connection <- NULL
  metaData$restrictPlpDataSettings <- restrictPlpDataSettings
  metaData$covariateSettings <- covariateSettings

  # create the temporal settings (if temporal use)
  timeReference <- NULL
  if (!is.null(covariateSettings$temporal)) {
    if (covariateSettings$temporal) {
      # make sure time days populated
      if (length(covariateSettings$temporalStartDays) > 0) {
        timeReference <- data.frame(
          timeId = seq_along(covariateSettings$temporalStartDays),
          startDay = covariateSettings$temporalStartDays,
          endDay = covariateSettings$temporalEndDays
        )
      }
    }
  }


  result <- list(
    cohorts = cohorts,
    outcomes = outcomes,
    covariateData = covariateData,
    timeRef = timeReference,
    metaData = metaData
  )

  class(result) <- "plpData"
  return(result)
}

#' Print a plpData object
#' @method print plpData
#' @param x The plpData object to print
#' @param ... Additional arguments
#' @return A message describing the object
#' @examples  
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n=10)
#' print(plpData)
#' @export
print.plpData <- function(x, ...) {
  writeLines("plpData object")
  writeLines("")
  writeLines(paste("At risk concept ID:", attr(x$cohorts, "metaData")$targetId))
  writeLines(paste("Outcome concept ID(s):", paste(attr(x$outcomes, "metaData")$outcomeIds, collapse = ",")))
}

#' Summarize a plpData object
#' @method summary plpData
#' @param object The plpData object to summarize
#' @param ... Additional arguments
#' @return A summary of the object containing the number of people, outcomes and covariates
#' @examples
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n=10)
#' summary(plpData)
#' @export
summary.plpData <- function(object, ...) {
  people <- length(unique(object$cohorts$subjectId))
  outcomeCounts <- data.frame(
    outcomeId = attr(object$outcomes, "metaData")$outcomeIds,
    eventCount = 0,
    personCount = 0
  )
  for (i in seq_len(nrow(outcomeCounts))) {
    outcomeCounts$eventCount[i] <- sum(object$outcomes$outcomeId == attr(object$outcomes, "metaData")$outcomeIds[i])
    outcomeCounts$personCount[i] <- length(unique(object$outcomes$rowId[object$outcomes$outcomeId == attr(object$outcomes, "metaData")$outcomeIds[i]]))
  }

  covDetails <- FeatureExtraction::summary(object$covariateData)
  result <- list(
    metaData = append(append(object$metaData, attr(object$cohorts, "metaData")), attr(object$outcomes, "metaData")),
    people = people,
    outcomeCounts = outcomeCounts,
    covariateCount = covDetails$covariateCount,
    covariateValueCount = covDetails$covariateValueCount
  )
  class(result) <- "summary.plpData"
  return(result)
}

#' Print a summary.plpData object
#' @export
#' @method print summary.plpData
#' @param x The summary.plpData object to print
#' @param ... Additional arguments
#' @return A message describing the object
#' @examples
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n=10)
#' summary <- summary(plpData)
#' print(summary)
print.summary.plpData <- function(x, ...) {
  writeLines("plpData object summary")
  writeLines("")
  writeLines(paste("At risk cohort concept ID:", x$metaData$targetId))
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

#' Create a plpData object from the Eunomia database'
#' @description
#' This function creates a plpData object from the Eunomia database. It gets
#' the connection details, creates the cohorts, and extracts the data. The cohort
#' is predicting GIbleed in new users of celecoxib.
#' @param covariateSettings A list of covariateSettings objects created using the
#' \code{createCovariateSettings} function in the \code{FeatureExtraction} package.
#' If nothing is specified covariates with age, gender, conditions and drug era are used.
#' @return `r plpDataObjectDoc()`
#' @examplesIf rlang::is_installed("Eunomia") && rlang::is_installed("curl") && curl::has_internet()
#' \donttest{ \dontshow{ # takes too long }
#' covariateSettings <- FeatureExtraction::createCovariateSettings(
#'   useDemographicsAge = TRUE,
#'   useDemographicsGender = TRUE,
#'   useConditionOccurrenceAnyTimePrior = TRUE
#' )
#' plpData <- getEunomiaPlpData(covariateSettings = covariateSettings)
#' }
#' @export
getEunomiaPlpData <- function(covariateSettings = NULL) {
  rlang::check_installed("Eunomia")
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  Eunomia::createCohorts(connectionDetails)
  outcomeId <- 3 # GIbleed

  databaseDetails <- createDatabaseDetails(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    cdmDatabaseName = "main",
    cohortDatabaseSchema = "main",
    cohortTable = "cohort",
    outcomeDatabaseSchema = "main",
    outcomeTable = "cohort",
    targetId = 1,
    outcomeIds = outcomeId,
    cdmVersion = 5
  )

  if (is.null(covariateSettings)) {
    covariateSettings <- FeatureExtraction::createCovariateSettings(
      useDemographicsAge = TRUE,
      useDemographicsGender = TRUE,
      useConditionOccurrenceLongTerm = TRUE,
      useDrugEraLongTerm = TRUE
    )
  }

  plpData <- getPlpData(
    databaseDetails = databaseDetails,
    covariateSettings = covariateSettings,
    restrictPlpDataSettings = createRestrictPlpDataSettings()
  )
  return(plpData)
}
