# @file DataLoadingSaving.R
#
# Copyright 2016 Observational Health Data Sciences and Informatics
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
#' occurring in these subjects. The at risk cohort can be identified using the drug_era table, or through
#' user-defined cohorts in a cohort table either inside the CDM instance or in a separate schema.
#' Similarly, outcomes are identified using the condition_era table or
#' through user-defined cohorts in a cohort table either inside the CDM instance or in a separate
#' schema. Covariates are automatically extracted from the appropriate tables within the CDM.
#' Important: The concepts used to define the at risk cohort must not be included in the covariates, including any
#' descendant concepts. If the \code{cohortId} arguments represent real
#' concept IDs, you can set the \code{excludeDrugsFromCovariates} argument to TRUE and automatically
#' the drugs and their descendants will be excluded from the covariates. However, if the
#' \code{cohortId} argument does not represent concept IDs, you will need to
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
#' @param outcomeIds                   A list of cohort_definition_ids used to define outcomes.
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
#' @param excludeDrugsFromCovariates   Should the target and comparator drugs (and their descendant
#'                                     concepts) be excluded from the covariates? Note that this will
#'                                     work if the drugs are actualy drug concept IDs (and not cohort
#'                                     IDs).
#' @param firstExposureOnly            Should only the first exposure per subject be included? Note that
#'                                     this is typically done in the \code{createStudyPopulation} function,
#'                                     but can already be done here for efficiency reasons.
#' @param washoutPeriod                The mininum required continuous observation time prior to index
#'                                     date for a person to be included in the at risk cohort. Note that
#'                                     this is typically done in the \code{createStudyPopulation} function,
#'                                     but can already be done here for efficiency reasons.
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
getDbPlpData <- function(connectionDetails,
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
                                  excludeDrugsFromCovariates = F,
                                  firstExposureOnly = FALSE,
                                  washoutPeriod = 0,
                                  covariateSettings) {
  if (studyStartDate != "" && regexpr("^[12][0-9]{3}[01][0-9][0-3][0-9]$", studyStartDate) == -1)
    stop("Study start date must have format YYYYMMDD")
  if (studyEndDate != "" && regexpr("^[12][0-9]{3}[01][0-9][0-3][0-9]$", studyEndDate) == -1)
    stop("Study end date must have format YYYYMMDD")
  
  connection <- DatabaseConnector::connect(connectionDetails)
  
  if (excludeDrugsFromCovariates) {
    if (cohortTable != "drug_era")
      warning("Removing drugs from covariates, but not sure if cohort IDs are valid drug concepts")
    sql <- "SELECT descendant_concept_id FROM @cdm_database_schema.concept_ancestor WHERE ancestor_concept_id IN (@cohort_id)"
    sql <- SqlRender::renderSql(sql,
                                cdm_database_schema = cdmDatabaseSchema,
                                cohort_id = cohortId)$sql
    sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
    conceptIds <- DatabaseConnector::querySql(connection, sql)
    names(conceptIds) <- SqlRender::snakeCaseToCamelCase(names(conceptIds))
    conceptIds <- conceptIds$descendantConceptId
    # TODO this needs to be edited for multi coariate setting
    covariateSettings$excludedCovariateConceptIds <- c(covariateSettings$excludedCovariateConceptIds,
                                                       conceptIds)
  }
  
  writeLines("\nConstructing the at risk cohort")
  renderedSql <- SqlRender::loadRenderTranslateSql("CreateCohorts.sql",
                                                   packageName = "PatientLevelPrediction",
                                                   dbms = connectionDetails$dbms,
                                                   oracleTempSchema = oracleTempSchema,
                                                   cdm_database_schema = cdmDatabaseSchema,
                                                   cohort_database_schema = cohortDatabaseSchema,
                                                   cohort_table = cohortTable,
                                                   cdm_version = cdmVersion,
                                                   cohort_id = cohortId,
                                                   study_start_date = studyStartDate,
                                                   study_end_date = studyEndDate,
                                                   first_only = firstExposureOnly,
                                                   washout_period = washoutPeriod)
  DatabaseConnector::executeSql(connection, renderedSql)
  
  writeLines("Fetching cohorts from server")
  start <- Sys.time()
  cohortSql <- SqlRender::loadRenderTranslateSql("GetCohorts.sql",
                                                 packageName = "PatientLevelPrediction",
                                                 dbms = connectionDetails$dbms,
                                                 oracleTempSchema = oracleTempSchema,
                                                 cdm_version = cdmVersion)
  cohorts <- DatabaseConnector::querySql(connection, cohortSql)
  colnames(cohorts) <- SqlRender::snakeCaseToCamelCase(colnames(cohorts))
  metaData.cohort <- list(cohortId = cohortId,
                   studyStartDate = studyStartDate,
                   studyEndDate = studyEndDate)
  

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
  writeLines("Fetching outcomes from server")
  start <- Sys.time()
  outcomeSql <- SqlRender::loadRenderTranslateSql("GetOutcomes.sql",
                                                  packageName = "PatientLevelPrediction",
                                                  dbms = connectionDetails$dbms,
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
  
  metaData.cohort$attrition <- getCounts(cohorts,nrow(outcomes), "Original cohorts")
  attr(cohorts, "metaData") <- metaData.cohort
  
  delta <- Sys.time() - start
  writeLines(paste("Loading outcomes took", signif(delta, 3), attr(delta, "units")))
  
  # Remove temp tables:
  renderedSql <- SqlRender::loadRenderTranslateSql("RemoveCohortTempTables.sql",
                                                   packageName = "PatientLevelPrediction",
                                                   dbms = connectionDetails$dbms,
                                                   oracleTempSchema = oracleTempSchema)
  DatabaseConnector::executeSql(connection, renderedSql, progressBar = FALSE, reportOverallTime = FALSE)
  RJDBC::dbDisconnect(connection)
  
  metaData <- covariateData$metaData
  metaData$call <- match.call()
  metaData$call$connectionDetails = connectionDetails
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
  metaData$call$excludeDrugsFromCovariates = excludeDrugsFromCovariates
  metaData$call$firstExposureOnly = firstExposureOnly
  metaData$call$washoutPeriod = washoutPeriod
  metaData$call$covariateSettings= covariateSettings

  result <- list(cohorts = cohorts,
                 outcomes = outcomes,
                 covariates = covariateData$covariates,
                 covariateRef = covariateData$covariateRef,
                 metaData = metaData)
  
  class(result) <- "plpData"
  return(result)
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
#'
#' @details
#' The data will be written to a set of files in the folder specified by the user.
#'
#' @examples
#' # todo
#'
#' @export
savePlpData <- function(plpData, file, envir=NULL) {
  if (missing(plpData))
    stop("Must specify plpData")
  if (missing(file))
    stop("Must specify file")
  if (!class(plpData) %in% c("plpData","plpData.libsvm"  ))
    stop("Data not of class plpData")
  
  # save the actual values in the metaData
  # TODO - only do this if exists in parent or environ
  for(i in 2:length(plpData$metaData$call)){
    plpData$metaData$call[[i]] <- eval(plpData$metaData$call[[i]], envir = envir)
  }
  
  if('ffdf'%in%class(plpData$covariates)){
    covariates <- plpData$covariates
    covariateRef <- plpData$covariateRef
    ffbase::save.ffdf(covariates, covariateRef, dir = file, clone = TRUE)
  } else{
    covariateRef <- plpData$covariateRef
    ffbase::save.ffdf(covariateRef, dir = file, clone = TRUE)
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
                 cohorts = readRDS(file.path(file, "cohorts.rds")),
                 outcomes = readRDS(file.path(file, "outcomes.rds")),
                 metaData = readRDS(file.path(file, "metaData.rds")))
  # Open all ffdfs to prevent annoying messages later:
  open(result$covariates, readonly = readOnly)
  open(result$covariateRef, readonly = readOnly)
  class(result) <- "plpData"
  } else{
    e <- new.env()
    ffbase::load.ffdf(absolutePath, e)
    result <- list(covariates = readRDS(file.path(file, "covariates.rds")),
                   covariateRef = get("covariateRef", envir = e),
                   cohorts = readRDS(file.path(file, "cohorts.rds")),
                   outcomes = readRDS(file.path(file, "outcomes.rds")),
                   metaData = readRDS(file.path(file, "metaData.rds")))
    # Open all ffdfs to prevent annoying messages later:
    open(result$covariateRef, readonly = readOnly)
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
  if (is(population, "plpData")) {
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
    sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
    DatabaseConnector::executeSql(connection = connection, sql = sql, progressBar = FALSE, reportOverallTime = FALSE)
  }
  DatabaseConnector::insertTable(connection = connection,
                                 tableName = paste(cohortDatabaseSchema, cohortTable, sep = "."),
                                 data = population,
                                 dropTableIfExists = dropTableIfExists,
                                 createTable = createTable,
                                 tempTable = FALSE,
                                 oracleTempSchema = NULL)
  RJDBC::dbDisconnect(connection)
  delta <- Sys.time() - start
  writeLines(paste("Inserting rows took", signif(delta, 3), attr(delta, "units")))
  invisible(TRUE)
}