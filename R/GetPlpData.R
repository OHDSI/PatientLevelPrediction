# @file GetPlpData.R
#
# Copyright 2015 Observational Health Data Sciences and Informatics
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

#' Get outcomes for persons in the cohort
#'
#' @description
#' Get all the data for the prediction problem from the server.
#'
#' @details
#' For the specified cohorts, retrieve the outcomes of interest and covariates to be used for the
#' prediction problem.
#'
#' @param connectionDetails                An R object of type \code{connectionDetails} created using
#'                                         the function \code{createConnectionDetails} in the
#'                                         \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema                The name of the database schema that contains the OMOP CDM
#'                                         instance.  Requires read permissions to this database. On
#'                                         SQL Server, this should specifiy both the database and the
#'                                         schema, so for example 'cdm_instance.dbo'.
#' @param oracleTempSchema                 A schema where temp tables can be created in Oracle.
#' @param cohortDatabaseSchema             Where is the source cohort table located? Note that on SQL
#'                                         Server, one should include both the database and schema,
#'                                         e.g. "cdm_schema.dbo".
#' @param cohortTable                      What is the name of the table holding the cohort?
#' @param cohortIds                        The IDs of the cohorts for which we want to create models.
#' @param washoutWindow                    The mininum required continuous observation time prior to
#'                                         index date for a person to be included in the cohort.
#' @param useCohortEndDate                 Use the cohort end date as the basis for the end of the risk
#'                                         window? If FALSE, the cohort start date will be used
#'                                         instead.
#' @param windowPersistence                The number of days the risk window should persist.
#' @param covariateSettings                An object of type \code{covariateSettings} as created using
#'                                         the \code{\link{createCovariateSettings}} function.
#' @param outcomeDatabaseSchema            The name of the database schema that is the location where
#'                                         the data used to define the outcome cohorts is available. If
#'                                         outcomeTable = CONDITION_ERA, outcomeDatabaseSchema is not
#'                                         used.  Requires read permissions to this database.
#' @param outcomeTable                     The tablename that contains the outcome cohorts.  If
#'                                         outcomeTable <> CONDITION_OCCURRENCE, then expectation is
#'                                         outcomeTable has format of COHORT table: COHORT_CONCEPT_ID,
#'                                         SUBJECT_ID, COHORT_START_DATE, COHORT_END_DATE.
#' @param outcomeIds                       A list of ids used to define outcomes.  If outcomeTable =
#'                                         CONDITION_OCCURRENCE, the list is a set of ancestor
#'                                         CONCEPT_IDs, and all occurrences of all descendant concepts
#'                                         will be selected.  If outcomeTable <> CONDITION_OCCURRENCE,
#'                                         the list contains records found in COHORT_DEFINITION_ID
#'                                         field.
#' @param outcomeConditionTypeConceptIds   A list of TYPE_CONCEPT_ID values that will restrict
#'                                         condition occurrences.  Only applicable if outcomeTable =
#'                                         CONDITION_OCCURRENCE.
#' @param firstOutcomeOnly                 Only keep the first outcome per person?
#' @param cdmVersion                       Define the OMOP CDM version used: currently support "4" and
#'                                         "5".
#'
#' @return
#' An object of type \code{plpData} containing information on the prediction problem. This object will
#' contain the following data:
#' \describe{ \item{cohorts}{An ffdf object listing all persons and their prediction periods. This
#' object will have these fields: row_id (a unique ID per period), person_id, cohort_start_date,
#' cohort_id, time (number of days in the window).} \item{outcomes}{An ffdf object listing all
#' outcomes per period. This object will have these fields: row_id, outcome_id, outcome_count,
#' time_to_event.} \item{exclude}{Either NULL or an ffdf object listing per outcome ID which windows
#' had the outcome prior to the window. This object will have these fields: rowId, outcomeId.}
#' \item{covariates}{An ffdf object listing the baseline covariates per person in the cohorts. This is
#' done using a sparse representation: covariates with a value of 0 are omitted to save space. The
#' covariates object will have three columns: rowId, covariateId, and covariateValue. }
#' \item{covariateRef}{An ffdf object describing the covariates that have been extracted.}
#' \item{metaData}{A list of objects with information on how the plpData object was constructed.} }
#'
#' @export
getDbPlpData <- function(connectionDetails = NULL,
                         cdmDatabaseSchema,
                         oracleTempSchema = NULL,
                         cohortDatabaseSchema = cdmDatabaseSchema,
                         cohortTable = "cohort",
                         cohortIds = c(0, 1),
                         washoutWindow = 183,
                         useCohortEndDate = TRUE,
                         windowPersistence = 0,
                         covariateSettings,
                         outcomeDatabaseSchema = cdmDatabaseSchema,
                         outcomeTable = "condition_occurrence",
                         outcomeIds = c(),
                         outcomeConditionTypeConceptIds = "",
                         firstOutcomeOnly = FALSE,
                         cdmVersion = "4") {
  conn <- connect(connectionDetails)

  cdmDatabase <- strsplit(cdmDatabaseSchema, "\\.")[[1]][1]

  if (cdmVersion == "4") {
    cohortDefinitionId <- "cohort_concept_id"
    conceptClassId <- "concept_class"
    measurement <- "observation"
  } else {
    cohortDefinitionId <- "cohort_definition_id"
    conceptClassId <- "concept_class_id"
    measurement <- "measurement"
  }


  ### Create cohort_person temp table, and fetch cohort data ###
  renderedSql <- SqlRender::loadRenderTranslateSql("GetCohorts.sql",
                                                   packageName = "PatientLevelPrediction",
                                                   dbms = attr(conn, "dbms"),
                                                   oracleTempSchema = oracleTempSchema,
                                                   cdm_database = cdmDatabase,
                                                   cohort_database_schema = cohortDatabaseSchema,
                                                   cohort_table = cohortTable,
                                                   cohort_ids = cohortIds,
                                                   washout_window = washoutWindow,
                                                   use_cohort_end_date = useCohortEndDate,
                                                   window_persistence = windowPersistence,
                                                   cdm_version = cdmVersion,
                                                   cohort_definition_id = cohortDefinitionId)

  writeLines("Constructing cohorts of interest")
  DatabaseConnector::executeSql(conn, renderedSql)
  writeLines("Fetching data from server")
  start <- Sys.time()
  cohortsSql <- "SELECT row_id, subject_id AS person_id, cohort_start_date, @cohort_definition_id AS cohort_id, DATEDIFF(DAY, cohort_start_date, cohort_end_date) AS time FROM #cohort_person ORDER BY row_id"
  cohortsSql <- SqlRender::renderSql(cohortsSql, cohort_definition_id = cohortDefinitionId)$sql
  cohortsSql <- SqlRender::translateSql(cohortsSql,
                                        "sql server",
                                        attr(conn, "dbms"),
                                        oracleTempSchema)$sql
  cohorts <- DatabaseConnector::querySql.ffdf(conn, cohortsSql)
  colnames(cohorts) <- SqlRender::snakeCaseToCamelCase(colnames(cohorts))
  if (nrow(cohorts) != 0) {
    open(cohorts)
  }

  delta <- Sys.time() - start
  writeLines(paste("Loading took", signif(delta, 3), attr(delta, "units")))


  ### Fetch outcomes ###
  renderedSql <- SqlRender::loadRenderTranslateSql("GetOutcomes.sql",
                                                   packageName = "PatientLevelPrediction",
                                                   dbms = attr(conn, "dbms"),
                                                   oracleTempSchema = oracleTempSchema,
                                                   cdm_database = cdmDatabase,
                                                   outcome_database_schema = outcomeDatabaseSchema,
                                                   outcome_table = outcomeTable,
                                                   outcome_ids = outcomeIds,
                                                   outcome_condition_type_concept_ids = outcomeConditionTypeConceptIds,
                                                   first_outcome_only = firstOutcomeOnly,
                                                   cdm_version = cdmVersion,
                                                   cohort_definition_id = cohortDefinitionId)

  writeLines("Constructing outcomes")
  DatabaseConnector::executeSql(conn, renderedSql)
  writeLines("Fetching data from server")
  start <- Sys.time()
  outcomeSql <- "SELECT row_id, outcome_id, outcome_count, time_to_event FROM #cohort_outcome ORDER BY row_id"
  outcomeSql <- SqlRender::renderSql(outcomeSql, cohort_definition_id = cohortDefinitionId)$sql
  outcomeSql <- SqlRender::translateSql(outcomeSql,
                                        "sql server",
                                        attr(conn, "dbms"),
                                        oracleTempSchema)$sql
  outcomes <- DatabaseConnector::querySql.ffdf(conn, outcomeSql)
  colnames(outcomes) <- SqlRender::snakeCaseToCamelCase(colnames(outcomes))
  if (nrow(outcomes) == 0) {
    warning("No outcome data found")
  } else {
    open(outcomes)
  }
  if (firstOutcomeOnly) {
    excludeSql <- "SELECT row_id, outcome_id FROM #cohort_excluded_person ORDER BY outcome_id, person_id"
    excludeSql <- SqlRender::translateSql(excludeSql,
                                          "sql server",
                                          attr(conn, "dbms"),
                                          oracleTempSchema)$sql
    exclude <- DatabaseConnector::querySql.ffdf(conn, excludeSql)
    colnames(exclude) <- SqlRender::snakeCaseToCamelCase(colnames(exclude))
    if (nrow(exclude) == 0) {
      exclude <- NULL
    } else {
      open(exclude)
    }
  } else {
    exclude <- NULL
  }

  renderedSql <- SqlRender::loadRenderTranslateSql("RemoveOutcomeTempTables.sql",
                                                   packageName = "PatientLevelPrediction",
                                                   dbms = attr(conn, "dbms"),
                                                   oracleTempSchema = oracleTempSchema,
                                                   first_outcome_only = firstOutcomeOnly)
  DatabaseConnector::executeSql(conn, renderedSql, progressBar = FALSE, reportOverallTime = FALSE)

  delta <- Sys.time() - start
  writeLines(paste("Loading took", signif(delta, 3), attr(delta, "units")))


  ### Fetch covariates ###
  covariateData <- PatientLevelPrediction::getDbCovariateData(connection = conn,
                                                              oracleTempSchema = oracleTempSchema,
                                                              cdmDatabaseSchema = cdmDatabaseSchema,
                                                              rowIdField = "row_id",
                                                              covariateSettings = covariateSettings,
                                                              cdmVersion = cdmVersion)

  ### Clean up ###
  renderedSql <- SqlRender::loadRenderTranslateSql("RemoveCohortTempTables.sql",
                                                   packageName = "PatientLevelPrediction",
                                                   dbms = attr(conn, "dbms"),
                                                   oracleTempSchema = oracleTempSchema)
  DatabaseConnector::executeSql(conn, renderedSql, progressBar = FALSE, reportOverallTime = FALSE)
  dummy <- RJDBC::dbDisconnect(conn)

  metaData <- list(cohortIds = cohortIds,
                   outcomeIds = outcomeIds,
                   useCohortEndDate = useCohortEndDate,
                   windowPersistence = windowPersistence,
                   deletedCovariateIds = covariateData$metaData$deletedCovariateIds,
                   call = match.call())

  result <- list(cohorts = cohorts,
                 outcomes = outcomes,
                 exclude = exclude,
                 covariates = covariateData$covariates,
                 covariateRef = covariateData$covariateRef,
                 metaData = metaData)

  class(result) <- "plpData"
  return(result)
}

#' Save the PatientLevelPrediction data to folder
#'
#' @description
#' \code{savePlpData} saves an object of type plpData to folder.
#'
#' @param plpData   An object of type \code{plpData} as generated using \code{getDbPlPData}.
#' @param file      The name of the folder where the data will be written. The folder should not yet
#'                  exist.
#'
#' @details
#' The data will be written to a set of files in the folder specified by the user.
#'
#' @examples
#' # todo
#'
#' @export
savePlpData <- function(plpData, file) {
  if (missing(plpData))
    stop("Must specify plpData")
  if (missing(file))
    stop("Must specify file")
  if (class(plpData) != "plpData")
    stop("Data not of class plpData")

  outcomes <- plpData$outcomes
  cohorts <- plpData$cohorts
  covariates <- plpData$covariates
  covariateRef <- plpData$covariateRef
  if (is.null(plpData$exclude)) {
    ffbase::save.ffdf(outcomes, cohorts, covariates, covariateRef, dir = file)
  } else {
    exclude <- plpData$exclude
    ffbase::save.ffdf(outcomes, cohorts, covariates, exclude, covariateRef, dir = file)
  }
  metaData <- plpData$metaData
  save(metaData, file = file.path(file, "metaData.Rdata"))
}

#' Load the PatientLevelPrediction data from a folder
#'
#' @description
#' \code{loadPlPData} loads an object of type \code{plpData} from a folder in the file system.
#'
#' @param file       The name of the folder containing the data.
#' @param readOnly   If true, the data is opened read only.
#'
#' @details
#' The data will be written to a set of files in the folder specified by the user.
#'
#' @return
#' An object of class PlPData
#'
#' @examples
#' # todo
#'
#' @export
loadPlpData <- function(file, readOnly = FALSE) {
  if (!file.exists(file))
    stop(paste("Cannot find folder", file))
  if (!file.info(file)$isdir)
    stop(paste("Not a folder", file))

  temp <- setwd(file)
  absolutePath <- setwd(temp)

  e <- new.env()
  ffbase::load.ffdf(absolutePath, e)
  load(file.path(absolutePath, "metaData.Rdata"), e)
  result <- list(outcomes = get("outcomes", envir = e),
                 cohorts = get("cohorts", envir = e),
                 covariates = get("covariates", envir = e),
                 covariateRef = get("covariateRef", envir = e),
                 metaData = mget("metaData",
                                 envir = e,
                                 ifnotfound = list(NULL))[[1]]  #For backwards compatibility
)
  if ("exclude" %in% ls(envir = e)) {
    result$exclude <- get("exclude", envir = e)
  }
  # Open all ffdfs to prevent annoying messages later:
  open(result$outcomes, readonly = readOnly)
  open(result$cohorts, readonly = readOnly)
  open(result$covariates, readonly = readOnly)
  if (!is.null(result$exclude)) {
    open(result$exclude, readonly = readOnly)
  }
  open(result$covariateRef, readonly = readOnly)

  class(result) <- "plpData"
  rm(e)
  return(result)
}

#' @export
print.plpData <- function(x, ...) {
  writeLines("PlPData object")
  writeLines("")
  writeLines(paste("Cohort ID:", x$metaData$cohortIds))
  writeLines(paste("Outcome concept ID(s):", paste(x$metaData$outcomeIds, collapse = ",")))
  writeLines(paste("Using cohort end date:", x$metaData$useCohortEndDate))
  writeLines(paste("Window persistence:", x$metaData$windowPersistence))
}

#' @export
summary.plpData <- function(object, ...) {
  subjectCount <- length(ffbase::unique.ff(object$cohorts$personId))
  windowCount <- nrow(object$cohorts)

  outcomeCounts <- data.frame(outcomeId = object$metaData$outcomeIds,
                              eventCount = 0,
                              windowCount = 0)
  for (i in 1:nrow(outcomeCounts)) {
    outcomeCounts$eventCount[i] <- ffbase::sum.ff(object$outcomes$outcomeId == object$metaData$outcomeIds[i])
    if (outcomeCounts$eventCount[i] == 0) {
      outcomeCounts$windowCount[i] <- 0
    } else {
      t <- (object$outcomes$outcomeId == object$metaData$outcomeIds[i])
      outcomeCounts$windowCount[i] <- length(ffbase::unique.ff(object$outcomes$rowId[ffbase::ffwhich(t,
                                                                                                     t == TRUE)]))
    }
  }

  result <- list(metaData = object$metaData,
                 subjectCount = subjectCount,
                 windowCount = windowCount,
                 outcomeCounts = outcomeCounts,
                 covariateCount = nrow(object$covariateRef),
                 covariateValueCount = nrow(object$covariates))
  class(result) <- "summary.plpData"
  return(result)
}

#' @export
print.summary.plpData <- function(x, ...) {
  writeLines("PlPData object summary")
  writeLines("")
  writeLines(paste("Cohort ID:", x$metaData$cohortIds))
  writeLines(paste("Outcome concept ID(s):", paste(x$metaData$outcomeIds, collapse = ",")))
  writeLines(paste("Using cohort end date:", x$metaData$useCohortEndDate))
  writeLines(paste("Window persistence:", x$metaData$windowPersistence))
  writeLines("")
  writeLines(paste("Persons:", paste(x$subjectCount)))
  writeLines(paste("Windows:", paste(x$windowCount)))
  writeLines("")
  writeLines("Outcome counts:")
  outcomeCounts <- x$outcomeCounts
  rownames(outcomeCounts) <- outcomeCounts$outcomeId
  outcomeCounts$outcomeId <- NULL
  colnames(outcomeCounts) <- c("Event count", "Window count")
  printCoefmat(outcomeCounts)
  writeLines("")
  writeLines("Covariates:")
  writeLines(paste("Number of covariates:", x$covariateCount))
  writeLines(paste("Number of non-zero covariate values:", x$covariateValueCount))
}

