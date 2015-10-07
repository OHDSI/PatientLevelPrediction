# @file GetCohorts.R
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

#' Get cohorts of interest
#'
#' @description
#' Gets the cohorts of interest from the database.
#'
#' @param connectionDetails         An R object of type \code{ConnectionDetails} created using the
#'                                  function \code{createConnectionDetails} in the
#'                                  \code{DatabaseConnector} package.
#' @param connection                A connection to the server containing the schema as created using
#'                                  the \code{connect} function in the \code{DatabaseConnector}
#'                                  package.
#' @param cdmDatabaseSchema         The name of the database schema that contains the OMOP CDM
#'                                  instance. Requires read permissions to this database. On SQL
#'                                  Server, this should specifiy both the database and the schema, so
#'                                  for example 'cdm_instance.dbo'.
#' @param oracleTempSchema          A schema where temp tables can be created in Oracle.
#' @param useExistingCohortPerson   Does the temporary table \code{cohort_person} already exists? Can
#'                                  only be used when the \code{connection} parameter is not NULL.
#' @param cohortDatabaseSchema      If not using an existing \code{cohort_person} temp table, where is
#'                                  the source cohort table located? Note that on SQL Server, one
#'                                  should include both the database and schema, e.g. "cdm_schema.dbo".
#' @param cohortTable               If not using an existing temp table, what is the name of the table
#'                                  holding the cohort?
#' @param cohortIds                 The list of IDs in the cohortTable that identify the cohort(s) of
#'                                  interest.
#' @param useCohortEndDate          Use the cohort end date as the basis for the end of the risk
#'                                  window? If FALSE, the cohort start date will be used instead.
#' @param windowPersistence         The number of days the risk window should persist.
#' @param cdmVersion                Define the OMOP CDM version used: currently support "4" and "5".
#'
#' @return
#' An object of type \code{cohortData} containing information on who are in the cohorts.
#'
#' @export
getDbCohortData <- function(connectionDetails = NULL,
                            connection = NULL,
                            cdmDatabaseSchema,
                            oracleTempSchema = NULL,
                            useExistingCohortPerson = FALSE,
                            cohortDatabaseSchema = cdmDatabaseSchema,
                            cohortTable = "cohort",
                            cohortIds = c(0, 1),
                            useCohortEndDate = TRUE,
                            windowPersistence = 0,
                            cdmVersion = "4") {
  cdmDatabase <- strsplit(cdmDatabaseSchema, "\\.")[[1]][1]
  if (is.null(connectionDetails) && is.null(connection))
    stop("Either connectionDetails or connection has to be specified")
  if (!is.null(connectionDetails) && !is.null(connection))
    stop("Cannot specify both connectionDetails and connection")

  if (cdmVersion == "4") {
    cohortDefinitionId <- "cohort_concept_id"
  } else {
    cohortDefinitionId <- "cohort_definition_id"
  }

  if (is.null(connection)) {
    conn <- DatabaseConnector::connect(connectionDetails)
  } else {
    conn <- connection
  }

  renderedSql <- SqlRender::loadRenderTranslateSql("GetCohorts.sql",
                                                   packageName = "PatientLevelPrediction",
                                                   dbms = attr(conn, "dbms"),
                                                   oracleTempSchema = oracleTempSchema,
                                                   cdm_database = cdmDatabase,
                                                   use_existing_cohort_person = useExistingCohortPerson,
                                                   cohort_database_schema = cohortDatabaseSchema,
                                                   cohort_table = cohortTable,
                                                   cohort_ids = cohortIds,
                                                   use_cohort_end_date = useCohortEndDate,
                                                   window_persistence = windowPersistence,
                                                   cdm_version = cdmVersion,
                                                   cohort_definition_id = cohortDefinitionId)

  writeLines("Executing multiple queries. This could take a while")
  DatabaseConnector::executeSql(conn, renderedSql)
  writeLines("Fetching data from server")
  start <- Sys.time()
  cohortsSql <- "SELECT subject_id AS person_id, cohort_start_date, @cohort_definition_id AS cohort_id, DATEDIFF(DAY, cohort_start_date, cohort_end_date) AS time FROM #cohort_person ORDER BY person_id, cohort_start_date"
  cohortsSql <- SqlRender::renderSql(cohortsSql, cohort_definition_id = cohortDefinitionId)$sql
  cohortsSql <- SqlRender::translateSql(cohortsSql,
                                        "sql server",
                                        attr(conn, "dbms"),
                                        oracleTempSchema)$sql
  cohorts <- DatabaseConnector::querySql.ffdf(conn, cohortsSql)
  colnames(cohorts) <- SqlRender::snakeCaseToCamelCase(colnames(cohorts))

  delta <- Sys.time() - start
  writeLines(paste("Loading took", signif(delta, 3), attr(delta, "units")))

  renderedSql <- SqlRender::loadRenderTranslateSql("RemoveCohortTempTables.sql",
                                                   packageName = "PatientLevelPrediction",
                                                   dbms = attr(conn, "dbms"),
                                                   oracleTempSchema = oracleTempSchema,
                                                   use_existing_cohort_person = useExistingCohortPerson)
  DatabaseConnector::executeSql(conn, renderedSql, progressBar = FALSE, reportOverallTime = FALSE)
  if (is.null(connection)) {
    dummy <- RJDBC::dbDisconnect(conn)
  }
  if (useExistingCohortPerson)
    cohortIds <- ffbase::unique.ff(cohorts$cohortId)
  metaData <- list(call = match.call(), cohortIds = cohortIds)
  result <- list(cohorts = cohorts, metaData = metaData)
  class(result) <- "cohortData"
  return(result)
}

#' Save the cohort data to folder
#'
#' @description
#' \code{saveCohortData} saves an object of type cohortData to folder.
#'
#' @param cohortData   An object of type \code{cohortData} as generated using \code{getDbcohortData}.
#' @param file         The name of the folder where the data will be written. The folder should not yet
#'                     exist.
#'
#' @details
#' The data will be written to a set of files in the folder specified by the user.
#'
#' @examples
#' # todo
#'
#' @export
saveCohortData <- function(cohortData, file) {
  if (missing(cohortData))
    stop("Must specify cohortData")
  if (missing(file))
    stop("Must specify file")
  if (class(cohortData) != "cohortData")
    stop("Data not of class cohortData")

  cohorts <- cohortData$cohorts
  ffbase::save.ffdf(cohorts, dir = file)
  open(cohortData$cohorts)
  metaData <- cohortData$metaData
  save(metaData, file = file.path(file, "metaData.Rdata"))
}

#' Load the cohorts data from a folder
#'
#' @description
#' \code{loadCohortData} loads an object of type cohortData from a folder in the file system.
#'
#' @param file       The name of the folder containing the data.
#' @param readOnly   If true, the data is opened read only.
#'
#' @details
#' The data will be written to a set of files in the folder specified by the user.
#'
#' @return
#' An object of class cohortData
#'
#'
#' @export
loadCohortData <- function(file, readOnly = FALSE) {
  if (!file.exists(file))
    stop(paste("Cannot find folder", file))
  if (!file.info(file)$isdir)
    stop(paste("Not a folder", file))
  temp <- setwd(file)
  absolutePath <- setwd(temp)
  e <- new.env()
  ffbase::load.ffdf(absolutePath, e)
  load(file.path(absolutePath, "metaData.Rdata"), e)
  result <- list(cohorts = get("cohorts", envir = e), metaData = get("metaData", envir = e))
  open(result$cohorts, readonly = readOnly)
  class(result) <- "cohortData"
  rm(e)
  return(result)
}

#' @export
print.cohortData <- function(x, ...) {
  writeLines("CohortData object")
  writeLines("")
  writeLines(paste("Cohort of interest concept ID(s):",
                   paste(x$metaData$cohortIds, collapse = ",")))
}

#' @export
summary.cohortData <- function(object, ...) {
  counts <- data.frame(cohortId = object$metaData$cohortIds, cohortCount = 0, personCount = 0)
  for (i in 1:nrow(counts)) {
    cohortId <- counts$cohortId[i]
    t <- object$cohorts$cohortId == cohortId
    t <- ffbase::ffwhich(t, t == TRUE)
    counts$cohortCount[i] <- length(t)
    counts$personCount[i] <- length(ffbase::unique.ff(object$cohorts$personId[t]))
  }
  result <- list(metaData = object$metaData, counts = counts)
  class(result) <- "summary.cohortData"
  return(result)
}

#' @export
print.summary.cohortData <- function(x, ...) {
  writeLines("CohortData object summary")
  writeLines("")
  counts <- x$counts
  rownames(counts) <- counts$cohortId
  counts$cohortId <- NULL
  colnames(counts) <- c("Cohort count", "Person count")
  printCoefmat(counts)
}

