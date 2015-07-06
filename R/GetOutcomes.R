# @file GetCovariates.R
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
#' Gets the outcomes for the specified cohort(s).
#'
#' @details
#' For the specified cohorts, retrieve the outcomes of interest during cohort start and end date.
#' Either a \code{connectionDetails} or a \code{connection} object has to be specified.
#'
#' @param connectionDetails                An R object of type \code{connectionDetails} created using
#'                                         the function \code{createConnectionDetails} in the
#'                                         \code{DatabaseConnector} package.
#' @param connection                       A connection to the server containing the schema as created
#'                                         using the \code{connect} function in the
#'                                         \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema                The name of the database schema that contains the OMOP CDM
#'                                         instance.  Requires read permissions to this database. On
#'                                         SQL Server, this should specifiy both the database and the
#'                                         schema, so for example 'cdm_instance.dbo'.
#' @param oracleTempSchema                 A schema where temp tables can be created in Oracle.#'
#' @param useExistingCohortPerson          Does the temporary table \code{cohort_person} already
#'                                         exists? Can only be used when the \code{connection}
#'                                         parameter is not NULL.
#' @param cohortDatabaseSchema             If not using an existing \code{cohort_person} temp table,
#'                                         where is the source cohort table located? Note that on SQL
#'                                         Server, one should include both the database and schema,
#'                                         e.g. "cdm_schema.dbo".
#' @param cohortTable                      If not using an existing temp table, what is the name of the
#'                                         table holding the cohort?
#' @param cohortConceptIds                 If not using an existing temp table, what is the name of the
#'                                         source cohort table?
#' @param outcomeDatabaseSchema            The name of the database schema that is the location where
#'                                         the data used to define the outcome cohorts is available. If
#'                                         exposureTable = CONDITION_ERA, exposureDatabaseSchema is not
#'                                         used by assumed to be cdmSchema.  Requires read permissions
#'                                         to this database.
#' @param outcomeTable                     The tablename that contains the outcome cohorts.  If
#'                                         outcomeTable <> CONDITION_OCCURRENCE, then expectation is
#'                                         outcomeTable has format of COHORT table: COHORT_CONCEPT_ID,
#'                                         SUBJECT_ID, COHORT_START_DATE, COHORT_END_DATE.
#' @param outcomeConceptIds                A list of CONCEPT_IDs used to define outcomes.  If
#'                                         outcomeTable = CONDITION_OCCURRENCE, the list is a set of
#'                                         ancestor CONCEPT_IDs, and all occurrences of all descendant
#'                                         concepts will be selected.  If outcomeTable <>
#'                                         CONDITION_OCCURRENCE, the list contains records found in
#'                                         COHORT_DEFINITION_ID field.
#' @param outcomeConditionTypeConceptIds   A list of TYPE_CONCEPT_ID values that will restrict
#'                                         condition occurrences.  Only applicable if outcomeTable =
#'                                         CONDITION_OCCURRENCE.
#' @param firstOutcomeOnly                 Only keep the first outcome per person?
#'
#' @return
#' An object of type \code{outcomeData} containing information on the outcomes in the cohort(s).
#'
#' @export
getDbOutcomeData <- function(connectionDetails = NULL,
                             connection = NULL,
                             cdmDatabaseSchema,
                             oracleTempSchema = cdmDatabaseSchema,
                             useExistingCohortPerson = FALSE,
                             cohortDatabaseSchema = cdmDatabaseSchema,
                             cohortTable = "cohort",
                             cohortConceptIds = c(0, 1),
                             outcomeDatabaseSchema = cdmDatabaseSchema,
                             outcomeTable = "condition_occurrence",
                             outcomeConceptIds = c(),
                             outcomeConditionTypeConceptIds = "",
                             firstOutcomeOnly = FALSE) {
  cdmDatabase <- strsplit(cdmDatabaseSchema, "\\.")[[1]][1]
  if (is.null(connectionDetails) && is.null(connection))
    stop("Either connectionDetails or connection has to be specified")
  if (!is.null(connectionDetails) && !is.null(connection))
    stop("Cannot specify both connectionDetails and connection")

  if (is.null(connection)) {
    conn <- DatabaseConnector::connect(connectionDetails)
  } else {
    conn <- connection
  }

  renderedSql <- SqlRender::loadRenderTranslateSql("GetOutcomes.sql",
                                                   packageName = "PatientLevelPrediction",
                                                   dbms = attr(conn, "dbms"),
                                                   oracleTempSchema = oracleTempSchema,
                                                   cdm_database = cdmDatabase,
                                                   use_existing_cohort_person = useExistingCohortPerson,
                                                   cohort_database_schema = cohortDatabaseSchema,
                                                   cohort_table = cohortTable,
                                                   cohort_concept_ids = cohortConceptIds,
                                                   outcome_database_schema = outcomeDatabaseSchema,
                                                   outcome_table = outcomeTable,
                                                   outcome_concept_ids = outcomeConceptIds,
                                                   outcome_condition_type_concept_ids = outcomeConditionTypeConceptIds,
                                                   first_outcome_only = firstOutcomeOnly)

  writeLines("Executing multiple queries. This could take a while")
  DatabaseConnector::executeSql(conn, renderedSql)
  writeLines("Fetching data from server")
  start <- Sys.time()
  outcomeSql <- "SELECT person_id, cohort_start_date, cohort_concept_id, outcome_id, outcome_count, time_to_event FROM #cohort_outcome ORDER BY person_id, cohort_start_date"
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
    excludeSql <- "SELECT person_id, cohort_start_date, cohort_concept_id, outcome_id FROM #cohort_excluded_person ORDER BY outcome_id, person_id"
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
  }
  delta <- Sys.time() - start
  writeLines(paste("Loading took", signif(delta, 3), attr(delta, "units")))

  renderedSql <- SqlRender::loadRenderTranslateSql("RemoveOutcomeTempTables.sql",
                                                   packageName = "PatientLevelPrediction",
                                                   dbms = attr(conn, "dbms"),
                                                   oracleTempSchema = oracleTempSchema,
                                                   use_existing_cohort_person = useExistingCohortPerson)
  DatabaseConnector::executeSql(conn, renderedSql, progressBar = FALSE, reportOverallTime = FALSE)
  if (is.null(connection)) {
    dummy <- RJDBC::dbDisconnect(conn)
  }
  metaData <- list(call = match.call(), outcomeConceptIds = outcomeConceptIds)
  result <- list(outcomes = outcomes, metaData = metaData)
  if (firstOutcomeOnly)
    result$exclude <- exclude
  class(result) <- "outcomeData"
  return(result)
}

#' Save the outcome data to folder
#'
#' @description
#' \code{saveOutcomeData} saves an object of type outcomeData to folder.
#'
#' @param outcomeData   An object of type \code{outcomeData} as generated using
#'                      \code{getDbOutcomeData}.
#' @param file          The name of the folder where the data will be written. The folder should not
#'                      yet exist.
#'
#' @details
#' The data will be written to a set of files in the folder specified by the user.
#'
#' @export
saveOutcomeData <- function(outcomeData, file) {
  if (missing(outcomeData))
    stop("Must specify outcomeData")
  if (missing(file))
    stop("Must specify file")
  if (class(outcomeData) != "outcomeData")
    stop("Data not of class outcomeData")

  outcomes <- outcomeData$outcomes
  if (!is.null(outcomeData$exclude)) {
    exclude <- outcomeData$exclude
    ffbase::save.ffdf(outcomes, exclude, dir = file)
    open(outcomeData$outcomes)
    open(outcomeData$exclude)
  } else {
    ffbase::save.ffdf(outcomes, dir = file)
    open(outcomeData$outcomes)
  }
  metaData <- outcomeData$metaData
  save(metaData, file = file.path(file, "metaData.Rdata"))
}

#' Load the outcome data from a folder
#'
#' @description
#' \code{loadOutcomeData} loads an object of type outcomeData from a folder in the file system.
#'
#' @param file       The name of the folder containing the data.
#' @param readOnly   If true, the data is opened read only.
#'
#' @details
#' The data will be written to a set of files in the folder specified by the user.
#'
#' @return
#' An object of class outcomeData
#'
#' @export
loadOutcomeData <- function(file, readOnly = FALSE) {
  if (!file.exists(file))
    stop(paste("Cannot find folder", file))
  if (!file.info(file)$isdir)
    stop(paste("Not a folder", file))
  temp <- setwd(file)
  absolutePath <- setwd(temp)
  e <- new.env()
  ffbase::load.ffdf(absolutePath, e)
  load(file.path(absolutePath, "metaData.Rdata"), e)
  if (any(ls(e) == "exclude")) {
    result <- list(outcomes = get("outcomes", envir = e),
                   exclude = get("exclude", envir = e),
                   metaData = get("metaData", envir = e))
    open(result$outcomes, readonly = readOnly)
    open(result$exclude, readonly = readOnly)
  } else {
    result <- list(outcomes = get("outcomes", envir = e), metaData = get("metaData", envir = e))
    open(result$outcomes, readonly = readOnly)
  }
  class(result) <- "outcomeData"
  rm(e)
  return(result)
}

#' @export
print.outcomeData <- function(x, ...) {
  writeLines("OutcomeData object")
  writeLines("")
  writeLines(paste("Outcome concept ID(s):", paste(x$metaData$outcomeConceptIds, collapse = ",")))
}

#' @export
summary.outcomeData <- function(object, ...) {
  counts <- data.frame(outcomeConceptId = object$metaData$outcomeConceptIds,
                       cohortCount = 0,
                       personCount = 0)
  for (i in 1:nrow(counts)) {
    outcomeConceptId <- counts$outcomeConceptId[i]
    t <- object$outcomes$outcomeId == outcomeConceptId
    t <- ffbase::ffwhich(t, t == TRUE)
    counts$cohortCount[i] <- length(t)
    counts$personCount[i] <- length(ffbase::unique.ff(object$outcomes$personId[t]))
  }
  result <- list(metaData = object$metaData, counts = counts)
  class(result) <- "summary.outcomeData"
  return(result)
}

#' @export
print.summary.outcomeData <- function(x, ...) {
  writeLines("OutcomeData object summary")
  writeLines("")
  counts <- x$counts
  rownames(counts) <- counts$outcomeConceptId
  counts$outcomeConceptId <- NULL
  colnames(counts) <- c("Cohort count", "Person count")
  printCoefmat(counts)
}
