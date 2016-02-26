# @file GetCovariatesFromCohortAttributes.R
#
# Copyright 2016 Observational Health Data Sciences and Informatics
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

#' Getcovariate information from the database through the cohort_attribute table
#'
#' @description
#' Constructs a large default set of covariates for one or more cohorts using data in the CDM schema.
#' Includes covariates for all drugs, drug classes, condition, condition classes, procedures,
#' observations, etc.
#'
#' @param covariateSettings   An object of type \code{covariateSettings} as created using the
#'                            \code{\link{cohortAttrCovariateSettings}} function.
#'
#' @template GetCovarParams
#'
#' @export
getDbCohortAttrCovariatesData <- function(connection,
                                          oracleTempSchema = NULL,
                                          cdmDatabaseSchema,
                                          cdmVersion = "4",
                                          cohortTempTable = "cohort_person",
                                          rowIdField = "subject_id",
                                          covariateSettings) {
  start <- Sys.time()
  writeLines("Constructing covariates from cohort attributes table")
  if (substr(cohortTempTable, 1, 1) != "#") {
    cohortTempTable <- paste("#", cohortTempTable, sep = "")
  }

  if (is.null(covariateSettings$includeAttrIds) || length(covariateSettings$includeAttrIds) == 0) {
    hasIncludeAttrIds <- FALSE
  } else {
    if (!is.numeric(covariateSettings$includeAttrIds))
      stop("includeAttrIds must be a (vector of) numeric")
    hasIncludeAttrIds <- TRUE
    DatabaseConnector::insertTable(connection,
                                   tableName = "#included_attr",
                                   data = data.frame(attribute_definition_id = as.integer(covariateSettings$includeAttrIds)),
                                   dropTableIfExists = TRUE,
                                   createTable = TRUE,
                                   tempTable = TRUE,
                                   oracleTempSchema = oracleTempSchema)
  }

  renderedSql <- SqlRender::loadRenderTranslateSql("GetAttrCovariates.sql",
                                                   packageName = "PatientLevelPrediction",
                                                   dbms = attr(connection, "dbms"),
                                                   oracleTempSchema = oracleTempSchema,
                                                   attr_database_schema = covariateSettings$attrDatabaseSchema,
                                                   cdm_version = cdmVersion,
                                                   cohort_temp_table = cohortTempTable,
                                                   row_id_field = rowIdField,
                                                   cohort_attribute_table = covariateSettings$cohortAttrTable,
                                                   has_include_attr_ids = hasIncludeAttrIds)

  covariates <- DatabaseConnector::querySql.ffdf(connection, renderedSql)

  covariateRefSql <- "SELECT attribute_definition_id AS covariate_id, attribute_name AS covariate_name FROM @attr_database_schema.@attr_definition_table ORDER BY attribute_definition_id"
  covariateRefSql <- SqlRender::renderSql(covariateRefSql,
                                          attr_database_schema = covariateSettings$attrDatabaseSchema,
                                          attr_definition_table = covariateSettings$attrDefinitionTable)$sql
  covariateRefSql <- SqlRender::translateSql(sql = covariateRefSql,
                                             sourceDialect = "sql server",
                                             targetDialect = attr(connection, "dbms"),
                                             oracleTempSchema = oracleTempSchema)$sql
  covariateRef <- DatabaseConnector::querySql.ffdf(connection, covariateRefSql)

  colnames(covariates) <- SqlRender::snakeCaseToCamelCase(colnames(covariates))
  colnames(covariateRef) <- SqlRender::snakeCaseToCamelCase(colnames(covariateRef))

  sql <- "SELECT COUNT_BIG(*) FROM @cohort_temp_table"
  sql <- SqlRender::renderSql(sql, cohort_temp_table = cohortTempTable)$sql
  sql <- SqlRender::translateSql(sql,
                                 targetDialect = attr(connection, "dbms"),
                                 oracleTempSchema = oracleTempSchema)$sql
  populationSize <- DatabaseConnector::querySql(connection, sql)[1, 1]

  delta <- Sys.time() - start
  writeLines(paste("Loading took", signif(delta, 3), attr(delta, "units")))

  # Remove redundant covariates
  writeLines("Removing redundant covariates")
  deletedCovariateIds <- c()
  if (nrow(covariates) != 0) {
    # First delete all single covariates that appear in every row with the same value
    valueCounts <- bySumFf(ff::ff(1, length = nrow(covariates)), covariates$covariateId)
    nonSparseIds <- valueCounts$bins[valueCounts$sums == populationSize]
    for (covariateId in nonSparseIds) {
      selection <- covariates$covariateId == covariateId
      idx <- ffbase::ffwhich(selection, selection == TRUE)
      values <- ffbase::unique.ff(covariates$covariateValue[idx])
      if (length(values) == 1) {
        idx <- ffbase::ffwhich(selection, selection == FALSE)
        covariates <- covariates[idx, ]
        deletedCovariateIds <- c(deletedCovariateIds, covariateId)
      }
    }
  }
  metaData <- list(sql = renderedSql,
                   call = match.call(),
                   deletedCovariateIds = deletedCovariateIds)
  result <- list(covariates = covariates, covariateRef = covariateRef, metaData = metaData)
  class(result) <- "covariateData"
  return(result)
}

#' Create cohort attribute covariate settings
#'
#' @details
#' Creates an object specifying where the cohort attributes can be found to construct covariates. The
#' attributes should be defined in a table with the same structure as the attribute_definition table
#' in the Common Data Model. It should at least have these columns: \describe{
#' \item{attribute_definition_id}{A unique identifier of type integer.} \item{attribute_name}{A short
#' description of the attribute.} } The cohort attributes themselves should be stored in a table with
#' the same format as the cohort_attribute table in the Common Data Model. It should at least have
#' these columns: \describe{ \item{cohort_definition_id}{A key to link to the cohort table. On CDM v4,
#' this field should be called \code{cohort_concept_id}.} \item{subject_id}{A key to link to the
#' cohort table.} \item{cohort_start_date}{A key to link to the cohort table.}
#' \item{attribute_definition_id}{An foreign key linking to the attribute definition table.}
#' \item{value_as_number}{A real number.} }
#'
#' @param attrDatabaseSchema    The database schema where the attribute definition and cohort attribute
#'                              table can be found.
#' @param attrDefinitionTable   The name of the attribute definition table.
#' @param cohortAttrTable       The name of the cohort attribute table.
#' @param includeAttrIds        (optional) A list of attribute definition IDs to restrict to.
#'
#' @return
#' An object of type \code{covariateSettings}, to be used in other functions.
#'
#' @export
createCohortAttrCovariateSettings <- function(attrDatabaseSchema,
                                              attrDefinitionTable = "attribute_definition",
                                              cohortAttrTable = "cohort_attribute",
                                              includeAttrIds = c()) {
  # First: get the default values:
  covariateSettings <- list()
  for (name in names(formals(createCohortAttrCovariateSettings))) {
    covariateSettings[[name]] <- get(name)
  }
  # Next: overwrite defaults with actual values if specified:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(covariateSettings))
      covariateSettings[[name]] <- values[[name]]
  }

  attr(covariateSettings, "fun") <- "getDbCohortAttrCovariatesData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}
