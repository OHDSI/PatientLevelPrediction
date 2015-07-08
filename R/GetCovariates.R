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

#' Get covariate information from the database
#'
#' @description
#' Constructs a large set of covariates for one or more cohorts using data in the CDM schema.
#'
#' @details
#' This function uses the data in the CDM to construct a large set of covariates for the provided
#' cohorts. The cohorts are assumed to be in a table with the same structure as the cohort table in
#' the OMOP CDM. The subject_id in this table must refer to person_ids in the CDM. One person can
#' occurr multiple times, but the combination of subject_id and cohort_start_date is assumed to be
#' unique.
#'
#' @param connectionDetailsAn       R object of type \code{connectionDetails} created using the
#'                                  function \code{createConnectionDetails} in the
#'                                  \code{DatabaseConnector} package.
#' @param connection                A connection to the server containing the schema as created using
#'                                  the \code{connect} function in the \code{DatabaseConnector}
#'                                  package.
#' @param oracleTempSchemaA         schema where temp tables can be created in Oracle.
#' @param cdmDatabaseSchema         The name of the database schema that contains the OMOP CDM
#'                                  instance.  Requires read permissions to this database. On SQL
#'                                  Server, this should specifiy both the database and the schema, so
#'                                  for example 'cdm_instance.dbo'.
#' @param useExistingCohortPerson   Does the temporary table \code{cohort_person} already exists? Can
#'                                  only be used when the \code{connection} parameter is not NULL.
#' @param cohortDatabaseSchema      If not using an existing \code{cohort_person} temp table, where is
#'                                  the source cohort table located? Note that on SQL Server, one
#'                                  should include both the database and schema, e.g. 'cdm_schema.dbo'.
#' @param cohortTable               If not using an existing \code{cohort_person} temp table, what is
#'                                  the name of the source cohort table?
#' @param cohortIds                 The IDs of the cohortsin the cohort table for which we want to build covariates.
#' @param covariateSettings         An object of type \code{covariateSettings} as created using the
#'                                  \code{\link{createCovariateSettings}} function.
#' @param cdmVersion                Define the OMOP CDM version used:  currently support "4" and "5".
#'
#' @return
#' Returns an object of type \code{covariateData}, containing information on the baseline covariates.
#' Information about multiple outcomes can be captured at once for efficiency reasons. This object is
#' a list with the following components: \describe{ \item{covariates}{An ffdf object listing the
#' baseline covariates per person in the two cohorts. This is done using a sparse representation:
#' covariates with a value of 0 are omitted to save space.} \item{covariateRef}{An ffdf object
#' describing the covariates that have been extracted.} \item{metaData}{A list of objects with
#' information on how the covariateData object was constructed.} }
#'
#' @export
getDbCovariateData <- function(connectionDetails = NULL,
                               connection = NULL,
                               oracleTempSchema = NULL,
                               cdmDatabaseSchema,
                               useExistingCohortPerson = FALSE,
                               cohortDatabaseSchema = cdmDatabaseSchema,
                               cohortTable = "cohort",
                               cohortIds = c(0, 1),
                               covariateSettings,
                               cdmVersion = "4") {
  if (is.null(connectionDetails) && is.null(connection))
    stop("Either connectionDetails or connection has to be specified")
  if (!is.null(connectionDetails) && !is.null(connection))
    stop("Cannot specify both connectionDetails and connection")
  if (useExistingCohortPerson && is.null(connection))
    stop("When using an existing cohort temp table, connection must be specified")
  if (!covariateSettings$useCovariateConditionGroupMeddra & !covariateSettings$useCovariateConditionGroupSnomed)
    covariateSettings$useCovariateConditionGroup <- FALSE
  
  cdmDatabase <- strsplit(cdmDatabaseSchema, "\\.")[[1]][1]
  
  if (cdmVersion == "4"){
    cohortDefinitionId <- "cohort_concept_id"
    conceptClassId <- "concept_class"
    measurement <- "observation"
  } else {
    cohortDefinitionId <- "cohort_definition_id"
    conceptClassId <- "concept_class_id"
    measurement <- "measurement"
  }
  
  if (is.null(connection)) {
    conn <- connect(connectionDetails)
  } else {
    conn <- connection
  }
  
  if (is.null(covariateSettings$excludedCovariateConceptIds) || length(covariateSettings$excludedCovariateConceptIds) ==
      0) {
    hasExcludedCovariateConceptIds <- FALSE
  } else {
    if (!is.numeric(covariateSettings$excludedCovariateConceptIds))
      stop("excludedCovariateConceptIds must be a (vector of) numeric")
    hasExcludedCovariateConceptIds <- TRUE
    DatabaseConnector::insertTable(conn,
                                   tableName = "#excluded_cov",
                                   data = data.frame(concept_id = as.integer(covariateSettings$excludedCovariateConceptIds)),
                                   dropTableIfExists = TRUE,
                                   createTable = TRUE,
                                   tempTable = TRUE,
                                   oracleTempSchema = oracleTempSchema)
  }
  
  if (is.null(covariateSettings$includedCovariateConceptIds) || length(covariateSettings$includedCovariateConceptIds) ==
      0) {
    hasIncludedCovariateConceptIds <- FALSE
  } else {
    if (!is.numeric(covariateSettings$includedCovariateConceptIds))
      stop("includedCovariateConceptIds must be a (vector of) numeric")
    hasIncludedCovariateConceptIds <- TRUE
    DatabaseConnector::insertTable(conn,
                                   tableName = "#included_cov",
                                   data = data.frame(concept_id = as.integer(covariateSettings$includedCovariateConceptIds)),
                                   dropTableIfExists = TRUE,
                                   createTable = TRUE,
                                   tempTable = TRUE,
                                   oracleTempSchema = oracleTempSchema)
  }
  
  renderedSql <- SqlRender::loadRenderTranslateSql("GetCovariates.sql",
                                                   packageName = "PatientLevelPrediction",
                                                   dbms = attr(conn, "dbms"),
                                                   oracleTempSchema = oracleTempSchema,
                                                   cdm_database = cdmDatabase,
                                                   use_existing_cohort_person = useExistingCohortPerson,
                                                   cohort_database_schema = cohortDatabaseSchema,
                                                   cohort_table = cohortTable,
                                                   cohort_ids = cohortIds,
                                                   use_covariate_demographics = covariateSettings$useCovariateDemographics,
                                                   use_covariate_demographics_gender = covariateSettings$useCovariateDemographicsGender,
                                                   use_covariate_demographics_race = covariateSettings$useCovariateDemographicsRace,
                                                   use_covariate_demographics_ethnicity = covariateSettings$useCovariateDemographicsEthnicity,
                                                   use_covariate_demographics_age = covariateSettings$useCovariateDemographicsAge,
                                                   use_covariate_demographics_year = covariateSettings$useCovariateDemographicsYear,
                                                   use_covariate_demographics_month = covariateSettings$useCovariateDemographicsMonth,
                                                   use_covariate_condition_occurrence = covariateSettings$useCovariateConditionOccurrence,
                                                   use_covariate_condition_occurrence_365d = covariateSettings$useCovariateConditionOccurrence365d,
                                                   use_covariate_condition_occurrence_30d = covariateSettings$useCovariateConditionOccurrence30d,
                                                   use_covariate_condition_occurrence_inpt180d = covariateSettings$useCovariateConditionOccurrenceInpt180d,
                                                   use_covariate_condition_era = covariateSettings$useCovariateConditionEra,
                                                   use_covariate_condition_era_ever = covariateSettings$useCovariateConditionEraEver,
                                                   use_covariate_condition_era_overlap = covariateSettings$useCovariateConditionEraOverlap,
                                                   use_covariate_condition_group = covariateSettings$useCovariateConditionGroup,
                                                   use_covariate_condition_group_meddra = covariateSettings$useCovariateConditionGroupMeddra,
                                                   use_covariate_condition_group_snomed = covariateSettings$useCovariateConditionGroupSnomed,
                                                   use_covariate_drug_exposure = covariateSettings$useCovariateDrugExposure,
                                                   use_covariate_drug_exposure_365d = covariateSettings$useCovariateDrugExposure365d,
                                                   use_covariate_drug_exposure_30d = covariateSettings$useCovariateDrugExposure30d,
                                                   use_covariate_drug_era = covariateSettings$useCovariateDrugEra,
                                                   use_covariate_drug_era_365d = covariateSettings$useCovariateDrugEra365d,
                                                   use_covariate_drug_era_30d = covariateSettings$useCovariateDrugEra30d,
                                                   use_covariate_drug_era_overlap = covariateSettings$useCovariateDrugEraOverlap,
                                                   use_covariate_drug_era_ever = covariateSettings$useCovariateDrugEraEver,
                                                   use_covariate_drug_group = covariateSettings$useCovariateDrugGroup,
                                                   use_covariate_procedure_occurrence = covariateSettings$useCovariateProcedureOccurrence,
                                                   use_covariate_procedure_occurrence_365d = covariateSettings$useCovariateProcedureOccurrence365d,
                                                   use_covariate_procedure_occurrence_30d = covariateSettings$useCovariateProcedureOccurrence30d,
                                                   use_covariate_procedure_group = covariateSettings$useCovariateProcedureGroup,
                                                   use_covariate_observation = covariateSettings$useCovariateObservation,
                                                   use_covariate_observation_365d = covariateSettings$useCovariateObservation365d,
                                                   use_covariate_observation_30d = covariateSettings$useCovariateObservation30d,
                                                   use_covariate_observation_below = covariateSettings$useCovariateObservationBelow,
                                                   use_covariate_observation_above = covariateSettings$useCovariateObservationAbove,
                                                   use_covariate_observation_count365d = covariateSettings$useCovariateObservationCount365d,
                                                   use_covariate_concept_counts = covariateSettings$useCovariateConceptCounts,
                                                   use_covariate_risk_scores = covariateSettings$useCovariateRiskScores,
                                                   use_covariate_risk_scores_Charlson = covariateSettings$useCovariateRiskScoresCharlson,
                                                   use_covariate_risk_scores_DCSI = covariateSettings$useCovariateRiskScoresDCSI,
                                                   use_covariate_risk_scores_CHADS2 = covariateSettings$useCovariateRiskScoresCHADS2,
                                                   use_covariate_interaction_year = covariateSettings$useCovariateInteractionYear,
                                                   use_covariate_interaction_month = covariateSettings$useCovariateInteractionMonth,
                                                   has_excluded_covariate_concept_ids = hasExcludedCovariateConceptIds,
                                                   has_included_covariate_concept_ids = hasIncludedCovariateConceptIds,
                                                   delete_covariates_small_count = covariateSettings$deleteCovariatesSmallCount,
                                                   cdm_version = cdmVersion,
                                                   cohort_definition_id = cohortDefinitionId,
                                                   concept_class_id = conceptClassId,
                                                   measurement = measurement)
  
  writeLines("Executing multiple queries. This could take a while")
  
  DatabaseConnector::executeSql(conn, renderedSql)
  writeLines("Done")
  
  writeLines("Fetching data from server")
  start <- Sys.time()
  covariateSql <- "SELECT person_id, cohort_start_date, @cohort_definition_id AS cohort_definition_id, covariate_id, covariate_value FROM #cov ORDER BY person_id, covariate_id"
  covariateSql <- SqlRender::renderSql(covariateSql, cohort_definition_id = cohortDefinitionId)$sql
  covariateSql <- SqlRender::translateSql(covariateSql,
                                          "sql server",
                                          attr(conn, "dbms"),
                                          oracleTempSchema)$sql
  covariates <- DatabaseConnector::querySql.ffdf(conn, covariateSql)
  covariateRefSql <- "SELECT covariate_id, covariate_name, analysis_id, concept_id  FROM #cov_ref ORDER BY covariate_id"
  covariateRefSql <- SqlRender::translateSql(covariateRefSql,
                                             "sql server",
                                             attr(conn, "dbms"),
                                             oracleTempSchema)$sql
  covariateRef <- DatabaseConnector::querySql.ffdf(conn, covariateRefSql)
  delta <- Sys.time() - start
  writeLines(paste("Loading took", signif(delta, 3), attr(delta, "units")))
  
  renderedSql <- SqlRender::loadRenderTranslateSql("RemoveCovariateTempTables.sql",
                                                   packageName = "PatientLevelPrediction",
                                                   dbms = attr(conn, "dbms"),
                                                   oracleTempSchema = oracleTempSchema,
                                                   has_excluded_covariate_concept_ids = hasExcludedCovariateConceptIds,
                                                   has_included_covariate_concept_ids = hasIncludedCovariateConceptIds)
  DatabaseConnector::executeSql(conn, renderedSql, progressBar = FALSE, reportOverallTime = FALSE)
  if (is.null(connection)) {
    RJDBC::dbDisconnect(conn)
  }
  
  colnames(covariates) <- SqlRender::snakeCaseToCamelCase(colnames(covariates))
  colnames(covariateRef) <- SqlRender::snakeCaseToCamelCase(colnames(covariateRef))
  metaData <- list(sql = renderedSql, call = match.call())
  result <- list(covariates = covariates, covariateRef = covariateRef, metaData = metaData)
  # Open all ffdfs to prevent annoying messages later:
  if (nrow(result$covariates) == 0) {
    warning("No data found")
  } else {
    open(result$covariates)
    open(result$covariateRef)
  }
  class(result) <- "covariateData"
  return(result)
}

#' Save the covariate data to folder
#'
#' @description
#' \code{saveCovariateData} saves an object of type covariateData to folder.
#'
#' @param covariateData   An object of type \code{covariateData} as generated using
#'                        \code{getDbCovariateData}.
#' @param file            The name of the folder where the data will be written. The folder should not
#'                        yet exist.
#'
#' @details
#' The data will be written to a set of files in the folder specified by the user.
#'
#' @examples
#' # todo
#'
#' @export
saveCovariateData <- function(covariateData, file) {
  if (missing(covariateData))
    stop("Must specify covariateData")
  if (missing(file))
    stop("Must specify file")
  if (class(covariateData) != "covariateData")
    stop("Data not of class covariateData")
  
  covariates <- covariateData$covariates
  covariateRef <- covariateData$covariateRef
  ffbase::save.ffdf(covariates, covariateRef, dir = file)
  open(covariateData$covariates)
  open(covariateData$covariateRef)
  metaData <- covariateData$metaData
  save(metaData, file = file.path(file, "metaData.Rdata"))
}

#' Load the covariate data from a folder
#'
#' @description
#' \code{loadCovariateData} loads an object of type covariateData from a folder in the file system.
#'
#' @param file       The name of the folder containing the data.
#' @param readOnly   If true, the data is opened read only.
#'
#' @details
#' The data will be written to a set of files in the folder specified by the user.
#'
#' @return
#' An object of class covariateData
#'
#' @examples
#' # todo
#'
#' @export
loadCovariateData <- function(file, readOnly = FALSE) {
  if (!file.exists(file))
    stop(paste("Cannot find folder", file))
  if (!file.info(file)$isdir)
    stop(paste("Not a folder", file))
  
  temp <- setwd(file)
  absolutePath <- setwd(temp)
  
  e <- new.env()
  ffbase::load.ffdf(absolutePath, e)
  load(file.path(absolutePath, "metaData.Rdata"), e)
  result <- list(covariates = get("covariates", envir = e),
                 covariateRef = get("covariateRef", envir = e),
                 metaData = get("metaData", envir = e))
  # Open all ffdfs to prevent annoying messages later:
  open(result$covariates, readonly = readOnly)
  open(result$covariateRef, readonly = readOnly)
  
  class(result) <- "covariateData"
  rm(e)
  return(result)
}

#' Create covariate settings
#'
#' @details
#' creates an object specifying how covariates should be contructed from data in the CDM model.
#'
#' @param excludedCovariateConceptIds               A list of concept IDs that should NOT be used to
#'                                                  construct covariates.
#' @param includedCovariateConceptIds               A list of concept IDs that should be used to
#'                                                  construct covariates.
#' @param useCovariateDemographics                  A boolean value (TRUE/FALSE) to determine if
#'                                                  demographic covariates (age in 5-yr increments,
#'                                                  gender, race, ethnicity, year of index date, month
#'                                                  of index date) will be created and included in
#'                                                  future models.
#' @param useCovariateDemographicsGender            A boolean value (TRUE/FALSE) to determine if gender
#'                                                  should be included in the model.
#' @param useCovariateDemographicsRace              A boolean value (TRUE/FALSE) to determine if race
#'                                                  should be included in the model.
#' @param useCovariateDemographicsEthnicity         A boolean value (TRUE/FALSE) to determine if
#'                                                  ethnicity should be included in the model.
#' @param useCovariateDemographicsAge               A boolean value (TRUE/FALSE) to determine if age
#'                                                  (in 5 year increments) should be included in the
#'                                                  model.
#' @param useCovariateDemographicsYear              A boolean value (TRUE/FALSE) to determine if
#'                                                  calendar year should be included in the model.
#' @param useCovariateDemographicsMonth             A boolean value (TRUE/FALSE) to determine if
#'                                                  calendar month should be included in the model.
#' @param useCovariateConditionOccurrence           A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates derived from CONDITION_OCCURRENCE table
#'                                                  will be created and included in future models.
#' @param useCovariateConditionOccurrence365d       A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates will be created and used in models that
#'                                                  look for presence/absence of condition in 365d
#'                                                  window prior to or on cohort index date.  Only
#'                                                  applicable if useCovariateConditionOccurrence =
#'                                                  TRUE.
#' @param useCovariateConditionOccurrence30d        A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates will be created and used in models that
#'                                                  look for presence/absence of condition in 30d
#'                                                  window prior to or on cohort index date.  Only
#'                                                  applicable if useCovariateConditionOccurrence =
#'                                                  TRUE.
#' @param useCovariateConditionOccurrenceInpt180d   A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates will be created and used in models that
#'                                                  look for presence/absence of condition within
#'                                                  inpatient type in 180d window prior to or on cohort
#'                                                  index date.  Only applicable if
#'                                                  useCovariateConditionOccurrence = TRUE.
#' @param useCovariateConditionEra                  A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates derived from CONDITION_ERA table will be
#'                                                  created and included in future models.
#' @param useCovariateConditionEraEver              A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates will be created and used in models that
#'                                                  look for presence/absence of condition era anytime
#'                                                  prior to or on cohort index date.  Only applicable
#'                                                  if useCovariateConditionEra = TRUE.
#' @param useCovariateConditionEraOverlap           A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates will be created and used in models that
#'                                                  look for presence/absence of condition era that
#'                                                  overlaps the cohort index date.  Only applicable if
#'                                                  useCovariateConditionEra = TRUE.
#' @param useCovariateConditionGroup                A boolean value (TRUE/FALSE) to determine if all
#'                                                  CONDITION_OCCURRENCE and CONDITION_ERA covariates
#'                                                  should be aggregated or rolled-up to higher-level
#'                                                  concepts based on vocabluary classification.
#' @param useCovariateConditionGroupMeddra          A boolean value (TRUE/FALSE) to determine if all
#'                                                  CONDITION_OCCURRENCE and CONDITION_ERA covariates
#'                                                  should be aggregated or rolled-up to higher-level
#'                                                  concepts based on the MEDDRA classification.
#' @param useCovariateConditionGroupSnomed          A boolean value (TRUE/FALSE) to determine if all
#'                                                  CONDITION_OCCURRENCE and CONDITION_ERA covariates
#'                                                  should be aggregated or rolled-up to higher-level
#'                                                  concepts based on the SNOMED classification.
#' @param useCovariateDrugExposure                  A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates derived from DRUG_EXPOSURE table will be
#'                                                  created and included in future models.
#' @param useCovariateDrugExposure365d              A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates will be created and used in models that
#'                                                  look for presence/absence of drug in 365d window
#'                                                  prior to or on cohort index date.  Only applicable
#'                                                  if useCovariateDrugExposure = TRUE.
#' @param useCovariateDrugExposure30d               A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates will be created and used in models that
#'                                                  look for presence/absence of drug in 30d window
#'                                                  prior to or on cohort index date.  Only applicable
#'                                                  if useCovariateDrugExposure = TRUE.
#' @param useCovariateDrugEra                       A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates derived from DRUG_ERA table will be
#'                                                  created and included in future models.
#' @param useCovariateDrugEra365d                   A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates will be created and used in models that
#'                                                  look for presence/absence of drug era in 365d
#'                                                  window prior to or on cohort index date.  Only
#'                                                  applicable if useCovariateDrugEra = TRUE.
#' @param useCovariateDrugEra30d                    A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates will be created and used in models that
#'                                                  look for presence/absence of drug era in 30d window
#'                                                  prior to or on cohort index date.  Only applicable
#'                                                  if useCovariateDrugEra = TRUE.
#' @param useCovariateDrugEraEver                   A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates will be created and used in models that
#'                                                  look for presence/absence of drug era anytime prior
#'                                                  to or on cohort index date.  Only applicable if
#'                                                  useCovariateDrugEra = TRUE.
#' @param useCovariateDrugEraOverlap                A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates will be created and used in models that
#'                                                  look for presence/absence of drug era that overlaps
#'                                                  the cohort index date.  Only applicable if
#'                                                  useCovariateDrugEra = TRUE.
#' @param useCovariateDrugGroup                     A boolean value (TRUE/FALSE) to determine if all
#'                                                  DRUG_EXPOSURE and DRUG_ERA covariates should be
#'                                                  aggregated or rolled-up to higher-level concepts of
#'                                                  drug classes based on vocabluary classification.
#' @param useCovariateProcedureOccurrence           A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates derived from PROCEDURE_OCCURRENCE table
#'                                                  will be created and included in future models.
#' @param useCovariateProcedureOccurrence365d       A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates will be created and used in models that
#'                                                  look for presence/absence of procedure in 365d
#'                                                  window prior to or on cohort index date.  Only
#'                                                  applicable if useCovariateProcedureOccurrence =
#'                                                  TRUE.
#' @param useCovariateProcedureOccurrence30d        A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates will be created and used in models that
#'                                                  look for presence/absence of procedure in 30d
#'                                                  window prior to or on cohort index date.  Only
#'                                                  applicable if useCovariateProcedureOccurrence =
#'                                                  TRUE.
#' @param useCovariateProcedureGroup                A boolean value (TRUE/FALSE) to determine if all
#'                                                  PROCEDURE_OCCURRENCE covariates should be
#'                                                  aggregated or rolled-up to higher-level concepts
#'                                                  based on vocabluary classification.
#' @param useCovariateObservation                   A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates derived from OBSERVATION table will be
#'                                                  created and included in future models.
#' @param useCovariateObservation365d               A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates will be created and used in models that
#'                                                  look for presence/absence of observation in 365d
#'                                                  window prior to or on cohort index date.  Only
#'                                                  applicable if useCovariateObservation = TRUE.
#' @param useCovariateObservation30d                A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates will be created and used in models that
#'                                                  look for presence/absence of observation in 30d
#'                                                  window prior to or on cohort index date.  Only
#'                                                  applicable if useCovariateObservation = TRUE.
#' @param useCovariateObservationBelow              A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates will be created and used in models that
#'                                                  look for presence/absence of observation with a
#'                                                  numeric value below normal range for latest value
#'                                                  within 180d of cohort index.  Only applicable if
#'                                                  useCovariateObservation = TRUE.
#' @param useCovariateObservationAbove              A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates will be created and used in models that
#'                                                  look for presence/absence of observation with a
#'                                                  numeric value above normal range for latest value
#'                                                  within 180d of cohort index.  Only applicable if
#'                                                  useCovariateObservation = TRUE.
#' @param useCovariateObservationCount365d          A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates will be created and used in models that
#'                                                  look for the count of each observation concept in
#'                                                  365d window prior to or on cohort index date.  Only
#'                                                  applicable if useCovariateObservation = TRUE.
#' @param useCovariateConceptCounts                 A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates will be created and used in models that
#'                                                  count the number of concepts that a person has
#'                                                  within each domain (CONDITION, DRUG, PROCEDURE,
#'                                                  OBSERVATION)
#' @param useCovariateRiskScores                    A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates will be created and used in models that
#'                                                  calculate various Risk Scores, including Charlson,
#'                                                  DCSI.
#' @param useCovariateRiskScoresCharlson            A boolean value (TRUE/FALSE) to determine if the
#'                                                  Charlson comorbidity index should be included in
#'                                                  the model.
#' @param useCovariateRiskScoresDCSI                A boolean value (TRUE/FALSE) to determine if the
#'                                                  DCSI score should be included in the model.
#' @param useCovariateRiskScoresCHADS2              A boolean value (TRUE/FALSE) to determine if the
#'                                                  CHADS2 score should be included in the model.
#' @param useCovariateInteractionYear               A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates will be created and used in models that
#'                                                  represent interaction terms between all other
#'                                                  covariates and the year of the cohort index date.
#' @param useCovariateInteractionMonth              A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates will be created and used in models that
#'                                                  represent interaction terms between all other
#'                                                  covariates and the month of the cohort index date.
#' @param deleteCovariatesSmallCount                A numeric value used to remove covariates that
#'                                                  occur in both cohorts fewer than
#'                                                  deleteCovariateSmallCounts time.
#'
#' @return
#' An object of type \code{covariateSettings}, to be used in other functions.
#'
#' @export
createCovariateSettings <- function(useCovariateDemographics = TRUE,
                                    useCovariateDemographicsGender = TRUE,
                                    useCovariateDemographicsRace = TRUE,
                                    useCovariateDemographicsEthnicity = TRUE,
                                    useCovariateDemographicsAge = TRUE,
                                    useCovariateDemographicsYear = TRUE,
                                    useCovariateDemographicsMonth = TRUE,
                                    useCovariateConditionOccurrence = TRUE,
                                    useCovariateConditionOccurrence365d = TRUE,
                                    useCovariateConditionOccurrence30d = FALSE,
                                    useCovariateConditionOccurrenceInpt180d = FALSE,
                                    useCovariateConditionEra = FALSE,
                                    useCovariateConditionEraEver = FALSE,
                                    useCovariateConditionEraOverlap = FALSE,
                                    useCovariateConditionGroup = FALSE,
                                    useCovariateConditionGroupMeddra = FALSE,
                                    useCovariateConditionGroupSnomed = FALSE,
                                    useCovariateDrugExposure = FALSE,
                                    useCovariateDrugExposure365d = FALSE,
                                    useCovariateDrugExposure30d = FALSE,
                                    useCovariateDrugEra = FALSE,
                                    useCovariateDrugEra365d = FALSE,
                                    useCovariateDrugEra30d = FALSE,
                                    useCovariateDrugEraOverlap = FALSE,
                                    useCovariateDrugEraEver = FALSE,
                                    useCovariateDrugGroup = FALSE,
                                    useCovariateProcedureOccurrence = FALSE,
                                    useCovariateProcedureOccurrence365d = FALSE,
                                    useCovariateProcedureOccurrence30d = FALSE,
                                    useCovariateProcedureGroup = FALSE,
                                    useCovariateObservation = FALSE,
                                    useCovariateObservation365d = FALSE,
                                    useCovariateObservation30d = FALSE,
                                    useCovariateObservationBelow = FALSE,
                                    useCovariateObservationAbove = FALSE,
                                    useCovariateObservationCount365d = FALSE,
                                    useCovariateConceptCounts = FALSE,
                                    useCovariateRiskScores = FALSE,
                                    useCovariateRiskScoresCharlson = FALSE,
                                    useCovariateRiskScoresDCSI = FALSE,
                                    useCovariateRiskScoresCHADS2 = FALSE,
                                    useCovariateInteractionYear = FALSE,
                                    useCovariateInteractionMonth = FALSE,
                                    excludedCovariateConceptIds = c(),
                                    includedCovariateConceptIds = c(),
                                    deleteCovariatesSmallCount = 100) {
  # First: get the default values:
  covariateSettings <- list()
  for (name in names(formals(createCovariateSettings))) {
    covariateSettings[[name]] <- get(name)
  }
  # Next: overwrite defaults with actual values if specified: values <- as.list(match.call()) Note:
  # need this funky code to make sure parameters are stored as values, not symbols:
  values <- c(list(as.character(match.call()[[1]])),
              lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3))))
  for (name in names(values)) {
    if (name %in% names(covariateSettings))
      covariateSettings[[name]] <- values[[name]]
  }
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}

#' @export
print.covariateData <- function(x, ...) {
  writeLines("CovariateData object")
  writeLines("")
  writeLines(paste("Cohort of interest concept ID(s):",
                   paste(x$metaData$cohortIds, collapse = ",")))
}

#' @export
summary.covariateData <- function(object, ...) {
  result <- list(metaData = object$metaData,
                 covariateCount = nrow(object$covariateRef),
                 covariateValueCount = nrow(object$covariates))
  class(result) <- "summary.covariateData"
  return(result)
}

#' @export
print.summary.covariateData <- function(x, ...) {
  writeLines("CovariateData object summary")
  writeLines("")
  writeLines(paste("Number of covariates:", x$covariateCount))
  writeLines(paste("Number of non-zero covariate values:", x$covariateValueCount))
}
