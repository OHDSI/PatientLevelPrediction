# @file GetDefaultCovariates.R
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

#' Get default covariate information from the database
#'
#' @description
#' Constructs a large default set of covariates for one or more cohorts using data in the CDM schema.
#' Includes covariates for all drugs, drug classes, condition, condition classes, procedures,
#' observations, etc.
#'
#' @param covariateSettings   An object of type \code{defaultCovariateSettings} as created using the
#'                            \code{\link{createCovariateSettings}} function.
#'
#' @template GetCovarParams
#'
#' @export
getDbDefaultCovariateData <- function(connection,
                                      oracleTempSchema = NULL,
                                      cdmDatabaseSchema,
                                      cdmVersion = "4",
                                      cohortTempTable = "cohort_person",
                                      rowIdField = "subject_id",
                                      covariateSettings) {
  writeLines("Constructing default covariates")
  if (substr(cohortTempTable, 1, 1) != "#") {
    cohortTempTable <- paste("#", cohortTempTable, sep = "")
  }
  cdmDatabase <- strsplit(cdmDatabaseSchema, "\\.")[[1]][1]
  if (!covariateSettings$useCovariateConditionGroupMeddra & !covariateSettings$useCovariateConditionGroupSnomed) {
    covariateSettings$useCovariateConditionGroup <- FALSE
  }

  if (cdmVersion == "4") {
    cohortDefinitionId <- "cohort_concept_id"
    conceptClassId <- "concept_class"
    measurement <- "observation"
  } else {
    cohortDefinitionId <- "cohort_definition_id"
    conceptClassId <- "concept_class_id"
    measurement <- "measurement"
  }

  if (is.null(covariateSettings$excludedCovariateConceptIds) || length(covariateSettings$excludedCovariateConceptIds) ==
      0) {
    hasExcludedCovariateConceptIds <- FALSE
  } else {
    if (!is.numeric(covariateSettings$excludedCovariateConceptIds))
      stop("excludedCovariateConceptIds must be a (vector of) numeric")
    hasExcludedCovariateConceptIds <- TRUE
    DatabaseConnector::insertTable(connection,
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
    DatabaseConnector::insertTable(connection,
                                   tableName = "#included_cov",
                                   data = data.frame(concept_id = as.integer(covariateSettings$includedCovariateConceptIds)),
                                   dropTableIfExists = TRUE,
                                   createTable = TRUE,
                                   tempTable = TRUE,
                                   oracleTempSchema = oracleTempSchema)
  }

  renderedSql <- SqlRender::loadRenderTranslateSql("GetCovariates.sql",
                                                   packageName = "PatientLevelPrediction",
                                                   dbms = attr(connection, "dbms"),
                                                   oracleTempSchema = oracleTempSchema,
                                                   cdm_database = cdmDatabase,
                                                   cdm_version = cdmVersion,
                                                   cohort_temp_table = cohortTempTable,
                                                   row_id_field = rowIdField,
                                                   cohort_definition_id = cohortDefinitionId,
                                                   concept_class_id = conceptClassId,
                                                   measurement = measurement,
                                                   use_covariate_cohort_id_is_1 = covariateSettings$useCovariateCohortIdIs1,
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
                                                   use_covariate_observation_count365d = covariateSettings$useCovariateObservationCount365d,
                                                   use_covariate_measurement = covariateSettings$useCovariateMeasurement,
                                                   use_covariate_measurement_365d = covariateSettings$useCovariateMeasurement365d,
                                                   use_covariate_measurement_30d = covariateSettings$useCovariateMeasurement30d,
                                                   use_covariate_measurement_count365d = covariateSettings$useCovariateMeasurementCount365d,
                                                   use_covariate_measurement_below = covariateSettings$useCovariateMeasurementBelow,
                                                   use_covariate_measurement_above = covariateSettings$useCovariateMeasurementAbove,
                                                   use_covariate_concept_counts = covariateSettings$useCovariateConceptCounts,
                                                   use_covariate_risk_scores = covariateSettings$useCovariateRiskScores,
                                                   use_covariate_risk_scores_Charlson = covariateSettings$useCovariateRiskScoresCharlson,
                                                   use_covariate_risk_scores_DCSI = covariateSettings$useCovariateRiskScoresDCSI,
                                                   use_covariate_risk_scores_CHADS2 = covariateSettings$useCovariateRiskScoresCHADS2,
                                                   use_covariate_risk_scores_CHADS2VASc = covariateSettings$useCovariateRiskScoresCHADS2VASc,
                                                   use_covariate_interaction_year = covariateSettings$useCovariateInteractionYear,
                                                   use_covariate_interaction_month = covariateSettings$useCovariateInteractionMonth,
                                                   has_excluded_covariate_concept_ids = hasExcludedCovariateConceptIds,
                                                   has_included_covariate_concept_ids = hasIncludedCovariateConceptIds,
                                                   delete_covariates_small_count = covariateSettings$deleteCovariatesSmallCount)

  DatabaseConnector::executeSql(connection, renderedSql)
  writeLines("Done")

  writeLines("Fetching data from server")
  start <- Sys.time()
  covariateSql <- "SELECT row_id, covariate_id, covariate_value FROM #cov ORDER BY covariate_id, row_id"
  covariateSql <- SqlRender::renderSql(covariateSql, cohort_definition_id = cohortDefinitionId)$sql
  covariateSql <- SqlRender::translateSql(covariateSql,
                                          "sql server",
                                          attr(connection, "dbms"),
                                          oracleTempSchema)$sql
  covariates <- DatabaseConnector::querySql.ffdf(connection, covariateSql)
  covariateRefSql <- "SELECT covariate_id, covariate_name, analysis_id, concept_id  FROM #cov_ref ORDER BY covariate_id"
  covariateRefSql <- SqlRender::translateSql(covariateRefSql,
                                             "sql server",
                                             attr(connection, "dbms"),
                                             oracleTempSchema)$sql
  covariateRef <- DatabaseConnector::querySql.ffdf(connection, covariateRefSql)

  sql <- "SELECT COUNT_BIG(*) FROM @cohort_temp_table"
  sql <- SqlRender::renderSql(sql, cohort_temp_table = cohortTempTable)$sql
  sql <- SqlRender::translateSql(sql,
                                 targetDialect = attr(connection, "dbms"),
                                 oracleTempSchema = oracleTempSchema)$sql
  populationSize <- DatabaseConnector::querySql(connection, sql)[1, 1]

  delta <- Sys.time() - start
  writeLines(paste("Loading took", signif(delta, 3), attr(delta, "units")))

  renderedSql <- SqlRender::loadRenderTranslateSql("RemoveCovariateTempTables.sql",
                                                   packageName = "PatientLevelPrediction",
                                                   dbms = attr(connection, "dbms"),
                                                   oracleTempSchema = oracleTempSchema)
  DatabaseConnector::executeSql(connection,
                                renderedSql,
                                progressBar = FALSE,
                                reportOverallTime = FALSE)

  colnames(covariates) <- SqlRender::snakeCaseToCamelCase(colnames(covariates))
  colnames(covariateRef) <- SqlRender::snakeCaseToCamelCase(colnames(covariateRef))

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
    # Next, from groups of covariates that together cover every row, remove the most prevalence one
    problematicAnalysisIds <- c(2, 3, 4, 5, 6, 7)  # Gender, race, ethnicity, age, year, month
    for (analysisId in problematicAnalysisIds) {
      t <- covariateRef$analysisId == analysisId
      if (ffbase::sum.ff(t) != 0) {
        covariateIds <- ff::as.ram(covariateRef$covariateId[ffbase::ffwhich(t, t == TRUE)])
        freq <- sapply(covariateIds, function(x) {
          ffbase::sum.ff(covariates$covariateId == x)
        })
        if (sum(freq) == populationSize) {
          # Each row belongs to one of the categories, making one redunant. Remove most prevalent one
          categoryToDelete <- covariateIds[which(freq == max(freq))[1]]
          deletedCovariateIds <- c(deletedCovariateIds, categoryToDelete)
          t <- covariates$covariateId == categoryToDelete
          covariates <- covariates[ffbase::ffwhich(t, t == FALSE), ]
        }
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


#' Create covariate settings
#'
#' @details
#' creates an object specifying how covariates should be contructed from data in the CDM model.
#'
#' @param excludedCovariateConceptIds               A list of concept IDs that should NOT be used to
#'                                                  construct covariates.
#' @param includedCovariateConceptIds               A list of concept IDs that should be used to
#'                                                  construct covariates.
#' @param useCovariateCohortIdIs1                   A boolean value (TRUE/FALSE) to determine if a
#'                                                  covariate should be contructed for whether the
#'                                                  cohort ID is 1 (currently primarily used in
#'                                                  CohortMethod).
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
#' @param useCovariateObservationCount365d          A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates will be created and used in models that
#'                                                  look for the count of each observation concept in
#'                                                  365d window prior to or on cohort index date.  Only
#'                                                  applicable if useCovariateObservation = TRUE.
#' @param useCovariateMeasurement                   A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates derived from OBSERVATION table will be
#'                                                  created and included in future models.
#' @param useCovariateMeasurement365d               A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates will be created and used in models that
#'                                                  look for presence/absence of measurement in 365d
#'                                                  window prior to or on cohort index date.  Only
#'                                                  applicable if useCovariateMeasurement = TRUE.
#' @param useCovariateMeasurement30d                A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates will be created and used in models that
#'                                                  look for presence/absence of measurement in 30d
#'                                                  window prior to or on cohort index date.  Only
#'                                                  applicable if useCovariateMeasurement = TRUE.
#' @param useCovariateMeasurementCount365d          A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates will be created and used in models that
#'                                                  look for the count of each measurement concept in
#'                                                  365d window prior to or on cohort index date.  Only
#'                                                  applicable if useCovariateMeasurement = TRUE.
#' @param useCovariateMeasurementBelow              A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates will be created and used in models that
#'                                                  look for presence/absence of measurement with a
#'                                                  numeric value below normal range for latest value
#'                                                  within 180d of cohort index.  Only applicable if
#'                                                  useCovariateMeasurement = TRUE (CDM v5+) or
#'                                                  useCovariateObservation = TRUE (CDM v4).
#' @param useCovariateMeasurementAbove              A boolean value (TRUE/FALSE) to determine if
#'                                                  covariates will be created and used in models that
#'                                                  look for presence/absence of measurement with a
#'                                                  numeric value above normal range for latest value
#'                                                  within 180d of cohort index.  Only applicable if
#'                                                  useCovariateMeasurement = TRUE (CDM v5+) or
#'                                                  useCovariateObservation = TRUE (CDM v4).
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
#' @param useCovariateRiskScoresCHADS2VASc          A boolean value (TRUE/FALSE) to determine if the
#'                                                  CHADS2VASc score should be included in the model.
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
#' An object of type \code{defaultCovariateSettings}, to be used in other functions.
#'
#' @export
createCovariateSettings <- function(useCovariateCohortIdIs1 = FALSE,
                                    useCovariateDemographics = TRUE,
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
                                    useCovariateObservationCount365d = FALSE,
                                    useCovariateMeasurement = FALSE,
                                    useCovariateMeasurement365d = FALSE,
                                    useCovariateMeasurement30d = FALSE,
                                    useCovariateMeasurementCount365d = FALSE,
                                    useCovariateMeasurementBelow = FALSE,
                                    useCovariateMeasurementAbove = FALSE,
                                    useCovariateConceptCounts = FALSE,
                                    useCovariateRiskScores = FALSE,
                                    useCovariateRiskScoresCharlson = FALSE,
                                    useCovariateRiskScoresDCSI = FALSE,
                                    useCovariateRiskScoresCHADS2 = FALSE,
                                    useCovariateRiskScoresCHADS2VASc = FALSE,
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
  # Next: overwrite defaults with actual values if specified:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(covariateSettings))
      covariateSettings[[name]] <- values[[name]]
  }
  attr(covariateSettings, "fun") <- "getDbDefaultCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}
