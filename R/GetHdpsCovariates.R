# @file GetHdpsCovariates.R
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

#' Get HDPS covariate information from the database
#'
#' @description
#' Constructs the set of covariates for one or more cohorts using data in the CDM schema. This
#' implements the covariates typically used in the HDPS algorithm.
#'
#' @param covariateSettings   An object of type \code{covariateSettings} as created using the
#'                            \code{\link{createHdpsCovariateSettings}} function.
#'
#' @template GetCovarParams
#'
#' @export
getDbHdpsCovariateData <- function(connection,
                                   oracleTempSchema = NULL,
                                   cdmDatabaseSchema,
                                   cdmVersion = "4",
                                   cohortTempTable = "cohort_person",
                                   rowIdField = "subject_id",
                                   covariateSettings) {
  writeLines("Constructing HDPS covariates")
  if (substr(cohortTempTable, 1, 1) != "#") {
    cohortTempTable <- paste("#", cohortTempTable, sep = "")
  }
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
  
  renderedSql <- SqlRender::loadRenderTranslateSql("GetHdpsCovariates.sql",
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
                                                   use_covariate_3_digit_icd_9_inpatient_180d = covariateSettings$useCovariate3DigitIcd9Inpatient180d,
                                                   use_covariate_3_digit_icd_9_inpatient_180d_med_f = covariateSettings$useCovariate3DigitIcd9Inpatient180dMedF,
                                                   use_covariate_3_digit_icd_9_inpatient_180d_75_f = covariateSettings$useCovariate3DigitIcd9Inpatient180d75F,
                                                   use_covariate_3_digit_icd_9_ambulatory_180d = covariateSettings$useCovariate3DigitIcd9Ambulatory180d,
                                                   use_covariate_3_digit_icd_9_ambulatory_180d_med_f = covariateSettings$useCovariate3DigitIcd9Ambulatory180dMedF,
                                                   use_covariate_3_digit_icd_9_ambulatory_180d_75_f = covariateSettings$useCovariate3DigitIcd9Ambulatory180d75F,
                                                   use_covariate_drug_exposure = covariateSettings$useCovariateDrugExposure,
                                                   use_covariate_ingredient_exposure_180d = covariateSettings$useCovariateIngredientExposure180d,
                                                   use_covariate_ingredient_exposure_180d_med_f = covariateSettings$useCovariateIngredientExposure180dMedF,
                                                   use_covariate_ingredient_exposure_180d_75_f = covariateSettings$useCovariateIngredientExposure180d75F,
                                                   use_covariate_procedure_occurrence = covariateSettings$useCovariateProcedureOccurrence,
                                                   use_covariate_inpatient_procedure_occurrence_180d = covariateSettings$useCovariateProcedureOccurrenceInpatient180d,
                                                   use_covariate_inpatient_procedure_occurrence_180d_med_f = covariateSettings$useCovariateProcedureOccurrenceInpatient180dMedF,
                                                   use_covariate_inpatient_procedure_occurrence_180d_75_f = covariateSettings$useCovariateProcedureOccurrenceInpatient180d75F,
                                                   use_covariate_ambulatory_procedure_occurrence_180d = covariateSettings$useCovariateProcedureOccurrenceAmbulatory180d,
                                                   use_covariate_ambulatory_procedure_occurrence_180d_med_f = covariateSettings$useCovariateProcedureOccurrenceAmbulatory180dMedF,
                                                   use_covariate_ambulatory_procedure_occurrence_180d_75_f = covariateSettings$useCovariateProcedureOccurrenceAmbulatory180d75F,
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


#' Create HDPS covariate settings
#'
#' @details
#' creates an object specifying how covariates should be contructed from data in the CDM model.
#'
#' @param useCovariateCohortIdIs1                             A boolean value (TRUE/FALSE) to determine
#'                                                            if a covariate should be contructed for
#'                                                            whether the cohort ID is 1 (currently
#'                                                            primarily used in CohortMethod).
#' @param useCovariateDemographics                            A boolean value (TRUE/FALSE) to determine
#'                                                            if demographic covariates (age in 5-yr
#'                                                            increments, gender, race, ethnicity, year
#'                                                            of index date, month of index date) will
#'                                                            be created and included in future models.
#' @param useCovariateDemographicsGender                      A boolean value (TRUE/FALSE) to determine
#'                                                            if gender should be included in the
#'                                                            model.
#' @param useCovariateDemographicsRace                        A boolean value (TRUE/FALSE) to determine
#'                                                            if race should be included in the model.
#' @param useCovariateDemographicsEthnicity                   A boolean value (TRUE/FALSE) to determine
#'                                                            if ethnicity should be included in the
#'                                                            model.
#' @param useCovariateDemographicsAge                         A boolean value (TRUE/FALSE) to determine
#'                                                            if age (in 5 year increments) should be
#'                                                            included in the model.
#' @param useCovariateDemographicsYear                        A boolean value (TRUE/FALSE) to determine
#'                                                            if calendar year should be included in
#'                                                            the model.
#' @param useCovariateDemographicsMonth                       A boolean value (TRUE/FALSE) to determine
#'                                                            if calendar month should be included in
#'                                                            the model.
#' @param useCovariateConditionOccurrence                     A boolean value (TRUE/FALSE) to determine
#'                                                            if covariates derived from
#'                                                            CONDITION_OCCURRENCE table will be
#'                                                            created and included in future models.
#' @param useCovariate3DigitIcd9Inpatient180d                 A boolean value (TRUE/FALSE) to determine
#'                                                            if covariates will be created and used in
#'                                                            models that look for presence/absence of
#'                                                            condition within inpatient setting in
#'                                                            180d window prior to or on cohort index
#'                                                            date. Conditions are aggregated at the
#'                                                            ICD-9 3-digit level. Only applicable if
#'                                                            useCovariateConditionOccurrence = TRUE.
#' @param useCovariate3DigitIcd9Inpatient180dMedF             Similar to
#'                                                            \code{useCovariate3DigitIcd9Inpatient180d},
#'                                                            but now only if the frequency of the
#'                                                            ICD-9 code is higher than the median.
#' @param useCovariate3DigitIcd9Inpatient180d75F              Similar to
#'                                                            \code{useCovariate3DigitIcd9Inpatient180d},
#'                                                            but now only if the frequency of the
#'                                                            ICD-9 code is higher than the 75th
#'                                                            percentile.
#' @param useCovariate3DigitIcd9Ambulatory180d                A boolean value (TRUE/FALSE) to determine
#'                                                            if covariates will be created and used in
#'                                                            models that look for presence/absence of
#'                                                            condition within ambulatory setting in
#'                                                            180d window prior to or on cohort index
#'                                                            date. Conditions are aggregated at the
#'                                                            ICD-9 3-digit level. Only applicable if
#'                                                            useCovariateConditionOccurrence = TRUE.
#' @param useCovariate3DigitIcd9Ambulatory180dMedF            Similar to
#'                                                            \code{useCovariate3DigitIcd9Ambulatory180d},
#'                                                            but now only if the frequency of the
#'                                                            ICD-9 code is higher than the median.
#' @param useCovariate3DigitIcd9Ambulatory180d75F             Similar to
#'                                                            \code{useCovariate3DigitIcd9Ambulatory180d},
#'                                                            but now only if the frequency of the
#'                                                            ICD-9 code is higher than the 75th
#'                                                            percentile.
#' @param useCovariateDrugExposure                            A boolean value (TRUE/FALSE) to determine
#'                                                            if covariates derived from DRUG_EXPOSURE
#'                                                            table will be created and included in
#'                                                            future models.
#' @param useCovariateIngredientExposure180d                  A boolean value (TRUE/FALSE) to determine
#'                                                            if covariates will be created and used in
#'                                                            models that look for presence/absence of
#'                                                            drug ingredients within inpatient setting
#'                                                            in 180d window prior to or on cohort
#'                                                            index date.  Only applicable if
#'                                                            useCovariateDrugExposure = TRUE.
#' @param useCovariateIngredientExposure180dMedF              Similar to
#'                                                            \code{useCovariateIngredientExposure180d},
#'                                                            but now only if the frequency of the
#'                                                            ingredient is higher than the median.
#' @param useCovariateIngredientExposure180d75F               Similar to
#'                                                            \code{useCovariateIngredientExposure180d},
#'                                                            but now only if the frequency of the
#'                                                            ingredient is higher than the 75th
#'                                                            percentile.
#' @param useCovariateProcedureOccurrence                     A boolean value (TRUE/FALSE) to determine
#'                                                            if covariates derived from
#'                                                            PROCEDURE_OCCURRENCE table will be
#'                                                            created and included in future models.
#' @param useCovariateProcedureOccurrenceInpatient180d        A boolean value (TRUE/FALSE) to determine
#'                                                            if covariates will be created and used in
#'                                                            models that look for presence/absence of
#'                                                            procedures within inpatient setting in
#'                                                            180d window prior to or on cohort index
#'                                                            date.  Only applicable if
#'                                                            useCovariateProcedureOccurrence = TRUE.
#' @param useCovariateProcedureOccurrenceInpatient180dMedF    Similar to
#'                                                            \code{useCovariateProcedureOccurrenceInpatient180d},
#'                                                            but now only if the frequency of the
#'                                                            procedure code is higher than the median.
#' @param useCovariateProcedureOccurrenceInpatient180d75F     Similar to
#'                                                            \code{useCovariateProcedureOccurrenceInpatient180d},
#'                                                            but now only if the frequency of the
#'                                                            procedure code is higher than the 75th
#'                                                            percentile.
#' @param useCovariateProcedureOccurrenceAmbulatory180d       A boolean value (TRUE/FALSE) to determine
#'                                                            if covariates will be created and used in
#'                                                            models that look for presence/absence of
#'                                                            procedures within ambulatory setting in
#'                                                            180d window prior to or on cohort index
#'                                                            date.  Only applicable if
#'                                                            useCovariateProcedureOccurrence = TRUE.
#' @param useCovariateProcedureOccurrenceAmbulatory180dMedF   Similar to
#'                                                            \code{useCovariateProcedureOccurrenceAmbulatory180d},
#'                                                            but now only if the frequency of the
#'                                                            procedure code is higher than the median.
#' @param useCovariateProcedureOccurrenceAmbulatory180d75F    Similar to
#'                                                            \code{useCovariateProcedureOccurrenceAmbulatory180d},
#'                                                            but now only if the frequency of the
#'                                                            procedure code is higher than the 75th
#'                                                            percentile.
#' @param excludedCovariateConceptIds                         A list of concept IDs that should NOT be
#'                                                            used to construct covariates.
#' @param includedCovariateConceptIds                         A list of concept IDs that should be used
#'                                                            to construct covariates.
#' @param deleteCovariatesSmallCount                          A numeric value used to remove covariates
#'                                                            that occur in both cohorts fewer than
#'                                                            deleteCovariateSmallCounts time.
#'
#' @return
#' An object of type \code{hdpsCovariateSettings}, to be used in other functions.
#'
#' @export
createHdpsCovariateSettings <- function(useCovariateCohortIdIs1 = FALSE,
                                        useCovariateDemographics = TRUE,
                                        useCovariateDemographicsGender = TRUE,
                                        useCovariateDemographicsRace = TRUE,
                                        useCovariateDemographicsEthnicity = TRUE,
                                        useCovariateDemographicsAge = TRUE,
                                        useCovariateDemographicsYear = TRUE,
                                        useCovariateDemographicsMonth = TRUE,
                                        useCovariateConditionOccurrence = TRUE,
                                        useCovariate3DigitIcd9Inpatient180d = FALSE,
                                        useCovariate3DigitIcd9Inpatient180dMedF = FALSE,
                                        useCovariate3DigitIcd9Inpatient180d75F = FALSE,
                                        useCovariate3DigitIcd9Ambulatory180d = FALSE,
                                        useCovariate3DigitIcd9Ambulatory180dMedF = FALSE,
                                        useCovariate3DigitIcd9Ambulatory180d75F = FALSE,
                                        useCovariateDrugExposure = FALSE,
                                        useCovariateIngredientExposure180d = FALSE,
                                        useCovariateIngredientExposure180dMedF = FALSE,
                                        useCovariateIngredientExposure180d75F = FALSE,
                                        useCovariateProcedureOccurrence = FALSE,
                                        useCovariateProcedureOccurrenceInpatient180d = FALSE,
                                        useCovariateProcedureOccurrenceInpatient180dMedF = FALSE,
                                        useCovariateProcedureOccurrenceInpatient180d75F = FALSE,
                                        useCovariateProcedureOccurrenceAmbulatory180d = FALSE,
                                        useCovariateProcedureOccurrenceAmbulatory180dMedF = FALSE,
                                        useCovariateProcedureOccurrenceAmbulatory180d75F = FALSE,
                                        excludedCovariateConceptIds = c(),
                                        includedCovariateConceptIds = c(),
                                        deleteCovariatesSmallCount = 100) {
  # First: get the default values:
  covariateSettings <- list()
  for (name in names(formals(createHdpsCovariateSettings))) {
    covariateSettings[[name]] <- get(name)
  }
  # Next: overwrite defaults with actual values if specified:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(covariateSettings))
      covariateSettings[[name]] <- values[[name]]
  }
  attr(covariateSettings, "fun") <- "getDbHdpsCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}
