# @file RunPlpHelpers.R
#
# Copyright 2024 Observational Health Data Sciences and Informatics
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
printHeader <- function(plpData,
                        targetId,
                        outcomeId,
                        analysisId,
                        analysisName,
                        executionDateTime) {
  ParallelLogger::logInfo(paste0("Patient-Level Prediction Package version ",
    utils::packageVersion("PatientLevelPrediction")))

  ParallelLogger::logInfo(paste0("Study started at: ", executionDateTime))

  ParallelLogger::logInfo(sprintf("%-20s%s", "AnalysisID: ", analysisId))
  ParallelLogger::logInfo(sprintf("%-20s%s", "AnalysisName: ", analysisName))

  # add header to analysis log
  ParallelLogger::logInfo(sprintf("%-20s%s", "TargetID: ", targetId))
  ParallelLogger::logInfo(sprintf("%-20s%s", "OutcomeID: ", outcomeId))
  ParallelLogger::logInfo(sprintf("%-20s%s", "Cohort size: ", nrow(plpData$cohorts)))
  if (!is.null(plpData$population)) {
    ParallelLogger::logInfo(sprintf("%-20s%s", "Initial population size: ",
      nrow(plpData$population)))
    ParallelLogger::logInfo(sprintf("%-20s%s", "Initial cases: ",
      sum(plpData$population$outcomeCount > 0)))
  }
  ParallelLogger::logInfo(sprintf("%-20s%s", "Covariates: ",
    plpData$covariateData$covariateRef %>% 
      dplyr::pull(.data$covariateId) %>% 
      length()))
  return(invisible(TRUE))
}


checkInputs <- function(inputs) {
  inputNames <- names(inputs)

  checkIsClass(inputs[["plpData"]], c("plpData"))
  checkIsClass(inputs[["outcomeId"]], c("numeric", "integer"))

  for (inputName in inputNames[!inputNames %in% c("plpData", "outcomeId")]) {
    ParallelLogger::logDebug(paste0(
      names(inputs[[inputName]]), " : ",
      unlist(lapply(inputs[[inputName]], function(x) paste0(unlist(x), collapse = "-")))
    ))

    # check class is correct
    if (!inherits(x = inputs[[inputName]], what = c(inputName, "list"))) {
      ParallelLogger::logError(paste0("Incorrect ", inputName))
      stop("Bad input")
    }

    if (inherits(x = inputs[[inputName]], what = "list")) {
      if (sum(unlist(lapply(inputs[[inputName]], function(obj) {
        inherits(x = obj, what = inputName)
      }))) != length(inputs[[inputName]])) {
        ParallelLogger::logError(paste0("Incorrect ", inputName))
        stop("Bad input list")
      }
    }
  }

  # return all the settings
  return(invisible(TRUE))
}



#' Creates list of settings specifying what parts of runPlp to execute
#'
#' @details
#' define what parts of runPlp to execute
#'
#' @param runSplitData            TRUE or FALSE whether to split data into train/test
#' @param runSampleData           TRUE or FALSE whether to over or under sample
#' @param runfeatureEngineering   TRUE or FALSE whether to do feature engineering
#' @param runPreprocessData       TRUE or FALSE whether to do preprocessing
#' @param runModelDevelopment     TRUE or FALSE whether to develop the model
#' @param runCovariateSummary     TRUE or FALSE whether to create covariate summary
#'
#' @return
#' list with TRUE/FALSE for each part of runPlp
#'
#' @export
createExecuteSettings <- function(
    runSplitData = FALSE,
    runSampleData = FALSE,
    runfeatureEngineering = FALSE,
    runPreprocessData = FALSE,
    runModelDevelopment = FALSE,
    runCovariateSummary = FALSE) {
  checkIsClass(runSplitData, "logical")
  checkIsClass(runSampleData, "logical")
  checkIsClass(runfeatureEngineering, "logical")
  checkIsClass(runPreprocessData, "logical")
  checkIsClass(runModelDevelopment, "logical")
  checkIsClass(runCovariateSummary, "logical")

  result <- list(
    runSplitData = runSplitData,
    runSampleData = runSampleData,
    runfeatureEngineering = runfeatureEngineering,
    runPreprocessData = runPreprocessData,
    runModelDevelopment = runModelDevelopment,
    runCovariateSummary = runCovariateSummary
  )
  class(result) <- "executeSettings"
  return(result)
}


#' Creates default list of settings specifying what parts of runPlp to execute
#'
#' @details
#' runs split, preprocess, model development and covariate summary
#'
#' @return
#' list with TRUE for split, preprocess, model development and covariate summary
#'
#' @export
createDefaultExecuteSettings <- function() {
  createExecuteSettings(
    runSplitData = TRUE,
    runSampleData = FALSE,
    runfeatureEngineering = FALSE,
    runPreprocessData = TRUE,
    runModelDevelopment = TRUE,
    runCovariateSummary = TRUE
  )
}
