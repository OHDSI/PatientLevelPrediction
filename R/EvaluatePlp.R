# @file EvaluatePlp.R
#
# Copyright 2025 Observational Health Data Sciences and Informatics
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

#' evaluatePlp
#'
#' @description
#' Evaluates the performance of the patient level prediction model
#' @details
#' The function calculates various metrics to measure the performance of the model
#' @param prediction                         The patient level prediction model's prediction
#' @param typeColumn                         The column name in the prediction object that is used to
#'                                           stratify the evaluation
#' @return
#' An object of class plpEvaluation containing the following components
#' - evaluationStatistics: A data frame containing the evaluation statistics'
#' - thresholdSummary: A data frame containing the threshold summary'
#' - demographicSummary: A data frame containing the demographic summary'
#' - calibrationSummary: A data frame containing the calibration summary'
#' - predictionDistribution: A data frame containing the prediction distribution'
#' @examples
#' \donttest{ \dontshow{ # takes too long to run }
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n= 1500)
#' population <- createStudyPopulation(plpData, outcomeId = 3, 
#'                                     populationSettings = createStudyPopulationSettings())
#' data <- splitData(plpData, population, splitSettings=createDefaultSplitSetting(splitSeed=42))
#' data$Train$covariateData <- preprocessData(data$Train$covariateData, 
#'                                            createPreprocessSettings())
#' path <- file.path(tempdir(), "plp")
#' model <- fitPlp(data$Train, modelSettings=setLassoLogisticRegression(seed=42),
#'                 analysisId=1, analysisPath = path)
#' evaluatePlp(model$prediction) # Train and CV metrics
#' }
#' @export
evaluatePlp <- function(prediction, typeColumn = "evaluationType") {
  start <- Sys.time()
  # checking inputs
  # ========================================
  modelType <- attr(prediction, "metaData")$modelType

  # could remove the bit below to let people add custom types (but currently
  # we are thinking this should be set - so people should add a new type
  # evaluation into the package rather than use custom
  if (!modelType %in% c("binary", "survival")) {
    stop("Currently only support binary or survival classification models")
  }

  if (is.null(prediction$outcomeCount)) {
    stop("No outcomeCount column present")
  }
  if (length(unique(prediction$value)) == 1) {
    stop("Cannot evaluate as predictions all the same value")
  }

  # 1) evaluationSummary
  ParallelLogger::logTrace(paste0("Calulating evaluation summary Started @ ", Sys.time()))
  evaluationStatistics <- getEvaluationStatistics(
    prediction = prediction,
    predictionType = modelType,
    typeColumn = typeColumn
  )

  # 2) thresholdSummary
  # need to update thresholdSummary this with all the requested values
  ParallelLogger::logTrace(paste0("Calulating threshold summary Started @ ", Sys.time()))
  thresholdSummary <- tryCatch(
    {
      getThresholdSummary(
        prediction = prediction,
        predictionType = modelType,
        typeColumn = typeColumn
      )
    },
    error = function(e) {
      ParallelLogger::logInfo("getThresholdSummary error")
      ParallelLogger::logInfo(e)
      return(NULL)
    }
  )

  # 3) demographicSummary
  ParallelLogger::logTrace(paste0("Calulating Demographic Based Evaluation Started @ ", Sys.time()))
  demographicSummary <- tryCatch(
    {
      getDemographicSummary(
        prediction = prediction,
        predictionType = modelType,
        typeColumn = typeColumn
      )
    },
    error = function(e) {
      ParallelLogger::logInfo("getDemographicSummary error")
      ParallelLogger::logInfo(e)
      return(NULL)
    }
  )

  # 4) calibrationSummary
  ParallelLogger::logTrace(paste0("Calculating Calibration Summary Started @ ", Sys.time()))
  calibrationSummary <- tryCatch(
    {
      getCalibrationSummary(
        prediction = prediction,
        predictionType = modelType,
        typeColumn = typeColumn,
        numberOfStrata = 100,
        truncateFraction = 0.01
      )
    },
    error = function(e) {
      ParallelLogger::logInfo("getCalibrationSummary error")
      ParallelLogger::logInfo(e)
      return(NULL)
    }
  )


  # 5) predictionDistribution - done
  ParallelLogger::logTrace(paste0("Calculating Quantiles Started @ ", Sys.time()))
  predictionDistribution <- tryCatch(
    {
      getPredictionDistribution(
        prediction = prediction,
        predictionType = modelType,
        typeColumn = typeColumn
      )
    },
    error = function(e) {
      ParallelLogger::logInfo("getPredictionDistribution error")
      ParallelLogger::logInfo(e)
      return(NULL)
    }
  )

  result <- list(
    evaluationStatistics = evaluationStatistics,
    thresholdSummary = thresholdSummary,
    demographicSummary = demographicSummary,
    calibrationSummary = calibrationSummary,
    predictionDistribution = predictionDistribution
  )

  class(result) <- "plpEvaluation"
  delta <- Sys.time() - start
  ParallelLogger::logInfo("Time to calculate evaluation metrics: ", 
    signif(delta, 3), " ", attr(delta, "units"))
  return(result)
}




#' Calculate the model-based concordance, which is a calculation of the expected 
#' discrimination performance of a model under the assumption the model predicts 
#' the "TRUE" outcome as detailed in van Klaveren et al. 
#' https://pubmed.ncbi.nlm.nih.gov/27251001/
#'
#' @details
#' Calculate the model-based concordance
#'
#' @param prediction         the prediction object found in the plpResult object
#'
#' @return
#' The model-based concordance value
#' @examples
#' prediction <- data.frame(value = runif(100))
#' modelBasedConcordance(prediction)
#' @export
modelBasedConcordance <- function(prediction) {
  if (!length(prediction$value > 0)) {
    stop("Prediction object not found")
  }
  prediction <- prediction$value
  n <- length(prediction)
  ord <- order(prediction)
  prediction <- prediction[ord]
  qHat <- 1 - prediction
  v1 <- (prediction * (cumsum(qHat) - qHat) + 
    qHat * (sum(prediction) - cumsum(prediction))) / (n - 1)
  v2 <- (prediction * (sum(qHat) - qHat) + qHat * (sum(prediction) - prediction)) / (n - 1)
  mbC <- sum(v1) / sum(v2)
  return(mbC)
}
