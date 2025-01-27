# @file PredictionDistribution.R
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

#' Calculates the prediction distribution
#'
#' @details
#' Calculates the quantiles from a predition object
#'
#' @param prediction            A prediction object
#' @param predictionType        The type of prediction (binary or survival)
#' @param typeColumn            A column that is used to stratify the results
#'
#' @return
#' The 0.00, 0.1, 0.25, 0.5, 0.75, 0.9, 1.00 quantile pf the prediction,
#' the mean and standard deviation per class
#' @export
getPredictionDistribution <- function(
    prediction,
    predictionType,
    typeColumn = "evaluation") {
  evaluation <- do.call(
    what = paste0("getPredictionDistribution_", predictionType),
    args = list(
      prediction = prediction,
      evalColumn = typeColumn,
      timepoint = attr(prediction, "metaData")$timepoint
    )
  )

  return(evaluation)
}


#' Calculates the prediction distribution
#'
#' @details
#' Calculates the quantiles from a predition object
#'
#' @param prediction            A prediction object
#' @param evalColumn            A column that is used to stratify the results
#' @param ...                   Other inputs
#'
#' @return
#' The 0.00, 0.1, 0.25, 0.5, 0.75, 0.9, 1.00 quantile pf the prediction,
#' the mean and standard deviation per class
#'
getPredictionDistribution_binary <- function(prediction, evalColumn, ...) {
  result <- c()
  evalTypes <- unique(as.data.frame(prediction)[, evalColumn])

  for (evalType in evalTypes) {
    predictionOfInterest <- prediction %>% dplyr::filter(.data[[evalColumn]] == evalType)


    # PersonCount - count the number of persons in the class
    predictionDistribution <- stats::aggregate(
      predictionOfInterest$value,
      list(predictionOfInterest$outcomeCount),
      length
    )

    colnames(predictionDistribution) <- c("class", "PersonCount")

    # averagePredictedProbability	StDevPredictedProbability
    averagePredictedProbability <- stats::aggregate(
      predictionOfInterest$value,
      list(predictionOfInterest$outcomeCount),
      mean
    )
    colnames(averagePredictedProbability) <- c("class", "averagePredictedProbability")

    StDevPredictedProbability <- stats::aggregate(
      predictionOfInterest$value,
      list(predictionOfInterest$outcomeCount),
      stats::sd
    )
    colnames(StDevPredictedProbability) <- c("class", "StDevPredictedProbability")

    predictionDistribution <- merge(predictionDistribution, averagePredictedProbability)
    predictionDistribution <- merge(predictionDistribution, StDevPredictedProbability)

    quantiles <- stats::aggregate(
      predictionOfInterest$value,
      list(predictionOfInterest$outcomeCount),
      function(x) {
        stats::quantile(
          x,
          probs = c(0.00, 0.05, 0.25, 0.5, 0.75, 0.95, 1.00)
        )
      }
    )
    quantiles <- as.matrix(quantiles)
    colnames(quantiles) <- c(
      "class", "MinPredictedProbability", "P05PredictedProbability",
      "P25PredictedProbability", "MedianPredictedProbability",
      "P75PredictedProbability", "P95PredictedProbability",
      "MaxPredictedProbability"
    )

    predictionDistribution <- merge(predictionDistribution, quantiles)
    predictionDistribution$evaluation <- evalType

    result <- rbind(result, predictionDistribution)
  }

  return(result)
}


getPredictionDistribution_survival <- function(prediction, evalColumn, timepoint, ...) {
  ParallelLogger::logWarn("PredictionDistribution not available for survival models")
  return(NULL)
}
