# @file PreprocessingData.R
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

#' Create the settings for preprocessing the trainData.
#'
#' @details
#' Returns an object of class `preprocessingSettings` that specifies how to
#' preprocess the training data
#'
#' @param minFraction The minimum fraction of target population who must have a 
#' covariate for it to be included in the model training
#' @param normalize Whether to normalise the covariates before training 
#' (Default: TRUE)
#' @param removeRedundancy Whether to remove redundant features (Default: TRUE)
#' Redundant features are features that within an analysisId together cover all
#' observations. For example with ageGroups, if you have ageGroup 0-18 and 18-100
#' and all patients are in one of these groups, then one of these groups is redundant.
#' @return
#' An object of class `preprocessingSettings`
#' @examples
#' # Create the settings for preprocessing, remove no features, normalise the data
#' createPreprocessSettings(minFraction = 0.0, normalize = TRUE, removeRedundancy = FALSE)
#' @export
createPreprocessSettings <- function(
    minFraction = 0.001,
    normalize = TRUE,
    removeRedundancy = TRUE) {
  checkIsClass(minFraction, c("numeric", "integer"))
  checkHigherEqual(minFraction, 0)

  checkIsClass(normalize, c("logical"))

  checkIsClass(removeRedundancy, c("logical"))

  preprocessingSettings <- list(
    minFraction = minFraction,
    normalize = normalize,
    removeRedundancy = removeRedundancy
  )

  class(preprocessingSettings) <- "preprocessSettings"
  return(preprocessingSettings)
}

#' A function that wraps around FeatureExtraction::tidyCovariateData to normalise 
#' the data and remove rare or redundant features
#'
#' @details
#' Returns an object of class \code{covariateData} that has been processed. 
#' This includes normalising the data and removing rare or redundant features.
#' Redundant features are features that within an analysisId together cover
#' all obervations.
#'
#' @param covariateData         The covariate part of the training data created by \code{splitData} after being sampled and having
#'                              any required feature engineering
#' @param preprocessSettings    The settings for the preprocessing created by \code{createPreprocessSettings}
#' The data processed
#' @return The covariateData object with the processed covariates
#' @examples
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n=1000)
#' preProcessedData <- preprocessData(plpData$covariateData, createPreprocessSettings())
#' # check age is normalized by max value
#' preProcessedData$covariates %>% dplyr::filter(.data$covariateId == 1002)
#' @export
preprocessData <- function(covariateData,
                           preprocessSettings = createPreprocessSettings()) {
  metaData <- attr(covariateData, "metaData")
  preprocessSettingsInput <- preprocessSettings # saving this before adding covariateData

  checkIsClass(covariateData, c("CovariateData"))
  checkIsClass(preprocessSettings, c("preprocessSettings"))

  ParallelLogger::logDebug(paste0("minFraction: ", preprocessSettings$minFraction))
  ParallelLogger::logDebug(paste0("normalize: ", preprocessSettings$normalize))
  ParallelLogger::logDebug(paste0("removeRedundancy: ", preprocessSettings$removeRedundancy))

  preprocessSettings$covariateData <- covariateData
  covariateData <- do.call(FeatureExtraction::tidyCovariateData, preprocessSettings)

  # update covariateRef
  removed <- unique(c(
    attr(covariateData, "metaData")$deletedInfrequentCovariateIds,
    attr(covariateData, "metaData")$deletedRedundantCovariateIds
  ))
  covariateData$covariateRef <- covariateData$covariateRef %>%
    dplyr::filter(!.data$covariateId %in% removed)

  metaData$tidyCovariateDataSettings <- attr(covariateData, "metaData")
  metaData$preprocessSettings <- preprocessSettingsInput
  attr(covariateData, "metaData") <- metaData

  return(covariateData)
}
