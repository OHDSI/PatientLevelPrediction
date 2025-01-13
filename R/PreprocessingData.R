# @file PreprocessingData.R
# Copyright 2021 Observational Health Data Sciences and Informatics
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
#' Returns an object of class \code{preprocessingSettings} that specifies how to preprocess the training data
#'
#' @param minFraction             The minimum fraction of target population who must have a covariate for it to be included in the model training
#' @param normalize                    Whether to normalise the covariates before training (Default: TRUE)
#' @param removeRedundancy                 Whether to remove redundant features (Default: TRUE)
#' @return
#' An object of class \code{preprocessingSettings}
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


#' A function that wraps around FeatureExtraction::tidyCovariateData to normalise the data
#' and remove rare or redundant features
#'
#' @details
#' Returns an object of class \code{covariateData} that has been processed
#'
#' @param covariateData         The covariate part of the training data created by \code{splitData} after being sampled and having
#'                              any required feature engineering
#' @param preprocessSettings    The settings for the preprocessing created by \code{createPreprocessSettings}
#' @return the processed data
#' @keywords internal
#' The data processed
preprocessData <- function(covariateData,
                           preprocessSettings) {
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

#' A function that normalizes continous features to have values between 0 and 1
#' @details uses value - min / (max - min) to normalize the data
#' @param trainData The training data to be normalized
#' @param featureEngineeringSettings The settings for the normalization
#' @param normalized Whether the data has already been normalized (bool)
#' @return The normalized data
#' @keywords internal
minMaxNormalize <- function(trainData, featureEngineeringSettings, normalized = FALSE) {
  if (!normalized) {
    # fit the normalization
    # find continuous features from trainData$covariateData$analysisRef
    continousFeatures <- trainData$covariateData$analysisRef %>%
      dplyr::filter(.data$isBinary == "N") %>%
      dplyr::select("analysisId") %>%
      dplyr::inner_join(trainData$covariateData$covariateRef, by = "analysisId") %>%
      dplyr::pull(.data$covariateId)

    # get max of each feature
    trainData$covariateData$minMaxs <- trainData$covariateData$covariates %>%
      dplyr::filter(.data$covariateId %in% continousFeatures) %>%
      dplyr::group_by(.data$covariateId) %>%
      dplyr::summarise(
        max = max(.data$covariateValue, na.rm = TRUE),
        min = min(.data$covariateValue, na.rm = TRUE)
      ) %>%
      dplyr::collect()
    on.exit(trainData$covariateData$minMaxs <- NULL, add = TRUE)

    # save the normalization
    attr(featureEngineeringSettings, "minMaxs") <-
      trainData$covariateData$minMaxs %>% dplyr::collect()

    # apply the normalization to trainData
    trainData$covariateData$covariates <- trainData$covariateData$covariates %>%
      dplyr::left_join(trainData$covariateData$minMaxs, by = "covariateId") %>%
      # use ifelse to only normalize if min and max are not NA as is the case
      # for continous features, else return original value
      dplyr::mutate(covariateValue = ifelse(!is.na(min) & !is.na(max),
        (.data$covariateValue - min) / (max - min),
        .data$covariateValue
      )) %>%
      dplyr::select(-c("max", "min"))
    trainData$covariateData$minMaxs <- NULL
    normalized <- TRUE
  } else {
    # apply the normalization to test data by using saved normalization values
    trainData$covariateData$covariates <- trainData$covariateData$covariates %>%
      dplyr::left_join(attr(featureEngineeringSettings, "minMaxs"),
        by = "covariateId", copy = TRUE
      ) %>%
      dplyr::mutate(covariateValue = ifelse(!is.na(min) & !is.na(max),
        (.data$covariateValue - min) / (max - min),
        .data$covariateValue
      )) %>%
      dplyr::select(-c("max", "min"))
  }
  featureEngineering <- list(
    funct = "minMaxNormalize",
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings,
      normalized = normalized
    )
  )

  attr(trainData$covariateData, "metaData")$featureEngineering[["minMaxNormalize"]] <-
    featureEngineering
  
  return(trainData)
}

#' A function that normalizes continous by the interquartile range and forces
#' the resulting values to be between -3 and 3 with f(x) = x / sqrt(1 + (x/3)^2)
#' @details uses (value - median) / iqr to normalize the data and then 
#' applies the function f(x) = x / sqrt(1 + (x/3)^2) to the normalized values.
#' This forces the values to be between -3 and 3 while preserving the relative
#' ordering of the values.'
#' based on https://arxiv.org/abs/2407.04491 for more details
#' @param trainData The training data to be normalized
#' @param featureEngineeringSettings The settings for the normalization
#' @param normalized Whether the data has already been normalized (bool)
#' @return The normalized data
#' @keywords internal
robustNormalize <- function(trainData, featureEngineeringSettings, normalized = FALSE) {
  if (!normalized) {
    # find continuous features from trainData$covariateData$analysisRef
    continousFeatures <- trainData$covariateData$analysisRef %>%
      dplyr::filter(.data$isBinary == "N") %>%
      dplyr::select("analysisId") %>%
      dplyr::inner_join(trainData$covariateData$covariateRef, by = "analysisId") %>%
      dplyr::pull(.data$covariateId)

    # get (25, 75)% quantiles of each feature
    # sqlite (used by Andromeda) doesn't have quantile function, so we need to load the extension
    # to get upper_quartile and lower_quartile_functions
    RSQLite::initExtension(trainData$covariateData, "math")

    trainData$covariateData$quantiles <- trainData$covariateData$covariates %>%
      dplyr::filter(.data$covariateId %in% continousFeatures) %>%
      dplyr::group_by(.data$covariateId) %>%
      dplyr::summarise(
        q25 = dplyr::sql("lower_quartile(covariateValue)"),
        q75 = dplyr::sql("upper_quartile(covariateValue)"),
        median = median(.data$covariateValue, na.rm = TRUE)
      ) %>%
      dplyr::mutate(iqr = .data$q75 - .data$q25) %>%
      dplyr::select(-c("q75", "q25")) %>%
      dplyr::collect()
    on.exit(trainData$covariateData$quantiles <- NULL, add = TRUE)

    # save the normalization
    attr(featureEngineeringSettings, "quantiles") <-
      trainData$covariateData$quantiles %>% dplyr::collect()

    # apply the normalization to trainData
    trainData$covariateData$covariates <- trainData$covariateData$covariates %>%
      dplyr::left_join(trainData$covariateData$quantiles, by = "covariateId") %>%
      # use ifelse to only normalize continous features
      dplyr::mutate(covariateValue = ifelse(!is.na(.data$iqr) & !is.na(.data$median),
        (.data$covariateValue - .data$median) / .data$iqr,
        .data$covariateValue
      )) %>%
      # smoothly clip the range to [-3, 3] with  x / sqrt(1 + (x/3)^2)
      # ref: https://arxiv.org/abs/2407.04491
      dplyr::mutate(covariateValue = ifelse(!is.na(.data$iqr) & !is.na(.data$median),
        .data$covariateValue / sqrt(1 + (.data$covariateValue / 3)^2),
        .data$covariateValue
      )) %>%
      dplyr::select(-c("median", "iqr"))
    normalized <- TRUE
  } else {
    # apply the normalization to test data by using saved normalization values
    trainData$covariateData$covariates <- trainData$covariateData$covariates %>%
      dplyr::left_join(attr(featureEngineeringSettings, "quantiles"),
        by = "covariateId", copy = TRUE
      ) %>%
      dplyr::mutate(covariateValue = ifelse(!is.na(.data$iqr) & !is.na(.data$median),
        (.data$covariateValue - .data$median) / .data$iqr,
        .data$covariateValue
      )) %>%
      dplyr::mutate(covariateValue = ifelse(!is.na(.data$iqr) & !is.na(.data$median),
        .data$covariateValue / sqrt(1 + (.data$covariateValue / 3)^2),
        .data$covariateValue
      )) %>%
      dplyr::select(-c("median", "iqr"))
  }
  featureEngineering <- list(
    funct = "robustNormalize",
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings,
      normalized = normalized
    )
  )

  attr(trainData$covariateData, "metaData")$featureEngineering[["robustNormalize"]] <-
    featureEngineering
  return(trainData)
}

#' Create the settings for normalizing the data @param type The type of normalization to use, either "minmax" or "robust"
#' @return An object of class \code{featureEngineeringSettings}
#' @param type The type of normalization to use, either "minmax" or "robust"
#' @return An object of class \code{featureEngineeringSettings}'
#' @export
createNormalization <- function(type = "minmax") {
  featureEngineeringSettings <- list(
    type = type
  )
  if (type == "minmax") {
    attr(featureEngineeringSettings, "fun") <- "minMaxNormalize"
  } else if (type == "robust") {
    attr(featureEngineeringSettings, "fun") <- "robustNormalize"
  }

  class(featureEngineeringSettings) <- "featureEngineeringSettings"
  return(featureEngineeringSettings)
}

#' A function that removes rare features from the data
#' @details removes features that are present in less than a certain fraction of the population
#' @param trainData The data to be normalized
#' @param featureEngineeringSettings The settings for the normalization
#' @param findRare Whether to find and remove rare features or remove them only (bool)
#' @return The data with rare features removed
#' @keywords internal
removeRareFeatures <- function(trainData, featureEngineeringSettings, findRare = FALSE) {
  if (!findRare) {
    rareFeatures <- trainData$covariateData$covariates %>%
      dplyr::group_by(.data$covariateId) %>%
      dplyr::summarise(count = dplyr::n()) %>%
      dplyr::collect()
    rareFeatures <- rareFeatures %>%
      dplyr::mutate(ratio = .data$count / (
        trainData$covariateData$covariates %>%
          dplyr::summarise(popSize = dplyr::n_distinct(.data$rowId)) %>%
          dplyr::pull()
      )) %>%
      dplyr::filter(.data$ratio <= featureEngineeringSettings$ratio) %>%
      dplyr::pull(c("covariateId"))

    trainData$covariateData$covariates <- trainData$covariateData$covariates %>%
      dplyr::filter(!.data$covariateId %in% rareFeatures)
    trainData$covariateData$covariateRef <- trainData$covariateData$covariateRef %>%
      dplyr::filter(!.data$covariateId %in% rareFeatures)

    attr(featureEngineeringSettings, "rareFeatures") <- rareFeatures

    findRare <- TRUE
  } else {
    trainData$covariateData$covariates <- trainData$covariateData$covariates %>%
      dplyr::filter(
        !.data$covariateId %in% !!attr(featureEngineeringSettings, "rareFeatures")
      )
    trainData$covariateData$covariateRef <- trainData$covariateData$covariateRef %>%
      dplyr::filter(
        !.data$covariateId %in% !!attr(featureEngineeringSettings, "rareFeatures")
      )
  }
  featureEngineering <- list(
    funct = "removeRareFeatures",
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings,
      findRare = findRare
    )
  )
  attr(trainData$covariateData, "metaData")$featureEngineering[["removeRare"]] <- 
    featureEngineering
  return(trainData)
}

#' Create the settings for removing rare features
#' @param ratio The minimum fraction of the training data that must have a 
#' feature for it to be included
#' @return An object of class \code{featureEngineeringSettings}
#' @export
createRareFeatureRemover <- function(ratio = 0.001) {
  checkIsClass(ratio, c("numeric"))
  checkHigherEqual(ratio, 0)
  checkLower(ratio, 1)
  featureEngineeringSettings <- list(
    ratio = ratio
  )
  attr(featureEngineeringSettings, "fun") <- "removeRareFeatures"

  class(featureEngineeringSettings) <- "featureEngineeringSettings"
  return(featureEngineeringSettings)
}
