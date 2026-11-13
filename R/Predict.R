# @file predict.R
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

#' predictPlp
#'
#' @description
#' Predict the risk of the outcome using the input plpModel for the input plpData
#' @details
#' The function applied the trained model on the plpData to make predictions
#' @param plpModel                         An object of type \code{plpModel} - a patient level prediction model
#' @param plpData                          An object of type \code{plpData} - the patient level prediction
#'                                         data extracted from the CDM.
#' @param population                       The population created using createStudyPopulation() who will have their risks predicted or a cohort without the outcome known
#' @param timepoint                        Survival models only: the number of days since the start of the
#'                                         time-at-risk window at which to predict risk, counted like
#'                                         survivalTime (the first day of the window is day 1). The default
#'                                         is the length of the window, riskWindowEnd - riskWindowStart + 1,
#'                                         which needs both ends of the window anchored to the same date (both
#'                                         cohort start or both cohort end). With other anchors the window
#'                                         length differs between patients, so a timepoint must be given. A
#'                                         timepoint after the end of the window is an error.
#' @return A data frame containing the predicted risk values
#' @examples
#' coefficients <- data.frame(
#'   covariateId = c(1002),
#'   coefficient = c(0.05)
#' )
#' model <- createGlmModel(coefficients, intercept = -2.5)
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n = 50, seed = 42)
#' prediction <- predictPlp(model, plpData, plpData$cohorts)
#' # see the predicted risk values
#' head(prediction)
#' @export
predictPlp <- function(plpModel, plpData, population, timepoint) {
  predictPlpData(
    plpModel = plpModel,
    plpData = plpData,
    population = population,
    timepoint = if (missing(timepoint)) NULL else timepoint,
    timepointGiven = !missing(timepoint)
  )
}

# predictPlp() for callers that own plpData and do not use its covariates
# afterwards (runPlp()'s test split): with ownsData, feature engineering and
# tidying work on plpData itself instead of on a copy. timepointGiven tells a
# timepoint of NULL that was given apart from one that was not.
predictPlpData <- function(plpModel,
                           plpData,
                           population,
                           timepoint = NULL,
                           timepointGiven = !is.null(timepoint),
                           ownsData = FALSE) {
  start <- Sys.time()
  if (is.null(plpModel)) {
    stop("No model input")
  }
  if (is.null(population)) {
    stop("No population input")
  }
  if (is.null(plpData)) {
    stop("No plpData input")
  }

  populationSettings <- attr(population, "metaData")$populationSettings
  if (identical(attr(plpModel, "modelType"), "survival")) {
    # one horizon, on the survivalTime scale, for the prediction and its evaluation
    timepoint <- getSurvivalTimepoint(
      populationSettings,
      if (timepointGiven) timepoint else NULL
    )
    attr(population, "timepoint") <- timepoint
  } else if (timepointGiven) {
    attr(population, "timepoint") <- timepoint
  } else {
    timepoint <- populationSettings$riskWindowEnd
  }

  # do feature engineering/selection
  if (!is.null(plpModel$preprocessing$featureEngineering)) {
    # imputers fill in the rows in labels, which must be the rows predicted
    plpData$labels <- population
    plpData <- do.call(
      applyFeatureEngineering,
      list(
        plpData = plpData,
        settings = plpModel$preprocessing$featureEngineering,
        copy = !ownsData
      )
    )
    featureEngineering <- TRUE
  } else {
    featureEngineering <- FALSE
  }

  ParallelLogger::logTrace("did FE")

  if (!is.null(plpModel$preprocessing$tidyCovariates)) {
    # do preprocessing
    # after feature engineering the data is already a private copy
    plpData$covariateData <- do.call(
      applyTidyCovariateData,
      list(
        covariateData = plpData$covariateData,
        preprocessSettings = plpModel$preprocessing$tidyCovariates,
        inPlace = featureEngineering || ownsData
      )
    )
    tidyCovariates <- TRUE
  } else {
    tidyCovariates <- FALSE
  }

  ParallelLogger::logTrace("did tidy")
  # feature engineering and tidying worked on copies, which are not returned
  if (featureEngineering || tidyCovariates) {
    on.exit(closeAndromedas(list(plpData$covariateData)), add = TRUE)
  }

  # apply prediction function
  prediction <- do.call(
    getPredictionFunction(plpModel),
    list(
      plpModel = plpModel,
      data = plpData,
      cohort = population
    )
  )

  if (!is.null(attr(prediction, "metaData"))) {
    metaData <- attr(prediction, "metaData")
  } else {
    metaData <- list()
  }

  # add metaData
  metaData$modelType <- attr(plpModel, "modelType")
  metaData$targetId <- attr(population, "metaData")$targetId
  metaData$outcomeId <- attr(population, "metaData")$outcomeId
  metaData$timepoint <- timepoint

  # added information about running preprocessing/FE
  metaData$tidyCovariates <- tidyCovariates
  metaData$featureEngineering <- featureEngineering

  attr(prediction, "metaData") <- metaData
  delta <- Sys.time() - start
  ParallelLogger::logInfo(
    "Prediction done in: ",
    signif(delta, 3), " ", attr(delta, "units")
  )
  return(prediction)
}

# Survival models are fitted, predicted and evaluated on the survivalTime scale:
# days since the start of the time-at-risk window, counting that day as 1. Their
# horizon is the length of the window, which is the same for every patient only
# when both ends of the window have the same anchor. NULL when the anchors differ.
getSurvivalHorizon <- function(populationSettings) {
  if (is.null(populationSettings) ||
    !identical(populationSettings$startAnchor, populationSettings$endAnchor)) {
    return(NULL)
  }
  return(populationSettings$riskWindowEnd - populationSettings$riskWindowStart + 1)
}

# The timepoint at which a survival model predicts risk and is evaluated: the
# given timepoint, which must lie within the time-at-risk window, or the window
# length.
getSurvivalTimepoint <- function(populationSettings, timepoint = NULL) {
  horizon <- getSurvivalHorizon(populationSettings)
  if (is.null(timepoint)) {
    if (is.null(horizon)) {
      reason <- if (is.null(populationSettings)) {
        "the population has no populationSettings"
      } else {
        paste0(
          "the time-at-risk window starts at ", populationSettings$startAnchor,
          " and ends at ", populationSettings$endAnchor,
          ", so its length differs between patients"
        )
      }
      stop(
        "A survival model predicts risk at a fixed time, but ", reason, ". ",
        "Anchor both ends of the time-at-risk window to the same date ",
        "(both cohort start or both cohort end), or give predictPlp() a timepoint."
      )
    }
    return(horizon)
  }
  if (!is.numeric(timepoint) || length(timepoint) != 1 || !is.finite(timepoint) || timepoint <= 0) {
    stop("timepoint must be one positive number of days since the start of the time-at-risk window.")
  }
  if (!is.null(horizon) && timepoint > horizon) {
    stop(
      "timepoint (", timepoint, ") is after the end of the time-at-risk window, which is ",
      horizon, " days long, so there is no follow-up to predict or evaluate at."
    )
  }
  return(timepoint)
}

# Baseline survival at a timepoint, read as the step function it is: the value at
# the last baseline time at or before the timepoint, or 1 before the first one.
baselineSurvivalAt <- function(baselineSurvival, timepoint) {
  atOrBefore <- which(baselineSurvival$time <= timepoint)
  if (length(atOrBefore) == 0) {
    return(1)
  }
  return(baselineSurvival$surv[atOrBefore[which.max(baselineSurvival$time[atOrBefore])]])
}


# Replays the fitted feature engineering steps on new data. Steps change the
# data they are given, and the caller's data may be shared with other models,
# so this makes the one copy they work on.
# Applies saved feature engineering steps to plpData; with copy (the default)
# they work on a copy, so the caller's data is left as it was.
applyFeatureEngineering <- function(
    plpData,
    settings,
    copy = TRUE) {
  steps <- featureEngineeringSteps(settings)
  if (copy) {
    plpData <- isolateCovariateData(plpData)
  }
  for (step in steps) {
    args <- step$settings
    args$trainData <- plpData
    plpData <- do.call(resolveHook(step$funct, "feature engineering"), args)
  }
  return(plpData)
}


# Applies the tidy settings of a model (removal of infrequent and redundant
# covariates, normalization) to covariateData in one query. With inPlace, the
# covariates are rewritten in covariateData itself, for data the caller already
# owns, such as the copy feature engineering made; otherwise the result is a new
# CovariateData and covariateData is left as it was.
applyTidyCovariateData <- function(
    covariateData,
    preprocessSettings,
    inPlace = FALSE) {
  if (!FeatureExtraction::isCovariateData(covariateData)) {
    stop("Data not of class CovariateData")
  }

  maxs <- preprocessSettings$normFactors
  deleteRedundantCovariateIds <- preprocessSettings$deletedRedundantCovariateIds
  deletedInfrequentCovariateIds <- preprocessSettings$deletedInfrequentCovariateIds

  deleteCovariateIds <- c(deleteRedundantCovariateIds, deletedInfrequentCovariateIds)
  covariateRef <- covariateData$covariateRef %>% dplyr::collect()
  includeCovariates <- data.frame(
    covariateId = covariateRef$covariateId[!covariateRef$covariateId %in% deleteCovariateIds]
  )
  covariateData$includeCovariates <- includeCovariates
  on.exit(covariateData$includeCovariates <- NULL, add = TRUE)

  ParallelLogger::logInfo("Removing infrequent and redundant covariates and normalizing")
  start <- Sys.time()

  covariates <- covariateData$covariates %>%
    dplyr::inner_join(covariateData$includeCovariates, by = "covariateId")
  # models trained without normalization and saved by earlier versions have a
  # data frame without columns instead of no factors
  if (!is.null(maxs) && ncol(maxs) > 0) {
    if ("bins" %in% colnames(maxs)) {
      covariateData$maxes <- dplyr::as_tibble(maxs) %>%
        dplyr::rename(covariateId = "bins") %>%
        dplyr::rename(maxValue = "maxs")
    } else {
      covariateData$maxes <- maxs
    }
    on.exit(covariateData$maxes <- NULL, add = TRUE)
    covariates <- covariates %>%
      dplyr::inner_join(covariateData$maxes, by = "covariateId") %>%
      dplyr::mutate(value = 1.0 * .data$covariateValue / .data$maxValue) %>%
      dplyr::select(-"covariateValue") %>%
      dplyr::rename(covariateValue = "value")
  }

  deletedCovariateTimeIds <- preprocessSettings$deletedRedundantCovariateTimeIds
  if (length(deletedCovariateTimeIds) > 0 &&
    "timeId" %in% colnames(covariateData$covariates)) {
    covariateData$deletedCovariateTimeIds <- as.data.frame(deletedCovariateTimeIds)
    on.exit(covariateData$deletedCovariateTimeIds <- NULL, add = TRUE)
    covariates <- covariates %>%
      dplyr::anti_join(covariateData$deletedCovariateTimeIds, by = c("covariateId", "timeId"))
  }

  # the reference tables are small: written once, as data frames
  covariateRef <- covariateRef %>%
    dplyr::inner_join(includeCovariates, by = "covariateId")
  if (inPlace) {
    covariateData$covariates <- covariates
    covariateData$covariateRef <- covariateRef
    newCovariateData <- covariateData
  } else {
    newCovariateData <- Andromeda::andromeda(
      covariateRef = covariateRef,
      analysisRef = covariateData$analysisRef %>% dplyr::collect()
    )
    if (!is.null(covariateData$timeRef)) {
      newCovariateData$timeRef <- covariateData$timeRef %>% dplyr::collect()
    }
    newCovariateData$covariates <- covariates
    newCovariateData <- asCovariateData(newCovariateData)
  }

  delta <- Sys.time() - start
  writeLines(paste("Removing infrequent and redundant covariates covariates and normalizing took", signif(delta, 3), attr(delta, "units")))

  # return processed data
  return(newCovariateData)
}

# The function that predicts with plpModel: the predict hook of its model
# settings, or the predictionFunction attribute that Cyclops models and models
# saved by earlier versions carry, resolved as resolveHook() does.
getPredictionFunction <- function(plpModel) {
  predictFun <- plpModel$modelDesign$modelSettings$settings$predict
  if (is.null(predictFun)) {
    predictFun <- attr(plpModel, "predictionFunction")
  }
  if (is.null(predictFun)) {
    stop("No prediction function available for this model.")
  }
  resolveHook(predictFun, "predict")
}
