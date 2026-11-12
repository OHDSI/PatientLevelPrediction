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
      if (missing(timepoint)) NULL else timepoint
    )
    attr(population, "timepoint") <- timepoint
  } else if (!missing(timepoint)) {
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
        settings = plpModel$preprocessing$featureEngineering
      )
    )
    featureEngineering <- TRUE
  } else {
    featureEngineering <- FALSE
  }

  ParallelLogger::logTrace("did FE")

  if (!is.null(plpModel$preprocessing$tidyCovariates)) {
    # do preprocessing
    plpData$covariateData <- do.call(
      applyTidyCovariateData,
      list(
        covariateData = plpData$covariateData,
        preprocessSettings = plpModel$preprocessing$tidyCovariates
      )
    )
    tidyCovariates <- TRUE
  } else {
    tidyCovariates <- FALSE
  }

  ParallelLogger::logTrace("did tidy")

  # apply prediction function
  settings <- plpModel$modelDesign$modelSettings$settings
  predictFun <- settings$predict
  if (is.null(predictFun)) {
    predictFun <- attr(plpModel, "predictionFunction")
  }
  if (is.null(predictFun)) {
    stop("No prediction function available for this model.")
  }
  prediction <- do.call(
    if (is.character(predictFun)) eval(parse(text = predictFun)) else predictFun,
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


applyFeatureEngineering <- function(
    plpData,
    settings) {
  settings <- featureEngineeringSteps(settings)

  # The caller's data may be shared between models, and transforms (including
  # user-written ones) rewrite Andromeda tables in place.
  plpData <- isolateCovariateData(plpData)

  # add code for implementing the feature engineering
  for (set in settings) {
    set$settings$trainData <- plpData
    plpData <- do.call(eval(parse(text = set$funct)), set$settings)
  }

  # dont do anything for now
  return(plpData)
}


# NEED TO UPDATE....
# fucntion for implementing the pre-processing (normalisation and redundant features removal)
applyTidyCovariateData <- function(
    covariateData,
    preprocessSettings) {
  if (!FeatureExtraction::isCovariateData(covariateData)) {
    stop("Data not of class CovariateData")
  }

  newCovariateData <- Andromeda::andromeda(
    covariateRef = covariateData$covariateRef,
    analysisRef = covariateData$analysisRef
  )
  if (!is.null(covariateData$timeRef)) {
    newCovariateData$timeRef <- covariateData$timeRef
  }

  maxs <- preprocessSettings$normFactors
  deleteRedundantCovariateIds <- preprocessSettings$deletedRedundantCovariateIds
  deletedInfrequentCovariateIds <- preprocessSettings$deletedInfrequentCovariateIds

  # --- added for speed
  deleteCovariateIds <- c(deleteRedundantCovariateIds, deletedInfrequentCovariateIds)
  temp <- covariateData$covariateRef %>% dplyr::collect()
  allCovariateIds <- temp$covariateId
  covariateData$includeCovariates <- data.frame(covariateId = allCovariateIds[!allCovariateIds %in% deleteCovariateIds])
  on.exit(covariateData$includeCovariates <- NULL, add = TRUE)
  # ---

  ParallelLogger::logInfo("Removing infrequent and redundant covariates and normalizing")
  start <- Sys.time()

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

    newCovariateData$covariates <- covariateData$covariates %>%
      dplyr::inner_join(covariateData$includeCovariates, by = "covariateId") %>% # added as join
      dplyr::inner_join(covariateData$maxes, by = "covariateId") %>%
      dplyr::mutate(value = 1.0 * .data$covariateValue / .data$maxValue) %>%
      dplyr::select(-"covariateValue") %>%
      dplyr::rename(covariateValue = "value")
  } else {
    newCovariateData$covariates <- covariateData$covariates %>%
      dplyr::inner_join(covariateData$includeCovariates, by = "covariateId")
  }

  deletedCovariateTimeIds <- preprocessSettings$deletedRedundantCovariateTimeIds
  if (length(deletedCovariateTimeIds) > 0 &&
    "timeId" %in% colnames(newCovariateData$covariates)) {
    newCovariateData$deletedCovariateTimeIds <- as.data.frame(deletedCovariateTimeIds)
    newCovariateData$covariates <- newCovariateData$covariates %>%
      dplyr::anti_join(newCovariateData$deletedCovariateTimeIds, by = c("covariateId", "timeId"))
    newCovariateData$deletedCovariateTimeIds <- NULL
  }

  # reduce covariateRef
  newCovariateData$covariateRef <- covariateData$covariateRef %>%
    dplyr::inner_join(covariateData$includeCovariates, by = "covariateId")

  class(newCovariateData) <- "CovariateData"

  delta <- Sys.time() - start
  writeLines(paste("Removing infrequent and redundant covariates covariates and normalizing took", signif(delta, 3), attr(delta, "units")))

  # return processed data
  return(newCovariateData)
}
