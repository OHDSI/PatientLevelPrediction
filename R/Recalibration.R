# @file Recalibration.R
#
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


#' recalibratePlpRefit
#'
#' @description
#' Recalibrating a model by refitting it
#'
#' @param plpModel                         The trained plpModel (runPlp$model)
#' @param newPopulation                    The population created using createStudyPopulation() who will have their risks predicted
#' @param newData                          An object of type \code{plpData} - the patient level prediction
#'                                         data extracted from the CDM.
#' @param returnModel                      Logical: return the refitted model
#' @return
#' An prediction dataframe with the predictions of the recalibrated model added
#'
#' @export
recalibratePlpRefit <- function(
    plpModel,
    newPopulation,
    newData, 
    returnModel = FALSE) {
  checkNotNull(plpModel)
  checkNotNull(newPopulation)
  checkNotNull(newData)
  checkIsClass(plpModel, "plpModel")
  checkIsClass(newData, "plpData")
  checkBoolean(returnModel)

  # get selected covariates
  includeCovariateIds <- plpModel$covariateImportance %>%
    dplyr::filter(.data$covariateValue != 0) %>%
    dplyr::select("covariateId") %>%
    dplyr::pull()

  # check which covariates are included in new data
  containedIds <- newData$covariateData$covariateRef %>% dplyr::collect()
  noShrinkage <- intersect(includeCovariateIds, containedIds$covariateId)

  # add intercept
  noShrinkage <- append(noShrinkage, 0, 0)

  setLassoRefit <- setLassoLogisticRegression(
    includeCovariateIds = includeCovariateIds,
    noShrinkage = noShrinkage,
    maxIterations = 10000 # increasing this due to test code often not converging
  )

  newData$labels <- newPopulation

  newData$folds <- data.frame(
    rowId = newData$labels$rowId,
    index = sample(2, length(newData$labels$rowId), replace = TRUE)
  )

  # add dummy settings to fit model
  attr(newData, "metaData")$outcomeId <- attr(newPopulation, "metaData")$outcomeId
  attr(newData, "metaData")$targetId <- attr(newPopulation, "metaData")$targetId
  attr(newData, "metaData")$restrictPlpDataSettings <- attr(newPopulation, "metaData")$restrictPlpDataSettings
  attr(newData, "metaData")$covariateSettings <- newData$metaData$covariateSettings
  attr(newData, "metaData")$populationSettings <- attr(newPopulation, "metaData")$populationSettings
  attr(newData$covariateData, "metaData")$featureEngineeringSettings <- PatientLevelPrediction::createFeatureEngineeringSettings()
  attr(newData$covariateData, "metaData")$preprocessSettings <- PatientLevelPrediction::createPreprocessSettings()
  attr(newData, "metaData")$splitSettings <- PatientLevelPrediction::createDefaultSplitSetting()
  attr(newData, "metaData")$sampleSettings <- PatientLevelPrediction::createSampleSettings()

  newModel <- tryCatch(
    {
      fitPlp(
        trainData = newData,
        modelSettings = setLassoRefit,
        analysisId = "recalibrationRefit",
        analysisPath = NULL
      )
    },
    error = function(e) {
      ParallelLogger::logInfo(e)
      return(NULL)
    }
  )
  if (is.null(newModel)) {
    ParallelLogger::logInfo("Recalibration fit failed")
    return(NULL)
  }

  newModel$prediction$evaluationType <- "recalibrationRefit"

  oldPred <- predictPlp(
    plpModel = plpModel,
    plpData = newData,
    population = newPopulation,
    timepoint = 0
  )

  oldPred$evaluationType <- "validation"

  prediction <- rbind(
    oldPred,
    newModel$prediction[, colnames(oldPred)]
  )

  if (!is.null(newModel$covariateImportance)) {
    adjust <- newModel$covariateImportance %>%
      dplyr::filter(.data$covariateValue != 0) %>%
      dplyr::select(
        "covariateId",
        "covariateValue"
      )
  } else {
    adjust <- c()
  }

  newIntercept <- newModel$model$coefficients[names(newModel$model$coefficients) == "(Intercept)"]

  attr(prediction, "metaData")$recalibratePlpRefit <- list(adjust = adjust, newIntercept = newIntercept)
  
  if (returnModel) {
    return(list(prediction = prediction, model = newModel))
  } else {
    return(prediction)
  }
}


#' recalibratePlp
#'
#' @description
#' Recalibrating a model using the recalibrationInTheLarge or weakRecalibration methods 
#' 
#' @details
#' 'recalibrationInTheLarge' calculates a single correction factor for the 
#' average predicted risks to match the average observed risks.
#' 'weakRecalibration' fits a glm model to the logit of the predicted risks,
#' also known as Platt scaling/logistic recalibration.
#'
#' @param prediction                      A prediction dataframe
#' @param analysisId                      The model analysisId
#' @param typeColumn                      The column name where the strata types are specified
#' @param method                          Method used to recalibrate ('recalibrationInTheLarge' or 'weakRecalibration' )
#' @return
#' A prediction dataframe with the recalibrated predictions added
#'

#' @export
recalibratePlp <- function(prediction, analysisId, typeColumn = "evaluationType",
                           method = c("recalibrationInTheLarge", "weakRecalibration")) {
  # check input:
  if (!inherits(x = prediction, what = "data.frame")) {
    stop("Incorrect prediction")
  }

  if (!method %in% c("recalibrationInTheLarge", "weakRecalibration")) {
    stop("Unknown recalibration method type. must be of type: recalibrationInTheLarge, weakRecalibration")
  }

  prediction <- do.call(method, list(prediction = prediction, columnType = typeColumn))

  return(prediction)
}


#' recalibrationInTheLarge
#'
#' @description
#' Recalibrate a model using the recalibrationInTheLarge method which calculates a single correction factor
#' for the average predicted risks to match the average observed risks
#' 
#' @param prediction                      A prediction dataframe
#' @param analysisId                      The model analysisId
#' @param typeColumn                      The column name where the strata types are specified
#' @param method                          Method used to recalibrate ('recalibrationInTheLarge' or 'weakRecalibration' )
#' @return
#' An prediction dataframe with the recalibrated predictions added
#' @internal
recalibrationInTheLarge <- function(prediction, columnType = "evaluationType") {
  if (attr(prediction, "metaData")$modelType == "binary") {
    misCal <- calibrationInLarge(prediction)
    obsOdds <- misCal$observedRisk / (1 - misCal$observedRisk)
    predOdds <- misCal$meanPredictionRisk / (1 - misCal$meanPredictionRisk)
    correctionFactor <- log(obsOdds / predOdds)

    recalibrated <- prediction
    recalibrated$value <- plogis(qlogis(recalibrated$value) + correctionFactor)

    recalibrated[, columnType] <- "recalibrationInTheLarge"
    prediction <- rbind(prediction, recalibrated)
    attr(prediction, "metaData")$recalibrationInTheLarge <-
      list(correctionFactor = correctionFactor)

    return(prediction)
  }

  if (attr(prediction, "metaData")$modelType == "survival") {
    ParallelLogger::logError("Survival recal in the large not currently available")
  }
}

#' weakRecalibration
#' 
#' @description 
#' Recalibrate a model using the weakRecalibration method which fits a glm model
#' to the logit of the predicted risks.
#' Alsi known as Platt scaling/logistic recalibration
#' @param prediction                    A prediction dataframe
#' @param columnType                    The column name where the strata types are specified
#' @return 
#' An prediction dataframe with the recalibrated predictions added
#' @internal
weakRecalibration <- function(prediction, columnType = "evaluationType") {
  # if binary:
  if (attr(prediction, "metaData")$modelType == "binary") {
    recalibrated <- prediction
    epsilon <- .Machine$double.eps
    recalibrated$value <- pmin(pmax(recalibrated$value, epsilon), 1 - epsilon)

    y <- ifelse(recalibrated$outcomeCount > 0, 1, 0)

    # convert risk probailities to logits
    inverseLog <- qlogis(recalibrated$value)
    refit <- suppressWarnings(stats::glm(y ~ inverseLog, family = "binomial"))

    recalibrated$value <- plogis((inverseLog * refit$coefficients[2]) + refit$coefficients[1])

    recalibrated[, columnType] <- "weakRecalibration"
    prediction <- rbind(prediction, recalibrated)
    attr(prediction, "metaData")$weakRecalibration <- list(
      adjustGradient = refit$coefficients[2],
      adjustIntercept = refit$coefficients[1]
    )

    return(prediction)
  }

  # add if survival
  if (attr(prediction, "metaData")$modelType == "survival") {
    recalibrated <- prediction

    # this will make the recalibration work if the baselineSurvival is missing
    baseline <- ifelse(is.null(attr(recalibrated, "baselineSurvival")), 0.9, attr(recalibrated, "baselineSurvival"))
    ParallelLogger::logInfo(paste0("recal initial baseline hazard: ", baseline))

    offset <- ifelse(is.null(attr(recalibrated, "offset")), 0, attr(recalibrated, "offset"))
    ParallelLogger::logInfo(paste0("recal initial offset: ", offset))

    timepoint <- ifelse(is.null(attr(recalibrated, "timePoint")), 365, attr(recalibrated, "timePoint"))
    ParallelLogger::logInfo(paste0("recal initial timepoint: ", timepoint))

    if (!is.null(baseline)) {
      lp <- log(log(1 - recalibrated$value) / log(baseline)) + offset
    } else {
      lp <- log(recalibrated$value)
    }


    t <- apply(cbind(recalibrated$daysToCohortEnd, recalibrated$survivalTime), 1, min)
    y <- ifelse(recalibrated$outcomeCount > 0, 1, 0) # observed outcome
    y[t > timepoint] <- 0
    t[t > timepoint] <- timepoint
    S <- survival::Surv(t, y)
    #### Intercept + Slope recalibration
    f.slope <- survival::coxph(S ~ lp)
    h.slope <- max(survival::basehaz(f.slope)$hazard) # maximum OK because of prediction_horizon
    lp.slope <- stats::predict(f.slope)
    recalibrated$value <- 1 - exp(-h.slope * exp(lp.slope))
    # 1-h.slope^exp(lp.slope)


    recalibrated[, columnType] <- "weakRecalibration"
    prediction <- rbind(prediction, recalibrated)
    attr(prediction, "metaData")$weakRecalibration <- list(
      adjustGradient = f.slope$coefficients["lp"],
      adjustIntercept = h.slope
    )

    return(prediction)
  }
}
