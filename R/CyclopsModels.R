# @file lassoLogisticRegression.R
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


fitCyclopsModel <- function(
    trainData,
    modelSettings, # old:param,
    search = "adaptive",
    analysisId,
    ...) {
  param <- modelSettings$param

  # check plpData is coo format:
  if (!FeatureExtraction::isCovariateData(trainData$covariateData)) {
    stop("Needs correct covariateData")
  }

  settings <- attr(param, "settings")

  trainData$covariateData$labels <- trainData$labels %>%
    dplyr::mutate(
      y = sapply(.data$outcomeCount, function(x) min(1, x)),
      time = .data$survivalTime
    )
  on.exit(trainData$covariateData$labels <- NULL, add = TRUE)

  covariates <- filterCovariateIds(param, trainData$covariateData)

  if (!is.null(param$priorCoefs)) {
    sourceCoefs <- param$priorCoefs %>%
      dplyr::filter(abs(.data$betas) > 0 & .data$covariateIds != "(Intercept)")

    newCovariates <- covariates %>%
      dplyr::filter(.data$covariateId %in% !!sourceCoefs$covariateIds) %>%
      dplyr::mutate(newCovariateId = .data$covariateId * -1) %>%
      dplyr::select(-"covariateId") %>%
      dplyr::rename(covariateId = .data$newCovariateId) %>%
      dplyr::collect()

    Andromeda::appendToTable(covariates, newCovariates)
  }

  start <- Sys.time()

  cyclopsData <- Cyclops::convertToCyclopsData(
    outcomes = trainData$covariateData$labels,
    covariates = covariates,
    addIntercept = settings$addIntercept,
    modelType = modelTypeToCyclopsModelType(settings$modelType),
    checkRowIds = FALSE,
    normalize = NULL,
    quiet = TRUE
  )

  if (!is.null(param$priorCoefs)) {
    fixedCoefficients <- c(
      FALSE,
      rep(TRUE, nrow(sourceCoefs)),
      rep(FALSE, length(cyclopsData$coefficientNames) - (nrow(sourceCoefs) + 1))
    )

    startingCoefficients <- rep(0, length(fixedCoefficients))

    # skip intercept index
    startingCoefficients[2:(nrow(sourceCoefs) + 1)] <- sourceCoefs$betas
  } else {
    startingCoefficients <- NULL
    fixedCoefficients <- NULL
  }

  if (settings$crossValidationInPrior) {
    param$priorParams$useCrossValidation <- max(trainData$folds$index) > 1
  }
  prior <- do.call(eval(parse(text = settings$priorfunction)), param$priorParams)

  if (settings$useControl) {
    control <- Cyclops::createControl(
      cvType = "auto",
      fold = max(trainData$folds$index),
      startingVariance = param$priorParams$variance,
      lowerLimit = param$lowerLimit,
      upperLimit = param$upperLimit,
      tolerance = settings$tolerance,
      cvRepetitions = 1, # make an option?
      selectorType = settings$selectorType,
      noiseLevel = "silent",
      threads = settings$threads,
      maxIterations = settings$maxIterations,
      seed = settings$seed
    )

    fit <- tryCatch(
      {
        ParallelLogger::logInfo("Running Cyclops")
        Cyclops::fitCyclopsModel(
          cyclopsData = cyclopsData,
          prior = prior,
          control = control,
          fixedCoefficients = fixedCoefficients,
          startingCoefficients = startingCoefficients
        )
      },
      finally = ParallelLogger::logInfo("Done.")
    )
  } else {
    fit <- tryCatch(
      {
        ParallelLogger::logInfo("Running Cyclops with fixed varience")
        Cyclops::fitCyclopsModel(cyclopsData, prior = prior)
      },
      finally = ParallelLogger::logInfo("Done.")
    )
  }


  modelTrained <- createCyclopsModel(
    fit = fit,
    modelType = settings$modelType,
    useCrossValidation = max(trainData$folds$index) > 1,
    cyclopsData = cyclopsData,
    labels = trainData$covariateData$labels,
    folds = trainData$folds,
    priorType = param$priorParams$priorType
  )

  if (!is.null(param$priorCoefs)) {
    modelTrained$coefficients <- reparamTransferCoefs(modelTrained$coefficients)
  }

  # TODO get optimal lambda value
  ParallelLogger::logTrace("Returned from fitting to LassoLogisticRegression")
  comp <- Sys.time() - start

  ParallelLogger::logTrace("Getting variable importance")
  variableImportance <- getVariableImportance(modelTrained, trainData)

  # get prediction on test set:
  ParallelLogger::logTrace("Getting predictions on train set")
  tempModel <- list(model = modelTrained)
  attr(tempModel, "modelType") <- attr(param, "modelType")
  prediction <- predictCyclops(
    plpModel = tempModel,
    cohort = trainData$labels,
    data = trainData
  )
  prediction$evaluationType <- "Train"

  # get cv AUC if exists
  cvPerFold <- data.frame()
  if (!is.null(modelTrained$cv)) {
    cvPrediction <- do.call(rbind, lapply(modelTrained$cv, function(x) {
      x$predCV
    }))
    cvPrediction$evaluationType <- "CV"
    # fit date issue convertion caused by andromeda
    cvPrediction$cohortStartDate <- as.Date(cvPrediction$cohortStartDate, origin = "1970-01-01")

    prediction <- rbind(prediction, cvPrediction[, colnames(prediction)])

    cvPerFold <- unlist(lapply(modelTrained$cv, function(x) {
      x$out_sample_auc
    }))
    if (length(cvPerFold) > 0) {
      cvPerFold <- data.frame(
        metric = "AUC",
        fold = 1:length(cvPerFold),
        value = cvPerFold,
        startingVariance = ifelse(is.null(param$priorParams$variance), "NULL", param$priorParams$variance),
        lowerLimit = ifelse(is.null(param$lowerLimit), "NULL", param$lowerLimit),
        upperLimit = ifelse(is.null(param$upperLimit), "NULL", param$upperLimit),
        tolerance = ifelse(is.null(settings$tolerance), "NULL", settings$tolerance)
      )
    } else {
      cvPerFold <- data.frame()
    }

    # remove the cv from the model:
    modelTrained$cv <- NULL
  }

  result <- list(
    model = modelTrained,
    preprocessing = list(
      featureEngineering = attr(trainData$covariateData, "metaData")$featureEngineering,
      tidyCovariates = attr(trainData$covariateData, "metaData")$tidyCovariateDataSettings, 
      requireDenseMatrix = FALSE
    ),
    prediction = prediction,
    modelDesign = PatientLevelPrediction::createModelDesign(
      targetId = attr(trainData, "metaData")$targetId, # added
      outcomeId = attr(trainData, "metaData")$outcomeId, # added
      restrictPlpDataSettings = attr(trainData, "metaData")$restrictPlpDataSettings, # made this restrictPlpDataSettings
      covariateSettings = attr(trainData, "metaData")$covariateSettings,
      populationSettings = attr(trainData, "metaData")$populationSettings,
      featureEngineeringSettings = attr(trainData, "metaData")$featureEngineeringSettings,
      preprocessSettings = attr(trainData$covariateData, "metaData")$preprocessSettings,
      modelSettings = modelSettings, # modified
      splitSettings = attr(trainData, "metaData")$splitSettings,
      sampleSettings = attr(trainData, "metaData")$sampleSettings
    ),
    trainDetails = list(
      analysisId = analysisId,
      analysisSource = "", # TODO add from model
      developmentDatabase = attr(trainData, "metaData")$cdmDatabaseName,
      developmentDatabaseSchema = attr(trainData, "metaData")$cdmDatabaseSchema,
      attrition = attr(trainData, "metaData")$attrition,
      trainingTime = paste(as.character(abs(comp)), attr(comp, "units")),
      trainingDate = Sys.Date(),
      modelName = settings$modelType,
      finalModelParameters = list(
        variance = modelTrained$priorVariance,
        log_likelihood = modelTrained$log_likelihood
      ),
      hyperParamSearch = cvPerFold
    ),
    covariateImportance = variableImportance
  )


  class(result) <- "plpModel"
  attr(result, "predictionFunction") <- "predictCyclops"
  attr(result, "modelType") <- attr(param, "modelType")
  attr(result, "saveType") <- attr(param, "saveType")
  return(result)
}



#' Create predictive probabilities
#'
#' @details
#' Generates predictions for the population specified in plpData given the model.
#'
#' @return
#' The value column in the result data.frame is: logistic: probabilities of the outcome, poisson:
#' Poisson rate (per day) of the outome, survival: hazard rate (per day) of the outcome.
#'
#' @param plpModel   An object of type \code{predictiveModel} as generated using
#'                          \code{\link{fitPlp}}.
#' @param data         The new plpData containing the covariateData for the new population
#' @param cohort       The cohort to calculate the prediction for
#' @examples
#' \donttest{ \dontshow{ # takes too long }
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n = 1000)
#' population <- createStudyPopulation(plpData, outcomeId = 3)
#' data <- splitData(plpData, population)
#' plpModel <- fitPlp(data$Train, modelSettings = setLassoLogisticRegression(),
#'                    analysisId = "test", analysisPath = NULL)
#' prediction <- predictCyclops(plpModel, data$Test, data$Test$labels)
#' # view prediction dataframe
#' head(prediction)
#' }
#' @export
predictCyclops <- function(plpModel, data, cohort) {
  start <- Sys.time()

  ParallelLogger::logTrace("predictProbabilities - predictAndromeda start")

  prediction <- predictCyclopsType(
    plpModel$model$coefficients,
    cohort,
    data$covariateData,
    plpModel$model$modelType
  )

  # survival cyclops use baseline hazard to convert to risk from exp(LP) to 1-S^exp(LP)
  if (attr(plpModel, "modelType") == "survival") {
    if (!is.null(plpModel$model$baselineSurvival)) {
      if (is.null(attr(cohort, "timepoint"))) {
        timepoint <- attr(cohort, "metaData")$populationSettings$riskWindowEnd
      } else {
        timepoint <- attr(cohort, "timepoint")
      }
      bhind <- which.min(abs(plpModel$model$baselineSurvival$time - timepoint))
      # 1- baseline survival(time)^ (exp(betas*values))
      prediction$value <- 1 - plpModel$model$baselineSurvival$surv[bhind]^prediction$value


      metaData <- list()
      metaData$baselineSurvivalTimepoint <- plpModel$model$baselineSurvival$time[bhind]
      metaData$baselineSurvival <- plpModel$model$baselineSurvival$surv[bhind]
      metaData$offset <- 0

      attr(prediction, "metaData") <- metaData
    }
  }

  delta <- Sys.time() - start
  ParallelLogger::logInfo("Prediction took ", signif(delta, 3), " ", attr(delta, "units"))
  return(prediction)
}

predictCyclopsType <- function(coefficients, population, covariateData, modelType = "logistic") {
  if (!(modelType %in% c("logistic", "poisson", "survival", "cox", "linear"))) {
    stop(paste("Unknown modelType:", modelType))
  }
  if (!FeatureExtraction::isCovariateData(covariateData)) {
    stop("Needs correct covariateData")
  }

  intercept <- coefficients$betas[coefficients$covariateIds %in% "(Intercept)"]
  if (length(intercept) == 0) intercept <- 0
  betas <- coefficients$betas[!coefficients$covariateIds %in% "(Intercept)"]
  coefficients <- data.frame(
    beta = betas,
    covariateId = coefficients$covariateIds[coefficients$covariateIds != "(Intercept)"]
  )
  coefficients <- coefficients[coefficients$beta != 0, ]
  if (sum(coefficients$beta != 0) > 0) {
    covariateData$coefficients <- coefficients
    on.exit(covariateData$coefficients <- NULL, add = TRUE)

    prediction <- covariateData$covariates %>%
      dplyr::inner_join(covariateData$coefficients, by = "covariateId") %>%
      dplyr::mutate(values = .data$covariateValue * .data$beta) %>%
      dplyr::group_by(.data$rowId) %>%
      dplyr::summarise(value = sum(.data$values, na.rm = TRUE)) %>%
      dplyr::select("rowId", "value")

    prediction <- as.data.frame(prediction)
    prediction <- merge(population, prediction, by = "rowId", all.x = TRUE, fill = 0)
    prediction$value[is.na(prediction$value)] <- 0
    prediction$value <- prediction$value + intercept
  } else {
    warning("Model had no non-zero coefficients so predicted same for all population...")
    prediction <- population
    prediction$value <- rep(0, nrow(population)) + intercept
  }
  if (modelType == "logistic") {
    link <- function(x) {
      return(1 / (1 + exp(0 - x)))
    }
    prediction$value <- link(prediction$value)
    attr(prediction, "metaData")$modelType <- "binary"
  } else if (modelType == "poisson" || modelType == "survival" || modelType == "cox") {
    # add baseline hazard stuff

    prediction$value <- exp(prediction$value)
    attr(prediction, "metaData")$modelType <- "survival"
    if (modelType == "survival") { # is this needed?
      attr(prediction, "metaData")$timepoint <- max(population$survivalTime, na.rm = TRUE)
    }
  }
  return(prediction)
}


createCyclopsModel <- function(fit, modelType, useCrossValidation, cyclopsData, labels, folds,
                               priorType) {
  if (is.character(fit)) {
    coefficients <- c(0)
    names(coefficients) <- ""
    status <- fit
  } else if (fit$return_flag == "ILLCONDITIONED") {
    coefficients <- c(0)
    names(coefficients) <- ""
    status <- "ILL CONDITIONED, CANNOT FIT"
    ParallelLogger::logWarn(paste("GLM fitting issue: ", status))
  } else if (fit$return_flag == "MAX_ITERATIONS") {
    coefficients <- c(0)
    names(coefficients) <- ""
    status <- "REACHED MAXIMUM NUMBER OF ITERATIONS, CANNOT FIT"
    ParallelLogger::logWarn(paste("GLM fitting issue: ", status))
  } else {
    status <- "OK"
    coefficients <- stats::coef(fit) # not sure this is stats??
    ParallelLogger::logInfo(paste("GLM fit status: ", status))
  }

  # use a dataframe for the coefficients
  betas <- as.numeric(coefficients)
  betaNames <- names(coefficients)
  coefficients <- data.frame(betas = betas, covariateIds = betaNames)

  outcomeModel <- list(
    priorVariance = fit$variance,
    log_likelihood = fit$log_likelihood,
    modelType = modelType,
    modelStatus = status,
    coefficients = coefficients
  )

  if (modelType == "cox" || modelType == "survival") {
    baselineSurvival <- tryCatch(
      {
        survival::survfit(fit, type = "aalen")
      },
      error = function(e) {
        ParallelLogger::logInfo(e)
        return(NULL)
      }
    )
    if (is.null(baselineSurvival)) {
      ParallelLogger::logInfo("No baseline hazard function returned")
    }
    outcomeModel$baselineSurvival <- baselineSurvival
  }
  class(outcomeModel) <- "plpModel"

  # get CV - added && status == "OK" to only run if the model fit sucsessfully
  if (modelType == "logistic" && useCrossValidation && status == "OK") {
    outcomeModel$cv <- getCV(cyclopsData, labels,
      cvVariance = fit$variance, folds = folds,
      priorType = priorType
    )
  }

  return(outcomeModel)
}

modelTypeToCyclopsModelType <- function(modelType, stratified = FALSE) {
  if (modelType == "logistic") {
    if (stratified) {
      return("clr")
    } else {
      return("lr")
    }
  } else if (modelType == "poisson") {
    if (stratified) {
      return("cpr")
    } else {
      return("pr")
    }
  } else if (modelType == "cox") {
    return("cox")
  } else {
    ParallelLogger::logError(paste("Unknown model type:", modelType))
    stop()
  }
}



getCV <- function(
    cyclopsData,
    labels,
    cvVariance,
    folds,
    priorType) {
  fixed_prior <- Cyclops::createPrior(
    priorType = priorType,
    variance = cvVariance,
    useCrossValidation = FALSE
  )

  # add the index to the labels
  labels <- merge(labels, folds, by = "rowId")

  result <- lapply(1:max(labels$index), function(i) {
    hold_out <- labels$index == i
    weights <- rep(1.0, Cyclops::getNumberOfRows(cyclopsData))
    weights[hold_out] <- 0.0
    subset_fit <- suppressWarnings(Cyclops::fitCyclopsModel(cyclopsData,
      prior = fixed_prior,
      weights = weights
    ))
    predict <- stats::predict(subset_fit)

    auc <- aucWithoutCi(predict[hold_out], labels$y[hold_out])

    predCV <- cbind(labels[hold_out, ],
      value = predict[hold_out]
    )
    return(list(
      out_sample_auc = auc,
      predCV = predCV,
      log_likelihood = subset_fit$log_likelihood,
      log_prior = subset_fit$log_prior,
      coef = stats::coef(subset_fit)
    ))
  })

  return(result)
}

getVariableImportance <- function(modelTrained, trainData) {
  varImp <- data.frame(
    covariateId = as.double(modelTrained$coefficients$covariateIds[modelTrained$coefficients$covariateIds != "(Intercept)"]),
    value = modelTrained$coefficients$betas[modelTrained$coefficients$covariateIds != "(Intercept)"]
  )

  if (sum(abs(varImp$value) > 0) == 0) {
    ParallelLogger::logWarn("No non-zero coefficients")
    varImp <- NULL
  } else {
    ParallelLogger::logInfo("Creating variable importance data frame")

    varImp <- trainData$covariateData$covariateRef %>%
      dplyr::collect() %>%
      dplyr::left_join(varImp, by = "covariateId") %>%
      dplyr::mutate(covariateValue = ifelse(is.na(.data$value), 0, .data$value)) %>%
      dplyr::select(-"value") %>%
      dplyr::arrange(-abs(.data$covariateValue)) %>%
      dplyr::collect()
  }

  return(varImp)
}


filterCovariateIds <- function(param, covariateData) {
  if ((length(param$includeCovariateIds) != 0) && (length(param$excludeCovariateIds) != 0)) {
    covariates <- covariateData$covariates %>%
      dplyr::filter(.data$covariateId %in% param$includeCovariateIds) %>%
      dplyr::filter(!.data$covariateId %in% param$excludeCovariateIds) # does not work
  } else if ((length(param$includeCovariateIds) == 0) && (length(param$excludeCovariateIds) != 0)) {
    covariates <- covariateData$covariates %>%
      dplyr::filter(!.data$covariateId %in% param$excludeCovariateIds) # does not work
  } else if ((length(param$includeCovariateIds) != 0) && (length(param$excludeCovariateIds) == 0)) {
    includeCovariateIds <- as.double(param$includeCovariateIds) # fixes odd dplyr issue with param
    covariates <- covariateData$covariates %>%
      dplyr::filter(.data$covariateId %in% includeCovariateIds)
  } else {
    covariates <- covariateData$covariates
  }
  return(covariates)
}

reparamTransferCoefs <- function(inCoefs) {
  transferCoefs <- inCoefs %>%
    dplyr::filter(grepl("-", .data$covariateIds))

  transferCoefs$covariateIds <- substring(transferCoefs$covariateIds, 2)

  originalCoefs <- inCoefs %>%
    dplyr::filter(!grepl("-", .data$covariateIds))

  coefs <- rbind(originalCoefs, transferCoefs)
  coefs <- rowsum(coefs$betas, coefs$covariateIds)
  coefs <- data.frame(betas = coefs, covariateIds = rownames(coefs), row.names = NULL)

  return(coefs)
}
