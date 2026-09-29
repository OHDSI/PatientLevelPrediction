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
    hyperparameterSettings = NULL,
    analysisId,
    ...) {
  
  # if hyperparameterSettings not NULL warn it is ignored
  if (!is.null(hyperparameterSettings)) {
    if (hyperparameterSettings$search != "grid" && hyperparameterSettings$tuningMetric$name != "AUC") {
      warning("Non-default hyperparameterSettings are not supported for Cyclops models.")
    }
  }
  
  param <- modelSettings$param

  # check plpData is coo format:
  if (!FeatureExtraction::isCovariateData(trainData$covariateData)) {
    stop("Needs correct covariateData")
  }

  settings <- modelSettings$settings
  if (isTRUE(settings$manualPenaltyCv) && max(trainData$folds$index) < 2) {
    stop('penalty = "auto" requires at least two training folds')
  }

  trainData$covariateData$labels <- trainData$labels %>%
    dplyr::mutate(
      y = sapply(.data$outcomeCount, function(x) min(1, x)),
      time = .data$survivalTime
    )
  on.exit(trainData$covariateData$labels <- NULL, add = TRUE)

  covariates <- filterCovariateIds(param, trainData$covariateData)

  transferMap <- NULL
  if (!is.null(param$priorCoefs)) {
    # Andromeda tables share storage; append transfer covariates to a separate table.
    fitData <- Andromeda::andromeda(covariates = covariates)
    on.exit(Andromeda::close(fitData), add = TRUE)
    covariates <- fitData$covariates
    covariateIds <- covariates %>%
      dplyr::distinct(.data$covariateId) %>%
      dplyr::pull()
    transferMap <- createTransferMap(
      priorCoefs = param$priorCoefs,
      covariateIds = covariateIds,
      includeCovariateIds = param$includeCovariateIds,
      excludeCovariateIds = param$excludeCovariateIds
    )
    if (nrow(transferMap) > 0) {
      matchedIds <- match(as.character(covariateIds), transferMap$covariateIds)
      present <- !is.na(matchedIds)
      fitData$transferIds <- data.frame(
        covariateId = covariateIds[present],
        syntheticId = as.integer(transferMap$syntheticId[matchedIds[present]])
      )
      # Rename rowId before joining: DuckDB also has an implicit rowid column.
      fitData$transferCovariates <- covariates %>%
        dplyr::semi_join(fitData$transferIds, by = "covariateId") %>%
        dplyr::rename(patientRowId = "rowId")
      newCovariates <- fitData$transferCovariates %>%
        dplyr::inner_join(fitData$transferIds, by = "covariateId") %>%
        dplyr::select(-"covariateId") %>%
        dplyr::rename(covariateId = "syntheticId", rowId = "patientRowId")
      Andromeda::appendToTable(tbl = covariates, data = newCovariates)
    }
  }

  start <- Sys.time()

  cyclopsData <- Cyclops::convertToCyclopsData(
    outcomes = trainData$covariateData$labels,
    covariates = covariates,
    addIntercept = settings$addIntercept,
    modelType = modelTypeToCyclopsModelType(settings$cyclopsModelType),
    checkRowIds = FALSE,
    normalize = NULL,
    quiet = TRUE
  )

  startingCoefficients <- NULL
  fixedCoefficients <- NULL
  if (!is.null(transferMap)) {
    matches <- match(cyclopsData$coefficientNames, transferMap$syntheticId)
    fixedCoefficients <- !is.na(matches)
    startingCoefficients <- numeric(length(matches))
    startingCoefficients[fixedCoefficients] <- transferMap$betas[matches[fixedCoefficients]]
    # Only the changes to the source coefficients should be penalized.
    param$priorParams$exclude <- unique(c(
      param$priorParams$exclude,
      as.integer(cyclopsData$coefficientNames[fixedCoefficients])
    ))
  }
  if (settings$crossValidationInPrior) {
    param$priorParams$useCrossValidation <- max(trainData$folds$index) > 1
  }

  param <- resolveCyclopsPriorParams(
    param = param,
    cyclopsData = cyclopsData,
    folds = trainData$folds,
    settings = settings
  )
  hyperParamSearch <- data.frame()
  cvPrior <- NULL

  isBar <- identical(
    settings$priorfunction,
    "BrokenAdaptiveRidge::createBarPrior"
  )
  finalBarPenalty <- NULL
  finalBarInitialRidgeVariance <- NULL
  if (isBar) {
    finalBarInitialRidgeVariance <- param$priorParams$initialRidgeVariance
    if (identical(param$priorParams$penalty, "bic")) {
      finalBarPenalty <- log(Cyclops::getNumberOfRows(cyclopsData)) / 2
    } else if (is.numeric(param$priorParams$penalty)) {
      finalBarPenalty <- param$priorParams$penalty
    }
  }

  prior <- NULL
  if (!isTRUE(settings$manualPenaltyCv)) {
    if (isBar) {
      prior <- do.call(BrokenAdaptiveRidge::createBarPrior, param$priorParams)
      cvPrior <- prior
    } else {
      prior <- do.call(eval(parse(text = settings$priorfunction)), param$priorParams)
    }
  }

  if (settings$useControl) {
    startingVariance <- param$priorParams$variance
    if (is.null(startingVariance)) {
      startingVariance <- param$priorParams$initialRidgeVariance
    }
    control <- Cyclops::createControl(
      cvType = "auto",
      fold = max(trainData$folds$index),
      startingVariance = startingVariance,
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
          forceNewObject = isBar,
          fixedCoefficients = fixedCoefficients,
          startingCoefficients = startingCoefficients
        )
      },
      finally = ParallelLogger::logInfo("Done.")
    )
  } else if (isTRUE(settings$manualPenaltyCv)) {
    result <- doCyclopsCvPenalty(
      trainData = trainData,
      cyclopsData = cyclopsData,
      modelSettings = modelSettings,
      priorParams = param$priorParams,
      fixedCoefficients = fixedCoefficients,
      startingCoefficients = startingCoefficients
    )
    fit <- result$modelFit
    hyperParamSearch <- result$hyperParamSearch
    cvPrior <- result$prior
    finalBarPenalty <- result$penalty
  } else {
    fit <- tryCatch(
      {
        ParallelLogger::logInfo("Running Cyclops with fixed varience")
        Cyclops::fitCyclopsModel(
          cyclopsData = cyclopsData,
          prior = prior,
          fixedCoefficients = fixedCoefficients,
          startingCoefficients = startingCoefficients
        )
      },
      finally = ParallelLogger::logInfo("Done.")
    )
  }


  modelTrained <- createCyclopsModel(
    fit = fit,
    modelType = settings$cyclopsModelType,
    useCrossValidation = max(trainData$folds$index) > 1,
    cyclopsData = cyclopsData,
    labels = trainData$covariateData$labels,
    folds = trainData$folds,
    modelSettings = modelSettings,
    covariateData  = trainData$covariateData,
    control = createCyclopsRefitControl(modelSettings),
    cvPrior = cvPrior,
    fixedCoefficients = fixedCoefficients,
    startingCoefficients = startingCoefficients,
    transferMap = transferMap
  )

  if (!is.null(param$priorCoefs)) {
    modelTrained$coefficients <- reparamTransferCoefs(
      inCoefs = modelTrained$coefficients,
      transferMap = transferMap
    )
  }

  # TODO get optimal lambda value
  ParallelLogger::logTrace("Returned from fitting Cyclops model")
  comp <- Sys.time() - start

  ParallelLogger::logTrace("Getting variable importance")
  variableImportance <- getVariableImportance(modelTrained, trainData)

  # get prediction on test set:
  ParallelLogger::logTrace("Getting predictions on train set")
  tempModel <- list(model = modelTrained)
  attr(tempModel, "modelType") <- settings$modelType
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
      cvMean <- mean(cvPerFold, na.rm = TRUE)
      if (!is.finite(cvMean)) {
        cvMean <- NA_real_
      }
      cvPerFold <- data.frame(
        metric = "AUC",
        fold = c("CV", paste0("Fold", seq_along(cvPerFold))),
        value = c(cvMean, cvPerFold),
        startingVariance = ifelse(is.null(param$priorParams$variance), "NULL", param$priorParams$variance),
        lowerLimit = ifelse(is.null(param$lowerLimit), "NULL", param$lowerLimit),
        upperLimit = ifelse(is.null(param$upperLimit), "NULL", param$upperLimit),
        tolerance = ifelse(is.null(settings$tolerance), "NULL", settings$tolerance),
        stringsAsFactors = FALSE
      )
    } else {
      cvPerFold <- data.frame()
    }

    # remove the cv from the model:
    modelTrained$cv <- NULL
  }
  hyperParamSearch <- dplyr::bind_rows(hyperParamSearch, cvPerFold)

  finalModelParameters <- list(
    variance = modelTrained$priorVariance,
    log_likelihood = modelTrained$log_likelihood
  )
  if (isBar) {
    finalModelParameters$initialRidgeVariance <- finalBarInitialRidgeVariance
    finalModelParameters$penalty <- finalBarPenalty
  }

  result <- list(
    model = modelTrained,
    preprocessing = list(
      featureEngineering = attr(trainData$covariateData, "metaData")$featureEngineering,
      tidyCovariates = attr(trainData$covariateData, "metaData")$tidyCovariateDataSettings, 
      requiresDenseMatrix = FALSE
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
      modelSettings = modelSettings,
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
      modelName = settings$modelName,
      finalModelParameters = finalModelParameters,
      hyperParamSearch = hyperParamSearch
    ),
    covariateImportance = variableImportance
  )


  class(result) <- "plpModel"
  attr(result, "predictionFunction") <- "predictCyclops"
  attr(result, "modelType") <- settings$modelType
  attr(result, "saveType") <- settings$saveType
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
#' plpData <- simulatePlpData(simulationProfile, n = 1000, seed = 42)
#' population <- createStudyPopulation(plpData, outcomeId = 3)
#' data <- splitData(plpData, population)
#' plpModel <- fitPlp(data$Train, modelSettings = setLassoLogisticRegression(seed = 42),
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
  predictionType <- attr(plpModel, "modelType")

  if (predictionType == "survival") {
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
    prediction$rawValue <- prediction$value + intercept
  } else {
    warning("Model had no non-zero coefficients so predicted same for all population...")
    prediction <- population
    prediction$rawValue <- rep(0, nrow(population)) + intercept
  }
  if (modelType == "logistic") {
    link <- function(x) {
      return(1 / (1 + exp(0 - x)))
    }
    prediction$value <- link(prediction$rawValue)
    attr(prediction, "metaData")$modelType <- "binary"
  } else if (modelType == "poisson" || modelType == "survival" || modelType == "cox") {
    # add baseline hazard stuff

    prediction$value <- exp(prediction$rawValue)
    attr(prediction, "metaData")$modelType <- "survival"
    if (modelType == "survival") { # is this needed?
      attr(prediction, "metaData")$timepoint <- max(population$survivalTime, na.rm = TRUE)
    }
  }
  return(prediction)
}


createCyclopsModel <- function(fit, modelType, useCrossValidation, cyclopsData, labels, folds,
                               modelSettings, covariateData = NULL, control = NULL,
                               cvPrior = NULL, fixedCoefficients = NULL,
                               startingCoefficients = NULL, transferMap = NULL) {
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
    if (is.null(cvPrior)) {
      cvPrior <- createCyclopsCvPrior(
        modelSettings = modelSettings,
        fit = fit,
        cyclopsData = cyclopsData
      )
    }
    if (!is.null(transferMap)) {
      cvPrior$exclude <- unique(c(
        cvPrior$exclude,
        as.integer(cyclopsData$coefficientNames[fixedCoefficients])
      ))
    }
    outcomeModel$cv <- getCV(
      cyclopsData, 
      labels,
      cvPrior = cvPrior,
      folds = folds,
      covariateData = covariateData,
      modelType = modelType,
      control = control,
      fixedCoefficients = fixedCoefficients,
      startingCoefficients = startingCoefficients,
      transferMap = transferMap,
      forceNewObject = !is.null(transferMap) || identical(
        modelSettings$settings$priorfunction,
        "BrokenAdaptiveRidge::createBarPrior"
      )
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

createCyclopsCvPrior <- function(modelSettings, fit, cyclopsData) {
  priorFunction <- modelSettings$settings$priorfunction
  priorParams <- modelSettings$param$priorParams

  if (identical(priorFunction, "Cyclops::createPrior")) {
    priorParams$variance <- fit$variance
    priorParams$useCrossValidation <- FALSE
    return(do.call(eval(parse(text = priorFunction)), priorParams))
  }

  cvVariance <- getFittedPriorVariance(fit)
  if (!is.null(cvVariance)) {
    if (length(cvVariance) != Cyclops::getNumberOfCovariates(cyclopsData)) {
      stop(
        "Fitted prior variance length does not match the number of Cyclops covariates"
      )
    }
    priorType <- createNormalPriorType(
      cyclopsData = cyclopsData,
      exclude = priorParams$exclude,
      forceIntercept = isTRUE(priorParams$forceIntercept)
    )
    return(Cyclops::createPrior(
      priorType = priorType$types,
      variance = cvVariance,
      forceIntercept = isTRUE(priorParams$forceIntercept)
    ))
  }
  if (identical(priorFunction, "BrokenAdaptiveRidge::createBarPrior")) {
    return(do.call(BrokenAdaptiveRidge::createBarPrior, priorParams))
  }

  stop(
    "Cyclops fit did not return fitted final prior variances for CV refitting"
  )
}

getFittedPriorVariance <- function(fit) {
  fittedVarianceNames <- names(fit)[
    grepl("FinalPriorVariance$", names(fit)) |
      grepl("FinalPriorVariances$", names(fit))
  ]
  if (length(fittedVarianceNames) == 0) {
    return(NULL)
  }
  fit[[fittedVarianceNames[1]]]
}

createNormalPriorType <- function(cyclopsData, exclude = c(), forceIntercept = FALSE) {
  exclude <- checkCyclopsCovariates(cyclopsData, exclude)
  covariateIds <- Cyclops::getCovariateIds(cyclopsData)
  if (0 %in% covariateIds && !forceIntercept) {
    interceptId <- 0
    if (inherits(exclude, "integer64")) {
      interceptId <- covariateIds[covariateIds == 0][1]
    }
    if (is.null(exclude)) {
      exclude <- interceptId
    } else if (!interceptId %in% exclude) {
      exclude <- c(interceptId, exclude)
    }
  }

  types <- rep("normal", Cyclops::getNumberOfCovariates(cyclopsData))
  if (!is.null(exclude)) {
    types[covariateIds %in% exclude] <- "none"
  }
  list(types = types, excludeCovariateIds = exclude)
}

checkCyclopsCovariates <- function(cyclopsData, covariates) {
  if (is.null(covariates) || length(covariates) == 0) {
    return(NULL)
  }
  saved <- covariates
  if (inherits(covariates, "character")) {
    indices <- match(covariates, cyclopsData$coefficientNames)
    covariates <- Cyclops::getCovariateIds(cyclopsData)[indices]
  }
  if (any(is.na(covariates))) {
    stop("Unable to match all covariates: ", paste(saved, collapse = ", "))
  }
  covariates
}

createCyclopsRefitControl <- function(modelSettings) {
  settings <- modelSettings$settings
  priorParams <- modelSettings$param$priorParams
  values <- list(
    tolerance = settings$tolerance %||% priorParams$tolerance,
    threads = settings$threads,
    maxIterations = settings$maxIterations %||% priorParams$maxIterations,
    seed = settings$seed
  )
  values <- values[!vapply(values, is.null, logical(1))]
  do.call(Cyclops::createControl, c(list(noiseLevel = "silent"), values))
}

resolveCyclopsPriorParams <- function(
    param,
    cyclopsData,
    folds,
    settings) {
  if (!is.null(param$priorParams$initialRidgeVariance) &&
      identical(param$priorParams$initialRidgeVariance, "auto")) {
    normalPrior <- Cyclops::createPrior(
      priorType = "normal",
      useCrossValidation = max(folds$index) > 1
    )
    normalControl <- Cyclops::createControl(
      cvType = "auto",
      fold = max(folds$index),
      lowerLimit = param$lowerLimit,
      upperLimit = param$upperLimit,
      tolerance = settings$tolerance,
      cvRepetitions = 1,
      selectorType = settings$selectorType,
      noiseLevel = "silent",
      threads = settings$threads,
      maxIterations = settings$maxIterations,
      seed = settings$seed
    )

    ridgeFit <- tryCatch(
      {
        ParallelLogger::logInfo("Determining initialRidgeVariance")
        Cyclops::fitCyclopsModel(
          cyclopsData = cyclopsData,
          prior = normalPrior,
          control = normalControl
        )
      },
      finally = ParallelLogger::logInfo("Done.")
    )
    param$priorParams$initialRidgeVariance <- ridgeFit$variance
  }
  param
}

doCyclopsCvPenalty <- function(
    trainData,
    cyclopsData,
    modelSettings,
    priorParams,
    fixedCoefficients = NULL,
    startingCoefficients = NULL) {
  penalties <- createBarPenaltyGrid(
    labels = trainData$labels,
    penaltyRatio = modelSettings$settings$penaltyRatio,
    penaltyGridSize = modelSettings$settings$penaltyGridSize
  )
  control <- createCyclopsRefitControl(modelSettings)

  ParallelLogger::logInfo("Performing hyperparameter tuning to determine best BAR penalty")
  labels <- merge(trainData$covariateData$labels, trainData$folds, by = "rowId")
  cvByFold <- lapply(seq_len(max(labels$index)), function(i) {
    holdOut <- labels$index == i
    weights <- rep(1.0, Cyclops::getNumberOfRows(cyclopsData))
    weights[holdOut] <- 0.0

    foldSearch <- vector("list", length(penalties))
    for (penaltyIndex in seq_along(penalties)) {
      penalty <- penalties[penaltyIndex]
      candidatePriorParams <- priorParams
      candidatePriorParams$penalty <- penalty
      cvPrior <- do.call(
        BrokenAdaptiveRidge::createBarPrior,
        candidatePriorParams
      )

      subsetFit <- suppressWarnings(Cyclops::fitCyclopsModel(
        cyclopsData,
        prior = cvPrior,
        control = control,
        weights = weights,
        # BAR fixes eliminated coefficients at zero; do not carry that state to another fit.
        forceNewObject = TRUE,
        fixedCoefficients = fixedCoefficients,
        startingCoefficients = startingCoefficients
      ))
      coefficients <- stats::coef(subsetFit)

      coefDf <- data.frame(
        betas = as.numeric(coefficients),
        covariateIds = names(coefficients),
        stringsAsFactors = FALSE
      )
      predAll <- predictCyclopsType(
        coefficients = coefDf,
        population = labels,
        covariateData = trainData$covariateData,
        modelType = modelSettings$settings$cyclopsModelType
      )
      auc <- aucWithoutCi(predAll$rawValue[holdOut], labels$y[holdOut])
      foldSearch[[penaltyIndex]] <- data.frame(
        metric = "AUC",
        fold = paste0("Fold", i),
        value = auc,
        penalty = penalty,
        stringsAsFactors = FALSE
      )
    }
    foldSearch
  })
  hyperParamSearch <- dplyr::bind_rows(unlist(cvByFold, recursive = FALSE))
  cvMeans <- hyperParamSearch %>%
    dplyr::group_by(.data$penalty) %>%
    dplyr::summarise(value = mean(.data$value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(
      metric = "AUC",
      fold = "CV"
    ) %>%
    dplyr::select("metric", "fold", "value", "penalty")
  hyperParamSearch <- dplyr::bind_rows(
    cvMeans,
    hyperParamSearch
  ) %>%
    dplyr::arrange(
      dplyr::desc(.data$penalty),
      match(.data$fold, c("CV", paste0("Fold", seq_len(max(labels$index)))))
    )
  bestRow <- hyperParamSearch %>%
    dplyr::filter(.data$fold == "CV") %>%
    dplyr::arrange(dplyr::desc(.data$value), dplyr::desc(.data$penalty)) %>%
    dplyr::slice(1)
  bestPenalty <- bestRow$penalty
  ParallelLogger::logInfo(paste0("Best BAR penalty: ", signif(bestPenalty, 4)))

  priorParams$penalty <- bestPenalty
  prior <- do.call(
    BrokenAdaptiveRidge::createBarPrior,
    priorParams
  )

  modelFit <- tryCatch(
    {
      ParallelLogger::logInfo("Refitting BAR model with best penalty")
      Cyclops::fitCyclopsModel(
        cyclopsData = cyclopsData,
        prior = prior,
        control = control,
        weights = rep(1.0, Cyclops::getNumberOfRows(cyclopsData)),
        forceNewObject = TRUE,
        fixedCoefficients = fixedCoefficients,
        startingCoefficients = startingCoefficients
      )
    },
    finally = ParallelLogger::logInfo("Done.")
  )

  list(
    modelFit = modelFit,
    prior = prior,
    penalty = bestPenalty,
    hyperParamSearch = hyperParamSearch
  )
}

createBarPenaltyGrid <- function(labels, penaltyRatio, penaltyGridSize) {
  startingPenalty <- log(nrow(labels)) / 2
  seq(
    from = startingPenalty,
    to = penaltyRatio * startingPenalty,
    length.out = penaltyGridSize
  )
}



getCV <- function(
    cyclopsData,
    labels,
    cvPrior,
    folds,
    covariateData = NULL,
    modelType = "logistic",
    control = NULL,
    forceNewObject = FALSE,
    fixedCoefficients = NULL,
    startingCoefficients = NULL,
    transferMap = NULL
) {
  # merge sorts by rowId, matching Cyclops' ordering of logistic outcomes.
  labels <- merge(labels, folds, by = "rowId")

  result <- lapply(1:max(labels$index), function(i) {
    hold_out <- labels$index == i
    weights <- rep(1.0, Cyclops::getNumberOfRows(cyclopsData))
    weights[hold_out] <- 0.0
    subset_fit <- suppressWarnings(Cyclops::fitCyclopsModel(cyclopsData,
      prior = cvPrior,
      weights = weights,
      control = control,
      forceNewObject = forceNewObject,
      fixedCoefficients = fixedCoefficients,
      startingCoefficients = startingCoefficients
    ))
    coefficients <- stats::coef(subset_fit)
    coefDf <- data.frame(
      betas = as.numeric(coefficients),
      covariateIds = names(coefficients),
      stringsAsFactors = FALSE
    )
    if (!is.null(transferMap)) {
      coefDf <- reparamTransferCoefs(inCoefs = coefDf, transferMap = transferMap)
    }
    if (!is.null(covariateData)) {
      predAll <- predictCyclopsType(
        coefficients = coefDf,
        population = labels,
        covariateData = covariateData,
        modelType = modelType
      )
      rowOrder <- match(labels$rowId, predAll$rowId)
      probsAll <- predAll$value[rowOrder]
      rawValueAll <- predAll$rawValue[rowOrder]
    } else {
      probsAll <- stats::predict(subset_fit)
      probsAllClipped <- pmin(pmax(probsAll, 1e-15), 1 - 1e-15)
      rawValueAll <- stats::qlogis(probsAllClipped)
    }

    auc <- aucWithoutCi(rawValueAll[hold_out], labels$y[hold_out])

    predCV <- cbind(
      labels[hold_out, ],
      value = probsAll[hold_out],
      rawValue = rawValueAll[hold_out]
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

# Keep covariate IDs as characters to avoid converting them to 32-bit integers.
createTransferMap <- function(priorCoefs, covariateIds,
                              includeCovariateIds = NULL, excludeCovariateIds = NULL) {
  if (!is.data.frame(priorCoefs) ||
      !all(c("betas", "covariateIds") %in% names(priorCoefs))) {
    stop("priorCoefs must contain betas and covariateIds")
  }
  priorCoefs$covariateIds <- as.character(priorCoefs$covariateIds)
  if (!is.numeric(priorCoefs$betas) || any(!is.finite(priorCoefs$betas)) ||
      anyNA(priorCoefs$covariateIds) || anyDuplicated(priorCoefs$covariateIds)) {
    stop("Source coefficients must have unique IDs and finite betas")
  }
  priorCoefs <- priorCoefs[priorCoefs$covariateIds != "(Intercept)", , drop = FALSE]
  # Apply covariate selection even when a covariate is absent from training.
  if (length(includeCovariateIds) > 0) {
    priorCoefs <- priorCoefs[
      priorCoefs$covariateIds %in% as.character(includeCovariateIds), , drop = FALSE
    ]
  }
  if (length(excludeCovariateIds) > 0) {
    priorCoefs <- priorCoefs[
      !priorCoefs$covariateIds %in% as.character(excludeCovariateIds), , drop = FALSE
    ]
  }
  ids <- suppressWarnings(as.numeric(c(as.character(covariateIds), priorCoefs$covariateIds)))
  if (any(!is.finite(ids) | ids <= 0 | ids != floor(ids))) {
    stop("Transfer requires positive integer covariate IDs; negative IDs are reserved")
  }
  priorCoefs <- priorCoefs[priorCoefs$betas != 0, c("covariateIds", "betas"), drop = FALSE]
  priorCoefs$syntheticId <- as.character(-seq_len(nrow(priorCoefs)))
  return(priorCoefs)
}

reparamTransferCoefs <- function(inCoefs, transferMap) {
  coefs <- inCoefs[!inCoefs$covariateIds %in% transferMap$syntheticId, ]
  coefs <- rbind(coefs, transferMap[, c("betas", "covariateIds"), drop = FALSE])
  coefs <- rowsum(coefs$betas, group = coefs$covariateIds)
  coefs <- data.frame(betas = coefs[, 1], covariateIds = rownames(coefs), row.names = NULL)
  return(coefs)
}
