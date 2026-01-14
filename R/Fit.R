# @file Fit.R
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

#' fitPlp
#'
#' @description
#' Train various models using a default parameter grid search or user specified 
#' parameters
#'
#' @details
#' The user can define the machine learning model to train 

#' @param trainData An object of type \code{trainData} created using \code{splitData}
#' data extracted from the CDM.
#' @param modelSettings An object of class \code{modelSettings} created using 
#' one of the \code{createModelSettings} functions
#' @param hyperparameterSettings An object of class \code{hyperparameterSettings}'
#' @param analysisId                       The id of the analysis
#' @param analysisPath                     The path of the analysis
#' @return
#' An object of class \code{plpModel} containing:
#'
#' \item{model}{The trained prediction model}
#' \item{preprocessing}{The preprocessing required when applying the model}
#' \item{prediction}{The cohort data.frame with the predicted risk column added}
#' \item{modelDesign}{A list specifiying the modelDesign settings used to fit the model}
#' \item{trainDetails}{The model meta data}
#' \item{covariateImportance}{The covariate importance for the model}
#' @examples
#' \donttest{ \dontshow{ # takes too long }
#' # simulate data
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n = 1000, seed = 42)
#' # create study population, split into train/test and preprocess with default settings
#' population <- createStudyPopulation(plpData, outcomeId = 3)
#' data <- splitData(plpData, population, createDefaultSplitSetting())
#' data$Train$covariateData <- preprocessData(data$Train$covariateData)
#' saveLoc <- file.path(tempdir(), "fitPlp")
#' # fit a lasso logistic regression model using the training data
#' plpModel <- fitPlp(data$Train, modelSettings=setLassoLogisticRegression(seed=42),
#'                    analysisId=1, analysisPath=saveLoc)
#' # show evaluationSummary for model
#' evaluatePlp(plpModel$prediction)$evaluationSummary
#' # clean up
#' unlink(saveLoc, recursive = TRUE)
#' }
#' @export
fitPlp <- function(
    trainData,
    modelSettings,
    hyperparameterSettings = createHyperparameterSettings(),
    analysisId,
    analysisPath) {
  start <- Sys.time()
  if (is.null(trainData)) {
    stop("trainData is NULL")
  }
  if (is.null(trainData$covariateData)) {
    stop("covariateData is NULL")
  }
  checkIsClass(trainData$covariateData, "CovariateData")
  checkIsClass(modelSettings, "modelSettings")
  modelSettings <- normalizeModelSettings(modelSettings)

  # =========================================================
  # run through pipeline list and apply:
  # =========================================================

  # Now apply the classifier:
  if (!is.null(modelSettings$fitFunction)) {
    fun <- modelSettings$fitFunction
  } else {
    fun <- "fitClassifier"
  }
  fun <- eval(parse(text = fun))
  args <- list(
    trainData = trainData,
    modelSettings = modelSettings, # old: param = modelSettings$param, # make this model settings?
    hyperparameterSettings = hyperparameterSettings,
    analysisId = analysisId,
    analysisPath = analysisPath
  )
  plpModel <- do.call(fun, args)
  ParallelLogger::logTrace("Returned from classifier function")

  # adding trainDetails databaseId to all classifiers
  # TODO - move other details into fit
  plpModel$trainDetails$developmentDatabaseId <- attr(trainData, "metaData")$cdmDatabaseId
  class(plpModel) <- "plpModel"
  delta <- Sys.time() - start
  ParallelLogger::logInfo("Time to fit model: ", 
    signif(delta, 3), " ", attr(delta, "units"))

  return(plpModel)
}

normalizeModelSettings <- function(modelSettings) {
  param <- modelSettings$param

  # pull settings from old-style attributes when missing
  settings <- modelSettings$settings
  if (is.null(settings) && !is.null(param)) {
    settings <- attr(param, "settings")
  }
  if (is.null(settings$saveType) && !is.null(attr(param, "saveType"))) {
    settings$saveType <- attr(param, "saveType")
  }
  if (is.null(settings)) {
    settings <- list()
  }

  # map legacy field names to the new ones
  if (is.null(settings$modelName) && !is.null(settings$name)) {
    settings$modelName <- settings$name
  }
  if (is.null(settings$requiresDenseMatrix) && !is.null(settings$requireDenseMatrix)) {
    settings$requiresDenseMatrix <- settings$requireDenseMatrix
  }
  if (is.null(settings$train) && !is.null(settings$trainRFunction)) {
    settings$train <- settings$trainRFunction
  }
  if (is.null(settings$predict) && !is.null(settings$predictRFunction)) {
    settings$predict <- settings$predictRFunction
  }
  if (is.null(settings$variableImportance) && !is.null(settings$varImpRFunction)) {
    settings$variableImportance <- settings$varImpRFunction
  }

  # defaults for python-based sklearn models
  if (!is.null(settings$pythonModule)) {
    if (is.null(settings$modelType)) settings$modelType <- "binary"
    if (is.null(settings$train)) settings$train <- "fitSklearn"
    if (is.null(settings$predict)) settings$predict <- "predictSklearn"
    if (is.null(settings$variableImportance)) settings$variableImportance <- "varImpSklearn"
    if (is.null(settings$prepareData)) settings$prepareData <- "toSparseM"
    if (is.null(settings$saveType)) settings$saveType <- "saveLoadSklearn"
    if (is.null(settings$requiresDenseMatrix)) settings$requiresDenseMatrix <- FALSE
  }

  # defaults for R-based classifiers that relied on RClassifier.R
  if (is.null(settings$prepareData) &&
      (!is.null(settings$train) || !is.null(settings$variableImportance))) {
    settings$prepareData <- "toSparseM"
  }
  if (is.null(settings$modelType)) {
    settings$modelType <- "binary"
  }
  if (is.null(settings$requiresDenseMatrix)) {
    settings$requiresDenseMatrix <- FALSE
  }

  modelSettings$settings <- settings

  # Backwards compatibility:
  # Historically fitPlp used modelSettings$fitFunction values like "fitRclassifier"
  # or "fitSklearn". Those are no longer valid entry points. For legacy objects,
  # the intended behavior is to route through fitClassifier using the mapped
  # settings$train/predict/variableImportance interface.
  if (!is.null(modelSettings$fitFunction) && is.character(modelSettings$fitFunction)) {
    legacyFitFunctions <- c("fitRclassifier", "fitSklearn")
    if (modelSettings$fitFunction %in% legacyFitFunctions) {
      modelSettings$fitFunction <- NULL
    }
  }

  if (is.null(modelSettings$modelName) && !is.null(settings$modelName)) {
    modelSettings$modelName <- settings$modelName
  }

  modelSettings
}

fitClassifier <- function(
  trainData,
  modelSettings,
  hyperparameterSettings = createHyperparameterSettings(),
  analysisId,
  ...
) {
  modelSettings <- normalizeModelSettings(modelSettings)
  if (!FeatureExtraction::isCovariateData(trainData$covariateData)) {
    stop("Needs correct covariateData")
  }

  # add folds to labels if present:
  if (!is.null(trainData$folds)) {
    trainData$labels <- merge(trainData$labels, trainData$folds, by = "rowId")
  }

  # get the param
  param <- modelSettings$param

  # get the settings
  settings <- modelSettings$settings
  # backwards compatible
  if (is.null(settings)) {
    settings <- attr(param, "settings")
  }

  # TODO add a check for the settings?
  # like the old checkPySettings()

  ParallelLogger::logInfo(paste0("Training ", settings$modelName))
  start <- Sys.time()
  set.seed(settings$seed)

  # make sure the inputs are valid
  #TODO: make this generic
  #checkPySettings(settings)

  # prepare data for training (if needed) 
  if (!is.null(settings$prepareData)) {
    preparedData <- do.call(
      settings$prepareData,
      args = list(
        trainData,
        map = NULL
      )
    )
    covariates <- preparedData$dataMatrix
  } else {
    preparedData <- trainData
    covariates <- preparedData$covariateData
  }

  labels <- preparedData$labels
  covariateRef <- preparedData$covariateRef

  # Hyperparameter optimization
  # input: matrixData, labels, settings, hyperparamSettings
  # output: model, prediction, finalParam, hyperSummary
  tuneResult <- tuneHyperparameters(
    covariates,
    labels,
    param = param,
    settings = settings,
    hyperparamSettings = hyperparameterSettings
  )

  # add code for var imp here - model and settings

  variableImportance <- tryCatch(
    {
      do.call(
        settings$variableImportance,
        list(model = tuneResult$model, covariateMap = covariateRef)
      )
    },
    error = function(e) {
      ParallelLogger::logInfo("Error calculating variableImportance")
      ParallelLogger::logInfo(e)
      return(NULL)
    }
  )

  if (!is.null(variableImportance)) {
    covariateRef <- merge(
      covariateRef,
      variableImportance,
      all.x = TRUE,
      by = "covariateId"
    )
    covariateRef$covariateValue[is.na(covariateRef$covariateValue)] <- 0
    covariateRef$included[is.na(covariateRef$included)] <- 0
  } else {
    covariateRef$covariateValue <- 0
    covariateRef$included <- 1
  }

  comp <- start - Sys.time()

  result <- list(
    model = tuneResult$model,
    preprocessing = list(
      featureEngineering = attr(
        trainData$covariateData,
        "metaData"
      )$featureEngineering,
      tidyCovariates = attr(
        trainData$covariateData,
        "metaData"
      )$tidyCovariateDataSettings,
      requiresDenseMatrix = settings$requiresDenseMatrix
    ),
    prediction = tuneResult$prediction,
    modelDesign = PatientLevelPrediction::createModelDesign(
      targetId = attr(trainData, "metaData")$targetId,
      outcomeId = attr(trainData, "metaData")$outcomeId,
      restrictPlpDataSettings = attr(
        trainData,
        "metaData"
      )$restrictPlpDataSettings,
      hyperparameterSettings = hyperparameterSettings,
      covariateSettings = attr(trainData, "metaData")$covariateSettings,
      populationSettings = attr(trainData, "metaData")$populationSettings,
      featureEngineeringSettings = attr(
        trainData$covariateData,
        "metaData"
      )$featureEngineeringSettings,
      preprocessSettings = attr(
        trainData$covariateData,
        "metaData"
      )$preprocessSettings,
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
      finalModelParameters = tuneResult$finalParam,
      hyperParamSearch = tuneResult$paramSearch
    ),
    covariateImportance = covariateRef
  )

  class(result) <- "plpModel"
  attr(result, "modelType") <- settings$modelType
  return(result)
}


tuneHyperparameters <- function(
  data,
  labels,
  param,
  settings,
  hyperparamSettings
) {
  if (is.null(hyperparamSettings) || is.null(hyperparamSettings$tuningMetric)) {
    hyperparamSettings <- createHyperparameterSettings()
  }
  iterator <- prepareHyperparameterGrid(
    paramDefinition = param,
    hyperSettings = hyperparamSettings
  )
  history <- list()
  metric <- hyperparamSettings$tuningMetric
  bestPerformance <- if (metric$maximize) -Inf else Inf
  bestCvPrediction <- c()

  repeat {
    candidate <- iterator$getNext(history)
    if (is.null(candidate)) {
      break
    }
    cvPrediction <- c()
    cvPerformance <- c()

    for (i in unique(labels$index)) {
      ind <- labels$index != i
      model <- do.call(
        settings$train,
        list(
          dataMatrix = data[ind, , drop = FALSE],
          labels = labels[ind, ],
          hyperParameters = candidate,
          settings = settings
        )
      )
      prediction <- do.call(
        settings$predict,
        list(
          plpModel = model,
          data = data[!ind, , drop = FALSE],
          cohort = labels[!ind, ]
        )
      )
      performance <- metric$fun(prediction)
      cvPrediction <- rbind(cvPrediction, prediction)
      cvPerformance <- c(performance, cvPerformance)
    }
    meanCvPerformance <- mean(cvPerformance, na.rm = TRUE)
    history[[length(history) + 1]] <- list(
      metric = metric$name,
      param = candidate,
      cvPerformance = meanCvPerformance,
      cvPerformancePerFold = cvPerformance,
      hyperSummary = makeHyperSummary(
        metric,
        meanCvPerformance,
        cvPerformance,
        candidate
      )
    )
    bestPerformance <- if (metric$maximize) {
      max(bestPerformance, meanCvPerformance, na.rm = TRUE)
    } else {
      min(bestPerformance, meanCvPerformance, na.rm = TRUE)
    }
    if (identical(bestPerformance, meanCvPerformance)) {
      ParallelLogger::logInfo(paste0(
        "New best performance ",
        round(bestPerformance, 4),
        " with param: ",
        paste(names(candidate), candidate, sep = "=", collapse = ", ")
      ))
      bestCvPrediction <- cvPrediction
    } else {
      ParallelLogger::logInfo(paste0(
        "Performance ",
        round(meanCvPerformance, 4),
        " with param: ",
        paste(names(candidate), candidate, sep = "=", collapse = ", ")
      ))
    }
  }
  iterator$finalize(history)
  best <- selectBest(history, metric)

  finalParam <- history[[best]]$param

  cvPrediction <- bestCvPrediction
  cvPrediction$evaluationType <- "CV"

  # fit final model
  ParallelLogger::logInfo("Fit best model on whole training set")
  finalModel <- do.call(
    settings$train,
    list(
      dataMatrix = data,
      labels = labels,
      hyperParameters = finalParam,
      settings = settings
    )
  )

  prediction <- do.call(
    settings$predict,
    list(
      plpModel = finalModel,
      data = data,
      cohort = labels
    )
  )

  prediction$evaluationType <- "Train"

  prediction <- rbind(
    prediction,
    cvPrediction
  )
  attr(prediction, "metaData") <- list(
    modelType = settings$modelType %||% attr(data, "modelType")
  )

  list(
    model = finalModel,
    prediction = prediction,
    finalParam = finalParam,
    paramSearch = history
  )
}

selectBest <- function(history, metric) {
  performances <- sapply(history, function(x) x$cvPerformance)
  if (metric$maximize) {
    best <- which.max(performances)
  } else {
    best <- which.min(performances)
  }
  best
}

makeHyperSummary <- function(metric, meanScore, foldScores, param) {
  perfRows <- data.frame(
    metric = metric$name,
    fold = c("CV", paste0("Fold", seq_along(foldScores))),
    value = c(meanScore, foldScores),
    stringsAsFactors = FALSE
  )
  paramValues <- lapply(param, function(x) {
    if (is.null(x)) {
      "null"
    } else if (length(x) == 1) {
      x
    } else {
      paste(x, collapse = ",")
    }
  })
  paramDf <- as.data.frame(paramValues, stringsAsFactors = FALSE)
  paramDf <- paramDf[rep(1L, nrow(perfRows)), , drop = FALSE]

  cbind(perfRows, paramDf, row.names = NULL)
}
