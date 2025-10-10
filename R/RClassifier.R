# @file RClassifier.R
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

fitRclassifier <- function(
    trainData,
    modelSettings,
    hyperparameterSettings = createHyperparameterSettings(),
    search = "grid",
    analysisId,
    ...) {

  if (!FeatureExtraction::isCovariateData(trainData$covariateData)) {
    stop("Needs correct covariateData")
  }


  # add folds to labels if present:
  if (!is.null(trainData$folds)) {
    trainData$labels <- merge(trainData$labels, trainData$folds, by = "rowId")
  }

  settings <- attr(modelSettings$param, "settings")
  ParallelLogger::logInfo(paste0("Training ", settings$modelName))

  set.seed(settings$seed)

  # convert data into sparse Matrix:
  result <- toSparseM(
    trainData,
    map = NULL
  )

  dataMatrix <- result$dataMatrix
  labels <- result$labels
  covariateRef <- result$covariateRef

  # set test/train sets (for printing performance as it trains)
  start <- Sys.time()

  # use the new R CV wrapper
  cvResult <- tuneHyperparameters(
    dataMatrix,
    labels,
    param = modelSettings$param,
    hyperparamSettings = hyperparameterSettings
  )
  
  variableImportance <- tryCatch(
    {
      do.call(
        settings$varImpRFunction,
        list(model = cvResult$model, covariateMap = covariateRef)
      )
    },
    error = function(e) {
      ParallelLogger::logInfo("Error calculating variableImportance")
      ParallelLogger::logInfo(e)
      return(NULL)
    }
  )


  hyperSummary <- do.call(rbind, lapply(cvResult$paramGridSearch, function(x) x$hyperSummary))

  prediction <- cvResult$prediction

  covariateRef <- merge(covariateRef, variableImportance, all.x = TRUE,
    by = "covariateId")
  covariateRef$covariateValue[is.na(covariateRef$covariateValue)] <- 0
  covariateRef$included[is.na(covariateRef$included)] <- 0

  comp <- start - Sys.time()

  result <- list(
    model = cvResult$model,
    preprocessing = list(
      featureEngineering = attr(trainData$covariateData, "metaData")$featureEngineering,
      tidyCovariates = attr(trainData$covariateData, "metaData")$tidyCovariateDataSettings,
      requireDenseMatrix = FALSE
    ),
    prediction = prediction,
    modelDesign = PatientLevelPrediction::createModelDesign(
      targetId = attr(trainData, "metaData")$targetId,
      outcomeId = attr(trainData, "metaData")$outcomeId,
      restrictPlpDataSettings = attr(trainData, "metaData")$restrictPlpDataSettings,
      covariateSettings = attr(trainData, "metaData")$covariateSettings,
      populationSettings = attr(trainData, "metaData")$populationSettings,
      featureEngineeringSettings = attr(trainData$covariateData, "metaData")$featureEngineeringSettings,
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
      modelName = attr(modelSettings$param, "settings")$trainRFunction,
      finalModelParameters = cvResult$finalParam,
      hyperParamSearch = hyperSummary
    ),
    covariateImportance = covariateRef
  )

  class(result) <- "plpModel"
  attr(result, "predictionFunction") <- settings$predictRFunction
  attr(result, "modelType") <- "binary"
  attr(result, "saveType") <- attr(modelSettings$param, "saveType")

  return(result)
}



tuneHyperparameters <- function(data, 
                                labels, 
                                param, 
                                hyperparamSettings) {
  settings <- attr(param, "settings")
  iterator <- prepareHyperparameterGrid(
    paramDefinition = param,
    hyperSettings = hyperparamSettings
  )
  history <- list()
  metric <- hyperparamSettings$tuningMetric
  bestPerformance <- if (metric$maximize) -Inf else Inf
  bestCvPrediction <- c()

  repeat  {
    candidate <- iterator$getNext(history)
    if (is.null(candidate)) break
    cvPrediction <- c()
    cvPerformance <- c()

    for (i in unique(labels$index)) {
      ind <- labels$index != i
      model <- do.call(
        settings$trainRFunction,
        list(
          dataMatrix = data[ind, , drop = FALSE],
          labels = labels[ind, ],
          hyperParameters = candidate,
          settings = settings
        )
      )
      prediction <- do.call(
        settings$predictRFunction,
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
        hyperSummary = makeHyperSummary(metric, meanCvPerformance, cvPerformance, candidate)
    )
    bestPerformance <- if (metric$maximize) {
      max(bestPerformance, meanCvPerformance, na.rm = TRUE) 
    } else {
      min(bestPerformance, meanCvPerformance, na.rm = TRUE)
    }
    if (identical(bestPerformance, meanCvPerformance)) {
      ParallelLogger::logInfo(paste0("New best performance ", round(bestPerformance, 4), " with param: ", paste(names(candidate), candidate, sep = "=", collapse = ", ")))
      bestCvPrediction <- cvPrediction
    } else {
      ParallelLogger::logInfo(paste0("Performance ", round(meanCvPerformance, 4), " with param: ", paste(names(candidate), candidate, sep = "=", collapse = ", ")))
    }
  }
  iterator$finalize(history)
  best <- selectBest(history, metric)

  finalParam <- history[[best]]$param

  cvPrediction <- bestCvPrediction
  cvPrediction$evaluationType <- "CV"

  # fit final model
  finalModel <- do.call(
    settings$trainRFunction,
    list(
      dataMatrix = data,
      labels = labels,
      hyperParameters = finalParam,
      settings = settings
    )
  )

  prediction <- do.call(
    settings$predictRFunction,
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

  result <- list(
    model = finalModel,
    prediction = prediction,
    finalParam = finalParam,
    paramGridSearch = history
  )

  return(result)
}

selectBest <- function(history, metric) {
  performances <- sapply(history, function(x) x$cvPerformance)
  if (metric$maximize) {
    best <- which.max(performances)
  } else {
    best <- which.min(performances)
  }
  return(best)
}

makeHyperSummary <- function(metric, meanScore, foldScores, param) { 
  perfRows <- data.frame(
    metric = metric$name,
    fold = c("CV", paste0("Fold", seq_along(foldScores))),
    value = c(meanScore, foldScores),
    stringsAsFactors = FALSE
  )
  paramValues <- lapply(param, function(x) { 
    if (is.null(x)) "null"
    else if (length(x) == 1) x
    else paste(x, collapse = ",")
  })
  paramDf <- as.data.frame(paramValues, stringsAsFactors = FALSE)
  paramDf <- paramDf[rep(1L, nrow(perfRows)), , drop = FALSE]

  cbind(perfRows, paramDf, row.names = NULL)
}
