# @file SklearnClassifier.R
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

fitSklearn <- function(trainData,
                       modelSettings,
                       search = "grid",
                       analysisId,
                       ...) {
  param <- modelSettings$param

  # check covariate data
  if (!FeatureExtraction::isCovariateData(trainData$covariateData)) {
    stop("Needs correct covariateData")
  }

  # get the settings from the param
  pySettings <- attr(param, "settings")

  # make sure the inputs are valid
  checkPySettings(pySettings)

  start <- Sys.time()

  if (!is.null(trainData$folds)) {
    trainData$labels <-
      merge(trainData$labels, trainData$fold, by = "rowId")
  }

  # convert the data to a sparse R Matrix and then use reticulate to convert to python sparse
  # need to save the covariateMap so we know the covariateId to columnId when applying model
  mappedData <- toSparseM(trainData)

  matrixData <- mappedData$dataMatrix
  labels <- mappedData$labels
  covariateRef <- mappedData$covariateRef

  # save the model to outLoc
  outLoc <- createTempModelLoc()

  cvResult <- do.call(
    what = gridCvPython,
    args = list(
      matrixData = matrixData,
      labels = labels,
      seed = pySettings$seed,
      requiresDenseMatrix = pySettings$requiresDenseMatrix,
      modelName = pySettings$name,
      pythonModule = pySettings$pythonModule,
      pythonClass = pySettings$pythonClass,
      modelLocation = outLoc,
      paramSearch = param,
      saveToJson = attr(param, "saveToJson")
    )
  )

  hyperSummary <-
    do.call(
      rbind,
      lapply(cvResult$paramGridSearch, function(x) {
        x$hyperSummary
      })
    )

  prediction <- cvResult$prediction

  variableImportance <- cvResult$variableImportance
  variableImportance[is.na(variableImportance)] <- 0

  incs <- rep(1, nrow(covariateRef))
  covariateRef$included <- incs
  covariateRef$covariateValue <-
    unlist(variableImportance) # check this is correct order

  comp <- start - Sys.time()

  result <- list(
    model = file.path(outLoc),
    preprocessing = list(
      featureEngineering = attr(trainData$covariateData, "metaData")$featureEngineering,
      tidyCovariates = attr(trainData$covariateData, "metaData")$tidyCovariateDataSettings,
      requireDenseMatrix = attr(param, "settings")$requiresDenseMatrix
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
      analysisSource = "",
      # TODO add from model
      developmentDatabase = attr(trainData, "metaData")$cdmDatabaseName,
      developmentDatabaseSchema = attr(trainData, "metaData")$cdmDatabaseSchema,
      attrition = attr(trainData, "metaData")$attrition,
      trainingTime = paste(as.character(abs(comp)), attr(comp, "units")),
      trainingDate = Sys.Date(),
      modelName = pySettings$name,
      finalModelParameters = cvResult$finalParam,
      hyperParamSearch = hyperSummary
    ),
    covariateImportance = covariateRef
  )

  class(result) <- "plpModel"
  attr(result, "predictionFunction") <- "predictPythonSklearn"
  attr(result, "modelType") <- "binary"
  attr(result, "saveType") <-
    attr(param, "saveType") # in save/load plp
  attr(result, "saveToJson") <-
    attr(param, "saveToJson") # when saving in reticulate

  return(result)
}


predictPythonSklearn <- function(plpModel,
                                 data,
                                 cohort) {
  if (inherits(data, "plpData")) {
    # convert
    matrixObjects <- toSparseM(
      plpData = data,
      cohort = cohort,
      map = plpModel$covariateImportance %>%
        dplyr::select("columnId", "covariateId")
    )

    newData <- matrixObjects$dataMatrix
    cohort <- matrixObjects$labels
  } else {
    newData <- data
  }

  # load model
  if (attr(plpModel, "saveToJson")) {
    modelLocation <-
      reticulate::r_to_py(file.path(plpModel$model, "model.json"))
    model <- sklearnFromJson(path = modelLocation)
  } else {
    os <- reticulate::import("os")
    joblib <- reticulate::import("joblib", convert = FALSE)
    modelLocation <-
      reticulate::r_to_py(file.path(plpModel$model, "model.pkl"))
    model <- joblib$load(os$path$join(modelLocation))
  }
  pythonData <- reticulate::r_to_py(newData)

  # make dense if needed
  if (plpModel$preprocessing$requireDenseMatrix) {
    pythonData <- pythonData$toarray()
  }

  cohort <- predictValues(
    model = model,
    data = pythonData,
    cohort = cohort,
    type = attr(plpModel, "modelType")
  )

  return(cohort)
}

predictValues <- function(model, data, cohort, type = "binary") {
  predictionValue <- model$predict_proba(data)
  cohort$value <- reticulate::py_to_r(predictionValue)[, 2]

  cohort <- cohort %>%
    dplyr::select(-"rowId") %>%
    dplyr::rename(rowId = "originalRowId")

  attr(cohort, "metaData")$modelType <- type

  return(cohort)
}

checkPySettings <- function(settings) {
  checkIsClass(settings$seed, c("numeric", "integer"))
  ParallelLogger::logDebug(paste0("classifier seed: ", settings$seed))

  checkIsClass(settings$requiresDenseMatrix, c("logical"))
  ParallelLogger::logDebug(paste0("requiresDenseMatrix: ", settings$requiresDenseMatrix))

  checkIsClass(settings$name, c("character"))
  ParallelLogger::logDebug(paste0("name: ", settings$name))

  checkIsClass(settings$pythonModule, c("character"))
  ParallelLogger::logDebug(paste0("pythonModule: ", settings$pythonModule))

  checkIsClass(settings$pythonClass, c("character"))
  ParallelLogger::logDebug(paste0("pythonClass: ", settings$pythonClass))
}

gridCvPython <- function(matrixData,
                         labels,
                         seed,
                         requiresDenseMatrix,
                         modelName,
                         pythonModule,
                         pythonClass,
                         modelLocation,
                         paramSearch,
                         saveToJson) {
  ParallelLogger::logInfo(paste0("Running CV for ", modelName, " model"))

  np <- reticulate::import("numpy")
  joblib <- reticulate::import("joblib")

  module <- reticulate::import(pythonModule, convert = FALSE)
  classifier <- module[pythonClass]

  ###########################################################################

  gridSearchPredictons <- list()
  length(gridSearchPredictons) <- length(paramSearch)

  for (gridId in 1:length(paramSearch)) {
    # initiate prediction
    prediction <- c()

    fold <- labels$index
    ParallelLogger::logInfo(paste0("Max fold: ", max(fold)))

    for (i in 1:max(fold)) {
      ParallelLogger::logInfo(paste0("Fold ", i))
      trainY <- reticulate::r_to_py(labels$outcomeCount[fold != i])
      trainX <- reticulate::r_to_py(matrixData[fold != i, ])
      testX <- reticulate::r_to_py(matrixData[fold == i, ])

      if (requiresDenseMatrix) {
        ParallelLogger::logInfo("Converting sparse martix to dense matrix (CV)")
        trainX <- trainX$toarray()
        testX <- testX$toarray()
      }

      model <-
        fitPythonModel(
          classifier,
          paramSearch[[gridId]],
          seed,
          trainX,
          trainY,
          np,
          pythonClass
        )

      ParallelLogger::logInfo("Calculating predictions on left out fold set...")
      prediction <-
        rbind(
          prediction,
          predictValues(
            model = model,
            data = testX,
            cohort = labels[fold == i, ],
            type = "binary"
          )
        )
    }

    gridSearchPredictons[[gridId]] <- list(
      prediction = prediction,
      param = paramSearch[[gridId]]
    )
  }

  # get best para (this could be modified to enable any metric instead of AUC, just need metric input in function)

  paramGridSearch <-
    lapply(gridSearchPredictons, function(x) {
      do.call(computeGridPerformance, x)
    }) # cvAUCmean, cvAUC, param

  optimalParamInd <-
    which.max(unlist(lapply(paramGridSearch, function(x) {
      x$cvPerformance
    })))

  finalParam <- paramGridSearch[[optimalParamInd]]$param

  cvPrediction <- gridSearchPredictons[[optimalParamInd]]$prediction
  cvPrediction$evaluationType <- "CV"

  ParallelLogger::logInfo("Training final model using optimal parameters")

  trainY <- reticulate::r_to_py(labels$outcomeCount)
  trainX <- reticulate::r_to_py(matrixData)

  if (requiresDenseMatrix) {
    ParallelLogger::logInfo("Converting sparse martix to dense matrix (final model)")
    trainX <- trainX$toarray()
  }

  model <-
    fitPythonModel(
      classifier,
      finalParam,
      seed,
      trainX,
      trainY,
      np,
      pythonClass
    )

  ParallelLogger::logInfo("Calculating predictions on all train data...")
  prediction <-
    predictValues(
      model = model,
      data = trainX,
      cohort = labels,
      type = "binary"
    )
  prediction$evaluationType <- "Train"

  prediction <- rbind(
    prediction,
    cvPrediction
  )

  # saving model
  if (!dir.exists(file.path(modelLocation))) {
    dir.create(file.path(modelLocation), recursive = TRUE)
  }
  if (saveToJson) {
    sklearnToJson(
      model = model,
      path = file.path(modelLocation, "model.json")
    )
  } else {
    joblib$dump(model, file.path(modelLocation, "model.pkl"), compress = TRUE)
  }

  # feature importance
  variableImportance <-
    tryCatch(
      {
        reticulate::py_to_r(model$feature_importances_)
      },
      error = function(e) {
        ParallelLogger::logInfo(e)
        return(rep(1, ncol(matrixData)))
      }
    )

  return(
    list(
      prediction = prediction,
      finalParam = finalParam,
      paramGridSearch = paramGridSearch,
      variableImportance = variableImportance
    )
  )
}


fitPythonModel <-
  function(classifier,
           param,
           seed,
           trainX,
           trainY,
           np,
           pythonClass) {
    ParallelLogger::logInfo(paste0("data X dim: ", trainX$shape[0], "x", trainX$shape[1]))
    ParallelLogger::logInfo(paste0(
      "data Y length: ",
      np$shape(trainY)[[1]],
      " with ",
      np$sum(trainY),
      " outcomes"
    ))

    timeStart <- Sys.time()

    # print parameters
    # convert NULL to NA values
    paramString <- param
    for (ind in 1:length(paramString)) {
      if (is.null(paramString[[ind]])) {
        paramString[[ind]] <- "null"
      }
    }
    ParallelLogger::logInfo(paste(
      names(param),
      unlist(paramString),
      sep = ":",
      collapse = "    "
    ))

    if (!is.null(param)) {
      model <-
        do.call(
          paste0(pythonClass, "Inputs"),
          list(classifier = classifier, param = param)
        )
    } else {
      model <- classifier()
    }
    model <- model$fit(trainX, trainY)
    timeEnd <- Sys.time()

    ParallelLogger::logInfo(paste0(
      "Training model took (mins): ",
      difftime(timeEnd, timeStart, units = "mins")
    ))

    return(model)
  }



#' Computes grid performance with a specified performance function
#'
#' @param prediction a dataframe with predictions and outcomeCount per rowId
#' @param param a list of hyperparameters
#' @param performanceFunct a string specifying which performance function to use
#' . Default ``'compute_AUC'``
#' @return A list with overview of the performance
#' @examples
#' prediction <- data.frame(rowId = c(1, 2, 3, 4, 5),
#'                          outcomeCount = c(0, 1, 0, 1, 0),
#'                          value = c(0.1, 0.9, 0.2, 0.8, 0.3),
#'                          index = c(1, 1, 1, 1, 1))
#' param <- list(hyperParam1 = 5, hyperParam2 = 100)
#' computeGridPerformance(prediction, param, performanceFunct = "computeAuc")
#' @export
computeGridPerformance <-
  function(prediction, param, performanceFunct = "computeAuc") {
    performance <- do.call(
      what = eval(parse(text = performanceFunct)),
      args = list(prediction = prediction)
    )

    performanceFold <- c()
    for (i in 1:max(prediction$index)) {
      performanceFold <- c(
        performanceFold,
        do.call(
          what = eval(parse(text = performanceFunct)),
          args = list(prediction = prediction[prediction$index == i, ])
        )
      )
    }

    paramString <- param
    for (ind in 1:length(paramString)) {
      if (is.null(paramString[[ind]])) {
        paramString[[ind]] <- "null"
      }
    }

    paramValues <- unlist(paramString)
    names(paramValues) <- names(param)

    hyperSummary <- as.data.frame(c(
      data.frame(
        metric = performanceFunct,
        fold = c("CV", as.character(1:length(performanceFold))),
        value = c(performance, performanceFold)
      ),
      paramValues
    ))
    return(
      list(
        metric = performanceFunct,
        cvPerformance = performance,
        cvPerformancePerFold = performanceFold,
        param = param,
        hyperSummary = hyperSummary
      )
    )
  }
