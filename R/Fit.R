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
#' @param analysisPath                     The path of the analysis. The hyperparameter search is
#' saved there after each candidate, so that an interrupted search resumes when it is run again
#' with the same data and settings (set `options(plp.resumeSearch = FALSE)` to turn this off).
#' `NULL` saves nothing.
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
  fun <- resolveHook(fun, "fitFunction")
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
  analysisPath = NULL,
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

  # find every hook before fitting, so that a misspelled or missing one fails
  # now rather than after the search
  prepareData <- if (!is.null(settings$prepareData)) {
    resolveHook(settings$prepareData, "prepareData")
  }
  resolveHook(settings$train, "train")
  resolveHook(settings$predict, "predict")
  variableImportanceFun <- if (!is.null(settings$variableImportance)) {
    resolveHook(settings$variableImportance, "variableImportance")
  }
  if (!is.null(settings$saveType)) {
    saveHandler(settings$saveType)
  }

  # prepare data for training (if needed)
  if (!is.null(prepareData)) {
    prepareArgs <- list(trainData, map = NULL)
    # the model's settings, such as dataset options, for a prepareData with a
    # settings argument; others, such as toSparseM(), are called as before
    if ("settings" %in% names(formals(prepareData))) {
      prepareArgs$settings <- settings
    }
    preparedData <- do.call(prepareData, args = prepareArgs)
    covariates <- preparedData$dataMatrix
  } else {
    preparedData <- trainData
    covariates <- preparedData$covariateData
  }

  labels <- preparedData$labels
  covariateRef <- preparedData$covariateRef

  # These classifiers are binary, so they are trained on outcome presence, as
  # Cyclops clamps counts; with binary = FALSE outcomeCount can be above 1.
  # The original counts are put back in the predictions used for evaluation.
  # They are taken from trainData: prepareData() can number the matrix rows
  # differently (toSparseM() keeps the original rowId in originalRowId), but
  # the predictions come back with the original rowIds.
  originalLabels <- data.frame(
    rowId = trainData$labels$rowId,
    outcomeCount = trainData$labels$outcomeCount
  )
  labels$outcomeCount <- as.numeric(labels$outcomeCount > 0)

  # Hyperparameter optimization
  # input: matrixData, labels, settings, hyperparamSettings
  # output: model, prediction, finalParam, hyperSummary
  tuneResult <- tuneHyperparameters(
    covariates,
    labels,
    param = param,
    settings = settings,
    hyperparamSettings = hyperparameterSettings,
    analysisPath = analysisPath
  )
  labelRows <- match(tuneResult$prediction$rowId, originalLabels$rowId)
  if (anyNA(labelRows)) {
    stop(
      "The predictions of ", settings$modelName, " have rowIds that are not in the training labels; ",
      "its predict function must return the original rowIds (toSparseM() keeps them in originalRowId)"
    )
  }
  tuneResult$prediction$outcomeCount <- originalLabels$outcomeCount[labelRows]

  # add code for var imp here - model and settings

  # the model is still usable without importances, so an error here is reported
  # and the covariates get importance 0
  variableImportance <- if (!is.null(variableImportanceFun)) {
    tryCatch(
      do.call(
        variableImportanceFun,
        list(model = tuneResult$model, covariateMap = covariateRef)
      ),
      error = function(e) {
        ParallelLogger::logWarn(paste0(
          "Could not compute the variable importance of ", settings$modelName, ": ",
          conditionMessage(e)
        ))
        NULL
      }
    )
  }

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
  # the search is no longer needed; runPlp() keeps it until the result is saved
  if (!isTRUE(.plpCheckpoint$deferRemoval)) {
    removeSearchCheckpoint(tuneResult$checkpoint)
  }
  return(result)
}


tuneHyperparameters <- function(
  data,
  labels,
  param,
  settings,
  hyperparamSettings,
  analysisPath = NULL
) {
  if (is.null(hyperparamSettings) || is.null(hyperparamSettings$tuningMetric)) {
    hyperparamSettings <- createHyperparameterSettings()
  }
  hooks <- resolveTuningHooks(settings)
  trainFun <- hooks$train
  predictFun <- hooks$predict
  metric <- resolveTuningMetric(hyperparamSettings, settings)
  selector <- resolveSelector(hyperparamSettings, settings)
  select <- function(history) {
    if (is.null(selector)) {
      return(selectBest(history, metric))
    }
    best <- selector(history = history, metric = metric)
    if (length(best) == 0) {
      return(integer(0))
    }
    if (length(best) != 1L || is.na(best) || !best %in% seq_along(history)) {
      stop("The selector must return the index of one candidate in the history", call. = FALSE)
    }
    if (is.na(history[[best]]$cvPerformance)) {
      stop("The selector chose a candidate without a performance", call. = FALSE)
    }
    as.integer(best)
  }
  # With an analysisPath, the search is saved after each candidate and an
  # interrupted search resumes (see SearchCheckpoint.R). The random number
  # generator is saved too: at the start of the search, so that a resumed
  # search makes the same draws when it builds the candidates, and after each
  # candidate.
  checkpoint <- openSearchCheckpoint(analysisPath, data, labels, param, settings,
                                     hyperparamSettings, metric, hooks)
  saved <- loadSearchCheckpoint(checkpoint)
  randomSeeds <- list()
  if (!is.null(checkpoint)) {
    if (!is.null(saved)) {
      setRandomSeed(saved$randomSeeds[[1]])
    }
    randomSeeds[[1]] <- getRandomSeed()
  }
  baseSeed <- NULL
  startCandidates <- function() {
    iterator <<- prepareHyperparameterGrid(
      paramDefinition = param,
      hyperSettings = hyperparamSettings
    )
    if (hooks$stateful) {
      # one seed for the search, from the model's seed or the random stream
      baseSeed <<- settings$seed %||% sample.int(2147483646L, 1L)
    }
  }
  iterator <- NULL
  startCandidates()
  if (!is.null(saved)) {
    # the candidates the saved search ran must be the ones this search makes
    for (i in seq_along(saved$history)) {
      if (i > 1) {
        setRandomSeed(saved$randomSeeds[[i]])
      }
      candidate <- iterator$getNext(saved$history[seq_len(i - 1)])
      if (!identical(candidate, saved$history[[i]]$param)) {
        ParallelLogger::logInfo(paste0(
          "The saved hyperparameter search of ", settings$modelName, " in ", checkpoint$directory,
          " made other candidates; starting the search over"
        ))
        saved <- NULL
        removeSearchCheckpoint(checkpoint)
        checkpoint$predictionFiles <- integer(0)
        setRandomSeed(randomSeeds[[1]])
        startCandidates()
        break
      }
    }
  }
  history <- list()
  bestCvPrediction <- c()
  bestIndex <- NA_integer_
  # with a selector that can choose an earlier candidate, every candidate's
  # predictions are kept
  cvPredictions <- list()
  foldIds <- unique(labels$index)
  # settings saved by earlier versions have no validation
  validation <- hyperparamSettings$validation %||% "cv"
  evaluationType <- "CV"
  if (identical(validation, "trainValidation")) {
    # one fit per candidate: train on all folds but the last, score on the last
    if (length(foldIds) < 2) {
      stop("validation = \"trainValidation\" needs at least 2 folds in the split settings", call. = FALSE)
    }
    foldIds <- max(foldIds)
    evaluationType <- trainValidationEvaluationType
  }

  executor <- NULL
  if (hooks$stateful) {
    executor <- createFoldExecutor(
      foldIds, data, labels, settings, hooks, baseSeed,
      foldWorkers = hyperparamSettings$foldWorkers %||% 1L
    )
    on.exit(executor$stop(), add = TRUE)
  }
  checkpointStates <- !is.null(checkpoint) && !is.null(hooks$updateState)

  candidateIndex <- 0L
  if (!is.null(saved)) {
    history <- saved$history
    candidateIndex <- length(history)
    savedPredictions <- stats::setNames(saved$predictions, saved$predictionIndex)
    if (!is.null(selector)) {
      cvPredictions <- vector("list", length(history))
      for (i in saved$predictionIndex) {
        cvPredictions[i] <- list(savedPredictions[[as.character(i)]])
      }
    } else if (length(saved$predictionIndex) > 0) {
      bestIndex <- saved$predictionIndex[[1]]
      bestCvPrediction <- savedPredictions[[1]]
    }
    rm(savedPredictions)
    if (checkpointStates) {
      executor$setStates(saved$foldStates)
    }
    randomSeeds <- saved$randomSeeds
    setRandomSeed(randomSeeds[[length(history) + 1]])
    ParallelLogger::logInfo(paste0(
      "Resuming the hyperparameter search of ", settings$modelName, " after ",
      length(history), " candidates, from ", checkpoint$directory
    ))
    for (entry in history) {
      if (!is.null(entry$error)) {
        ParallelLogger::logInfo(paste0(
          "Param ", paste(names(entry$param), entry$param, sep = "=", collapse = ", "),
          " was skipped in the saved search: ", entry$error
        ))
      }
    }
    saved <- NULL
  }
  repeat {
    candidate <- iterator$getNext(history)
    if (is.null(candidate)) {
      break
    }
    candidateIndex <- candidateIndex + 1L
    foldInfo <- NULL
    if (hooks$stateful) {
      # every fold runs the candidate; it is skipped if any fold could not fit it
      foldResults <- executor$run(candidate, candidateIndex)
      foldErrors <- unique(unlist(lapply(foldResults, function(x) x$error)))
      foldSeconds <- vapply(foldResults, function(x) x$seconds, numeric(1))
      if (!is.null(hooks$fitInfo)) {
        foldInfo <- lapply(foldResults, function(x) x$info)
      }
      folds <- if (length(foldErrors) > 0) {
        errorCondition(paste(foldErrors, collapse = "; "), class = "plpCandidateError")
      } else {
        list(
          prediction = do.call(rbind, lapply(foldResults, function(x) x$prediction)),
          performance = vapply(foldResults, function(x) metric$fun(x$prediction), numeric(1))
        )
      }
    } else {
      # A candidate the model cannot run (it signals this with stopCandidate(),
      # for example when it runs out of memory) is recorded with a missing
      # performance and the search goes on with the next one; selectBest() skips
      # missing performances. Any other error stops the search.
      foldSeconds <- c()
      folds <- tryCatch(
        {
          cvPrediction <- c()
          cvPerformance <- c()
          for (i in foldIds) {
            foldStart <- Sys.time()
            ind <- labels$index != i
            model <- do.call(
              trainFun,
              list(
                dataMatrix = data[ind, , drop = FALSE],
                labels = labels[ind, ],
                hyperParameters = candidate,
                settings = settings
              )
            )
            prediction <- do.call(
              predictFun,
              list(
                plpModel = model,
                data = data[!ind, , drop = FALSE],
                cohort = labels[!ind, ]
              )
            )
            performance <- metric$fun(prediction)
            cvPrediction <- rbind(cvPrediction, prediction)
            cvPerformance <- c(cvPerformance, performance)
            foldSeconds <- c(foldSeconds, as.numeric(difftime(Sys.time(), foldStart, units = "secs")))
          }
          list(prediction = cvPrediction, performance = cvPerformance)
        },
        plpCandidateError = function(e) e
      )
    }
    candidateError <- NULL
    if (inherits(folds, "plpCandidateError")) {
      candidateError <- conditionMessage(folds)
      skipMessage <- paste0(
        "Skipping param ",
        paste(names(candidate), candidate, sep = "=", collapse = ", "),
        " of ", settings$modelName, ": ", candidateError
      )
      ParallelLogger::logWarn(skipMessage)
      warning(warningCondition(skipMessage, class = "plpCandidateWarning"))
      cvPrediction <- NULL
      cvPerformance <- rep(NA_real_, length(foldIds))
      meanCvPerformance <- NA_real_
    } else {
      cvPrediction <- folds$prediction
      cvPerformance <- folds$performance
      meanCvPerformance <- mean(cvPerformance, na.rm = TRUE)
      if (is.nan(meanCvPerformance)) {
        meanCvPerformance <- NA_real_
      }
    }
    entry <- list(
      metric = metric$name,
      param = candidate,
      cvPerformance = meanCvPerformance,
      cvPerformancePerFold = cvPerformance,
      error = candidateError,
      hyperSummary = makeHyperSummary(
        metric,
        meanCvPerformance,
        cvPerformance,
        candidate,
        foldIds,
        evaluationType
      ),
      foldIds = foldIds,
      seconds = foldSeconds
    )
    if (!is.null(foldInfo)) {
      entry$info <- foldInfo
    }
    history[[length(history) + 1]] <- entry
    if (!is.null(selector)) {
      cvPredictions[length(history)] <- list(cvPrediction)
    }
    if (is.null(candidateError)) {
      # Same rule as the final selection, so CV predictions match the fitted candidate on ties.
      if (identical(select(history), length(history))) {
        ParallelLogger::logInfo(paste0(
          "New best performance ",
          round(meanCvPerformance, 4),
          " with param: ",
          paste(names(candidate), candidate, sep = "=", collapse = ", ")
        ))
        bestCvPrediction <- cvPrediction
        bestIndex <- length(history)
      } else {
        ParallelLogger::logInfo(paste0(
          "Performance ",
          round(meanCvPerformance, 4),
          " with param: ",
          paste(names(candidate), candidate, sep = "=", collapse = ", ")
        ))
      }
    }
    if (!is.null(checkpoint) && checkpoint$active) {
      randomSeeds[[length(history) + 1]] <- getRandomSeed()
      keep <- if (!is.null(selector)) {
        which(!vapply(cvPredictions, is.null, logical(1)))
      } else if (!is.na(bestIndex)) {
        bestIndex
      } else {
        integer(0)
      }
      predictions <- if (!is.null(selector)) cvPredictions[keep] else list(bestCvPrediction)[seq_along(keep)]
      commitSearchCheckpoint(
        checkpoint,
        history,
        randomSeeds,
        stats::setNames(predictions, keep),
        foldStates = if (checkpointStates) executor$getStates()
      )
    }
  }
  iterator$finalize(history)
  best <- select(history)
  if (length(best) == 0) {
    errors <- unique(unlist(lapply(history, function(x) x$error)))
    stop(
      "All ", length(history), " hyperparameter candidates of ", settings$modelName,
      " were skipped",
      if (length(errors) > 0) paste0(": ", paste(errors, collapse = "; ")) else
        " or had no performance",
      call. = FALSE
    )
  }

  finalParam <- history[[best]]$param

  cvPrediction <- if (!is.null(selector)) cvPredictions[[best]] else bestCvPrediction
  rm(cvPredictions)
  cvPrediction$evaluationType <- evaluationType

  # fit final model
  ParallelLogger::logInfo("Fit best model on whole training set")
  finalArgs <- list(
    dataMatrix = data,
    labels = labels,
    hyperParameters = finalParam,
    settings = settings
  )
  if (hooks$stateful) {
    # the folds are freed first; the final fit starts cold, on all rows
    executor$stop()
    if (!is.null(hooks$prepareFold)) {
      finalArgs$dataMatrix <- withLocalSeed(tuningSeed(baseSeed, 0, 0), do.call(
        hooks$prepareFold,
        list(dataMatrix = data, labels = labels, settings = settings)
      ))
    }
    if (hooks$trainContext) {
      finalArgs$context <- list(foldId = NULL, final = TRUE, state = NULL, candidateIndex = best)
    }
  }
  finalModel <- if (hooks$stateful) {
    withLocalSeed(tuningSeed(baseSeed, 0, -1), do.call(trainFun, finalArgs))
  } else {
    do.call(trainFun, finalArgs)
  }
  rm(finalArgs)

  prediction <- do.call(
    predictFun,
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
    paramSearch = history,
    checkpoint = checkpoint
  )
}

#' Skip a hyperparameter candidate that cannot be fitted
#'
#' @description
#' Called from a model's train or predict function to say that the current
#' hyperparameter candidate cannot be fitted, for example because it runs out
#' of memory. The hyperparameter search records the candidate with a missing
#' performance and this message, warns, and goes on with the next candidate.
#' The search stops if every candidate is skipped.
#'
#' Any other error in train or predict stops the search, so a model that never
#' calls `stopCandidate()` behaves as before.
#'
#' @param message Why the candidate cannot be fitted.
#' @param ... Further fields for the condition, passed to [errorCondition()].
#' @return Does not return; signals an error of class `plpCandidateError`.
#' @examples
#' train <- function(dataMatrix, labels, hyperParameters, settings) {
#'   if (hyperParameters$size > 1000) {
#'     stopCandidate("size larger than the memory allows")
#'   }
#'   list(size = hyperParameters$size)
#' }
#' tryCatch(
#'   train(NULL, NULL, list(size = 5000), NULL),
#'   plpCandidateError = function(e) conditionMessage(e)
#' )
#' @export
stopCandidate <- function(message, ...) {
  stop(errorCondition(message, ..., class = "plpCandidateError", call = sys.call(-1)))
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

makeHyperSummary <- function(metric, meanScore, foldScores, param, foldIds, evaluationType = "CV") {
  perfRows <- data.frame(
    metric = metric$name,
    fold = c(evaluationType, paste0("Fold", foldIds)),
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
