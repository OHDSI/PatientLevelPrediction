# @file CatBoostPython.R
#
# Copyright 2026 Observational Health Data Sciences and Informatics
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

#' Create settings for CatBoost classifier using Python catboost
#'
#' @param iterations            Number of boosting iterations.
#' @param depth                 Depth of trees.
#' @param learningRate          Learning rate.
#' @param l2LeafReg             L2 regularization coefficient.
#' @param randomStrength        Score standard deviation multiplier.
#' @param borderCount           Number of splits for numerical features.
#' @param rsm                   Fraction of features used at each split.
#' @param subsample             Fraction of samples used for each tree.
#' @param earlyStoppingRounds   Stop if metric does not improve for this many rounds.
#' @param threadCount           Number of threads to use.
#' @param verbose               Verbose output level.
#' @param seed                  Random seed.
#' @return A modelSettings object that can be used by \code{runPlp}.
#' @examples
#' \dontshow{ # dontrun reason: requires python's catboost, checkCatBoostPython() will error without it }
#' \dontrun{
#' modelCatBoost <- setCatBoost(
#'   iterations = c(100, 300),
#'   depth = c(4, 6, 8),
#'   learningRate = c(0.05, 0.1, 0.3)
#' )
#' }
#' @export
setCatBoost <- function(iterations = c(100, 300),
                        depth = c(4, 6, 8),
                        learningRate = c(0.05, 0.1, 0.3),
                        l2LeafReg = c(3),
                        randomStrength = c(1),
                        borderCount = c(254),
                        rsm = c(1),
                        subsample = c(1),
                        earlyStoppingRounds = c(25),
                        threadCount = 20,
                        verbose = FALSE,
                        seed = sample(10000000, 1)) {
  checkCatBoostPython()
  checkIsClass(seed, c("numeric", "integer"))

  if (length(threadCount) > 1) {
    stop("threadCount must be length 1")
  }
  if (!inherits(seed, c("numeric", "integer"))) {
    stop("Invalid seed")
  }
  if (sum(iterations < 1) > 0) {
    stop("iterations must be greater than 0")
  }
  if (sum(depth < 1) > 0) {
    stop("depth must be greater than 0")
  }
  if (sum(learningRate <= 0) > 0) {
    stop("learningRate must be greater than 0")
  }
  if (sum(l2LeafReg < 0) > 0) {
    stop("l2LeafReg must be 0 or greater")
  }
  if (sum(randomStrength < 0) > 0) {
    stop("randomStrength must be 0 or greater")
  }
  if (sum(borderCount < 1) > 0) {
    stop("borderCount must be greater than 0")
  }
  if (sum(rsm <= 0) > 0 || sum(rsm > 1) > 0) {
    stop("rsm must be > 0 and <= 1")
  }
  if (sum(subsample <= 0) > 0 || sum(subsample > 1) > 0) {
    stop("subsample must be > 0 and <= 1")
  }
  if (sum(earlyStoppingRounds < 1) > 0) {
    stop("earlyStoppingRounds must be greater than 0")
  }

  param <- list(
    iterations = iterations,
    depth = depth,
    learningRate = learningRate,
    l2LeafReg = l2LeafReg,
    randomStrength = randomStrength,
    borderCount = borderCount,
    rsm = rsm,
    subsample = subsample,
    earlyStoppingRounds = earlyStoppingRounds
  )

  settings <- list(
    modelType = "binary",
    modelName = "catboost",
    seed = seed[[1]],
    threads = threadCount[1],
    verbose = verbose,
    requiresDenseMatrix = FALSE,
    pythonModule = "catboost",
    pythonClass = "CatBoostClassifier",
    variableImportance = "varImpCatBoostPython",
    train = "fitCatBoostPython",
    predict = "predictCatBoostPython",
    prepareData = "toSparseM",
    saveType = "saveLoadCatBoostPython"
  )

  result <- list(
    param = param,
    settings = settings
  )
  class(result) <- "modelSettings"
  return(result)
}

predictCatBoostPython <- function(plpModel,
                                  data,
                                  cohort) {
  if (inherits(data, "plpData")) {
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

  model <- if (inherits(plpModel, "plpModel")) plpModel$model else plpModel

  pythonData <- reticulate::r_to_py(newData)
  if (reticulate::py_has_attr(pythonData, "copy")) {
    pythonData <- pythonData$copy()
  }
  predictionValues <- reticulate::py_to_r(model$predict_proba(pythonData))
  if (is.null(dim(predictionValues))) {
    values <- as.numeric(predictionValues)
  } else if (ncol(predictionValues) >= 2) {
    values <- predictionValues[, 2]
  } else {
    values <- predictionValues[, 1]
  }

  prediction <- cohort
  prediction$value <- values
  prediction <- prediction %>%
    dplyr::select(-"rowId") %>%
    dplyr::rename(rowId = "originalRowId")

  modelType <- if (inherits(plpModel, "plpModel")) {
    attr(plpModel, "modelType")
  } else {
    "binary"
  }
  attr(prediction, "metaData") <- list(modelType = modelType)
  return(prediction)
}

fitCatBoostPython <- function(dataMatrix,
                              labels,
                              hyperParameters,
                              settings) {
  checkCatBoostPython()
  catboost <- reticulate::import("catboost", convert = FALSE)
  set.seed(settings$seed)

  trainMatrix <- dataMatrix
  trainLabels <- labels$outcomeCount
  evalSet <- NULL
  earlyStopping <- hyperParameters$earlyStoppingRounds

  if (!is.null(earlyStopping)) {
    trainInd <- sample(nrow(dataMatrix), nrow(dataMatrix) * 0.9)
    if (sum(labels$outcomeCount[-trainInd]) == 0) {
      stop("No outcomes in early stopping set, either increase size of training set or turn off early stopping")
    }
    trainMatrix <- dataMatrix[trainInd, , drop = FALSE]
    trainLabels <- labels$outcomeCount[trainInd]
    validMatrix <- dataMatrix[-trainInd, , drop = FALSE]
    validLabels <- labels$outcomeCount[-trainInd]

    evalSet <- reticulate::tuple(list(
      reticulate::r_to_py(validMatrix),
      reticulate::r_to_py(validLabels)
    ))
  }

  modelArgs <- list(
    loss_function = "Logloss",
    eval_metric = "AUC",
    random_seed = settings$seed,
    thread_count = settings$threads,
    allow_writing_files = FALSE,
    verbose = settings$verbose,
    iterations = hyperParameters$iterations,
    depth = hyperParameters$depth,
    learning_rate = hyperParameters$learningRate,
    l2_leaf_reg = hyperParameters$l2LeafReg,
    random_strength = hyperParameters$randomStrength,
    border_count = hyperParameters$borderCount,
    rsm = hyperParameters$rsm,
    subsample = hyperParameters$subsample
  )

  model <- do.call(catboost$CatBoostClassifier, modelArgs)
  trainX <- reticulate::r_to_py(trainMatrix)
  if (reticulate::py_has_attr(trainX, "copy")) {
    trainX <- trainX$copy()
  }
  fitArgs <- list(
    X = trainX,
    y = reticulate::r_to_py(trainLabels),
    verbose = settings$verbose
  )
  if (!is.null(evalSet)) {
    evalX <- reticulate::r_to_py(validMatrix)
    if (reticulate::py_has_attr(evalX, "copy")) {
      evalX <- evalX$copy()
    }
    fitArgs$eval_set <- evalSet
    fitArgs$eval_set <- reticulate::tuple(list(
      evalX,
      reticulate::r_to_py(validLabels)
    ))
    fitArgs$use_best_model <- TRUE
    fitArgs$early_stopping_rounds <- as.integer(earlyStopping)
  }
  model <- do.call(model$fit, fitArgs)
  reticulate::py_set_attr(model, "dense", FALSE)
  return(model)
}

varImpCatBoostPython <- function(model,
                                 covariateMap) {
  values <- tryCatch(
    {
      as.numeric(reticulate::py_to_r(model$get_feature_importance()))
    },
    error = function(e) {
      ParallelLogger::logInfo("Error calculating CatBoost variable importance")
      ParallelLogger::logInfo(e)
      rep(0, nrow(covariateMap))
    }
  )

  if (length(values) < nrow(covariateMap)) {
    values <- c(values, rep(0, nrow(covariateMap) - length(values)))
  }
  if (length(values) > nrow(covariateMap)) {
    values <- values[seq_len(nrow(covariateMap))]
  }

  covariateMap$covariateValue <- values
  covariateMap$included <- covariateMap$covariateValue != 0
  covariateMap <- covariateMap %>%
    dplyr::select("covariateId", "covariateValue", "included")

  return(covariateMap)
}

saveLoadCatBoostPython <- function() {
  checkCatBoostPython()
  list(
    save = function(model, file) {
      model$save_model(file)
    },
    load = function(file) {
      catboost <- reticulate::import("catboost", convert = FALSE)
      model <- catboost$CatBoostClassifier()
      model$load_model(file)
      reticulate::py_set_attr(model, "dense", FALSE)
      model
    }
  )
}
