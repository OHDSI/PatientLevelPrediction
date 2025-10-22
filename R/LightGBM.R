# @file LightGBM.R
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

#' Create setting for gradient boosting machine model using lightGBM 
#' (https://github.com/microsoft/LightGBM/tree/master/R-package).
#'
#' @param nthread           The number of computer threads to use (how many cores do you have?)
#' @param earlyStopRound    If the performance does not increase over earlyStopRound number of trees then training stops (this prevents overfitting)
#' @param numIterations     Number of boosting iterations.
#' @param numLeaves         This hyperparameter sets the maximum number of leaves. Increasing this parameter can lead to higher model complexity and potential overfitting.
#' @param maxDepth          This hyperparameter sets the maximum depth . Increasing this parameter can also lead to higher model complexity and potential overfitting.
#' @param minDataInLeaf     This hyperparameter sets the minimum number of data points that must be present in a leaf node. Increasing this parameter can help to reduce overfitting
#' @param learningRate      This hyperparameter controls the step size at each iteration of the gradient descent algorithm. Lower values can lead to slower convergence but may result in better performance.
#' @param lambdaL1          This hyperparameter controls L1 regularization, which can help to reduce overfitting by encouraging sparse models.
#' @param lambdaL2          This hyperparameter controls L2 regularization, which can also help to reduce overfitting by discouraging large weights in the model.
#' @param scalePosWeight    Controls weight of positive class in loss - useful for imbalanced classes
#' @param isUnbalance       This parameter cannot be used at the same time with scalePosWeight, choose only one of them. While enabling this should increase the overall performance metric of your model, it will also result in poor estimates of the individual class probabilities.
#' @param seed              An option to add a seed when training the final model
#' @return A list of settings that can be used to train a model with \code{runPlp}
#'
#' @examplesIf rlang::is_installed("lightgbm")
#' modelLightGbm <- setLightGBM(
#'   numLeaves = c(20, 31, 50), maxDepth = c(-1, 5, 10),
#'   minDataInLeaf = c(10, 20, 30), learningRate = c(0.05, 0.1, 0.3)
#' )
#' @export
setLightGBM <- function(nthread = 20,
                        earlyStopRound = 25,
                        numIterations = c(100),
                        numLeaves = c(31),
                        maxDepth = c(5, 10),
                        minDataInLeaf = c(20),
                        learningRate = c(0.05, 0.1, 0.3),
                        lambdaL1 = c(0),
                        lambdaL2 = c(0),
                        scalePosWeight = 1,
                        isUnbalance = FALSE,
                        seed = sample(10000000, 1)) {
  rlang::check_installed("lightgbm")
  checkIsClass(seed, c("numeric", "integer"))

  if (length(nthread) > 1) {
    stop("nthread must be length 1")
  }
  if (!inherits(x = seed, what = c("numeric", "integer"))) {
    stop("Invalid seed")
  }
  if (sum(numIterations < 1) > 0) {
    stop("numIterations must be greater that 0")
  }
  if (sum(numLeaves < 2) > 0) {
    stop("numLeaves must be greater that 1")
  }
  if (sum(numLeaves > 131072) > 0) {
    stop("numLeaves must be less that or equal 131072")
  }
  if (sum(learningRate <= 0) > 0) {
    stop("learningRate must be greater that 0")
  }
  if (sum(lambdaL1 < 0) > 0) {
    stop("lambdaL1 must be 0 or greater")
  }
  if (sum(lambdaL2 < 0) > 0) {
    stop("lambdaL2 must be 0 or greater")
  }
  if (sum(scalePosWeight < 0) > 0) {
    stop("scalePosWeight must be 0 or greater")
  }
  if (isUnbalance == TRUE && sum(scalePosWeight != 1) > 0) {
    stop("isUnbalance cannot be used at the same time with scale_pos_weight != 1, choose only one of them")
  }

  param <- list(
    earlyStopRound = earlyStopRound,
    numIterations = numIterations,
    numLeaves = numLeaves,
    maxDepth = maxDepth,
    minDataInLeaf = minDataInLeaf,
    learningRate = learningRate,
    lambdaL1 = lambdaL1,
    lambdaL2 = lambdaL2,
    isUnbalance = isUnbalance,
    scalePosWeight = scalePosWeight
  )


  settings <- list(
    modelType = "LightGBM",
    seed = seed[[1]],
    modelName = "LightGBM",
    threads = nthread[1],
    varImpRFunction = "varImpLightGBM",
    trainRFunction = "fitLightGBM",
    predictRFunction = "predictLightGBM"
  )

  result <- list(
    fitFunction = "fitRclassifier", # or fitBinaryClassifier?
    param = param,
    settings = settings,
    saveToJson = TRUE,
    saveType = "lightgbm"
  )

  class(result) <- "modelSettings"

  return(result)
}



varImpLightGBM <- function(model,
                           covariateMap) {
  varImp <- lightgbm::lgb.importance(model, percentage = TRUE) %>% dplyr::select("Feature", "Gain")

  varImp <- data.frame(
    covariateId = gsub(".*_", "", varImp$Feature),
    covariateValue = varImp$Gain,
    included = 1
  )

  varImp <- merge(covariateMap, varImp, by.x = "columnId", by.y = "covariateId")
  varImp <- varImp %>%
    dplyr::select("covariateId", "covariateValue", "included")

  return(varImp)
}

predictLightGBM <- function(plpModel,
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

    # use the include??

    newData <- matrixObjects$dataMatrix
    cohort <- matrixObjects$labels
  } else {
    newData <- data
  }

  if (inherits(plpModel, "plpModel")) {
    model <- plpModel$model
  } else {
    model <- plpModel
  }

  pred <- data.frame(value = stats::predict(model, newData))
  prediction <- cohort
  prediction$value <- pred$value

  prediction <- prediction %>%
    dplyr::select(-"rowId") %>%
    dplyr::rename(rowId = "originalRowId")

  attr(prediction, "metaData") <- list(modelType = attr(plpModel, "modelType"))

  return(prediction)
}

fitLightGBM <- function(dataMatrix,
                        labels,
                        hyperParameters,
                        settings) {
  if (!is.null(hyperParameters$earlyStopRound)) {
    trainInd <- sample(nrow(dataMatrix), nrow(dataMatrix) * 0.9)
    train <- lightgbm::lgb.Dataset(
      data = dataMatrix[trainInd, , drop = FALSE],
      label = labels$outcomeCount[trainInd]
    )
    test <- lightgbm::lgb.Dataset(
      data = dataMatrix[-trainInd, , drop = FALSE],
      label = labels$outcomeCount[-trainInd]
    )
    watchlist <- list(train = train, test = test)
  } else {
    train <- lightgbm::lgb.Dataset(
      data = dataMatrix,
      label = labels$outcomeCount,
      free_raw_data = FALSE,
    )
    watchlist <- list()
  }

  set.seed(settings$seed)
  model <- lightgbm::lgb.train(
    data = train,
    params = list(
      objective = "binary",
      boost = "gbdt",
      metric = "auc",
      num_iterations = hyperParameters$numIterations,
      num_leaves = hyperParameters$numLeaves,
      max_depth = hyperParameters$maxDepth,
      learning_rate = hyperParameters$learningRate,
      feature_pre_filter = FALSE,
      min_data_in_leaf = hyperParameters$minDataInLeaf,
      scale_pos_weight = hyperParameters$scalePosWeight,
      lambda_l1 = hyperParameters$lambdaL1,
      lambda_l2 = hyperParameters$lambdaL2,
      seed = settings$seed,
      is_unbalance = hyperParameters$isUnbalance,
      max_bin = 255,
      num_threads = settings$threads
    ),
    verbose = 1,
    early_stopping_rounds = hyperParameters$earlyStopRound,
    valids = watchlist
  )

  return(model)
}
