# @file GradientBoostingMachine.R
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

#' Create setting for gradient boosting machine model using gbm_xgboost implementation
#'
#' @param ntrees     The number of trees to build
#' @param nthread   The number of computer threads to use (how many cores do you have?)
#' @param earlyStopRound   If the performance does not increase over earlyStopRound number of trees then training stops (this prevents overfitting)
#' @param maxDepth  Maximum depth of each tree - a large value will lead to slow model training
#' @param minChildWeight  Minimum sum of of instance weight in a child node - larger values are more conservative
#' @param learnRate The boosting learn rate
#' @param scalePosWeight Controls weight of positive class in loss - useful for imbalanced classes
#' @param lambda    L2 regularization on weights - larger is more conservative
#' @param alpha     L1 regularization on weights - larger is more conservative
#' @param seed       An option to add a seed when training the final model
#' @return A modelSettings object that can be used to fit the model
#'
#' @examplesIf rlang::is_installed("xgboost")
#' modelGbm <- setGradientBoostingMachine(
#'   ntrees = c(10, 100), nthread = 20,
#'   maxDepth = c(4, 6), learnRate = c(0.1, 0.3)
#' )
#'
#' @export
setGradientBoostingMachine <- function(ntrees = c(100, 300),
                                       nthread = 20,
                                       earlyStopRound = 25,
                                       maxDepth = c(4, 6, 8),
                                       minChildWeight = 1,
                                       learnRate = c(0.05, 0.1, 0.3),
                                       scalePosWeight = 1,
                                       lambda = 1, 
                                       alpha = 0,
                                       seed = sample(10000000, 1)) {
  rlang::check_installed("xgboost")

  checkIsClass(seed, c("numeric", "integer"))

  if (length(nthread) > 1) {
    stop(paste("nthreads must be length 1"))
  }
  if (!inherits(x = seed, what = c("numeric", "NULL", "integer"))) {
    stop("Invalid seed")
  }
  if (!inherits(x = ntrees, what = c("numeric", "integer"))) {
    stop("ntrees must be a numeric value ")
  }
  if (sum(ntrees < 1) > 0) {
    stop("ntrees must be greater than 0 or -1")
  }
  if (!inherits(x = maxDepth, what = c("numeric", "integer"))) {
    stop("maxDepth must be a numeric value")
  }
  if (sum(maxDepth < 1) > 0) {
    stop("maxDepth must be greater that 0")
  }
  if (!inherits(x = minChildWeight, what = c("numeric", "integer"))) {
    stop("minChildWeight must be a numeric value")
  }
  if (sum(minChildWeight < 0) > 0) {
    stop("minChildWeight must be greater that 0")
  }
  if (!inherits(x = learnRate, what = "numeric")) {
    stop("learnRate must be a numeric value")
  }
  if (sum(learnRate <= 0) > 0) {
    stop("learnRate must be greater that 0")
  }
  if (sum(learnRate > 1) > 0) {
    stop("learnRate must be less that or equal to 1")
  }
  if (!inherits(x = earlyStopRound, what = c("numeric", "integer", "NULL"))) {
    stop("incorrect class for earlyStopRound")
  }
  if (!inherits(x = lambda, what = c("numeric", "integer"))) {
    stop("lambda must be a numeric value")
  }
  if (sum(lambda < 0) > 0) {
    stop("lambda must be 0 or greater")
  }
  if (!inherits(x = alpha, what = c("numeric", "integer"))) {
    stop("alpha must be a numeric value")
  }
  if (sum(alpha < 0) > 0) {
    stop("alpha must be 0 or greater")
  }
  if (!inherits(x = scalePosWeight, what = c("numeric", "integer"))) {
    stop("scalePosWeight must be a numeric value >= 0")
  }
  if (sum(scalePosWeight < 0) > 0) {
    stop("scalePosWeight must be 0 or greater")
  }

  param <- list(
    ntrees = ntrees,
    earlyStopRound = earlyStopRound,
    maxDepth = maxDepth,
    minChildWeight = minChildWeight,
    learnRate = learnRate,
    lambda = lambda,
    alpha = alpha,
    scalePosWeight = scalePosWeight
  )
  
  settings <- list(
    modelType = "Xgboost",
    seed = seed[[1]],
    modelName = "Gradient Boosting Machine",
    threads = nthread[1],
    varImpRFunction = "varImpXgboost",
    trainRFunction = "fitXgboost",
    predictRFunction = "predictXgboost"
    # add data conversion function?
  )

  result <- list(
    fitFunction = "fitBinaryClassifier",
    param = param,
    settings = settings,
    saveToJson = TRUE,
    saveType = "xgboost"
  )

  class(result) <- "modelSettings"

  return(result)
}



varImpXgboost <- function(
    model,
    covariateMap) {
  varImp <- xgboost::xgb.importance(model = model)

  varImp$Feature <- as.numeric(substring(varImp$Feature, 2)) + 1 # adding +1 as xgboost index starts at 0
  varImp <- merge(covariateMap, varImp, by.x = "columnId", by.y = "Feature")
  varImp <- varImp %>%
    dplyr::mutate(included = 1) %>%
    dplyr::rename(covariateValue = "Gain") %>%
    dplyr::select("covariateId", "covariateValue", "included")

  return(varImp)
}

predictXgboost <- function(
    plpModel,
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

  if (inherits(plpModel, "plpModel")) {
    model <- plpModel$model
  } else {
    model <- plpModel
  }

  pred <- data.frame(value = stats::predict(model, newData))
  prediction <- cohort
  prediction$value <- pred$value

  # fix the rowIds to be the old ones?
  # now use the originalRowId and remove the matrix rowId
  prediction <- prediction %>%
    dplyr::select(-"rowId") %>%
    dplyr::rename(rowId = "originalRowId")

  attr(prediction, "metaData") <- list(modelType = attr(plpModel, "modelType"))

  return(prediction)
}

fitXgboost <- function(
    dataMatrix,
    labels,
    hyperParameters,
    settings
    ) {
  
  # this function will just fit the parameters for given hyperparam
  # values and data 
  
  set.seed(settings$seed)
  if (!is.null(hyperParameters$earlyStopRound)) {
    trainInd <- sample(nrow(dataMatrix), nrow(dataMatrix) * 0.9)
    if (sum(labels$outcomeCount[-trainInd]) == 0) {
      stop("No outcomes in early stopping set, either increase size of training
            set or turn off early stopping")
    }
    train <- xgboost::xgb.DMatrix(
      data = dataMatrix[trainInd, , drop = FALSE],
      label = labels$outcomeCount[trainInd]
    )
    test <- xgboost::xgb.DMatrix(
      data = dataMatrix[-trainInd, , drop = FALSE],
      label = labels$outcomeCount[-trainInd]
    )
    evals <- list(train = train, test = test)
  } else {
    train <- xgboost::xgb.DMatrix(
      data = dataMatrix,
      label = labels$outcomeCount
    )
    evals <- list()
  }

  outcomes <- sum(labels$outcomeCount > 0)
  N <- nrow(labels)
  outcomeProportion <- outcomes / N
  evalsArgument <- if (utils::packageVersion("xgboost") >= "3.1.0.1") "evals" else "watchlist"
  trainArgs <- list(
    data = train,
    params = list(
      booster = "gbtree",
      max_depth = hyperParameters$maxDepth,
      eta = hyperParameters$learnRate,
      min_child_weight = hyperParameters$minChildWeight,
      scale_pos_weight = hyperParameters$scalePosWeight,
      lambda = hyperParameters$lambda,
      alpha = hyperParameters$alpha,
      objective = "binary:logistic",
      base_score = outcomeProportion,
      eval_metric = "auc", # TODO make this flexible?
      nthread = settings$threads
    ),
    nrounds = hyperParameters$ntrees,
    print_every_n = 10,
    early_stopping_rounds = hyperParameters$earlyStopRound,
    maximize = TRUE
  )
  trainArgs[[evalsArgument]] <- evals
  model <- do.call(xgboost::xgb.train, trainArgs)

  return(model)
}
