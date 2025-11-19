# @file CyclopsSettings.R
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

#' Create modelSettings for lasso logistic regression
#'
#' @param variance   	Numeric: prior distribution starting variance
#' @param seed       An option to add a seed when training the model
#' @param includeCovariateIds a set of covariateIds to limit the analysis to
#' @param noShrinkage a set of covariates whcih are to be forced to be included in
#' in the final model. Default is the intercept
#' @param threads    An option to set number of threads when training model.
#' @param forceIntercept  	Logical: Force intercept coefficient into prior
#' @param upperLimit  	Numeric: Upper prior variance limit for grid-search
#' @param lowerLimit  	Numeric: Lower prior variance limit for grid-search
#' @param tolerance   Numeric: maximum relative change in convergence criterion from
#' from successive iterations to achieve convergence
#' @param maxIterations 	Integer: maximum iterations of Cyclops to attempt 
#' before returning a failed-to-converge error
#' @param priorCoefs    Use coefficients from a previous model as starting 
#' points for model fit (transfer learning)
#'
#' @return `modelSettings` object
#'
#' @examples
#' modelLasso <- setLassoLogisticRegression(seed=42)
#' @export
setLassoLogisticRegression <- function(
    variance = 0.01,
    seed = NULL,
    includeCovariateIds = c(),
    noShrinkage = c(0),
    threads = -1,
    forceIntercept = FALSE,
    upperLimit = 20,
    lowerLimit = 0.01,
    tolerance = 2e-06,
    maxIterations = 3000,
    priorCoefs = NULL) {
  checkIsClass(seed, c("numeric", "NULL", "integer"))
  if (is.null(seed[1])) {
    seed <- as.integer(sample(100000000, 1))
  }
  checkIsClass(threads, c("numeric", "integer"))
  checkIsClass(variance, c("numeric", "integer"))
  checkHigherEqual(variance, 0)

  checkIsClass(lowerLimit, c("numeric", "integer"))
  checkIsClass(upperLimit, c("numeric", "integer"))

  checkHigherEqual(upperLimit, lowerLimit)

  param <- list(
    priorParams = list(
      priorType = "laplace",
      forceIntercept = forceIntercept,
      variance = variance,
      exclude = noShrinkage
    ),
    includeCovariateIds = includeCovariateIds,
    upperLimit = upperLimit,
    lowerLimit = lowerLimit,
    priorCoefs = priorCoefs
  )

  settings <- list(
    priorfunction = "Cyclops::createPrior",
    selectorType = "byPid", # is this correct?
    crossValidationInPrior = TRUE,
    cyclopsModelType = "logistic",
    addIntercept = TRUE,
    useControl = TRUE,
    seed = seed[1],
    modelName = "Lasso Logistic Regression",
    threads = threads[1],
    tolerance = tolerance[1],
    cvRepetitions = 1,
    maxIterations = maxIterations[1],
    saveType = "RtoJson",
    modelType = "binary",
    predict = "predictCyclops"
  )

  result <- list(
    fitFunction = "fitCyclopsModel",
    param = param,
    settings = settings
  )
  class(result) <- "modelSettings"

  return(result)
}



#' Create setting for lasso Cox model
#'
#' @param variance   	Numeric: prior distribution starting variance
#' @param seed       An option to add a seed when training the model
#' @param includeCovariateIds a set of covariate IDS to limit the analysis to
#' @param noShrinkage a set of covariates whcih are to be forced to be included in the final model. default is the intercept
#' @param threads    An option to set number of threads when training model
#' @param upperLimit  	Numeric: Upper prior variance limit for grid-search
#' @param lowerLimit  	Numeric: Lower prior variance limit for grid-search
#' @param tolerance   Numeric: maximum relative change in convergence criterion from successive iterations to achieve convergence
#' @param maxIterations 	Integer: maximum iterations of Cyclops to attempt before returning a failed-to-converge error
#'
#' @return `modelSettings` object
#' @examples
#' coxL1 <- setCoxModel()
#' @export
setCoxModel <- function(
    variance = 0.01,
    seed = NULL,
    includeCovariateIds = c(),
    noShrinkage = c(),
    threads = -1,
    upperLimit = 20,
    lowerLimit = 0.01,
    tolerance = 2e-07,
    maxIterations = 3000) {

  checkSurvivalPackages()
  checkIsClass(seed, c("numeric", "NULL", "integer"))
  if (is.null(seed[1])) {
    seed <- as.integer(sample(100000000, 1))
  }
  checkIsClass(threads, c("numeric", "integer"))
  checkIsClass(variance, c("numeric", "integer"))
  checkHigherEqual(variance, 0)

  checkIsClass(lowerLimit, c("numeric", "integer"))
  checkIsClass(upperLimit, c("numeric", "integer"))

  checkHigherEqual(upperLimit, lowerLimit)

  param <- list(
    priorParams = list(
      priorType = "laplace",
      variance = variance,
      exclude = noShrinkage
    ),
    includeCovariateIds = includeCovariateIds,
    upperLimit = upperLimit,
    lowerLimit = lowerLimit
  )

  settings <- list(
    priorfunction = "Cyclops::createPrior",
    selectorType = "byRow",
    crossValidationInPrior = TRUE,
    cyclopsModelType = "cox",
    addIntercept = FALSE,
    useControl = TRUE,
    seed = seed[1],
    modelName = "LASSO Cox Regression",
    threads = threads[1],
    tolerance = tolerance[1],
    cvRepetitions = 1,
    maxIterations = maxIterations[1],
    saveType = "RtoJson",
    predict = "predictCyclops",
    modelType = "survival"
  )

  result <- list(
    fitFunction = "fitCyclopsModel",
    param = param,
    settings = settings
  )
  class(result) <- "modelSettings"

  return(result)
}


#' Create setting for Iterative Hard Thresholding model
#'
#' @param K              The maximum number of non-zero predictors
#' @param penalty        Specifies the IHT penalty; possible values are `BIC` or `AIC` or a numeric value
#' @param seed           An option to add a seed when training the model
#' @param exclude        A vector of numbers or covariateId names to exclude from prior
#' @param forceIntercept Logical: Force intercept coefficient into regularization
#' @param fitBestSubset  Logical: Fit final subset with no regularization
#' @param initialRidgeVariance  integer
#' @param tolerance      numeric
#' @param maxIterations  integer
#' @param threshold      numeric
#' @param delta          numeric
#' 
#' @return `modelSettings` object
#'
#' @examplesIf rlang::is_installed("IterativeHardThresholding")
#' modelIht <- setIterativeHardThresholding(K = 5, seed = 42)
#' @export
setIterativeHardThresholding <- function(
    K = 10,
    penalty = "bic",
    seed = sample(100000, 1),
    exclude = c(),
    forceIntercept = FALSE,
    fitBestSubset = FALSE,
    initialRidgeVariance = 0.1,
    tolerance = 1e-08,
    maxIterations = 10000,
    threshold = 1e-06,
    delta = 0) {
  rlang::check_installed("IterativeHardThresholding")

  if (K < 1) {
    stop("Invalid maximum number of predictors")
  }
  if (!(penalty %in% c("aic", "bic") || is.numeric(penalty))) {
    stop('Penalty must be "aic", "bic" or numeric')
  }
  if (!is.logical(forceIntercept)) {
    stop("forceIntercept must be of type: logical")
  }
  if (!is.logical(fitBestSubset)) {
    stop("fitBestSubset must be of type: logical")
  }
  if (!inherits(x = seed, what = c("numeric", "NULL", "integer"))) {
    stop("Invalid seed")
  }


  # set seed
  if (is.null(seed[1])) {
    seed <- as.integer(sample(100000000, 1))
  }

  param <- list(
    priorParams = list(
      K = K,
      penalty = penalty,
      exclude = exclude,
      forceIntercept = forceIntercept,
      fitBestSubset = fitBestSubset,
      initialRidgeVariance = initialRidgeVariance,
      tolerance = tolerance[1],
      maxIterations = maxIterations[1],
      threshold = threshold,
      delta = delta
    )
  )

  settings <- list(
    priorfunction = "IterativeHardThresholding::createIhtPrior",
    selectorType = "byRow",
    crossValidationInPrior = FALSE,
    cyclopsModelType = "logistic",
    addIntercept = FALSE,
    useControl = FALSE,
    seed = seed[1],
    modelName = "Iterative Hard Thresholding",
    predict = "predictCyclops",
    saveType = "RtoJson",
    modelType = "binary"
  )

  result <- list(
    fitFunction = "fitCyclopsModel",
    param = param,
    settings = settings
  )
  class(result) <- "modelSettings"

  return(result)
}
