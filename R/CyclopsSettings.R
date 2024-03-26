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
    modelName = "lassoLogisticRegression",
    modelType = "binary",
    cyclopsModelType = "logistic",
    priorfunction = "Cyclops::createPrior",
    selectorType = "byPid", # is this correct?
    crossValidationInPrior = TRUE,
    addIntercept = TRUE,
    useControl = TRUE,
    seed = seed[1],
    threads = threads[1],
    tolerance = tolerance[1],
    cvRepetitions = 1,
    maxIterations = maxIterations[1],
    saveType = "RtoJson",
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


#' Create modelSettings for ridge logistic regression
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
#' modelRidge <- setRidgeRegression(seed = 42)
#' @export
setRidgeRegression <- function(
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
      priorType = "normal",
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
    modelName = "ridgeLogisticRegression",
    modelType = "binary",
    cyclopsModelType = "logistic",
    priorfunction = "Cyclops::createPrior",
    selectorType = "byPid", # is this correct?
    crossValidationInPrior = TRUE,
    addIntercept = TRUE,
    useControl = TRUE,
    seed = seed[1],
    threads = threads[1],
    tolerance = tolerance[1],
    cvRepetitions = 1,
    maxIterations = maxIterations[1],
    saveType = "RtoJson",
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
    cyclopsModelType = "cox",
    modelType = "survival",
    modelName = "coxLasso",
    priorfunction = "Cyclops::createPrior",
    selectorType = "byRow",
    crossValidationInPrior = TRUE,
    addIntercept = FALSE,
    useControl = TRUE,
    seed = seed[1],
    threads = threads[1],
    tolerance = tolerance[1],
    cvRepetitions = 1,
    maxIterations = maxIterations[1],
    saveType = "RtoJson",
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
    modelType = "binary",
    modelName = "iterativeHardThresholding",
    cyclopsModelType = "logistic",
    priorfunction = "IterativeHardThresholding::createIhtPrior",
    selectorType = "byRow",
    crossValidationInPrior = FALSE,
    addIntercept = TRUE,
    useControl = FALSE,
    seed = seed[1],
    predict = "predictCyclops",
    saveType = "RtoJson"
  )

  result <- list(
    fitFunction = "fitCyclopsModel",
    param = param,
    settings = settings
  )
  class(result) <- "modelSettings"

  return(result)
}


#' Create setting for Broken Adaptive Ridge logistic regression
#'
#' @description
#' Creates model settings for Broken Adaptive Ridge logistic regression using
#' Cyclops and the BrokenAdaptiveRidge prior. `initialRidgeVariance = "auto"`
#' first fits a ridge model with Cyclops cross-validation and uses the selected
#' ridge variance to initialize BAR. `penalty = "auto"` cross-validates over a
#' BAR penalty grid and refits using the penalty with the highest mean
#' out-of-fold AUC.
#'
#' @param initialRidgeVariance Numeric prior starting variance, or `"auto"` to
#'   estimate this using ridge cross-validation.
#' @param seed An option to add a seed when training the model.
#' @param includeCovariateIds A set of covariateIds to limit the analysis to.
#' @param noShrinkage A set of covariates which are forced into the model. The
#'   default is the intercept.
#' @param penalty Numeric BAR penalty, `"logN"` to use `log(n) / 2`, or `"auto"`
#'   to cross-validate over a penalty grid.
#' @param penaltyRatio Minimum penalty in the automatic grid as a ratio of the
#'   `log(n) / 2` starting penalty.
#' @param penaltyGridSize Number of penalties to evaluate when `penalty = "auto"`.
#' @param threads An option to set number of threads when training model.
#' @param forceIntercept Logical: Force intercept coefficient into prior.
#' @param upperLimit Numeric: Upper prior variance limit for grid-search.
#' @param lowerLimit Numeric: Lower prior variance limit for grid-search.
#' @param tolerance Numeric: maximum relative change in convergence criterion
#'   from successive iterations to achieve convergence.
#' @param maxIterations Integer: maximum iterations of Cyclops to attempt before
#'   returning a failed-to-converge error.
#' @param threshold Numeric BAR threshold.
#' @param prior Which BAR prior implementation to use: `"regular"` or `"fast"`.
#'
#' @return `modelSettings` object
#'
#' @examplesIf rlang::is_installed("BrokenAdaptiveRidge")
#' modelBar <- setBrokenAdaptiveRidge(seed = 42)
#' @export
setBrokenAdaptiveRidge <- function(
    initialRidgeVariance = "auto",
    seed = NULL,
    includeCovariateIds = c(),
    noShrinkage = c("(Intercept)"),
    penalty = "auto",
    penaltyRatio = 0.1,
    penaltyGridSize = 10,
    threads = -1,
    forceIntercept = FALSE,
    upperLimit = 20,
    lowerLimit = 0.01,
    tolerance = 2e-06,
    maxIterations = 3000,
    threshold = 1e-06,
    prior = "regular") {
  rlang::check_installed("BrokenAdaptiveRidge")

  checkIsClass(seed, c("numeric", "NULL", "integer"))
  if (is.null(seed[1])) {
    seed <- as.integer(sample(100000000, 1))
  }
  checkIsClass(threads, c("numeric", "integer"))
  checkIsClass(initialRidgeVariance, c("numeric", "integer", "character"))
  if (length(initialRidgeVariance) != 1) {
    stop("initialRidgeVariance must be a single value")
  }
  if (inherits(initialRidgeVariance, "character") && !identical(initialRidgeVariance, "auto")) {
    stop('initialRidgeVariance must be numeric or "auto"')
  }
  checkIsClass(penalty, c("numeric", "integer", "character"))
  if (length(penalty) != 1) {
    stop("penalty must be a single value")
  }
  if (inherits(penalty, "character") && !penalty %in% c("auto", "logN")) {
    stop('penalty must be numeric, "auto", or "logN"')
  }
  checkIsClass(penaltyRatio, c("numeric", "integer"))
  checkHigherEqual(penaltyRatio, 0)
  if (length(penaltyRatio) != 1 || penaltyRatio <= 0 || penaltyRatio >= 1) {
    stop("penaltyRatio must be a single value greater than 0 and less than 1")
  }
  checkIsClass(penaltyGridSize, c("numeric", "integer"))
  if (length(penaltyGridSize) != 1 || penaltyGridSize < 1 || penaltyGridSize != floor(penaltyGridSize)) {
    stop("penaltyGridSize must be a single positive whole number")
  }
  checkIsClass(lowerLimit, c("numeric", "integer"))
  checkIsClass(upperLimit, c("numeric", "integer"))
  checkHigherEqual(upperLimit, lowerLimit)
  if (!is.logical(forceIntercept)) {
    stop("forceIntercept must be of type: logical")
  }

  prior <- tolower(prior[1])
  if (!prior %in% c("regular", "fast")) {
    stop('prior must be "regular" or "fast"')
  }
  priorFunction <- if (identical(prior, "fast")) {
    "BrokenAdaptiveRidge::createFastBarPrior"
  } else {
    "BrokenAdaptiveRidge::createBarPrior"
  }

  param <- list(
    priorParams = list(
      forceIntercept = forceIntercept,
      initialRidgeVariance = initialRidgeVariance,
      exclude = noShrinkage,
      tolerance = tolerance[1],
      maxIterations = maxIterations[1],
      penalty = penalty,
      threshold = threshold
    ),
    includeCovariateIds = includeCovariateIds,
    upperLimit = upperLimit,
    lowerLimit = lowerLimit
  )

  settings <- list(
    modelName = "brokenAdaptiveRidge",
    modelType = "binary",
    cyclopsModelType = "logistic",
    priorfunction = priorFunction,
    selectorType = "byPid",
    crossValidationInPrior = FALSE,
    addIntercept = TRUE,
    useControl = !identical(penalty, "auto"),
    manualPenaltyCv = identical(penalty, "auto"),
    manualPenaltyCvWarmStart = TRUE,
    penaltyRatio = penaltyRatio[1],
    penaltyGridSize = penaltyGridSize[1],
    seed = seed[1],
    threads = threads[1],
    tolerance = tolerance[1],
    cvRepetitions = 1,
    maxIterations = maxIterations[1],
    saveType = "RtoJson",
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
