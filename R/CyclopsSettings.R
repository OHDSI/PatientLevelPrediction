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
#' @param priorCoefs A data frame with `covariateIds` and `betas` from a previous
#' model. Numeric and character covariate IDs are matched by value. Supply IDs at
#' or above `2^53` as full integer strings. Coefficients must use the same covariate
#' scaling as the training data.
#' Changes from these coefficients are penalized; the intercept is estimated
#' separately and any supplied intercept is ignored. `includeCovariateIds` also
#' applies to these coefficients. Source coefficients for covariates absent from
#' the target training data are dropped, and their count is logged.
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
#' @param priorCoefs A data frame with `covariateIds` and `betas` from a previous
#' model. Numeric and character covariate IDs are matched by value. Supply IDs at
#' or above `2^53` as full integer strings. Coefficients must use the same covariate
#' scaling as the training data.
#' Changes from these coefficients are penalized; the intercept is estimated
#' separately and any supplied intercept is ignored. `includeCovariateIds` also
#' applies to these coefficients. Source coefficients for covariates absent from
#' the target training data are dropped, and their count is logged.
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
#' @details
#' The model predicts the risk of the outcome by the end of the time-at-risk window,
#' that is riskWindowEnd - riskWindowStart + 1 days after the window starts. Both ends
#' of the window therefore need the same anchor (both cohort start or both cohort end);
#' runPlp() stops otherwise. predictPlp() can predict at an earlier time with its
#' timepoint argument.
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
#' @description
#' Logistic regression with `K` non-zero coefficients, fitted with the
#' IterativeHardThresholding package (version 2.0.0 or later). It maximizes the
#' log-likelihood minus a small ridge penalty (`variance`) over models with
#' `K` covariates. When `K` or `variance` has several values, the
#' hyperparameter search chooses between them on the training folds, as for
#' other models, by the held-out log-likelihood unless the hyperparameter
#' settings name another `tuningMetric`. Each fold's training rows are
#' converted to Cyclops data once, and each fold starts a candidate from its
#' own fit at the nearest earlier one (the next smaller `K`). The `foldWorkers`
#' of [createHyperparameterSettings()] fit the folds in parallel.
#'
#' @param K              Number of covariates in the model (default 25, or 10
#'   with `algorithm = "legacy"`, as in earlier versions); the intercept and
#'   `exclude` do not count. With fewer covariates than `K`, the model has all
#'   of them. One whole number, or several to
#'   choose from by cross-validation, for example `c(5, 10, 25, 50, 100)`,
#'   which takes several times longer to fit.
#' @param variance       Ridge variance (default 1): a positive number, `Inf` for plain L0
#'   (the best subset of size `K`), several numbers to choose from by
#'   cross-validation, or `"tune"` for the grid 0.01, 0.03, 0.1, 0.3, 1, 3 and
#'   `Inf`. In earlier versions the second argument was `penalty`, so give
#'   `variance` by name; given by position, it gives a warning.
#' @param exclude        Covariate ids (numbers) that are not penalized and do
#'   not count towards `K`.
#' @param forceIntercept Logical: penalize the intercept and count it towards `K`.
#' @param seed           An option to add a seed when training the model.
#' @param algorithm      `"default"`, `"chu2020"` (the algorithm of Chu et al.,
#'   2020) or `"legacy"` (IterativeHardThresholding 1.0.3: Broken Adaptive Ridge
#'   with at most `K` covariates, counting the intercept; deprecated, to
#'   reproduce earlier models).
#' @param control        A named list of algorithm switches; see
#'   `IterativeHardThresholding::createIhtControl()`. Empty entries take the
#'   defaults of `algorithm`.
#' @param oneStandardError With several `K`: choose the smallest `K` whose
#'   held-out log-likelihood is within one standard error of the best. This
#'   gives smaller models, which can discriminate less well than the best `K`.
#' @param threads        Kept for earlier code; not used. The refits run on
#'   `control$threads` threads (default 1); to fit the folds in parallel, use
#'   `createHyperparameterSettings(foldWorkers = )`.
#' @param ...            Only with `algorithm = "legacy"`: the arguments of
#'   IterativeHardThresholding 1.0.3 (`penalty`, `fitBestSubset`,
#'   `initialRidgeVariance`, `tolerance`, `maxIterations`, `threshold`, `delta`).
#'
#' @return `modelSettings` object
#'
#' @references
#' Chu BB, Keys KL, German CA, Zhou H, Zhou JJ, Sobel EM, Sinsheimer JS, Lange K
#' (2020). Iterative hard thresholding in genome-wide association studies:
#' Generalized linear models, prior weights, and double sparsity. GigaScience
#' 9(6), giaa044.
#'
#' @examplesIf rlang::is_installed("IterativeHardThresholding", version = "2.0.0")
#' modelIht <- setIterativeHardThresholding(K = 5, seed = 42)
#' # choose K by cross-validation
#' modelIht <- setIterativeHardThresholding(K = c(10, 25, 50), seed = 42)
#' @export
setIterativeHardThresholding <- function(
    K = 25,
    variance = 1,
    exclude = c(),
    forceIntercept = FALSE,
    seed = NULL,
    algorithm = "default",
    control = list(),
    oneStandardError = FALSE,
    threads = -1,
    ...) {
  rlang::check_installed("IterativeHardThresholding", version = "2.0.0")
  algorithm <- match.arg(algorithm, c("default", "chu2020", "legacy"))
  callArguments <- as.list(sys.call())[-1]
  argumentNames <- names(callArguments) %||% rep("", length(callArguments))
  if (algorithm != "legacy" && sum(argumentNames == "") >= 2) {
    warning(
      "The second argument of setIterativeHardThresholding() is now variance (the ridge variance), ",
      "not penalty as in earlier versions; give it by name",
      call. = FALSE
    )
  }
  if (algorithm == "legacy" && missing(K)) {
    # the default of earlier versions, so that a legacy model is reproduced
    K <- 10
  }
  legacyArguments <- list(...)
  legacyNames <- c(
    "penalty", "fitBestSubset", "initialRidgeVariance", "tolerance",
    "maxIterations", "threshold", "delta"
  )
  if (length(legacyArguments) > 0 &&
      (is.null(names(legacyArguments)) || any(!names(legacyArguments) %in% legacyNames))) {
    stop("Unknown arguments; see ?setIterativeHardThresholding", call. = FALSE)
  }
  if (algorithm == "legacy" &&
      (!missing(variance) || length(control) > 0 || !missing(oneStandardError))) {
    stop("variance, control and oneStandardError are not used with algorithm = \"legacy\"", call. = FALSE)
  }
  if (length(legacyArguments) > 0 && algorithm != "legacy") {
    stop(
      paste(names(legacyArguments), collapse = ", "),
      " belong to IterativeHardThresholding 1.0.3; use algorithm = \"legacy\" ",
      "to reproduce models fitted with it",
      call. = FALSE
    )
  }
  if (!is.numeric(K) || length(K) < 1 || any(!is.finite(K)) || any(K < 1) || any(K != round(K)) ||
      any(K > .Machine$integer.max)) {
    stop("K must be one or more whole numbers of at least 1", call. = FALSE)
  }
  if (algorithm == "legacy" && length(K) != 1) {
    stop("algorithm = \"legacy\" takes one K", call. = FALSE)
  }
  if (!is.logical(forceIntercept) || length(forceIntercept) != 1 || is.na(forceIntercept)) {
    stop("forceIntercept must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.logical(oneStandardError) || length(oneStandardError) != 1 || is.na(oneStandardError)) {
    stop("oneStandardError must be TRUE or FALSE", call. = FALSE)
  }
  if (algorithm != "legacy" && length(exclude) > 0 && (!is.numeric(exclude) || anyNA(exclude))) {
    stop(
      "exclude must be covariate ids (numbers); the intercept is not penalized ",
      "unless forceIntercept = TRUE",
      call. = FALSE
    )
  }
  if (!is.list(control) || (length(control) > 0 &&
      (is.null(names(control)) || any(names(control) == "")))) {
    stop("control must be a named list", call. = FALSE)
  }
  checkIsClass(seed, c("numeric", "NULL", "integer"))
  if (is.null(seed[1])) {
    seed <- as.integer(sample(100000000, 1))
  }
  checkIsClass(threads, c("numeric", "integer"))

  if (algorithm == "legacy") {
    priorParams <- c(
      list(K = K, exclude = exclude, forceIntercept = forceIntercept, algorithm = "legacy"),
      legacyArguments
    )
    # IterativeHardThresholding checks the values
    suppressWarnings(createIhtPriorFromSettings(priorParams))
  } else {
    # tuned by the generic hyperparameter search: a grid with K varying
    # fastest, so that each fold starts K from its fit at the next smaller K
    varianceGrid <- if (identical(variance, "tune")) {
      ihtTuneVarianceGrid
    } else {
      if (!(is.numeric(variance) || (is.character(variance) && all(variance == "Inf"))) ||
          length(variance) < 1 || anyNA(variance)) {
        stop("variance must be one or more positive numbers, Inf, or \"tune\"", call. = FALSE)
      }
      sort(unique(as.numeric(variance)))
    }
    # IterativeHardThresholding checks the values, one variance at a time
    for (v in varianceGrid) {
      createIhtPriorFromSettings(list(
        K = K[1], variance = v, exclude = exclude, forceIntercept = forceIntercept,
        algorithm = algorithm, control = control
      ))
    }
  }

  if (algorithm != "legacy") {
    settings <- list(
      modelType = "binary",
      modelName = "iterativeHardThresholding",
      seed = seed[1],
      threads = threads[1],
      prepareData = "prepareIhtMatrix",
      prepareFold = "prepareIhtFold",
      train = "fitIhtModel",
      predict = "predictIht",
      updateState = "updateIhtState",
      fitInfo = "ihtFitInfo",
      variableImportance = "ihtVariableImportance",
      parallelFolds = c("fork", "psock"),
      tuningMetric = "logLikelihoodMetric",
      saveType = "RtoJson",
      priorParams = list(
        exclude = exclude,
        forceIntercept = forceIntercept,
        algorithm = algorithm,
        control = control
      )
    )
    if (isTRUE(oneStandardError)) {
      settings$selector <- "ihtOneStandardErrorSelector"
    }
    result <- list(
      param = list(K = sort(unique(as.integer(K))), variance = varianceGrid),
      settings = settings
    )
    class(result) <- "modelSettings"
    return(result)
  }

  settings <- list(
    modelType = "binary",
    modelName = "iterativeHardThresholding",
    cyclopsModelType = "logistic",
    priorfunction = "IterativeHardThresholding::createIhtPrior",
    crossValidationInPrior = FALSE,
    addIntercept = TRUE,
    # the legacy algorithm runs without PLP's control, as in earlier versions
    useControl = FALSE,
    seed = seed[1],
    threads = threads[1],
    predict = "predictCyclops",
    saveType = "RtoJson"
  )

  result <- list(
    fitFunction = "fitCyclopsModel",
    param = list(priorParams = priorParams),
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
#' out-of-fold AUC. Automatic penalty tuning fits one model per fold and grid
#' value, in addition to the final model and cross-validation refits, so it can
#' be substantially slower than using a fixed penalty.
#'
#' @param initialRidgeVariance Numeric prior starting variance, or `"auto"` to
#'   estimate this using ridge cross-validation.
#' @param seed An option to add a seed when training the model.
#' @param includeCovariateIds A set of covariateIds to limit the analysis to.
#' @param noShrinkage A set of covariates which are forced into the model. The
#'   default is the intercept. This takes precedence over `forceIntercept`: a
#'   covariate listed here is excluded from the prior even when
#'   `forceIntercept = TRUE`.
#' @param penalty Numeric BAR penalty, `"bic"` to use `log(n) / 2`, or `"auto"`
#'   to cross-validate over a penalty grid.
#' @param penaltyRatio Minimum penalty in the automatic grid as a ratio of the
#'   `log(n) / 2` starting penalty.
#' @param penaltyGridSize Number of penalties to evaluate when `penalty = "auto"`.
#' @param threads An option to set number of threads when training model.
#' @param forceIntercept Logical: Include the intercept coefficient in the prior,
#'   unless it is listed in `noShrinkage`. To penalize the intercept, set this to
#'   `TRUE` and remove the intercept from `noShrinkage`.
#' @param upperLimit Numeric: Upper prior variance limit for grid-search.
#' @param lowerLimit Numeric: Lower prior variance limit for grid-search.
#' @param tolerance Numeric convergence tolerance passed to both Cyclops and BAR.
#'   In Cyclops, this controls the maximum relative change in its convergence
#'   criterion; in BAR, it controls the maximum absolute coefficient change
#'   between outer iterations.
#' @param maxIterations Integer maximum iteration count passed to both the Cyclops
#'   optimizer and the BAR outer loop.
#' @param threshold Numeric BAR threshold.
#'
#' @return `modelSettings` object
#'
#' @references
#' Fridgeirsson EA, Williams R, Rijnbeek P, Suchard MA, Reps JM. Comparing
#' penalization methods for linear models on large observational health data.
#' Journal of the American Medical Informatics Association. 2024;31(7):1514-1521.
#' \doi{10.1093/jamia/ocae109}
#'
#' Li N, Peng X, Kawaguchi E, Suchard MA, Li G. A scalable surrogate L0 sparse
#' regression method for generalized linear models with applications to large
#' scale data. Journal of Statistical Planning and Inference. 2021;213:262-281.
#' \doi{10.1016/j.jspi.2020.12.001}
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
    threshold = 1e-06) {
  rlang::check_installed("BrokenAdaptiveRidge")

  checkIsClass(seed, c("numeric", "NULL", "integer"))
  if (is.null(seed[1])) {
    seed <- as.integer(sample(100000000, 1))
  } else {
    checkIsWholeNumber(seed)
  }
  checkIsClass(threads, c("numeric", "integer"))
  checkIsWholeNumber(threads)
  if (threads != -1 && threads < 1) {
    stop("threads must be -1 or a positive whole number")
  }
  checkIsClass(initialRidgeVariance, c("numeric", "integer", "character"))
  if (length(initialRidgeVariance) != 1) {
    stop("initialRidgeVariance must be a single value")
  }
  if (inherits(initialRidgeVariance, "character")) {
    checkInStringVector(initialRidgeVariance, "auto")
  } else {
    checkSingleFiniteNumeric(initialRidgeVariance)
    checkHigher(initialRidgeVariance, 0)
  }
  checkIsClass(penalty, c("numeric", "integer", "character"))
  if (length(penalty) != 1) {
    stop("penalty must be a single value")
  }
  if (inherits(penalty, "character")) {
    checkInStringVector(penalty, c("auto", "bic"))
  } else {
    checkSingleFiniteNumeric(penalty)
    checkHigher(penalty, 0)
  }
  checkIsClass(penaltyRatio, c("numeric", "integer"))
  checkSingleFiniteNumeric(penaltyRatio)
  if (penaltyRatio <= 0 || penaltyRatio >= 1) {
    stop("penaltyRatio must be a single value greater than 0 and less than 1")
  }
  checkIsClass(penaltyGridSize, c("numeric", "integer"))
  checkIsWholeNumber(penaltyGridSize)
  checkHigher(penaltyGridSize, 0)
  checkIsClass(lowerLimit, c("numeric", "integer"))
  checkIsClass(upperLimit, c("numeric", "integer"))
  checkSingleFiniteNumeric(lowerLimit)
  checkSingleFiniteNumeric(upperLimit)
  checkHigherEqual(upperLimit, lowerLimit)
  if (length(forceIntercept) != 1 || !is.logical(forceIntercept) || is.na(forceIntercept)) {
    stop("forceIntercept must be a single non-missing logical value")
  }
  checkIsClass(tolerance, c("numeric", "integer"))
  checkSingleFiniteNumeric(tolerance)
  checkHigher(tolerance, 0)
  checkIsClass(maxIterations, c("numeric", "integer"))
  checkIsWholeNumber(maxIterations)
  checkHigher(maxIterations, 0)
  checkIsClass(threshold, c("numeric", "integer"))
  checkSingleFiniteNumeric(threshold)
  checkHigher(threshold, 0)

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
    priorfunction = "BrokenAdaptiveRidge::createBarPrior",
    crossValidationInPrior = FALSE,
    addIntercept = TRUE,
    useControl = !identical(penalty, "auto"),
    manualPenaltyCv = identical(penalty, "auto"),
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
