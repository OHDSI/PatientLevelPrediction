# @file Imputation.R
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

#' @title Create Iterative Imputer settings
#' @description This function creates the settings for an iterative imputer
#' which first removes features with more than `missingThreshold` missing values
#' and then imputes the missing values iteratively using chained equations
#' @param missingThreshold The threshold for missing values to remove a feature
#' @param method The method to use for imputation, currently only "pmm" is supported
#' @param methodSettings A list of settings for the imputation method to use.
#' Currently only "pmm" is supported with the following settings:
#' - k: The number of donors to use for matching
#' - iterations: The number of iterations to use for imputation
#' - alpha: Elastic-net mixing parameter for PMM fit (`1` = lasso, `0` = ridge)
#' - seed: The seed of the random draws of the imputation (the cross-validation
#'   folds of each imputation model and the donor sampling), drawn at random when
#'   not given. The imputations then depend only on the settings and the data.
#' @param addMissingIndicator Add a binary missingness indicator per feature that
#' passes the imputation missingness threshold.
#' @return The settings for the iterative imputer of class `featureEngineeringSettings`
#' @examples
#' # create imputer to impute values with missingness less than 30% using 
#' # predictive mean matching in 5 iterations with 5 donors
#' createIterativeImputer(missingThreshold = 0.3, method = "pmm",
#'                        methodSettings = list(
#'                          pmm = list(k = 5, iterations = 5, alpha = 0.5)
#'                        ))
#' @export
createIterativeImputer <- function(missingThreshold = 0.3,
                                   method = "pmm",
                                   methodSettings = list(
                                      pmm = list(
                                        k = 5,
                                        iterations = 5,
                                        alpha = 1
                                      )
                                   ),
                                   addMissingIndicator = FALSE) {
  ParallelLogger::logWarn("Imputation is experimental and may have bugs. 
    Please report any issues on the GitHub repository.")
  checkIsClass(missingThreshold, "numeric")
  checkInStringVector(method, c("pmm"))
  checkIsClass(methodSettings, "list")
  checkIsClass(addMissingIndicator, "logical")
  if (length(addMissingIndicator) != 1) {
    stop("addMissingIndicator should be a logical value")
  }
  if (method == "pmm") {
    checkIsClass(methodSettings$pmm$k, "numeric")
    checkHigher(methodSettings$pmm$k, 0)
    checkIsClass(methodSettings$pmm$iterations, "numeric")
    checkHigher(methodSettings$pmm$iterations, 0)
    if (is.null(methodSettings$pmm$alpha)) {
      methodSettings$pmm$alpha <- 1
    }
    checkIsClass(methodSettings$pmm$alpha, "numeric")
    if (length(methodSettings$pmm$alpha) != 1 ||
      methodSettings$pmm$alpha < 0 ||
      methodSettings$pmm$alpha > 1) {
      stop("methodSettings$pmm$alpha should be a single numeric value in [0, 1]")
    }
    if (is.null(methodSettings$pmm$seed)) {
      methodSettings$pmm$seed <- sample(100000, 1)
    }
    checkIsClass(methodSettings$pmm$seed, c("numeric", "integer"))
    if (length(methodSettings$pmm$seed) != 1) {
      stop("methodSettings$pmm$seed should be a single number")
    }
  }
  checkHigher(missingThreshold, 0)
  checkLower(missingThreshold, 1)
  featureEngineeringSettings <- list(
    missingThreshold = missingThreshold,
    method = method,
    methodSettings = methodSettings[[method]],
    addMissingIndicator = addMissingIndicator
  )
  if (method == "pmm") {
    # at the moment this requires glmnet
    rlang::check_installed("glmnet")
  }
  attr(featureEngineeringSettings, "fun") <- "iterativeImpute"

  class(featureEngineeringSettings) <- "featureEngineeringSettings"
  return(featureEngineeringSettings)
}

#' @title Create Simple Imputer settings
#' @description This function creates the settings for a simple imputer
#' which imputes missing values with the mean or median
#' @param method The method to use for imputation, either "mean" or "median"
#' @param missingThreshold The threshold for missing values to be imputed vs removed
#' @param addMissingIndicator Add a binary missingness indicator per feature that
#' passes the imputation missingness threshold.
#' @return The settings for the single imputer of class `featureEngineeringSettings`
#' @examples
#' # create imputer to impute values with missingness less than 10% using the median
#' # of observed values
#' createSimpleImputer(method = "median", missingThreshold = 0.10)
#' @export
createSimpleImputer <- function(method = "mean",
                                missingThreshold = 0.3,
                                addMissingIndicator = FALSE) {
  ParallelLogger::logWarn("Imputation is experimental and may have bugs,
    please report any issues on the GitHub repository.")
  checkIsClass(method, "character")
  checkInStringVector(method, c("mean", "median"))
  checkIsClass(missingThreshold, "numeric")
  checkIsClass(addMissingIndicator, "logical")
  if (length(addMissingIndicator) != 1) {
    stop("addMissingIndicator should be a logical value")
  }
  checkHigher(missingThreshold, 0)
  checkLower(missingThreshold, 1)
  featureEngineeringSettings <- list(
    method = method,
    missingThreshold = missingThreshold,
    addMissingIndicator = addMissingIndicator
  )
  attr(featureEngineeringSettings, "fun") <- "simpleImpute"

  class(featureEngineeringSettings) <- "featureEngineeringSettings"
  return(featureEngineeringSettings)
}

defaultSklearnIterativeMethodSettings <- function() {
  list(
    maxIter = 10,
    tol = 1e-3,
    samplePosterior = FALSE,
    nNearestFeatures = NULL,
    initialStrategy = "mean",
    imputationOrder = "ascending",
    skipComplete = FALSE,
    randomState = 42,
    minValue = -Inf,
    maxValue = Inf
  )
}

normalizeSklearnIterativeMethodSettings <- function(methodSettings = list()) {
  if (is.null(methodSettings)) {
    methodSettings <- list()
  }
  checkIsClass(methodSettings, "list")
  defaults <- defaultSklearnIterativeMethodSettings()
  unknown <- setdiff(names(methodSettings), names(defaults))
  if (length(unknown) > 0) {
    stop(
      "Unknown methodSettings for sklearn iterative imputer: ",
      paste(unknown, collapse = ", ")
    )
  }
  settings <- utils::modifyList(defaults, methodSettings)

  checkIsClass(settings$maxIter, "numeric")
  if (length(settings$maxIter) != 1 || settings$maxIter < 1 ||
    settings$maxIter != as.integer(settings$maxIter)) {
    stop("methodSettings$maxIter should be a single integer >= 1")
  }
  settings$maxIter <- as.integer(settings$maxIter)

  checkIsClass(settings$tol, "numeric")
  if (length(settings$tol) != 1 || settings$tol <= 0 || !is.finite(settings$tol)) {
    stop("methodSettings$tol should be a single positive finite numeric value")
  }

  checkIsClass(settings$samplePosterior, "logical")
  if (length(settings$samplePosterior) != 1) {
    stop("methodSettings$samplePosterior should be a logical value")
  }

  if (!is.null(settings$nNearestFeatures)) {
    checkIsClass(settings$nNearestFeatures, "numeric")
    if (length(settings$nNearestFeatures) != 1 ||
      settings$nNearestFeatures < 1 ||
      settings$nNearestFeatures != as.integer(settings$nNearestFeatures)) {
      stop("methodSettings$nNearestFeatures should be NULL or a single integer >= 1")
    }
    settings$nNearestFeatures <- as.integer(settings$nNearestFeatures)
  }

  checkIsClass(settings$initialStrategy, "character")
  checkInStringVector(
    settings$initialStrategy,
    c("mean", "median", "most_frequent", "constant")
  )
  checkIsClass(settings$imputationOrder, "character")
  checkInStringVector(
    settings$imputationOrder,
    c("ascending", "descending", "roman", "arabic", "random")
  )

  checkIsClass(settings$skipComplete, "logical")
  if (length(settings$skipComplete) != 1) {
    stop("methodSettings$skipComplete should be a logical value")
  }

  if (!is.null(settings$randomState)) {
    checkIsClass(settings$randomState, "numeric")
    if (length(settings$randomState) != 1 ||
      settings$randomState != as.integer(settings$randomState)) {
      stop("methodSettings$randomState should be NULL or a single integer")
    }
    settings$randomState <- as.integer(settings$randomState)
  }

  checkIsClass(settings$minValue, "numeric")
  if (length(settings$minValue) != 1) {
    stop("methodSettings$minValue should be a single numeric value")
  }
  checkIsClass(settings$maxValue, "numeric")
  if (length(settings$maxValue) != 1) {
    stop("methodSettings$maxValue should be a single numeric value")
  }
  if (settings$minValue > settings$maxValue) {
    stop("methodSettings$minValue cannot be greater than methodSettings$maxValue")
  }

  settings
}

checkSklearnIterativeImputerDependencies <- function() {
  checkSklearn()
  tryCatch(
    {
      reticulate::import("sklearn.experimental.enable_iterative_imputer")
    },
    error = function(e) {
      stop(
        "This function requires sklearn.experimental.enable_iterative_imputer: ",
        conditionMessage(e)
      )
    }
  )
}

createSklearnIterativeImputerObject <- function(methodSettings) {
  checkSklearnIterativeImputerDependencies()
  sklearn <- reticulate::import("sklearn", convert = FALSE)

  args <- list(
    max_iter = as.integer(methodSettings$maxIter),
    tol = methodSettings$tol,
    sample_posterior = methodSettings$samplePosterior,
    n_nearest_features = if (is.null(methodSettings$nNearestFeatures)) NULL else as.integer(methodSettings$nNearestFeatures),
    initial_strategy = methodSettings$initialStrategy,
    imputation_order = methodSettings$imputationOrder,
    skip_complete = methodSettings$skipComplete,
    random_state = if (is.null(methodSettings$randomState)) NULL else as.integer(methodSettings$randomState),
    min_value = methodSettings$minValue,
    max_value = methodSettings$maxValue
  )

  do.call(sklearn$impute$IterativeImputer, args)
}

#' @title Create scikit-learn Iterative Imputer settings
#' @description This function creates settings for a dense iterative imputer
#' powered by scikit-learn's `IterativeImputer` through `reticulate`.
#' @details scikit-learn fits the imputer; at prediction its fitted steps (the
#' starting value of each feature and the coefficients of each BayesianRidge
#' model) are applied in R, so a saved model can be loaded and applied without
#' Python. With `samplePosterior = TRUE` the imputer draws random values, which
#' cannot be repeated in R, so such a model can only be applied in the R session
#' it was fitted in.
#' @param missingThreshold The threshold for missing values to remove a feature
#' @param methodSettings A list of settings for sklearn `IterativeImputer`.
#' Supported settings are:
#' - maxIter
#' - tol
#' - samplePosterior
#' - nNearestFeatures
#' - initialStrategy
#' - imputationOrder
#' - skipComplete
#' - randomState
#' - minValue
#' - maxValue
#' @param addMissingIndicator Add a binary missingness indicator per feature that
#' passes the imputation missingness threshold.
#' @return The settings for the sklearn iterative imputer of class `featureEngineeringSettings`
#' @examples
#' \dontshow{ # dontrun reason: requires python and scikit-learn }
#' \dontrun{
#' createSklearnIterativeImputer(
#'   missingThreshold = 0.3,
#'   methodSettings = list(maxIter = 5, nNearestFeatures = 20)
#' )
#' }
#' @export
createSklearnIterativeImputer <- function(
    missingThreshold = 0.3,
    methodSettings = list(),
    addMissingIndicator = FALSE) {
  ParallelLogger::logWarn("Imputation is experimental and may have bugs. 
    Please report any issues on the GitHub repository.")

  checkIsClass(missingThreshold, "numeric")
  checkHigher(missingThreshold, 0)
  checkLower(missingThreshold, 1)
  checkIsClass(addMissingIndicator, "logical")
  if (length(addMissingIndicator) != 1) {
    stop("addMissingIndicator should be a logical value")
  }

  methodSettings <- normalizeSklearnIterativeMethodSettings(methodSettings)
  checkSklearnIterativeImputerDependencies()

  featureEngineeringSettings <- list(
    missingThreshold = missingThreshold,
    method = "sklearnIterative",
    methodSettings = methodSettings,
    addMissingIndicator = addMissingIndicator
  )
  attr(featureEngineeringSettings, "fun") <- "sklearnIterativeImpute"
  class(featureEngineeringSettings) <- "featureEngineeringSettings"
  featureEngineeringSettings
}

sklearnIterativeImputerRuntime <- new.env(parent = emptyenv())

registerSklearnIterativeImputerRuntime <- function(imputer) {
  key <- paste0(
    "sklearn_iterative_",
    format(Sys.time(), "%Y%m%d%H%M%OS6"),
    "_",
    sample.int(1000000, 1)
  )
  assign(key, imputer, envir = sklearnIterativeImputerRuntime)
  key
}

getSklearnIterativeImputerRuntime <- function(key) {
  if (!(is.character(key) && length(key) == 1 && nzchar(key))) {
    return(NULL)
  }
  if (!exists(key, envir = sklearnIterativeImputerRuntime, inherits = FALSE)) {
    return(NULL)
  }
  get(key, envir = sklearnIterativeImputerRuntime, inherits = FALSE)
}

# The fitted scikit-learn IterativeImputer is a Python object, so it cannot be
# saved with the model. With the default samplePosterior = FALSE its transform is
# deterministic and needs only numbers: a starting value for each column, then
# for each step in order the column it imputes, the columns it uses, and the
# coefficients and intercept of its BayesianRidge model. Those are saved as the
# step list and applied in R at prediction. Returns NULL when the imputer cannot
# be replayed this way (samplePosterior = TRUE draws random values).
extractSklearnImputerSteps <- function(imputer, covariateIds, methodSettings) {
  if (isTRUE(methodSettings$samplePosterior)) {
    return(NULL)
  }
  sequence <- imputer$imputation_sequence_
  stepRows <- list()
  coefficientRows <- list()
  for (i in seq_len(reticulate::py_len(sequence))) {
    step <- reticulate::py_get_item(sequence, i - 1L)
    if (!inherits(step$estimator, "sklearn.linear_model._bayes.BayesianRidge")) {
      return(NULL)
    }
    predictors <- as.integer(reticulate::py_to_r(step$neighbor_feat_idx)) + 1L
    stepRows[[i]] <- data.frame(
      step = i,
      target = as.integer(reticulate::py_to_r(step$feat_idx)) + 1L,
      intercept = as.numeric(reticulate::py_to_r(step$estimator$intercept_))
    )
    coefficientRows[[i]] <- data.frame(
      step = rep(i, length(predictors)),
      predictor = predictors,
      coefficient = as.numeric(reticulate::py_to_r(step$estimator$coef_))
    )
  }
  emptySteps <- data.frame(step = integer(0), target = integer(0), intercept = numeric(0))
  emptyCoefficients <- data.frame(step = integer(0), predictor = integer(0), coefficient = numeric(0))
  list(
    covariateIds = covariateIds,
    initialValues = as.numeric(reticulate::py_to_r(imputer$initial_imputer_$statistics_)),
    nIterations = as.integer(reticulate::py_to_r(imputer$n_iter_)),
    minValue = methodSettings$minValue,
    maxValue = methodSettings$maxValue,
    steps = if (length(stepRows) > 0) do.call(rbind, stepRows) else emptySteps,
    coefficients = if (length(coefficientRows) > 0) do.call(rbind, coefficientRows) else emptyCoefficients
  )
}

# Does what IterativeImputer.transform() does with samplePosterior = FALSE: fill
# every missing value with its column's starting value, then run the steps in
# order, each replacing the missing values of its column with the clipped linear
# prediction from the current values of its predictor columns.
applySklearnImputerSteps <- function(matrixData, imputerSteps) {
  initialValues <- as.numeric(unlist(imputerSteps$initialValues))
  if (ncol(matrixData) != length(initialValues)) {
    stop("The saved sklearn imputer steps do not match the number of columns to impute.")
  }
  missing <- is.na(matrixData)
  for (j in seq_len(ncol(matrixData))) {
    matrixData[missing[, j], j] <- initialValues[j]
  }
  if (imputerSteps$nIterations == 0 || all(missing)) {
    return(matrixData)
  }
  steps <- restoreSavedTables(imputerSteps$steps)
  coefficients <- restoreSavedTables(imputerSteps$coefficients)
  minValue <- as.numeric(imputerSteps$minValue)
  maxValue <- as.numeric(imputerSteps$maxValue)
  for (i in seq_len(nrow(steps))) {
    target <- steps$target[i]
    rows <- missing[, target]
    if (!any(rows)) {
      next
    }
    stepCoefficients <- coefficients[coefficients$step == steps$step[i], , drop = FALSE]
    predicted <- matrixData[rows, stepCoefficients$predictor, drop = FALSE] %*%
      stepCoefficients$coefficient + steps$intercept[i]
    matrixData[rows, target] <- pmin(pmax(as.numeric(predicted), minValue), maxValue)
  }
  return(matrixData)
}

normalizeIdKey <- function(x) {
  trimws(as.character(x))
}

allocateIndicatorCovariateIds <- function(candidateIds, usedIds) {
  usedKeys <- normalizeIdKey(usedIds)
  candidateKeys <- normalizeIdKey(candidateIds)
  collides <- candidateKeys %in% usedKeys | duplicated(candidateKeys)
  usedKeys <- c(usedKeys, candidateKeys[!collides])
  for (i in which(collides)) {
    repeat {
      candidateIds[i] <- candidateIds[i] + 1000
      candidateKey <- normalizeIdKey(candidateIds[i])
      if (!candidateKey %in% usedKeys) {
        break
      }
    }
    usedKeys <- c(usedKeys, candidateKey)
  }
  candidateIds
}

createMissingIndicatorInfo <- function(outputData, sourceCovariateIds) {
  sourceCovariateIds <- unique(sourceCovariateIds)
  if (length(sourceCovariateIds) == 0) {
    return(list(
      map = data.frame(),
      covariateRef = data.frame(),
      analysisRef = data.frame()
    ))
  }

  sourceCovariateRef <- outputData$covariateData$covariateRef %>%
    dplyr::filter(.data$covariateId %in% !!sourceCovariateIds) %>%
    dplyr::select("covariateId", "covariateName", "analysisId", "conceptId") %>%
    dplyr::collect()

  if (nrow(sourceCovariateRef) == 0) {
    return(list(
      map = data.frame(),
      covariateRef = data.frame(),
      analysisRef = data.frame()
    ))
  }

  sourceAnalysisIds <- unique(sourceCovariateRef$analysisId)
  sourceAnalysisRef <- outputData$covariateData$analysisRef %>%
    dplyr::filter(.data$analysisId %in% !!sourceAnalysisIds) %>%
    dplyr::select("analysisId", "analysisName", "domainId", "startDay", "endDay") %>%
    dplyr::collect()

  analysisMap <- data.frame(
    sourceAnalysisId = sourceAnalysisIds,
    indicatorAnalysisId = NA_real_
  )
  usedAnalysisIds <- outputData$covariateData$analysisRef %>%
    dplyr::pull(.data$analysisId) %>%
    unique()
  usedAnalysisIdKeys <- normalizeIdKey(usedAnalysisIds)

  for (i in seq_len(nrow(analysisMap))) {
    candidate <- analysisMap$sourceAnalysisId[i] + 1000
    candidateKey <- normalizeIdKey(candidate)
    while (candidateKey %in% usedAnalysisIdKeys) {
      candidate <- candidate + 1000
      candidateKey <- normalizeIdKey(candidate)
    }
    analysisMap$indicatorAnalysisId[i] <- candidate
    usedAnalysisIds <- c(usedAnalysisIds, candidate)
    usedAnalysisIdKeys <- c(usedAnalysisIdKeys, candidateKey)
  }

  map <- sourceCovariateRef %>%
    dplyr::inner_join(
      analysisMap,
      by = c("analysisId" = "sourceAnalysisId")
    ) %>%
    dplyr::arrange(.data$covariateId) %>%
    dplyr::mutate(indicatorCovariateId = .data$covariateId -
      .data$analysisId + .data$indicatorAnalysisId)
  map$indicatorCovariateId <- allocateIndicatorCovariateIds(
    candidateIds = map$indicatorCovariateId,
    usedIds = c(
      outputData$covariateData$covariateRef %>%
        dplyr::pull(.data$covariateId),
      outputData$covariateData$covariates %>%
        dplyr::distinct(.data$covariateId) %>%
        dplyr::pull(.data$covariateId)
    )
  )

  indicatorCovariateRef <- map %>%
    dplyr::transmute(
      covariateId = .data$indicatorCovariateId,
      covariateName = paste0(.data$covariateName, " missing indicator"),
      conceptId = .data$conceptId,
      analysisId = .data$indicatorAnalysisId
    ) %>%
    dplyr::distinct()

  indicatorAnalysisRef <- analysisMap %>%
    dplyr::left_join(
      sourceAnalysisRef,
      by = c("sourceAnalysisId" = "analysisId")
    ) %>%
    dplyr::transmute(
      analysisId = .data$indicatorAnalysisId,
      analysisName = paste0(.data$analysisName, " missing indicator"),
      domainId = dplyr::if_else(
        is.na(.data$domainId),
        "feature engineering",
        .data$domainId
      ),
      startDay = .data$startDay,
      endDay = .data$endDay,
      isBinary = "Y",
      missingMeansZero = "Y"
    ) %>%
    dplyr::distinct()

  map <- map %>%
    dplyr::transmute(
      sourceCovariateId = .data$covariateId,
      indicatorCovariateId = .data$indicatorCovariateId
    ) %>%
    dplyr::distinct()

  return(list(
    map = map,
    covariateRef = indicatorCovariateRef,
    analysisRef = indicatorAnalysisRef
  ))
}

appendMissingIndicatorMetadata <- function(outputData, indicatorInfo) {
  if (nrow(indicatorInfo$map) == 0) {
    return(outputData)
  }

  existingAnalysisIds <- outputData$covariateData$analysisRef %>%
    dplyr::pull(.data$analysisId) %>%
    unique()
  existingAnalysisIdKeys <- normalizeIdKey(existingAnalysisIds)
  analysisRefToAdd <- indicatorInfo$analysisRef
  if (nrow(analysisRefToAdd) > 0) {
    analysisRefToAdd$.analysisIdKey <- normalizeIdKey(analysisRefToAdd$analysisId)
    analysisRefToAdd <- analysisRefToAdd %>%
      dplyr::filter(!.data$.analysisIdKey %in% !!existingAnalysisIdKeys) %>%
      dplyr::select(-".analysisIdKey")
  }
  if (nrow(analysisRefToAdd) > 0) {
    Andromeda::appendToTable(outputData$covariateData$analysisRef, analysisRefToAdd)
  }

  existingCovariateIds <- outputData$covariateData$covariateRef %>%
    dplyr::pull(.data$covariateId) %>%
    unique()
  existingCovariateIdKeys <- normalizeIdKey(existingCovariateIds)
  covariateRefToAdd <- indicatorInfo$covariateRef
  if (nrow(covariateRefToAdd) > 0) {
    covariateRefToAdd$.covariateIdKey <- normalizeIdKey(covariateRefToAdd$covariateId)
    covariateRefToAdd <- covariateRefToAdd %>%
      dplyr::filter(!.data$.covariateIdKey %in% !!existingCovariateIdKeys) %>%
      dplyr::select(-".covariateIdKey")
  }
  if (nrow(covariateRefToAdd) > 0) {
    Andromeda::appendToTable(outputData$covariateData$covariateRef, covariateRefToAdd)
  }

  return(outputData)
}

validatePmmK <- function(k) {
  if (!(is.numeric(k) &&
      length(k) == 1 &&
      is.finite(k) &&
      (k == as.integer(k)) &&
      k >= 1)) {
    stop("k must be a single positive integer")
  }
  as.integer(k)
}

# PMM's random draws (the cross-validation folds of a fit, the donor sampling)
# run under seeds derived from the imputer's seed, one pair per imputation
# model, so they do not depend on what else used the random stream, such as
# Andromeda's names for temporary tables, and do not advance the global stream.
# Without a seed the draws use the global stream.
withPmmSeed <- function(seed, offset, expr) {
  if (is.null(seed)) {
    return(expr)
  }
  withLocalSeed(seed + offset, expr)
}

# The seed of the imputation model with index modelIndex (1, 2, ...).
pmmModelSeed <- function(baseSeed, modelIndex) {
  baseSeed + 2 * modelIndex
}

# The imputer's seed; models saved before it existed get one drawn once.
pmmBaseSeed <- function(featureEngineeringSettings) {
  seed <- featureEngineeringSettings$methodSettings$seed
  if (is.null(seed)) {
    seed <- sample(100000, 1)
  }
  as.numeric(seed)
}

findNearestSortedIndices <- function(sortedPredictions, prediction, k) {
  n <- length(sortedPredictions)
  if (n == 0) {
    return(integer(0))
  }
  if (k >= n) {
    return(seq_len(n))
  }
  right <- findInterval(prediction, sortedPredictions) + 1L
  left <- right - 1L
  nearest <- integer(k)
  nextPos <- 1L

  while (nextPos <= k) {
    if (left < 1L) {
      nearest[nextPos] <- right
      right <- right + 1L
    } else if (right > n) {
      nearest[nextPos] <- left
      left <- left - 1L
    } else {
      dLeft <- abs(prediction - sortedPredictions[left])
      dRight <- abs(sortedPredictions[right] - prediction)
      if (dLeft <= dRight) {
        nearest[nextPos] <- left
        left <- left - 1L
      } else {
        nearest[nextPos] <- right
        right <- right + 1L
      }
    }
    nextPos <- nextPos + 1L
  }

  nearest
}

samplePmmDonors <- function(predsObs, donorMapping, predsTarget, k) {
  nDonors <- length(donorMapping)
  if (nDonors == 0) {
    stop("No observed donors available for PMM imputation")
  }
  kEff <- min(k, nDonors)

  predsObs <- as.numeric(predsObs)
  predsTarget <- as.numeric(predsTarget)
  donorOrder <- order(predsObs)
  predsObsSorted <- predsObs[donorOrder]
  donorMappingSorted <- donorMapping[donorOrder]

  imputedValues <- numeric(length(predsTarget))
  for (j in seq_along(predsTarget)) {
    donorIndices <- findNearestSortedIndices(predsObsSorted, predsTarget[j], kEff)
    donorValues <- donorMappingSorted[donorIndices]
    donorValues <- donorValues[!is.na(donorValues)]
    if (length(donorValues) == 0) {
      stop("No non-missing donors available for PMM imputation")
    }
    imputedValues[j] <- donorValues[sample.int(length(donorValues), 1)]
  }

  imputedValues
}

scalePmmCovariates <- function(xMiss) {
  xMiss %>%
    dplyr::left_join(
      xMiss %>%
        dplyr::group_by(.data$covariateId) %>%
        dplyr::summarise(
          n_unique = dplyr::n_distinct(.data$covariateValue),
          max = max(.data$covariateValue, na.rm = TRUE),
          min = min(.data$covariateValue, na.rm = TRUE)
        ),
      by = "covariateId"
    ) %>%
    dplyr::group_by(.data$covariateId) %>%
    dplyr::mutate(
      covariateValue = ifelse(.data$n_unique > 2 & (.data$max - .data$min) > 0,
        (.data$covariateValue - .data$min) / (.data$max - .data$min),
        .data$covariateValue
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c("n_unique", "min", "max"))
}

appendMissingIndicatorValues <- function(outputData, missingIndex, indicatorMap) {
  if (nrow(indicatorMap) == 0) {
    return(outputData)
  }
  indicatorRows <- missingIndex %>%
    dplyr::inner_join(
      indicatorMap,
      by = c("covariateId" = "sourceCovariateId"),
      copy = TRUE
    ) %>%
    dplyr::transmute(
      rowId = .data$rowId,
      covariateId = .data$indicatorCovariateId,
      covariateValue = 1
    )
  Andromeda::appendToTable(outputData$covariateData$covariates, indicatorRows)
  return(outputData)
}

addMissingIndicators <- function(outputData,
                                 featureEngineeringSettings,
                                 done = FALSE) {
  if (!isTRUE(featureEngineeringSettings$addMissingIndicator)) {
    return(list(
      outputData = outputData,
      featureEngineeringSettings = featureEngineeringSettings
    ))
  }
  missingThreshold <- featureEngineeringSettings$missingThreshold

  if (!done) {
    sourceCovariateIds <- outputData$covariateData$missingInfo %>%
      dplyr::filter(.data$missing <= !!missingThreshold) %>%
      dplyr::pull(.data$covariateId) %>%
      unique()
    indicatorInfo <- createMissingIndicatorInfo(outputData, sourceCovariateIds)
    outputData <- appendMissingIndicatorMetadata(outputData, indicatorInfo)
    attr(featureEngineeringSettings, "missingIndicatorInfo") <- indicatorInfo
  } else {
    indicatorInfo <- attr(featureEngineeringSettings, "missingIndicatorInfo")
    if (is.null(indicatorInfo) ||
      is.null(indicatorInfo$map) ||
      nrow(indicatorInfo$map) == 0) {
      return(list(
        outputData = outputData,
        featureEngineeringSettings = featureEngineeringSettings
      ))
    }
    outputData <- appendMissingIndicatorMetadata(outputData, indicatorInfo)
  }

  return(list(
    outputData = outputData,
    featureEngineeringSettings = featureEngineeringSettings
  ))
}

buildSklearnDenseMatrix <- function(outputData, rowIds, covariateIds) {
  if (length(rowIds) == 0 || length(covariateIds) == 0) {
    return(matrix(numeric(0), nrow = length(rowIds), ncol = length(covariateIds)))
  }

  covariateMissingMeansZero <- outputData$covariateData$covariateRef %>%
    dplyr::filter(.data$covariateId %in% !!covariateIds) %>%
    dplyr::select("covariateId", "analysisId") %>%
    dplyr::inner_join(
      outputData$covariateData$analysisRef %>%
        dplyr::select("analysisId", "missingMeansZero"),
      by = "analysisId"
    ) %>%
    dplyr::collect()

  defaultValues <- rep(NA_real_, length(covariateIds))
  names(defaultValues) <- as.character(covariateIds)
  if (nrow(covariateMissingMeansZero) > 0) {
    zeroIds <- covariateMissingMeansZero %>%
      dplyr::filter(.data$missingMeansZero == "Y") %>%
      dplyr::pull(.data$covariateId)
    defaultValues[as.character(zeroIds)] <- 0
  }

  matrixData <- matrix(
    rep(defaultValues, each = length(rowIds)),
    nrow = length(rowIds),
    ncol = length(covariateIds)
  )
  colnames(matrixData) <- as.character(covariateIds)
  rownames(matrixData) <- as.character(rowIds)

  observedValues <- outputData$covariateData$covariates %>%
    dplyr::filter(
      .data$rowId %in% !!rowIds,
      .data$covariateId %in% !!covariateIds
    ) %>%
    dplyr::select("rowId", "covariateId", "covariateValue") %>%
    dplyr::collect()

  if (nrow(observedValues) > 0) {
    rowIndex <- match(observedValues$rowId, rowIds)
    colIndex <- match(observedValues$covariateId, covariateIds)
    valid <- !is.na(rowIndex) & !is.na(colIndex)
    matrixData[cbind(rowIndex[valid], colIndex[valid])] <- observedValues$covariateValue[valid]
  }

  matrixData
}

extractSklearnMissingIndex <- function(
    originalMatrix,
    rowIds,
    targetCovariateIds,
    predictorCovariateIds) {
  if (length(targetCovariateIds) == 0 || length(predictorCovariateIds) == 0) {
    return(data.frame(rowId = numeric(0), covariateId = numeric(0)))
  }

  targetPositions <- match(targetCovariateIds, predictorCovariateIds)
  keep <- !is.na(targetPositions)
  targetCovariateIds <- targetCovariateIds[keep]
  targetPositions <- targetPositions[keep]
  if (length(targetPositions) == 0) {
    return(data.frame(rowId = numeric(0), covariateId = numeric(0)))
  }

  missingMask <- is.na(originalMatrix[, targetPositions, drop = FALSE])
  if (!any(missingMask)) {
    return(data.frame(rowId = numeric(0), covariateId = numeric(0)))
  }
  missingLocations <- which(missingMask, arr.ind = TRUE)
  data.frame(
    rowId = rowIds[missingLocations[, 1]],
    covariateId = targetCovariateIds[missingLocations[, 2]]
  )
}

extractSklearnImputedRows <- function(
    originalMatrix,
    imputedMatrix,
    rowIds,
    targetCovariateIds,
    predictorCovariateIds) {
  missingIndex <- extractSklearnMissingIndex(
    originalMatrix = originalMatrix,
    rowIds = rowIds,
    targetCovariateIds = targetCovariateIds,
    predictorCovariateIds = predictorCovariateIds
  )
  if (nrow(missingIndex) == 0) {
    return(data.frame(rowId = numeric(0), covariateId = numeric(0), covariateValue = numeric(0)))
  }

  targetPositions <- match(missingIndex$covariateId, predictorCovariateIds)
  rowPositions <- match(missingIndex$rowId, rowIds)
  imputedValues <- imputedMatrix[cbind(rowPositions, targetPositions)]

  data.frame(
    rowId = missingIndex$rowId,
    covariateId = missingIndex$covariateId,
    covariateValue = imputedValues
  ) %>%
    dplyr::filter(!is.na(.data$covariateValue))
}

asSklearnImputedMatrix <- function(imputedData, nRows, nCols) {
  matrixData <- as.matrix(imputedData)
  if (nrow(matrixData) != nRows || ncol(matrixData) != nCols) {
    stop("Unexpected transformed matrix shape from sklearn IterativeImputer")
  }
  matrixData
}

appendSimpleImputedRows <- function(outputData,
                                    covariates,
                                    imputedValues,
                                    rowIds,
                                    indicatorMap = NULL,
                                    tempData = NULL) {
  if (is.null(imputedValues)) {
    return(invisible(NULL))
  }
  imputedValues <- as.data.frame(imputedValues)
  nImputed <- nrow(imputedValues)
  if (nImputed == 0 || length(rowIds) == 0) {
    return(invisible(NULL))
  }
  if (!all(c("covariateId", "imputedValues") %in% names(imputedValues))) {
    stop("Imputed values must contain columns covariateId and imputedValues")
  }
  rowIds <- sort(unique(rowIds))
  if (is.null(tempData)) {
    tempData <- outputData$covariateData
  }
  tempData$tmpSimpleImputeRowIds <- data.frame(rowId = rowIds)
  on.exit(tempData$tmpSimpleImputeRowIds <- NULL, add = TRUE)
  tempData$tmpSimpleImputeValues <- imputedValues %>%
    dplyr::select("covariateId", "imputedValues")
  on.exit(tempData$tmpSimpleImputeValues <- NULL, add = TRUE)

  candidatePairs <- tempData$tmpSimpleImputeRowIds %>%
    dplyr::mutate(joinKey = 1L) %>%
    dplyr::inner_join(
      tempData$tmpSimpleImputeValues %>%
        dplyr::transmute(
          covariateId = .data$covariateId,
          imputedValues = .data$imputedValues,
          joinKey = 1L
        ),
      by = "joinKey"
    ) %>%
    dplyr::select(-"joinKey")
  # stored once: covariates can be the table the imputed rows are appended to,
  # and the indicators below need the pairs that were missing before
  tempData$tmpSimpleImputeMissing <- candidatePairs %>%
    dplyr::anti_join(
      covariates %>%
        dplyr::select("rowId", "covariateId"),
      by = c("rowId", "covariateId")
    )
  on.exit(tempData$tmpSimpleImputeMissing <- NULL, add = TRUE)
  missingPairs <- tempData$tmpSimpleImputeMissing
  imputedRows <- missingPairs %>%
    dplyr::transmute(
      rowId = .data$rowId,
      covariateId = .data$covariateId,
      covariateValue = .data$imputedValues
    )
  Andromeda::appendToTable(outputData$covariateData$covariates, imputedRows)
  if (!is.null(indicatorMap) && nrow(indicatorMap) > 0) {
    tempData$tmpSimpleImputeIndicators <- indicatorMap %>%
      dplyr::select("sourceCovariateId", "indicatorCovariateId")
    on.exit(tempData$tmpSimpleImputeIndicators <- NULL, add = TRUE)
    indicatorRows <- missingPairs %>%
      dplyr::inner_join(
        tempData$tmpSimpleImputeIndicators,
        by = c("covariateId" = "sourceCovariateId")
      ) %>%
      dplyr::transmute(
        rowId = .data$rowId,
        covariateId = .data$indicatorCovariateId,
        covariateValue = 1
      )
    Andromeda::appendToTable(outputData$covariateData$covariates, indicatorRows)
  }
  invisible(NULL)
}

#' @title Simple Imputation
#' @description This function does single imputation with the mean or median
#' @param trainData The data to impute: the training data, or new data when
#'   `done = TRUE`. It is changed in place.
#' @param featureEngineeringSettings The settings for the imputation
#' @param done Whether the imputer has already been fitted (bool)
#' @return `trainData`, imputed, with the step recorded for prediction
#' @keywords internal
simpleImpute <- function(trainData, featureEngineeringSettings, done = FALSE) {
  start <- Sys.time()
  outputData <- trainData
  if (!done) {
    ParallelLogger::logInfo("Imputing missing values with simpleImputer using: ",
      featureEngineeringSettings$method, " and missing threshold: ",
      featureEngineeringSettings$missingThreshold)
    missingInfo <- extractMissingInfo(outputData)
    outputData$covariateData$missingInfo <- missingInfo$missingInfo
    continuousFeatures <- missingInfo$continuousFeatures
    on.exit(outputData$covariateData$missingInfo <- NULL, add = TRUE)
    missingThreshold <- featureEngineeringSettings$missingThreshold

    removeCovariatesAboveMissingThreshold(
      outputData$covariateData, missingInfo$missingInfo, missingThreshold
    )
    missingIndicatorResults <- addMissingIndicators(outputData, featureEngineeringSettings, done = FALSE)
    outputData <- missingIndicatorResults$outputData
    featureEngineeringSettings <- missingIndicatorResults$featureEngineeringSettings

    numericCovariates <- outputData$covariateData$covariates %>%
      dplyr::filter(.data$covariateId %in% !!continuousFeatures)
    if (featureEngineeringSettings$method == "mean") {
      imputedValues <- numericCovariates %>%
        dplyr::group_by(.data$covariateId) %>%
        dplyr::summarise(imputedValues = mean(.data$covariateValue, na.rm = TRUE)) %>%
        dplyr::collect()
    } else if (featureEngineeringSettings$method == "median") {
      imputedValues <- numericCovariates %>%
        dplyr::collect() %>%
        dplyr::group_by(.data$covariateId) %>%
        dplyr::summarise(imputedValues = stats::median(.data$covariateValue, na.rm = TRUE))
    }
    indicatorMap <- NULL
    if (isTRUE(featureEngineeringSettings$addMissingIndicator)) {
      indicatorInfo <- attr(featureEngineeringSettings, "missingIndicatorInfo")
      if (!is.null(indicatorInfo) &&
        !is.null(indicatorInfo$map) &&
        nrow(indicatorInfo$map) > 0) {
        indicatorMap <- indicatorInfo$map
      }
    }
    appendSimpleImputedRows(
      outputData = outputData,
      covariates = numericCovariates,
      imputedValues = imputedValues,
      rowIds = trainData$labels$rowId,
      indicatorMap = indicatorMap
    )
    attr(featureEngineeringSettings, "missingInfo") <-
      outputData$covariateData$missingInfo %>%
      dplyr::collect()
    attr(featureEngineeringSettings, "imputer") <- as.data.frame(imputedValues)
    done <- TRUE
  } else {
    ParallelLogger::logInfo("Applying imputation to test data with simpleImputer
      using method: ", featureEngineeringSettings$method, " and missing threshold: ",
      featureEngineeringSettings$missingThreshold)
    outputData$covariateData$missingInfo <- attr(
      featureEngineeringSettings,
      "missingInfo"
    )
    on.exit(outputData$covariateData$missingInfo <- NULL, add = TRUE)
    missingThreshold <- featureEngineeringSettings$missingThreshold
    removeCovariatesAboveMissingThreshold(
      outputData$covariateData, attr(featureEngineeringSettings, "missingInfo"), missingThreshold
    )
    missingIndicatorResults <- addMissingIndicators(outputData, featureEngineeringSettings, done = TRUE)
    outputData <- missingIndicatorResults$outputData
    featureEngineeringSettings <- missingIndicatorResults$featureEngineeringSettings

    continuousFeatures <- outputData$covariateData$analysisRef %>%
      dplyr::filter(.data$isBinary == "N") %>%
      dplyr::select("analysisId") %>%
      dplyr::inner_join(outputData$covariateData$covariateRef, by = "analysisId") %>%
      dplyr::pull(.data$covariateId)
    numericCovariates <- outputData$covariateData$covariates %>%
      dplyr::filter(.data$covariateId %in% !!continuousFeatures)
    indicatorMap <- NULL
    if (isTRUE(featureEngineeringSettings$addMissingIndicator)) {
      indicatorInfo <- attr(featureEngineeringSettings, "missingIndicatorInfo")
      if (!is.null(indicatorInfo) &&
        !is.null(indicatorInfo$map) &&
        nrow(indicatorInfo$map) > 0) {
        indicatorMap <- indicatorInfo$map
      }
    }
    appendSimpleImputedRows(
      outputData = outputData,
      covariates = numericCovariates,
      imputedValues = attr(featureEngineeringSettings, "imputer"),
      rowIds = outputData$labels$rowId,
      indicatorMap = indicatorMap
    )
  }
  outputData <- recordFeatureEngineeringStep(
    outputData,
    funct = "simpleImpute",
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings,
      done = done
    )
  )
  delta <- Sys.time() - start
  ParallelLogger::logInfo("Imputation done in time: ", signif(delta, 3), " ",
  attr(delta, "units"))
  return(outputData)
}


#' @title Imputation
#' @description This function does single imputation with predictive mean matching
#' @param trainData The data to impute: the training data, or new data when
#'   `done = TRUE`. It is changed in place.
#' @param featureEngineeringSettings The settings for the imputation
#' @param done Whether the imputer has already been fitted (bool)
#' @return `trainData`, imputed, with the step recorded for prediction
#' @keywords internal
iterativeImpute <- function(trainData, featureEngineeringSettings, done = FALSE) {
  start <- Sys.time()
  outputData <- trainData
  if (!done) {
    ParallelLogger::logInfo("Imputing missing values with iterativeImputer using: ",
      featureEngineeringSettings$method, ", missing threshold: ",
      featureEngineeringSettings$missingThreshold, " and method settings: ",
      featureEngineeringSettings$methodSettings)
    missingInfo <- extractMissingInfo(outputData)
    outputData$covariateData$missingInfo <- missingInfo$missingInfo
    continuousFeatures <- missingInfo$continuousFeatures
    on.exit(outputData$covariateData$missingInfo <- NULL, add = TRUE)
    missingThreshold <- featureEngineeringSettings$missingThreshold

    removeCovariatesAboveMissingThreshold(
      outputData$covariateData, missingInfo$missingInfo, missingThreshold
    )
    missingIndicatorResults <- addMissingIndicators(outputData, featureEngineeringSettings, done = FALSE)
    outputData <- missingIndicatorResults$outputData
    featureEngineeringSettings <- missingIndicatorResults$featureEngineeringSettings

    numericData <- separateFeatures(outputData, continuousFeatures)
    on.exit(Andromeda::close(numericData), add = TRUE)

    numericData <- initializeImputation(numericData, "mean",
      labels = outputData$labels
    )
    attr(featureEngineeringSettings, "initialValues") <- numericData$covariates %>%
      dplyr::group_by(.data$covariateId) %>%
      dplyr::summarise(initialValue = mean(.data$covariateValue, na.rm = TRUE)) %>%
      dplyr::collect()
    if (isTRUE(featureEngineeringSettings$addMissingIndicator)) {
      indicatorInfo <- attr(featureEngineeringSettings, "missingIndicatorInfo")
      if (!is.null(indicatorInfo) &&
        !is.null(indicatorInfo$map) &&
        nrow(indicatorInfo$map) > 0) {
        outputData <- appendMissingIndicatorValues(
          outputData = outputData,
          missingIndex = numericData$missingIndex,
          indicatorMap = indicatorInfo$map
        )
      }
    }
    # add imputed values in data
    iterativeImputeResults <- iterativeChainedImpute(numericData,
      outputData,
      featureEngineeringSettings,
      direction = "ascending",
      iterations = featureEngineeringSettings$methodSettings$iterations
    )

    Andromeda::appendToTable(
      outputData$covariateData$covariates,
      iterativeImputeResults$numericData$imputedCovariates %>%
        dplyr::filter(is.na(.data$covariateValue)) %>%
        dplyr::mutate(covariateValue = .data$imputedValue) %>%
        dplyr::select(-c("imputedValue"))
    )


    attr(featureEngineeringSettings, "missingInfo") <-
      outputData$covariateData$missingInfo %>%
      dplyr::collect()
    attr(featureEngineeringSettings, "imputer") <- iterativeImputeResults$modelInfo
    attr(featureEngineeringSettings, "kdeEstimates") <- iterativeImputeResults$kdeEstimates
    done <- TRUE
  } else {
    ParallelLogger::logInfo("Applying imputation to test data with iterativeImputer
      using method: ", featureEngineeringSettings$method, " and missing threshold: ",
      featureEngineeringSettings$missingThreshold)
    # remove data with more than missingThreshold
    outputData$covariateData$missingInfo <- attr(
      featureEngineeringSettings,
      "missingInfo"
    )
    on.exit(outputData$covariateData$missingInfo <- NULL, add = TRUE)
    missingThreshold <- featureEngineeringSettings$missingThreshold
    # the same missing-threshold rule as training, so the covariates that are
    # not imputed are the same too
    removeCovariatesAboveMissingThreshold(
      outputData$covariateData, attr(featureEngineeringSettings, "missingInfo"), missingThreshold
    )
    missingIndicatorResults <- addMissingIndicators(outputData, featureEngineeringSettings, done = TRUE)
    outputData <- missingIndicatorResults$outputData
    featureEngineeringSettings <- missingIndicatorResults$featureEngineeringSettings

    # Use the numeric features seen in training, as columns can be absent from this batch.
    initValues <- attr(featureEngineeringSettings, "initialValues")
    continuousFeatures <- unique(c(
      as.numeric(names(attr(featureEngineeringSettings, "imputer"))),
      as.numeric(initValues$covariateId)
    ))
    # the working tables are removed when numericData is closed
    numericData <- separateFeatures(outputData, continuousFeatures)
    on.exit(Andromeda::close(numericData), add = TRUE)
    # impute missing values
    allRowIds <- outputData$labels$rowId
    allColumnIds <- sort(continuousFeatures)
    completeIds <- expand.grid(rowId = allRowIds, covariateId = allColumnIds)
    # now we have NAs for missing combinations
    numericData$covariates <- merge(completeIds, numericData$covariates,
      all.x = TRUE
    )

    # get index of NAs for every feature to be imputed
    numericData$missingIndex <- numericData$covariates %>%
      dplyr::filter(is.na(.data$covariateValue)) %>%
      dplyr::select(-c("covariateValue"))
    if (isTRUE(featureEngineeringSettings$addMissingIndicator)) {
      indicatorInfo <- attr(featureEngineeringSettings, "missingIndicatorInfo")
      if (!is.null(indicatorInfo) &&
        !is.null(indicatorInfo$map) &&
        nrow(indicatorInfo$map) > 0) {
        outputData <- appendMissingIndicatorValues(
          outputData = outputData,
          missingIndex = numericData$missingIndex,
          indicatorMap = indicatorInfo$map
        )
      }
    }

    if (is.null(initValues)) {
      initValues <- numericData$covariates %>%
        dplyr::group_by(.data$covariateId) %>%
        dplyr::summarise(initialValue = mean(.data$covariateValue, na.rm = TRUE)) %>%
        dplyr::collect()
    }
    numericData$imputedCovariates <- numericData$covariates %>%
      dplyr::left_join(initValues, by = "covariateId", copy = TRUE) %>%
      dplyr::mutate(
        imputedValue = ifelse(
          is.na(.data$covariateValue),
          .data$initialValue,
          .data$covariateValue
        )
      ) %>%
      dplyr::select(-"initialValue")


    varsToImpute <- numericData$missingIndex %>%
      dplyr::pull(.data$covariateId) %>%
      unique()
    baseSeed <- pmmBaseSeed(featureEngineeringSettings)
    modelIndex <- 0
    for (varId in varsToImpute) {
      modelIndex <- modelIndex + 1
      varName <- outputData$covariateData$covariateRef %>%
        dplyr::filter(.data$covariateId == varId) %>%
        dplyr::pull(.data$covariateName)
      ParallelLogger::logInfo("Imputing variable: ", varName)
      numericData$y <- numericData$covariates %>%
        dplyr::filter(.data$covariateId == varId) %>%
        dplyr::mutate(y = .data$covariateValue) %>%
        dplyr::select("y", "rowId")
      missRowIds <- numericData$y %>%
        dplyr::filter(is.na(.data$y)) %>%
        dplyr::pull(.data$rowId)
      numericData$xMiss <- imputationPredictors(numericData, varId) %>%
        dplyr::filter(.data$rowId %in% !!missRowIds)

      imputer <-
        attr(featureEngineeringSettings, "imputer")[[as.character(varId)]]
      if (is.null(imputer)) {
        # Complete in training so no PMM model exists; keep the initial value.
        next
      }
      varStart <- Sys.time()
      kPmm <- featureEngineeringSettings$methodSettings$k
      if (is.null(kPmm)) {
        kPmm <- 5
      }
      nMissRows <- length(missRowIds)
      nDonors <- if (!is.null(imputer$predictions)) nrow(imputer$predictions) else 0
      pmmResults <- pmmPredict(numericData, k = kPmm, imputer, seed = pmmModelSeed(baseSeed, modelIndex))
      varDelta <- Sys.time() - varStart
      varNameSafe <- if (length(varName) == 0 || is.na(varName[1])) as.character(varId) else varName[1]
      ParallelLogger::logInfo(
        "PMM predict for variable ", varNameSafe,
        " (covariateId=", varId, "): missing=", nMissRows,
        ", donors=", nDonors, ", k=", kPmm,
        ", time=", signif(varDelta, 3), " ", attr(varDelta, "units")
      )

      # update imputations in data
      numericData$imputedValues <- pmmResults$imputedValues
      numericData$imputedCovariates <- numericData$imputedCovariates %>%
        dplyr::left_join(numericData$imputedValues,
          by = "rowId",
          suffix = c("", ".new")
        ) %>%
        dplyr::mutate(
          imputedValue =
            dplyr::if_else(.data$covariateId == varId &&
              !is.na(.data$imputedValue.new),
            .data$imputedValue.new,
            .data$imputedValue
            )
        ) %>%
        dplyr::select(-"imputedValue.new")
    }
    # add imputed values in data
    Andromeda::appendToTable(
      outputData$covariateData$covariates,
      numericData$imputedCovariates %>%
        dplyr::filter(is.na(.data$covariateValue)) %>%
        dplyr::mutate(covariateValue = .data$imputedValue) %>%
        dplyr::select(-c("imputedValue"))
    )
  }
  outputData <- recordFeatureEngineeringStep(
    outputData,
    funct = "iterativeImpute",
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings,
      done = done
    )
  )
  delta <- Sys.time() - start
  ParallelLogger::logInfo("Imputation done in time: ", signif(delta, 3), " ",
  attr(delta, "units"))
  return(outputData)
}

#' @title scikit-learn Iterative Imputation
#' @description This function imputes missing values using
#' scikit-learn `IterativeImputer` through `reticulate`.
#' @param trainData The data to impute: the training data, or new data when
#'   `done = TRUE`. It is changed in place.
#' @param featureEngineeringSettings The settings for the imputation
#' @param done Whether the imputer has already been fitted (bool)
#' @return `trainData`, imputed, with the step recorded for prediction
#' @keywords internal
sklearnIterativeImpute <- function(trainData, featureEngineeringSettings, done = FALSE) {
  start <- Sys.time()

  outputData <- trainData
  if (!done) {
    # fitting needs Python; applying the saved steps does not
    checkSklearnIterativeImputerDependencies()
    ParallelLogger::logInfo(
      "Imputing missing values with sklearn IterativeImputer, missing threshold: ",
      featureEngineeringSettings$missingThreshold
    )

    missingInfo <- extractMissingInfo(outputData)
    outputData$covariateData$missingInfo <- missingInfo$missingInfo
    on.exit(outputData$covariateData$missingInfo <- NULL, add = TRUE)
    missingThreshold <- featureEngineeringSettings$missingThreshold

    removeCovariatesAboveMissingThreshold(
      outputData$covariateData, missingInfo$missingInfo, missingThreshold
    )

    missingIndicatorResults <- addMissingIndicators(outputData, featureEngineeringSettings, done = FALSE)
    outputData <- missingIndicatorResults$outputData
    featureEngineeringSettings <- missingIndicatorResults$featureEngineeringSettings

    predictorCovariateIds <- outputData$covariateData$covariates %>%
      dplyr::pull(.data$covariateId) %>%
      unique() %>%
      sort()
    targetCovariateIds <- outputData$covariateData$missingInfo %>%
      dplyr::filter(.data$missing <= !!missingThreshold) %>%
      dplyr::pull(.data$covariateId) %>%
      unique()
    targetCovariateIds <- targetCovariateIds[targetCovariateIds %in% predictorCovariateIds]
    allRowIds <- outputData$labels$rowId

    if (length(predictorCovariateIds) > 0) {
      denseMatrix <- buildSklearnDenseMatrix(
        outputData = outputData,
        rowIds = allRowIds,
        covariateIds = predictorCovariateIds
      )
      missingIndex <- extractSklearnMissingIndex(
        originalMatrix = denseMatrix,
        rowIds = allRowIds,
        targetCovariateIds = targetCovariateIds,
        predictorCovariateIds = predictorCovariateIds
      )

      if (isTRUE(featureEngineeringSettings$addMissingIndicator) && nrow(missingIndex) > 0) {
        indicatorInfo <- attr(featureEngineeringSettings, "missingIndicatorInfo")
        if (!is.null(indicatorInfo) &&
          !is.null(indicatorInfo$map) &&
          nrow(indicatorInfo$map) > 0) {
          outputData <- appendMissingIndicatorValues(
            outputData = outputData,
            missingIndex = missingIndex,
            indicatorMap = indicatorInfo$map
          )
        }
      }

      imputer <- createSklearnIterativeImputerObject(featureEngineeringSettings$methodSettings)
      imputedMatrix <- asSklearnImputedMatrix(
        reticulate::py_to_r(imputer$fit_transform(denseMatrix)),
        nRows = nrow(denseMatrix),
        nCols = ncol(denseMatrix)
      )
      imputedRows <- extractSklearnImputedRows(
        originalMatrix = denseMatrix,
        imputedMatrix = imputedMatrix,
        rowIds = allRowIds,
        targetCovariateIds = targetCovariateIds,
        predictorCovariateIds = predictorCovariateIds
      )
      if (nrow(imputedRows) > 0) {
        Andromeda::appendToTable(outputData$covariateData$covariates, imputedRows)
      }
      # the Python object is kept for this session, and the step list is saved
      featureEngineeringSettings$sklearnImputerKey <- registerSklearnIterativeImputerRuntime(imputer)
      attr(featureEngineeringSettings, "sklearnImputerSteps") <- extractSklearnImputerSteps(
        imputer = imputer,
        covariateIds = predictorCovariateIds,
        methodSettings = featureEngineeringSettings$methodSettings
      )
    } else {
      featureEngineeringSettings$sklearnImputerKey <- NULL
    }

    missingInfoValue <- outputData$covariateData$missingInfo %>%
      dplyr::collect()
    attr(featureEngineeringSettings, "missingInfo") <- missingInfoValue
    attr(featureEngineeringSettings, "sklearnPredictorCovariateIds") <- predictorCovariateIds
    attr(featureEngineeringSettings, "sklearnTargetCovariateIds") <- targetCovariateIds
    featureEngineeringSettings$sklearnMissingInfo <- missingInfoValue
    featureEngineeringSettings$sklearnPredictorCovariateIds <- predictorCovariateIds
    featureEngineeringSettings$sklearnTargetCovariateIds <- targetCovariateIds

    done <- TRUE
  } else {
    ParallelLogger::logInfo(
      "Applying sklearn IterativeImputer to test data, missing threshold: ",
      featureEngineeringSettings$missingThreshold
    )

    missingInfoValue <- attr(featureEngineeringSettings, "missingInfo")
    if (is.null(missingInfoValue)) {
      missingInfoValue <- featureEngineeringSettings$sklearnMissingInfo
    }
    if (is.null(missingInfoValue)) {
      stop("sklearn iterative imputer settings are incomplete; refit model in current session.")
    }
    outputData$covariateData$missingInfo <- missingInfoValue
    on.exit(outputData$covariateData$missingInfo <- NULL, add = TRUE)

    predictorCovariateIds <- attr(featureEngineeringSettings, "sklearnPredictorCovariateIds")
    if (is.null(predictorCovariateIds)) {
      predictorCovariateIds <- featureEngineeringSettings$sklearnPredictorCovariateIds
    }
    if (is.null(predictorCovariateIds)) {
      stop("sklearn iterative imputer settings are incomplete; refit model in current session.")
    }
    targetCovariateIds <- attr(featureEngineeringSettings, "sklearnTargetCovariateIds")
    if (is.null(targetCovariateIds)) {
      targetCovariateIds <- featureEngineeringSettings$sklearnTargetCovariateIds
    }
    if (is.null(targetCovariateIds)) {
      targetCovariateIds <- numeric(0)
    }
    missingThreshold <- featureEngineeringSettings$missingThreshold

    # only the covariates the imputer was fitted with
    removeCovariatesAboveMissingThreshold(
      outputData$covariateData, missingInfoValue, missingThreshold,
      keepCovariateIds = predictorCovariateIds
    )

    missingIndicatorResults <- addMissingIndicators(outputData, featureEngineeringSettings, done = TRUE)
    outputData <- missingIndicatorResults$outputData
    featureEngineeringSettings <- missingIndicatorResults$featureEngineeringSettings

    if (length(predictorCovariateIds) > 0) {
      allRowIds <- outputData$labels$rowId
      denseMatrix <- buildSklearnDenseMatrix(
        outputData = outputData,
        rowIds = allRowIds,
        covariateIds = predictorCovariateIds
      )
      missingIndex <- extractSklearnMissingIndex(
        originalMatrix = denseMatrix,
        rowIds = allRowIds,
        targetCovariateIds = targetCovariateIds,
        predictorCovariateIds = predictorCovariateIds
      )
      if (isTRUE(featureEngineeringSettings$addMissingIndicator) && nrow(missingIndex) > 0) {
        indicatorInfo <- attr(featureEngineeringSettings, "missingIndicatorInfo")
        if (!is.null(indicatorInfo) &&
          !is.null(indicatorInfo$map) &&
          nrow(indicatorInfo$map) > 0) {
          outputData <- appendMissingIndicatorValues(
            outputData = outputData,
            missingIndex = missingIndex,
            indicatorMap = indicatorInfo$map
          )
        }
      }

      imputerSteps <- attr(featureEngineeringSettings, "sklearnImputerSteps")
      if (!is.null(imputerSteps)) {
        if (!identical(as.numeric(unlist(imputerSteps$covariateIds)), as.numeric(predictorCovariateIds))) {
          stop("The saved sklearn imputer steps do not match the covariates it was fitted on.")
        }
        imputedMatrix <- applySklearnImputerSteps(denseMatrix, imputerSteps)
      } else {
        imputer <- getSklearnIterativeImputerRuntime(featureEngineeringSettings$sklearnImputerKey)
        if (is.null(imputer)) {
          stop(
            "This sklearn iterative imputer has no saved steps, because it uses ",
            "samplePosterior = TRUE or was fitted by an earlier version, so it can ",
            "only be applied in the R session it was fitted in. Refit the model."
          )
        }
        imputedMatrix <- asSklearnImputedMatrix(
          reticulate::py_to_r(imputer$transform(denseMatrix)),
          nRows = nrow(denseMatrix),
          nCols = ncol(denseMatrix)
        )
      }
      imputedRows <- extractSklearnImputedRows(
        originalMatrix = denseMatrix,
        imputedMatrix = imputedMatrix,
        rowIds = allRowIds,
        targetCovariateIds = targetCovariateIds,
        predictorCovariateIds = predictorCovariateIds
      )
      if (nrow(imputedRows) > 0) {
        Andromeda::appendToTable(outputData$covariateData$covariates, imputedRows)
      }
    }
  }

  outputData <- recordFeatureEngineeringStep(
    outputData,
    funct = "sklearnIterativeImpute",
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings,
      done = done
    )
  )
  delta <- Sys.time() - start
  ParallelLogger::logInfo("Imputation done in time: ", signif(delta, 3), " ",
  attr(delta, "units"))
  outputData
}

#' @title Predictive mean matching using lasso
#' @param data An andromeda object with the following fields:
#'      xObs: covariates table for observed data
#'      xMiss: covariates table for missing data
#'      yObs: outcome variable that we want to impute
#' @param k The number of donors to use for matching (default 5)
#' @param alpha Elastic-net mixing parameter (`1` = lasso, `0` = ridge)
#' @param seed The seed of the model's random draws (cross-validation folds and
#'   donor sampling), or NULL to use the global random stream
#' @keywords internal
pmmFit <- function(data, k = 5, alpha = 1, seed = NULL) {
  rlang::check_installed("glmnet")
  k <- validatePmmK(k)
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha < 0 || alpha > 1) {
    stop("alpha should be a single numeric value in [0, 1]")
  }
  data$rowMap <- data$xObs %>%
    dplyr::group_by(.data$rowId) %>%
    dplyr::summarise() %>%
    dplyr::collect() %>%
    dplyr::arrange(.data$rowId) %>%
    dplyr::mutate(
      oldRowId = .data$rowId,
      newRowId = dplyr::row_number()
    ) %>%
    dplyr::select(c("newRowId", "oldRowId"))
  on.exit(data$rowMap <- NULL, add = TRUE)
  data$colMap <- data$xObs %>%
    dplyr::group_by(.data$covariateId) %>%
    dplyr::summarise() %>%
    dplyr::collect() %>%
    dplyr::arrange(.data$covariateId) %>%
    dplyr::mutate(
      oldCovariateId = .data$covariateId,
      newCovariateId = dplyr::row_number()
    ) %>%
    dplyr::select(c("newCovariateId", "oldCovariateId"))
  on.exit(data$colMap <- NULL, add = TRUE)

  # row, column and value in one query, so the three belong together
  obsEntries <- data$xObs %>%
    dplyr::left_join(data$rowMap, by = c("rowId" = "oldRowId")) %>%
    dplyr::left_join(data$colMap, by = c("covariateId" = "oldCovariateId")) %>%
    dplyr::select("newRowId", "newCovariateId", "covariateValue") %>%
    dplyr::collect()
  xObs <- Matrix::sparseMatrix(
    i = obsEntries$newRowId,
    j = obsEntries$newCovariateId,
    x = obsEntries$covariateValue,
    dims = c(
      data$rowMap %>% dplyr::pull(.data$newRowId) %>% max(),
      data$colMap %>% dplyr::pull(.data$newCovariateId) %>% max()
    )
  )

  # Align y to xObs row index (newRowId) explicitly.
  yObsAligned <- data$rowMap %>%
    dplyr::left_join(data$yObs, by = c("oldRowId" = "rowId")) %>%
    dplyr::arrange(.data$newRowId) %>%
    dplyr::select(c("newRowId", "oldRowId", "y")) %>%
    dplyr::collect()
  if (nrow(yObsAligned) != nrow(xObs)) {
    stop("xObs/yObs row count mismatch in pmmFit")
  }
  if (any(is.na(yObsAligned$y))) {
    stop("Missing y values after xObs/yObs alignment in pmmFit")
  }

  fit <- withPmmSeed(seed, 0, glmnet::cv.glmnet(
    xObs,
    yObsAligned$y,
    alpha = alpha,
    nfolds = 3
  ))

  # predict on both XObs and XMiss
  predsObs <- stats::predict(fit, xObs, fit$lambda.min)
  # Keep xMiss on the same scale representation as xObs.
  # Applying an xMiss-only scaling step can distort PMM donor matching.
  rowMapMiss <- data$xMiss %>%
    dplyr::group_by(.data$rowId) %>%
    dplyr::summarise() %>%
    dplyr::collect() %>%
    dplyr::arrange(.data$rowId) %>%
    dplyr::mutate(
      oldRowId = .data$rowId,
      newRowId = dplyr::row_number()
    ) %>%
    dplyr::select(c("newRowId", "oldRowId"))
  data$rowMapMiss <- rowMapMiss
  on.exit(data$rowMapMiss <- NULL, add = TRUE)
  missEntries <- data$xMiss %>%
    dplyr::left_join(data$rowMapMiss, by = c("rowId" = "oldRowId")) %>%
    dplyr::left_join(data$colMap, by = c("covariateId" = "oldCovariateId")) %>%
    dplyr::filter(!is.na(.data$newCovariateId), !is.na(.data$newRowId)) %>%
    dplyr::select("newRowId", "newCovariateId", "covariateValue") %>%
    dplyr::collect()

  # precompute mapping to use - straight from xId (row index) to
  # covariateValue of donor
  donorMapping <- yObsAligned$y
  # a row for every patient to impute: one whose predictors were all unseen in
  # the observed patients has no entries, so a prediction from the intercept,
  # as in pmmPredict()
  nMissRows <- nrow(rowMapMiss)
  nCols <- data$colMap %>%
    dplyr::pull(.data$newCovariateId) %>%
    max()

  if (nMissRows == 0) {
    imputedValues <- numeric(0)
  } else {
    xMiss <- Matrix::sparseMatrix(
      i = missEntries$newRowId,
      j = missEntries$newCovariateId,
      x = missEntries$covariateValue,
      dims = c(nMissRows, nCols)
    )
    predsMiss <- stats::predict(fit, xMiss, fit$lambda.min)
    imputedValues <- withPmmSeed(seed, 1, samplePmmDonors(predsObs, donorMapping, predsMiss, k))
  }

  results <- list()
  results$imputedValues <- data.frame(
    rowId = data$rowMapMiss %>%
      dplyr::pull(.data$oldRowId),
    imputedValue = imputedValues
  )
  bestIndex <- which(fit$lambda == fit$lambda.min)
  nonZero <- which(fit$glmnet.fit$beta[, bestIndex] != 0)
  nonZeroCovariateIds <- data$colMap %>%
    dplyr::filter(.data$newCovariateId %in% nonZero) %>%
    dplyr::pull(.data$oldCovariateId)
  if (length(nonZero) == 0) {
    ParallelLogger::logWarn("Imputation model only has intercept. It does not fit the data well")
  }
  results$model <- list(
    intercept = as.numeric(fit$glmnet.fit$a0[bestIndex]),
    coefficients = data.frame(
      covariateId = nonZeroCovariateIds,
      values = as.numeric(fit$glmnet.fit$beta[nonZero, bestIndex])
    ),
    predictions = data.frame(
      rowId = yObsAligned$oldRowId,
      prediction = as.numeric(predsObs),
      observedValue = yObsAligned$y
    )
  )
  return(results)
}

pmmPredict <- function(data, k = 5, imputer, seed = NULL) {
  k <- validatePmmK(k)
  coefficients <- imputer$coefficients
  if (!is.data.frame(coefficients)) {
    # a model without predictors has an empty table, which the four-decimal
    # format of earlier versions loads back as an empty list
    coefficients <- if (length(coefficients) == 0) {
      data.frame(covariateId = numeric(0), values = numeric(0))
    } else {
      as.data.frame(dplyr::bind_rows(coefficients))
    }
  }
  data$coefficients <- coefficients
  predictionMissing <- data$xMiss %>%
    dplyr::left_join(data$coefficients, by = "covariateId") %>%
    dplyr::mutate(values = .data$covariateValue * .data$values) %>%
    dplyr::group_by(.data$rowId) %>%
    dplyr::summarise(value = sum(.data$values, na.rm = TRUE)) %>%
    # rowId without any of nonzero coefficient will have NA
    # and should use only intercept for prediction
    dplyr::mutate(value = ifelse(is.na(.data$value), 0, .data$value)) %>%
    dplyr::select("rowId", "value")
  predictionMissing <- as.data.frame(predictionMissing)
  if (length(predictionMissing$value) == 0) {
    # prediction model for imputing only has intercept
    ParallelLogger::logWarn("Imputation model only has intercept,
      imputing with intercept. Something went wrong during fitting probably.")
    predictionMissing <- data.frame(
      rowId = data$xMiss %>%
        dplyr::pull(.data$rowId) %>% 
        unique(),
      value = imputer$intercept
    )
  } else {
    predictionMissing$value <- predictionMissing$value + imputer$intercept
  }

  # precompute mapping to use - straight from xId (row index) to
  # covariateValue of donor
  if ("observedValue" %in% names(imputer$predictions)) {
    donorMapping <- imputer$predictions %>% dplyr::pull(.data$observedValue)
  } else {
    # Backward compatibility for older saved imputers without observed donors.
    donorMapping <- imputer$predictions %>% dplyr::pull(.data$prediction)
  }

  # for each missing value, find the k closest observed values
  nRows <- data$xMiss %>%
    dplyr::pull(.data$rowId) %>%
    dplyr::n_distinct()
  predsObs <- imputer$predictions$prediction
  if (nRows == 0) {
    imputedValues <- numeric(0)
  } else {
    imputedValues <- withPmmSeed(seed, 1, samplePmmDonors(
      predsObs = predsObs,
      donorMapping = donorMapping,
      predsTarget = predictionMissing$value,
      k = k
    ))
  }

  results <- list()
  results$imputedValues <- data.frame(
    rowId = predictionMissing %>%
      dplyr::pull(.data$rowId),
    imputedValue = imputedValues
  )
  return(results)
}

# Removes covariates whose share of missing values in training (missingInfo) is
# above missingThreshold, and, with keepCovariateIds, every covariate not in it.
# Covariates without missingInfo are kept. The covariates table is rewritten
# only when one of them is present, so the common case leaves it untouched.
removeCovariatesAboveMissingThreshold <- function(covariateData,
                                                  missingInfo,
                                                  missingThreshold,
                                                  keepCovariateIds = NULL) {
  missingInfo <- as.data.frame(dplyr::collect(missingInfo))
  dropIds <- missingInfo$covariateId[
    !is.na(missingInfo$missing) & missingInfo$missing > missingThreshold
  ]
  if (!is.null(keepCovariateIds)) {
    presentIds <- covariateData$covariates %>%
      dplyr::distinct(.data$covariateId) %>%
      dplyr::pull()
    dropIds <- union(dropIds, setdiff(presentIds, keepCovariateIds))
  }
  if (length(dropIds) == 0) {
    return(invisible(FALSE))
  }
  covariateData$tmpMissingDropIds <- data.frame(covariateId = dropIds)
  on.exit(covariateData$tmpMissingDropIds <- NULL, add = TRUE)
  present <- covariateData$covariates %>%
    dplyr::semi_join(covariateData$tmpMissingDropIds, by = "covariateId") %>%
    utils::head(1) %>%
    dplyr::collect()
  if (nrow(present) == 0) {
    return(invisible(FALSE))
  }
  covariateData$covariates <- covariateData$covariates %>%
    dplyr::anti_join(covariateData$tmpMissingDropIds, by = "covariateId")
  invisible(TRUE)
}

extractMissingInfo <- function(trainData) {
  ParallelLogger::logInfo("Calculating missingness in data")
  covariateData <- trainData$covariateData
  covariateData$tmpMissingInfoRowIds <- data.frame(rowId = unique(trainData$labels$rowId))
  on.exit(covariateData$tmpMissingInfoRowIds <- NULL, add = TRUE)
  total <- length(unique(trainData$labels$rowId))
  continuousFeatures <- trainData$covariateData$analysisRef %>%
    dplyr::filter(.data$isBinary == "N",
                  .data$missingMeansZero == "N") %>%
    dplyr::select("analysisId") %>%
    dplyr::inner_join(trainData$covariateData$covariateRef, by = "analysisId") %>%
    dplyr::pull(.data$covariateId)

  missingInfo <- trainData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId %in% continuousFeatures) %>%
    dplyr::inner_join(covariateData$tmpMissingInfoRowIds, by = "rowId") %>%
    dplyr::group_by(.data$covariateId) %>%
    dplyr::summarise(counts = dplyr::n()) %>%
    dplyr::collect() %>% # necessary because of integer division in sqlite
    dplyr::mutate(missing = 1 - .data$counts / total) %>%
    dplyr::select(c("covariateId", "missing"))
  results <- list(
    "missingInfo" = missingInfo,
    "continuousFeatures" = continuousFeatures
  )
  ParallelLogger::logInfo("Found ", nrow(missingInfo$missing > 0), " features with missing values")

  return(results)
}

# The working Andromeda of iterative imputation, which the caller closes: the
# continuous covariates to impute, and the other covariates, the predictors of
# every imputation model, copied once.
separateFeatures <- function(trainData, continuousFeatures) {
  numericData <- Andromeda::andromeda()
  numericData$covariates <- trainData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId %in% continuousFeatures)
  numericData$binaryCovariates <- trainData$covariateData$covariates %>%
    dplyr::filter(!.data$covariateId %in% !!continuousFeatures) %>%
    dplyr::select("rowId", "covariateId", "covariateValue")
  return(numericData)
}

# The predictors for imputing varId: the current imputations of the other
# continuous covariates and the other covariates, as a query.
imputationPredictors <- function(numericData, varId) {
  dplyr::union_all(
    numericData$imputedCovariates %>%
      dplyr::filter(.data$covariateId != !!varId) %>%
      dplyr::select("rowId", "covariateId", covariateValue = "imputedValue"),
    numericData$binaryCovariates
  )
}

initializeImputation <- function(numericData, method = "mean", labels) {
  allRowIds <- labels$rowId
  allColumnIds <- numericData$covariates %>%
    dplyr::pull(.data$covariateId) %>%
    unique() %>%
    sort()
  completeIds <- expand.grid(rowId = allRowIds, covariateId = allColumnIds)
  numericData$covariates <- merge(completeIds, numericData$covariates,
    all.x = TRUE
  )

  # get index of NAs for every feature to be imputed
  numericData$missingIndex <- numericData$covariates %>%
    dplyr::filter(is.na(.data$covariateValue)) %>%
    dplyr::select(-c("covariateValue"))

  if (method == "mean") {
    numericData$imputedCovariates <- numericData$covariates %>%
      dplyr::group_by(.data$covariateId) %>%
      dplyr::mutate(imputedValue = ifelse(is.na(.data$covariateValue),
        mean(.data$covariateValue, na.rm = TRUE),
        .data$covariateValue
      ))
  } else {
    stop("Unknown initialization method: ", method)
  }
  return(numericData)
}

# Main (M)ICE algorithm - iterative imputation with chained equations
iterativeChainedImpute <- function(numericData,
                                   originalData,
                                   featureEngineeringSettings,
                                   direction = "ascending",
                                   iterations = 5) {
  prevImputations <- list()
  allRowIds <- numericData$covariates %>%
    dplyr::pull(.data$rowId) %>%
    unique() %>%
    sort()
  maxIter <- iterations # TODO check
  varsToImpute <- numericData$missingIndex %>%
    dplyr::pull(.data$covariateId) %>%
    unique()
  convergenceParameters <- list()
  modelInfo <- list()
  baseSeed <- pmmBaseSeed(featureEngineeringSettings)
  modelIndex <- 0

  for (iter in 1:maxIter) {
    ParallelLogger::logInfo("Imputation iteration: ", iter)
    currentImputations <- list()

    # TODO do in order from least missing to most missing
    for (varId in varsToImpute) {
      varName <- originalData$covariateData$covariateRef %>%
        dplyr::filter(.data$covariateId == varId) %>%
        dplyr::pull(.data$covariateName)
      ParallelLogger::logInfo("Imputing variable: ", varName)
      numericData$y <- numericData$covariates %>%
        dplyr::filter(.data$covariateId == varId) %>%
        dplyr::mutate(y = .data$covariateValue) %>%
        dplyr::select("y", "rowId")
      on.exit(numericData$y <- NULL, add = TRUE)
      obsIdx <- which(!is.na(numericData$y %>% dplyr::pull(.data$y)))
      missIdx <- which(is.na(numericData$y %>% dplyr::pull(.data$y)))
      numericData$yObs <- numericData$y %>%
        dplyr::filter(.data$rowId %in% !!allRowIds[obsIdx])
      on.exit(numericData$yObs <- NULL, add = TRUE)

      predictors <- imputationPredictors(numericData, varId)
      numericData$xObs <- predictors %>% dplyr::filter(.data$rowId %in% !!allRowIds[obsIdx])
      on.exit(numericData$xObs <- NULL, add = TRUE)
      numericData$xMiss <- predictors %>% dplyr::filter(.data$rowId %in% !!allRowIds[missIdx])
      on.exit(numericData$xMiss <- NULL, add = TRUE)

      modelIndex <- modelIndex + 1
      pmmResults <- pmmFit(
        numericData,
        k = featureEngineeringSettings$methodSettings$k,
        alpha = featureEngineeringSettings$methodSettings$alpha,
        seed = pmmModelSeed(baseSeed, modelIndex)
      )

      # update imputations in data
      numericData$imputedValues <- pmmResults$imputedValues
      on.exit(numericData$imputedValues <- NULL, add = TRUE)
      numericData$imputedCovariates <- numericData$imputedCovariates %>%
        dplyr::left_join(numericData$imputedValues,
          by = "rowId",
          suffix = c("", ".new")
        ) %>%
        dplyr::mutate(
          imputedValue =
            dplyr::if_else(.data$covariateId == varId &&
              !is.na(.data$imputedValue.new),
            .data$imputedValue.new,
            .data$imputedValue
            )
        ) %>%
        dplyr::select(-"imputedValue.new")

      # store current imputations for convergence check
      currentImputations[[as.character(varId)]] <- pmmResults$imputedValues$imputedValue

      # store pmm info for each variable
      modelInfo[[as.character(varId)]] <- pmmResults$model
    }

    # save values for convergence checking afterwards
    # store mean and variance of imputed values for each variable
    # as well as average change from previous iteration
    meanVector <- numeric(length(varsToImpute))
    varVector <- numeric(length(varsToImpute))
    idx <- 1
    for (varId in varsToImpute) {
      currentImputation <- currentImputations[[as.character(varId)]]
      meanVector[idx] <- mean(currentImputation)
      varVector[idx] <- stats::var(currentImputation)
      idx <- idx + 1
    }
    convergenceInfo <- list(
      meanVector = meanVector,
      varVector = varVector
    )
    if (iter > 1) {
      meanVarChange <- numeric(length(varsToImpute))
      for (varId in varsToImpute) {
        prevImputation <- prevImputations[[as.character(varId)]]
        currentImputation <- currentImputations[[as.character(varId)]]
        meanVarChange <- c(
          meanVarChange,
          mean(abs(currentImputation - prevImputation))
        )
      }
      convergenceInfo$meanVarChange <- meanVarChange
    }
    convergenceParameters[[iter]] <- convergenceInfo

    prevImputations <- currentImputations
  }

  # calculate kde estimates of imputed and observed distributions per imputed variable
  # and store in featureEngineeringSettings
  kdeEstimates <- list()
  for (varId in varsToImpute) {
    varName <- originalData$covariateData$covariateRef %>%
      dplyr::filter(.data$covariateId == varId) %>%
      dplyr::pull(.data$covariateName)
    rows <- numericData$missingIndex %>%
      dplyr::filter(.data$covariateId == varId) %>%
      dplyr::pull(.data$rowId)
    imputedValues <- numericData$imputedCovariates %>%
      dplyr::filter(
        .data$covariateId == varId,
        .data$rowId %in% rows
      ) %>%
      dplyr::pull(.data$imputedValue)
    observedValues <- numericData$covariates %>%
      dplyr::filter(
        .data$covariateId == varId,
        !is.na(.data$covariateValue)
      ) %>%
      dplyr::pull(.data$covariateValue)
    kdeEstimates[[as.character(varId)]] <- list(
      imputed = stats::density(imputedValues),
      observed = stats::density(observedValues)
    )
  }
  results <- list(
    "numericData" = numericData,
    "convergenceParameters" = convergenceParameters,
    "modelInfo" = modelInfo,
    "kdeEstimates" = kdeEstimates
  )
  return(results)
}
