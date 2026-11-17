# @file IterativeHardThresholding.R
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

# Iterative hard thresholding (IterativeHardThresholding >= 2.0) as a model of
# the generic hyperparameter search: K and variance are tuned on PLP's folds
# like the hyperparameters of other models. Each fold's training rows become a
# Cyclops data object once (prepareIhtFold), and a fold fit starts from that
# fold's fit at the nearest earlier candidate (smaller K at the same variance
# first). The "legacy" algorithm and settings saved before
# IterativeHardThresholding 2.0 are fitted by fitCyclopsModel() instead.

# The variances that variance = "tune" tries (IterativeHardThresholding's
# default grid, without its extension past an edge winner)
ihtTuneVarianceGrid <- c(0.01, 0.03, 0.1, 0.3, 1, 3, Inf)

# The sparse matrix of toSparseM(), with the covariateIds as column names, so
# that the model can map its coefficients and exclude to covariates
prepareIhtMatrix <- function(plpData, cohort = NULL, map = NULL) {
  result <- toSparseM(plpData, cohort = cohort, map = map)
  covariateIds <- rep(NA_real_, ncol(result$dataMatrix))
  covariateIds[result$covariateRef$columnId] <- result$covariateRef$covariateId
  colnames(result$dataMatrix) <- sprintf("%.0f", covariateIds)
  result
}

# A fold's training rows as a Cyclops data object. Cyclops covariate ids are
# the matrix columns; covariates without a value in these rows are left out.
prepareIhtFold <- function(dataMatrix, labels, settings) {
  if (inherits(dataMatrix, "plpIhtData")) {
    return(dataMatrix)
  }
  if (is.null(colnames(dataMatrix))) {
    stop("The IHT model needs the covariateIds as column names of the data matrix (prepareIhtMatrix)")
  }
  triplets <- Matrix::summary(methods::as(dataMatrix, "CsparseMatrix"))
  covariates <- data.frame(
    rowId = as.numeric(triplets$i),
    covariateId = as.numeric(triplets$j),
    covariateValue = as.numeric(triplets$x)
  )
  rm(triplets)
  covariates <- covariates[order(covariates$rowId, covariates$covariateId), , drop = FALSE]
  outcomes <- data.frame(
    rowId = as.numeric(seq_len(nrow(dataMatrix))),
    y = as.numeric(labels$outcomeCount > 0)
  )
  cyclopsData <- Cyclops::convertToCyclopsData(
    outcomes = outcomes,
    covariates = covariates,
    modelType = "lr",
    addIntercept = TRUE,
    checkRowIds = FALSE,
    quiet = TRUE
  )
  structure(
    list(
      cyclopsData = cyclopsData,
      covariateIds = as.numeric(colnames(dataMatrix)),
      # coefficient names in Cyclops's order: "(Intercept)" and the columns
      coefficientNames = names(Cyclops::evaluateLikelihood(
        cyclopsData, rep(0, Cyclops::getNumberOfCovariates(cyclopsData))
      )$gradient),
      nRows = nrow(dataMatrix)
    ),
    class = "plpIhtData"
  )
}

fitIhtModel <- function(dataMatrix, labels, hyperParameters, settings, context = NULL) {
  rlang::check_installed("IterativeHardThresholding", version = "2.0.0")
  fold <- prepareIhtFold(dataMatrix, labels, settings)
  K <- as.integer(hyperParameters$K)
  # Inf comes back from JSON as "Inf"
  variance <- as.numeric(hyperParameters$variance)
  priorParams <- settings$priorParams
  forceIntercept <- isTRUE(priorParams$forceIntercept)

  columns <- setdiff(fold$coefficientNames, "(Intercept)")
  exclude <- match(as.numeric(priorParams$exclude), fold$covariateIds)
  exclude <- exclude[!is.na(exclude) & as.character(exclude) %in% columns]
  nPenalized <- length(columns) - length(exclude) + forceIntercept
  if (K > nPenalized) {
    stopCandidate(paste0(
      "K = ", K, " is more than the ", nPenalized, " penalized covariates with values in the training rows"
    ))
  }

  prior <- IterativeHardThresholding::createIhtPrior(
    K = K,
    variance = variance,
    exclude = exclude,
    forceIntercept = forceIntercept,
    algorithm = priorParams$algorithm %||% "default",
    control = do.call(IterativeHardThresholding::createIhtControl, as.list(priorParams$control))
  )
  start <- ihtStartingCoefficients(context$state, K, variance, fold$coefficientNames)
  control <- Cyclops::createControl(noiseLevel = "silent", seed = settings$seed, threads = 1)
  # convergence is reported in the search history (ihtFitInfo) and logged for
  # the final fit, rather than warned for every fold and candidate
  fit <- withCallingHandlers(
    Cyclops::fitCyclopsModel(
      fold$cyclopsData,
      prior = prior,
      control = control,
      startingCoefficients = start,
      warnings = FALSE
    ),
    warning = function(w) {
      if (grepl("did not converge", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
  if (!identical(fit$return_flag, "SUCCESS")) {
    stopCandidate(paste0("the IHT fit ended with ", fit$return_flag))
  }
  coefficients <- stats::coef(fit)
  isIntercept <- names(coefficients) == "(Intercept)"
  nonZero <- which(coefficients != 0 & !isIntercept)
  columnId <- as.integer(names(coefficients)[nonZero])
  converged <- isTRUE(fit$iht$converged)
  if (isTRUE(context$final) && !converged) {
    ParallelLogger::logWarn("IHT did not converge for K = ", K, ", variance = ", variance)
  }
  list(
    intercept = if (any(isIntercept)) unname(coefficients[isIntercept]) else 0,
    coefficients = data.frame(
      covariateId = fold$covariateIds[columnId],
      coefficient = unname(coefficients[nonZero])
    ),
    columnId = columnId,
    mapping = "logistic",
    K = K,
    variance = variance,
    algorithm = priorParams$algorithm %||% "default",
    converged = converged,
    iterations = fit$iht$iterations %||% NA_real_,
    refits = fit$iht$refits %||% NA_real_,
    warmStart = !is.null(start)
  )
}

# What a fold keeps of a fit to start later candidates from: the non-zero
# coefficients, by (K, variance)
updateIhtState <- function(state, model, hyperParameters, settings) {
  key <- paste(model$K, model$variance)
  state[[key]] <- list(
    K = model$K,
    variance = model$variance,
    intercept = model$intercept,
    columnId = model$columnId,
    coefficient = model$coefficients$coefficient
  )
  state
}

# The starting coefficients for K and variance from a fold's earlier fits: the
# nearest variance, then a K not above this K, then the nearest K. NULL for the
# first candidate.
ihtStartingCoefficients <- function(state, K, variance, coefficientNames) {
  if (length(state) == 0) {
    return(NULL)
  }
  logVariance <- function(v) ifelse(is.finite(v), log10(v), 10)
  earlierK <- vapply(state, function(s) as.numeric(s$K), numeric(1))
  earlierVariance <- vapply(state, function(s) as.numeric(s$variance), numeric(1))
  nearest <- order(
    abs(logVariance(earlierVariance) - logVariance(variance)),
    earlierK > K,
    abs(earlierK - K)
  )[1]
  earlier <- state[[nearest]]
  start <- numeric(length(coefficientNames))
  start[coefficientNames == "(Intercept)"] <- earlier$intercept
  position <- match(as.character(earlier$columnId), coefficientNames)
  start[position[!is.na(position)]] <- earlier$coefficient[!is.na(position)]
  start
}

ihtFitInfo <- function(model, hyperParameters, settings) {
  list(
    nonZero = nrow(model$coefficients),
    converged = model$converged,
    iterations = model$iterations,
    refits = model$refits,
    warmStart = model$warmStart
  )
}

# Risks from an IHT model: on the sparse matrix in the hyperparameter search,
# and on plpData (new data) as a logistic model of the covariates
predictIht <- function(plpModel, data, cohort) {
  model <- if (inherits(plpModel, "plpModel")) plpModel$model else plpModel
  if (inherits(data, "plpData")) {
    if (!inherits(plpModel, "plpModel")) {
      plpModel <- structure(list(model = model), class = "plpModel")
    }
    return(predictGlm(plpModel, data, cohort))
  }
  linearPredictor <- rep(model$intercept, nrow(data))
  if (length(model$columnId) > 0) {
    linearPredictor <- linearPredictor +
      as.numeric(data[, model$columnId, drop = FALSE] %*% model$coefficients$coefficient)
  }
  prediction <- cohort
  prediction$value <- stats::plogis(linearPredictor)
  if (!is.null(prediction$originalRowId)) {
    prediction$rowId <- prediction$originalRowId
    prediction$originalRowId <- NULL
  }
  attr(prediction, "metaData") <- list(modelType = "binary")
  prediction
}

ihtVariableImportance <- function(model, covariateMap) {
  data.frame(
    covariateId = model$coefficients$covariateId,
    covariateValue = model$coefficients$coefficient,
    included = rep(1, nrow(model$coefficients))
  )
}

# oneStandardError = TRUE: the smallest K whose mean held-out performance is
# within one standard error (over the folds) of the best; the best of those
# when several variances have that K
ihtOneStandardErrorSelector <- function(history, metric) {
  best <- selectBest(history, metric)
  if (length(best) == 0) {
    return(best)
  }
  foldScores <- history[[best]]$cvPerformancePerFold
  foldScores <- foldScores[!is.na(foldScores)]
  se <- if (length(foldScores) > 1) stats::sd(foldScores) / sqrt(length(foldScores)) else 0
  means <- vapply(history, function(x) as.numeric(x$cvPerformance), numeric(1))
  within <- if (metric$maximize) means >= means[best] - se else means <= means[best] + se
  within <- which(!is.na(means) & within)
  K <- vapply(history[within], function(x) as.numeric(x$param$K), numeric(1))
  smallest <- within[K == min(K)]
  smallest[if (metric$maximize) which.max(means[smallest]) else which.min(means[smallest])]
}
