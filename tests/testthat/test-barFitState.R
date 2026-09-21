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

createBarFitStateData <- function() {
  withr::local_seed(42)
  n <- 300
  p <- 10
  x <- matrix(rnorm(n * p), n, p)
  y <- rbinom(n, 1, plogis(-0.5 + x[, 1] + 0.5 * x[, 2]))
  labels <- data.frame(rowId = seq_len(n), y = y)
  covariates <- data.frame(
    rowId = rep(seq_len(n), p),
    covariateId = rep(seq_len(p), each = n),
    covariateValue = as.vector(x)
  )
  list(
    labels = labels,
    covariates = covariates,
    folds = data.frame(rowId = seq_len(n), index = rep(1:3, length.out = n)),
    designMatrix = cbind(1, x),
    newCyclopsData = function() {
      Cyclops::convertToCyclopsData(labels, covariates, quiet = TRUE)
    }
  )
}

test_that("BAR tuning matches independent fits and refits all training rows", {
  skip_if_not_installed("BrokenAdaptiveRidge")
  skip_on_cran()
  fixture <- createBarFitStateData()
  covariateData <- Andromeda::andromeda(
    covariates = fixture$covariates, labels = fixture$labels
  )
  class(covariateData) <- "CovariateData"
  withr::defer(Andromeda::close(covariateData))
  trainData <- list(
    labels = fixture$labels, folds = fixture$folds, covariateData = covariateData
  )
  settings <- setBrokenAdaptiveRidge(
    initialRidgeVariance = 0.5, penaltyGridSize = 3, threads = 1, seed = 42
  )
  control <- createCyclopsRefitControl(settings)
  penalties <- createBarPenaltyGrid(fixture$labels, penaltyRatio = 0.1, penaltyGridSize = 3)
  result <- doCyclopsCvPenalty(
    trainData, fixture$newCyclopsData(), settings, settings$param$priorParams
  )

  referenceAuc <- matrix(NA_real_, nrow = 3, ncol = length(penalties))
  for (foldIndex in 1:3) {
    holdOut <- fixture$folds$index == foldIndex
    for (j in seq_along(penalties)) {
      priorParams <- settings$param$priorParams
      priorParams$penalty <- penalties[j]
      prior <- do.call(BrokenAdaptiveRidge::createBarPrior, priorParams)
      independentFit <- Cyclops::fitCyclopsModel(
        fixture$newCyclopsData(), prior = prior, control = control,
        weights = as.numeric(!holdOut)
      )
      scores <- as.numeric(fixture$designMatrix %*% stats::coef(independentFit))
      referenceAuc[foldIndex, j] <- aucWithoutCi(scores[holdOut], fixture$labels$y[holdOut])
      candidate <- subset(
        result$hyperParamSearch, fold == paste0("Fold", foldIndex) & penalty == penalties[j]
      )
      expect_equal(candidate$value, referenceAuc[foldIndex, j])
    }
  }
  expectedPenalty <- penalties[order(-colMeans(referenceAuc), -penalties)[1]]
  expect_equal(result$penalty, expectedPenalty)

  independentFullFit <- Cyclops::fitCyclopsModel(
    fixture$newCyclopsData(), prior = result$prior, control = control
  )
  expect_equal(stats::coef(result$modelFit), stats::coef(independentFullFit))
  expect_equal(result$modelFit$log_likelihood, independentFullFit$log_likelihood)
  scores <- as.numeric(fixture$designMatrix %*% stats::coef(result$modelFit))
  expect_equal(
    result$modelFit$log_likelihood,
    sum(fixture$labels$y * scores - log1p(exp(scores)))
  )

  # Reversing candidate order must not change scores, selection, or the final fit.
  testthat::local_mocked_bindings(
    createBarPenaltyGrid = function(...) rev(penalties),
    .package = "PatientLevelPrediction"
  )
  reversed <- doCyclopsCvPenalty(
    trainData, fixture$newCyclopsData(), settings, settings$param$priorParams
  )
  expect_equal(reversed$hyperParamSearch, result$hyperParamSearch)
  expect_equal(reversed$penalty, result$penalty)
  expect_equal(stats::coef(reversed$modelFit), stats::coef(result$modelFit))
})

test_that("BAR numeric and BIC penalty CV refits match independent folds", {
  skip_if_not_installed("BrokenAdaptiveRidge")
  skip_on_cran()
  fixture <- createBarFitStateData()
  covariateData <- Andromeda::andromeda(covariates = fixture$covariates)
  class(covariateData) <- "CovariateData"
  withr::defer(Andromeda::close(covariateData))

  for (penalty in list(0.5, "bic")) {
    settings <- setBrokenAdaptiveRidge(
      initialRidgeVariance = 0.5, penalty = penalty, threads = 1, seed = 42
    )
    prior <- do.call(BrokenAdaptiveRidge::createBarPrior, settings$param$priorParams)
    control <- createCyclopsRefitControl(settings)
    cyclopsData <- fixture$newCyclopsData()
    fullFit <- Cyclops::fitCyclopsModel(cyclopsData, prior = prior, control = control)
    model <- createCyclopsModel(
      fit = fullFit, modelType = "logistic", useCrossValidation = TRUE,
      cyclopsData = cyclopsData, labels = fixture$labels, folds = fixture$folds,
      modelSettings = settings, covariateData = covariateData, control = control,
      cvPrior = prior
    )

    for (fold in 1:3) {
      holdOut <- fixture$folds$index == fold
      independentFit <- Cyclops::fitCyclopsModel(
        fixture$newCyclopsData(), prior = prior, control = control,
        weights = as.numeric(!holdOut)
      )
      scores <- as.numeric(fixture$designMatrix %*% stats::coef(independentFit))
      expect_equal(model$cv[[fold]]$coef, stats::coef(independentFit))
      expect_equal(model$cv[[fold]]$predCV$value, plogis(scores[holdOut]))
      expect_equal(
        model$cv[[fold]]$out_sample_auc,
        aucWithoutCi(scores[holdOut], fixture$labels$y[holdOut])
      )
    }
  }
})
