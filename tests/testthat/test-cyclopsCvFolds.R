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

# Cyclops models cross-validate on PLP's training folds (fitCyclopsModel(cvFolds = ))
makeCvFoldsFixture <- function(n = 600L, p = 8L, nfold = 3L) {
  withr::local_seed(11)
  x <- matrix(rbinom(n * p, 1, 0.3), n, p)
  lp <- -1 + x %*% c(1, -1, 0.5, rep(0, p - 3))
  y <- rbinom(n, 1, plogis(lp))
  nonZero <- which(x != 0, arr.ind = TRUE)
  ids <- as.numeric(seq_len(p) * 1000 + 102)
  covariateData <- makeCovariateData(
    covariates = data.frame(rowId = nonZero[, 1], covariateId = ids[nonZero[, 2]], covariateValue = 1),
    covariateRef = data.frame(
      covariateId = ids, covariateName = paste0("c", ids), analysisId = 102L, conceptId = ids
    ),
    analysisRef = data.frame(
      analysisId = 102L, analysisName = "test", domainId = "Condition", isBinary = "Y"
    ),
    metaData = list(populationSize = n)
  )
  labels <- data.frame(
    rowId = seq_len(n), subjectId = seq_len(n), outcomeCount = y,
    survivalTime = ifelse(y == 1, sample(1:300, n, replace = TRUE), 365),
    cohortStartDate = as.Date("2000-01-01"),
    daysToCohortEnd = 365, daysToObsEnd = 365, ageYear = 40, gender = 1
  )
  # Cox risks are predicted at the end of the time-at-risk
  attr(labels, "metaData") <- list(
    populationSettings = createStudyPopulationSettings(riskWindowStart = 1, riskWindowEnd = 365)
  )
  makePlpData(
    covariateData = covariateData, labels = labels,
    folds = data.frame(rowId = seq_len(n), index = sample(rep(seq_len(nfold), length.out = n))),
    metaData = syntheticTrainMetaData()
  )
}

test_that("lasso CV predictions come from refits of PLP's folds", {
  train <- makeCvFoldsFixture()
  on.exit(Andromeda::close(train$covariateData))
  settings <- setLassoLogisticRegression(threads = 1, seed = 42)
  fit <- fitCyclopsModel(train, settings, analysisId = "test")
  cv <- fit$prediction[fit$prediction$evaluationType == "CV", ]
  expect_equal(sort(cv$rowId), sort(train$labels$rowId))

  # each fold refitted at the selected variance without its rows predicts them
  covariates <- dplyr::collect(train$covariateData$covariates)
  cyclopsData <- Cyclops::convertToCyclopsData(
    outcomes = data.frame(rowId = train$labels$rowId, y = train$labels$outcomeCount),
    covariates = covariates, modelType = "lr", addIntercept = TRUE, quiet = TRUE
  )
  for (i in 1:3) {
    holdOut <- train$folds$rowId[train$folds$index == i]
    foldFit <- Cyclops::fitCyclopsModel(
      cyclopsData,
      prior = Cyclops::createPrior("laplace", variance = fit$model$priorVariance, exclude = 0),
      control = Cyclops::createControl(tolerance = 2e-6, noiseLevel = "silent", threads = 1),
      weights = as.numeric(!train$labels$rowId %in% holdOut)
    )
    expected <- stats::predict(foldFit)[train$labels$rowId %in% holdOut]
    expect_equal(
      cv$value[match(train$labels$rowId[train$labels$rowId %in% holdOut], cv$rowId)],
      unname(expected),
      tolerance = 1e-6
    )
  }

  search <- fit$trainDetails$hyperParamSearch
  expect_equal(search$fold, c("CV", "Fold1", "Fold2", "Fold3"))
  expect_equal(search$value[1], mean(search$value[-1]))
})

test_that("the selected variance depends on PLP's folds, not on the Cyclops seed", {
  train <- makeCvFoldsFixture()
  on.exit(Andromeda::close(train$covariateData))
  variance <- function(modelSettings, folds = train$folds) {
    train$folds <- folds
    suppressWarnings(fitCyclopsModel(train, modelSettings, analysisId = "test"))$model$priorVariance
  }
  for (setter in list(setLassoLogisticRegression, setRidgeRegression, setCoxModel)) {
    expect_equal(
      variance(setter(threads = 1, seed = 1)),
      variance(setter(threads = 1, seed = 2))
    )
  }
  # relabelling or reordering the folds changes nothing
  folds <- train$folds[rev(seq_len(nrow(train$folds))), ]
  folds$index <- c(3, 1, 2)[folds$index]
  expect_equal(
    variance(setLassoLogisticRegression(threads = 1, seed = 1), folds),
    variance(setLassoLogisticRegression(threads = 1, seed = 1))
  )
  # another partition of the rows does
  withr::local_seed(99)
  otherFolds <- data.frame(rowId = train$folds$rowId, index = sample(train$folds$index))
  expect_false(isTRUE(all.equal(
    variance(setLassoLogisticRegression(threads = 1, seed = 1), otherFolds),
    variance(setLassoLogisticRegression(threads = 1, seed = 1))
  )))
})

test_that("CV results are named by PLP's fold labels", {
  train <- makeCvFoldsFixture()
  on.exit(Andromeda::close(train$covariateData))
  train$folds$index <- c(2, 5, 7)[train$folds$index]
  fit <- fitCyclopsModel(train, setLassoLogisticRegression(threads = 1, seed = 42), analysisId = "test")
  expect_equal(fit$trainDetails$hyperParamSearch$fold, c("CV", "Fold2", "Fold5", "Fold7"))
  cv <- fit$prediction[fit$prediction$evaluationType == "CV", ]
  expect_equal(nrow(cv), nrow(train$labels))
})

test_that("a model whose fold refits all failed keeps its fit without CV rows", {
  train <- makeCvFoldsFixture()
  on.exit(Andromeda::close(train$covariateData))
  createModel <- createCyclopsModel
  local_mocked_bindings(createCyclopsModel = function(...) {
    model <- createModel(...)
    model$cv <- lapply(model$cv, function(x) {
      x$predCV <- x$predCV[0, ]
      x$out_sample_auc <- NA_real_
      x
    })
    model
  })
  fit <- fitCyclopsModel(train, setLassoLogisticRegression(threads = 1, seed = 42), analysisId = "test")
  expect_equal(unique(fit$prediction$evaluationType), "Train")
  expect_true(all(is.na(fit$trainDetails$hyperParamSearch$value)))
})

test_that("CV predictions do not depend on the number of threads", {
  train <- makeCvFoldsFixture()
  on.exit(Andromeda::close(train$covariateData))
  fits <- lapply(c(1, 3), function(threads) {
    fitCyclopsModel(train, setLassoLogisticRegression(threads = threads, seed = 42), analysisId = "test")
  })
  expect_equal(fits[[2]]$model$priorVariance, fits[[1]]$model$priorVariance)
  expect_equal(fits[[2]]$prediction, fits[[1]]$prediction, tolerance = 1e-8)
})

test_that("cyclopsCvByFold keeps folds whose refit failed", {
  labels <- data.frame(rowId = 1:6, y = c(0, 1, 0, 1, 0, 1), cohortStartDate = 0)
  cvPredictions <- data.frame(
    rowId = 6:1, fold = c(2, 2, 2, 1, 1, 1),
    prediction = c(NA, NA, NA, 0.2, 0.7, 0)
  )
  cv <- cyclopsCvByFold(cvPredictions, labels)
  expect_length(cv, 2)
  expect_equal(cv[[1]]$predCV$rowId, 1:3)
  expect_equal(cv[[1]]$predCV$index, c(1, 1, 1))
  expect_equal(cv[[1]]$predCV$value, c(0, 0.7, 0.2))
  expect_equal(cv[[1]]$predCV$rawValue, stats::qlogis(c(1e-15, 0.7, 0.2)))
  expect_equal(cv[[1]]$out_sample_auc, 1)
  expect_equal(nrow(cv[[2]]$predCV), 0)
  expect_true(is.na(cv[[2]]$out_sample_auc))
})

test_that("Cyclops gets PLP's folds for every model that cross-validates", {
  train <- list(
    folds = data.frame(rowId = 1:6, index = c(1, 2, 3, 1, 2, 3)),
    labels = data.frame(rowId = 1:5)
  )
  cvPrior <- Cyclops::createPrior("laplace", useCrossValidation = TRUE)
  fixedPrior <- Cyclops::createPrior("laplace", variance = 1)
  hookPrior <- structure(list(useCrossValidation = FALSE, fitHook = function(...) NULL), class = "cyclopsPrior")

  for (prior in list(cvPrior, fixedPrior, hookPrior)) {
    cv <- cyclopsCvArguments(prior, train, "logistic")
    expect_equal(cv$cvFolds, data.frame(rowId = 1:5, fold = c(1, 2, 3, 1, 2)))
    expect_true(cv$returnCvPredictions)
  }
  # Cox selects its variance on the folds; Cyclops has no CV predictions for it
  cv <- cyclopsCvArguments(cvPrior, train, "cox")
  expect_equal(nrow(cv$cvFolds), 5)
  expect_false(cv$returnCvPredictions)
  expect_null(cyclopsCvArguments(fixedPrior, train, "cox")$cvFolds)
  # one fold: nothing to cross-validate
  train$folds$index <- 1
  cv <- cyclopsCvArguments(cvPrior, train, "logistic")
  expect_null(cv$cvFolds)
  expect_false(cv$returnCvPredictions)
})

# Refits of each fold, with the prior of modelSettings, predicting its held-out rows
independentFoldPredictions <- function(train, prior) {
  covariates <- dplyr::collect(train$covariateData$covariates)
  outcomes <- data.frame(rowId = train$labels$rowId, y = train$labels$outcomeCount)
  outcomes <- outcomes[order(outcomes$rowId), ]
  fold <- train$folds$index[match(outcomes$rowId, train$folds$rowId)]
  predictions <- rep(NA_real_, nrow(outcomes))
  for (i in sort(unique(fold))) {
    cyclopsData <- Cyclops::convertToCyclopsData(
      outcomes, covariates, modelType = "lr", addIntercept = TRUE, quiet = TRUE
    )
    foldFit <- Cyclops::fitCyclopsModel(
      cyclopsData, prior = prior, weights = as.numeric(fold != i),
      control = Cyclops::createControl(noiseLevel = "silent", threads = 1)
    )
    predictions[fold == i] <- stats::predict(foldFit)[fold == i]
  }
  data.frame(rowId = outcomes$rowId, value = predictions)
}

test_that("legacy IHT CV predictions select covariates within each fold", {
  skip_if_not_installed("IterativeHardThresholding", minimum_version = "2.0.0")
  skip_on_cran()
  train <- makeCvFoldsFixture()
  on.exit(Andromeda::close(train$covariateData))
  settings <- setIterativeHardThresholding(K = 5, seed = 42, threads = 1, algorithm = "legacy")
  fit <- suppressWarnings(fitCyclopsModel(train, settings, analysisId = "test"))
  cv <- fit$prediction[fit$prediction$evaluationType == "CV", ]
  expect_equal(sort(cv$rowId), sort(train$labels$rowId))

  prior <- suppressWarnings(createIhtPriorFromSettings(settings$param$priorParams))
  expected <- suppressWarnings(independentFoldPredictions(train, prior))
  expect_equal(cv$value[match(expected$rowId, cv$rowId)], expected$value, tolerance = 1e-6)
})

test_that("Cyclops fit control gets the model's threads, seed and variance search", {
  lasso <- setLassoLogisticRegression(variance = 0.05, threads = 3, seed = 1)
  control <- createCyclopsFitControl(lasso$param, lasso$settings)
  expect_equal(control$startingVariance, 0.05)
  expect_equal(control$threads, 3)
  expect_equal(control$seed, 1)
  expect_equal(control$lowerLimit, lasso$param$lowerLimit)
})

test_that("IHT settings saved before IterativeHardThresholding 2.0 run the legacy algorithm", {
  skip_if_not_installed("IterativeHardThresholding", minimum_version = "2.0.0")
  # the priorParams setIterativeHardThresholding() saved in PatientLevelPrediction 6.x
  old <- list(
    K = 10, penalty = "bic", exclude = c(), forceIntercept = FALSE, fitBestSubset = FALSE,
    initialRidgeVariance = 0.1, tolerance = 1e-08, maxIterations = 10000, threshold = 1e-06, delta = 0
  )
  messages <- character()
  prior <- withCallingHandlers(
    createIhtPriorFromSettings(old),
    warning = function(w) {
      messages <<- c(messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("saved before", messages)))
  expect_s3_class(prior, "ihtLegacyPrior")
  expect_equal(prior$penalty, "bic")

  # a model setting saved by PLP 6.x, through JSON, fits with CV predictions
  old$K <- 3
  saved <- structure(list(
    fitFunction = "fitCyclopsModel",
    param = list(priorParams = old),
    settings = list(
      modelType = "binary", modelName = "iterativeHardThresholding", cyclopsModelType = "logistic",
      priorfunction = "IterativeHardThresholding::createIhtPrior", selectorType = "byRow",
      crossValidationInPrior = FALSE, addIntercept = TRUE, useControl = FALSE, seed = 42,
      predict = "predictCyclops", saveType = "RtoJson"
    )
  ), class = "modelSettings")
  saved <- ParallelLogger::convertJsonToSettings(ParallelLogger::convertSettingsToJson(saved))
  train <- makeCvFoldsFixture()
  on.exit(Andromeda::close(train$covariateData))
  fit <- suppressWarnings(fitCyclopsModel(train, saved, analysisId = "test"))
  expect_equal(sum(fit$prediction$evaluationType == "CV"), nrow(train$labels))
  expect_lte(sum(fit$model$coefficients$betas != 0), 3)
})


test_that("BAR CV predictions refit each fold with the BAR prior", {
  skip_if_not_installed("BrokenAdaptiveRidge")
  skip_on_cran()
  train <- makeCvFoldsFixture()
  on.exit(Andromeda::close(train$covariateData))
  settings <- setBrokenAdaptiveRidge(initialRidgeVariance = 0.5, penalty = 1, threads = 1, seed = 42)
  fit <- fitCyclopsModel(train, settings, analysisId = "test")
  cv <- fit$prediction[fit$prediction$evaluationType == "CV", ]

  prior <- do.call(BrokenAdaptiveRidge::createBarPrior, settings$param$priorParams)
  expected <- independentFoldPredictions(train, prior)
  expect_equal(cv$value[match(expected$rowId, cv$rowId)], expected$value, tolerance = 1e-6)
})
