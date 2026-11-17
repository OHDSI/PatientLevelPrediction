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

# Iterative hard thresholding (default algorithm) tuned by the generic
# hyperparameter search

makeIhtFixture <- function(n = 600L, p = 12L, nfold = 3L) {
  withr::local_seed(21)
  x <- matrix(rbinom(n * p, 1, 0.3), n, p)
  lp <- -1 + x %*% c(1.2, -1, 0.7, 0.4, rep(0, p - 4))
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
    survivalTime = 365, cohortStartDate = as.Date("2000-01-01"),
    daysToCohortEnd = 365, daysToObsEnd = 365, ageYear = 40, gender = 1
  )
  attr(labels, "metaData") <- list(populationSettings = createStudyPopulationSettings())
  makePlpData(
    covariateData = covariateData, labels = labels,
    folds = data.frame(rowId = seq_len(n), index = sample(rep(seq_len(nfold), length.out = n))),
    metaData = syntheticTrainMetaData()
  )
}

fitIht <- function(train, modelSettings, ...) {
  fitPlp(
    trainData = train, modelSettings = modelSettings,
    hyperparameterSettings = createHyperparameterSettings(...),
    analysisId = "iht", analysisPath = tempdir()
  )
}

cvRows <- function(plpModel) {
  cv <- plpModel$prediction[plpModel$prediction$evaluationType == "CV", ]
  cv[order(cv$rowId), ]
}

test_that("IHT CV predictions equal IHT's own fits of each fold", {
  skip_if_not_installed("IterativeHardThresholding", minimum_version = "2.0.0")
  skip_on_cran()
  train <- makeIhtFixture()
  on.exit(Andromeda::close(train$covariateData))
  model <- fitIht(train, setIterativeHardThresholding(K = 3, seed = 42))
  cv <- cvRows(model)
  expect_equal(cv$rowId, sort(train$labels$rowId))

  # IterativeHardThresholding's cross-validation on the same folds
  covariates <- dplyr::collect(train$covariateData$covariates)
  outcomes <- data.frame(rowId = train$labels$rowId, y = train$labels$outcomeCount)
  cyclopsData <- Cyclops::convertToCyclopsData(outcomes, covariates, modelType = "lr", quiet = TRUE)
  own <- suppressWarnings(Cyclops::fitCyclopsModel(
    cyclopsData,
    prior = IterativeHardThresholding::createIhtPrior(K = 3, variance = 1),
    control = Cyclops::createControl(noiseLevel = "silent", threads = 1),
    cvFolds = data.frame(rowId = train$folds$rowId, fold = train$folds$index),
    returnCvPredictions = TRUE
  ))
  expected <- own$cvPredictions[order(own$cvPredictions$rowId), ]
  expect_equal(cv$value, expected$prediction, tolerance = 1e-6)

  # the final model: K covariates, the same as a fit on all rows
  expect_equal(nrow(model$model$coefficients), 3)
  allRows <- suppressWarnings(Cyclops::fitCyclopsModel(
    cyclopsData, prior = IterativeHardThresholding::createIhtPrior(K = 3, variance = 1),
    control = Cyclops::createControl(noiseLevel = "silent", threads = 1)
  ))
  betas <- stats::coef(allRows)
  betas <- betas[betas != 0 & names(betas) != "(Intercept)"]
  expect_setequal(model$model$coefficients$covariateId, as.numeric(names(betas)))
  expect_equal(
    model$model$coefficients$coefficient[order(model$model$coefficients$covariateId)],
    unname(betas[order(as.numeric(names(betas)))]),
    tolerance = 1e-6
  )
})

test_that("IHT tunes K and variance on the folds, starting each fold from its earlier fits", {
  skip_if_not_installed("IterativeHardThresholding", minimum_version = "2.0.0")
  skip_on_cran()
  train <- makeIhtFixture()
  on.exit(Andromeda::close(train$covariateData))
  model <- fitIht(train, setIterativeHardThresholding(K = c(1, 2, 4), variance = c(1, Inf), seed = 42))
  search <- model$trainDetails$hyperParamSearch
  expect_length(search, 6)
  expect_true(all(vapply(search, function(e) e$metric, character(1)) == "logLikelihood"))
  # K varies fastest
  expect_equal(vapply(search, function(e) as.numeric(e$param$K), numeric(1)), c(1, 2, 4, 1, 2, 4))
  expect_equal(vapply(search, function(e) as.numeric(e$param$variance), numeric(1)), rep(c(1, Inf), each = 3))
  warm <- lapply(search, function(e) vapply(e$info, function(f) f$warmStart, logical(1)))
  expect_false(any(warm[[1]]))
  expect_true(all(unlist(warm[-1])))
  nonZero <- lapply(search, function(e) vapply(e$info, function(f) f$nonZero, numeric(1)))
  expect_equal(unlist(nonZero), rep(c(1, 2, 4, 1, 2, 4), each = 3))

  final <- model$trainDetails$finalModelParameters
  best <- which.max(vapply(search, function(e) e$cvPerformance, numeric(1)))
  expect_equal(final, search[[best]]$param)
  expect_equal(nrow(model$model$coefficients), final$K)
  importance <- model$covariateImportance
  expect_equal(sum(importance$covariateValue != 0), final$K)
})

test_that("IHT gives the same results with one and several fold workers", {
  skip_if_not_installed("IterativeHardThresholding", minimum_version = "2.0.0")
  skip_on_cran()
  skip_on_os("windows")
  train <- makeIhtFixture()
  on.exit(Andromeda::close(train$covariateData))
  settings <- setIterativeHardThresholding(K = c(1, 3), variance = c(0.5, Inf), seed = 42)
  one <- fitIht(train, settings)
  three <- fitIht(train, settings, foldWorkers = 3)
  dropTimes <- function(search) lapply(search, function(e) {
    e$seconds <- NULL
    e
  })
  expect_identical(three$prediction, one$prediction)
  expect_identical(three$model, one$model)
  expect_equal(dropTimes(three$trainDetails$hyperParamSearch), dropTimes(one$trainDetails$hyperParamSearch))
})

test_that("IHT predicts new data with its coefficients, also after saving", {
  skip_if_not_installed("IterativeHardThresholding", minimum_version = "2.0.0")
  skip_on_cran()
  train <- makeIhtFixture()
  on.exit(Andromeda::close(train$covariateData))
  model <- fitIht(train, setIterativeHardThresholding(K = 3, seed = 42))
  trainRows <- model$prediction[model$prediction$evaluationType == "Train", ]
  newData <- predictIht(model, data = train, cohort = train$labels)
  expect_equal(newData$value[match(trainRows$rowId, newData$rowId)], trainRows$value, tolerance = 1e-12)

  dir <- tempfile("ihtModel")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  savePlpModel(model, dir)
  loaded <- loadPlpModel(dir)
  expect_equal(loaded$model$coefficients, model$model$coefficients)
  expect_equal(predictIht(loaded, data = train, cohort = train$labels)$value, newData$value)
})

test_that("IHT settings survive saving as JSON", {
  skip_if_not_installed("IterativeHardThresholding", minimum_version = "2.0.0")
  skip_on_cran()
  settings <- setIterativeHardThresholding(
    K = c(1, 2), variance = Inf, control = list(maxIterations = 50), oneStandardError = TRUE, seed = 42
  )
  loaded <- ParallelLogger::convertJsonToSettings(ParallelLogger::convertSettingsToJson(settings))
  train <- makeIhtFixture()
  on.exit(Andromeda::close(train$covariateData))
  model <- fitIht(train, loaded)
  original <- fitIht(train, settings)
  expect_equal(as.numeric(model$trainDetails$finalModelParameters$variance), Inf)
  expect_equal(model$trainDetails$finalModelParameters$K, original$trainDetails$finalModelParameters$K)
  expect_equal(cvRows(model)$value, cvRows(original)$value)
  expect_equal(model$model$coefficients, original$model$coefficients)
})

test_that("an IHT candidate with more covariates than the data have is skipped", {
  skip_if_not_installed("IterativeHardThresholding", minimum_version = "2.0.0")
  skip_on_cran()
  train <- makeIhtFixture(p = 6L)
  on.exit(Andromeda::close(train$covariateData))
  expect_warning(
    model <- fitIht(train, setIterativeHardThresholding(K = c(2, 7), seed = 42)),
    class = "plpCandidateWarning"
  )
  search <- model$trainDetails$hyperParamSearch
  expect_true(is.na(search[[2]]$cvPerformance))
  expect_match(search[[2]]$error, "more than the 6 penalized covariates")
  expect_equal(model$trainDetails$finalModelParameters$K, 2L)
})

test_that("IHT is tuned on another metric when the hyperparameter settings name one", {
  skip_if_not_installed("IterativeHardThresholding", minimum_version = "2.0.0")
  skip_on_cran()
  train <- makeIhtFixture()
  on.exit(Andromeda::close(train$covariateData))
  model <- fitIht(train, setIterativeHardThresholding(K = c(1, 2), seed = 42), tuningMetric = aucMetric)
  expect_equal(model$trainDetails$hyperParamSearch[[1]]$metric, "AUC")
})

test_that("the one-standard-error selector takes the smallest K close to the best", {
  entry <- function(K, variance, folds) {
    list(param = list(K = K, variance = variance), cvPerformance = mean(folds), cvPerformancePerFold = folds)
  }
  history <- list(
    entry(1, 1, c(-0.70, -0.72, -0.68)),
    entry(5, 1, c(-0.66, -0.70, -0.65)),
    entry(10, 1, c(-0.65, -0.69, -0.64)),
    entry(5, Inf, c(-0.655, -0.695, -0.645))
  )
  metric <- list(maximize = TRUE)
  # best: K = 10 (-0.66), se 0.0153; K = 5 at both variances are within, K = 1 is not
  expect_equal(ihtOneStandardErrorSelector(history, metric), 4L)
  history[[1]]$cvPerformance <- NA_real_
  expect_equal(ihtOneStandardErrorSelector(history, metric), 4L)
  expect_equal(ihtOneStandardErrorSelector(history[3], metric), 1L)
})

test_that("a fold starts from the nearest earlier fit", {
  names <- c("(Intercept)", "1", "2", "3", "4")
  state <- list(
    "1 1" = list(K = 1, variance = 1, intercept = -1, columnId = 2L, coefficient = 0.5),
    "3 1" = list(K = 3, variance = 1, intercept = -2, columnId = c(1L, 2L, 4L), coefficient = c(1, 2, 3)),
    "1 Inf" = list(K = 1, variance = Inf, intercept = -3, columnId = 3L, coefficient = 4)
  )
  expect_null(ihtStartingCoefficients(NULL, 2, 1, names))
  # same variance, the largest K not above 2
  expect_equal(ihtStartingCoefficients(state, 2, 1, names), c(-1, 0, 0.5, 0, 0))
  # same variance; only larger K: the nearest
  expect_equal(ihtStartingCoefficients(state[2], 2, 1, names), c(-2, 1, 2, 0, 3))
  # the nearest variance
  expect_equal(ihtStartingCoefficients(state, 2, 1e6, names), c(-3, 0, 0, 4, 0))
})
