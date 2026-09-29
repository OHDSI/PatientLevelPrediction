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
makeTransferFixture <- function(nfold = 3) {
  set.seed(43)
  n <- 300L
  x <- cbind(rbinom(n, 1, 0.4), rbinom(n, 1, 0.6))
  y <- rbinom(n, 1, plogis(-1 + x[, 1] - 2 * x[, 2]))
  covariateData <- Andromeda::andromeda(
    covariates = data.frame(
      rowId = rep(seq_len(n), 2),
      covariateId = rep(c(10, 20), each = n),
      covariateValue = as.vector(x)
    ),
    covariateRef = data.frame(
      covariateId = c(10, 20), covariateName = c("a", "b"),
      analysisId = 1L, conceptId = c(10, 20)
    ),
    analysisRef = data.frame(
      analysisId = 1L, analysisName = "test", domainId = "Condition", isBinary = "Y"
    )
  )
  class(covariateData) <- "CovariateData"
  attr(covariateData, "metaData") <- list(populationSize = n)
  labels <- data.frame(
    rowId = seq_len(n), subjectId = seq_len(n), outcomeCount = y,
    survivalTime = 365, cohortStartDate = as.Date("2000-01-01"),
    daysToCohortEnd = 365, daysToObsEnd = 365, ageYear = 40, gender = 1
  )
  train <- list(
    covariateData = covariateData, labels = labels,
    folds = data.frame(rowId = seq_len(n), index = rep(seq_len(nfold), length.out = n))
  )
  class(train) <- "plpData"
  attr(train, "metaData") <- list(
    targetId = 1L, outcomeId = 2L,
    populationSettings = createStudyPopulationSettings(),
    covariateSettings = FeatureExtraction::createCovariateSettings(useDemographicsAge = TRUE),
    splitSettings = createDefaultSplitSetting(splitSeed = 42),
    cdmDatabaseId = "synthetic", cdmDatabaseName = "synthetic"
  )
  train
}

test_that("transfer coefficients match IDs without mutating caller data", {
  train <- makeTransferFixture(nfold = 1)
  on.exit(Andromeda::close(train$covariateData))
  before <- dplyr::collect(train$covariateData$covariates)
  source <- data.frame(
    covariateIds = c("10", "20", "30", "(Intercept)"), betas = c(1, -2, 0.7, -6)
  )
  settings <- setLassoLogisticRegression(
    priorCoefs = source, variance = 0.001,
    threads = 1, seed = 42
  )
  fit <- fitCyclopsModel(train, settings, analysisId = "test")
  settings$param$priorCoefs <- source[c(3, 2, 4, 1), ]
  shuffled <- fitCyclopsModel(train, settings, analysisId = "test")
  expect_equal(fit$prediction$value, shuffled$prediction$value, tolerance = 1e-8)
  expect_equal(dplyr::collect(train$covariateData$covariates), before)
  coefs <- fit$model$coefficients
  expect_equal(coefs$betas[match(c("10", "20", "30"), coefs$covariateIds)], c(1, -2, 0.7))
  expect_false(any(grepl("^-", coefs$covariateIds)))
  expect_false(coefs$betas[coefs$covariateIds == "(Intercept)"] == -6)

  # A previously absent source covariate must contribute when it appears later.
  baseline <- predictPlp(fit, train, train$labels)
  Andromeda::appendToTable(train$covariateData$covariates, data.frame(
    rowId = 1L, covariateId = 30, covariateValue = 2
  ))
  withSourceCovariate <- predictPlp(fit, train, train$labels)
  expect_equal(
    withSourceCovariate$rawValue - baseline$rawValue,
    ifelse(baseline$rowId == 1, 1.4, 0), tolerance = 1e-8
  )
})

test_that("a single training fold uses the supplied variance without CV predictions", {
  train <- makeTransferFixture(nfold = 1)
  on.exit(Andromeda::close(train$covariateData))
  source <- data.frame(covariateIds = c("10", "20"), betas = c(1, -2))
  settings <- setLassoLogisticRegression(
    priorCoefs = source, variance = 0.123456, lowerLimit = 1, upperLimit = 2,
    threads = 1, seed = 42
  )
  fit <- fitCyclopsModel(train, settings, analysisId = "test")
  expect_equal(fit$model$priorVariance, 0.123456)
  expect_identical(unique(fit$prediction$evaluationType), "Train")
  expect_equal(nrow(fit$trainDetails$hyperParamSearch), 0)

})

test_that("native CV transfer fits preserve caller data and source predictions", {
  train <- makeTransferFixture()
  on.exit(Andromeda::close(train$covariateData))
  before <- dplyr::collect(train$covariateData$covariates)
  settings <- setLassoLogisticRegression(
    priorCoefs = data.frame(covariateIds = c("10", "20"), betas = c(1, -2)),
    variance = 0.001, lowerLimit = 0.001, upperLimit = 0.001, threads = 1, seed = 42
  )
  fit <- fitCyclopsModel(train, settings, analysisId = "test")
  cv <- fit$prediction[fit$prediction$evaluationType == "CV", ]
  expect_equal(nrow(cv), nrow(train$labels))
  expect_equal(dplyr::collect(train$covariateData$covariates), before)

  # Strong shrinkage leaves corrections at zero. Within each fold, subtracting
  # the known source contribution must leave only its fitted target intercept.
  sourceContribution <- before %>%
    dplyr::mutate(contribution = .data$covariateValue * ifelse(.data$covariateId == 10, 1, -2)) %>%
    dplyr::group_by(.data$rowId) %>%
    dplyr::summarise(value = sum(.data$contribution))
  residual <- cv$rawValue - sourceContribution$value[match(cv$rowId, sourceContribution$rowId)]
  fold <- train$folds$index[match(cv$rowId, train$folds$rowId)]
  for (i in 1:3) {
    expect_lt(diff(range(residual[fold == i])), 1e-8)
  }
})

test_that("native CV refits retain explicitly fixed source coefficients", {
  train <- makeTransferFixture()
  on.exit(Andromeda::close(train$covariateData))
  covariates <- dplyr::collect(train$covariateData$covariates)
  duplicates <- covariates
  duplicates$covariateId <- -duplicates$covariateId
  labels <- train$labels
  labels$y <- labels$outcomeCount
  cyclopsData <- Cyclops::convertToCyclopsData(
    labels, rbind(covariates, duplicates), modelType = "lr", quiet = TRUE
  )
  ids <- cyclopsData$coefficientNames
  start <- numeric(length(ids))
  start[match(c("-10", "-20"), ids)] <- c(1, -2)
  cv <- getCV(
    cyclopsData, labels,
    Cyclops::createPrior("laplace", variance = 0.1, exclude = c(0, -10, -20)),
    train$folds, fixedCoefficients = ids %in% c("-10", "-20"),
    startingCoefficients = start, forceNewObject = TRUE,
    control = Cyclops::createControl(threads = 1, noiseLevel = "silent")
  )
  for (fold in cv) {
    expect_equal(unname(fold$coef[c("-10", "-20")]), c(1, -2))
  }
})

test_that("CV predictions are invariant to label and fold row order", {
  train <- makeTransferFixture()
  on.exit(Andromeda::close(train$covariateData))
  for (source in list(NULL, data.frame(covariateIds = c("10", "20"), betas = c(1, -2)))) {
    settings <- setLassoLogisticRegression(
      priorCoefs = source, variance = 1, lowerLimit = 1, upperLimit = 1,
      threads = 1, seed = 42
    )
    fit <- fitCyclopsModel(train, settings, analysisId = "test")
    set.seed(123)
    shuffledTrain <- train
    shuffledTrain$labels <- train$labels[sample(nrow(train$labels)), ]
    shuffledTrain$folds <- train$folds[sample(nrow(train$folds)), ]
    shuffled <- fitCyclopsModel(shuffledTrain, settings, analysisId = "test")
    cv <- fit$prediction[fit$prediction$evaluationType == "CV", ]
    shuffledCv <- shuffled$prediction[shuffled$prediction$evaluationType == "CV", ]
    expect_equal(cv$value, shuffledCv$value[match(cv$rowId, shuffledCv$rowId)], tolerance = 1e-8)
  }
})

test_that("explicit covariate selection also filters transferred source coefficients", {
  train <- makeTransferFixture(nfold = 1)
  on.exit(Andromeda::close(train$covariateData))
  source <- data.frame(covariateIds = c("10", "20", "30"), betas = c(1, -2, 0.7))
  settings <- setLassoLogisticRegression(
    priorCoefs = source, includeCovariateIds = c(10, 30),
    variance = 0.001, threads = 1, seed = 42
  )
  fit <- fitCyclopsModel(train, settings, analysisId = "test")
  expect_false("20" %in% fit$model$coefficients$covariateIds)
  expect_equal(fit$model$coefficients$betas[fit$model$coefficients$covariateIds == "30"], 0.7)

  # Removing excluded source terms before fitting must give the same predictor.
  settings$param$priorCoefs <- source[source$covariateIds != "20", ]
  reference <- fitCyclopsModel(train, settings, analysisId = "test")
  expect_equal(fit$prediction$value, reference$prediction$value, tolerance = 1e-8)
  settings$param$priorCoefs <- source
  settings$param$includeCovariateIds <- NULL
  settings$param$excludeCovariateIds <- 20
  excluded <- fitCyclopsModel(train, settings, analysisId = "test")
  expect_equal(excluded$prediction$value, reference$prediction$value, tolerance = 1e-8)
  expect_false("20" %in% excluded$model$coefficients$covariateIds)
})

test_that("zero source equals target-only and invalid coefficients leave data intact", {
  train <- makeTransferFixture(nfold = 1)
  on.exit(Andromeda::close(train$covariateData))
  before <- dplyr::collect(train$covariateData$covariates)
  settings <- setLassoLogisticRegression(
    variance = 0.1, threads = 1, seed = 42
  )
  baseline <- fitCyclopsModel(train, settings, analysisId = "test")
  settings$param$priorCoefs <- data.frame(covariateIds = "10", betas = 0)
  transfer <- fitCyclopsModel(train, settings, analysisId = "test")
  expect_equal(transfer$prediction$value, baseline$prediction$value, tolerance = 1e-5)
  for (source in list(
    data.frame(covariateIds = c("10", "10"), betas = 1),
    data.frame(covariateIds = "10", betas = Inf),
    data.frame(covariateIds = NA_character_, betas = 1)
  )) {
    settings$param$priorCoefs <- source
    expect_error(fitCyclopsModel(train, settings, analysisId = "test"), "unique IDs and finite betas")
    expect_equal(dplyr::collect(train$covariateData$covariates), before)
  }
})

test_that("ridge transfer also matches source coefficients by ID", {
  train <- makeTransferFixture(nfold = 1)
  on.exit(Andromeda::close(train$covariateData))
  source <- data.frame(covariateIds = c("10", "20"), betas = c(1, -2))
  settings <- setRidgeRegression(priorCoefs = source, variance = 0.001, threads = 1, seed = 42)
  fit <- fitCyclopsModel(train, settings, analysisId = "test")
  settings$param$priorCoefs <- source[2:1, ]
  shuffled <- fitCyclopsModel(train, settings, analysisId = "test")
  expect_equal(fit$prediction$value, shuffled$prediction$value, tolerance = 1e-8)
})

test_that("transfer predictions do not depend on covariate ID formatting", {
  train <- makeTransferFixture()
  on.exit(Andromeda::close(train$covariateData))
  covariates <- dplyr::collect(train$covariateData$covariates)
  covariates$covariateId <- covariates$covariateId * 1e8
  train$covariateData$covariates <- covariates
  covariateRef <- dplyr::collect(train$covariateData$covariateRef)
  covariateRef$covariateId <- covariateRef$covariateId * 1e8
  train$covariateData$covariateRef <- covariateRef
  source <- data.frame(covariateIds = c(1e9, 2e9, 3e9), betas = c(1, -2, 0.7))
  ids <- c("1000000000", "2000000000", "3000000000")
  for (nfold in c(1, 3)) {
    train$folds$index <- rep(seq_len(nfold), length.out = nrow(train$labels))
    for (variance in c(0.001, 1)) {
      settings <- setLassoLogisticRegression(
        priorCoefs = source, includeCovariateIds = source$covariateIds,
        variance = variance, lowerLimit = variance, upperLimit = variance,
        threads = 1, seed = 42
      )
      reference <- fitCyclopsModel(train, settings, analysisId = "test")
      settings$param$priorCoefs$covariateIds <- c(ids[1], "2e+09", ids[3])
      fit <- fitCyclopsModel(train, settings, analysisId = "test")
      expect_equal(fit$prediction, reference$prediction, tolerance = 1e-8)
      expect_equal(fit$model$coefficients, reference$model$coefficients, tolerance = 1e-8)
      coefs <- fit$model$coefficients
      expect_setequal(coefs$covariateIds, c("(Intercept)", ids))
      expect_equal(nrow(coefs), 4)
      expect_equal(coefs$betas[coefs$covariateIds == ids[3]], 0.7)
      if (variance == 0.001) {
        expect_equal(coefs$betas[match(ids, coefs$covariateIds)], source$betas)
      } else {
        expect_gt(max(abs(coefs$betas[match(ids, coefs$covariateIds)] - source$betas)), 0.01)
      }
    }
  }

  settings$param$includeCovariateIds <- c(1e9, 3e9)
  included <- fitCyclopsModel(train, settings, analysisId = "test")
  settings$param$includeCovariateIds <- NULL
  settings$param$excludeCovariateIds <- 2e9
  settings$param$priorCoefs$covariateIds <- ids
  excluded <- fitCyclopsModel(train, settings, analysisId = "test")
  expect_equal(included$prediction, excluded$prediction, tolerance = 1e-8)
  expect_setequal(excluded$model$coefficients$covariateIds, c("(Intercept)", ids[c(1, 3)]))

  withr::local_options(scipen = 999)
  decimal <- fitCyclopsModel(train, settings, analysisId = "test")
  expect_equal(decimal$prediction, excluded$prediction, tolerance = 1e-8)
  expect_equal(decimal$model$coefficients, excluded$model$coefficients, tolerance = 1e-8)
})

test_that("transfer ID validation detects aliases without rounding large IDs", {
  expect_error(
    createTransferMap(
      priorCoefs = data.frame(covariateIds = c("1e+09", "1000000000"), betas = c(1, 2)),
      covariateIds = 1e9
    ),
    "unique IDs and finite betas"
  )
  ids <- c("9007199254740992", "9007199254740993")
  map <- createTransferMap(
    priorCoefs = data.frame(covariateIds = ids, betas = c(1, 2)),
    covariateIds = ids,
    includeCovariateIds = ids
  )
  combined <- reparamTransferCoefs(
    inCoefs = data.frame(betas = c(0.1, 0.2), covariateIds = ids),
    transferMap = map
  )
  expect_equal(combined$covariateIds, ids)
  expect_equal(combined$betas, c(1.1, 2.2))
  expect_error(
    createTransferMap(
      priorCoefs = data.frame(covariateIds = "9.007199254740993e15", betas = 1),
      covariateIds = ids
    ),
    "full integer strings"
  )
})
