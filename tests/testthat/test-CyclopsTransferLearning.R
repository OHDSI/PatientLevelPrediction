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
  withr::local_seed(43)
  n <- 300L
  x <- cbind(rbinom(n, 1, 0.4), rbinom(n, 1, 0.6))
  y <- rbinom(n, 1, plogis(-1 + x[, 1] - 2 * x[, 2]))
  covariateData <- makeCovariateData(
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
    ),
    metaData = list(populationSize = n)
  )
  labels <- data.frame(
    rowId = seq_len(n), subjectId = seq_len(n), outcomeCount = y,
    survivalTime = 365, cohortStartDate = as.Date("2000-01-01"),
    daysToCohortEnd = 365, daysToObsEnd = 365, ageYear = 40, gender = 1
  )
  makePlpData(
    covariateData = covariateData, labels = labels,
    folds = data.frame(rowId = seq_len(n), index = rep(seq_len(nfold), length.out = n)),
    metaData = syntheticTrainMetaData()
  )
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
  messages <- character()
  fit <- with_mocked_bindings(
    fitCyclopsModel(train, settings, analysisId = "test"),
    logInfo = function(...) messages <<- c(messages, paste0(..., collapse = "")),
    .package = "ParallelLogger"
  )
  expect_true(
    "Source covariates absent from target training data: 1. Dropping their coefficients." %in% messages
  )
  settings$param$priorCoefs <- source[c(3, 2, 4, 1), ]
  shuffled <- fitCyclopsModel(train, settings, analysisId = "test")
  expect_equal(fit$prediction$value, shuffled$prediction$value, tolerance = 1e-8)
  expect_equal(dplyr::collect(train$covariateData$covariates), before)
  coefs <- fit$model$coefficients
  expect_equal(coefs$betas[match(c("10", "20"), coefs$covariateIds)], c(1, -2))
  expect_false("30" %in% coefs$covariateIds)
  expect_false(30 %in% fit$covariateImportance$covariateId)
  expect_false(any(grepl("^-", coefs$covariateIds)))
  expect_false(coefs$betas[coefs$covariateIds == "(Intercept)"] == -6)

  # Dropped source covariates must not contribute in new prediction data.
  baseline <- predictPlp(fit, train, train$labels)
  Andromeda::appendToTable(train$covariateData$covariates, data.frame(
    rowId = 1L, covariateId = 30, covariateValue = 2
  ))
  withSourceCovariate <- predictPlp(fit, train, train$labels)
  expect_equal(
    withSourceCovariate$rawValue, baseline$rawValue, tolerance = 1e-8
  )
})

test_that("no overlapping source covariates gives a target-only model", {
  train <- makeTransferFixture()
  on.exit(Andromeda::close(train$covariateData))
  before <- dplyr::collect(train$covariateData$covariates)
  for (nfold in c(1, 3)) {
    train$folds$index <- rep(seq_len(nfold), length.out = nrow(train$labels))
    settings <- setLassoLogisticRegression(
      variance = 1,
      threads = 1, seed = 42, tolerance = 1e-10
    )
    reference <- fitCyclopsModel(train, settings, analysisId = "test")
    settings$param$priorCoefs <- data.frame(covariateIds = "30", betas = 0.7)
    transfer <- fitCyclopsModel(train, settings, analysisId = "test")
    expect_equal(transfer$prediction, reference$prediction, tolerance = 1e-6)
    expect_false("30" %in% transfer$model$coefficients$covariateIds)
    expect_false(30 %in% transfer$covariateImportance$covariateId)
    expect_equal(dplyr::collect(train$covariateData$covariates), before)
  }
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

  # the variance is used in the fit, not only stored: a wide prior moves the
  # coefficients away from the source (a narrow one keeps them, see the first test)
  wide <- setLassoLogisticRegression(priorCoefs = source, variance = 1, threads = 1, seed = 42)
  coefs <- fitCyclopsModel(train, wide, analysisId = "test")$model$coefficients
  expect_gt(max(abs(coefs$betas[match(c("10", "20"), coefs$covariateIds)] - source$betas)), 0.01)
})

test_that("native CV transfer fits preserve caller data and source predictions", {
  train <- makeTransferFixture()
  on.exit(Andromeda::close(train$covariateData))
  before <- dplyr::collect(train$covariateData$covariates)
  settings <- setLassoLogisticRegression(
    priorCoefs = data.frame(covariateIds = c("10", "20", "30"), betas = c(1, -2, 0.7)),
    variance = 0.001, threads = 1, seed = 42
  )
  fit <- fitCyclopsModel(train, settings, analysisId = "test")
  cv <- fit$prediction[fit$prediction$evaluationType == "CV", ]
  expect_equal(nrow(cv), nrow(train$labels))
  expect_false("30" %in% fit$model$coefficients$covariateIds)
  expect_equal(dplyr::collect(train$covariateData$covariates), before)

  # Refit each training fold at the variance selected by CV.
  settings$param$priorParams$variance <- fit$model$priorVariance
  foldTrain <- makeTransferFixture(nfold = 1)
  on.exit(Andromeda::close(foldTrain$covariateData), add = TRUE)
  for (i in 1:3) {
    holdout <- train$folds$rowId[train$folds$index == i]
    foldTrain$labels <- train$labels[!train$labels$rowId %in% holdout, ]
    foldTrain$folds <- data.frame(rowId = foldTrain$labels$rowId, index = 1L)
    foldTrain$covariateData$covariates <- before[!before$rowId %in% holdout, ]
    foldFit <- fitCyclopsModel(foldTrain, settings, analysisId = "test")
    expected <- predictCyclops(
      plpModel = foldFit, data = train,
      cohort = train$labels[train$labels$rowId %in% holdout, ]
    )
    expect_equal(
      cv$rawValue[match(expected$rowId, cv$rowId)], expected$rawValue,
      tolerance = 1e-8
    )
  }
})

test_that("transfer fits the source as an unpenalized column held at 1", {
  # the reference: Cyclops with the source linear predictor as a column whose
  # coefficient is fixed at 1 and not penalized, the definition of the model
  train <- makeTransferFixture(nfold = 1)
  on.exit(Andromeda::close(train$covariateData))
  source <- data.frame(covariateIds = c("10", "20"), betas = c(0.8, -1.5))
  settings <- setLassoLogisticRegression(priorCoefs = source, variance = 0.05, threads = 1, seed = 42)
  fit <- fitCyclopsModel(train, settings, analysisId = "test")

  covariates <- dplyr::collect(train$covariateData$covariates)
  sourceLp <- stats::aggregate(
    list(covariateValue = covariates$covariateValue * source$betas[match(covariates$covariateId, c(10, 20))]),
    list(rowId = covariates$rowId), sum
  )
  sourceLp$covariateId <- -5
  outcomes <- data.frame(rowId = train$labels$rowId, y = train$labels$outcomeCount)
  cyclopsData <- Cyclops::convertToCyclopsData(
    outcomes, rbind(covariates, sourceLp[, names(covariates)]),
    modelType = "lr", addIntercept = TRUE, quiet = TRUE
  )
  fixed <- cyclopsData$coefficientNames == "-5"
  reference <- Cyclops::fitCyclopsModel(
    cyclopsData,
    prior = Cyclops::createPrior("laplace", variance = 0.05, exclude = c(0, -5)),
    fixedCoefficients = fixed, startingCoefficients = as.numeric(fixed),
    control = Cyclops::createControl(threads = 1, noiseLevel = "silent", tolerance = 1e-10)
  )
  changes <- stats::coef(reference)[!fixed]
  coefs <- fit$model$coefficients
  expect_equal(
    coefs$betas[match(c("10", "20"), coefs$covariateIds)],
    unname(changes[c("10", "20")] + source$betas),
    tolerance = 1e-5
  )
  expect_equal(
    coefs$betas[coefs$covariateIds == "(Intercept)"], unname(changes["(Intercept)"]),
    tolerance = 1e-5
  )
  # the offset does not appear as a coefficient
  expect_false(any(as.numeric(coefs$covariateIds[coefs$covariateIds != "(Intercept)"]) < 0))
})

test_that("transfer with cross-validation does not depend on the number of threads", {
  # Cyclops 3.7.1 loses fixed coefficients in the solver copies it makes for
  # extra threads, so source coefficients held fixed in copied columns became
  # free on some folds (here the selected variance went from 1.1 to 5e14 on 3
  # threads). The offset is part of the data, so every copy keeps it.
  withr::local_seed(7)
  n <- 2000L
  p <- 30L
  x <- matrix(rbinom(n * p, 1, 0.15), n, p)
  target <- c(rnorm(10, 0, 0.8), rep(0, p - 10))
  # a related but imperfect source, so CV has a clear optimum
  source <- target + c(rnorm(10, 0, 0.4), rnorm(p - 10, 0, 0.3))
  y <- rbinom(n, 1, plogis(-1.5 + x %*% target))
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
  train <- makePlpData(
    covariateData = covariateData, labels = labels,
    folds = data.frame(rowId = seq_len(n), index = rep(1:3, length.out = n)),
    metaData = syntheticTrainMetaData()
  )
  on.exit(Andromeda::close(train$covariateData))
  priorCoefs <- data.frame(covariateIds = as.character(ids), betas = source)
  fits <- lapply(c(1, 3), function(threads) {
    settings <- setLassoLogisticRegression(priorCoefs = priorCoefs, threads = threads, seed = 42)
    suppressWarnings(fitCyclopsModel(train, settings, analysisId = "test"))
  })
  # Cyclops 3.7.1 differs slightly between thread counts also without transfer
  # (here 7e-5 relative); 4.0 gives the same result
  expect_equal(fits[[2]]$model$priorVariance, fits[[1]]$model$priorVariance, tolerance = 1e-3)
  expect_equal(fits[[2]]$model$coefficients, fits[[1]]$model$coefficients, tolerance = 1e-3)
  expect_equal(fits[[2]]$prediction$value, fits[[1]]$prediction$value, tolerance = 1e-3)
})

test_that("CV predictions are invariant to label and fold row order", {
  train <- makeTransferFixture()
  on.exit(Andromeda::close(train$covariateData))
  for (source in list(NULL, data.frame(covariateIds = c("10", "20"), betas = c(1, -2)))) {
    settings <- setLassoLogisticRegression(
      priorCoefs = source, variance = 1,
      threads = 1, seed = 42
    )
    fit <- fitCyclopsModel(train, settings, analysisId = "test")
    withr::local_seed(123)
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
  expect_false("30" %in% fit$model$coefficients$covariateIds)

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
  source <- data.frame(covariateIds = c("10", "20", "30"), betas = c(1, -2, 0.7))
  settings <- setRidgeRegression(priorCoefs = source, variance = 0.001, threads = 1, seed = 42)
  fit <- fitCyclopsModel(train, settings, analysisId = "test")
  settings$param$priorCoefs <- source[3:1, ]
  shuffled <- fitCyclopsModel(train, settings, analysisId = "test")
  expect_equal(fit$prediction$value, shuffled$prediction$value, tolerance = 1e-8)
  expect_false("30" %in% fit$model$coefficients$covariateIds)
  expect_false(30 %in% fit$covariateImportance$covariateId)
})

test_that("transfer covariate IDs normalize the same whatever their format", {
  expected <- c("1000000000", "2000000000", "3000000000")
  for (scipen in c(0, 999)) {
    withr::with_options(list(scipen = scipen), {
      expect_equal(normalizeTransferCovariateIds(c(1e9, 2e9, 3e9)), expected)
      expect_equal(normalizeTransferCovariateIds(c("1000000000", "2e+09", "3e9")), expected)
      expect_equal(normalizeTransferCovariateIds(c("+01000000000", "2000000000", "3000000000")), expected)
    })
  }
  numericMap <- createTransferMap(
    priorCoefs = data.frame(covariateIds = c(1e9, 2e9, 3e9), betas = c(1, -2, 0.7)),
    covariateIds = c(1e9, 2e9),
    excludeCovariateIds = 3e9
  )
  textMap <- createTransferMap(
    priorCoefs = data.frame(covariateIds = c("1000000000", "2e+09", "3000000000"), betas = c(1, -2, 0.7)),
    covariateIds = c(1e9, 2e9),
    excludeCovariateIds = "3e+09"
  )
  expect_equal(textMap, numericMap)
  expect_equal(numericMap$covariateIds, expected[1:2])
})

test_that("transfer predictions do not depend on covariate ID formatting", {
  # three folds: the cross-validation path maps IDs for every fold as well as
  # for the final fit, so it covers the single-fold path too
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
  settings <- setLassoLogisticRegression(
    priorCoefs = source, includeCovariateIds = source$covariateIds,
    variance = 1, threads = 1, seed = 42
  )
  reference <- fitCyclopsModel(train, settings, analysisId = "test")
  settings$param$priorCoefs$covariateIds <- c(ids[1], "2e+09", ids[3])
  fit <- fitCyclopsModel(train, settings, analysisId = "test")
  expect_equal(fit$prediction, reference$prediction, tolerance = 1e-8)
  expect_equal(fit$model$coefficients, reference$model$coefficients, tolerance = 1e-8)
  coefs <- fit$model$coefficients
  expect_setequal(coefs$covariateIds, c("(Intercept)", ids[1:2]))
  expect_equal(nrow(coefs), 3)
  expect_false(ids[3] %in% coefs$covariateIds)

  settings$param$includeCovariateIds <- c(1e9, 3e9)
  included <- fitCyclopsModel(train, settings, analysisId = "test")
  settings$param$includeCovariateIds <- NULL
  settings$param$excludeCovariateIds <- 2e9
  settings$param$priorCoefs$covariateIds <- ids
  excluded <- fitCyclopsModel(train, settings, analysisId = "test")
  expect_equal(included$prediction, excluded$prediction, tolerance = 1e-8)
  expect_setequal(excluded$model$coefficients$covariateIds, c("(Intercept)", ids[1]))

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

test_that("Cyclops coefficient names keep large covariate IDs apart under any scipen", {
  # distinct doubles whose formatted names can be the same
  ids <- c(10000000000000000, 10000000000000002)
  outcomes <- data.frame(rowId = 1:4, y = c(0, 1, 0, 1))
  covariates <- data.frame(rowId = 1:4, covariateId = rep(ids, 2), covariateValue = 1)
  for (scipen in c(0, 999)) {
    coefficientNames <- withr::with_options(list(scipen = scipen), {
      cyclopsData <- Cyclops::convertToCyclopsData(
        outcomes, covariates, modelType = "lr", addIntercept = TRUE, quiet = TRUE
      )
      getCyclopsCoefficientNames(cyclopsData)
    })
    expect_equal(
      coefficientNames, c("(Intercept)", "10000000000000000", "10000000000000002"),
      info = paste("scipen:", scipen)
    )
  }
})

test_that("large transfer covariate IDs survive Cyclops coefficient formatting", {
  # three folds, as in the test above; ridge matches IDs the same way as lasso
  train <- makeTransferFixture()
  on.exit(Andromeda::close(train$covariateData))
  covariates <- dplyr::collect(train$covariateData$covariates)
  covariateRef <- dplyr::collect(train$covariateData$covariateRef)
  ids <- c("10000000000000000", "10000000000000002")
  settings <- setLassoLogisticRegression(
    priorCoefs = data.frame(covariateIds = c("10", "20"), betas = c(1, -2)),
    variance = 1, threads = 1, seed = 42
  )
  reference <- fitCyclopsModel(train, settings, analysisId = "test")

  covariates$covariateId <- as.numeric(ids)[match(covariates$covariateId, c(10, 20))]
  covariateRef$covariateId <- as.numeric(ids)
  train$covariateData$covariates <- covariates
  train$covariateData$covariateRef <- covariateRef
  settings$param$priorCoefs$covariateIds <- ids
  scientific <- withr::with_options(
    list(scipen = 0), fitCyclopsModel(train, settings, analysisId = "test")
  )
  decimal <- withr::with_options(
    list(scipen = 999), fitCyclopsModel(train, settings, analysisId = "test")
  )
  expect_equal(scientific$prediction, reference$prediction, tolerance = 1e-8)
  expect_equal(scientific$prediction, decimal$prediction, tolerance = 1e-8)
  expect_equal(scientific$model$coefficients, decimal$model$coefficients, tolerance = 1e-8)
  expect_setequal(scientific$model$coefficients$covariateIds, c("(Intercept)", ids))
  expect_equal(nrow(scientific$model$coefficients), 3)
  expect_equal(
    scientific$model$coefficients$betas, reference$model$coefficients$betas,
    tolerance = 1e-8
  )
})

test_that("transfer matches the penalized likelihood with the source as offset on continuous data", {
  # continuous values, patients without any covariates (so without an offset
  # row), large IDs, a source covariate absent from the target and a target
  # covariate absent from the source; checked against optim()
  withr::local_seed(1)
  n <- 600L
  p <- 6L
  ids <- c(2^40 + 1002, 2^40 + 2002, 3002, 4002, 5002, 6002)
  x <- matrix(0, n, p)
  for (j in seq_len(p)) {
    on <- runif(n) < 0.4
    x[on, j] <- if (j %% 2 == 0) round(runif(sum(on), 0.1, 3), 3) else 1
  }
  x[1:80, ] <- 0
  sourceBetas <- c(0.9, -0.6, 0.4, 0, 0, 0.3)
  y <- rbinom(n, 1, plogis(-1 + x %*% c(1.1, -0.4, 0.2, 0.5, 0, 0.1)))
  nonZero <- which(x != 0, arr.ind = TRUE)
  covariateData <- makeCovariateData(
    covariates = data.frame(rowId = nonZero[, 1], covariateId = ids[nonZero[, 2]], covariateValue = x[nonZero]),
    covariateRef = data.frame(covariateId = ids, covariateName = paste0("c", 1:p), analysisId = 1L, conceptId = 1:p),
    analysisRef = data.frame(analysisId = 1L, analysisName = "test", domainId = "Condition", isBinary = "N"),
    metaData = list(populationSize = n)
  )
  labels <- data.frame(
    rowId = seq_len(n), subjectId = seq_len(n), outcomeCount = y, survivalTime = 365,
    cohortStartDate = as.Date("2000-01-01"), daysToCohortEnd = 365, daysToObsEnd = 365,
    ageYear = 40, gender = 1
  )
  train <- makePlpData(
    covariateData = covariateData, labels = labels,
    folds = data.frame(rowId = seq_len(n), index = 1L), metaData = syntheticTrainMetaData()
  )
  on.exit(Andromeda::close(train$covariateData))
  tablesBefore <- names(train$covariateData)
  covariatesBefore <- dplyr::collect(train$covariateData$covariates)
  # "999" is not in the target; the source intercept is not transferred
  source <- data.frame(
    covariateIds = c(format(ids[c(1, 2, 3, 6)], scientific = FALSE, trim = TRUE), "999", "(Intercept)"),
    betas = c(sourceBetas[c(1, 2, 3, 6)], 2, -3)
  )
  variance <- 0.3
  settings <- setRidgeRegression(priorCoefs = source, variance = variance, threads = 1, seed = 1, tolerance = 1e-12)
  fit <- fitCyclopsModel(train, settings, analysisId = "test")
  expect_identical(names(train$covariateData), tablesBefore)
  expect_equal(dplyr::collect(train$covariateData$covariates), covariatesBefore)

  offset <- x %*% sourceBetas
  penalizedNll <- function(par) {
    eta <- par[1] + offset + x %*% par[-1]
    -sum(y * eta - log1p(exp(eta))) + sum(par[-1]^2) / (2 * variance)
  }
  gradient <- function(par) {
    residual <- y - plogis(par[1] + offset + x %*% par[-1])
    -c(sum(residual), t(x) %*% residual - par[-1] / variance)
  }
  optimum <- stats::optim(
    rep(0, p + 1), penalizedNll, gradient,
    method = "BFGS", control = list(reltol = 1e-14, maxit = 1000)
  )$par
  expected <- c(optimum[1], optimum[-1] + sourceBetas)
  coefs <- fit$model$coefficients
  actual <- c(
    coefs$betas[coefs$covariateIds == "(Intercept)"],
    coefs$betas[match(format(ids, scientific = FALSE, trim = TRUE), coefs$covariateIds)]
  )
  expect_equal(actual, expected, tolerance = 1e-6)
  expect_false("999" %in% coefs$covariateIds)
  expect_equal(
    fit$prediction$value[match(seq_len(n), fit$prediction$rowId)],
    as.vector(plogis(expected[1] + x %*% expected[-1])),
    tolerance = 1e-6
  )
})

test_that("a transfer fit that fails does not take the source coefficients", {
  train <- makeTransferFixture(nfold = 1)
  on.exit(Andromeda::close(train$covariateData))
  settings <- setLassoLogisticRegression(
    priorCoefs = data.frame(covariateIds = c("10", "20"), betas = c(1, -2)),
    variance = 1, threads = 1, seed = 42, maxIterations = 1
  )
  fit <- suppressWarnings(fitCyclopsModel(train, settings, analysisId = "test"))
  expect_match(fit$model$modelStatus, "MAXIMUM NUMBER OF ITERATIONS")
  expect_false(any(c("10", "20") %in% fit$model$coefficients$covariateIds))
  # as a failed fit without transfer: the same risk for everyone
  expect_length(unique(round(fit$prediction$value, 12)), 1)
})
