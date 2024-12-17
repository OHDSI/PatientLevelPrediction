# Copyright 2021 Observational Health Data Sciences and Informatics
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

library("testthat")

context("CyclopsModels")


# ================ SETTING TESTINGS


test_that("set LR inputs", {
  # =====================================
  # checking Logistic Regression
  # =====================================

  model_set <- setLassoLogisticRegression()
  testthat::expect_that(model_set, testthat::is_a("modelSettings"))

  expect_equal(model_set$fitFunction, "fitCyclopsModel")
  expect_is(model_set$param, "list")

  expect_equal(model_set$param$priorParams$priorType, "laplace")

  expect_equal(attr(model_set$param, "settings")$modelType, "logistic")
  expect_equal(attr(model_set$param, "settings")$priorfunction, "Cyclops::createPrior")
  expect_equal(attr(model_set$param, "settings")$addIntercept, T)
  expect_equal(attr(model_set$param, "settings")$useControl, T)
  expect_equal(attr(model_set$param, "settings")$name, "Lasso Logistic Regression")
  expect_equal(attr(model_set$param, "settings")$cvRepetitions, 1)


  variance <- runif(1)
  model_set <- setLassoLogisticRegression(variance = variance)
  expect_equal(model_set$param$priorParams$variance, variance)

  seed <- sample(10, 1)
  model_set <- setLassoLogisticRegression(seed = seed)
  expect_equal(attr(model_set$param, "settings")$seed, seed)

  model_set <- setLassoLogisticRegression(includeCovariateIds = c(1, 2))
  expect_equal(model_set$param$includeCovariateIds, c(1, 2))

  model_set <- setLassoLogisticRegression(noShrinkage = c(1, 3))
  expect_equal(model_set$param$priorParams$exclude, c(1, 3))

  threads <- sample(10, 1)
  model_set <- setLassoLogisticRegression(threads = threads)
  expect_equal(attr(model_set$param, "settings")$threads, threads)

  model_set <- setLassoLogisticRegression(forceIntercept = T)
  expect_equal(model_set$param$priorParams$forceIntercept, T)

  model_set <- setLassoLogisticRegression(upperLimit = 1)
  expect_equal(model_set$param$upperLimit, 1)

  model_set <- setLassoLogisticRegression(lowerLimit = 1)
  expect_equal(model_set$param$lowerLimit, 1)

  tolerance <- runif(1)
  model_set <- setLassoLogisticRegression(tolerance = tolerance)
  expect_equal(attr(model_set$param, "settings")$tolerance, tolerance)

  maxIterations <- sample(100, 1)
  model_set <- setLassoLogisticRegression(maxIterations = maxIterations)
  expect_equal(attr(model_set$param, "settings")$maxIterations, maxIterations)
})


test_that("set LR incorrect inputs", {
  expect_error(setLassoLogisticRegression(variance = -0.01))
  expect_error(setLassoLogisticRegression(variance = "variance"))
  expect_error(setLassoLogisticRegression(seed = "seed"))
  expect_error(setLassoLogisticRegression(threads = "threads"))

  expect_error(setLassoLogisticRegression(lowerLimit = "lowerLimit"))
  expect_error(setLassoLogisticRegression(upperLimit = "upperLimit"))
  expect_error(setLassoLogisticRegression(lowerLimit = 3, upperLimit = 1))
})




test_that("set cox regression inputs", {
  skip_if_not_installed("polspline")
  skip_on_cran()
  # =====================================
  # checking Cox Regression
  # =====================================

  modelSet <- setCoxModel()
  testthat::expect_that(modelSet, testthat::is_a("modelSettings"))

  expect_equal(modelSet$fitFunction, "fitCyclopsModel")
  expect_is(modelSet$param, "list")

  expect_equal(modelSet$param$priorParams$priorType, "laplace")

  expect_equal(attr(modelSet$param, "settings")$modelType, "cox")
  expect_equal(attr(modelSet$param, "settings")$priorfunction, "Cyclops::createPrior")
  expect_equal(attr(modelSet$param, "settings")$addIntercept, FALSE)
  expect_equal(attr(modelSet$param, "settings")$useControl, TRUE)
  expect_equal(attr(modelSet$param, "settings")$name, "LASSO Cox Regression")
  expect_equal(attr(modelSet$param, "settings")$cvRepetitions, 1)

  variance <- runif(1)
  modelSet <- setCoxModel(variance = variance)
  expect_equal(modelSet$param$priorParams$variance, variance)

  seed <- sample(10, 1)
  modelSet <- setCoxModel(seed = seed)
  expect_equal(attr(modelSet$param, "settings")$seed, seed)

  modelSet <- setCoxModel(includeCovariateIds = c(1, 2))
  expect_equal(modelSet$param$includeCovariateIds, c(1, 2))

  modelSet <- setCoxModel(upperLimit = 1)
  expect_equal(modelSet$param$upperLimit, 1)

  modelSet <- setCoxModel(lowerLimit = 1)
  expect_equal(modelSet$param$lowerLimit, 1)

  modelSet <- setCoxModel(noShrinkage = c(1, 3))
  expect_equal(modelSet$param$priorParams$exclude, c(1, 3))

  threads <- sample(10, 1)
  modelSet <- setCoxModel(threads = threads)
  expect_equal(attr(modelSet$param, "settings")$threads, threads)

  tolerance <- runif(1)
  modelSet <- setCoxModel(tolerance = tolerance)
  expect_equal(attr(modelSet$param, "settings")$tolerance, tolerance)

  maxIterations <- sample(100, 1)
  modelSet <- setCoxModel(maxIterations = maxIterations)
  expect_equal(attr(modelSet$param, "settings")$maxIterations, maxIterations)
})


test_that("set cox regression incorrect inputs", {
  skip_if_not_installed("polspline")
  skip_on_cran()
  expect_error(setCoxModel(variance = -0.01))
  expect_error(setCoxModel(variance = "variance"))
  expect_error(setCoxModel(seed = "seed"))
  expect_error(setCoxModel(threads = "threads"))

  expect_error(setCoxModel(lowerLimit = "lowerLimit"))
  expect_error(setCoxModel(upperLimit = "upperLimit"))
  expect_error(setCoxModel(lowerLimit = 3, upperLimit = 1))
})





test_that("set IHT inputs", {
  skip_if_not_installed("IterativeHardThresholding")
  skip_on_cran()
  # =====================================
  # checking IHT
  # =====================================
  modelSet <- setIterativeHardThresholding()
  testthat::expect_that(modelSet, testthat::is_a("modelSettings"))

  expect_equal(modelSet$fitFunction, "fitCyclopsModel")
  expect_is(modelSet$param, "list")

  expect_equal(attr(modelSet$param, "settings")$modelType, "logistic")
  expect_equal(attr(modelSet$param, "settings")$priorfunction, "IterativeHardThresholding::createIhtPrior")
  expect_equal(attr(modelSet$param, "settings")$addIntercept, FALSE)
  expect_equal(attr(modelSet$param, "settings")$useControl, FALSE)
  expect_equal(attr(modelSet$param, "settings")$name, "Iterative Hard Thresholding")
  expect_equal(attr(modelSet$param, "settings")$crossValidationInPrior, FALSE)

  k <- sample(100, 1)
  modelSet <- setIterativeHardThresholding(K = k)
  expect_equal(modelSet$param$priorParams$K, k)


  penalty <- sample(c("bic", "aic"), 1)
  modelSet <- setIterativeHardThresholding(penalty = penalty)
  expect_equal(modelSet$param$priorParams$penalty, penalty)

  modelSet <- setIterativeHardThresholding(exclude = c(1, 2))
  expect_equal(modelSet$param$priorParams$exclude, c(1, 2))

  modelSet <- setIterativeHardThresholding(forceIntercept = TRUE)
  expect_equal(modelSet$param$priorParams$forceIntercept, TRUE)

  modelSet <- setIterativeHardThresholding(fitBestSubset = TRUE)
  expect_equal(modelSet$param$priorParams$fitBestSubset, TRUE)

  # add other parameter checks
  ## initialRidgeVariance
  ## tolerance
  ## maxIterations
  ## threshold
  ## delta

  seed <- sample(10, 1)
  modelSet <- setIterativeHardThresholding(seed = seed)
  expect_equal(attr(modelSet$param, "settings")$seed, seed)
})


test_that("test IHT incorrect inputs", {
  skip_if_not_installed("IterativeHardThresholding")
  skip_on_cran()
  testthat::expect_error(setIterativeHardThresholding(K = 0))
  testthat::expect_error(setIterativeHardThresholding(penalty = "L1"))
  testthat::expect_error(setIterativeHardThresholding(fitBestSubset = "true"))
  testthat::expect_error(setIterativeHardThresholding(seed = "F"))
})



# ================ FUNCTION TESTING

test_that("test logistic regression runs", {
  modelSettings <- setLassoLogisticRegression()

  fitModel <- fitPlp(
    trainData = trainData,
    modelSettings = modelSettings,
    search = "grid",
    analysisId = "lrTest",
    analysisPath = tempdir()
  )

  expect_equal(length(unique(fitModel$prediction$evaluationType)), 2)
  expect_equal(nrow(fitModel$prediction), nrow(trainData$labels) * 2)
  expect_true(length(fitModel$model$coefficients) < trainData$covariateData$covariateRef %>%
    dplyr::tally() %>%
    dplyr::pull() + 1)


  expect_true(!is.null(fitModel$trainDetails$trainingTime))
  expect_equal(fitModel$trainDetails$trainingDate, Sys.Date())

  expect_equal(
    nrow(fitModel$covariateImportance),
    trainData$covariateData$covariateRef %>% dplyr::tally() %>% dplyr::pull()
  )

  expect_true("covariateValue" %in% colnames(fitModel$covariateImportance))

  expect_equal(fitModel$modelDesign$outcomeId, attr(trainData, "metaData")$outcomeId)
  expect_equal(fitModel$modelDesign$targetId, attr(trainData, "metaData")$targetId)
})
