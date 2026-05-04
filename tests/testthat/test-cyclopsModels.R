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
# ================ SETTING TESTINGS


test_that("set LR inputs", {
  # =====================================
  # checking Logistic Regression
  # =====================================

  model_set <- setLassoLogisticRegression()
  expect_s3_class(model_set, "modelSettings")

  expect_equal(model_set$fitFunction, "fitCyclopsModel")
  expect_type(model_set$param, "list")

  expect_equal(model_set$param$priorParams$priorType, "laplace")

  expect_equal(model_set$settings$cyclopsModelType, "logistic")
  expect_equal(model_set$settings$modelType, "binary")
  expect_equal(model_set$settings$modelName, "lassoLogisticRegression")
  expect_equal(model_set$settings$priorfunction, "Cyclops::createPrior")
  expect_equal(model_set$settings$addIntercept, TRUE)
  expect_equal(model_set$settings$useControl, TRUE)
  expect_equal(model_set$settings$cvRepetitions, 1)


  variance <- runif(1)
  model_set <- setLassoLogisticRegression(variance = variance)
  expect_equal(model_set$param$priorParams$variance, variance)

  seed <- sample(10, 1)
  model_set <- setLassoLogisticRegression(seed = seed)
  expect_equal(model_set$settings$seed, seed)

  model_set <- setLassoLogisticRegression(includeCovariateIds = c(1, 2))
  expect_equal(model_set$param$includeCovariateIds, c(1, 2))

  model_set <- setLassoLogisticRegression(noShrinkage = c(1, 3))
  expect_equal(model_set$param$priorParams$exclude, c(1, 3))

  threads <- sample(10, 1)
  model_set <- setLassoLogisticRegression(threads = threads)
  expect_equal(model_set$settings$threads, threads)

  model_set <- setLassoLogisticRegression(forceIntercept = TRUE)
  expect_equal(model_set$param$priorParams$forceIntercept, TRUE)

  model_set <- setLassoLogisticRegression(upperLimit = 1)
  expect_equal(model_set$param$upperLimit, 1)

  model_set <- setLassoLogisticRegression(lowerLimit = 1)
  expect_equal(model_set$param$lowerLimit, 1)

  tolerance <- runif(1)
  model_set <- setLassoLogisticRegression(tolerance = tolerance)
  expect_equal(model_set$settings$tolerance, tolerance)

  maxIterations <- sample(100, 1)
  model_set <- setLassoLogisticRegression(maxIterations = maxIterations)
  expect_equal(model_set$settings$maxIterations, maxIterations)
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


test_that("set ridge regression inputs", {
  modelSet <- setRidgeRegression()
  expect_s3_class(modelSet, "modelSettings")

  expect_equal(modelSet$fitFunction, "fitCyclopsModel")
  expect_type(modelSet$param, "list")
  expect_equal(modelSet$param$priorParams$priorType, "normal")

  expect_equal(modelSet$settings$cyclopsModelType, "logistic")
  expect_equal(modelSet$settings$modelType, "binary")
  expect_equal(modelSet$settings$modelName, "ridgeLogisticRegression")
  expect_equal(modelSet$settings$priorfunction, "Cyclops::createPrior")
  expect_equal(modelSet$settings$addIntercept, TRUE)
  expect_equal(modelSet$settings$useControl, TRUE)
  expect_equal(modelSet$settings$cvRepetitions, 1)

  variance <- runif(1)
  modelSet <- setRidgeRegression(variance = variance)
  expect_equal(modelSet$param$priorParams$variance, variance)

  seed <- sample(10, 1)
  modelSet <- setRidgeRegression(seed = seed)
  expect_equal(modelSet$settings$seed, seed)

  modelSet <- setRidgeRegression(includeCovariateIds = c(1, 2))
  expect_equal(modelSet$param$includeCovariateIds, c(1, 2))

  modelSet <- setRidgeRegression(noShrinkage = c(1, 3))
  expect_equal(modelSet$param$priorParams$exclude, c(1, 3))

  threads <- sample(10, 1)
  modelSet <- setRidgeRegression(threads = threads)
  expect_equal(modelSet$settings$threads, threads)

  modelSet <- setRidgeRegression(forceIntercept = TRUE)
  expect_equal(modelSet$param$priorParams$forceIntercept, TRUE)

  modelSet <- setRidgeRegression(upperLimit = 1)
  expect_equal(modelSet$param$upperLimit, 1)

  modelSet <- setRidgeRegression(lowerLimit = 1)
  expect_equal(modelSet$param$lowerLimit, 1)

  tolerance <- runif(1)
  modelSet <- setRidgeRegression(tolerance = tolerance)
  expect_equal(modelSet$settings$tolerance, tolerance)

  maxIterations <- sample(100, 1)
  modelSet <- setRidgeRegression(maxIterations = maxIterations)
  expect_equal(modelSet$settings$maxIterations, maxIterations)
})


test_that("set ridge regression incorrect inputs", {
  expect_error(setRidgeRegression(variance = -0.01))
  expect_error(setRidgeRegression(variance = "variance"))
  expect_error(setRidgeRegression(seed = "seed"))
  expect_error(setRidgeRegression(threads = "threads"))

  expect_error(setRidgeRegression(lowerLimit = "lowerLimit"))
  expect_error(setRidgeRegression(upperLimit = "upperLimit"))
  expect_error(setRidgeRegression(lowerLimit = 3, upperLimit = 1))
})




test_that("set cox regression inputs", {
  skip_if_not_installed("polspline")
  skip_on_cran()
  # =====================================
  # checking Cox Regression
  # =====================================

  modelSet <- setCoxModel()
  expect_s3_class(modelSet, "modelSettings")

  expect_equal(modelSet$fitFunction, "fitCyclopsModel")
  expect_type(modelSet$param, "list")

  expect_equal(modelSet$param$priorParams$priorType, "laplace")

  expect_equal(modelSet$settings$cyclopsModelType, "cox")
  expect_equal(modelSet$settings$modelType, "survival")
  expect_equal(modelSet$settings$modelName, "coxLasso")
  expect_equal(modelSet$settings$priorfunction, "Cyclops::createPrior")
  expect_equal(modelSet$settings$addIntercept, FALSE)
  expect_equal(modelSet$settings$useControl, TRUE)
  expect_equal(modelSet$settings$cvRepetitions, 1)

  variance <- runif(1)
  modelSet <- setCoxModel(variance = variance)
  expect_equal(modelSet$param$priorParams$variance, variance)

  seed <- sample(10, 1)
  modelSet <- setCoxModel(seed = seed)
  expect_equal(modelSet$settings$seed, seed)

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
  expect_equal(modelSet$settings$threads, threads)

  tolerance <- runif(1)
  modelSet <- setCoxModel(tolerance = tolerance)
  expect_equal(modelSet$settings$tolerance, tolerance)

  maxIterations <- sample(100, 1)
  modelSet <- setCoxModel(maxIterations = maxIterations)
  expect_equal(modelSet$settings$maxIterations, maxIterations)
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
  expect_s3_class(modelSet, "modelSettings")

  expect_equal(modelSet$fitFunction, "fitCyclopsModel")
  expect_type(modelSet$param, "list")

  expect_equal(modelSet$settings$modelName, "iterativeHardThresholding")
  expect_equal(modelSet$settings$cyclopsModelType, "logistic")
  expect_equal(modelSet$settings$modelType, "binary")
  expect_equal(modelSet$settings$priorfunction, "IterativeHardThresholding::createIhtPrior")
  expect_equal(modelSet$settings$addIntercept, FALSE)
  expect_equal(modelSet$settings$useControl, FALSE)
  expect_equal(modelSet$settings$crossValidationInPrior, FALSE)

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

  modelSet <- setIterativeHardThresholding(initialRidgeVariance = 0.5)
  expect_equal(modelSet$param$priorParams$initialRidgeVariance, 0.5)

  modelSet <- setIterativeHardThresholding(tolerance = 0.01)
  expect_equal(modelSet$param$priorParams$tolerance, 0.01)
  
  modelSet <- setIterativeHardThresholding(maxIterations = 200)
  expect_equal(modelSet$param$priorParams$maxIterations, 200)

  modelSetg <- setIterativeHardThresholding(threshold = 0.1) 
  expect_equal(modelSetg$param$priorParams$threshold, 0.1)

  modelSet <- setIterativeHardThresholding(delta = 0.01)
  expect_equal(modelSet$param$priorParams$delta, 0.01)


  seed <- sample(10, 1)
  modelSet <- setIterativeHardThresholding(seed = seed)
  expect_equal(modelSet$settings$seed, seed)
})


test_that("test IHT incorrect inputs", {
  skip_if_not_installed("IterativeHardThresholding")
  skip_on_cran()
  expect_error(setIterativeHardThresholding(K = 0))
  expect_error(setIterativeHardThresholding(penalty = "L1"))
  expect_error(setIterativeHardThresholding(fitBestSubset = "true"))
  expect_error(setIterativeHardThresholding(seed = "F"))
})



# ================ FUNCTION TESTING

test_that("Cyclops CV prior uses fitted standard variance", {
  cyclopsData <- Cyclops::convertToCyclopsData(
    outcomes = data.frame(rowId = 1:4, y = c(0, 1, 0, 1)),
    covariates = data.frame(
      rowId = c(1, 2, 3, 4),
      covariateId = c(100, 100, 200, 200),
      covariateValue = c(1, 1, 1, 1)
    ),
    addIntercept = TRUE,
    modelType = "lr",
    checkRowIds = FALSE,
    quiet = TRUE
  )

  cvPrior <- createCyclopsCvPrior(
    modelSettings = setRidgeRegression(variance = 0.01),
    fit = list(variance = 0.25),
    cyclopsData = cyclopsData
  )

  expect_s3_class(cvPrior, "cyclopsPrior")
  expect_equal(cvPrior$priorType, "normal")
  expect_equal(cvPrior$variance, 0.25)
  expect_false(cvPrior$useCrossValidation)
})

test_that("Cyclops CV prior supports fitted final variances", {
  skip_if_not_installed("IterativeHardThresholding")
  skip_on_cran()

  cyclopsData <- Cyclops::convertToCyclopsData(
    outcomes = data.frame(rowId = 1:4, y = c(0, 1, 0, 1)),
    covariates = data.frame(
      rowId = c(1, 2, 3, 4),
      covariateId = c(100, 100, 200, 200),
      covariateValue = c(1, 1, 1, 1)
    ),
    addIntercept = FALSE,
    modelType = "lr",
    checkRowIds = FALSE,
    quiet = TRUE
  )
  finalVariance <- c(0.2, 0.3)
  modelSettings <- setIterativeHardThresholding()

  cvPrior <- createCyclopsCvPrior(
    modelSettings = modelSettings,
    fit = list(ihtFinalPriorVariance = finalVariance),
    cyclopsData = cyclopsData
  )

  expect_s3_class(cvPrior, "cyclopsPrior")
  expect_equal(length(cvPrior$priorType), length(finalVariance))
  expect_equal(cvPrior$variance, finalVariance)
})

test_that("Cyclops CV prior maps per-covariate prior exclusions", {
  cyclopsData <- Cyclops::convertToCyclopsData(
    outcomes = data.frame(rowId = 1:4, y = c(0, 1, 0, 1)),
    covariates = data.frame(
      rowId = c(1, 2, 3, 4),
      covariateId = c(100, 100, 200, 200),
      covariateValue = c(1, 1, 1, 1)
    ),
    addIntercept = TRUE,
    modelType = "lr",
    checkRowIds = FALSE,
    quiet = TRUE
  )

  priorType <- createNormalPriorType(cyclopsData, exclude = 100)
  expect_equal(priorType$types, c("none", "none", "normal"))
  expect_equal(as.numeric(priorType$excludeCovariateIds), c(0, 100))

  priorType <- createNormalPriorType(cyclopsData, exclude = c("(Intercept)", "200"))
  expect_equal(priorType$types, c("none", "normal", "none"))
  expect_equal(as.numeric(priorType$excludeCovariateIds), c(0, 200))

  priorType <- createNormalPriorType(cyclopsData, exclude = c(), forceIntercept = TRUE)
  expect_equal(priorType$types, c("normal", "normal", "normal"))
  expect_null(priorType$excludeCovariateIds)
})

test_that("Cyclops CV prior fails clearly for invalid per-covariate prior inputs", {
  skip_if_not_installed("IterativeHardThresholding")
  skip_on_cran()

  cyclopsData <- Cyclops::convertToCyclopsData(
    outcomes = data.frame(rowId = 1:4, y = c(0, 1, 0, 1)),
    covariates = data.frame(
      rowId = c(1, 2, 3, 4),
      covariateId = c(100, 100, 200, 200),
      covariateValue = c(1, 1, 1, 1)
    ),
    addIntercept = FALSE,
    modelType = "lr",
    checkRowIds = FALSE,
    quiet = TRUE
  )

  expect_error(
    createNormalPriorType(cyclopsData, exclude = "unknown"),
    "Unable to match all covariates: unknown"
  )
  expect_error(
    createCyclopsCvPrior(
      modelSettings = setIterativeHardThresholding(),
      fit = list(ihtFinalPriorVariance = c(0.2)),
      cyclopsData = cyclopsData
    ),
    "Fitted prior variance length does not match the number of Cyclops covariates"
  )
  expect_error(
    createCyclopsCvPrior(
      modelSettings = setIterativeHardThresholding(),
      fit = list(),
      cyclopsData = cyclopsData
    ),
    "Cyclops fit did not return fitted final prior variances for CV refitting"
  )
})

test_that("test IHT returns CV predictions", {
  skip_if_offline()
  skip_if_not_installed("IterativeHardThresholding")
  skip_on_cran()

  fitModel <- fitPlp(
    trainData = trainData,
    modelSettings = setIterativeHardThresholding(K = 5, seed = 42, maxIterations = 100),
    analysisId = "ihtTest",
    analysisPath = tempdir()
  )

  expect_equal(length(unique(fitModel$prediction$evaluationType)), 2)
  expect_true("CV" %in% fitModel$prediction$evaluationType)
  expect_equal(nrow(fitModel$prediction), nrow(trainData$labels) * 2)
  expect_true(is.data.frame(fitModel$trainDetails$hyperParamSearch))
  expect_true("CV" %in% fitModel$trainDetails$hyperParamSearch$fold)
})

test_that("test logistic regression runs", {
  skip_if_offline()
  modelSettings <- setLassoLogisticRegression()

  fitModel <- fitPlp(
    trainData = trainData,
    modelSettings = modelSettings,
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
  expect_true(is.data.frame(fitModel$trainDetails$hyperParamSearch))
  if (nrow(fitModel$trainDetails$hyperParamSearch) > 0) {
    expect_true("CV" %in% fitModel$trainDetails$hyperParamSearch$fold)
  }

  expect_equal(fitModel$modelDesign$outcomeId, attr(trainData, "metaData")$outcomeId)
  expect_equal(fitModel$modelDesign$targetId, attr(trainData, "metaData")$targetId)
})


test_that("test ridge regression runs", {
  skip_if_offline()
  modelSettings <- setRidgeRegression()

  fitModel <- fitPlp(
    trainData = trainData,
    modelSettings = modelSettings,
    analysisId = "ridgeLrTest",
    analysisPath = tempdir()
  )

  expect_equal(length(unique(fitModel$prediction$evaluationType)), 2)
  expect_equal(nrow(fitModel$prediction), nrow(trainData$labels) * 2)
  expect_true(!is.null(fitModel$trainDetails$trainingTime))
  expect_equal(fitModel$trainDetails$trainingDate, Sys.Date())
  expect_equal(fitModel$modelDesign$outcomeId, attr(trainData, "metaData")$outcomeId)
  expect_equal(fitModel$modelDesign$targetId, attr(trainData, "metaData")$targetId)
})
