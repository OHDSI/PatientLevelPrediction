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

test_that("normalizeModelSettings maps legacy sklearn settings", {
  oldParam <- list(list(nEstimators = 10L))
  attr(oldParam, "settings") <- list(
    name = "AdaBoost",
    pythonModule = "sklearn.ensemble",
    pythonClass = "AdaBoostClassifier",
    requiresDenseMatrix = TRUE,
    seed = 1
  )

  modelSettings <- list(
    fitFunction = "fitSklearn",
    param = oldParam
  )
  class(modelSettings) <- "modelSettings"

  normalized <- PatientLevelPrediction:::normalizeModelSettings(modelSettings)

  expect_equal(normalized$modelName, "AdaBoost")
  expect_equal(normalized$settings$train, "fitSklearn")
  expect_equal(normalized$settings$predict, "predictSklearn")
  expect_equal(normalized$settings$prepareData, "toSparseM")
  expect_equal(normalized$settings$saveType, "saveLoadSklearn")
  expect_true(normalized$settings$requiresDenseMatrix)
})

test_that("normalizeModelSettings maps legacy R classifier settings", {
  oldParam <- list(list(ntrees = 10L))
  attr(oldParam, "settings") <- list(
    modelName = "Gradient Boosting Machine",
    trainRFunction = "fitXgboost",
    predictRFunction = "predictXgboost",
    varImpRFunction = "varImpXgboost",
    seed = 2
  )

  modelSettings <- list(
    fitFunction = "fitRclassifier",
    param = oldParam
  )
  class(modelSettings) <- "modelSettings"

  normalized <- PatientLevelPrediction:::normalizeModelSettings(modelSettings)

  expect_equal(normalized$modelName, "Gradient Boosting Machine")
  expect_equal(normalized$settings$train, "fitXgboost")
  expect_equal(normalized$settings$predict, "predictXgboost")
  expect_equal(normalized$settings$variableImportance, "varImpXgboost")
  expect_equal(normalized$settings$prepareData, "toSparseM")
})

test_that("computeGridPerformance remains exported and works", {
  prediction <- data.frame(
    rowId = 1:4,
    outcomeCount = c(0, 1, 0, 1),
    value = c(0.2, 0.8, 0.3, 0.7),
    index = c(1, 1, 2, 2)
  )
  param <- list(alpha = 1, lambda = NULL)

  result <- computeGridPerformance(prediction, param, performanceFunct = "computeAuc")

  expect_true(is.list(result))
  expect_equal(result$metric, "computeAuc")
  expect_length(result$cvPerformancePerFold, 2)
  expect_true("hyperSummary" %in% names(result))
})

test_that("predictPlp falls back to predictionFunction attribute", {
  dummyPredict <- function(plpModel, data, cohort) {
    data.frame(
      rowId = cohort$rowId,
      outcomeCount = cohort$outcomeCount,
      value = 0.5,
      originalRowId = cohort$rowId
    )
  }

  plpModel <- list(
    preprocessing = list(featureEngineering = NULL, tidyCovariates = NULL),
    modelDesign = list(
      modelSettings = list(
        settings = list(
          modelType = "binary",
          predict = NULL
        )
      )
    )
  )
  class(plpModel) <- "plpModel"
  attr(plpModel, "predictionFunction") <- dummyPredict
  attr(plpModel, "modelType") <- "binary"

  population <- data.frame(rowId = 1:2, outcomeCount = c(0, 1))
  attr(population, "metaData") <- list(
    targetId = 1,
    outcomeId = 2,
    populationSettings = list(riskWindowEnd = 1)
  )

  plpData <- list()

  pred <- predictPlp(plpModel, plpData, population)
  expect_s3_class(pred, "data.frame")
  expect_equal(nrow(pred), 2)
  expect_equal(attr(pred, "metaData")$predictionType, "binary")
})

test_that("savePlpModel falls back to saveType attribute", {
  tempModel <- file.path(tempdir(), "dummyModelDir")
  dir.create(tempModel, showWarnings = FALSE)
  file.create(file.path(tempModel, "model.pkl"))

  plpModel <- list(
    covariateImportance = data.frame(
      covariateId = 1,
      covariateValue = 0,
      included = 1
    ),
    trainDetails = list(),
    modelDesign = list(
      modelSettings = list(
        settings = list()
      )
    ),
    model = tempModel
  )
  class(plpModel) <- "plpModel"
  attr(plpModel, "saveType") <- "file"

  savePath <- tempfile("savedDummyModel")
  expect_no_error(savePlpModel(plpModel, savePath))
})

test_that("tuneHyperparameters defaults tuning metric when missing", {
  dummyTrain <- function(dataMatrix, labels, hyperParameters, settings) {
    list(hp = hyperParameters)
  }
  dummyPredict <- function(plpModel, data, cohort) {
    data.frame(
      rowId = cohort$rowId,
      originalRowId = cohort$rowId,
      outcomeCount = cohort$outcomeCount,
      value = rep(0.5, nrow(cohort))
    )
  }

  data <- matrix(1:8, nrow = 4)
  labels <- data.frame(
    rowId = 1:4,
    outcomeCount = c(0, 1, 0, 1),
    index = c(1, 1, 2, 2)
  )
  settings <- list(
    train = dummyTrain,
    predict = dummyPredict,
    requiresDenseMatrix = FALSE
  )
  param <- list(lambda = list(1))

  res <- PatientLevelPrediction:::tuneHyperparameters(
    data = data,
    labels = labels,
    param = param,
    settings = settings,
    hyperparamSettings = NULL
  )

  expect_true(is.list(res))
  expect_true("finalParam" %in% names(res))
})
