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
