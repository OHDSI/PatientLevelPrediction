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

catboost_is_available <- function() {
  if (!rlang::is_installed("reticulate")) {
    return(FALSE)
  }
  tryCatch({
    reticulate::import("catboost", convert = FALSE)
    TRUE
  }, error = function(e) FALSE)
}

test_that("CatBoost settings work", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  if (!catboost_is_available()) {
    skip("python package 'catboost' is not available")
  }

  catSet <- setCatBoost(
    iterations = c(50, 100),
    depth = c(4, 6),
    learningRate = c(0.03, 0.1),
    l2LeafReg = c(3),
    randomStrength = c(1),
    borderCount = c(254),
    rsm = c(1),
    subsample = c(1),
    earlyStoppingRounds = c(10),
    threadCount = 2,
    seed = 42
  )

  expect_s3_class(catSet, "modelSettings")
  expect_equal(catSet$settings$modelType, "binary")
  expect_equal(catSet$settings$modelName, "catboost")
  expect_equal(catSet$settings$pythonModule, "catboost")
  expect_equal(catSet$settings$pythonClass, "CatBoostClassifier")
  expect_equal(catSet$settings$train, "fitCatBoostPython")
  expect_equal(catSet$settings$predict, "predictCatBoostPython")
  expect_equal(catSet$settings$saveType, "saveLoadCatBoostPython")
  expect_false(catSet$settings$requiresDenseMatrix)
})

test_that("CatBoost settings expected errors", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  if (!catboost_is_available()) {
    skip("python package 'catboost' is not available")
  }

  expect_error(setCatBoost(iterations = c(0)))
  expect_error(setCatBoost(depth = c(0)))
  expect_error(setCatBoost(learningRate = c(0)))
  expect_error(setCatBoost(l2LeafReg = c(-1)))
  expect_error(setCatBoost(randomStrength = c(-1)))
  expect_error(setCatBoost(borderCount = c(0)))
  expect_error(setCatBoost(rsm = c(0)))
  expect_error(setCatBoost(rsm = c(2)))
  expect_error(setCatBoost(subsample = c(0)))
  expect_error(setCatBoost(subsample = c(2)))
  expect_error(setCatBoost(earlyStoppingRounds = c(0)))
})

test_that("CatBoost working checks", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  skip_if_offline()
  if (!catboost_is_available()) {
    skip("python package 'catboost' is not available")
  }

  modelSettings <- setCatBoost(
    iterations = c(30),
    depth = c(4),
    learningRate = c(0.1),
    earlyStoppingRounds = c(10),
    seed = 42
  )

  fitModel <- fitPlp(
    trainData = tinyTrainData,
    modelSettings = modelSettings,
    analysisId = "catBoostTest",
    analysisPath = tempdir()
  )

  expect_equal(nrow(fitModel$prediction), nrow(tinyTrainData$labels) * 2)
  expect_equal(length(unique(fitModel$prediction$evaluationType)), 2)
  expect_true(reticulate::py_has_attr(fitModel$model, "predict_proba"))

  expect_gte(min(fitModel$prediction$value), 0)
  expect_lte(max(fitModel$prediction$value), 1)
  expect_equal(fitModel$modelDesign$outcomeId, outcomeId)
  expect_equal(fitModel$modelDesign$targetId, 1)

  savePath <- tempfile("catboostTest_")
  unlink(savePath, recursive = TRUE)
  savePlpModel(fitModel, savePath)
  loadModel <- loadPlpModel(savePath)
  expect_s3_class(loadModel, "plpModel")
  expect_true(reticulate::py_has_attr(loadModel$model, "predict_proba"))

  predFit <- predictPlp(fitModel, testData, testData$labels)
  predLoad <- predictPlp(loadModel, testData, testData$labels)
  expect_equal(predLoad$value, predFit$value, tolerance = 1e-10)
  expect_true(all(predLoad$value >= 0))
  expect_true(all(predLoad$value <= 1))
})
