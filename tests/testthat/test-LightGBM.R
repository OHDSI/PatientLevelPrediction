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
test_that("LightGBM settings work", {
  skip_if_not_installed("lightgbm")
  skip_on_cran()
  seed <- sample(10000000, 1)
  # =====================================
  # checking Light GBM
  # =====================================
  lgbmSet <- setLightGBM(
    nthread = 5,
    earlyStopRound = 25,
    numIterations = 10,
    numLeaves = c(31, 20),
    maxDepth = 5,
    minDataInLeaf = 10,
    learningRate = 0.1,
    lambdaL1 = 0,
    lambdaL2 = 0,
    scalePosWeight = 1,
    isUnbalance = FALSE,
    seed = seed
  )

  expect_s3_class(lgbmSet, "modelSettings")
  expect_type(lgbmSet$param, "list")

  expect_equal(lgbmSet$settings$modelType, "binary")
  expect_equal(lgbmSet$settings$seed, seed)
  expect_equal(lgbmSet$settings$modelName, "lightGBM")

  expect_equal(lgbmSet$settings$threads, 5)
  expect_equal(lgbmSet$settings$variableImportance, "varImpLightGBM")
  expect_equal(lgbmSet$settings$train, "fitLightGBM")
  expect_equal(lgbmSet$settings$predict, "predictLightGBM")
})


test_that("LightGBM settings expected errors", {
  skip_if_not_installed("lightgbm")
  skip_on_cran()
  # =====================================
  # checking Gradient Boosting Machine
  # =====================================

  expect_error(setLightGBM(numIterations = -1))
  expect_error(setLightGBM(numLeaves = -1))
  expect_error(setLightGBM(numLeaves = 10000000))
  expect_error(setLightGBM(learningRate = -2))
  expect_error(setLightGBM(seed = "F"))
  expect_error(setLightGBM(lambdaL1 = -1))
  expect_error(setLightGBM(lambdaL2 = -1))
  expect_error(setLightGBM(scalePosWeight = -1))
  expect_error(setLightGBM(isUnbalance = TRUE, scalePosWeight = 0.5))
})




test_that("LightGBM working checks", {
  skip_if_not_installed("lightgbm")
  skip_on_cran()
  skip_if_no_eunomia()
  modelSettings <- setLightGBM(
    numIterations = 10, 
    maxDepth = 3, 
    learningRate = 0.1, 
    numLeaves = 31, 
    minDataInLeaf = 10, 
    lambdaL1 = 0, 
    lambdaL2 = 0
  )

  fitModel <- fitPlp(
    trainData = trainData,
    modelSettings = modelSettings,
    analysisId = "lgbmTest",
    analysisPath = tempdir()
  )

  expect_equal(nrow(fitModel$prediction), nrow(trainData$labels) * 2)
  expect_equal(length(unique(fitModel$prediction$evaluationType)), 2)
  # every prediction carries its own patient's outcome
  expect_equal(
    fitModel$prediction$outcomeCount,
    trainData$labels$outcomeCount[match(fitModel$prediction$rowId, trainData$labels$rowId)]
  )

  # check prediction between 0 and 1
  expect_gte(min(fitModel$prediction$value), 0)
  expect_lte(max(fitModel$prediction$value), 1)

  expect_equal(class(fitModel$model), c("lgb.Booster", "R6"))

  expect_lte(
    nrow(fitModel$covariateImportance), 
    trainData$covariateData$covariateRef %>% dplyr::tally() %>% dplyr::pull()
  )

  expect_equal(fitModel$modelDesign$outcomeId, outcomeId)
  expect_equal(fitModel$modelDesign$targetId, 1)
  # TODO check other model design values?

  # test that at least some features have importances that are not zero
  expect_equal(sum(abs(fitModel$covariateImportance$covariateValue)) > 0, TRUE)

  # test save and load
  savePath <- tempfile("lgbmTest_")
  unlink(savePath, recursive = TRUE)
  savePlpModel(fitModel, savePath)

  # current format should be JSON:
  expect_true(file.exists(file.path(savePath, "model.json")))
  serialized <- ParallelLogger::loadSettingsFromJson(file.path(savePath, "model.json"))
  expect_equal(serialized$modelType, "lightGBM")
  expect_equal(serialized$modelFormat, "lightgbm_txt")
  expect_true(is.character(serialized$model))
  expect_length(serialized$model, 1)
  expect_match(serialized$model, "^tree")
  expect_match(serialized$model, "version=")
  expect_equal(serialized$model, fitModel$model$save_model_to_string(NULL))

  loadedBooster <- lightgbm::lgb.load(model_str = serialized$model)
  expect_equal(class(loadedBooster), c("lgb.Booster", "R6"))
  expect_equal(loadedBooster$current_iter(), fitModel$model$current_iter())

  loadModel <- loadPlpModel(savePath)

  expect_s3_class(loadModel, "plpModel")
  expect_equal(class(loadModel$model), c("lgb.Booster", "R6"))
  expect_equal(loadModel$model$current_iter(), fitModel$model$current_iter())

  predFit <- predictPlp(
    plpModel = fitModel,
    plpData = testData,
    population = testData$labels
  )
  predLoad <- predictPlp(
    plpModel = loadModel,
    plpData = testData,
    population = testData$labels
  )
  expect_equal(predLoad$value, predFit$value, tolerance = 0e-10)
  expect_true(all(predLoad$value >= 0))
  expect_true(all(predLoad$value <= 1))

  # backwards compatibility: old LightGBM serialization (raw text in model.json)
  lightgbm::lgb.save(fitModel$model, file.path(savePath, "model.json"))
  expect_error(ParallelLogger::loadSettingsFromJson(file.path(savePath, "model.json")))
  loadModelOld <- loadPlpModel(savePath)
  expect_s3_class(loadModelOld, "plpModel")
  expect_equal(class(loadModelOld$model), c("lgb.Booster", "R6"))
})

test_that("LightGBM importance is reported against the correct covariates", {
  skip_if_not_installed("lightgbm")
  skip_on_cran()
  set.seed(42)
  n <- 1000L
  x <- matrix(rbinom(3 * n, 1, 0.5), ncol = 3)
  fitImportance <- function(y) {
    model <- fitLightGBM(
      dataMatrix = Matrix::Matrix(x, sparse = TRUE),
      labels = data.frame(outcomeCount = y),
      hyperParameters = list(
        numIterations = 30, numLeaves = 7, maxDepth = 3, learningRate = 0.1,
        minDataInLeaf = 5, scalePosWeight = 1, lambdaL1 = 0, lambdaL2 = 0,
        isUnbalance = FALSE
      ),
      settings = list(seed = 1, threads = 1)
    )
    raw <- lightgbm::lgb.importance(model, percentage = TRUE)
    imported <- varImpLightGBM(
      model,
      data.frame(columnId = c(1, 2, 3), covariateId = c(1001, 1002, 1003))
    )
    list(raw = raw, imported = imported)
  }

  strongFirst <- fitImportance(rbinom(n, 1, plogis(-1 + 3 * x[, 1] + x[, 3])))
  imported <- strongFirst$imported
  expect_equal(imported$covariateId[which.max(imported$covariateValue)], 1001)
  expect_equal(
    imported$covariateValue[imported$covariateId == 1001],
    strongFirst$raw$Gain[strongFirst$raw$Feature == "Column_0"]
  )

  # A column LightGBM never splits on must not receive another column's importance.
  noFirst <- fitImportance(rbinom(n, 1, plogis(-1 + 3 * x[, 2])))
  expect_false(1001 %in% noFirst$imported$covariateId[noFirst$imported$covariateValue > 0.5])
  expect_equal(
    noFirst$imported$covariateValue[noFirst$imported$covariateId == 1002],
    noFirst$raw$Gain[noFirst$raw$Feature == "Column_1"]
  )
  expect_true(all(noFirst$imported$included == 1))
})

