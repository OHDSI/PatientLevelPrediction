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
  expect_equal(lgbmSet$fitFunction, "fitRclassifier")
  expect_type(lgbmSet$param, "list")

  expect_equal(attr(lgbmSet$param, "settings")$modelType, "LightGBM")
  expect_equal(attr(lgbmSet$param, "settings")$seed, seed)
  expect_equal(attr(lgbmSet$param, "settings")$modelName, "LightGBM")

  expect_equal(attr(lgbmSet$param, "settings")$threads, 5)
  expect_equal(attr(lgbmSet$param, "settings")$varImpRFunction, "varImpLightGBM")
  expect_equal(attr(lgbmSet$param, "settings")$trainRFunction, "fitLightGBM")
  expect_equal(attr(lgbmSet$param, "settings")$predictRFunction, "predictLightGBM")
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
  skip_if_offline()
  modelSettings <- setLightGBM(numIterations = 10, maxDepth = 3, learningRate = 0.1, numLeaves = 31, minDataInLeaf = 10, lambdaL1 = 0, lambdaL2 = 0)

  fitModel <- fitPlp(
    trainData = trainData,
    modelSettings = modelSettings,
    analysisId = "lgbmTest",
    analysisPath = tempdir()
  )

  expect_equal(nrow(fitModel$prediction), nrow(trainData$labels) * 2)
  expect_equal(length(unique(fitModel$prediction$evaluationType)), 2)

  # check prediction between 0 and 1
  expect_gte(min(fitModel$prediction$value), 0)
  expect_lte(max(fitModel$prediction$value), 1)

  expect_equal(class(fitModel$model), c("lgb.Booster", "R6"))

  expect_lte(nrow(fitModel$covariateImportance), trainData$covariateData$covariateRef %>% dplyr::tally() %>% dplyr::pull())

  expect_equal(fitModel$modelDesign$outcomeId, outcomeId)
  expect_equal(fitModel$modelDesign$targetId, 1)
  # TODO check other model design values?

  # test that at least some features have importances that are not zero
  expect_equal(sum(abs(fitModel$covariateImportance$covariateValue)) > 0, TRUE)
})
