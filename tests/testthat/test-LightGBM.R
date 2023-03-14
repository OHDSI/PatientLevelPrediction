# Copyright 2023 Observational Health Data Sciences and Informatics
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

context("LightGBM")


test_that("LightGBM settings work", {
  
  seed <- sample(10000000,1)
  #=====================================
  # checking Light GBM
  #=====================================
  lgbmSet <- setLightGBM(
    nthread = 5, 
    earlyStopRound = 25,
    numIterations = 10,
    numLeaves = c(31, 20),
    maxDepth = 5, 
    minDataInLeaf = 10, 
    learningRate = 0.1,
    lambdaL1 = 0,
    lambdaL2 =0,
    scalePosWeight = 1,
    isUnbalance = F,
    seed = seed
  )
  
  expect_is(lgbmSet, 'modelSettings')
  expect_equal(lgbmSet$fitFunction, 'fitRclassifier')
  expect_is(lgbmSet$param, 'list')
  
  expect_equal(attr(lgbmSet$param, 'settings')$modelType, 'LightGBM')
  expect_equal(attr(lgbmSet$param, 'settings')$seed, seed)
  expect_equal(attr(lgbmSet$param, 'settings')$modelName, "LightGBM")
  
  expect_equal(attr(lgbmSet$param, 'settings')$threads, 5)
  expect_equal(attr(lgbmSet$param, 'settings')$varImpRFunction, 'varImpLightGBM')
  expect_equal(attr(lgbmSet$param, 'settings')$trainRFunction, 'fitLightGBM')
  expect_equal(attr(lgbmSet$param, 'settings')$predictRFunction, 'predictLightGBM')
  
  expect_equal(length(lgbmSet$param),2)
  
  expect_equal(length(unique(unlist(lapply(lgbmSet$param, function(x) x$numIterations)))), 1)
  expect_equal(length(unique(unlist(lapply(lgbmSet$param, function(x) x$numLeaves)))), 2)
  expect_equal(length(unique(unlist(lapply(lgbmSet$param, function(x) x$earlyStopRound)))), 1)
  expect_equal(length(unique(unlist(lapply(lgbmSet$param, function(x) x$maxDepth)))), 1)
  expect_equal(length(unique(unlist(lapply(lgbmSet$param, function(x) x$minDataInLeaf)))), 1)
  expect_equal(length(unique(unlist(lapply(lgbmSet$param, function(x) x$learningRate)))), 1)
  expect_equal(length(unique(unlist(lapply(lgbmSet$param, function(x) x$lambdaL1)))), 1)
  expect_equal(length(unique(unlist(lapply(lgbmSet$param, function(x) x$lambdaL2)))), 1)
  expect_equal(length(unique(unlist(lapply(lgbmSet$param, function(x) x$scalePosWeight)))), 1)
  expect_equal(length(unique(unlist(lapply(lgbmSet$param, function(x) x$isUnbalance)))), 1)
  
})


test_that("LightGBM settings expected errors", {
  #=====================================
  # checking Gradient Boosting Machine
  #=====================================
  
  testthat::expect_error(setLightGBM(numIterations = -1))
  testthat::expect_error(setLightGBM(numLeaves = -1))
  testthat::expect_error(setLightGBM(numLeaves = 10000000))
  testthat::expect_error(setLightGBM(learningRate = -2))
  testthat::expect_error(setLightGBM(seed = 'F'))
  testthat::expect_error(setLightGBM(lambdaL1 = -1))
  testthat::expect_error(setLightGBM(lambdaL2 = -1))
  testthat::expect_error(setLightGBM(scalePosWeight = -1))
  testthat::expect_error(setLightGBM(isUnbalance = TRUE, scalePosWeight = 0.5))
  
})




test_that("LightGBM working checks", {
  
  modelSettings <- setLightGBM(numIterations = 10, maxDepth = 3, learningRate = 0.1, numLeaves = 31, minDataInLeaf = 10, lambdaL1 = 0, lambdaL2 = 0)
  
  fitModel <- fitPlp(
    trainData = trainData,   
    modelSettings = modelSettings, 
    analysisId = 'lgbmTest'
  )
  
  expect_equal(nrow(fitModel$prediction), nrow(trainData$labels)*2)
  expect_equal(length(unique(fitModel$prediction$evaluationType)),2)
  
  # check prediction between 0 and 1
  expect_gte(min(fitModel$prediction$value), 0)
  expect_lte(max(fitModel$prediction$value), 1)
  
  expect_equal(class(fitModel$model), c("lgb.Booster", "R6"))
  
  expect_lte(nrow(fitModel$covariateImportance), trainData$covariateData$covariateRef %>% dplyr::tally() %>% dplyr::pull())
  
  expect_equal(fitModel$modelDesign$outcomeId, 2)
  expect_equal(fitModel$modelDesign$targetId, 1)
  # TODO check other model design values?
  
  # test that at least some features have importances that are not zero
  expect_equal(sum(abs(fitModel$covariateImportance$covariateValue))>0, TRUE)
  
})
