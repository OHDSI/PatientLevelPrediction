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

context("RClassifier")


test_that("GBM settings work", {
  
  seed <- sample(10000000,1)
  #=====================================
  # checking Gradient Boosting Machine
  #=====================================
  gbmSet <- setGradientBoostingMachine(
    ntrees = c(2, 10), 
    nthread = 5, 
    earlyStopRound = 25,
    maxDepth = 4, 
    minChildWeight = 1, 
    learnRate = 0.1,
    alpha = 0,
    lambda =1,
    seed = seed
    )
  
  expect_is(gbmSet, 'modelSettings')
  expect_equal(gbmSet$fitFunction, 'fitRclassifier')
  expect_is(gbmSet$param, 'list')
  
  expect_equal(attr(gbmSet$param, 'settings')$modeType, 'Xgboost')
  expect_equal(attr(gbmSet$param, 'settings')$seed, seed)
  expect_equal(attr(gbmSet$param, 'settings')$modelName, "Gradient Boosting Machine")
  
  expect_equal(attr(gbmSet$param, 'settings')$threads, 5)
  expect_equal(attr(gbmSet$param, 'settings')$varImpRFunction, 'varImpXgboost')
  expect_equal(attr(gbmSet$param, 'settings')$trainRFunction, 'fitXgboost')
  expect_equal(attr(gbmSet$param, 'settings')$predictRFunction, 'predictXgboost')
  
  expect_equal(length(gbmSet$param),2)
  
  expect_equal(length(unique(unlist(lapply(gbmSet$param, function(x) x$ntrees)))), 2)
  expect_equal(length(unique(unlist(lapply(gbmSet$param, function(x) x$earlyStopRound)))), 1)
  expect_equal(length(unique(unlist(lapply(gbmSet$param, function(x) x$maxDepth)))), 1)
  expect_equal(length(unique(unlist(lapply(gbmSet$param, function(x) x$minChildWeight)))), 1)
  expect_equal(length(unique(unlist(lapply(gbmSet$param, function(x) x$learnRate)))), 1)
  expect_equal(length(unique(unlist(lapply(gbmSet$param, function(x) x$lambda)))), 1)
  expect_equal(length(unique(unlist(lapply(gbmSet$param, function(x) x$alpha)))), 1)
  
  
  
})


test_that("GBM settings expected errors", {
#=====================================
# checking Gradient Boosting Machine
#=====================================

testthat::expect_error(setGradientBoostingMachine(ntrees = -1))
testthat::expect_error(setGradientBoostingMachine(minChildWeight = -1))
testthat::expect_error(setGradientBoostingMachine(maxDepth = 0))
testthat::expect_error(setGradientBoostingMachine(learnRate = -2))
testthat::expect_error(setGradientBoostingMachine(seed = 'F'))
testthat::expect_error(setGradientBoostingMachine(lambda = -1))
testthat::expect_error(setGradientBoostingMachine(alpha = -1))
testthat::expect_error(setGradientBoostingMachine(scalePosWeight = -1))



})




test_that("GBM working checks", {
  
  trainData <- createTrainData(plpData = plpData, population = population)
  modelSettings <- setGradientBoostingMachine(ntrees = 10, maxDepth = 3, learnRate = 0.1)
  
  fitModel <- fitPlp(
    trainData = trainData,   
    modelSettings = modelSettings, 
    analysisId = 'gbmTest'
  )
  
  expect_equal(nrow(fitModel$prediction), nrow(trainData$labels)*2)
  expect_equal(length(unique(fitModel$prediction$evaluationType)),2)
  
  # check prediction between 0 and 1
  expect_gte(min(fitModel$prediction$value), 0)
  expect_lte(max(fitModel$prediction$value), 1)
  
  expect_equal(class(fitModel$model),"xgb.Booster")
  
  expect_lte(nrow(fitModel$covariateImportance), trainData$covariateData$covariateRef %>% dplyr::tally() %>% dplyr::pull())
  
  expect_equal(fitModel$trainDetails$outcomeId, 2)
  expect_equal(fitModel$trainDetails$cohortId, 1)
  
  # test that at least some features have importances that are not zero
  expect_equal(sum(abs(fitModel$covariateImportance$covariateValue))>0, TRUE)
  
})
