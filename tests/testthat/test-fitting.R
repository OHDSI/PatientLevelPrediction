# Copyright 2019 Observational Health Data Sciences and Informatics
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

context("Fitting")

plpResultKNN <- runPlp(population = population,
                       plpData = plpData, 
                       modelSettings = knnSet, 
                       savePlpData = F, 
                       savePlpResult = F, 
                       saveEvaluation = F, 
                       savePlpPlots = F, 
                       analysisId = 'knnTest',
                       saveDirectory =  saveLoc)


test_that("covRef is correct size", {
  
  # varImp contains all variables in LR
  testthat::expect_equal(nrow(as.data.frame(plpData$covariateData$covariateRef)), 
                         nrow(plpResult$model$varImp))
  
  testthat::expect_equal(nrow(as.data.frame(plpDataReal$covariateData$covariateRef)), 
                         nrow(plpResultReal$model$varImp))
  
  testthat::expect_equal(nrow(as.data.frame(plpData$covariateData$covariateRef)), 
                         nrow(plpResultKNN$model$varImp))
  
})


test_that("LR, KNN and GBM results have same structure", {
  

  # same output names for LR, KNN and GBM
  testthat::expect_equal(names(plpResult), 
                         names(plpResultReal))
  testthat::expect_equal(names(plpResultReal), 
                         names(plpResultKNN))
 
})

test_that("fitting", {
  #=====================================
  # check fitPlp
  #=====================================
  testthat::expect_error(fitPlp(population=popualtion, data=plpData, modelSettings=NULL,
                                                        cohortId=1, outcomeId=2))
  testthat::expect_error(fitPlp(population=NULL, data=plpData, 
                                                        modelSettings=list(model='test'),
                                                        cohortId=1, outcomeId=2))
  testthat::expect_error(fitPlp(population=population, data=NULL, 
                                                        modelSettings=list(model='test'),
                                                        cohortId=1, outcomeId=2))
  #=====================================
  # checking Logistic Regression 
  #=====================================
  model_set <- setLassoLogisticRegression()
  testthat::expect_that(model_set, testthat::is_a("modelSettings"))
  testthat::expect_length(model_set,3)
  testthat::expect_error(setLassoLogisticRegression(variance = -3))
  testthat::expect_error(setLassoLogisticRegression(seed = 'F'))

  
  #=====================================
  # checking Cox Regression 
  #=====================================
  model_set <- setCoxModel()
  testthat::expect_that(model_set, testthat::is_a("modelSettings"))
  testthat::expect_length(model_set,3)
  testthat::expect_error(setCoxModel(variance = -3))
  testthat::expect_error(setCoxModel(seed = 'F'))
 
   #=====================================
  # checking Gradient Boosting Machine
  #=====================================
  gbm_set <- setGradientBoostingMachine(ntrees = 10)
  testthat::expect_that(gbm_set, testthat::is_a("modelSettings"))
  testthat::expect_length(gbm_set,3)
  testthat::expect_error(setGradientBoostingMachine(ntrees = -1))
  testthat::expect_error(setGradientBoostingMachine(minRows = 1))
  testthat::expect_error(setGradientBoostingMachine(maxDepth = 0))
  testthat::expect_error(setGradientBoostingMachine(learnRate = -2))
  testthat::expect_error(setGradientBoostingMachine(seed = 'F'))
  
 
  #=====================================
  # checking Random forest
  #=====================================
  #rf_set <- PatientLevelPrediction::setRandomForest(ntrees=10)
  #testthat::expect_that(rf_set, testthat::is_a("modelSettings"))
  #testthat::expect_length(rf_set,3)
  testthat::expect_error(setRandomForest(ntrees=-1))
  testthat::expect_error(setRandomForest(mtries = -4))
  testthat::expect_error(setRandomForest(maxDepth = 0))
  testthat::expect_error(setRandomForest(varImp = 3))
  testthat::expect_error(setRandomForest(seed = 'F'))
  
  #=====================================
  # checking Decision Tree
  #=====================================
  #dt_set <- PatientLevelPrediction::setDecisionTree()
  #testthat::expect_that(dt_set, is_a("modelSettings"))
  #testthat::expect_length(dt_set,3)
  testthat::expect_error(setDecisionTree(maxDepth = 0))
  testthat::expect_error(setDecisionTree(minSamplesSplit = 1))
  testthat::expect_error(setDecisionTree(minSamplesLeaf = -1))
  testthat::expect_error(setDecisionTree(minImpuritySplit = -1))
  testthat::expect_error(setDecisionTree(classWeight = 'dfds'))
  testthat::expect_error(setDecisionTree(classWeight = 4))
  testthat::expect_error(setDecisionTree(seed = 'F'))

  
  #=====================================
  # checking Ada Boost
  #=====================================
  #dt_set <- PatientLevelPrediction::setAdaBoost()
  #testthat::expect_that(dt_set, is_a("modelSettings"))
  #testthat::expect_length(dt_set,3)
  testthat::expect_error(setAdaBoost(nEstimators = 0))
  testthat::expect_error(setAdaBoost(learningRate = -1))
  testthat::expect_error(setAdaBoost(learningRate = 2))
  testthat::expect_error(setAdaBoost(seed = 'F'))
  
  #=====================================
  # checking KNN
  #=====================================
  model_set <- setKNN()
  testthat::expect_that(model_set, is_a("modelSettings"))
  testthat::expect_length(model_set,3)
  testthat::expect_error(setKNN(k = 0))
  testthat::expect_error(setKNN(indexFolder = 2372))
  
  #=====================================
  # checking Naive Bayes
  #=====================================
  #model_set <- setNaiveBayes()
  #testthat::expect_that(model_set, is_a("modelSettings"))
  #testthat::expect_length(model_set,3)

  
  #=====================================
  # checking MLP
  #=====================================
  #gbm_set <- setMLP()
  #testthat::expect_that(gbm_set, is_a("modelSettings"))
  #testthat::expect_length(gbm_set,3)
  testthat::expect_error(setMLP(size = -1))
  testthat::expect_error(setMLP(alpha = -1))
  testthat::expect_error(setMLP(seed = 'F'))
  
  
  })


gbmachSet <- setGradientBoostingMachine(ntrees = 10, maxDepth = 3, learnRate = 0.1)
plpResultGbmach <- runPlp(population = population,
                      plpData = plpData, 
                      modelSettings = gbmachSet, 
                      savePlpData = F, 
                      savePlpResult = F, 
                      saveEvaluation = F, 
                      savePlpPlots = F, 
                      analysisId = 'gbmachTest',
                      saveDirectory =  saveLoc)

test_that("GBM working checks", {
  
  # check same structure
  testthat::expect_equal(names(plpResultGbmach), 
                         names(plpResult))
  
  # check prediction same size as pop
  testthat::expect_equal(nrow(plpResultGbmach$prediction), nrow(population))
  
  # check prediction between 0 and 1
  testthat::expect_gte(min(plpResultGbmach$prediction$value), 0)
  testthat::expect_lte(max(plpResultGbmach$prediction$value), 1)
  
})


rfSet <- setRandomForest(ntrees = 10, maxDepth = 3)
plpResultRF <- runPlp(population = population,
                       plpData = plpData, 
                       modelSettings = rfSet, 
                       savePlpData = F, 
                       savePlpResult = F, 
                       saveEvaluation = F, 
                       savePlpPlots = F, 
                       analysisId = 'rfTest',
                       saveDirectory =  saveLoc)

test_that("RF working checks", {
  
  # check same structure
  testthat::expect_equal(names(plpResultRF), 
                         names(plpResult))
  
  # check prediction same size as pop
  testthat::expect_equal(nrow(plpResultRF$prediction), nrow(population))
  
  # check prediction between 0 and 1
  testthat::expect_gte(min(plpResultRF$prediction$value), 0)
  testthat::expect_lte(max(plpResultRF$prediction$value), 1)
  
})

abSet <- setAdaBoost(nEstimators = 5)
plpResultAb <- runPlp(population = population,
                      plpData = plpData, 
                      modelSettings = abSet, 
                      savePlpData = F, 
                      savePlpResult = F, 
                      saveEvaluation = F, 
                      savePlpPlots = F, 
                      analysisId = 'abTest',
                      saveDirectory =  saveLoc)

test_that("AdaBoost working checks", {
  # check same structure
  testthat::expect_equal(names(plpResultAb), 
                         names(plpResult))
  
  # check prediction same size as pop
  testthat::expect_equal(nrow(plpResultAb$prediction), nrow(population))
  
  # check prediction between 0 and 1
  testthat::expect_gte(min(plpResultAb$prediction$value), 0)
  testthat::expect_lte(max(plpResultAb$prediction$value), 1)
})

nbSet <- setNaiveBayes()
plpResultNb <- runPlp(population = population,
                      plpData = plpData, 
                      modelSettings = nbSet, 
                      savePlpData = F, 
                      savePlpResult = F, 
                      saveEvaluation = F, 
                      savePlpPlots = F, 
                      analysisId = 'nbTest',
                      saveDirectory =  saveLoc)

test_that("AdaBoost working checks", {
  # check same structure
  testthat::expect_equal(names(plpResultNb), 
                         names(plpResult))
  
  # check prediction same size as pop
  testthat::expect_equal(nrow(plpResultNb$prediction), nrow(population))
  
  # check prediction between 0 and 1
  testthat::expect_gte(min(plpResultNb$prediction$value), 0)
  testthat::expect_lte(max(plpResultNb$prediction$value), 1)
})


dtSet <- setDecisionTree(maxDepth = 2)
plpResultDt <- runPlp(population = population,
                      plpData = plpData, 
                      modelSettings = dtSet, 
                      savePlpData = F, 
                      savePlpResult = F, 
                      saveEvaluation = F, 
                      savePlpPlots = F, 
                      analysisId = 'dtTest',
                      saveDirectory =  saveLoc)

test_that("Decision tree working checks", {
  # check same structure
  testthat::expect_equal(names(plpResultDt), 
                         names(plpResult))
  
  # check prediction same size as pop
  testthat::expect_equal(nrow(plpResultDt$prediction), nrow(population))
  
  # check prediction between 0 and 1
  testthat::expect_gte(min(plpResultDt$prediction$value), 0)
  testthat::expect_lte(max(plpResultDt$prediction$value), 1)
})


mlpSet <- setMLP()
plpResultMlp <- runPlp(population = population,
                      plpData = plpData, 
                      modelSettings = mlpSet, 
                      savePlpData = F, 
                      savePlpResult = F, 
                      saveEvaluation = F, 
                      savePlpPlots = F, 
                      analysisId = 'mlpTest',
                      saveDirectory =  saveLoc)

test_that("MLP  working checks", {
  # check same structure
  testthat::expect_equal(names(plpResultMlp), 
                         names(plpResult))
  
  # check prediction same size as pop
  testthat::expect_equal(nrow(plpResultMlp$prediction), nrow(population))
  
  # check prediction between 0 and 1
  testthat::expect_gte(min(plpResultMlp$prediction$value), 0)
  testthat::expect_lte(max(plpResultMlp$prediction$value), 1)
})
