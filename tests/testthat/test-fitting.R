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

# this no longer checks predictions as models don't exist and take too long to train during test
# generate simulated data:
set.seed(1234)
data(plpDataSimulationProfile)
sampleSize <- 2000
plpData <- simulatePlpData(plpDataSimulationProfile, n = sampleSize)

# create popualtion for outcome 2
population <- createStudyPopulation(plpData,
                                    outcomeId = 2,
                                    firstExposureOnly = FALSE,
                                    washoutPeriod = 0,
                                    removeSubjectsWithPriorOutcome = FALSE,
                                    priorOutcomeLookback = 99999,
                                    requireTimeAtRisk = FALSE,
                                    minTimeAtRisk=0,
                                    riskWindowStart = 0,
                                    addExposureDaysToStart = FALSE,
                                    riskWindowEnd = 365,
                                    addExposureDaysToEnd = FALSE
                                    #,verbosity=INFO
)

index <- PatientLevelPrediction::randomSplitter(population, test=0.2, seed=1)
population <- merge(population, index)
colnames(population)[colnames(population)=='index'] <- 'indexes'

test_that("fitting", {
  # function for testing each model:
  #checkPrediction <- function(model, plpData, population, index){
  #  
  #  # model class is correct
  #  testthat::expect_that(model, is_a("plpModel"))
  #  prediction <- PatientLevelPrediction::predictPlp(model, population, plpData, index)
  #  # prediction size is correct
  #  testthat::expect_that(length(population$rowId[population$indexes<0]), 
  #                        is_equivalent_to(nrow(prediction)))
  #  testthat::expect_that(sum(population$rowId[population$indexes<0]%in%prediction$rowId), 
  #                        is_equivalent_to(sum(population$indexes<0)))
  #  
  #  # varImp contains all variables
  #  testthat::expect_that(nrow(ff::as.ram(plpData$covariateRef)), 
  #                        testthat::is_equivalent_to(nrow(model$varImp)))
  #  
  #}
  
  
  #=====================================
  # check fitPlp
  #=====================================
  testthat::expect_error(PatientLevelPrediction::fitPlp(population=popualtion, data=plpData, modelSettings=NULL,
                                                        cohortId=1, outcomeId=2))
  testthat::expect_error(PatientLevelPrediction::fitPlp(population=NULL, data=plpData, 
                                                        modelSettings=list(model='test'),
                                                        cohortId=1, outcomeId=2))
  testthat::expect_error(PatientLevelPrediction::fitPlp(population=population, data=NULL, 
                                                        modelSettings=list(model='test'),
                                                        cohortId=1, outcomeId=2))
  #=====================================
  # checking Logistic Regression 
  #=====================================
  model_set <- setLassoLogisticRegression()
  testthat::expect_that(model_set, testthat::is_a("modelSettings"))
  testthat::expect_length(model_set,3)
  testthat::expect_error(PatientLevelPrediction::setLassoLogisticRegression(variance = -3))
  testthat::expect_error(PatientLevelPrediction::setLassoLogisticRegression(seed = 'F'))
  testthat::expect_error(PatientLevelPrediction:::fitLassoLogisticRegression())
  testthat::expect_error(PatientLevelPrediction:::fitLassoLogisticRegression(population,plpData=list(), 
                                                                            param=NULL, outcomeId = 1,
                                                                            cohortId = 2))
  
  
  #=====================================
  # checking Cox Regression 
  #=====================================
  model_set <- setCoxModel()
  testthat::expect_that(model_set, testthat::is_a("modelSettings"))
  testthat::expect_length(model_set,3)
  testthat::expect_error(PatientLevelPrediction::setCoxModel(variance = -3))
  testthat::expect_error(PatientLevelPrediction::setCoxModel(seed = 'F'))
  testthat::expect_error(PatientLevelPrediction:::fitCoxModel())
  testthat::expect_error(PatientLevelPrediction:::fitCoxModel(population,plpData=list(), 
                                                                             param=NULL, outcomeId = 1,
                                                                             cohortId = 2))
  
  #checkPrediction(model_set=lr_set, plpData=plpData, population, index)
  ##model <-PatientLevelPrediction::fitPlp(population[population$indexes>0,], data=plpData,  
  ##               modelSettings=model_set, 
  ##               cohortId=0, outcomeId=2)
  ##savePlpModel(model, './tests/testthat/lr_model')
  ##model <- loadPlpModel('lr_model') # (NEEDED MODEL FOLDER)
  ##checkPrediction(model, plpData=plpData, population, index)
  
  #=====================================
  # checking Gradient Boosting Machine
  #=====================================
  gbm_set <- setGradientBoostingMachine(ntrees = 10)
  testthat::expect_that(gbm_set, testthat::is_a("modelSettings"))
  testthat::expect_length(gbm_set,3)
  testthat::expect_error(PatientLevelPrediction::setGradientBoostingMachine(ntrees = -1))
  testthat::expect_error(PatientLevelPrediction::setGradientBoostingMachine(minRows = 1))
  testthat::expect_error(PatientLevelPrediction::setGradientBoostingMachine(maxDepth = 0))
  testthat::expect_error(PatientLevelPrediction::setGradientBoostingMachine(learnRate = -2))
  testthat::expect_error(PatientLevelPrediction::setGradientBoostingMachine(seed = 'F'))
  
  testthat::expect_error(PatientLevelPrediction:::fitGradientBoostingMachine())
  testthat::expect_error(PatientLevelPrediction:::fitGradientBoostingMachine(population,plpData=list(), 
                                                                 param=NULL, outcomeId = 1,
                                                                 cohortId = 2))
  testthat::expect_error(PatientLevelPrediction:::fitGradientBoostingMachine(population,plpData=plpData, 
                                                                 param=NULL, outcomeId = NULL,
                                                                 cohortId = 2))
  ##model <- loadPlpModel('gbm_model')  # (NEEDED MODEL FOLDER)
  ##checkPrediction(model, plpData=plpData, population, index)
  
  
  #=====================================
  # checking Random forest
  #=====================================
  #rf_set <- PatientLevelPrediction::setRandomForest(ntrees=10)
  #testthat::expect_that(rf_set, testthat::is_a("modelSettings"))
  #testthat::expect_length(rf_set,3)
  testthat::expect_error(PatientLevelPrediction::setRandomForest(ntrees=-1))
  testthat::expect_error(PatientLevelPrediction::setRandomForest(mtries = -4))
  testthat::expect_error(PatientLevelPrediction::setRandomForest(maxDepth = 0))
  testthat::expect_error(PatientLevelPrediction::setRandomForest(varImp = 3))
  testthat::expect_error(PatientLevelPrediction::setRandomForest(seed = 'F'))
  
  testthat::expect_error(PatientLevelPrediction:::fitRandomForest())
  testthat::expect_error(PatientLevelPrediction:::fitRandomForest(population,plpData=list(), 
                                                                 param=NULL, outcomeId = 1,
                                                                 cohortId = 2))
  testthat::expect_error(PatientLevelPrediction:::fitRandomForest(population,plpData=plpData, 
                                                                 param=NULL, outcomeId = NULL,
                                                                 cohortId = 2))
  
  #testthat::expect_that(rf_set, is_a("modelSettings"))
  #model <- loadPlpModel('rf_model')
  #checkPrediction(model, plpData=plpData.lsvm, population, index)
  
  
  #=====================================
  # checking Decision Tree
  #=====================================
  #dt_set <- PatientLevelPrediction::setDecisionTree()
  #testthat::expect_that(dt_set, is_a("modelSettings"))
  #testthat::expect_length(dt_set,3)
  testthat::expect_error(PatientLevelPrediction::setDecisionTree(maxDepth = 0))
  testthat::expect_error(PatientLevelPrediction::setDecisionTree(minSamplesSplit = 1))
  testthat::expect_error(PatientLevelPrediction::setDecisionTree(minSamplesLeaf = -1))
  testthat::expect_error(PatientLevelPrediction::setDecisionTree(minImpuritySplit = -1))
  testthat::expect_error(PatientLevelPrediction::setDecisionTree(classWeight = 'dfds'))
  testthat::expect_error(PatientLevelPrediction::setDecisionTree(classWeight = 4))
  testthat::expect_error(PatientLevelPrediction::setDecisionTree(seed = 'F'))
  
  testthat::expect_error(PatientLevelPrediction:::fitDecisionTree())
  testthat::expect_error(PatientLevelPrediction:::fitDecisionTree(population,plpData=list(), 
                                                                 param=NULL, outcomeId = 1,
                                                                 cohortId = 2))
  testthat::expect_error(PatientLevelPrediction:::fitDecisionTree(population,plpData=plpData, 
                                                                 param=NULL, outcomeId = NULL,
                                                                 cohortId = 2))
  
  
  #=====================================
  # checking Ada Boost
  #=====================================
  #dt_set <- PatientLevelPrediction::setAdaBoost()
  #testthat::expect_that(dt_set, is_a("modelSettings"))
  #testthat::expect_length(dt_set,3)
  testthat::expect_error(PatientLevelPrediction::setAdaBoost(nEstimators = 0))
  testthat::expect_error(PatientLevelPrediction::setAdaBoost(learningRate = -1))
  testthat::expect_error(PatientLevelPrediction::setAdaBoost(learningRate = 2))
  testthat::expect_error(PatientLevelPrediction::setAdaBoost(seed = 'F'))
  
  testthat::expect_error(PatientLevelPrediction:::fitAdaBoost())
  testthat::expect_error(PatientLevelPrediction:::fitAdaBoost(population,plpData=list(), 
                                                                 param=NULL, outcomeId = 1,
                                                                 cohortId = 2))
  testthat::expect_error(PatientLevelPrediction:::fitAdaBoost(population,plpData=plpData, 
                                                                 param=NULL, outcomeId = NULL,
                                                                 cohortId = 2))
  
  #=====================================
  # checking KNN
  #=====================================
  model_set <- setKNN()
  testthat::expect_that(model_set, is_a("modelSettings"))
  testthat::expect_length(model_set,3)
  testthat::expect_error(PatientLevelPrediction::setKNN(k = 0))
  testthat::expect_error(PatientLevelPrediction::setKNN(indexFolder = 2372))
  testthat::expect_error(PatientLevelPrediction:::fitKNN())
  testthat::expect_error(PatientLevelPrediction:::fitKNN(population,plpData=list(), 
                                                                            param=NULL, outcomeId = 1,
                                                                            cohortId = 2))
  testthat::expect_error(PatientLevelPrediction:::fitKNN(population,plpData=plpData, 
                                                                            param=NULL, outcomeId = NULL,
                                                                            cohortId = 2))
  
  #=====================================
  # checking Naive Bayes
  #=====================================
  #model_set <- setNaiveBayes()
  #testthat::expect_that(model_set, is_a("modelSettings"))
  #testthat::expect_length(model_set,3)
  testthat::expect_error(PatientLevelPrediction:::fitNaiveBayes())
  testthat::expect_error(PatientLevelPrediction:::fitNaiveBayes(population,plpData=list(), 
                                                        param=NULL, outcomeId = 1,
                                                        cohortId = 2))
  testthat::expect_error(PatientLevelPrediction:::fitNaiveBayes(population,plpData=plpData, 
                                                        param=NULL, outcomeId = NULL,
                                                        cohortId = 2))
  
  #nb_set <- naiveBayes.set()
  #testthat::expect_that(nb_set, is_a("modelSettings"))
  #model <- loadPlpModel('nb_model')
  #checkPrediction(model, plpData=plpData.lsvm, population, index)
  
  
  
  
  #=====================================
  # checking MLP
  #=====================================
  #gbm_set <- setMLP()
  #testthat::expect_that(gbm_set, is_a("modelSettings"))
  #testthat::expect_length(gbm_set,3)
  testthat::expect_error(PatientLevelPrediction::setMLP(size = -1))
  testthat::expect_error(PatientLevelPrediction::setMLP(alpha = -1))
  testthat::expect_error(PatientLevelPrediction::setMLP(seed = 'F'))
  
  testthat::expect_error(PatientLevelPrediction:::fitMLP())
  testthat::expect_error(PatientLevelPrediction:::fitMLP(population,plpData=list(), 
                                                                            param=NULL, outcomeId = 1,
                                                                            cohortId = 2))
  testthat::expect_error(PatientLevelPrediction:::fitMLP(population,plpData=plpData, 
                                                                            param=NULL, outcomeId = NULL,
                                                                            cohortId = 2))
  ##model <- loadPlpModel('gbm_model')  # (NEEDED MODEL FOLDER)
  ##checkPrediction(model, plpData=plpData, population, index)
  
  # removing files created duirng the test:
  unlink('./plpmodels', recursive = T, force = T)
  unlink('./python_models', recursive = T, force = T)
  
  })


