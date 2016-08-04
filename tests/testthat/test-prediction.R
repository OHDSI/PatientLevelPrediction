# Copyright 2016 Observational Health Data Sciences and Informatics
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

context("Prediction")

# this no longer checks predictions as models don't exist and take too long to train during test

test_that("prediction", {
  # function for testing each model:
  checkPrediction <- function(model, plpData, population, index){
    
    # model class is correct
    testthat::expect_that(model, is_a("plpModel"))
    prediction <- PatientLevelPrediction::predictPlp(model, population, plpData, index)
    # prediction size is correct
    testthat::expect_that(length(population$rowId[population$indexes<0]), 
                          is_equivalent_to(nrow(prediction)))
    testthat::expect_that(sum(population$rowId[population$indexes<0]%in%prediction$rowId), 
                          is_equivalent_to(sum(population$indexes<0)))
    
    # varImp contains all variables
    testthat::expect_that(nrow(ff::as.ram(plpData$covariateRef)), 
                          testthat::is_equivalent_to(nrow(model$varImp)))
    
  }
  # generate simulated data:
  set.seed(1234)
  data(plpDataSimulationProfile)
  sampleSize <- 2000
  plpData <- simulatePlpData(plpDataSimulationProfile, n = sampleSize)
  
  plpData.lsvm <- convertToLibsvm(plpData,'./libsvm')
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
  
  index <- PatientLevelPrediction::personSplitter(population, test=0.2, seed=1)
  population <- merge(population, index)
  colnames(population)[colnames(population)=='index'] <- 'indexes'
  
  #=====================================
  # checking Logistic Regression 
  #=====================================
  model_set <- setLassoLogisticRegression()
  testthat::expect_that(model_set, is_a("modelSettings"))
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
  testthat::expect_that(gbm_set, is_a("modelSettings"))
  ##model <- loadPlpModel('gbm_model')  # (NEEDED MODEL FOLDER)
  ##checkPrediction(model, plpData=plpData, population, index)
  
  
  #=====================================
  # checking Random forest
  #=====================================
  #rf_set <- PatientLevelPrediction::randomForest.set(ntrees=10)
  #testthat::expect_that(rf_set, is_a("modelSettings"))
  #model <- loadPlpModel('rf_model')
  #checkPrediction(model, plpData=plpData.lsvm, population, index)
  
  #=====================================
  # checking Naive Bayes
  #=====================================
  #nb_set <- naiveBayes.set()
  #testthat::expect_that(nb_set, is_a("modelSettings"))
  #model <- loadPlpModel('nb_model')
  #checkPrediction(model, plpData=plpData.lsvm, population, index)
  
  # removing files created duirng the test:
  unlink('./plpmodels', recursive = T, force = T)
  unlink('./mycache', recursive = T, force = T)
  unlink('./python_models', recursive = T, force = T)
  unlink('./libsvm', recursive = T, force = T)
  
  })


