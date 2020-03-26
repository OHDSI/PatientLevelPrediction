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

coxSet <- setCoxModel()

plpResultCox <- runPlp(population = population,
                                               plpData = plpData, 
                                               modelSettings = coxSet, 
                                               savePlpData = F, 
                                               savePlpResult = F, 
                                               saveEvaluation = F, 
                                               savePlpPlots = F, 
                                               analysisId = 'coxTest',
                                               saveDirectory =  saveLoc)



context("Survival")
#TODO: add input checks and test these...
#options(fftempdir = getwd())


test_that("Cox working checks", {
  
  # check same structure
  testthat::expect_equal(names(plpResult), 
                         names(plpResultCox))
  
  # check prediction same size as pop
  testthat::expect_equal(nrow(plpResultCox$prediction), nrow(population))


})



test_that("GBM survival errors", {
  
  # check seed error
  testthat::expect_error(setGBMSurvival(loss = 'coxph', 
                                        learningRate = 0.1,
                                        nEstimators = 10, 
                                        criterion = 'friedman_mse', 
                                        minSamplesSplit = 2, 
                                        minSamplesLeaf = 1, 
                                        minWeightFractionLeaf = 0, 
                                        maxDepth = 3, 
                                        minImpuritySplit = NULL, 
                                        minImpurityDecrease = 0, 
                                        maxFeatures = NULL, 
                                        maxLeafNodes = NULL,
                                        presort = 'auto', 
                                        subsample = 1, 
                                        dropoutRate = 0, 
                                        seed = 'error', 
                                        quiet = F))
  
  # check learning rate error
  testthat::expect_error(setGBMSurvival(loss = 'coxph', 
                                        learningRate = 'none',
                                        nEstimators = 10, 
                                        criterion = 'friedman_mse', 
                                        minSamplesSplit = 2, 
                                        minSamplesLeaf = 1, 
                                        minWeightFractionLeaf = 0, 
                                        maxDepth = 3, 
                                        minImpuritySplit = NULL, 
                                        minImpurityDecrease = 0, 
                                        maxFeatures = NULL, 
                                        maxLeafNodes = NULL,
                                        presort = 'auto', 
                                        subsample = 1, 
                                        dropoutRate = 0, 
                                        seed = NULL, 
                                        quiet = F))
  
  # check learning rate error <0
  testthat::expect_error(setGBMSurvival(loss = 'coxph', 
                                        learningRate = -0.1,
                                        nEstimators = 10, 
                                        criterion = 'friedman_mse', 
                                        minSamplesSplit = 2, 
                                        minSamplesLeaf = 1, 
                                        minWeightFractionLeaf = 0, 
                                        maxDepth = 3, 
                                        minImpuritySplit = NULL, 
                                        minImpurityDecrease = 0, 
                                        maxFeatures = NULL, 
                                        maxLeafNodes = NULL,
                                        presort = 'auto', 
                                        subsample = 1, 
                                        dropoutRate = 0, 
                                        seed = NULL, 
                                        quiet = F))
  
  # check learning rate error > 1
  testthat::expect_error(setGBMSurvival(loss = 'coxph', 
                                        learningRate = 1.1,
                                        nEstimators = 10, 
                                        criterion = 'friedman_mse', 
                                        minSamplesSplit = 2, 
                                        minSamplesLeaf = 1, 
                                        minWeightFractionLeaf = 0, 
                                        maxDepth = 3, 
                                        minImpuritySplit = NULL, 
                                        minImpurityDecrease = 0, 
                                        maxFeatures = NULL, 
                                        maxLeafNodes = NULL,
                                        presort = 'auto', 
                                        subsample = 1, 
                                        dropoutRate = 0, 
                                        seed = NULL, 
                                        quiet = F))
  
  # check nEstimators error
  testthat::expect_error(setGBMSurvival(loss = 'coxph', 
                                        learningRate = 0.1,
                                        nEstimators = 'none', 
                                        criterion = 'friedman_mse', 
                                        minSamplesSplit = 2, 
                                        minSamplesLeaf = 1, 
                                        minWeightFractionLeaf = 0, 
                                        maxDepth = 3, 
                                        minImpuritySplit = NULL, 
                                        minImpurityDecrease = 0, 
                                        maxFeatures = NULL, 
                                        maxLeafNodes = NULL,
                                        presort = 'auto', 
                                        subsample = 1, 
                                        dropoutRate = 0, 
                                        seed = NULL, 
                                        quiet = F))
  
  
  # check nEstimators error < 1
  testthat::expect_error(setGBMSurvival(loss = 'coxph', 
                                        learningRate = 0.1,
                                        nEstimators = 0.5, 
                                        criterion = 'friedman_mse', 
                                        minSamplesSplit = 2, 
                                        minSamplesLeaf = 1, 
                                        minWeightFractionLeaf = 0, 
                                        maxDepth = 3, 
                                        minImpuritySplit = NULL, 
                                        minImpurityDecrease = 0, 
                                        maxFeatures = NULL, 
                                        maxLeafNodes = NULL,
                                        presort = 'auto', 
                                        subsample = 1, 
                                        dropoutRate = 0, 
                                        seed = NULL, 
                                        quiet = F))
  
})


setGBMSurv <- setGBMSurvival(loss = 'coxph', 
                             learningRate = 0.1,
                             nEstimators = 1, 
                             criterion = 'friedman_mse', 
                             minSamplesSplit = 2, 
                             minSamplesLeaf = 1, 
                             minWeightFractionLeaf = 0, 
                             maxDepth = 3, 
                             minImpuritySplit = NULL, 
                             minImpurityDecrease = 0, 
                             maxFeatures = NULL, 
                             maxLeafNodes = NULL,
                             presort = 'auto', 
                             subsample = 1, 
                             dropoutRate = 0, 
                             seed = NULL, 
                             quiet = F)

plpResultGBMSurv <- runPlp(population = population,
                       plpData = plpData, 
                       modelSettings = setGBMSurv, 
                       savePlpData = F, 
                       savePlpResult = F, 
                       saveEvaluation = F, 
                       savePlpPlots = F, 
                       analysisId = 'gbmSurvTest',
                       saveDirectory =  saveLoc)




test_that("GBM survival", {
  
  # check same structure
  testthat::expect_equal(names(plpResult), 
                         names(plpResultGBMSurv))
  
  # check prediction same size as pop
  testthat::expect_equal(nrow(plpResultGBMSurv$prediction), nrow(population))
  
  
})

  
