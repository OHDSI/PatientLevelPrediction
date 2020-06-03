# Copyright 2020 Observational Health Data Sciences and Informatics
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
context("Keras")

deepSet <- setDeepNN(epochs = 1)

plpResultDeep <- runPlp(population = population,
                                               plpData = plpData, 
                                               modelSettings = deepSet, 
                                               savePlpData = F, 
                                               savePlpResult = F, 
                                               saveEvaluation = F, 
                                               savePlpPlots = F, 
                                               analysisId = 'deepTest',
                                               saveDirectory =  saveLoc)



#TODO: add input checks and test these...
#options(fftempdir = getwd())


test_that("deepNN working checks", {
  
  # check same structure
  testthat::expect_equal(names(plpResultDeep), 
                         names(plpResult))
  
  # check prediction same size as pop
  testthat::expect_equal(nrow(plpResultDeep$prediction), nrow(population))

  # check prediction between 0 and 1
  testthat::expect_gte(min(plpResultDeep$prediction$value), 0)
  testthat::expect_lte(max(plpResultDeep$prediction$value), 1)

})


# add temporal data:
CNN1Set <- setCovNN(epochs = 1, kernelSize = 3, batchSize =30)
plpResultCNN1 <- runPlp(population = population2,
                                                plpData = plpData3, 
                                                modelSettings = CNN1Set, 
                                                savePlpData = F, 
                                                savePlpResult = F, 
                                                saveEvaluation = F, 
                                                savePlpPlots = F, 
                                                analysisId = 'cnn1Test',
                                                saveDirectory =  saveLoc)

test_that("covNN working checks", {
  
  # check same structure
  testthat::expect_equal(names(plpResultCNN1), 
                         names(plpResult))
  
  # check prediction same size as pop
  testthat::expect_equal(nrow(plpResultCNN1$prediction), nrow(population2))
  
  # check prediction between 0 and 1
  testthat::expect_gte(min(plpResultCNN1$prediction$value), 0)
  testthat::expect_lte(max(plpResultCNN1$prediction$value), 1)
  
})

CNN2Set <- setCovNN2(epochs = 1, kernelSize = 4, filters=4)
plpResultCNN2 <- runPlp(population = population2,
                                                plpData = plpData3, 
                                                modelSettings = CNN2Set, 
                                                savePlpData = F, 
                                                savePlpResult = F, 
                                                saveEvaluation = F, 
                                                savePlpPlots = F, 
                                                analysisId = 'cnn1Test',
                                                saveDirectory =  saveLoc)

test_that("covNN2 working checks", {
  
  # check same structure
  testthat::expect_equal(names(plpResultCNN2), 
                         names(plpResult))
  
  # check prediction same size as pop
  testthat::expect_equal(nrow(plpResultCNN2$prediction), nrow(population2))
  
  # check prediction between 0 and 1
  testthat::expect_gte(min(plpResultCNN2$prediction$value), 0)
  testthat::expect_lte(max(plpResultCNN2$prediction$value), 1)
  
})


if(!travis){
CIReNNSet <- setCIReNN(epochs = 1, useVae = F, units=c(10) )
plpResultCIReNN <- runPlp(population = population2, plpData = plpData3, 
                                         minCovariateFraction = 0.001, normalizeData = F, 
                                         modelSettings = CIReNNSet, testSplit = 'person', 
                                         testFraction = 0.25, splitSeed = 1, 
                                         nfold = 3, savePlpData = F, savePlpResult = F,
                                         savePlpPlots = F,
                                         saveEvaluation = F, 
                          analysisId = 'cireNNTest',
                          saveDirectory =  saveLoc)
}

test_that("CIReNN working checks", {
  if(travis){
    skip("Too slow for travis")
  }
  # check same structure
  testthat::expect_equal(names(plpResultCIReNN), 
                         names(plpResult))
  
  # check prediction same size as pop
  testthat::expect_equal(nrow(plpResultCIReNN$prediction), nrow(population2))
  
  # check prediction between 0 and 1
  testthat::expect_gte(min(plpResultCIReNN$prediction$value), 0)
  testthat::expect_lte(max(plpResultCIReNN$prediction$value), 1)
  
})

