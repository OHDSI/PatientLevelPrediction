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
context("Torch")

lrtSet <- setLRTorch(w_decay=0.0005, 
                     epochs=1, 
                     class_weight = 0, 
                     autoencoder = FALSE, 
                     vae =FALSE)
  

plpResultLrt <- runPlp(population = population,
                                               plpData = plpData, 
                                               modelSettings = lrtSet, 
                                               savePlpData = F, 
                                               savePlpResult = F, 
                                               saveEvaluation = F, 
                                               savePlpPlots = F, 
                                               analysisId = 'lrtTest',
                                               saveDirectory =  saveLoc)



#TODO: add input checks and test these...
#options(fftempdir = getwd())


test_that("torch LR working checks", {
  
  # check same structure
  testthat::expect_equal(names(plpResultLrt), 
                         names(plpResult))
  
  # check prediction same size as pop
  testthat::expect_equal(nrow(plpResultLrt$prediction), nrow(population))

  # check prediction between 0 and 1
  testthat::expect_gte(min(plpResultLrt$prediction$value), 0)
  testthat::expect_lte(max(plpResultLrt$prediction$value), 1)

})

mlptSet <- setMLPTorch(size = 10, 
                       w_decay = 0.001, 
                       epochs = 1,
                       autoencode = F)


plpResultMlpt <- runPlp(population = population,
                       plpData = plpData, 
                       modelSettings = mlptSet, 
                       savePlpData = F, 
                       savePlpResult = F, 
                       saveEvaluation = F, 
                       savePlpPlots = F, 
                       analysisId = 'mlptTest',
                       saveDirectory =  saveLoc)

test_that("MLP LR working checks", {
  
  # check same structure
  testthat::expect_equal(names(plpResultMlpt), 
                         names(plpResult))
  
  # check prediction same size as pop
  testthat::expect_equal(nrow(plpResultMlpt$prediction), nrow(population))
  
  # check prediction between 0 and 1
  testthat::expect_gte(min(plpResultMlpt$prediction$value), 0)
  testthat::expect_lte(max(plpResultMlpt$prediction$value), 1)
  
})


# add temporal data:
if(!travis){
RNNTSet <- setRNNTorch(hidden_size = 1, 
                        epochs =1)
plpResultRNNT <- runPlp(population = population2,
                                                plpData = plpData3, 
                                                modelSettings = RNNTSet, 
                                                savePlpData = F, 
                                                savePlpResult = F, 
                                                saveEvaluation = F, 
                                                savePlpPlots = F, 
                                                analysisId = 'rnntTest',
                                                saveDirectory =  saveLoc)
}

test_that("RNN Torch working checks", {
  if(travis){
    skip("Too slow for travis")
  }
  # check same structure
  testthat::expect_equal(names(plpResultRNNT), 
                         names(plpResult))
  
  # check prediction same size as pop
  testthat::expect_equal(nrow(plpResultRNNT$prediction), nrow(population2))
  
  # check prediction between 0 and 1
  testthat::expect_gte(min(plpResultRNNT$prediction$value), 0)
  testthat::expect_lte(max(plpResultRNNT$prediction$value), 1)
  
})

# add CNN when it is fixed

