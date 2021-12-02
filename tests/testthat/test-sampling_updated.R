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
context("Sampling")

trainData <- list()
trainData$covaraiteData <- plpData$covaraiteData
trainData$labels <- population
trainData$folds <- data.frame(
  rowId = population$rowId, 
  index = sample(3, length(population$rowId), replace = t )
  )
attr(trainData, "metaData") <- list(settings = 'bla')


testType <- sample(c('none', 'underSample', 'overSample'), 1)
testNumberOutcomestoNonOutcomes <- 2
testSampleSeed <- sample(10000,1)

sampleSettingFunc <- function(
  type = testType,
  numberOutcomestoNonOutcomes = testNumberOutcomestoNonOutcomes,
  sampleSeed = testSampleSeed 
){
  
  result <- createSampleSettings(
    type = type ,
    numberOutcomestoNonOutcomes = numberOutcomestoNonOutcomes,
    sampleSeed = sampleSeed
  )
  
  return(result)
  
}


  
test_that("createSampleSettings works", {
  
  sampleSettings <- sampleSettingFunc()
  expect_is(sampleSettings, "sampleSettings")
  
  sampleFun <- 'sameData'
  if(testType == 'underSample'){
    sampleFun <- 'underSampleData'
  }
  if(testType == 'overSample'){
    sampleFun <- 'overSampleData'
  }
  
  expect_equal(
    attr(sampleSettings, "fun") , 
    sampleFun
  )
  
  expect_equal(
    sampleSettings$numberOutcomestoNonOutcomes, 
    testNumberOutcomestoNonOutcomes
    )
  
  expect_equal(
    sampleSettings$sampleSeed , 
    testSampleSeed 
  )
  
})


test_that("createSampleSettings expected errors", {
  
  expect_error(
    sampleSettingFunc(numberOutcomestoNonOutcomes = 'fsfd')
  )
  expect_error(
    sampleSettingFunc(numberOutcomestoNonOutcomes = -1)
  )
  
  expect_error(
    sampleSettingFunc(sampleSeed =  'fsfd')
  )
  
  expect_error(
    sampleSettingFunc(type =  'fsfd')
  )
  expect_error(
    sampleSettingFunc(type =  NULL)
  )
 
})


test_that("sampleData outputs are correct", {
  
  sampleSettings <- sampleSettingFunc(type = 'none') 
  
  sampleData <- sampleData(trainData, sampleSettings)
  
  # make sure metaData captures
  expect_equal(
    attr(sampleData, "metaData"),
    attr(trainData, "metaData")
  )
  
  expect_equal(
    attr(sampleData, "metaData")$sampling$settings,
    sampleSettings
  )
  
  # check the data is the same:
  expect_equal(
    nrow(sampleData$labels), 
    nrow(trainData$labels)
  )
  
  expect_equal(
    nrow(sampleData$folds), 
    nrow(trainData$folds)
  )
  
  expect_equal(
    sampleData$covariateData$covariates %>% dplyr::tally() %>% dplyr::pull(), 
    trainData$covariateData$covariates  %>% dplyr::tally() %>% dplyr::pull()
  )
  
  
})

# specific functions for sampling

 
test_that("underSampleData works", {
  
  sampleSettings <- list(sampleSeed = 1,
    numberOutcomestoNonOutcomes = 1)
  
  underSampleData <- underSampleData(trainData, sampleSettings)
  
  # the sampled data should be smaller...
  expect_true(nrow(underSampleData$labels) <= nrow(trainData$labels))
  
  expect_true(nrow(underSampleData$folds) <= nrow(trainData$folds))
  
  expect_true(
    underSampleData$covariateData$covariates %>% dplyr::tally() %>% dplyr::pull() <= trainData$covariateData$covariates  %>% dplyr::tally() %>% dplyr::pull()
  )
  
  # perhaps add manual data test
  
  
})

