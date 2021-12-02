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
context("preprocessingData")

trainData <- createTrainData(plpData, population)

createDefaultSettings <- function(
  minCovariateFraction = 0.001,
  normalizeData = TRUE,
  removeRedundancy = TRUE
){
  result <- createPreprocessSettings(
    minCovariateFraction = minCovariateFraction,
    normalizeData = normalizeData,
    removeRedundancy = removeRedundancy
  )
  return(result)
}

test_that("createPreprocessSettings", {
  
  normalizeDataTest <- sample(c(T,F),1)
  removeRedundancyTest <- sample(c(T,F),1)
  minCovariateFractionTest <- 1/(1000+sample(1000,1))
  
  settings <- createDefaultSettings(
    minCovariateFraction = minCovariateFractionTest,
    normalizeData = normalizeDataTest, 
    removeRedundancy = removeRedundancyTest)
  
  expect_is(settings, "preprocessingSettings")
  expect_equal(settings$minCovariateFraction, minCovariateFractionTest)
  expect_equal(settings$normalizeData, normalizeDataTest)
  expect_equal(settings$removeRedundancy, removeRedundancyTest)
  
  expect_error(createDefaultSettings(minCovariateFraction = -1))
  expect_error(createDefaultSettings(minCovariateFraction = 'dfdfdf'))
  
  expect_error(createDefaultSettings(removeRedundancy = 'dfdfdf'))
  expect_error(createDefaultSettings(removeRedundancy = NULL))
  
  expect_error(createDefaultSettings(normalizeData = 'dfdfdf'))
  expect_error(createDefaultSettings(normalizeData = NULL))
})

test_that("createPreprocessSettings", {
  trainData <- createTrainData(plpData, population)
  
  metaData <- attr(trainData$covariateData, "metaData")
  covSize <- trainData$covariateData %>% dplyr::tally() %>% dplyr::pull()
  metaLength <- length(attr(trainData$covariateData, "metaData"))
  oldFeatureCount <- trainData$covariateData$covariateRef %>% dplyr::tally %>% dplyr::count()
  
  preprocessSettings <- createDefaultSettings(
    minCovariateFraction = 0.01,
    normalizeData = F,
    removeRedundancy = F
  )
  newData <- preprocessData(trainData$covariateData, preprocessSettings)
  
  expect_is(newData, 'covariateData')
  expect_equal(attr(newData, "metaData"), metaData)
  expect_true(newData %>% dplyr::tally() %>% dplyr::pull() < covSize)
  
  # metaData should have tidyCovariateDataSettings (so 1 bigger)
  expect_equal(length(attr(newData, "metaData")), metaLength+1)
  
  expect_true(length(attr(newData, "metaData")$tidyCovariateDataSettings$deletedInfrequentCovariateIds)>=0)
  expect_equal(attr(newData, "metaData")$tidyCovariateDataSettings$deletedRedundantCovariateIds, NULL)
  expect_equal(attr(newData, "metaData")$tidyCovariateDataSettings$normFactors, NULL)
  
  newFeatureCount <- newData$covariateRef %>% dplyr::tally %>% dplyr::count() + length(attr(newData, "metaData")$tidyCovariateDataSettings$deletedInfrequentCovariateIds)
  
  expect_equal(newFeatureCount, oldFeatureCount)
  
  trainData <- createTrainData(plpData, population)
  metaData <- attr(trainData$covariateData, "metaData")
  preprocessSettings <- createDefaultSettings(
    minCovariateFraction = 0,
    normalizeData = T,
    removeRedundancy = T
  )
  newData <- preprocessData(trainData$covariateData, preprocessSettings)
  expect_true(length(attr(newData, "metaData")$tidyCovariateDataSettings$deletedInfrequentCovariateIds)==0)
  expect_true(length(attr(newData, "metaData")$tidyCovariateDataSettings$deletedRedundantCovariateIds)>=0)
  expect_true(length(attr(newData, "metaData")$tidyCovariateDataSettings$normFactors)>=0)
  
  newFeatureCount <- newData$covariateRef %>% dplyr::tally %>% dplyr::count() + 
    length(attr(newData, "metaData")$tidyCovariateDataSettings$deletedRedundantCovariateIds) 
  
  expect_equal(newFeatureCount, oldFeatureCount)
  
})
