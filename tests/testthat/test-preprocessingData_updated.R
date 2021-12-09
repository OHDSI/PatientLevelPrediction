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


createDefaultSettings <- function(
  minCovariateFraction = 0.001,
  normalizeData = TRUE,
  removeRedundancy = TRUE
){
  result <- createPreprocessSettings(
    minFraction = minCovariateFraction,
    normalize = normalizeData,
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
  
  expect_is(settings, "preprocessSettings")
  expect_equal(settings$minFraction, minCovariateFractionTest)
  expect_equal(settings$normalize, normalizeDataTest)
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
  covSize <- trainData$covariateData$covariates %>% dplyr::tally() %>% dplyr::pull()
  metaLength <- length(metaData)
  oldFeatureCount <- trainData$covariateData$covariateRef %>% dplyr::tally() %>% dplyr::pull()
  
  preprocessSettings <- createDefaultSettings(
    minCovariateFraction = 0.01,
    normalizeData = F,
    removeRedundancy = F
  )
  newData <- preprocessData(trainData$covariateData, preprocessSettings)
  
  expect_is(newData, 'CovariateData')
  expect_equal(length(attr(newData, "metaData")), 1+length(metaData))
  expect_true(newData$covariates %>% dplyr::tally() %>% dplyr::pull() < covSize)
  
  # metaData should have tidyCovariateDataSettings (so 1 bigger)
  expect_equal(length(attr(newData, "metaData")), metaLength+1)
  
  expect_true(length(attr(newData, "metaData")$tidyCovariateDataSettings$deletedInfrequentCovariateIds)>=0)
  expect_equal(attr(newData, "metaData")$tidyCovariateDataSettings$deletedRedundantCovariateIds, NULL)
  expect_equal(attr(newData, "metaData")$tidyCovariateDataSettings$normFactors, NULL)
  
  newFeatureCount <- newData$covariateRef %>% dplyr::tally() %>% dplyr::pull() + length(attr(newData, "metaData")$tidyCovariateDataSettings$deletedInfrequentCovariateIds)
  
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
  
  newFeatureCount <- newData$covariateRef %>% dplyr::tally() %>% dplyr::pull() + 
    length(attr(newData, "metaData")$tidyCovariateDataSettings$deletedRedundantCovariateIds) 
  
  expect_equal(newFeatureCount, oldFeatureCount)
  
})
