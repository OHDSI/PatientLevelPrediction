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

context("Fitting")

trainData <- createTrainData(plpData, population)
modelSettings <- setLassoLogisticRegression()

test_that("fitPlp", {
  
plpModel <- fitPlp(
  trainData = trainData,   
  modelSettings = modelSettings,
  search = "grid", 
  analysisId = 'fitting'
  )

expect_is(plpModel, 'plpModel')

})

test_that("fitPlp input errors", {
  
  expect_error(
    fitPlp(
      trainData = trainData,   
      modelSettings = modelSettings
    )
  )
  
  expect_error(
    fitPlp(
      trainData = list(covariateData = NULL),   
      modelSettings = modelSettings,
      analysisId = 'fitting'
    )
  )
  
  expect_error(
    fitPlp(
      trainData = trainData,   
      modelSettings = NULL,
      analysisId = 'fitting'
    )
  )
  
})

