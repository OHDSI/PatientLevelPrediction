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
context("DemographicSummary binary")

test_that("getDemographicSummary", {
  prediction <- data.frame(
    rowId = 1:100, 
    ageYear = sample(100, 100, replace = T),
    gender = sample(c(8507,'female'), 100, replace = T),
    value= runif(100), 
    outcomeCount = round(runif(100)),
    evaluation = rep('Test', 100)
    )

  demoSum <- getDemographicSummary(
    prediction = prediction,
    predictionType = 'binary',
    typeColumn = 'evaluation'
    )
  
  expect_that(ncol(demoSum), equals(12))
  expect_true('evaluation' %in% colnames(demoSum))
  
  # check correct gender length
  expect_equal(length(unique(prediction$gender)), length(unique(demoSum$genGroup)))
  
  
  demoSumBin <- getDemographicSummary_binary(
    prediction = prediction,
    evalColumn = 'evaluation'
  )
  expect_equal(demoSum,demoSumBin)
  
})


test_that("getDemographicSummary", {
  prediction <- data.frame(
    rowId = 1:100, 
    ageYear = sample(100, 100, replace = T),
    gender = sample(c(8507,'female'), 100, replace = T),
    value= runif(100), 
    outcomeCount = round(runif(100)),
    evaluation = rep('Test', 100),
    survivalTime = 50 + sample(730, 100, replace = T)
  )
  
  demoSumSurv <- getDemographicSummary_survival(
    prediction = prediction,
    evalColumn = 'evaluation',
    timepoint = 365
  )
  
  expect_is(demoSumSurv, 'data.frame')
  expect_that(ncol(demoSumSurv), equals(8))
  expect_true('evaluation' %in% colnames(demoSumSurv))
  
  # check correct gender length
  expect_equal(length(unique(prediction$gender)), length(unique(demoSumSurv$genGroup)))
  
  
})
