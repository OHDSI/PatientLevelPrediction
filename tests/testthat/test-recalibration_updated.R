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

context("Recalibration")


prediction <- data.frame(
  rowId = 1:100,
  value = c(runif(20)/30,runif(80)/300),
  outcomeCount = c(runif(20)>0.5, runif(80)>0.9)*1,
  gender = sample(c(8507, 1111), 100, replace = T),
  ageYear = sample(1:100,100, replace = T ),
  survivalTime = rep(365,100),
  evaluationType = rep('Test', 100)
  )

metaData <- list(
  modelType = "binary", 
  cohortId = 1,
  outcomeId = 2,
  timepoint = 365
)

attr(prediction, "metaData") <- metaData

test_that("recalibrationInTheLarge", {
  
  test <- recalibratePlp(prediction, analysisId = 'Analysis_1',
                 method = 'recalibrationInTheLarge')
  
  testthat::expect_true(sum(test$evaluationType == 'recalibrationInTheLarge') == 100)
  
})


#'weakRecalibration'
test_that("weakRecalibration", {

test <- recalibratePlp(prediction, analysisId = 'Analysis_1',
                       method = 'weakRecalibration')

testthat::expect_true(sum(test$evaluationType == 'weakRecalibration') == 100)

})




test_that("recalibratePlpRefit", {
  testRecal <- recalibratePlpRefit(
    plpModel = plpResult$model, 
    newPopulation = plpResult$prediction %>% dplyr::select(-.data$value) %>% dplyr::filter(.data$evaluationType %in% c('Test','Train')), 
    newData = plpData
  )
  
  testthat::expect_true(
    sum(testRecal$evaluationType == 'recalibrationRefit')>0
  )
  
  testthat::expect_s3_class(testRecal, 'data.frame')
  
  # add more test...
})



test_that("survival", {
# survival
metaData <- list(
  modelType = "survival", 
  cohortId = 1,
  outcomeId = 2,
  timepoint = 365
)

attr(prediction, "metaData") <- metaData

  test <- recalibratePlp(prediction, analysisId = 'Analysis_1',
    method = 'weakRecalibration')

  testthat::expect_true(sum(test$evaluationType == 'weakRecalibration') == 100)
  
})




