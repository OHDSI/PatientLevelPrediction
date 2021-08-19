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

prediction <- data.frame(rowId = 1:100,
                        value = c(runif(20)/30,runif(80)/300),
                        outcomeCount = c(runif(20)>0.5, runif(80)>0.9)*1,
                        gender = sample(c(8507, 1111), 100, replace = T),
                        ageYear = sample(1:100,100, replace = T ),
                        survivalTime = rep(365,100))


metaData <- list(predictionType = "binary", 
                 cohortId = 1,
                 outcomeId = 2,
                 timepoint = 365)

attr(prediction, "metaData") <- metaData

test_that("recalibrationInTheLarge", {
  
  test <- recalibratePlp(prediction, analysisId = 'Analysis_1',
                 method = 'recalibrationInTheLarge')
  
  testthat::expect_equal(names(test), c("prediction",
                                        "performanceEvaluation"))
  
  testthat::expect_s3_class(test$prediction, 'data.frame')
  testthat::expect_equal(nrow(test$prediction), 100)
  testthat::expect_equal(ncol(prediction), ncol(test$prediction)-1)
  
  testthat::expect_s3_class(test$performanceEvaluation$demographicSummary, 'data.frame')
  testthat::expect_s3_class(test$performanceEvaluation$calibrationSummary, 'data.frame')
  testthat::expect_s3_class(test$performanceEvaluation$evaluationStatistics, 'data.frame')
  testthat::expect_s3_class(test$performanceEvaluation$thresholdSummary, 'data.frame')
})


#'weakRecalibration'
test_that("weakRecalibration", {

test <- recalibratePlp(prediction, analysisId = 'Analysis_1',
                       method = 'weakRecalibration')

testthat::expect_equal(names(test), c("prediction",
                                      "performanceEvaluation"))

testthat::expect_s3_class(test$prediction, 'data.frame')
testthat::expect_equal(nrow(test$prediction), 100)
testthat::expect_equal(ncol(prediction), ncol(test$prediction)-1)

testthat::expect_s3_class(test$performanceEvaluation$demographicSummary, 'data.frame')
testthat::expect_s3_class(test$performanceEvaluation$calibrationSummary, 'data.frame')
testthat::expect_s3_class(test$performanceEvaluation$evaluationStatistics, 'data.frame')
testthat::expect_s3_class(test$performanceEvaluation$thresholdSummary, 'data.frame')

})


test_that("addRecalibration", {
  
  test <- recalibratePlp(prediction, analysisId = 'Analysis_1',
                         method = 'weakRecalibration')
  
  extras <- addRecalibration(plpResult$performanceEvaluation, test$performanceEvaluation)
  
  testthat::expect_s3_class(extras$demographicSummary, 'data.frame')
  testthat::expect_s3_class(extras$calibrationSummary, 'data.frame')
  testthat::expect_equal(nrow(extras$evaluationStatistics),  nrow(plpResult$performanceEvaluation$evaluationStatistics) + nrow(test$performanceEvaluation$evaluationStatistics) )
  
})


test_that("recalibratePlpRefit", {
  testRecal <- recalibratePlpRefit(plpModel = plpResult$model,
                      newPopulation = population, 
                      newData = plpData, 
                      testFraction = 0.25)
  
  testthat::expect_equal(names(testRecal$performanceEvalution), 
                         c("evaluationStatistics","thresholdSummary","demographicSummary",    
                           "calibrationSummary","predictionDistribution"))
  testthat::expect_s3_class(testRecal$performanceEvalution$evaluationStatistics, 'data.frame')
  
  # add more test...
})

test_that("differentialRecalibration",{
  testDiffRecal <- differentialRecalibration(validationResult, plpResult)
  noRecal <- differentialRecalibration(plpResult, plpResult)
  expect_equal(noRecal$prediction$prediction$value, plpResult$prediction$value)
  expect_equal(length(testDiffRecal$prediction$prediction$value), length(validationResult$prediction$value))
  notBinary <- validationResult
  attr(notBinary$prediction, "metaData")$predictionType = "survival"
  expect_error(differentialRecalibration(notBinary, plpResult))
})



