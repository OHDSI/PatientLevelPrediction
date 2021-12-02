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
context("Calibration")


test_that("getCalibration binary", {
  Eprediction <- data.frame(
    rowId = 1:100, 
    evaluation = rep('Test',100),
    value = runif(100), 
    outcomeCount = round(runif(100))
    )
  attr(Eprediction, "metaData")$predictionType <-  "binary"
  calib <- getCalibrationSummary(
    prediction = Eprediction,
    predictionType = 'binary',
    typeColumn = 'evaluation',
    numberOfStrata = 100,
    truncateFraction = 0.05
  )
  
  expect_that(nrow(calib ), equals(100))
  expect_that(ncol(calib ), equals(12))
  expect_true('evaluation' %in% colnames(calib))
  
  
  calibBinary <-getCalibrationSummary_binary(
    prediction = Eprediction,
    evalColumn = 'evaluation',
    numberOfStrata = 100,
    truncateFraction = 0.05
    )
  
  expect_equal(calib, calibBinary)
    
})




test_that("getCalibration survival", {
  Eprediction <- data.frame(
    rowId = 1:100, 
    evaluation = rep('Test',100),
    value = runif(100), 
    survivalTime = 50+sample(2*365,100),
    outcomeCount = round(runif(100))
  )
  
  calib <- getCalibrationSummary_survival(
    prediction = Eprediction,
    evalColumn = 'evaluation',
    numberOfStrata = 50,
    truncateFraction = 0.05,
    timepoint = 365
  )
  
  expect_true('evaluation' %in% colnames(calib))
  expect_that(nrow(calib ), equals(50))
  expect_that(ncol(calib ), equals(7))
  
})
