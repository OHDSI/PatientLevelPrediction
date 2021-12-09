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
context("PredictionDistribution")

test_that("getPredictionDistribution binary type", {
  Eprediction <- data.frame(
    value= runif(100), 
    outcomeCount =round(runif(100)),
    evaluation = rep('Test',100)
    )
  predSum <- getPredictionDistribution(
    prediction = Eprediction,
    predictionType = 'binary',
    typeColumn = 'evaluation'
    )
  
  expect_that(nrow(predSum ), equals(2))
  expect_that(ncol(predSum ), equals(12))
  
  
  
  predBinary <- getPredictionDistribution_binary(
    prediction = Eprediction, 
    evaluation = rep('Test',100), 
    evalColumn = 'evaluation'
    )
  
  expect_equal(predBinary, predSum)
  
})


test_that("getPredictionDistribution survival type", {
  Eprediction <- data.frame(
    value= runif(100), 
    outcomeCount =round(runif(100)),
    evaluation = rep('Test',100)
  )
  
  predSurvival <-  getPredictionDistribution_survival(
    prediction = Eprediction, 
    evaluation = rep('Test',100),
    evalColumn = 'evaluation'
  )
  
  expect_true(is.null(predSurvival))

  
})

