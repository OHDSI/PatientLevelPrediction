# Copyright 2025 Observational Health Data Sciences and Informatics
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
test_that("getPredictionDistribution binary type", {
  ePrediction <- data.frame(
    value = runif(100),
    outcomeCount = round(runif(100)),
    evaluation = rep("Test", 100)
  )
  predSum <- getPredictionDistribution(
    prediction = ePrediction,
    predictionType = "binary",
    typeColumn = "evaluation"
  )

  expect_equal(nrow(predSum), 2)
  expect_equal(ncol(predSum), 12)



  predBinary <- getPredictionDistribution_binary(
    prediction = ePrediction,
    evaluation = rep("Test", 100),
    evalColumn = "evaluation"
  )

  expect_equal(predBinary, predSum)
})


test_that("getPredictionDistribution survival type", {
  ePrediction <- data.frame(
    value = runif(100),
    outcomeCount = round(runif(100)),
    evaluation = rep("Test", 100)
  )

  predSurvival <- getPredictionDistribution_survival(
    prediction = ePrediction,
    evaluation = rep("Test", 100),
    evalColumn = "evaluation"
  )

  expect_true(is.null(predSurvival))
})
