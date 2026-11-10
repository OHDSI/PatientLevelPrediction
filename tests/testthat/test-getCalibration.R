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
test_that("getCalibration binary", {
  pErediction <- data.frame(
    rowId = 1:100,
    evaluation = rep("Test", 100),
    value = runif(100),
    outcomeCount = round(runif(100))
  )
  attr(pErediction, "metaData")$modelType <- "binary"
  calib <- getCalibrationSummary(
    prediction = pErediction,
    predictionType = "binary",
    typeColumn = "evaluation",
    numberOfStrata = 100,
    truncateFraction = 0.05
  )

  expect_equal(nrow(calib), 100)
  expect_equal(ncol(calib), 12)
  expect_true("evaluation" %in% colnames(calib))


  calibBinary <- getCalibrationSummary_binary(
    prediction = pErediction,
    evalColumn = "evaluation",
    numberOfStrata = 100,
    truncateFraction = 0.05
  )

  expect_equal(calib, calibBinary)
})




test_that("getCalibration survival", {
  pErediction <- data.frame(
    rowId = 1:100,
    evaluation = rep("Test", 100),
    value = runif(100),
    survivalTime = 50 + sample(2 * 365, 100),
    outcomeCount = round(runif(100))
  )

  calib <- getCalibrationSummary_survival(
    prediction = pErediction,
    evalColumn = "evaluation",
    numberOfStrata = 50,
    truncateFraction = 0.05,
    timepoint = 365
  )

  expect_true("evaluation" %in% colnames(calib))
  expect_equal(nrow(calib), 50)
  expect_equal(ncol(calib), 7)
})

test_that("survival calibration is evaluated at the requested horizon", {
  prediction <- data.frame(
    rowId = 1:4,
    value = rep(0.25, 4),
    survivalTime = c(100, 250, 365, 365),
    outcomeCount = c(1, 1, 0, 0),
    evaluation = "Test"
  )
  calibrate <- function(horizon) {
    attr(prediction, "metaData") <- list(timepoint = horizon, modelType = "survival")
    suppressWarnings(getCalibrationSummary(
      prediction = prediction, predictionType = "survival",
      numberOfStrata = 10, truncateFraction = 0
    ))
  }
  at180 <- calibrate(180)
  at365 <- calibrate(365)

  # by hand: S(180) = 3/4, S(365) = 3/4 * 2/3 = 1/2
  expect_equal(unname(at180$observedIncidence), 1 / 4)
  expect_equal(unname(at365$observedIncidence), 1 / 2)
  # log-transformed Greenwood interval at 180: se(log S) = sqrt(1 / (4 * 3))
  seLogSurvival <- sqrt(1 / (4 * 3))
  expect_equal(unname(at180$observedIncidenceLB), 0)
  expect_equal(
    unname(at180$observedIncidenceUB),
    1 - 0.75 * exp(-stats::qnorm(0.975) * seLogSurvival),
    tolerance = 1e-6
  )

  # censoring administratively at the horizon gives the same answer
  censored <- prediction
  censored$outcomeCount[censored$survivalTime > 180] <- 0
  censored$survivalTime <- pmin(censored$survivalTime, 180)
  attr(censored, "metaData") <- list(timepoint = 180, modelType = "survival")
  expect_equal(
    unname(suppressWarnings(getCalibrationSummary(
      censored, "survival", numberOfStrata = 10, truncateFraction = 0
    ))$observedIncidence),
    1 / 4
  )
})

test_that("survival calibration reports NA when follow-up ends before the horizon", {
  prediction <- data.frame(
    rowId = 1:12,
    value = rep(c(0.1, 0.2, 0.3), each = 4),
    # value 0.1: two events then censored after the horizon
    # value 0.2: censored before the horizon
    # value 0.3: all events before the horizon, so survival has reached zero
    survivalTime = c(100, 200, 500, 500, 50, 100, 150, 200, 50, 100, 150, 200),
    outcomeCount = c(1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1),
    evaluation = "Test"
  )
  result <- suppressWarnings(getCalibrationSummary_survival(
    prediction, "evaluation", numberOfStrata = 10, truncateFraction = 0, timepoint = 365
  ))
  expect_equal(unname(result$observedIncidence), c(1 / 2, NA, 1))
  expect_equal(unname(result$PersonCountAtRisk), c(4, 4, 4))
})

test_that("survival calibration without a timepoint uses each group's last follow-up", {
  prediction <- data.frame(
    rowId = 1:8,
    value = rep(c(0.1, 0.2), each = 4),
    survivalTime = c(100, 200, 500, 500, 50, 100, 150, 200),
    outcomeCount = c(1, 1, 0, 0, 0, 0, 0, 0),
    evaluation = "Test"
  )
  result <- suppressWarnings(getCalibrationSummary_survival(
    prediction, "evaluation", numberOfStrata = 10, truncateFraction = 0, timepoint = NULL
  ))
  expect_equal(unname(result$observedIncidence), c(1 / 2, 0))
})
