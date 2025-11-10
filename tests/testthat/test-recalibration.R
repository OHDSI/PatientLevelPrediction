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
if (!exists("outcomeId")) {
  outcomeId <- NA_integer_
}
prediction <- data.frame(
  rowId = 1:100,
  value = c(runif(20) / 30, runif(80) / 300),
  outcomeCount = c(runif(20) > 0.5, runif(80) > 0.9) * 1,
  gender = sample(c(8507, 1111), 100, replace = TRUE),
  ageYear = sample(1:100, 100, replace = TRUE),
  survivalTime = rep(365, 100),
  evaluationType = rep("Test", 100)
)

metaData <- list(
  modelType = "binary",
  targetId = 1,
  outcomeId = outcomeId,
  timepoint = 365
)

attr(prediction, "metaData") <- metaData

test_that("recalibrationInTheLarge", {
  test <- recalibratePlp(prediction,
    analysisId = "Analysis_1",
    method = "recalibrationInTheLarge"
  )

  expect_true(sum(test$evaluationType == "recalibrationInTheLarge") == 100)
})


#' weakRecalibration'
test_that("weakRecalibration", {
  test <- recalibratePlp(prediction,
    analysisId = "Analysis_1",
    method = "weakRecalibration"
  )

  expect_true(sum(test$evaluationType == "weakRecalibration") == 100)
})


test_that("recalibratePlpRefit", {
  skip_if_not_installed("Eunomia")
  skip_if_offline()
  newPop <- plpResult$prediction %>%
    dplyr::select(-"value") %>%
    dplyr::filter(.data$evaluationType %in% c("Test", "Train"))
  attr(newPop, "metaData") <- list(
    targetId = 1,
    outcomeId = outcomeId,
    restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(),
    populationSettings = PatientLevelPrediction::createStudyPopulationSettings()
  )

  testRecal <- recalibratePlpRefit(
    plpModel = plpResult$model,
    newPopulation = newPop,
    newData = plpData
  )

  if (!is.null(testRecal)) {
    expect_true(
      sum(testRecal$evaluationType == "recalibrationRefit") > 0
    )
    expect_s3_class(testRecal, "data.frame")
  }

  testRecalWithModel <- recalibratePlpRefit(
    plpModel = plpResult$model,
    newPopulation = newPop,
    newData = plpData,
    returnModel = TRUE
  )
  expect_type(testRecalWithModel, "list")
  expect_s3_class(testRecalWithModel$model, "plpModel")
  expect_s3_class(testRecalWithModel$prediction, "data.frame")

  # add more test...
})



test_that("recalibratePlpRefit augments missing columns and raw values", {
  set.seed(123)

  plpModel <- structure(
    list(
      covariateImportance = data.frame(
        covariateId = c(1, 2),
        covariateValue = c(0.5, 0)
      ),
      model = list(coefficients = c("(Intercept)" = 0.2))
    ),
    class = "plpModel"
  )

  newPopulation <- data.frame(
    rowId = 1:2,
    outcomeCount = c(0, 1)
  )
  attr(newPopulation, "metaData") <- list(
    outcomeId = 100,
    targetId = 200,
    restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(),
    populationSettings = PatientLevelPrediction::createStudyPopulationSettings()
  )

  newData <- structure(
    list(
      covariateData = list(
        covariateRef = dplyr::tibble(covariateId = c(1, 2))
      ),
      metaData = list(covariateSettings = list())
    ),
    class = "plpData"
  )
  attr(newData, "metaData") <- list()
  attr(newData$covariateData, "metaData") <- list()

  mockFit <- structure(
    list(
      prediction = data.frame(
        rowId = 1:2,
        value = c(0.25, 0.75),
        newOnly = c(1, 2)
      ),
      covariateImportance = data.frame(
        covariateId = 1,
        covariateValue = 0.25
      ),
      model = list(coefficients = c("(Intercept)" = -0.1))
    ),
    class = "plpModel"
  )
  attr(mockFit$prediction, "metaData") <- list(modelType = "binary")

  mockPredict <- data.frame(
    rowId = 1:2,
    value = c(0.4, 0.6),
    oldOnly = c(3, 4)
  )
  attr(mockPredict, "metaData") <- list(modelType = "binary")

  result <- with_mocked_bindings(
    recalibratePlpRefit(
      plpModel = plpModel,
      newPopulation = newPopulation,
      newData = newData
    ),
    fitPlp = function(...) mockFit,
    predictPlp = function(...) mockPredict,
    setLassoLogisticRegression = function(...) list()
  )

  expect_s3_class(result, "data.frame")
  expect_true("rawValue" %in% names(result))
  expect_setequal(unique(result$evaluationType), c("validation", "recalibrationRefit"))
  expect_equal(
    sum(is.na(result$newOnly[result$evaluationType == "validation"])),
    sum(result$evaluationType == "validation")
  )
  expect_equal(
    sum(is.na(result$oldOnly[result$evaluationType == "recalibrationRefit"])),
    sum(result$evaluationType == "recalibrationRefit")
  )
  expect_false(any(is.na(result$rawValue)))
})


test_that("recalibrationInTheLarge adjusts raw values when available", {
  predictionWithRaw <- data.frame(
    rowId = 1:3,
    value = c(0.2, 0.3, 0.4),
    rawValue = stats::qlogis(c(0.2, 0.3, 0.4)),
    outcomeCount = c(0, 1, 0),
    evaluationType = rep("validation", 3)
  )
  attr(predictionWithRaw, "metaData") <- list(modelType = "binary")

  observedRisk <- 0.35
  meanRisk <- 0.25
  correction <- stats::qlogis(observedRisk) - stats::qlogis(meanRisk)

  recalibrated <- with_mocked_bindings(
    PatientLevelPrediction:::recalibrationInTheLarge(predictionWithRaw),
    calibrationInLarge = function(prediction) {
      list(
        observedRisk = observedRisk,
        meanPredictionRisk = meanRisk
      )
    }
  )

  newRows <- recalibrated[recalibrated$evaluationType == "recalibrationInTheLarge", ]
  expect_equal(
    newRows$rawValue,
    predictionWithRaw$rawValue + correction
  )
  expect_equal(
    newRows$value,
    stats::plogis(newRows$rawValue)
  )
})


test_that("weakRecalibration survival uses baseline when missing raw values", {
  skip_if_not_installed("survival")

  set.seed(42)
  n <- 40
  survPrediction <- data.frame(
    rowId = seq_len(n),
    value = runif(n, min = 0.02, max = 0.45),
    outcomeCount = rbinom(n, 1, 0.4),
    daysToCohortEnd = sample(250:450, n, replace = TRUE),
    survivalTime = sample(60:400, n, replace = TRUE),
    evaluationType = rep("validation", n)
  )
  attr(survPrediction, "metaData") <- list(modelType = "survival")
  attr(survPrediction, "baselineSurvival") <- 0.8
  attr(survPrediction, "offset") <- 0.15
  attr(survPrediction, "timePoint") <- 220

  recalibrated <- suppressWarnings(PatientLevelPrediction:::weakRecalibration(
    prediction = survPrediction,
    columnType = "evaluationType"
  ))

  newRows <- recalibrated[recalibrated$evaluationType == "weakRecalibration", ]
  expect_equal(nrow(newRows), nrow(survPrediction))
  expect_true(all(newRows$value >= 0 & newRows$value <= 1))
  expect_gt(
    sum(abs(newRows$value - survPrediction$value)),
    0
  )
})


test_that("weakRecalibration survival propagates raw values", {
  skip_if_not_installed("survival")

  set.seed(99)
  n <- 40
  rawVals <- rnorm(n, mean = -0.2, sd = 0.6)
  survPrediction <- data.frame(
    rowId = seq_len(n),
    rawValue = rawVals,
    value = stats::plogis(rawVals),
    outcomeCount = rbinom(n, 1, 0.35),
    daysToCohortEnd = sample(300:500, n, replace = TRUE),
    survivalTime = sample(80:420, n, replace = TRUE),
    evaluationType = rep("validation", n)
  )
  attr(survPrediction, "metaData") <- list(modelType = "survival")
  attr(survPrediction, "baselineSurvival") <- 0.85
  attr(survPrediction, "offset") <- -0.05
  attr(survPrediction, "timePoint") <- 240

  recalibrated <- suppressWarnings(PatientLevelPrediction:::weakRecalibration(
    prediction = survPrediction,
    columnType = "evaluationType"
  ))

  newRows <- recalibrated[recalibrated$evaluationType == "weakRecalibration", ]
  expect_true("rawValue" %in% names(newRows))
  expect_equal(nrow(newRows), nrow(survPrediction))
  expect_gt(
    sum(abs(newRows$rawValue - survPrediction$rawValue)),
    0
  )
  expect_true(all(newRows$value >= 0 & newRows$value <= 1))
})


test_that("survival", {
  # survival
  metaData <- list(
    modelType = "survival",
    targetId = 1,
    outcomeId = outcomeId,
    timepoint = 365
  )

  attr(prediction, "metaData") <- metaData

  test <- recalibratePlp(prediction,
    analysisId = "Analysis_1",
    method = "weakRecalibration"
  )

  testthat::expect_true(sum(test$evaluationType == "weakRecalibration") == 100)
})
