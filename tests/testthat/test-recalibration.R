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
    setLassoLogisticRegression = function(...) list(),
    limitCovariatesToPopulation = function(covariateData, rowIds) covariateData
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

  correction <- unname(stats::coef(stats::glm(
    outcomeCount ~ 1,
    offset = rawValue,
    family = "binomial",
    data = predictionWithRaw
  )))

  recalibrated <- PatientLevelPrediction:::recalibrationInTheLarge(predictionWithRaw)

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


recalFixture <- function() {
  # five score groups of 20 people with 2, 4, 6, 8 and 10 outcomes
  fixture <- data.frame(
    rowId = 1:100,
    value = rep(c(.5, .6, .7, .8, .9), each = 20),
    outcomeCount = unlist(lapply(c(2, 4, 6, 8, 10), function(k) {
      c(rep(1, k), rep(0, 20 - k))
    })),
    evaluationType = "Validation"
  )
  attr(fixture, "metaData") <- list(modelType = "binary")
  fixture
}

recalRows <- function(result, method) {
  result[result$evaluationType == method, ]
}

test_that("recalibratePlp methods fit on the original predictions only", {
  methods <- c("recalibrationInTheLarge", "weakRecalibration")
  fixture <- recalFixture()
  single <- lapply(methods, function(m) recalibratePlp(fixture, method = m))
  names(single) <- methods

  for (order in list(methods, rev(methods))) {
    combined <- fixture
    for (m in order) {
      combined <- recalibratePlp(combined, method = m)
    }
    expect_equal(nrow(combined), 300)
    expect_equal(nrow(recalRows(combined, "Validation")), 100)
    for (m in methods) {
      expect_equal(nrow(recalRows(combined, m)), 100)
      expect_equal(anyDuplicated(recalRows(combined, m)$rowId), 0)
      expect_equal(recalRows(combined, m)$value, recalRows(single[[m]], m)$value)
    }
    expect_equal(
      attr(combined, "metaData")$weakRecalibration$adjustGradient,
      attr(single$weakRecalibration, "metaData")$weakRecalibration$adjustGradient
    )
    expect_equal(
      attr(combined, "metaData")$recalibrationInTheLarge$correctionFactor,
      attr(single$recalibrationInTheLarge, "metaData")$recalibrationInTheLarge$correctionFactor
    )
  }
})

test_that("rerunning a recalibration method replaces its rows", {
  once <- recalibratePlp(recalFixture(), method = "weakRecalibration")
  twice <- recalibratePlp(once, method = "weakRecalibration")
  expect_equal(nrow(twice), 200)
  expect_equal(recalRows(twice, "weakRecalibration")$value, recalRows(once, "weakRecalibration")$value)
})

test_that("recalibrating after recalibratePlpRefit ignores the refit rows", {
  fixture <- recalFixture()
  refitRows <- fixture
  refitRows$value <- 0.3
  refitRows$evaluationType <- "recalibrationRefit"
  withRefit <- rbind(fixture, refitRows)
  attr(withRefit, "metaData") <- attr(fixture, "metaData")

  result <- recalibratePlp(withRefit, method = "weakRecalibration")
  alone <- recalibratePlp(fixture, method = "weakRecalibration")
  expect_equal(nrow(recalRows(result, "weakRecalibration")), 100)
  expect_equal(recalRows(result, "weakRecalibration")$value, recalRows(alone, "weakRecalibration")$value)
  expect_equal(nrow(recalRows(result, "recalibrationRefit")), 100)
})

recalValidationModel <- function() {
  predictFixture <- function(plpModel, data, cohort) {
    ans <- cohort[c("rowId", "outcomeCount")]
    ans$ageYear <- 50
    ans$gender <- 8507
    ans$value <- cohort$score
    ans$rawValue <- stats::qlogis(ans$value)
    ans
  }
  structure(
    list(modelDesign = list(modelSettings = list(settings = list(predict = predictFixture)))),
    class = "plpModel",
    modelType = "binary"
  )
}

recalValidationPopulation <- function() {
  fixture <- recalFixture()
  population <- data.frame(
    rowId = fixture$rowId,
    score = fixture$value,
    outcomeCount = fixture$outcomeCount
  )
  attr(population, "metaData") <- list(
    populationSettings = list(riskWindowEnd = 365),
    outcomeId = 2,
    targetId = 1
  )
  population
}

recalValidationPlpData <- list(metaData = list(databaseDetails = list(
  cdmDatabaseSchema = "fixture", cdmDatabaseId = "fixture", targetId = 1
)))

test_that("externalValidatePlp evaluates both recalibration methods on the original patients", {
  methods <- c("recalibrationInTheLarge", "weakRecalibration")
  population <- recalValidationPopulation()
  runValidation <- function(recalibrate) {
    PatientLevelPrediction:::externalValidatePlp(
      plpModel = recalValidationModel(),
      plpData = recalValidationPlpData,
      population = population,
      settings = createValidationSettings(recalibrate = recalibrate, runCovariateSummary = FALSE)
    )
  }
  metric <- function(result, evaluation, name) {
    stats <- result$performanceEvaluation$evaluationStatistics
    as.numeric(stats$value[stats$evaluation == evaluation & stats$metric == name])
  }

  for (order in list(methods, rev(methods))) {
    combined <- runValidation(order)
    expect_equal(nrow(recalRows(combined$prediction, "Validation")), 100)
    for (m in methods) {
      alone <- runValidation(m)
      expect_equal(nrow(recalRows(combined$prediction, m)), 100)
      expect_equal(metric(combined, m, "populationSize"), 100)
      expect_equal(metric(combined, m, "outcomeCount"), 30)
      expect_equal(metric(combined, m, "AUROC"), metric(alone, m, "AUROC"))
    }
  }
})

test_that("a failing recalibration method does not lose the validation predictions", {
  result <- with_mocked_bindings(
    PatientLevelPrediction:::externalValidatePlp(
      plpModel = recalValidationModel(),
      plpData = recalValidationPlpData,
      population = recalValidationPopulation(),
      settings = createValidationSettings(
        recalibrate = c("recalibrationInTheLarge", "weakRecalibration"),
        runCovariateSummary = FALSE
      )
    ),
    recalibratePlp = function(prediction, method, ...) stop("recalibration failed")
  )
  expect_equal(nrow(result$prediction), 100)
  expect_equal(unique(result$prediction$evaluationType), "Validation")
})

test_that("recalibrationInTheLarge keeps survival predictions unchanged", {
  survivalPrediction <- recalFixture()
  attr(survivalPrediction, "metaData")$modelType <- "survival"
  result <- recalibratePlp(survivalPrediction, method = "recalibrationInTheLarge")
  expect_equal(nrow(result), 100)
  expect_equal(unique(result$evaluationType), "Validation")
})

test_that("recalibrationInTheLarge matches mean risk for heterogeneous predictions", {
  values <- c(0.1, 0.1, 0.9, 0.9)
  for (withRaw in c(FALSE, TRUE)) {
    heterogeneous <- data.frame(
      rowId = 1:4,
      value = values,
      outcomeCount = c(0, 0, 0, 1),
      evaluationType = "validation"
    )
    if (withRaw) {
      heterogeneous$rawValue <- stats::qlogis(values)
    }
    attr(heterogeneous, "metaData") <- list(modelType = "binary")

    result <- recalibratePlp(heterogeneous, method = "recalibrationInTheLarge")
    newRows <- recalRows(result, "recalibrationInTheLarge")
    expect_equal(mean(newRows$value), 0.25, tolerance = 1e-7)

    reference <- unname(stats::coef(stats::glm(
      c(0, 0, 0, 1) ~ 1,
      offset = stats::qlogis(values),
      family = "binomial"
    )))
    expect_equal(
      attr(result, "metaData")$recalibrationInTheLarge$correctionFactor,
      reference,
      tolerance = 1e-7
    )
    expect_null(names(attr(result, "metaData")$recalibrationInTheLarge$correctionFactor))
  }
})

test_that("recalibrationInTheLarge handles homogeneous risks and no outcomes", {
  homogeneous <- data.frame(
    rowId = 1:4,
    value = rep(0.5, 4),
    outcomeCount = c(0, 0, 0, 1),
    evaluationType = "validation"
  )
  attr(homogeneous, "metaData") <- list(modelType = "binary")
  result <- recalibratePlp(homogeneous, method = "recalibrationInTheLarge")
  expect_equal(recalRows(result, "recalibrationInTheLarge")$value, rep(0.25, 4), tolerance = 1e-7)

  noOutcomes <- homogeneous
  noOutcomes$outcomeCount <- 0
  noOutcomes$value <- c(0.1, 0.2, 0.3, 0.4)
  result <- recalibratePlp(noOutcomes, method = "recalibrationInTheLarge")
  newValues <- recalRows(result, "recalibrationInTheLarge")$value
  expect_true(all(is.finite(newValues)))
  expect_lt(max(newValues), 1e-6)
})

survivalHorizonFixture <- function(timepoint = 730) {
  # six pairs with opposite linear predictors: the Cox slope is zero by symmetry
  fixture <- data.frame(
    rowId = 1:12,
    rawValue = rep(c(-1, 1), 6),
    outcomeCount = rep(c(1, 1, 1, 1, 0, 0), each = 2),
    survivalTime = rep(c(100, 200, 400, 500, 900, 900), each = 2),
    daysToCohortEnd = rep(900, 12),
    evaluationType = "validation"
  )
  fixture$value <- 1 - exp(-0.5 * exp(fixture$rawValue))
  attr(fixture, "metaData") <- list(modelType = "survival", timepoint = timepoint)
  fixture
}

test_that("weakRecalibration survival uses the timepoint stored in metaData", {
  skip_if_not_installed("survival")
  expected365 <- 1 - exp(-sum(1 / (9:12)))
  expected730 <- 1 - exp(-sum(1 / (5:12)))

  for (setting in list(list(timepoint = 730, expected = expected730), list(timepoint = 365, expected = expected365))) {
    result <- recalibratePlp(survivalHorizonFixture(setting$timepoint), method = "weakRecalibration")
    expect_equal(
      recalRows(result, "weakRecalibration")$value,
      rep(setting$expected, 12),
      tolerance = 1e-6
    )
    expect_equal(attr(result, "metaData")$timepoint, setting$timepoint)
  }

  # metaData$timepoint is the canonical horizon, read by evaluation as well
  both <- survivalHorizonFixture(730)
  attr(both, "timePoint") <- 365
  result <- recalibratePlp(both, method = "weakRecalibration")
  expect_equal(recalRows(result, "weakRecalibration")$value, rep(expected730, 12), tolerance = 1e-6)
})

test_that("weakRecalibration survival rejects a missing or non-positive horizon", {
  skip_if_not_installed("survival")
  missingHorizon <- survivalHorizonFixture()
  attr(missingHorizon, "metaData")$timepoint <- NULL
  expect_error(
    recalibratePlp(missingHorizon, method = "weakRecalibration"),
    "positive timepoint"
  )
  expect_error(
    recalibratePlp(survivalHorizonFixture(timepoint = 0), method = "weakRecalibration"),
    "positive timepoint"
  )
  expect_error(
    recalibratePlp(survivalHorizonFixture(timepoint = NA_real_), method = "weakRecalibration"),
    "positive timepoint"
  )
})

test_that("weakRecalibration survival uses survivalTime, not cohort end", {
  skip_if_not_installed("survival")
  fixture <- data.frame(
    rowId = 1:12,
    rawValue = c(1.2, 0.2, -0.4, 0.9, -0.6, 0.5, 1, -1, 0.3, -0.8, 0.7, -0.1),
    survivalTime = c(200, 80, 150, 260, 300, 120, rep(365, 6)),
    outcomeCount = c(rep(1, 6), rep(0, 6)),
    daysToCohortEnd = c(30, 365, 60, 100, rep(365, 8)),
    evaluationType = "validation"
  )
  fixture$value <- 1 - 0.9^exp(fixture$rawValue)
  attr(fixture, "metaData") <- list(modelType = "survival", timepoint = 365)

  reference <- survival::coxph(survival::Surv(survivalTime, outcomeCount) ~ rawValue, data = fixture)
  hazard <- max(survival::basehaz(reference)$hazard)
  expectedRisk <- as.numeric(1 - exp(-hazard * exp(stats::predict(reference))))

  result <- recalibratePlp(fixture, method = "weakRecalibration")
  expect_equal(recalRows(result, "weakRecalibration")$value, expectedRisk, tolerance = 1e-6)
  expect_equal(
    unname(attr(result, "metaData")$weakRecalibration$adjustGradient),
    unname(stats::coef(reference)),
    tolerance = 1e-6
  )

  # censoring at the horizon still holds: outcomes after the timepoint are censored
  shortHorizon <- fixture
  attr(shortHorizon, "metaData")$timepoint <- 150
  shortResult <- recalibratePlp(shortHorizon, method = "weakRecalibration")
  censoredTime <- pmin(fixture$survivalTime, 150)
  censoredEvent <- fixture$outcomeCount == 1 & fixture$survivalTime <= 150
  shortReference <- survival::coxph(survival::Surv(censoredTime, censoredEvent) ~ fixture$rawValue)
  expect_equal(
    unname(attr(shortResult, "metaData")$weakRecalibration$adjustGradient),
    unname(stats::coef(shortReference)),
    tolerance = 1e-6
  )
})

refitFixture <- function(excludedValue = NULL) {
  # 60 people with x = 0 (15 cases) and 60 with x = 1 (45 cases)
  x <- rep(c(0, 1), each = 60)
  y <- c(rep(0, 45), rep(1, 15), rep(0, 15), rep(1, 45))
  population <- data.frame(
    rowId = 1:120, subjectId = 1:120,
    cohortStartDate = as.Date("2020-01-01"),
    outcomeCount = y, survivalTime = 365, timeAtRisk = 365
  )
  attr(population, "metaData") <- list(
    targetId = 1, outcomeId = 2,
    restrictPlpDataSettings = createRestrictPlpDataSettings(),
    populationSettings = createStudyPopulationSettings()
  )
  covariateData <- FeatureExtraction::createEmptyCovariateData(1, FALSE, FALSE)
  records <- data.frame(rowId = which(x != 0), covariateId = 1001, covariateValue = x[x != 0])
  if (!is.null(excludedValue)) {
    records <- rbind(records, data.frame(rowId = 121, covariateId = 1001, covariateValue = excludedValue))
  }
  covariateData$covariates <- records
  covariateData$covariateRef <- data.frame(
    covariateId = 1001, covariateName = "Selected predictor", analysisId = 1, conceptId = 100
  )
  covariateData$analysisRef <- data.frame(
    analysisId = 1, analysisName = "Test", domainId = "Measurement",
    startDay = -30, endDay = 0, isBinary = "N", missingMeansZero = "Y"
  )
  cohorts <- rbind(population, transform(population[1, ], rowId = 121, subjectId = 121))
  plpData <- list(covariateData = covariateData, cohorts = cohorts, metaData = list(covariateSettings = NULL))
  class(plpData) <- "plpData"
  list(
    population = population,
    plpData = plpData,
    model = createGlmModel(data.frame(covariateId = 1001, coefficient = 1), intercept = -1),
    y = y,
    x = x
  )
}

test_that("recalibratePlpRefit ignores covariates of patients outside newPopulation", {
  skip_if_not_installed("Cyclops")
  clean <- refitFixture()
  contaminated <- refitFixture(excludedValue = 10)
  withr::defer({
    Andromeda::close(clean$plpData$covariateData)
    Andromeda::close(contaminated$plpData$covariateData)
  })

  fitModel <- function(data) {
    withr::with_seed(42, recalibratePlpRefit(
      plpModel = data$model,
      newPopulation = data$population,
      newData = data$plpData,
      returnModel = TRUE
    ))
  }
  coefs <- function(fit) {
    z <- fit$model$model$coefficients
    stats::setNames(z$betas, z$covariateIds)[c("(Intercept)", "1001")]
  }

  cleanFit <- fitModel(clean)
  contaminatedFit <- fitModel(contaminated)
  expect_equal(unname(coefs(cleanFit)), c(log(1 / 3), log(9)), tolerance = 1e-3)
  expect_equal(unname(coefs(contaminatedFit)), unname(coefs(cleanFit)), tolerance = 1e-5)

  # the caller's covariate data keeps the excluded patient
  contaminatedRows <- contaminated$plpData$covariateData$covariates %>% dplyr::collect()
  expect_equal(sum(contaminatedRows$rowId == 121), 1)
})
