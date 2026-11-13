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

test_that("getMaxEndDaysFromCovariates works", {
  covariateSettings <- FeatureExtraction::createCovariateSettings(
    useDemographicsGender = TRUE,
    endDays = -1
  )
  expect_equal(getMaxEndDaysFromCovariates(covariateSettings), -1)

  covariateSettings <- list(
    FeatureExtraction::createCovariateSettings(
      useDemographicsGender = TRUE,
      endDays = -1
    ),
    FeatureExtraction::createCovariateSettings(
      useDemographicsGender = TRUE,
      endDays = 2
    )
  )
  expect_equal(getMaxEndDaysFromCovariates(covariateSettings), 2)

  covariateSettings <- list(
    FeatureExtraction::createCovariateSettings(
      useDemographicsGender = TRUE,
      endDays = -1,
    ),
    PatientLevelPrediction::createCohortCovariateSettings(
      endDay = 5,
      settingId = 1,
      cohortName = "test",
      cohortId = 1,
      analysisId = 111,
      cohortDatabaseSchema = "", cohortTable = ""
    )
  )
  expect_equal(getMaxEndDaysFromCovariates(covariateSettings), 5)

  # no window field anywhere means the window cannot be established
  expect_true(is.na(getMaxEndDaysFromCovariates(list(empty = list(gfg = 2), empty2 = list(ff = 1)))))
})

test_that("test diagnosePlp works", {
  skip_if_no_eunomia()
  test <- diagnosePlp(
    plpData = tinyPlpData,
    outcomeId = outcomeId,
    analysisId = "diagnoseTest",
    populationSettings = createStudyPopulationSettings(
      riskWindowStart = 1,
      startAnchor = "cohort start",
      riskWindowEnd = 365,
      endAnchor = "cohort start"
    ),
    splitSettings = createDefaultSplitSetting(),
    sampleSettings = createSampleSettings(), # default none
    saveDirectory = file.path(saveLoc, "diagnostics"),
    featureEngineeringSettings = createFeatureEngineeringSettings(), # default none
    modelSettings = setLassoLogisticRegression(), # default to logistic regression
    preprocessSettings = createPreprocessSettings()
  )
  # check results are a list
  expect_s3_class(test, "diagnosePlp")

  # check list names
  expect_equal(
    sum(names(test) %in%
      c(
        "summary", "participants", "predictors",
        "outcomes", "designs", "modelDesign",
        "databaseSchema"
      )),
    7
  )

  # check the results are saved into the databaseName directory
  expect_equal(TRUE, dir.exists(file.path(saveLoc, "diagnostics")))
  expect_equal(TRUE, file.exists(file.path(saveLoc, "diagnostics", "diagnoseTest", "diagnosePlp.rds")))

  expect_s3_class(test$summary, "data.frame")
  expect_s3_class(test$participants, "data.frame")
  expect_s3_class(test$predictors, "data.frame") # rename this outcome survival?
  expect_s3_class(test$outcomes, "data.frame")
  expect_type(test$databaseSchema, "character")

  expect_true(!is.null(test$modelDesign$targetId))
  expect_true(!is.null(test$modelDesign$outcomeId))
  expect_true(!is.null(test$modelDesign$restrictPlpDataSettings))
  expect_true(!is.null(test$modelDesign$covariateSettings))
  expect_true(!is.null(test$modelDesign$populationSettings))
})


test_that("test diagnoseMultiplePlp works", {
  skip_if_no_eunomia()
  analysis1 <- createModelDesign(
    targetId = 1,
    outcomeId = outcomeId,
    restrictPlpDataSettings = createRestrictPlpDataSettings(
      firstExposureOnly = FALSE,
      washoutPeriod = 0,
      sampleSize = 100
    ),
    populationSettings = createStudyPopulationSettings(),
    covariateSettings = covariateSettings,
    featureEngineeringSettings = NULL,
    sampleSettings = NULL,
    splitSettings = createDefaultSplitSetting(),
    preprocessSettings = createPreprocessSettings(),
    modelSettings = setLassoLogisticRegression(seed = 12)
  )

  analysis2 <- createModelDesign(
    targetId = 1,
    outcomeId = outcomeId,
    restrictPlpDataSettings = createRestrictPlpDataSettings(
      firstExposureOnly = FALSE,
      washoutPeriod = 0,
      sampleSize = 100
    ),
    populationSettings = createStudyPopulationSettings(washoutPeriod = 400),
    covariateSettings = covariateSettings,
    featureEngineeringSettings = NULL,
    sampleSettings = NULL,
    splitSettings = createDefaultSplitSetting(),
    preprocessSettings = createPreprocessSettings(),
    modelSettings = setLassoLogisticRegression(seed = 12)
  )

  diagnoseMultiplePlp(
    databaseDetails = databaseDetails,
    modelDesignList = list(
      analysis1,
      analysis2
    ),
    cohortDefinitions = data.frame(
      cohortId = c(1, outcomeId),
      cohortName = c("target", "outcome")
    ),
    saveDirectory = file.path(saveLoc, "diagnosticsMultiple")
  )

  # file.path(saveDirectory,'settings.csv') exits
  expect_true(file.exists(file.path(saveLoc, "diagnosticsMultiple", "settings.csv")))

  # file.path(saveDirectory, settings$analysisId, 'diagnosePlp.rds') exists
  expect_true(length(dir(file.path(saveLoc, "diagnosticsMultiple"), pattern = "Analysis_")) == 2)

  expect_true(file.exists(file.path(saveLoc, "diagnosticsMultiple", "Analysis_1", "diagnosePlp.rds")))
})

# Minimal plpData for the probast functions: they only read cohorts, outcomes and metaData.
makeDiagnosticPlpData <- function(cohorts, outcomes, covariateSettings = NULL) {
  plpData <- list(
    cohorts = cohorts,
    outcomes = outcomes,
    metaData = list(covariateSettings = covariateSettings)
  )
  class(plpData) <- "plpData"
  plpData
}

makeDiagnosticPopulationSettings <- function(riskWindowStart = 1, startAnchor = "cohort start",
                                             riskWindowEnd = 365) {
  createStudyPopulationSettings(
    requireTimeAtRisk = FALSE,
    riskWindowStart = riskWindowStart,
    startAnchor = startAnchor,
    riskWindowEnd = riskWindowEnd,
    endAnchor = "cohort start"
  )
}

leakageChecks <- function(covariateSettings, populationSettings = makeDiagnosticPopulationSettings()) {
  n <- 40
  plpData <- makeDiagnosticPlpData(
    cohorts = data.frame(
      rowId = 1:n, subjectId = 1:n, targetId = 1, cohortStartDate = as.Date("2020-01-01"),
      daysFromObsStart = 400, daysToCohortEnd = 0, daysToObsEnd = 400, ageYear = 60, gender = 8507
    ),
    outcomes = data.frame(rowId = 1:10, outcomeId = 1, outcomeCount = 1, daysToEvent = 10 * (1:10)),
    covariateSettings = covariateSettings
  )
  predictors <- suppressWarnings(probastPredictors(plpData, 1, populationSettings))$diagnosticPredictorsAggregate
  outcome <- suppressWarnings(probastOutcome(plpData, 1, populationSettings))$diagnosticOutcomeAggregate
  c(
    "2.2" = predictors$resultValue[predictors$probastId == "2.2"],
    "2.3" = predictors$resultValue[predictors$probastId == "2.3"],
    "3.6" = outcome$resultValue[outcome$probastId == "3.6"]
  )
}

detailedSettings <- function(endDay, sqlFileName = "DomainConcept.sql") {
  parameters <- list(analysisId = 1, startDay = 1, domainId = "Condition")
  if (!is.null(endDay)) {
    parameters$endDay <- endDay
  }
  FeatureExtraction::createDetailedCovariateSettings(
    analyses = list(FeatureExtraction::createAnalysisDetails(
      analysisId = 1,
      sqlFileName = sqlFileName,
      parameters = parameters,
      includedCovariateConceptIds = c(),
      addDescendantsToInclude = FALSE,
      excludedCovariateConceptIds = c(),
      addDescendantsToExclude = FALSE,
      includedCovariateIds = c()
    ))
  )
}

temporalSettings <- function(endDays = c(-1, 30)) {
  FeatureExtraction::createTemporalCovariateSettings(
    useConditionOccurrence = TRUE,
    temporalStartDays = endDays - 30,
    temporalEndDays = endDays
  )
}

test_that("getMaxEndDaysFromCovariates reads temporal, detailed and cohort-based windows", {
  expect_equal(getMaxEndDaysFromCovariates(temporalSettings()), 30)
  expect_equal(getMaxEndDaysFromCovariates(temporalSettings(c(-30, -1))), -1)
  expect_equal(getMaxEndDaysFromCovariates(detailedSettings(endDay = 30)), 30)
  expect_equal(
    getMaxEndDaysFromCovariates(list(
      FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE, endDays = -1),
      temporalSettings(c(-30, 7))
    )),
    7
  )
  expect_equal(
    getMaxEndDaysFromCovariates(FeatureExtraction::createCohortBasedCovariateSettings(
      analysisId = 123, covariateCohortDatabaseSchema = "s", covariateCohortTable = "t",
      covariateCohorts = data.frame(cohortId = 1, cohortName = "c"), startDay = -5, endDay = 5,
      warnOnAnalysisIdOverlap = FALSE
    )),
    5
  )
  expect_equal(
    getMaxEndDaysFromCovariates(FeatureExtraction::createTemporalSequenceCovariateSettings(
      useConditionOccurrence = TRUE, timePart = "DAY", timeInterval = 1,
      sequenceEndDay = 4, sequenceStartDay = -30
    )),
    4
  )
})

test_that("getMaxEndDaysFromCovariates returns NA when no window can be established", {
  expect_true(is.na(getMaxEndDaysFromCovariates(list(empty = list(gfg = 2), empty2 = list(ff = 1)))))
  expect_true(is.na(getMaxEndDaysFromCovariates(
    FeatureExtraction::createCohortAttrCovariateSettings("schema", "table", "defTable", "attrTable")
  )))
  expect_true(is.na(getMaxEndDaysFromCovariates(detailedSettings(endDay = NULL))))
  expect_true(is.na(getMaxEndDaysFromCovariates(list(
    FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE, endDays = -1),
    FeatureExtraction::createCohortAttrCovariateSettings("schema", "table", "defTable", "attrTable")
  ))))
  # demographics have no window, the condition analysis decides
  mixed <- detailedSettings(endDay = -1)
  mixed$analyses <- c(mixed$analyses, detailedSettings(endDay = NULL, sqlFileName = "DemographicsGender.sql")$analyses)
  expect_equal(getMaxEndDaysFromCovariates(mixed), -1)
})

test_that("leakage diagnostics fail when temporal or detailed windows overlap the risk window", {
  temporal <- leakageChecks(temporalSettings())
  expect_equal(unname(temporal[c("2.2", "2.3", "3.6")]), c("Fail", "Fail", "Fail"))

  detailed <- leakageChecks(detailedSettings(endDay = 30))
  expect_equal(unname(detailed[c("2.2", "2.3", "3.6")]), c("Fail", "Fail", "Fail"))

  # risk window after the last covariate window passes 2.2 and 3.6; 2.3 still fails on day-30 covariates
  later <- leakageChecks(temporalSettings(), makeDiagnosticPopulationSettings(riskWindowStart = 31))
  expect_equal(unname(later[c("2.2", "2.3", "3.6")]), c("Pass", "Fail", "Pass"))
})

test_that("leakage diagnostics report Unknown when no window can be established", {
  unknown <- leakageChecks(
    FeatureExtraction::createCohortAttrCovariateSettings("schema", "table", "defTable", "attrTable")
  )
  expect_equal(unname(unknown), c("Unknown", "Unknown", "Unknown"))
})

test_that("leakage diagnostics are unchanged for top-level endDays settings", {
  negative <- leakageChecks(FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE, endDays = -1))
  expect_equal(unname(negative[c("2.2", "2.3", "3.6")]), c("Pass", "Pass", "Pass"))

  zero <- leakageChecks(FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE, endDays = 0))
  expect_equal(unname(zero[c("2.2", "2.3", "3.6")]), c("Pass", "Unknown", "Pass"))

  cohortEnd <- leakageChecks(
    FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE, endDays = -1),
    makeDiagnosticPopulationSettings(startAnchor = "cohort end")
  )
  expect_equal(unname(cohortEnd[c("2.2", "3.6")]), c("Unknown", "Unknown"))
})

test_that("leakage diagnostics fail on known overlapping windows even when other windows are unknown", {
  cohortAttr <- FeatureExtraction::createCohortAttrCovariateSettings("schema", "table", "defTable", "attrTable")

  overlapping <- leakageChecks(list(temporalSettings(), cohortAttr))
  expect_equal(unname(overlapping), c("Fail", "Fail", "Fail"))

  # known windows pass, so the unknown one decides
  before <- leakageChecks(list(
    FeatureExtraction::createCovariateSettings(useConditionOccurrenceLongTerm = TRUE, endDays = -1),
    cohortAttr
  ))
  expect_equal(unname(before), c("Unknown", "Unknown", "Unknown"))

  window <- PatientLevelPrediction:::getCovariateWindowEnd(list(temporalSettings(), cohortAttr))
  expect_equal(window$maxEndDay, 30)
  expect_false(window$complete)
})

test_that("leakage diagnostics pass detailed settings holding only demographics", {
  demographics <- FeatureExtraction::convertPrespecSettingsToDetailedSettings(
    FeatureExtraction::createCovariateSettings(
      useDemographicsGender = TRUE, useDemographicsAge = TRUE,
      useDemographicsIndexYear = TRUE, useDemographicsPriorObservationTime = TRUE
    )
  )
  expect_equal(getMaxEndDaysFromCovariates(demographics), -Inf)
  expect_equal(unname(leakageChecks(demographics)), c("Pass", "Pass", "Pass"))

  # no covariate settings at all is not the same as no windows
  expect_true(is.na(getMaxEndDaysFromCovariates(NULL)))
  expect_equal(unname(leakageChecks(NULL)), c("Unknown", "Unknown", "Unknown"))
})

test_that("leakage diagnostics fail covariates that use time after index", {
  for (flag in c("useDemographicsPostObservationTime", "useDemographicsTimeInCohort")) {
    args <- list(useConditionOccurrenceLongTerm = TRUE, endDays = -1)
    args[[flag]] <- TRUE
    default <- do.call(FeatureExtraction::createCovariateSettings, args)
    expect_equal(getMaxEndDaysFromCovariates(default), Inf)
    expect_equal(unname(leakageChecks(default)), c("Fail", "Fail", "Fail"))

    detailed <- FeatureExtraction::convertPrespecSettingsToDetailedSettings(default)
    expect_equal(getMaxEndDaysFromCovariates(detailed), Inf)
    expect_equal(unname(leakageChecks(detailed)), c("Fail", "Fail", "Fail"))

    temporalArgs <- list(useConditionOccurrence = TRUE, temporalStartDays = -30, temporalEndDays = -1)
    temporalArgs[[flag]] <- TRUE
    temporal <- do.call(FeatureExtraction::createTemporalCovariateSettings, temporalArgs)
    expect_equal(getMaxEndDaysFromCovariates(temporal), Inf)
  }

  # prior observation time ends at index
  prior <- FeatureExtraction::createCovariateSettings(
    useConditionOccurrenceLongTerm = TRUE, useDemographicsPriorObservationTime = TRUE, endDays = -1
  )
  expect_equal(unname(leakageChecks(prior)), c("Pass", "Pass", "Pass"))
  expect_equal(
    getMaxEndDaysFromCovariates(FeatureExtraction::convertPrespecSettingsToDetailedSettings(prior)),
    -1
  )
})

test_that("probastPredictors counts risk sets on the index time axis", {
  cohorts <- data.frame(
    rowId = 1:3, subjectId = 1:3, targetId = 1, cohortStartDate = as.Date("2020-01-01"),
    daysFromObsStart = 400, daysToCohortEnd = 0, daysToObsEnd = c(365, 40, 365),
    ageYear = 60, gender = 8507
  )
  outcomes <- data.frame(rowId = 1, outcomeId = 1, outcomeCount = 1, daysToEvent = 30)
  plpData <- makeDiagnosticPlpData(cohorts, outcomes, FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE))

  delayed <- suppressWarnings(probastPredictors(plpData, 1, makeDiagnosticPopulationSettings(riskWindowStart = 30)))
  full <- as.data.frame(delayed$diagnosticPredictorsFull)
  day30 <- full[!is.na(full$daysToEvent) & full$daysToEvent == 30, ]
  expect_equal(unname(day30$observedAtStartOfDay[day30$inputType == "populationSettings"]), 3)
  expect_equal(unname(day30$observedAtStartOfDay[day30$inputType == "10-year"]), 3)
  # the non-event row has no time and no risk set
  noEvent <- full[is.na(full$daysToEvent), ]
  expect_equal(unique(noEvent$observedAtStartOfDay), 0)
})

test_that("probastPredictors risk sets follow delayed entry from a cohort-end anchor", {
  cohorts <- data.frame(
    rowId = 1:3, subjectId = 1:3, targetId = 1, cohortStartDate = as.Date("2020-01-01"),
    daysFromObsStart = 400, daysToCohortEnd = c(10, 0, 100), daysToObsEnd = c(365, 100, 150),
    ageYear = 60, gender = 8507
  )
  # person 1 enters on day 40 (event day), person 2 on day 30, person 3 on day 130
  outcomes <- data.frame(rowId = 1, outcomeId = 1, outcomeCount = 1, daysToEvent = 40)
  plpData <- makeDiagnosticPlpData(cohorts, outcomes, FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE))

  result <- suppressWarnings(probastPredictors(
    plpData, 1,
    makeDiagnosticPopulationSettings(riskWindowStart = 30, startAnchor = "cohort end")
  ))
  full <- as.data.frame(result$diagnosticPredictorsFull)
  day40 <- full[!is.na(full$daysToEvent) & full$daysToEvent == 40 & full$inputType == "populationSettings", ]
  expect_equal(unname(day40$observedAtStartOfDay), 2)
})

test_that("probastPredictors risk sets are unchanged for the default risk window", {
  n <- 20
  cohorts <- data.frame(
    rowId = 1:n, subjectId = 1:n, targetId = 1, cohortStartDate = as.Date("2020-01-01"),
    daysFromObsStart = 400, daysToCohortEnd = 0, daysToObsEnd = seq(40, 400, length.out = n),
    ageYear = 60, gender = 8507
  )
  outcomes <- data.frame(rowId = 1:5, outcomeId = 1, outcomeCount = 1, daysToEvent = c(1, 10, 20, 30, 35))
  plpData <- makeDiagnosticPlpData(cohorts, outcomes, FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE))

  settings <- makeDiagnosticPopulationSettings(riskWindowStart = 1)
  result <- suppressWarnings(probastPredictors(plpData, 1, settings))
  full <- as.data.frame(result$diagnosticPredictorsFull)
  full <- full[!is.na(full$daysToEvent) & full$inputType == "populationSettings", ]
  population <- suppressWarnings(createStudyPopulation(plpData, 1, settings))
  expected <- vapply(full$daysToEvent, function(x) sum(population$survivalTime >= x), integer(1))
  expect_equal(unname(full$observedAtStartOfDay), expected)
})
