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

  # if no covariate setting has endDays return 0
  expect_equal(
    getMaxEndDaysFromCovariates(list(empty = list(gfg = 2), empty2 = list(ff = 1))),
    0
  )
})

test_that("test diagnosePlp works", {
  skip_if_offline()
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
  skip_if_offline()
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
