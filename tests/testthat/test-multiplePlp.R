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
analysis1 <- createModelDesign(
  targetId = 1,
  outcomeId = outcomeId,
  restrictPlpDataSettings =
  createRestrictPlpDataSettings(firstExposureOnly = FALSE, washoutPeriod = 0),
  populationSettings = createStudyPopulationSettings(),
  covariateSettings = covariateSettings,
  featureEngineeringSettings = NULL,
  sampleSettings = NULL,
  splitSettings = createDefaultSplitSetting(splitSeed = 1),
  preprocessSettings = createPreprocessSettings(),
  modelSettings = setLassoLogisticRegression(seed = 12)
)

test_that("createModelDesign - test working", {
  expect_equal(analysis1$targetId, 1)
  expect_equal(analysis1$outcomeId, outcomeId)
  expect_equal(analysis1$restrictPlpDataSettings,
    createRestrictPlpDataSettings(firstExposureOnly = FALSE, washoutPeriod = 0))
  expect_equal(analysis1$covariateSettings, covariateSettings)
  expect_equal(analysis1$featureEngineeringSettings, list(createFeatureEngineeringSettings(type = "none")))
  expect_equal(analysis1$sampleSettings, list(createSampleSettings(type = "none")))
  expect_equal(analysis1$preprocessSettings, createPreprocessSettings())
  expect_equal(analysis1$splitSettings, createDefaultSplitSetting(splitSeed = 1))
  expect_equal(analysis1$modelSettings, setLassoLogisticRegression(seed = 12))
  expect_equal(
    analysis1$executeSettings,
    createExecuteSettings(
      runSplitData = TRUE,
      runSampleData = FALSE,
      runFeatureEngineering = FALSE,
      runPreprocessData = TRUE,
      runModelDevelopment = TRUE,
      runCovariateSummary = TRUE
    )
  )
})

test_that("saving analyses settings", {
  fileLocation <- savePlpAnalysesJson(
    modelDesignList = list(analysis1),
    saveDirectory = file.path(saveLoc, "settings")
  )

  expect_true(file.exists(fileLocation))
})

test_that("loading analyses settings", {
  analysisSetting <- loadPlpAnalysesJson(file.path(saveLoc, "settings", "predictionAnalysisList.json"))

  expect_equal(analysis1$targetId, analysisSetting$analyses[[1]]$targetId)
  expect_equal(analysis1$outcomeId, analysisSetting$analyses[[1]]$outcomeId)
  expect_equal(analysis1$restrictPlpDataSettings, analysisSetting$analyses[[1]]$restrictPlpDataSettings)
  expect_equal(attr(analysis1$covariateSettings, "fun"), attr(analysisSetting$analyses[[1]]$covariateSettings, "fun"))
  expect_equal(analysis1$populationSettings, analysisSetting$analyses[[1]]$populationSettings)
  expect_equal(analysis1$sampleSettings, analysisSetting$analyses[[1]]$sampleSettings)
  expect_equal(attr(analysis1$featureEngineeringSettings, "class"), attr(analysisSetting$analyses[[1]]$featureEngineeringSettings, "class"))
  expect_equal(attr(analysis1$featureEngineeringSettings, "fun"), attr(analysisSetting$analyses[[1]]$featureEngineeringSettings, "fun"))
  expect_equal(analysis1$preprocessSettings, analysisSetting$analyses[[1]]$preprocessSettings)
  expect_equal(analysis1$modelSettings, analysisSetting$analyses[[1]]$modelSettings)
  expect_equal(analysis1$splitSettings, analysisSetting$analyses[[1]]$splitSettings)
  expect_equal(analysis1$executeSettings, analysisSetting$analyses[[1]]$executeSettings)
})

test_that("test run multiple", {
  skip_if_not_installed("ResultModelManager")
  skip_on_cran()
  skip_if_offline()

  analysis3 <- createModelDesign(
    targetId = 1,
    outcomeId = outcomeId,
    restrictPlpDataSettings = 
    createRestrictPlpDataSettings(firstExposureOnly = FALSE, washoutPeriod = 0),
    populationSettings = createStudyPopulationSettings(),
    covariateSettings = covariateSettings,
    featureEngineeringSettings = createFeatureEngineeringSettings(),
    sampleSettings = createSampleSettings(),
    preprocessSettings = createPreprocessSettings(),
    modelSettings = setLassoLogisticRegression(seed = 12),
    splitSettings = createDefaultSplitSetting(
      type = "stratified",
      testFraction = 0.25,
      trainFraction = 0.75,
      splitSeed = 123,
      nfold = 3
    ),
    runCovariateSummary = FALSE
  )

  runMultiplePlp(
    databaseDetails = databaseDetails,
    modelDesignList = list(
      # add this twice to make sure no issue with overlapping ids?
      analysis3
    ),
    onlyFetchData = FALSE,
    logSettings = createLogSettings(
      verbosity = "DEBUG",
      timeStamp = TRUE,
      logName = "runPlp Log"
    ),
    saveDirectory = file.path(saveLoc, "multiple")
  )

  expect_true(file.exists(file.path(saveLoc, "multiple", "settings.csv")))
  expect_true(dir.exists(file.path(saveLoc, "multiple", "Analysis_1")))
  expect_true(file.exists(file.path(saveLoc, "multiple", "Analysis_1", "plpResult", "runPlp.rds")))
})

test_that("validateMultiplePlp errors", {
  skip_if_not_installed("ResultModelManager")
  skip_on_cran()
  skip_if_offline()
  PatientLevelPrediction::validateMultiplePlp(
    analysesLocation = file.path(saveLoc, "multiple"),
    validationDatabaseDetails = databaseDetails,
    validationRestrictPlpDataSettings = createRestrictPlpDataSettings(),
    recalibrate = NULL
  )

  expect_true(dir.exists(file.path(saveLoc, "multiple", "Validation", "main")))
  expect_true(dir.exists(file.path(saveLoc, "multiple", "Validation", "main", "Analysis_1", "validationResult")))
  expect_true(file.exists(file.path(saveLoc, "multiple", "Validation", "main", "Analysis_1", "validationResult", "runPlp.rds")))

  # no results error
  expect_error(evaluateMultiplePlp(
    analysesLocation = file.path(saveLoc, "madeup123"),
    validationDatabaseDetails = databaseDetails,
    validationRestrictPlpDataSettings = createRestrictPlpDataSettings(),
    recalibrate = NULL
  ))
})
