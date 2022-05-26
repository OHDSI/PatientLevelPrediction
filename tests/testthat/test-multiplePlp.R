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
context("MultiplePlp")

connectionDetails <- Eunomia::getEunomiaConnectionDetails()
Eunomia::createCohorts(connectionDetails)

databaseDetails <- createDatabaseDetails(
  connectionDetails = connectionDetails, 
  cdmDatabaseSchema = "main", 
  cdmDatabaseName = "main",
  cohortDatabaseSchema = "main", 
  cohortTable = "cohort", 
  outcomeDatabaseSchema = "main", 
  outcomeTable =  "cohort",
  cohortId = 1, 
  outcomeIds = 3, #make this ids
  cdmVersion = 5
  )


analysis1 <- createModelDesign(
  cohortId = 1,
  outcomeId = 3,
  restrictPlpDataSettings = createRestrictPlpDataSettings(firstExposureOnly = F, washoutPeriod = 0),
  populationSettings = createStudyPopulationSettings(),
  covariateSettings = FeatureExtraction::createDefaultCovariateSettings(),
  featureEngineeringSettings = NULL,
  sampleSettings = NULL,
  splitSettings = createDefaultSplitSetting(splitSeed = 1),
  preprocessSettings = createPreprocessSettings(),
  modelSettings = setLassoLogisticRegression(seed = 12)
)

test_that("createModelDesign - test working", {
  
  
  expect_equal(analysis1$cohortId, 1)
  expect_equal(analysis1$outcomeId, 3)
  expect_equal(analysis1$restrictPlpDataSettings, createRestrictPlpDataSettings(firstExposureOnly = F, washoutPeriod = 0))
  expect_equal(analysis1$covariateSettings, FeatureExtraction::createDefaultCovariateSettings())
  expect_equal(analysis1$featureEngineeringSettings, createFeatureEngineeringSettings())
  expect_equal(analysis1$preprocessSettings, createPreprocessSettings())
  expect_equal(analysis1$splitSettings, createDefaultSplitSetting(splitSeed = 1))
  expect_equal(analysis1$modelSettings, setLassoLogisticRegression(seed = 12))
  expect_equal(
    analysis1$executeSettings, 
    createExecuteSettings(
      runSplitData = T,
      runSampleData = F,
      runfeatureEngineering = F,
      runPreprocessData = T,
      runModelDevelopment = T,
      runCovariateSummary = T
    )
  )
  
})

test_that("saving analyses settings", {
  
  fileLocation <- savePlpAnalysesJson(
    modelDesignList = list(analysis1),
    saveDirectory = file.path(saveLoc, 'settings')
  )
  
  expect_true(file.exists(fileLocation))
  
}
)

test_that("loading analyses settings", {
  
  analysisSetting <- loadPlpAnalysesJson(file.path(saveLoc, 'settings',"predictionAnalysisList.json"))
  
  expect_equal(analysis1$cohortId, analysisSetting$analyses[[1]]$cohortId)
  expect_equal(analysis1$outcomeId, analysisSetting$analyses[[1]]$outcomeId)
  expect_equal(analysis1$restrictPlpDataSettings, analysisSetting$analyses[[1]]$restrictPlpDataSettings)
  expect_equal(attr(analysis1$covariateSettings, 'fun'), attr(analysisSetting$analyses[[1]]$covariateSettings,'fun') ) 
  expect_equal(analysis1$populationSettings, analysisSetting$analyses[[1]]$populationSettings)
  expect_equal(analysis1$sampleSettings, analysisSetting$analyses[[1]]$sampleSettings)
  expect_equal(attr(analysis1$featureEngineeringSettings,'class'), attr(analysisSetting$analyses[[1]]$featureEngineeringSettings,'class'))
  expect_equal(attr(analysis1$featureEngineeringSettings,'fun'), attr(analysisSetting$analyses[[1]]$featureEngineeringSettings,'fun'))
  expect_equal(analysis1$preprocessSettings, analysisSetting$analyses[[1]]$preprocessSettings)
  expect_equal(analysis1$modelSettings, analysisSetting$analyses[[1]]$modelSettings)
  expect_equal(analysis1$splitSettings, analysisSetting$analyses[[1]]$splitSettings)
  expect_equal(analysis1$executeSettings, analysisSetting$analyses[[1]]$executeSettings)
}
)

analysis2 <- createModelDesign(
  cohortId = 10,
  outcomeId = 2,
  restrictPlpDataSettings = createRestrictPlpDataSettings(firstExposureOnly = F, washoutPeriod = 9999),
  populationSettings = createStudyPopulationSettings(),
  covariateSettings = FeatureExtraction::createCovariateSettings(useDemographicsAge = T),
  featureEngineeringSettings = NULL,
  sampleSettings = NULL,
  preprocessSettings = createPreprocessSettings(),
  modelSettings = setLassoLogisticRegression(seed = 12)
)


test_that("test run multiple", {
  
  analysis3 <- createModelDesign(
    cohortId = 1,
    outcomeId = 3,
    restrictPlpDataSettings = createRestrictPlpDataSettings(firstExposureOnly = F, washoutPeriod = 0),
    populationSettings = createStudyPopulationSettings(),
    covariateSettings = FeatureExtraction::createDefaultCovariateSettings(),
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
    )
  )
  
  runMultiplePlp(
    databaseDetails = databaseDetails,
    modelDesignList = list(
      analysis3
    ),
    onlyFetchData = F,
    logSettings = createLogSettings(
      verbosity = "DEBUG", 
      timeStamp = T, 
      logName = "runPlp Log"
    ),
    saveDirectory = file.path(saveLoc, 'multiple')
  )
  
  expect_true(file.exists(file.path(saveLoc, 'multiple', 'settings.csv')))
  expect_true(dir.exists(file.path(saveLoc, 'multiple', 'Analysis_1')))
  expect_true(file.exists(file.path(saveLoc, 'multiple', 'Analysis_1','plpResult', 'runPlp.rds')))
  
})

test_that("validateMultiplePlp errors", {
  
  PatientLevelPrediction::validateMultiplePlp(
    analysesLocation = file.path(saveLoc,'multiple'),
    validationDatabaseDetails = databaseDetails, 
    validationRestrictPlpDataSettings = createRestrictPlpDataSettings(), 
    recalibrate = NULL
    )
  
expect_true(dir.exists(file.path(saveLoc, 'multiple', 'Validation', 'main')))
expect_true(dir.exists(file.path(saveLoc, 'multiple', 'Validation', 'main', 'Analysis_1', 'validationResult')))
expect_true(file.exists(file.path(saveLoc, 'multiple', 'Validation', 'main', 'Analysis_1', 'validationResult', 'runPlp.rds')))

  # no results error
  expect_error(evaluateMultiplePlp(
    analysesLocation = file.path(saveLoc,'madeup123') ,
    validationDatabaseDetails = databaseDetails, 
    validationRestrictPlpDataSettings = createRestrictPlpDataSettings(), 
    recalibrate = NULL
    ))
})

