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

context("Diagnostic")

test_that("test diagnosePlp works", {
  test <- diagnosePlp(
    plpData = plpData,
    outcomeId = 2,
    analysisId = 'diagnoseTest',
    populationSettings = createStudyPopulationSettings(
      riskWindowStart = 1, 
      startAnchor = 'cohort start', 
      riskWindowEnd = 365,
      endAnchor = 'cohort start'
    ),
    splitSettings = createDefaultSplitSetting(),
    sampleSettings = createSampleSettings(), # default none
    saveDirectory = file.path(saveLoc, 'diagnostics'),
    featureEngineeringSettings = createFeatureEngineeringSettings(), # default none
    modelSettings = setLassoLogisticRegression(), # default to logistic regression
    preprocessSettings = createPreprocessSettings()
  )
  #check results are a list
  testthat::expect_is(test, 'diagnosePlp')
                         
  # check list names
  testthat::expect_equal(
    sum( names(test) %in% 
           c('summary','participants','predictors',
             'outcomes', 'designs', 'modelDesign',
             'databaseSchema')
         )
    , 7)
  
  # check the results are saved into the databaseName directory
  testthat::expect_equal(T, dir.exists(file.path(saveLoc, 'diagnostics')))
  testthat::expect_equal(T, file.exists(file.path(saveLoc, 'diagnostics','diagnoseTest','diagnosePlp.rds')))
  
  testthat::expect_is(test$summary, 'data.frame')
  testthat::expect_is(test$participants, 'data.frame')
  testthat::expect_is(test$predictors, 'data.frame') # rename this outcome survival?
  testthat::expect_is(test$outcomes, 'data.frame') 
  testthat::expect_is(test$databaseSchema, 'character') 
  
  testthat::expect_true(!is.null(test$modelDesign$targetId))
  testthat::expect_true(!is.null(test$modelDesign$outcomeId))
  testthat::expect_true(!is.null(test$modelDesign$restrictPlpDataSettings))
  testthat::expect_true(!is.null(test$modelDesign$covariateSettings))
  testthat::expect_true(!is.null(test$modelDesign$populationSettings))


})


test_that("test diagnoseMultiplePlp works", {
  
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
    targetId = 1, 
    outcomeIds = 3, #make this ids
    cdmVersion = 5
  )
  
  analysis1 <- createModelDesign(
    targetId = 1,
    outcomeId = 3,
    restrictPlpDataSettings = createRestrictPlpDataSettings(firstExposureOnly = F, washoutPeriod = 0),
    populationSettings = createStudyPopulationSettings(),
    covariateSettings = FeatureExtraction::createDefaultCovariateSettings(),
    featureEngineeringSettings = NULL,
    sampleSettings = NULL,
    splitSettings = createDefaultSplitSetting(),
    preprocessSettings = createPreprocessSettings(),
    modelSettings = setLassoLogisticRegression(seed = 12)
  )
  
  analysis2 <- createModelDesign(
    targetId = 1,
    outcomeId = 3,
    restrictPlpDataSettings = createRestrictPlpDataSettings(firstExposureOnly = F, washoutPeriod = 0),
    populationSettings = createStudyPopulationSettings(washoutPeriod = 400),
    covariateSettings = FeatureExtraction::createDefaultCovariateSettings(),
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
      cohortId = c(1,3),
      cohortName = c('target', 'outcome')
    ), 
    saveDirectory = file.path(saveLoc, 'diagnosticsMultiple')
  )
  
  # file.path(saveDirectory,'settings.csv') exits
  testthat::expect_true(file.exists(file.path(saveLoc, 'diagnosticsMultiple', 'settings.csv')))
  
  # file.path(saveDirectory, settings$analysisId, 'diagnosePlp.rds') exists
  testthat::expect_true(length(dir(file.path(saveLoc, 'diagnosticsMultiple'), pattern = 'Analysis_')) == 2)
  
  testthat::expect_true(file.exists(file.path(saveLoc, 'diagnosticsMultiple', 'Analysis_1', 'diagnosePlp.rds')))
  
})
