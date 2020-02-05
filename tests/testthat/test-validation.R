# Copyright 2019 Observational Health Data Sciences and Informatics
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

context("Validation")

# Test unit for the creation of the study externalValidatePlp

test_that("input checks work", {
  
  # fails when plpResult is NULL
  expect_error(externalValidatePlp(plpResult=NULL))
  # fails when plpResult is not class 'plpResult'
  expect_error(externalValidatePlp(plpResult=list()))
  
  
  #fails with no connection
  expect_error(externalValidatePlp(plpResult=plpResult))
  
  connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = NULL,
                                                                  server = NULL,
                                                                  user = NULL,
                                                                  password = NULL,
                                                                  port = NULL)
  #fails as target and outcome schemas differ
  expect_error(externalValidatePlp(plpResult=plpResult, connectionDetails = connectionDetails, 
                                   validation_schema_target = list('cdm1', 'cdm2'), 
                                   validation_schema_outcome = 'cdm', 
                                   validation_schema_cdm = 'cdm', 
                                   validation_table_target = 'table', validation_table_outcome = 'table',
                                   validation_id_target = 1, validation_id_outcome = 1))
  
  #fails as target and outcome schemas differ
  expect_error(externalValidatePlp(plpResult=plpResult, connectionDetails = connectionDetails, 
                                   validation_schema_target = list('cdm1', 'cdm2'), 
                                   validation_schema_outcome = 'cdm', 
                                   validation_schema_cdm = list('cdm1', 'cdm2'), 
                                   validation_table_target = 'table', validation_table_outcome = 'table',
                                   validation_id_target = 1, validation_id_outcome = 1))
  
  #fails as target and outcome schemas differ
  expect_error(externalValidatePlp(plpResult=plpResult, connectionDetails = connectionDetails, 
                                   validation_schema_target = list('cdm1', 'cdm2'), 
                                   validation_schema_outcome = list('cdm1', 'cdm2','cdm3'), 
                                   validation_schema_cdm = list('cdm1', 'cdm2'), 
                                   validation_table_target = 'table', validation_table_outcome = 'table',
                                   validation_id_target = 1, validation_id_outcome = 1))
  
  # fails as no modelTable
  expect_error(evaluateExistingModel())
  
  #fails as incorrect columns: 'modelId','modelCovariateId'
  expect_error(evaluateExistingModel(modelTable=data.frame(modelId=rep(1,2), modelCovariateId=1:2)))
  
  #fails as covariateTable missin
  expect_error(evaluateExistingModel(modelTable=data.frame(modelId=rep(1,2), modelCovariateId=1:2, 
                                                           coefficientValue=rep(1,2))))
  
  expect_error(evaluateExistingModel(modelTable=data.frame(modelId=rep(1,2), modelCovariateId=1:2, 
                                                           coefficientValue=rep(1,2)),
                                     covariateTable = data.frame(modelCovariateId=1:2)))
  
  expect_error(evaluateExistingModel(modelTable=data.frame(modelId=rep(1,2), modelCovariateId=1:2, 
                                                           coefficientValue=rep(1,2)),
                                     covariateTable = data.frame(modelCovariateId=1:2, covariateId=1:2),
                                     type='false'))
  
  
  
  
})

exVal <- externalValidatePlp(plpResult=plpResultReal, 
                             connectionDetails = connectionDetails, 
                             validationSchemaTarget = ohdsiDatabaseSchema, 
                             validationSchemaOutcome = ohdsiDatabaseSchema, 
                             validationSchemaCdm = cdmDatabaseSchema, 
                             validationTableTarge = 'cohorts', 
                             validationTableOutcome = 'outs_test',
                             databaseNames = 'test',
                             validationIdTarget = 1, 
                             validationIdOutcome = 2)
test_that("external validate", {
  
  testthat::expect_equal(class(exVal), 'validatePlp')
  testthat::expect_equal(sum(names(exVal)%in%c('summary','validation')), 2)
  
})

test_that("getSummary with external validation", {
  res <- getSummary(result = plpResultReal, inputType = 'plpResult', validation = exVal)
  testthat::expect_equal(class(res), 'data.frame')
  testthat::expect_equal(nrow(res), 2)
})

existingModel <- evaluateExistingModel(modelTable = data.frame(modelId = c(1,1,1,1,1),
                                                               modelCovariateId = 1:5, 
                                                               coefficientValue = c(1, 1, 1, 1, 2)), 
                                       covariateTable = data.frame(modelCovariateId = c(1,2,3,3,3,3,3,4,5),
                                                                   covariateId = c(319835102, 316866102, 
                                                                                   15003, 16003, 17003, 18003, 19003, 
                                                                                   201820102, 381591102)), 
                                       interceptTable=NULL, 
                                       type='score',
                                       covariateSettings = FeatureExtraction::createCovariateSettings(useDemographicsAgeGroup = T),
                                       customCovariates=NULL,
                                       addExposureDaysToStart = F,
                                       riskWindowStart = 1, 
                                       addExposureDaysToEnd = F,
                                       riskWindowEnd = 365,
                                       requireTimeAtRisk = T, 
                                       minTimeAtRisk = 364,
                                       includeAllOutcomes = T,
                                       removeSubjectsWithPriorOutcome=T,
                                       priorOutcomeLookback = 99999,
                                       verbosity = 'INFO', 
                                       washoutPeriod = 0,
                                       firstExposureOnly= F, 
                                       binary = T,
                                       connectionDetails =connectionDetails,
                                       cdmDatabaseSchema = cdmDatabaseSchema,
                                       cohortDatabaseSchema = ohdsiDatabaseSchema, 
                                       cohortTable ='cohorts', 
                                       cohortId = 1,
                                       outcomeDatabaseSchema = ohdsiDatabaseSchema, 
                                       outcomeTable = 'outs_test', 
                                       outcomeId = 2,
                                       oracleTempSchema = cdmDatabaseSchema,
                                       modelName='existingModel',
                                       scoreToProb = NULL,
                                       recalibrate = T,
                                       calibrationPopulation=NULL,
                                       covariateSummary = T,
                                       cdmVersion = 5
)

test_that("validate existing models", {

  testthat::expect_equal(class(existingModel), 'list')
})




test_that("createPlpJournalDocument document works with validation", {
  doc <- createPlpJournalDocument(plpResult=plpResultReal, 
                                  plpValidation = exVal,
                                  table1 = F, 
                                  connectionDetails = connectionDetails,
                                  plpData = plpData, 
                                  targetName='target test', 
                                  outcomeName='outcome test',
                                  includeTrain=T, includeTest=T, 
                                  includePredictionPicture=T, 
                                  includeAttritionPlot=T, save=F)
  expect_equal(class(doc), "rdocx")
  
})

test_that("createPlpReport document works with validation", {
  doc <- createPlpReport(plpResult=plpResultReal, plpValidation=exVal,
                         plpData = plpData, 
                         targetName = '<target population>',
                         outcomeName = '<outcome>',
                         targetDefinition = 'NULL',
                         outcomeDefinition = 'NULL',
                         outputLocation= file.path(saveLoc,'plp_val_report.docx'),
                         save = F)
  
  expect_equal(class(doc), "rdocx")
  
})
