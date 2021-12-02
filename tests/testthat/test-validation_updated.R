# Copyright 2020 Observational Health Data Sciences and Informatics
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
