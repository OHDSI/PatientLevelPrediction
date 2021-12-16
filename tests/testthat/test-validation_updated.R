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

context("Validation")

# Test unit for the creation of the study externalValidatePlp

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
  cdmVersion = 5)

modelVal <- plpResult$model
validationDatabaseDetailsVal <- databaseDetails  # from run multiple tests
validationRestrictPlpDataSettingsVal <- createRestrictPlpDataSettings(washoutPeriod = 0, sampleSize = NULL)
recalSet <- createValidationSettings(recalibrate = 'weakRecalibration')
saveLocation <- file.path(saveLoc, 'extern')

setEV <- function(
  model = modelVal,
  validationDatabaseDetails = validationDatabaseDetailsVal,
  validationRestrictPlpDataSettings = validationRestrictPlpDataSettingsVal,
  settings = recalSet,
  outputFolder = saveLocation
){
  result <- externalValidateDbPlp(
    plpModel = model,
    validationDatabaseDetails = validationDatabaseDetails,
    validationRestrictPlpDataSettings = validationRestrictPlpDataSettings,
    settings = settings,
    outputFolder = outputFolder
  )
  
  return(result)
}


test_that("incorrect input externalValidateDbPlp checks work", {
  
  # fails when plpResult is NULL
  expect_error(externalValidateDbPlp(setEV(model=NULL)))
  # fails when plpResult is not class 'plpResult'
  expect_error(externalValidateDbPlp(setEV(model=list())))
  
  
  expect_error(externalValidateDbPlp(
    setEV(validationDatabaseDetails = NULL)
  ))
    
  expect_error(externalValidateDbPlp(
    setEV(validationRestrictPlpDataSettings = NULL)
  ))
  
  expect_error(externalValidateDbPlp(
    setEV(outputFolder = NULL)
  ))

  
})



test_that("external validate", {
  
  exVal <- setEV()
  testthat::expect_equal(class(exVal[[1]]), 'externalValidatePlp')
  
})
