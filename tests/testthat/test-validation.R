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
modelVal <- loadPlpModel(file.path(saveLoc, 'Test', 'plpResult', 'model'))
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

test_that("fromDesignOrModel helper works", {
    
   settingName <- "restrictPlpDataSettings"
   validationDesign <- list(targetId = 1,
                            outcomeId = 2,
                            restrictPlpDataSettings = list(a = 1, b = 2)
   )
   modelDesigns <- list(
     list(targetId = 1,
          outcomeId = 2,
          restrictPlpDataSettings = list(a = 3, b = 4)
     ),
     list(targetId = 1,
          outcomeId = 2,
          restrictPlpDataSettings = list(a = 3, b = 4)
     )
   )
   output <- fromDesignOrModel(validationDesign, modelDesigns, settingName)
   
   expect_equal(output[[settingName]], list(a = 1, b = 2))
   validationDesign[[settingName]] <- NULL
   output <- fromDesignOrModel(validationDesign, modelDesigns, settingName)
   expect_equal(output[[settingName]], list(a = 3, b = 4))

})

test_that("createValidationDesign errors", {
  
  expect_error(createValidationDesign(targetId = NULL, outcomeId = 2,
                                      plpModelList = list()))
  expect_error(createValidationDesign(targetId = 1, outcomeId = NULL,
                                      plpModelList = list()))
  expect_error(createValidationDesign(targetId = "a", outcomeId = 2,
                                      plpModelList = list()))
  expect_error(createValidationDesign(targetId = 1, outcomeId = "a",
                                      plpModelList = list()))
  expect_error(createValidationDesign(targetId = 1, outcomeId = 2,
                                      plpModelList = list(),
                                      populationSettings = list()))
  expect_error(createValidationDesign(targetId = 1, outcomeId = 2,
                                      plpModelList = list(),
                                      recalibrate = 1))
  expect_error(createValidationDesign(targetId = 1, outcomeId = 2,
                                      plpModelList = list(),
                                      runCovariateSummary = 1))
})

test_that("createValidationDesign works with minimal required arguments", {
  targetId <- 1
  outcomeId <- 2
  plpModelList <- list()
  
  design <- createValidationDesign(
    targetId = targetId, 
    outcomeId = outcomeId, 
    plpModelList = plpModelList
    )
  expect_s3_class(design, "validationDesign")
  expect_equal(design$targetId, targetId)
  expect_equal(design$outcomeId, outcomeId)
  expect_equal(design$plpModelList, plpModelList)
})

test_that("single createValidationDesign works with all arguments", {
  targetId <- 1
  outcomeId <- 2
  plpModelList <- list("model1", "model2")
  populationSettings <- createStudyPopulationSettings()  
  restrictPlpDataSettings <- createRestrictPlpDataSettings()  # Replace with actual restrictPlpDataSettings object
  recalibrate <- c("recalibrationInTheLarge") 
  runCovariateSummary <- FALSE
  
  design <- createValidationDesign(
    targetId = targetId, 
    outcomeId = outcomeId, 
    plpModelList = plpModelList, 
    populationSettings = populationSettings, 
    restrictPlpDataSettings = restrictPlpDataSettings, 
    recalibrate = recalibrate, 
    runCovariateSummary = runCovariateSummary
    )
  expect_s3_class(design, "validationDesign")
  expect_equal(design$targetId, targetId)
  expect_equal(design$outcomeId, outcomeId)
  expect_equal(design$plpModelList, plpModelList)
  expect_equal(design$populationSettings, populationSettings)
  expect_equal(design$restrictPlpDataSettings, restrictPlpDataSettings)
  expect_equal(design$recalibrate, recalibrate)
  expect_equal(design$runCovariateSummary, runCovariateSummary)
})

test_that("createValidationDesigns correctly handles multiple restrictSettings", {
  targetId <- 1
  outcomeId <- 2
  plpModelList <- list()
  restrictPlpDataSettings <- list(createRestrictPlpDataSettings(), createRestrictPlpDataSettings())
  
  design <- createValidationDesign(
    targetId = targetId, 
    outcomeId =outcomeId, 
    plpModelList = plpModelList, 
    restrictPlpDataSettings = restrictPlpDataSettings
    )
  expect_s3_class(design[[1]], "validationDesign")
  expect_equal(design[[1]]$targetId, targetId)
  expect_equal(design[[1]]$outcomeId, outcomeId)
  expect_equal(design[[1]]$plpModelList, plpModelList)
  expect_equal(design[[1]]$restrictPlpDataSettings, restrictPlpDataSettings[[1]])
  expect_equal(design[[2]]$restrictPlpDataSettings, restrictPlpDataSettings[[2]])
  expect_equal(length(design), length(restrictPlpDataSettings))
})