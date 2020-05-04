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

context("SaveLoadPlp")

# print.plpData, summary.plpData, print.summary.plpData, 
#grepCovariateNames, insertDbPopulation , savePlpModel, moveHdModel, loadPlpModel, 
#updateModelLocation , savePrediction, loadPrediction, savePlpResult, loadPlpResult, 
#writeOutput, saveCirceDefinition

test_that("summary.plpData", {
  attr(plpData$outcomes, "metaData")$outcomeIds <- 2
  sum <- summary.plpData(plpData)
  testthat::expect_equal(class(sum),'summary.plpData')
  })

test_that("getPlpData errors", {
  testthat::expect_error(getPlpData(cohortId = NULL))
  testthat::expect_error(getPlpData(cohortId = c(1,2)))
  testthat::expect_error(getPlpData(cohortId = 1, outcomeIds = NULL))
})


test_that("getPlpData works", {
  testthat::expect_true(is(plpDataReal, "plpData"))
})

test_that("getCovariateData", {
  testthat::expect_error(getCovariateData())
})

test_that("savePlpDataError", {
  testthat::expect_error(savePlpData())
  testthat::expect_error(savePlpData(plpData=1))
  testthat::expect_error(savePlpData(plpData=1, file='testing'))
})


oldCohorts <- plpData$cohorts
oldOutcomes <- plpData$outcomes
oldCovariates <- as.data.frame(plpData$covariateData$covariates)
oldCovariateRef <- as.data.frame(plpData$covariateData$covariateRef)
test_that("savePlpData", {
  savePlpData(plpData = plpData, 
              file =  file.path(saveLoc,"saveDataTest"), overwrite = T)
  testExist <- dir.exists(file.path(saveLoc,"saveDataTest"))
  testthat::expect_equal(testExist, T)
})

test_that("loadPlpDataError", {
  testthat::expect_error(loadPlpData(file='madeup/dffdf/testing'))
})

test_that("loadPlpData", {
  plpData <- loadPlpData(file = file.path(saveLoc,"saveDataTest"))
  testthat::expect_identical(plpData$cohorts, oldCohorts)
  testthat::expect_identical(plpData$outcomes, oldOutcomes)
  testthat::expect_equal(as.data.frame(plpData$covariateData$covariates), 
                         oldCovariates)
  testthat::expect_equal(as.data.frame(plpData$covariateData$covariateRef), 
                         oldCovariateRef)
})

# add tests using simualted data...
test_that("print.plpData", {
  testthat::expect_equal(PatientLevelPrediction:::print.plpData(NULL), NULL)
})

test_that("summary.plpData", {
  testthat::expect_error(PatientLevelPrediction:::summary.plpData(NULL))
})

test_that("print.summary.plpData", {
  testthat::expect_error(PatientLevelPrediction:::print.summary.plpData(NULL))
})

test_that("grepCovariateNames", {
  testthat::expect_error(grepCovariateNames(object=NULL))
})


test_that("savePlpModelError", {
  testthat::expect_error(savePlpModel(dirPath=NULL))
  testthat::expect_error(savePlpModel(plpModel=NULL))
  testthat::expect_error(savePlpModel(plpModel=NULL,dirPath=NULL))
})

plpModel <- list()
attr(plpModel, 'type') <- 'madeup'
test_that("moveHdModel", {
  testthat::expect_error(PatientLevelPrediction:::moveHdModel(plpModel=NULL, dirPath=NULL))
  testthat::expect_equal(PatientLevelPrediction:::moveHdModel(plpModel=plpModel,dirPath=NULL), T)
})

test_that("loadPlpModelError", {
  testthat::expect_error(loadPlpModel(dirPath=NULL))
  testthat::expect_error(loadPlpModel(dirPath='madeup.txt'))
})


test_that("updateModelLocation", {
  testthat::expect_equivalent(PatientLevelPrediction:::updateModelLocation(plpModel=plpModel, dirPath=NULL), plpModel)
})


# savePrediction and loadPrediction - will save so no tests
test_that("savePrediction", {
  predLoc <- savePrediction(prediction = plpResult$prediction, 
                            dirPath = saveLoc, fileName = "pred.csv"  )
  fileExists <- file.exists(predLoc)
  testthat::expect_equal(fileExists, T)
})

test_that("loadPrediction", {
  pred <- loadPrediction(file.path(saveLoc,"pred.csv"))
  testthat::expect_identical(plpResult$prediction, pred)
})


test_that("savePlpResultError", {
  testthat::expect_error(savePlpResult(dirPath=NULL))
  testthat::expect_error(savePlpResult(result=NULL))
})

test_that("savePlpResult", {
  savePlpResult(result = plpResult, dirPath = file.path(saveLoc,"plpResultTest"))
  testExist <- dir.exists(file.path(saveLoc,"plpResultTest"))
  testthat::expect_equal(testExist, T)
})


test_that("loadPlpResultError", {
  testthat::expect_error(loadPlpResult(dirPath=NULL))
  testthat::expect_error(loadPlpResult(dirPath = 'madeup/dfdfd/j'))
  write.csv(c(1), file.path(saveLoc,"file.csv"))
  testthat::expect_error(loadPlpResult(dirPath = file.path(saveLoc,"file.csv")))
})

test_that("loadPlpResult", {
  plpResultLoaded <- loadPlpResult(file.path(saveLoc,"plpResultTest"))
  if(is.null(plpResultLoaded$model$dense)){
    ind <- which(names(plpResultLoaded$model)=='dense')
    if(length(ind)>0){
      plpResultLoaded$model[[ind]] <- NULL
    }
  }
  testthat::expect_equal(plpResultLoaded$model, plpResult$model)
  testthat::expect_identical(plpResultLoaded$analysisRef, plpResult$analysisRef)
  testthat::expect_identical(plpResultLoaded$covariateSummary, plpResult$covariateSummary)
  testthat::expect_identical(plpResultLoaded$executionSummary, plpResult$executionSummary)
  testthat::expect_identical(plpResultLoaded$inputSetting, plpResult$inputSetting)
  testthat::expect_identical(plpResultLoaded$performanceEvaluation, plpResult$performanceEvaluation)
  testthat::expect_identical(plpResultLoaded$prediction, plpResult$prediction)
  
})

