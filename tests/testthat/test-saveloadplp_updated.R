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

context("SaveLoadPlp")

saveLoc <- tempdir()

test_that("savePlpDataError", {
  expect_error(savePlpData())
  expect_error(savePlpData(plpData=1))
  expect_error(savePlpData(plpData=1, file='testing'))
})


oldCohorts <- plpData$cohorts
oldOutcomes <- plpData$outcomes
oldCovariates <- as.data.frame(plpData$covariateData$covariates)
oldCovariateRef <- as.data.frame(plpData$covariateData$covariateRef)
test_that("savePlpData", {
  savePlpData(plpData = plpData, 
              file =  file.path(saveLoc,"saveDataTest"), overwrite = T)
  testExist <- dir.exists(file.path(saveLoc,"saveDataTest"))
  expect_equal(testExist, T)
})

test_that("loadPlpDataError", {
  expect_error(loadPlpData(file='madeup/dffdf/testing'))
})

test_that("loadPlpData", {
  plpData <- loadPlpData(file = file.path(saveLoc,"saveDataTest"))
  expect_identical(plpData$cohorts, oldCohorts)
  expect_identical(plpData$outcomes, oldOutcomes)
  expect_equal(as.data.frame(plpData$covariateData$covariates), 
                         oldCovariates)
  expect_equal(as.data.frame(plpData$covariateData$covariateRef), 
                         oldCovariateRef)
})

# add tests using simualted data...
test_that("print.plpData", {
  expect_equal(print.plpData(NULL), NULL)
})

test_that("summary.plpData", {
  expect_error(summary.plpData(NULL))
})

test_that("print.summary.plpData", {
  expect_error(print.summary.plpData(NULL))
})


test_that("savePlpModelError", {
  expect_error(savePlpModel(dirPath=NULL))
  expect_error(savePlpModel(plpModel=NULL))
  expect_error(savePlpModel(plpModel=NULL,dirPath=NULL))
})

test_that("loadPlpModelError", {
  expect_error(loadPlpModel(dirPath=NULL))
  expect_error(loadPlpModel(dirPath='madeup.txt'))
})

# make an sklearn model to test file transport
dir.create(file.path(saveLoc, 'testMoveStart'))
write.csv(data.frame(a=1,b=2), file = file.path(saveLoc, 'testMoveStart', 'file.csv'), row.names = F)
plpModelTemp <- plpResult$model
plpModelTemp$model = file.path(saveLoc, 'testMoveStart')
attr(plpModelTemp, 'saveType') <- 'file'

test_that("move model files when saveType is file", {
  
  savePlpModel(plpModel = plpModelTemp, dirPath = file.path(saveLoc, 'testMoveEnd'))
  
  expect_equal(dir(file.path(saveLoc, 'testMoveStart')), dir(file.path(saveLoc, 'testMoveEnd', 'model')))
})

test_that("savePrediction", {
  predLoc <- savePrediction(prediction = data.frame(rowId=1:10, value=1:10), 
                            dirPath = saveLoc, fileName = "pred.json"  )
  expect_equal(file.exists(predLoc), T)
  
})

test_that("loadPrediction", {
  pred <- loadPrediction(file.path(saveLoc,"pred.json"))
  expect_identical(data.frame(rowId=1:10, value=1:10), pred)
})


test_that("savePlpResultError", {
  expect_error(savePlpResult(dirPath=NULL))
  expect_error(savePlpResult(result=NULL))
})

test_that("savePlpResult", {
  emptyModel <- list()
  attr(emptyModel, 'predictionFunction') <- 'none'
  attr(emptyModel, 'saveType') <- 'RtoJson'
  class(emptyModel) <- "plpModel"
  emptyResult <- list(
    model = emptyModel,
    prediction = data.frame(rowId=1:5, value = 1:5),
    performanceEvaluation = data.frame(),
    covariateSummary = NULL,
    executionSettings = NULL
    )
  class(emptyResult) <- 'runPlp'
  
  savePlpResult(result = emptyResult, dirPath = file.path(saveLoc,"plpResultTest"))
  expect_equal(dir.exists(file.path(saveLoc,"plpResultTest")), T)
  expect_equal(dir(file.path(saveLoc,"plpResultTest")), c("model","runPlp.rds"))
  
})


test_that("loadPlpResultError", {
  expect_error(loadPlpResult(dirPath=NULL))
  expect_error(loadPlpResult(dirPath = 'madeup/dfdfd/j'))
  write.csv(c(1), file.path(saveLoc,"file2.csv"))
  expect_error(loadPlpResult(dirPath = file.path(saveLoc,"file2.csv")))
})

test_that("loadPlpResult", {
  emptyModel <- list()
  attr(emptyModel, 'predictionFunction') <- 'none'
  attr(emptyModel, 'saveType') <- 'RtoJson'
  class(emptyModel) <- "plpModel"
  emptyResult <- list(
    model = emptyModel,
    prediction = data.frame(rowId=1:5, value = 1:5),
    performanceEvaluation = data.frame(),
    covariateSummary = NULL,
    executionSettings = NULL
  )
  class(emptyResult) <- 'runPlp'
  
  plpResultLoaded <- loadPlpResult(file.path(saveLoc,"plpResultTest"))

  expect_identical(plpResultLoaded$covariateSummary, emptyResult$covariateSummary)
  expect_identical(plpResultLoaded$executionSummary, emptyResult$executionSummary)
  expect_identical(plpResultLoaded$performanceEvaluation, emptyResult$performanceEvaluation)
  expect_identical(plpResultLoaded$prediction, emptyResult$prediction)
  
})


test_that("savePlpShareable works", {
  
  #check it works
  savePlpShareable(plpResult, file.path(saveLoc,"plpFriendly"), minCellCount = 0)
  shareableLoad <- loadPlpShareable(file.path(saveLoc,"plpFriendly"))
  
  # check covariateSummary
  testthat::expect_equal(nrow(shareableLoad$covariateSummary), nrow(plpResult$covariateSummary))
  
  # check performanceEvaluation
  expect_equal(dim(shareableLoad$performanceEvaluation$evaluationStatistics), 
                         dim(plpResult$performanceEvaluation$evaluationStatistics)
                         )
  expect_equal(dim(shareableLoad$performanceEvaluation$thresholdSummary), 
                         dim(plpResult$performanceEvaluation$thresholdSummary)
  )
  expect_equal(dim(shareableLoad$performanceEvaluation$demographicSummary), 
                         dim(plpResult$performanceEvaluation$demographicSummary)
  )
  expect_equal(dim(shareableLoad$performanceEvaluation$calibrationSummary), 
                         dim(plpResult$performanceEvaluation$calibrationSummary)
  )
  expect_equal(dim(shareableLoad$performanceEvaluation$predictionDistribution), 
                         dim(plpResult$performanceEvaluation$predictionDistribution)
  )
  
})
