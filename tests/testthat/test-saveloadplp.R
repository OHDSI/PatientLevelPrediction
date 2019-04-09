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

library("testthat")
context("SaveLoadPlp")


# print.plpData, summary.plpData, print.summary.plpData, 
#grepCovariateNames, insertDbPopulation , savePlpModel, moveHdModel, loadPlpModel, 
#updateModelLocation , savePrediction, loadPrediction, savePlpResult, loadPlpResult, 
#writeOutput, saveCirceDefinition

test_that("getPlpData", {
  testthat::expect_error(getPlpData(cohortId = NULL))
  testthat::expect_error(getPlpData(cohortId = c(1,2)))
  testthat::expect_error(getPlpData(cohortId = 1, outcomeIds = NULL))
})

test_that("getCovariateData", {
  testthat::expect_error(getCovariateData())
})

test_that("savePlpData", {
  testthat::expect_error(savePlpData())
  testthat::expect_error(savePlpData(plpData=1))
  testthat::expect_error(savePlpData(plpData=1, file='testing'))
})

test_that("loadPlpData", {
  testthat::expect_error(loadPlpData(file='madeup/dffdf/testing'))
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

# can't test insertDbPopulation


test_that("savePlpModel", {
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

test_that("loadPlpModel", {
  testthat::expect_error(loadPlpModel(dirPath=NULL))
  testthat::expect_error(loadPlpModel(dirPath='madeup.txt'))
})


test_that("updateModelLocation", {
  testthat::expect_equivalent(PatientLevelPrediction:::updateModelLocation(plpModel=plpModel, dirPath=NULL), plpModel)
})


# savePrediction and loadPrediction - will save so no tests


test_that("savePlpResult", {
  testthat::expect_error(savePlpResult(dirPath=NULL))
  testthat::expect_error(savePlpResult(result=NULL))
})


test_that("loadPlpResult", {
  testthat::expect_error(loadPlpResult(dirPath=NULL))
  testthat::expect_error(loadPlpResult(dirPath = 'madeup/dfdfd/j'))
})


# saveCirceDefinition  - might be removed soon?



