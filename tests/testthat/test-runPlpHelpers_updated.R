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
context("runPlpHelpers")

test_that("check printHeader runs", {
  
  header <- printHeader(
    plpData = plpData, 
    cohortId = 1, 
    outcomeId = 2, 
    analysisId = 123, 
    analysisName = 'test', 
    ExecutionDateTime = Sys.time()
    )
  expect_is(header, "logical")
  
})

test_that("checkInputs", {

  check <- checkInputs(
    list(
      plpData = plpData, 
      outcomeId = 2, 
      populationSettings = populationSettings
    )
  )
  
  expect_is(check, "logical")
  expect_equal(check, TRUE)
  
  # error as NULL plpData
  expect_error(
    checkInputs(
      list(
        plpData = NULL, 
        outcomeId = 2, 
        populationSettings = populationSettings
      )
    )
  )
  
  # error as incorrect outcomeId
  expect_error(
    checkInputs(
      list(
        plpData = plpData, 
        outcomeId = 'test', 
        populationSettings = populationSettings
      )
    )
  )
  
  # error as incorrect populationSettings
  expect_error(
    checkInputs(
      list(
        plpData = plpData, 
        outcomeId = 2, 
        populationSettings = 'populationSettings'
      )
    )
  )
  
})


test_that("createExecuteSettings", {
  
  getTF <- function(){return(sample(c(T,F), 1))}
  runSplitData <- getTF()
  runSampleData <- getTF()
  runfeatureEngineering <- getTF()
  runPreprocessData <- getTF()
  runModelDevelopment <- getTF()
  runCovariateSummary <- getTF()
  
  executeSettings <- createExecuteSettings(
    runSplitData = runSplitData,
    runSampleData = runSampleData,
    runfeatureEngineering = runfeatureEngineering,
    runPreprocessData = runPreprocessData,
    runModelDevelopment = runModelDevelopment,
    runCovariateSummary = runCovariateSummary
  )
    
  expect_is(executeSettings, "executeSettings")
  expect_equal(executeSettings$runSplitData, runSplitData)
  expect_equal(executeSettings$runSampleData , runSampleData)
  expect_equal(executeSettings$runfeatureEngineering, runfeatureEngineering)
  expect_equal(executeSettings$runPreprocessData, runPreprocessData)
  expect_equal(executeSettings$runModelDevelopment, runModelDevelopment)
  expect_equal(executeSettings$runCovariateSummary, runCovariateSummary)
  
  expect_error(
    executeSettings <- createExecuteSettings(
      runSplitData = 12,
      runSampleData = runSampleData,
      runfeatureEngineering = runfeatureEngineering,
      runPreprocessData = runPreprocessData,
      runModelDevelopment = runModelDevelopment,
      runCovariateSummary = runCovariateSummary
    )
  )
  
  expect_error(
    executeSettings <- createExecuteSettings(
      runSplitData = runSplitData,
      runSampleData = 12,
      runfeatureEngineering = runfeatureEngineering,
      runPreprocessData = runPreprocessData,
      runModelDevelopment = runModelDevelopment,
      runCovariateSummary = runCovariateSummary
    )
  )
  
  expect_error(
    executeSettings <- createExecuteSettings(
      runSplitData = runSplitData,
      runSampleData = runSampleData,
      runfeatureEngineering = 12,
      runPreprocessData = runPreprocessData,
      runModelDevelopment = runModelDevelopment,
      runCovariateSummary = runCovariateSummary
    )
  )
  
  expect_error(
    executeSettings <- createExecuteSettings(
      runSplitData = runSplitData,
      runSampleData = runSampleData,
      runfeatureEngineering = runfeatureEngineering,
      runPreprocessData = 12,
      runModelDevelopment = runModelDevelopment,
      runCovariateSummary = runCovariateSummary
    )
  )
  
  expect_error(
    executeSettings <- createExecuteSettings(
      runSplitData = runSplitData,
      runSampleData = runSampleData,
      runfeatureEngineering = runfeatureEngineering,
      runPreprocessData = runPreprocessData,
      runModelDevelopment = 12,
      runCovariateSummary = runCovariateSummary
    )
  )
  
  expect_error(
    executeSettings <- createExecuteSettings(
      runSplitData = runSplitData,
      runSampleData = runSampleData,
      runfeatureEngineering = runfeatureEngineering,
      runPreprocessData = runPreprocessData,
      runModelDevelopment = runModelDevelopment,
      runCovariateSummary = 12
    )
  )
  
})



test_that("createDefaultExecuteSettings", {
  
  executeSettings <- createDefaultExecuteSettings()
  expect_is(executeSettings, "executeSettings")
  expect_equal(executeSettings$runSplitData, T)
  expect_equal(executeSettings$runSampleData , F)
  expect_equal(executeSettings$runfeatureEngineering, F)
  expect_equal(executeSettings$runPreprocessData, T)
  expect_equal(executeSettings$runModelDevelopment, T)
  expect_equal(executeSettings$runCovariateSummary, T)
  
  
})
