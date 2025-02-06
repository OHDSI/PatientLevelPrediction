# Copyright 2025 Observational Health Data Sciences and Informatics
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
test_that("check printHeader runs", {
  skip_if_not_installed("Eunomia")
  skip_if_offline()
  header <- printHeader(
    plpData = plpData,
    targetId = 1,
    outcomeId = outcomeId,
    analysisId = 123,
    analysisName = "test",
    executionDateTime = Sys.time()
  )
  expect_type(header, "logical")
})

test_that("checkInputs", {
  skip_if_not_installed("Eunomia")
  skip_if_offline()
  check <- checkInputs(
    list(
      plpData = plpData,
      outcomeId = outcomeId,
      populationSettings = populationSettings
    )
  )

  expect_type(check, "logical")
  expect_equal(check, TRUE)

  # error as NULL plpData
  expect_error(
    checkInputs(
      list(
        plpData = NULL,
        outcomeId = outcomeId,
        populationSettings = populationSettings
      )
    )
  )

  # error as incorrect outcomeId
  expect_error(
    checkInputs(
      list(
        plpData = plpData,
        outcomeId = "test",
        populationSettings = populationSettings
      )
    )
  )

  # error as incorrect populationSettings
  expect_error(
    checkInputs(
      list(
        plpData = plpData,
        outcomeId = outcomeId,
        populationSettings = "populationSettings"
      )
    )
  )
})


test_that("createExecuteSettings", {
  getTF <- function() {
    return(sample(c(TRUE, FALSE), 1))
  }
  runSplitData <- getTF()
  runSampleData <- getTF()
  runFeatureEngineering <- getTF()
  runPreprocessData <- getTF()
  runModelDevelopment <- getTF()
  runCovariateSummary <- getTF()

  executeSettings <- createExecuteSettings(
    runSplitData = runSplitData,
    runSampleData = runSampleData,
    runFeatureEngineering = runFeatureEngineering,
    runPreprocessData = runPreprocessData,
    runModelDevelopment = runModelDevelopment,
    runCovariateSummary = runCovariateSummary
  )

  expect_s3_class(executeSettings, "executeSettings")
  expect_equal(executeSettings$runSplitData, runSplitData)
  expect_equal(executeSettings$runSampleData, runSampleData)
  expect_equal(executeSettings$runFeatureEngineering, runFeatureEngineering)
  expect_equal(executeSettings$runPreprocessData, runPreprocessData)
  expect_equal(executeSettings$runModelDevelopment, runModelDevelopment)
  expect_equal(executeSettings$runCovariateSummary, runCovariateSummary)

  expect_error(
    executeSettings <- createExecuteSettings(
      runSplitData = 12,
      runSampleData = runSampleData,
      runFeatureEngineering = runFeatureEngineering,
      runPreprocessData = runPreprocessData,
      runModelDevelopment = runModelDevelopment,
      runCovariateSummary = runCovariateSummary
    )
  )

  expect_error(
    executeSettings <- createExecuteSettings(
      runSplitData = runSplitData,
      runSampleData = 12,
      runFeatureEngineering = runFeatureEngineering,
      runPreprocessData = runPreprocessData,
      runModelDevelopment = runModelDevelopment,
      runCovariateSummary = runCovariateSummary
    )
  )

  expect_error(
    executeSettings <- createExecuteSettings(
      runSplitData = runSplitData,
      runSampleData = runSampleData,
      runFeatureEngineering = 12,
      runPreprocessData = runPreprocessData,
      runModelDevelopment = runModelDevelopment,
      runCovariateSummary = runCovariateSummary
    )
  )

  expect_error(
    executeSettings <- createExecuteSettings(
      runSplitData = runSplitData,
      runSampleData = runSampleData,
      runFeatureEngineering = runFeatureEngineering,
      runPreprocessData = 12,
      runModelDevelopment = runModelDevelopment,
      runCovariateSummary = runCovariateSummary
    )
  )

  expect_error(
    executeSettings <- createExecuteSettings(
      runSplitData = runSplitData,
      runSampleData = runSampleData,
      runFeatureEngineering = runFeatureEngineering,
      runPreprocessData = runPreprocessData,
      runModelDevelopment = 12,
      runCovariateSummary = runCovariateSummary
    )
  )

  expect_error(
    executeSettings <- createExecuteSettings(
      runSplitData = runSplitData,
      runSampleData = runSampleData,
      runFeatureEngineering = runFeatureEngineering,
      runPreprocessData = runPreprocessData,
      runModelDevelopment = runModelDevelopment,
      runCovariateSummary = 12
    )
  )
})



test_that("createDefaultExecuteSettings", {
  executeSettings <- createDefaultExecuteSettings()
  expect_s3_class(executeSettings, "executeSettings")
  expect_equal(executeSettings$runSplitData, TRUE)
  expect_equal(executeSettings$runSampleData, FALSE)
  expect_equal(executeSettings$runFeatureEngineering, FALSE)
  expect_equal(executeSettings$runPreprocessData, TRUE)
  expect_equal(executeSettings$runModelDevelopment, TRUE)
  expect_equal(executeSettings$runCovariateSummary, TRUE)
})
