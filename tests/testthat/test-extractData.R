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

context("extractPlp")

test_that("summary.plpData", {
  attr(plpData$outcomes, "metaData")$outcomeIds <- c(outcomeId)
  sum <- summary.plpData(plpData)
  testthat::expect_equal(class(sum), "summary.plpData")
})

test_that("getPlpData errors", {
  testthat::expect_error(
    getPlpData(
      databaseDetails = list(targetId = NULL)
    )
  )
  testthat::expect_error(
    getPlpData(
      databaseDetails = list(targetId = c(1, 2))
    )
  )
  testthat::expect_error(
    getPlpData(
      databaseDetails = list(targetId = 1, outcomeIds = NULL)
    )
  )
})


test_that("getPlpData works", {
  testthat::expect_true(is(plpData, "plpData"))
})

test_that("getCovariateData", {
  testthat::expect_error(getCovariateData())
})

test_that("createDatabaseDetails with NULL cdmDatabaseId errors", {
  testthat::expect_error(createDatabaseDetails(
    connectionDetails = list(),
    cdmDatabaseSchema = "main",
    cdmDatabaseId = NULL,
    targetId = 1,
    outcomeIds = outcomeId
  ))
})

test_that("getPlpData checks covariateSettings object", {
  testthat::expect_error(getPlpData(
    databaseDetails = list(targetId = 1, outcomeIds = outcomeId),
    covariateSettings = list()
  ))

  settings1 <-
    FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE)
  settings2 <-
    FeatureExtraction::createCovariateSettings(useDemographicsAge = TRUE)
  plpData <- getPlpData(
    databaseDetails = databaseDetails,
    covariateSettings = list(settings1, settings2)
  )
  expect_equal(plpData$covariateData$covariateRef %>% dplyr::pull(.data$analysisId %>% length()), 3)

  settings3 <- list(covariateId = 3)
  class(settings3) <- "NotCovariateSettings"

  expect_Error(getPlpData(
    databaseDetails = databaseDetails,
    covariateSettings = list(settings1, settings3)
  ))
})
