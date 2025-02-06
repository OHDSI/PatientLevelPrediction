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
test_that("summary.plpData", {
  skip_if_offline()
  attr(plpData$outcomes, "metaData")$outcomeIds <- c(outcomeId)
  sum <- summary.plpData(plpData)
  expect_equal(class(sum), "summary.plpData")
})

test_that("getPlpData errors", {
  expect_error(
    getPlpData(
      databaseDetails = list(targetId = NULL)
    )
  )
  expect_error(
    getPlpData(
      databaseDetails = list(targetId = c(1, 2))
    )
  )
  expect_error(
    getPlpData(
      databaseDetails = list(targetId = 1, outcomeIds = NULL)
    )
  )
})


test_that("getPlpData works", {
  skip_if_offline()
  expect_true(is(plpData, "plpData"))
})

test_that("getCovariateData", {
  expect_error(getCovariateData())
})

test_that("createDatabaseDetails with NULL cdmDatabaseId errors", {
  expect_error(createDatabaseDetails(
    connectionDetails = list(),
    cdmDatabaseSchema = "main",
    cdmDatabaseId = NULL,
    targetId = 1,
    outcomeIds = 3
  ))
})

test_that("getPlpData checks covariateSettings object", {
  skip_if_offline()
  expect_error(getPlpData(
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
  expect_equal(
    plpData$covariateData$covariateRef %>%
      dplyr::pull(.data$analysisId) %>%
      length(),
    3
  )

  settings3 <- list(covariateId = 3)
  class(settings3) <- "NotCovariateSettings"

  expect_error(getPlpData(
    databaseDetails = databaseDetails,
    covariateSettings = list(settings1, settings3)
  ))
})

test_that("Get Eunomia plp data works", {
  skip_if_offline()
  skip_if_not_installed("Eunomia")
  plpData <- getEunomiaPlpData()

  expect_s3_class(plpData, "plpData")
  expect_true(is.list(plpData))
  expect_true("cohorts" %in% names(plpData))
  expect_true("covariateData" %in% names(plpData))
  expect_true("outcomes" %in% names(plpData))
  expect_true("metaData" %in% names(plpData))
  expect_true(is.data.frame(plpData$cohorts))
  expect_gt(nrow(plpData$cohorts), 0)
  expect_true("covariates" %in% names(plpData$covariateData))
  expect_true("covariateRef" %in% names(plpData$covariateData))
  expect_true("analysisRef" %in% names(plpData$covariateData))
  expect_equal(plpData$metaData$databaseDetails$outcomeIds, 3)
})

test_that("Get eunomia with custom covariates", {
  skip_if_offline()
  skip_if_not_installed("Eunomia")
  cs <- FeatureExtraction::createCovariateSettings(
    useDemographicsAge = FALSE,
    useDemographicsGender = FALSE,
    useConditionOccurrenceLongTerm = TRUE,
    useDrugEraLongTerm = TRUE
  )
  plpData <- getEunomiaPlpData(covariateSettings = cs)
  expect_s3_class(plpData, "plpData")
  covRef <- plpData$covariateData$covariateRef %>% dplyr::collect()
  expect_false(1002 %in% covRef$covariateId)
  expect_false(8532 %in% covRef$covariateId)
})

test_that("plpDataObjectDoc returns expected documentation text", {
  doc <- plpDataObjectDoc()
  expect_type(doc, "character")
  expect_true(nzchar(doc)) # Non-empty string
  expect_true(grepl("plpData", doc))
  expect_true(grepl("cohorts", doc))
  expect_true(grepl("covariateData", doc))
})

test_that("Print and print summary plpData work", {
  out <- capture.output(print(plpData))
  expect_true(any(grepl("plpData object", out)))
  expect_true(any(grepl("At risk concept ID: 1", out)))
  expect_true(any(grepl("Outcome concept ID.s.: 3", out)))
  summ <- summary(plpData)
  expect_type(summ, "list")
  expect_s3_class(summ, "summary.plpData")
  expect_true("targetId" %in% names(summ$metaData))
  expect_true("outcomeIds" %in% names(summ$metaData))
  printed <- capture.output(print(summ))
  expect_true(any(grepl("plpData object summary", printed)))
  expect_true(any(grepl("At risk cohort concept ID: 1", printed)))
  expect_true(any(grepl("Outcome concept ID.s.: 3", printed)))
  expect_true(any(grepl("People: 1800", printed)))
  expect_true(any(grepl("Number of covariates: 75", printed)))
  expect_true(any(grepl("Number of non-zero covariate values: 21949", printed)))
})
