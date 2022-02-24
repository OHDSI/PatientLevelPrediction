# Copyright 2022 Observational Health Data Sciences and Informatics
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

context("CovariateExtras")

connectionDetails <- Eunomia::getEunomiaConnectionDetails()
Eunomia::createCohorts(connectionDetails)


test_that("settings creation", {
  
covSet <- createCohortCovariateSettings(
  cohortName = 'covariateName', 
  settingId = 4,
  cohortDatabaseSchema = 'main', 
  cohortTable = 'cohort', 
  cohortId = 2,
  startDay = -350, 
  endDay = -2, 
  count = F, 
  ageInteraction = F, 
  lnAgeInteraction = F,
  analysisId = 456
)

expect_equal(class(covSet), "covariateSettings")

expect_equal(attr(covSet, 'fun'), "PatientLevelPrediction::getCohortCovariateData")

})

test_that("settings creation errors", {
  
expect_error(createCohortCovariateSettings(
  cohortName = 'covariateName', 
  settingId = -1,
  cohortDatabaseSchema = 'test_cdm', 
  cohortTable = 'table', 
  cohortId = 2,
  startDay = -350, 
  endDay = -2, 
  count = F, 
  ageInteraction = F, 
  lnAgeInteraction = F,
  analysisId = 456
))

expect_error(createCohortCovariateSettings(
  cohortName = 'covariateName', 
  settingId = 101,
  cohortDatabaseSchema = 'test_cdm', 
  cohortTable = 'table', 
  cohortId = 2,
  startDay = -350, 
  endDay = -2, 
  count = F, 
  ageInteraction = F, 
  lnAgeInteraction = F,
  analysisId = 456
))

expect_error(createCohortCovariateSettings(
  cohortName = 'covariateName', 
  settingId = '101',
  cohortDatabaseSchema = 'test_cdm', 
  cohortTable = 'table', 
  cohortId = 2,
  startDay = -350, 
  endDay = -2, 
  count = F, 
  ageInteraction = F, 
  lnAgeInteraction = F,
  analysisId = 456
))

})

test_that("extraction works", {
  
  covSet <- createCohortCovariateSettings(
    cohortName = 'covariateName', 
    settingId = 4,
    cohortDatabaseSchema = 'main', 
    cohortTable = 'cohort', 
    cohortId = 2,
    startDay = -350, 
    endDay = -2, 
    count = F, 
    ageInteraction = F, 
    lnAgeInteraction = F,
    analysisId = 456
  )
  
covs <- FeatureExtraction::getDbCovariateData(
  connectionDetails = connectionDetails, 
  cdmDatabaseSchema = "main", 
  cdmVersion = 5, 
  cohortTable = "cohort", 
  cohortDatabaseSchema = "main", 
  cohortTableIsTemp = F, 
  cohortId = 1, 
  rowIdField = 'rowId', 
  covariateSettings = covSet, 
  aggregated = F
  )

expect_equal(1, covs$covariateRef %>% dplyr::tally() %>% dplyr::pull())
expect_equal(as.double(covs$covariateRef %>% dplyr::select(.data$covariateId) %>% dplyr::collect()), covSet$covariateId)
expect_true(covs$covariates %>% dplyr::tally() %>% dplyr::pull() > 0)

})

