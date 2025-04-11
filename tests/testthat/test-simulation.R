# @file test-simulation.R
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
test_that("create simulation profile works", {
  cohorts <- data.frame(rowId = 1:10, subjectId = 1:10, targetId = rep(1, 10))
  covariates <- data.frame(
    rowId = c(rep(1:10, each = 2), 1:10),
    covariateId = c(rep(c(1002, 14310024102), 10), rep(8532, 10)),
    covariateValue = c(stats::runif(20), stats::rbinom(10, 1, 0.5))
  )
  covariateRef <- data.frame(
    covariateId = c(1002, 14310024102, 8532),
    covariateName = c("cont1", "cont2", "bin1"),
    analysisId = c(1, 1, 2)
  )
  analysisRef <- data.frame(
    analysisId = c(1, 2),
    analysisName = c("continuous", "binary"),
    isBinary = c("N", "Y")
  )
  outcomes <- data.frame(
    rowId = c(1, 2, 3, 4, 5),
    daysToEvent = c(80, 90, 100, 110, 120)
  )
  metaData <- list(
    databaseDetails = list(outcomeIds = c(1))
  )
  simData <- list(
    cohorts = cohorts,
    outcomes = outcomes,
    metaData = metaData,
    covariateData = list(
      covariates = covariates,
      covariateRef = covariateRef,
      analysisRef = analysisRef
    )
  )
  simProfile <- createSimulationProfile(simData)
  expect_type(simProfile, "list")
  expect_s3_class(simProfile, "plpDataSimulationProfile")
  expect_true(all(c(
    "covariateInfo", "timeMax", "outcomeRate",
    "outcomeModels", "metaData", "covariateRef"
  ) %in% names(simProfile)))

  prevalence <- simProfile$covariateInfo$covariatePrevalence
  expect_type(prevalence, "double")
  expect_equal(as.numeric(prevalence["1002"]), 1)
  expect_equal(as.numeric(prevalence["8532"]), 1)
  expect_equal(as.numeric(prevalence["14310024102"]), 1)

  continuousCovs <- simProfile$covariateInfo$continuousCovariates
  expect_equal(nrow(continuousCovs), 2)
  expect_equal(continuousCovs$covariateId, c(1002, 14310024102))
  expect_equal(simProfile$outcomeRate, 0.5)
  expect_equal(simProfile$timeMax, max(outcomes$daysToEvent))
  expect_equal(
    length(simProfile$outcomeModels),
    length(metaData$databaseDetails$outcomeIds)
  )
  expect_true(all(list("(Intercept)" = -2.0, "1002" = 0.04, "8532" = 0.50)
  %in% simProfile$outcomeModels[[1]]))
  expect_equal(simProfile$metaData, metaData)
  expect_equal(simProfile$covariateRef, as.data.frame(covariateRef))
})

test_that("simulatePlpData works", {
  # mock predictCyclops function
  predictCyclopsType <- function(coefficients, cohorts, covariateData, modelType) {
    data.frame(rowId = cohorts$rowId, value = rep(0.5, nrow(cohorts)))
  }
  dummyProfile <- list(
    covariateInfo = list(
      covariatePrevalence = c("1002" = 1, "8532" = 0.3, "2001" = 0.5), continuousCovariates = data.frame(
        covariateId = 1002,
        mean = 50,
        sd = 5,
        min = 30,
        max = 70
      )
    ),
    covariateRef = data.frame(
      covariateId = 2001,
      covariateName = "continuous feature",
      stringsAsFactors = FALSE
    ), timeMax = c(100), outcomeModels = list(c("(Intercept)" = -2, "1002" = 0.04, "8532" = 0.05)),
    metaData = list(
      databaseDetails = list(
        outcomeIds = 3
      )
    )
  )

  n <- 100 # population size for the simulation
  simData <- simulatePlpData(dummyProfile, n = n)
  expect_s3_class(simData, "plpData")
  expect_true(is.data.frame(simData$cohorts))
  expect_equal(nrow(simData$cohorts), n)
  expect_true("rowId" %in% names(simData$cohorts))
  expect_true("subjectId" %in% names(simData$cohorts))
  expect_true("targetId" %in% names(simData$cohorts))
  expect_true("cohortStartDate" %in% names(simData$cohorts))
  expect_s3_class(simData$cohorts$cohortStartDate, "Date")
  expect_s4_class(simData$covariateData, "CovariateData")
  expect_true("covariates" %in% names(simData$covariateData))
  expect_true("covariateRef" %in% names(simData$covariateData))
  expect_true("analysisRef" %in% names(simData$covariateData))
  covMeta <- attr(simData$covariateData, "metaData")
  expect_type(covMeta, "list")
  expect_equal(covMeta$populationSize, n)
  expect_true(is.data.frame(simData$outcomes))
  if (nrow(simData$outcomes) > 0) {
    for (col in c("rowId", "outcomeId", "outcomeCount", "daysToEvent")) {
      expect_true(col %in% names(simData$outcomes))
    }
  }
  expect_true(is.list(simData$metaData))
  expect_true("databaseDetails" %in% names(simData$metaData))
  expect_equal(simData$metaData$databaseDetails$cdmDatabaseSchema, "CDM_SCHEMA")
  expect_equal(simData$metaData$databaseDetails$cdmDatabaseName, "CDM_NAME") 
  expect_equal(simData$metaData$databaseDetails$cdmVersion, 5)
  expect_equal(simData$metaData$databaseDetails$targetId, 1) 
  expect_equal(simData$metaData$databaseDetails$outcomeIds, 3)
})
