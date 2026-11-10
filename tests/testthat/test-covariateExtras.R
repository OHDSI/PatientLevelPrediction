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

test_that("settings creation", {
  covSet <- createCohortCovariateSettings(
    cohortName = "covariateName",
    settingId = 4,
    cohortDatabaseSchema = "main",
    cohortTable = "cohort",
    cohortId = 2,
    startDay = -350,
    endDay = -2,
    count = FALSE,
    ageInteraction = FALSE,
    lnAgeInteraction = FALSE,
    analysisId = 456
  )

  expect_equal(class(covSet), "covariateSettings")

  expect_equal(attr(covSet, "fun"), "PatientLevelPrediction::getCohortCovariateData")
})

test_that("settings creation errors", {
  expect_error(createCohortCovariateSettings(
    cohortName = "covariateName",
    settingId = -1,
    cohortDatabaseSchema = "test_cdm",
    cohortTable = "table",
    cohortId = 2,
    startDay = -350,
    endDay = -2,
    count = FALSE,
    ageInteraction = FALSE,
    lnAgeInteraction = FALSE,
    analysisId = 456
  ))

  expect_error(createCohortCovariateSettings(
    cohortName = "covariateName",
    settingId = 101,
    cohortDatabaseSchema = "test_cdm",
    cohortTable = "table",
    cohortId = 2,
    startDay = -350,
    endDay = -2,
    count = FALSE,
    ageInteraction = FALSE,
    lnAgeInteraction = FALSE,
    analysisId = 456
  ))

  expect_error(createCohortCovariateSettings(
    cohortName = "covariateName",
    settingId = "101",
    cohortDatabaseSchema = "test_cdm",
    cohortTable = "table",
    cohortId = 2,
    startDay = -350,
    endDay = -2,
    count = FALSE,
    ageInteraction = FALSE,
    lnAgeInteraction = FALSE,
    analysisId = 456
  ))
})

test_that("extraction works", {
  skip_if_offline()
  covSet <- createCohortCovariateSettings(
    cohortName = "covariateName",
    settingId = 4,
    cohortDatabaseSchema = "main",
    cohortTable = "cohort",
    cohortId = 2,
    startDay = -350,
    endDay = -2,
    count = FALSE,
    ageInteraction = FALSE,
    lnAgeInteraction = FALSE,
    analysisId = 456
  )

  covs <- FeatureExtraction::getDbCovariateData(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    cdmVersion = 5,
    cohortTable = "cohort",
    cohortDatabaseSchema = "main",
    cohortTableIsTemp = FALSE,
    cohortIds = c(3), # only cohort 3 entries have cohort 2 in this window
    rowIdField = "rowId",
    covariateSettings = covSet,
    aggregated = FALSE
  )

  expect_equal(1, covs$covariateRef %>% dplyr::tally() %>% dplyr::pull())
  expect_equal(as.double(covs$covariateRef %>% dplyr::select("covariateId") %>% dplyr::collect()), covSet$covariateId)
  expect_true(covs$covariates %>% dplyr::tally() %>% dplyr::pull() > 0)
  expect_equal(covs$analysisRef %>% dplyr::pull(.data$startDay), -350)
  expect_equal(covs$analysisRef %>% dplyr::pull(.data$endDay), -2)
})

createCohortCovariateTestDb <- function(targetRows, covariateRows) {
  connection <- DatabaseConnector::connect(dbms = "sqlite", server = ":memory:")
  unixDate <- function(date) {
    sprintf("CAST(strftime('%%s','%s') AS INTEGER)", date)
  }
  insertSql <- c(
    "CREATE TABLE person (person_id BIGINT, year_of_birth INTEGER);",
    "CREATE TABLE target_cohort (row_id BIGINT, subject_id BIGINT, cohort_definition_id INTEGER,
      cohort_start_date DATE, cohort_end_date DATE);",
    "CREATE TABLE covariate_cohort (subject_id BIGINT, cohort_definition_id INTEGER,
      cohort_start_date DATE, cohort_end_date DATE);",
    sprintf(
      "INSERT INTO person VALUES (%d, %d);",
      c(1L, 2L, 3L), c(1960L, 1980L, 1990L)
    ),
    sprintf(
      "INSERT INTO target_cohort VALUES (%d, %d, %d, %s, %s);",
      targetRows$rowId, targetRows$subjectId, targetRows$cohortId,
      unixDate(targetRows$startDate), unixDate(targetRows$startDate)
    ),
    sprintf(
      "INSERT INTO covariate_cohort VALUES (%d, %d, %s, %s);",
      covariateRows$subjectId, covariateRows$cohortId,
      unixDate(covariateRows$startDate), unixDate(covariateRows$startDate)
    )
  )
  DatabaseConnector::executeSql(
    connection,
    paste(insertSql, collapse = "\n"),
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
  connection
}

extractCohortCovariate <- function(connection, settings, ...) {
  result <- getCohortCovariateData(
    connection = connection,
    cdmDatabaseSchema = "main",
    cohortTable = "main.target_cohort",
    rowIdField = "row_id",
    aggregated = FALSE,
    covariateSettings = settings,
    ...
  )
  on.exit(Andromeda::close(result))
  list(
    covariates = as.data.frame(dplyr::collect(result$covariates)),
    covariateRef = as.data.frame(dplyr::collect(result$covariateRef)),
    analysisRef = as.data.frame(dplyr::collect(result$analysisRef))
  )
}

cohortCovariateSettingsFor <- function(count = FALSE, ageInteraction = FALSE,
                                       lnAgeInteraction = FALSE,
                                       cohortId = 2, settingId = 1, analysisId = 456,
                                       startDay = -30, endDay = 0) {
  createCohortCovariateSettings(
    cohortName = "covariate", settingId = settingId,
    cohortDatabaseSchema = "main", cohortTable = "covariate_cohort",
    cohortId = cohortId, startDay = startDay, endDay = endDay,
    count = count, ageInteraction = ageInteraction,
    lnAgeInteraction = lnAgeInteraction, analysisId = analysisId
  )
}

test_that("cohort covariate value multiplies count and age interactions", {
  connection <- createCohortCovariateTestDb(
    targetRows = data.frame(rowId = 1, subjectId = 1, cohortId = 1, startDate = "2020-06-01"),
    covariateRows = data.frame(
      subjectId = 1, cohortId = 2,
      startDate = c("2020-05-10", "2020-05-20", "2020-05-30")
    )
  )
  on.exit(DatabaseConnector::disconnect(connection))

  age <- 2020 - 1960
  cases <- list(
    list(args = list(), expected = 1),
    list(args = list(count = TRUE), expected = 3),
    list(args = list(ageInteraction = TRUE), expected = age),
    list(args = list(lnAgeInteraction = TRUE), expected = log(age)),
    list(args = list(count = TRUE, ageInteraction = TRUE), expected = 3 * age),
    list(args = list(count = TRUE, lnAgeInteraction = TRUE), expected = 3 * log(age))
  )
  for (case in cases) {
    settings <- do.call(cohortCovariateSettingsFor, case$args)
    extracted <- extractCohortCovariate(connection, settings)
    expect_equal(nrow(extracted$covariates), 1)
    expect_equal(as.numeric(extracted$covariates$covariateValue), case$expected, tolerance = 1e-8)
  }
})

test_that("getCohortCovariateData restricts rows to the requested target cohorts", {
  connection <- createCohortCovariateTestDb(
    targetRows = data.frame(
      rowId = c(1, 2), subjectId = 1, cohortId = c(1, 2),
      startDate = c("2020-01-01", "2020-07-01")
    ),
    covariateRows = data.frame(subjectId = 1, cohortId = 10, startDate = "2020-06-25")
  )
  on.exit(DatabaseConnector::disconnect(connection))
  settings <- cohortCovariateSettingsFor(cohortId = 10, startDay = -30, endDay = 0)

  # The event is only in range for the July index date (rowId 2).
  requestedOne <- extractCohortCovariate(connection, settings, cohortIds = 1)
  expect_equal(nrow(requestedOne$covariates), 0)

  requestedTwo <- extractCohortCovariate(connection, settings, cohortIds = 2)
  expect_equal(as.numeric(requestedTwo$covariates$rowId), 2)

  allCohorts <- extractCohortCovariate(connection, settings, cohortIds = -1)
  expect_equal(as.numeric(allCohorts$covariates$rowId), 2)

  # cohortIds has a default so the documented direct call without it keeps working.
  withoutCohortIds <- extractCohortCovariate(connection, settings)
  expect_equal(as.numeric(withoutCohortIds$covariates$rowId), 2)
})
