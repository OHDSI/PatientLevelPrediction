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
  skip_if_no_eunomia()
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

test_that("cohort covariate metadata marks counts and interactions as non-binary", {
  connection <- createCohortCovariateTestDb(
    targetRows = data.frame(rowId = 1, subjectId = 1, cohortId = 1, startDate = "2020-06-01"),
    covariateRows = data.frame(subjectId = 1, cohortId = 2, startDate = "2020-05-20")
  )
  on.exit(DatabaseConnector::disconnect(connection))

  nonBinaryCases <- list(
    list(count = TRUE),
    list(ageInteraction = TRUE),
    list(lnAgeInteraction = TRUE),
    list(count = TRUE, ageInteraction = TRUE)
  )
  for (args in nonBinaryCases) {
    settings <- do.call(cohortCovariateSettingsFor, args)
    extracted <- extractCohortCovariate(connection, settings)
    expect_equal(extracted$analysisRef$isBinary, "N")
    expect_equal(extracted$analysisRef$missingMeansZero, "Y")
    expect_equal(extracted$analysisRef$analysisId, 1456)
    expect_equal(as.numeric(extracted$covariateRef$analysisId), 1456)
    # The covariateId formula is unchanged so stored models keep matching.
    expect_equal(as.numeric(extracted$covariateRef$covariateId), settings$covariateId)
  }

  binary <- extractCohortCovariate(connection, cohortCovariateSettingsFor())
  expect_equal(binary$analysisRef$isBinary, "Y")
  expect_equal(binary$analysisRef$analysisId, 456)
  expect_equal(as.numeric(binary$covariateRef$analysisId), 456)

  binaryCustomId <- extractCohortCovariate(
    connection, cohortCovariateSettingsFor(analysisId = 789)
  )
  expect_equal(binaryCustomId$analysisRef$analysisId, 789)
})

test_that("normalizers scale count covariates and leave binary cohort covariates alone", {
  connection <- createCohortCovariateTestDb(
    targetRows = data.frame(
      rowId = 1:3, subjectId = 1:3, cohortId = 1,
      startDate = "2020-06-01"
    ),
    covariateRows = data.frame(
      subjectId = c(1, 2, 3, 1, 2, 2, 3, 3, 3),
      cohortId = c(11, 11, 11, 12, 12, 12, 12, 12, 12),
      startDate = c(
        "2020-05-15", "2020-05-15", "2020-05-15",
        "2020-05-10", "2020-05-10", "2020-05-20",
        "2020-05-10", "2020-05-20", "2020-05-30"
      )
    )
  )
  on.exit(DatabaseConnector::disconnect(connection))

  # Both use the default analysisId, as they would in a real covariate settings list.
  binarySettings <- cohortCovariateSettingsFor(cohortId = 11, settingId = 1)
  countSettings <- cohortCovariateSettingsFor(cohortId = 12, settingId = 2, count = TRUE)
  binary <- extractCohortCovariate(connection, binarySettings)
  count <- extractCohortCovariate(connection, countSettings)

  makeTrainData <- function() {
    covariateData <- Andromeda::andromeda(
      covariates = rbind(binary$covariates, count$covariates),
      covariateRef = rbind(binary$covariateRef, count$covariateRef),
      analysisRef = rbind(binary$analysisRef, count$analysisRef)
    )
    class(covariateData) <- "CovariateData"
    attr(covariateData, "metaData") <- list()
    trainData <- list(
      labels = data.frame(rowId = 1:3),
      folds = data.frame(rowId = 1:3, index = 1),
      covariateData = covariateData
    )
    class(trainData) <- "plpData"
    trainData
  }
  valuesFor <- function(normalized, covariateId) {
    stored <- normalized$covariateData$covariates %>%
      dplyr::filter(.data$covariateId == !!covariateId) %>%
      dplyr::collect()
    # Normalizers keep the data sparse, so a row that normalizes to zero isn't stored.
    values <- stored$covariateValue[match(1:3, stored$rowId)]
    values[is.na(values)] <- 0
    return(values)
  }

  minMax <- minMaxNormalize(makeTrainData(), createNormalizer(type = "minmax"))
  expect_equal(valuesFor(minMax, binarySettings$covariateId), c(1, 1, 1))
  expect_equal(valuesFor(minMax, countSettings$covariateId), c(0, 0.5, 1))

  robust <- robustNormalize(makeTrainData(), createNormalizer(type = "robust", settings = list(clip = FALSE)))
  expect_equal(valuesFor(robust, binarySettings$covariateId), c(1, 1, 1))
  expect_equal(valuesFor(robust, countSettings$covariateId), c(-1, 0, 1))
})

test_that("cohort covariate settings record the analysisId reported in covariateRef", {
  binary <- cohortCovariateSettingsFor()
  expect_equal(binary$reportedAnalysisId, 456)
  for (args in list(list(count = TRUE), list(ageInteraction = TRUE), list(lnAgeInteraction = TRUE))) {
    nonBinary <- do.call(cohortCovariateSettingsFor, args)
    expect_equal(nonBinary$reportedAnalysisId, 1456)
    # covariateId keeps the configured analysisId, so stored models keep matching
    expect_equal(nonBinary$covariateId, binary$covariateId)
  }
  custom <- cohortCovariateSettingsFor(count = TRUE, analysisId = 668)
  expect_equal(custom$reportedAnalysisId, 1668)
  roundTrip <- ParallelLogger::convertJsonToSettings(ParallelLogger::convertSettingsToJson(custom))
  expect_equal(roundTrip$reportedAnalysisId, 1668)
})

test_that("getCohortCovariateData reports the analysisId stored in the settings", {
  connection <- createCohortCovariateTestDb(
    targetRows = data.frame(rowId = 1, subjectId = 1, cohortId = 1, startDate = "2020-06-01"),
    covariateRows = data.frame(subjectId = 1, cohortId = 2, startDate = "2020-05-20")
  )
  on.exit(DatabaseConnector::disconnect(connection))

  settings <- cohortCovariateSettingsFor(count = TRUE)
  settings$reportedAnalysisId <- 1999
  result <- extractCohortCovariate(connection, settings)
  expect_equal(result$covariateRef$analysisId, 1999)
  expect_equal(result$analysisRef$analysisId, 1999)

  # settings saved before reportedAnalysisId existed
  settings$reportedAnalysisId <- NULL
  result <- extractCohortCovariate(connection, settings)
  expect_equal(result$covariateRef$analysisId, 1456)
  expect_equal(result$analysisRef$isBinary, "N")
})

test_that("mergeDuplicateAnalysisRef keeps one row per analysisId so aggregation counts covariates once", {
  covariateData <- Andromeda::andromeda(
    covariates = data.frame(
      rowId = c(1, 1, 2, 1, 2),
      covariateId = c(201456, 203456, 203456, 202456, 202456),
      covariateValue = c(1, 1, 1, 3, 1)
    ),
    covariateRef = data.frame(
      covariateId = c(201456, 202456, 203456), covariateName = c("a", "b", "c"),
      analysisId = c(456, 1456, 456), conceptId = -1
    ),
    # one row per cohort covariate setting, as FeatureExtraction appends them
    analysisRef = data.frame(
      analysisId = c(456, 1456, 456), analysisName = "cohort covariate",
      domainId = "cohort covariate", startDay = c(-30, -365, -365), endDay = 0,
      isBinary = c("Y", "N", "Y"), missingMeansZero = "Y"
    )
  )
  class(covariateData) <- "CovariateData"
  attr(covariateData, "metaData") <- list(populationSize = 3)
  withr::defer(Andromeda::close(covariateData))

  PatientLevelPrediction:::mergeDuplicateAnalysisRef(covariateData)
  analysisRef <- as.data.frame(dplyr::collect(covariateData$analysisRef))
  expect_equal(sort(analysisRef$analysisId), c(456, 1456))
  expect_true(is.na(analysisRef$startDay[analysisRef$analysisId == 456]))
  expect_equal(analysisRef$startDay[analysisRef$analysisId == 1456], -365)
  expect_equal(analysisRef$endDay, c(0, 0))

  aggregated <- FeatureExtraction::aggregateCovariates(covariateData)
  withr::defer(Andromeda::close(aggregated))
  binary <- as.data.frame(dplyr::collect(aggregated$covariates))
  expect_equal(binary$sumValue[match(c(201456, 203456), binary$covariateId)], c(1, 2))
  continuous <- as.data.frame(dplyr::collect(aggregated$covariatesContinuous))
  expect_equal(continuous$averageValue[continuous$covariateId == 202456], 4 / 3)
})

test_that("mergeDuplicateAnalysisRef stops when one analysisId mixes covariate types", {
  covariateData <- Andromeda::andromeda(
    analysisRef = data.frame(analysisId = c(456, 456), isBinary = c("Y", "N"), missingMeansZero = "Y")
  )
  withr::defer(Andromeda::close(covariateData))
  expect_error(PatientLevelPrediction:::mergeDuplicateAnalysisRef(covariateData), "different isBinary")

  covariateData$analysisRef <- data.frame(analysisId = c(1, 456), isBinary = c("N", "Y"), missingMeansZero = "Y")
  PatientLevelPrediction:::mergeDuplicateAnalysisRef(covariateData)
  expect_equal(nrow(dplyr::collect(covariateData$analysisRef)), 2)
})

test_that("getPlpData keeps one analysisRef row per analysisId for several cohort covariates", {
  skip_if_no_eunomia()
  cohortSettings <- function(settingId, startDay, count = FALSE) {
    createCohortCovariateSettings(
      cohortName = paste("cohort", settingId), settingId = settingId,
      cohortDatabaseSchema = "main", cohortTable = "cohort", cohortId = 2,
      startDay = startDay, endDay = -1, count = count
    )
  }
  plpDataCohorts <- getPlpData(
    databaseDetails = databaseDetails,
    covariateSettings = list(
      FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE),
      cohortSettings(1, -30), cohortSettings(2, -365), cohortSettings(3, -365, count = TRUE)
    ),
    restrictPlpDataSettings = createRestrictPlpDataSettings()
  )
  analysisRef <- as.data.frame(dplyr::collect(plpDataCohorts$covariateData$analysisRef))
  expect_equal(anyDuplicated(analysisRef$analysisId), 0)
  expect_true(all(c(456, 1456) %in% analysisRef$analysisId))
  expect_true(is.na(analysisRef$startDay[analysisRef$analysisId == 456]))
  expect_equal(analysisRef$isBinary[analysisRef$analysisId == 1456], "N")
})
