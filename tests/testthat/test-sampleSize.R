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

# Tests for the target cohort sampling that sampleSize applies in
# inst/sql/sql_server/CreateCohorts.sql.

ageCovariateSettings <- function() {
  FeatureExtraction::createCovariateSettings(useDemographicsAge = TRUE)
}

# Replaces person, observation_period and cohort in a copy of Eunomia, because
# Eunomia is too small to measure bias and has one entry per person. Index dates
# are uniform over 2005-2020 and person IDs are sequential.
createSamplingCdm <- function(nPersons = 20000L, maxEntriesPerPerson = 1L) {
  connectionDetails <- Eunomia::getEunomiaConnectionDetails(
    databaseFile = tempfile(fileext = ".sqlite")
  )
  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  personIds <- seq_len(nPersons)
  entries <- withr::with_seed(42, {
    nEntries <- sample.int(maxEntriesPerPerson, nPersons, replace = TRUE)
    subjectIds <- rep(personIds, times = nEntries)
    startDates <- as.Date("2005-01-01") +
      sample.int(5844L, length(subjectIds), replace = TRUE) - 1L
    data.frame(subject_id = subjectIds, cohort_start_date = startDates)
  })
  entries <- unique(entries)
  entries$cohort_definition_id <- 1L
  entries$cohort_end_date <- entries$cohort_start_date + 30L
  # Everyone has an outcome, because getPlpData stops if a sample has none
  outcomes <- entries
  outcomes$cohort_definition_id <- 2L

  replaceTable <- function(tableName, data) {
    DatabaseConnector::insertTable(
      connection = connection,
      databaseSchema = "main",
      tableName = tableName,
      data = data,
      dropTableIfExists = TRUE,
      createTable = TRUE,
      progressBar = FALSE
    )
  }
  replaceTable("person", data.frame(
    person_id = personIds,
    gender_concept_id = 8507L,
    year_of_birth = 1960L,
    race_concept_id = 0L,
    ethnicity_concept_id = 0L
  ))
  replaceTable("observation_period", data.frame(
    observation_period_id = personIds,
    person_id = personIds,
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2022-01-01"),
    period_type_concept_id = 0L
  ))
  replaceTable("cohort", rbind(entries, outcomes))

  list(
    entries = entries,
    databaseDetails = createDatabaseDetails(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = "main",
      cdmDatabaseName = "main",
      cohortDatabaseSchema = "main",
      cohortTable = "cohort",
      outcomeDatabaseSchema = "main",
      outcomeTable = "cohort",
      targetId = 1,
      outcomeIds = 2,
      cdmVersion = 5
    )
  )
}

extractCohorts <- function(databaseDetails, ...) {
  plpData <- getPlpData(
    databaseDetails = databaseDetails,
    covariateSettings = ageCovariateSettings(),
    restrictPlpDataSettings = createRestrictPlpDataSettings(...)
  )
  plpData$cohorts
}

topYearShare <- function(cohorts) {
  years <- format(as.Date(cohorts$cohortStartDate), "%Y")
  max(table(years)) / length(years)
}

test_that("sampleSize is representative of the target cohort", {
  skip_if_offline()
  skip_if_not_installed("Eunomia")
  cdm <- createSamplingCdm()

  # The limits are well above what a random sample of this size would give.
  # Uniform years are 6.2% each and 2.4% of IDs are divisible by 41.
  small <- extractCohorts(cdm$databaseDetails, sampleSize = 600)
  expect_lt(topYearShare(small), 0.15)
  expect_lt(mean(small$subjectId %% 41 == 0), 0.06)
  days <- as.integer(format(as.Date(small$cohortStartDate), "%d"))
  expect_lt(mean(days %% 3 == 0), 0.42)

  large <- extractCohorts(cdm$databaseDetails, sampleSize = 5000)
  expect_lt(topYearShare(large), 0.085)
  expect_lt(mean(large$subjectId %% 41 == 0), 0.035)

  # Without 2009 there is no calendar year spike, but the ID bias remains
  recent <- extractCohorts(
    cdm$databaseDetails,
    studyStartDate = "20150101",
    studyEndDate = "20210101",
    sampleSize = 600
  )
  expect_lt(mean(recent$subjectId %% 41 == 0), 0.06)
})

test_that("sampleSize counts people and keeps all their entries", {
  skip_if_offline()
  skip_if_not_installed("Eunomia")
  cdm <- createSamplingCdm(nPersons = 5000L, maxEntriesPerPerson = 3L)

  sampled <- extractCohorts(cdm$databaseDetails, sampleSize = 200)

  sampledPersons <- unique(sampled$subjectId)
  expect_equal(length(sampledPersons), 200)
  expected <- cdm$entries[cdm$entries$subject_id %in% sampledPersons, ]
  expect_equal(nrow(sampled), nrow(expected))
})

test_that("sampleSize is deterministic and smaller samples are nested", {
  skip_if_offline()
  skip_if_not_installed("Eunomia")
  cdm <- createSamplingCdm(nPersons = 2000L)

  small <- extractCohorts(cdm$databaseDetails, sampleSize = 100)
  again <- extractCohorts(cdm$databaseDetails, sampleSize = 100)
  large <- extractCohorts(cdm$databaseDetails, sampleSize = 500)

  expect_setequal(small$subjectId, again$subjectId)
  expect_true(all(small$subjectId %in% large$subjectId))
})

# The database servers are reached through the OHDSI organization secrets. These
# tests only run in .github/workflows/database-tests.yaml, which is much faster
# than the full check because it skips R CMD check and tests/testthat/setup.R.
databaseServers <- list(
  "oracle" = list(prefix = "CDM5_ORACLE", connection = "SERVER"),
  "postgresql" = list(prefix = "CDM5_POSTGRESQL", connection = "SERVER"),
  "sql server" = list(prefix = "CDM5_SQL_SERVER", connection = "SERVER"),
  "redshift" = list(prefix = "CDM5_REDSHIFT", connection = "SERVER"),
  "spark" = list(prefix = "CDM5_SPARK", connection = "CONNECTION_STRING"),
  "snowflake" = list(
    prefix = "CDM_SNOWFLAKE",
    connection = "CONNECTION_STRING",
    cdmSchema = "CDM53_SCHEMA"
  ),
  "iris" = list(prefix = "CDM_IRIS", connection = "CONNECTION_STRING"),
  "bigquery" = list(prefix = "CDM_BIG_QUERY", connection = "CONNECTION_STRING"),
  "duckdb" = list(local = TRUE)
)

serverSetting <- function(dbms, name) {
  Sys.getenv(paste(databaseServers[[dbms]]$prefix, name, sep = "_"))
}

serverCdmSchema <- function(dbms) {
  schema <- databaseServers[[dbms]]$cdmSchema
  serverSetting(dbms, if (is.null(schema)) "CDM_SCHEMA" else schema)
}

serverIsAvailable <- function(dbms) {
  if (Sys.getenv("PLP_TEST_DATABASES") != "true") {
    return(FALSE)
  }
  server <- databaseServers[[dbms]]
  if (isTRUE(server$local)) {
    return(requireNamespace(dbms, quietly = TRUE))
  }
  credentials <- if (dbms == "bigquery") "KEY_FILE" else c("USER", "PASSWORD")
  settings <- c(server$connection, "OHDSI_SCHEMA", credentials)
  nzchar(serverCdmSchema(dbms)) &&
    all(vapply(settings, function(name) nzchar(serverSetting(dbms, name)), TRUE))
}

createServerConnectionDetails <- function(dbms) {
  if (isTRUE(databaseServers[[dbms]]$local)) {
    return(Eunomia::getEunomiaConnectionDetails(
      databaseFile = tempfile(fileext = ".duckdb"),
      dbms = dbms
    ))
  }
  DatabaseConnector::downloadJdbcDrivers(dbms)
  if (dbms == "bigquery") {
    keyFile <- tempfile(fileext = ".json")
    writeLines(serverSetting(dbms, "KEY_FILE"), keyFile)
    connectionString <- gsub(
      "<keyfile path>",
      normalizePath(keyFile, winslash = "/"),
      serverSetting(dbms, "CONNECTION_STRING"),
      fixed = TRUE
    )
    return(DatabaseConnector::createConnectionDetails(
      dbms = dbms,
      user = "",
      password = "",
      connectionString = !!connectionString
    ))
  }
  user <- serverSetting(dbms, "USER")
  password <- URLdecode(serverSetting(dbms, "PASSWORD"))
  if (databaseServers[[dbms]]$connection == "SERVER") {
    server <- serverSetting(dbms, "SERVER")
    return(DatabaseConnector::createConnectionDetails(
      dbms = dbms,
      user = !!user,
      password = !!password,
      server = !!server
    ))
  }
  connectionString <- serverSetting(dbms, "CONNECTION_STRING")
  DatabaseConnector::createConnectionDetails(
    dbms = dbms,
    user = !!user,
    password = !!password,
    connectionString = !!connectionString
  )
}

for (serverDbms in names(databaseServers)) {
  test_that(paste("sampleSize runs on", serverDbms), {
    dbms <- serverDbms
    skip_if_not(serverIsAvailable(dbms), paste(dbms, "is not configured"))
    isLocal <- isTRUE(databaseServers[[dbms]]$local)
    cdmSchema <- if (isLocal) "main" else serverCdmSchema(dbms)
    ohdsiSchema <- if (isLocal) "main" else serverSetting(dbms, "OHDSI_SCHEMA")

    connectionDetails <- createServerConnectionDetails(dbms)
    connection <- DatabaseConnector::connect(connectionDetails)
    # Other jobs may run at the same time on the same server
    cohortTable <- paste0(
      "plp_ss_", substr(.Platform$OS.type, 1, 3),
      format(Sys.time(), "%m%d%H%M%S"), sample.int(99L, 1L)
    )
    withr::defer({
      DatabaseConnector::renderTranslateExecuteSql(
        connection,
        "DROP TABLE @schema.@table;",
        schema = ohdsiSchema,
        table = cohortTable,
        tempEmulationSchema = ohdsiSchema,
        progressBar = FALSE,
        reportOverallTime = FALSE
      )
      DatabaseConnector::disconnect(connection)
    })
    DatabaseConnector::renderTranslateExecuteSql(
      connection,
      "SELECT cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
      INTO @schema.@table
      FROM (
        SELECT 1 AS cohort_definition_id,
          person_id AS subject_id,
          observation_period_start_date AS cohort_start_date,
          observation_period_end_date AS cohort_end_date
        FROM @cdm_schema.observation_period
        UNION ALL
        SELECT 2 AS cohort_definition_id,
          person_id AS subject_id,
          observation_period_start_date AS cohort_start_date,
          observation_period_end_date AS cohort_end_date
        FROM @cdm_schema.observation_period
      ) entries;",
      schema = ohdsiSchema,
      table = cohortTable,
      cdm_schema = cdmSchema,
      tempEmulationSchema = ohdsiSchema,
      progressBar = FALSE,
      reportOverallTime = FALSE
    )
    databaseDetails <- createDatabaseDetails(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmSchema,
      cdmDatabaseName = dbms,
      tempEmulationSchema = ohdsiSchema,
      cohortDatabaseSchema = ohdsiSchema,
      cohortTable = cohortTable,
      outcomeDatabaseSchema = ohdsiSchema,
      outcomeTable = cohortTable,
      targetId = 1,
      outcomeIds = 2,
      cdmVersion = 5
    )

    # Without sampleSize as a control: a failure below is then due to sampling
    allCohorts <- extractCohorts(databaseDetails, firstExposureOnly = TRUE)
    expect_gt(nrow(allCohorts), 100)

    sampled <- extractCohorts(
      databaseDetails,
      firstExposureOnly = TRUE,
      sampleSize = 100
    )
    expect_equal(nrow(sampled), 100)
    expect_equal(length(unique(sampled$subjectId)), 100)
  })
}
