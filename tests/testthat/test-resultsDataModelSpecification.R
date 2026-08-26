test_that("diagnostic predictor specification matches the SQLite schema", {
  skip_if_not_installed("RSQLite")

  specification <- utils::read.csv(system.file(
    "settings",
    "resultsDataModelSpecification.csv",
    package = "PatientLevelPrediction"
  ))
  expectedColumns <- specification$column_name[
    specification$table_name == "diagnostic_predictors"
  ]

  sqliteFile <- tempfile(fileext = ".sqlite")
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "sqlite",
    server = sqliteFile
  )
  createPlpResultTables(
    connectionDetails = connectionDetails,
    targetDialect = "sqlite",
    resultSchema = "main",
    deleteTables = TRUE,
    createTables = TRUE
  )
  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection), add = TRUE)

  actualColumns <- DatabaseConnector::querySql(
    connection,
    "pragma table_info(diagnostic_predictors);",
    snakeCaseToCamelCase = FALSE
  )$name

  expect_equal(expectedColumns, actualColumns)
  expect_true("probast_id" %in% expectedColumns)
})

test_that("diagnostic CSV import preserves predictor probast IDs", {
  skip_if_not_installed("readr")

  csvFolder <- tempfile("diagnostic-csv-")
  dir.create(csvFolder)
  writeCsv <- function(name, data) {
    readr::write_csv(data, file.path(csvFolder, paste0(name, ".csv")))
  }

  writeCsv("diagnostics", data.frame(
    diagnostic_id = 1,
    model_design_id = 2,
    database_id = 3,
    execution_date_time = "2026-08-26"
  ))
  writeCsv("model_designs", data.frame(model_design_id = 2))
  writeCsv("database_details", data.frame(
    database_id = 3,
    database_meta_data_id = "database-key"
  ))
  writeCsv("database_meta_data", data.frame(
    database_id = "database-key",
    cdm_source_name = "Test database"
  ))
  writeCsv("diagnostic_predictors", data.frame(
    diagnostic_id = 1,
    days_to_event = 30,
    outcome_at_time = 4,
    observed_at_start_of_day = 100,
    probast_id = "2.2.1",
    input_type = "Outcome"
  ))
  writeCsv("diagnostic_outcomes", data.frame(
    diagnostic_id = 1,
    xvalue = 30,
    outcome_percent = 0.04,
    aggregation = "month",
    probast_id = "3.1",
    input_type = "Outcome"
  ))
  writeCsv("diagnostic_participants", data.frame(
    diagnostic_id = 1,
    design = "Design",
    metric = "Count",
    value = 100,
    probast_id = "1.1"
  ))
  writeCsv("diagnostic_summary", data.frame(
    diagnostic_id = 1,
    probast_id = "2.2.1",
    result_value = "Pass"
  ))

  testthat::local_mocked_bindings(
    getModelDesignSettingTable = function(...) data.frame(),
    getModelDesignCsv = function(...) structure(list(), class = "modelDesign"),
    .package = "PatientLevelPrediction"
  )

  diagnostic <- extractDiagnosticFromCsv(1, csvFolder)

  expect_s3_class(diagnostic, "diagnosePlp")
  expect_equal(diagnostic$predictors$probastId, "2.2.1")
  expect_equal(diagnostic$databaseId, "database-key")
})
