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
