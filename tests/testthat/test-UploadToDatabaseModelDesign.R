test_that("addModelSetting handles minimal modelSettings objects", {
  skip_if_not_installed("RSQLite")

  sqliteFile <- file.path(tempdir(), "plp-model-setting-minimal.sqlite")
  if (file.exists(sqliteFile)) unlink(sqliteFile)

  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "sqlite",
    server = sqliteFile
  )

  PatientLevelPrediction::createPlpResultTables(
    connectionDetails = connectionDetails,
    targetDialect = "sqlite",
    resultSchema = "main",
    deleteTables = TRUE,
    createTables = TRUE,
    tablePrefix = "",
    tempEmulationSchema = NULL
  )

  conn <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn), add = TRUE)

  minimalModelSettings <- structure(list(model = "existingGlm"), class = "modelSettings")
  modelSettingId <- PatientLevelPrediction:::addModelSetting(
    conn = conn,
    resultSchema = "main",
    targetDialect = "sqlite",
    tablePrefix = "",
    json = minimalModelSettings,
    tempEmulationSchema = NULL
  )

  expect_true(!is.null(modelSettingId))
  expect_true(is.numeric(modelSettingId) || is.integer(modelSettingId))
  expect_true(is.finite(modelSettingId) && modelSettingId > 0)

  res <- DatabaseConnector::querySql(
    conn,
    "select model_type as modelType from main.model_settings;"
  )
  expect_true("modelType" %in% names(res))
  expect_true(any(res$modelType == "NULL"))
})

