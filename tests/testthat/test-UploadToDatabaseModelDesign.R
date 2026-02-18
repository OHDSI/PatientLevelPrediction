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

test_that("addModelSetting derives modelType from supported paths and sanitizes invalid values", {
  skip_if_not_installed("RSQLite")

  sqliteFile <- file.path(tempdir(), "plp-model-setting-branches.sqlite")
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

  settingsModelType <- list(
    settings = list(modelType = "binary"),
    model = "from-settings"
  )
  fallbackModelType <- list(
    settings = list(),
    param = structure(list(alpha = 0.1), settings = list(modelType = "survival")),
    model = "from-param-settings"
  )
  listModelType <- list(settings = list(modelType = list("bad")), model = "list-model-type")
  emptyModelType <- list(settings = list(modelType = character(0)), model = "empty-model-type")
  blankModelType <- list(settings = list(modelType = ""), model = "blank-model-type")
  naModelType <- list(settings = list(modelType = NA_character_), model = "na-model-type")
  charJson <- '{"settings":{"modelType":"ignored"}}'

  inputs <- list(
    settingsModelType,
    fallbackModelType,
    listModelType,
    emptyModelType,
    blankModelType,
    naModelType,
    charJson
  )
  expectedModelTypes <- c("binary", "survival", "NULL", "NULL", "NULL", "NULL", "NULL")

  for (i in seq_along(inputs)) {
    modelSettingId <- PatientLevelPrediction:::addModelSetting(
      conn = conn,
      resultSchema = "main",
      targetDialect = "sqlite",
      tablePrefix = "",
      json = inputs[[i]],
      tempEmulationSchema = NULL
    )

    res <- DatabaseConnector::querySql(
      conn,
      paste0(
        "select model_type as modelType from main.model_settings ",
        "where model_setting_id = ", modelSettingId, ";"
      )
    )
    expect_equal(res$modelType[1], expectedModelTypes[i])
  }
})
