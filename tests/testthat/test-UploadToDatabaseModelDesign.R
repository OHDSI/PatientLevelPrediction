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

test_that("addHyperparameterSetting deduplicates identical settings json", {
  skip_if_not_installed("RSQLite")

  sqliteFile <- file.path(tempdir(), "plp-hyperparameter-setting.sqlite")
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

  hyperparameterSettings <- PatientLevelPrediction::createHyperparameterSettings(
    search = "grid"
  )

  rowCountBefore <- DatabaseConnector::querySql(
    conn,
    "select count(*) as n from main.hyperparameter_settings;"
  )

  firstId <- PatientLevelPrediction:::addHyperparameterSetting(
    conn = conn,
    resultSchema = "main",
    targetDialect = "sqlite",
    tablePrefix = "",
    json = hyperparameterSettings,
    tempEmulationSchema = NULL
  )
  secondId <- PatientLevelPrediction:::addHyperparameterSetting(
    conn = conn,
    resultSchema = "main",
    targetDialect = "sqlite",
    tablePrefix = "",
    json = hyperparameterSettings,
    tempEmulationSchema = NULL
  )

  expect_equal(firstId, secondId)

  rowCountAfter <- DatabaseConnector::querySql(
    conn,
    "select count(*) as n from main.hyperparameter_settings;"
  )
  expect_equal(rowCountAfter$n[1], rowCountBefore$n[1] + 1)
})

test_that("insertModelDesignInDatabase differentiates model_design_id by hyperparameter settings", {
  skip_if_not_installed("RSQLite")

  sqliteFile <- file.path(tempdir(), "plp-model-design-hyperparameter.sqlite")
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

  databaseSchemaSettings <- PatientLevelPrediction::createDatabaseSchemaSettings(
    resultSchema = "main",
    targetDialect = "sqlite",
    tempEmulationSchema = NULL
  )

  baseModelSettings <- PatientLevelPrediction::setLassoLogisticRegression(seed = 42)
  cohortDefinitions <- data.frame(
    cohortName = c("target", "outcome"),
    cohortId = c(1, 2),
    json = c("{}", "{}"),
    stringsAsFactors = FALSE
  )

  modelDesignGrid <- PatientLevelPrediction::createModelDesign(
    targetId = 1,
    outcomeId = 2,
    modelSettings = baseModelSettings,
    hyperparameterSettings = PatientLevelPrediction::createHyperparameterSettings(
      search = "grid"
    )
  )
  modelDesignRandom <- PatientLevelPrediction::createModelDesign(
    targetId = 1,
    outcomeId = 2,
    modelSettings = baseModelSettings,
    hyperparameterSettings = PatientLevelPrediction::createHyperparameterSettings(
      search = "random",
      sampleSize = 1,
      randomSeed = 42
    )
  )

  modelDesignGridId <- PatientLevelPrediction:::insertModelDesignInDatabase(
    object = modelDesignGrid,
    conn = conn,
    databaseSchemaSettings = databaseSchemaSettings,
    cohortDefinitions = cohortDefinitions
  )
  modelDesignRandomId <- PatientLevelPrediction:::insertModelDesignInDatabase(
    object = modelDesignRandom,
    conn = conn,
    databaseSchemaSettings = databaseSchemaSettings,
    cohortDefinitions = cohortDefinitions
  )
  modelDesignGridIdRepeat <- PatientLevelPrediction:::insertModelDesignInDatabase(
    object = modelDesignGrid,
    conn = conn,
    databaseSchemaSettings = databaseSchemaSettings,
    cohortDefinitions = cohortDefinitions
  )

  expect_equal(modelDesignGridId, modelDesignGridIdRepeat)
  expect_false(modelDesignGridId == modelDesignRandomId)

  modelDesignRows <- DatabaseConnector::querySql(
    conn,
    paste0(
      "select model_design_id as modelDesignId, ",
      "hyperparameter_setting_id as hyperparameterSettingId ",
      "from main.model_designs where model_design_id in (",
      modelDesignGridId, ", ", modelDesignRandomId, ");"
    )
  )
  expect_equal(length(unique(modelDesignRows$hyperparameterSettingId)), 2)

  hyperparameterRows <- DatabaseConnector::querySql(
    conn,
    "select count(*) as n from main.hyperparameter_settings;"
  )
  expect_gte(hyperparameterRows$n[1], 2)
})
