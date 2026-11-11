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

runPostgresUploadTests <- Sys.getenv("CI") == "true" &&
  Sys.getenv("GITHUB_REPOSITORY") == "OHDSI/PatientLevelPrediction" &&
  all(nzchar(Sys.getenv(c(
    "CDM5_POSTGRESQL_CDM_SCHEMA",
    "CDM5_POSTGRESQL_OHDSI_SCHEMA",
    "CDM5_POSTGRESQL_PASSWORD",
    "CDM5_POSTGRESQL_SERVER",
    "CDM5_POSTGRESQL_USER"
  )))) &&
  grepl("/", Sys.getenv("CDM5_POSTGRESQL_SERVER"), fixed = TRUE)

skip_if_no_postgres_upload <- function() {
  skip_if(!runPostgresUploadTests, "PostgreSQL test connection is not configured")
}

# only run this during CI in main repo when PostgreSQL secrets are available
if (runPostgresUploadTests) {
  cdmDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
  ohdsiDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_OHDSI_SCHEMA")
  connectionRedshift <- DatabaseConnector::createConnectionDetails(
    dbms = "postgresql",
    user = Sys.getenv("CDM5_POSTGRESQL_USER"),
    password = URLdecode(Sys.getenv("CDM5_POSTGRESQL_PASSWORD")),
    server = Sys.getenv("CDM5_POSTGRESQL_SERVER")
  )
  conn <- DatabaseConnector::connect(connectionRedshift)
  targetDialect <- "postgresql"

  set.seed(NULL)
  randVar <- rawToChar(as.raw(sample(c(65:90, 97:122), 5, replace = TRUE)))

  appendRandom <- function(x, rand = randVar) {
    return(paste("plp", rand, x, sep = ""))
  }
}
test_that("test createDatabaseSchemaSettings works", {
  skip_if_no_postgres_upload()
  databaseSchemaSettings <- createDatabaseSchemaSettings(
    resultSchema = ohdsiDatabaseSchema,
    tablePrefix = "",
    targetDialect = targetDialect
  )

  # check inputs as expected
  expect_true(databaseSchemaSettings$resultSchema == ohdsiDatabaseSchema)
  expect_true(databaseSchemaSettings$tablePrefix == "")
  expect_true(databaseSchemaSettings$targetDialect == targetDialect)
  expect_true(databaseSchemaSettings$cohortDefinitionSchema == ohdsiDatabaseSchema)
  expect_true(databaseSchemaSettings$databaseDefinitionSchema == ohdsiDatabaseSchema)
  expect_true(databaseSchemaSettings$tablePrefixCohortDefinitionTables == "")
  expect_true(databaseSchemaSettings$tablePrefixDatabaseDefinitionTables == "")

  databaseSchemaSettings <- createDatabaseSchemaSettings(
    resultSchema = ohdsiDatabaseSchema,
    tablePrefix = "",
    targetDialect = targetDialect,
    cohortDefinitionSchema = "test 123",
    tablePrefixCohortDefinitionTables = "a",
    databaseDefinitionSchema = "test234",
    tablePrefixDatabaseDefinitionTables = "b"
  )

  expect_true(databaseSchemaSettings$cohortDefinitionSchema == "test 123")
  expect_true(databaseSchemaSettings$databaseDefinitionSchema == "test234")
  expect_true(databaseSchemaSettings$tablePrefixCohortDefinitionTables == "A_")
  expect_true(databaseSchemaSettings$tablePrefixDatabaseDefinitionTables == "B_")
  expect_true(class(databaseSchemaSettings) == "plpDatabaseResultSchema")
})


test_that("test createDatabaseDetails works", {
  databaseList <- createDatabaseList(
    cdmDatabaseSchemas = paste0("database", 1:5)
  )

  expect_true(length(databaseList) == length(paste0("database", 1:5)))
  expect_true(class(databaseList) == "list")
  expect_true(!is.null(databaseList$database1$databaseDetails))
  expect_true(!is.null(databaseList$database1$databaseMetaData))

  expect_equal(
    databaseList$database1$databaseDetails$databaseMetaDataId,
    databaseList$database1$databaseMetaData$databaseId
  )
})


test_that("database creation", {
  skip_if_no_postgres_upload()
  createPlpResultTables(
    connectionDetails = connectionRedshift,
    resultSchema = ohdsiDatabaseSchema,
    targetDialect = targetDialect,
    deleteTables = TRUE,
    createTables = TRUE,
    tablePrefix = appendRandom("test")
  )

  # check the results table is created
  expect_true(DatabaseConnector::existsTable(
    connection = conn,
    databaseSchema = ohdsiDatabaseSchema,
    tableName = paste0(appendRandom("test"), "_PERFORMANCES")
  ))
})


test_that("results uploaded to database", {
  skip_if_no_postgres_upload()
  resultsLoc <- file.path(saveLoc, "dbUp")

  plpResult$model$trainDetails$developmentDatabase <- "test"
  savePlpResult(plpResult, file.path(resultsLoc, "Analysis_1", "plpResult"))
  # save validation
  if (!dir.exists(file.path(resultsLoc, "Validation", "test", "Analysis_1"))) {
    dir.create(file.path(resultsLoc, "Validation", "test", "Analysis_1"), recursive = TRUE)
  }
  plpResult$model$validationDetails <- list(
    targetId = 1,
    outcomeId = outcomeId,
    developmentDatabase = "test",
    validationDatabase = "test",
    populationSettings = plpResult$model$modelDesign$populationSettings,
    restrictPlpDataSettings = plpResult$model$modelDesign$restrictPlpDataSettings
  )
  savePlpResult(plpResult, file.path(resultsLoc, "Validation", "test", "Analysis_1", "validationResult"))

  # add results:
  addMultipleRunPlpToDatabase(
    connectionDetails = connectionRedshift,
    databaseSchemaSettings = createDatabaseSchemaSettings(
      resultSchema = ohdsiDatabaseSchema,
      tablePrefix = appendRandom("test"),
      targetDialect = targetDialect
    ),
    cohortDefinitions = data.frame(
      cohortName = c("blank1", "blank2", "blank3"),
      cohortId = c(1, 2, 3),
      json = rep("bla", 3)
    ),
    databaseList = createDatabaseList(
      cdmDatabaseSchemas = c("test")
    ),
    resultLocation = resultsLoc,
    modelSaveLocation = file.path(saveLoc, "modelLocation") # new
  )

  # check the results table is populated
  sql <- "select count(*) as n from @resultSchema.@appendperformances;"
  sql <- SqlRender::render(sql, resultSchema = ohdsiDatabaseSchema, append = appendRandom("test_"))
  res <- DatabaseConnector::querySql(conn, sql, snakeCaseToCamelCase = TRUE)
  expect_true(res$n[1] > 0)

  # add test: check model location has result?
})

test_that("database deletion", {
  skip_if_no_postgres_upload()
  createPlpResultTables(
    connectionDetails = connectionRedshift,
    resultSchema = ohdsiDatabaseSchema,
    targetDialect = targetDialect,
    deleteTables = TRUE,
    createTables = FALSE,
    tablePrefix = appendRandom("test")
  )

  # check the results table is then deleted
  expect_false(DatabaseConnector::existsTable(
    connection = conn,
    databaseSchema = ohdsiDatabaseSchema,
    tableName = paste0(appendRandom("test"), "_PERFORMANCES")
  ))
  
  expect_false(DatabaseConnector::existsTable(
    connection = conn,
    databaseSchema = ohdsiDatabaseSchema,
    tableName = paste0(appendRandom("test"), "_migration")
  ))
  
  expect_false(DatabaseConnector::existsTable(
    connection = conn,
    databaseSchema = ohdsiDatabaseSchema,
    tableName = paste0(appendRandom("test"), "_package_version")
  ))
})

# disconnect
if (runPostgresUploadTests) {
  DatabaseConnector::disconnect(conn)
}

# code to test sqlite creation, result and diagnostic upload all in one
test_that("temporary sqlite with results works", {
  skip_if_not_installed("ResultModelManager")
  skip_if_not_installed("Eunomia")
  skip_if_no_eunomia()

  resultsLoc <- file.path(saveLoc, "sqliteTest")

  savePlpResult(plpResult, file.path(resultsLoc, "Analysis_1", "plpResult"))
  # save diagnostic
  diagnoseResult <- diagnosePlp(
    plpData = plpData,
    outcomeId = outcomeId,
    analysisId = "Test",
    populationSettings = populationSettings,
    splitSettings = createDefaultSplitSetting(splitSeed = 12),
    saveDirectory = file.path(saveLoc, "diagnose"),
    modelSettings = setLassoLogisticRegression(seed = 42),
    logSettings = createLogSettings(verbosity = "ERROR"),
    preprocessSettings = createPreprocessSettings(),
    sampleSettings = NULL,
    featureEngineeringSettings = NULL
  )
  saveRDS(diagnoseResult, file.path(resultsLoc, "Analysis_1", "diagnosePlp.rds"))

  sqliteLocation <- insertResultsToSqlite(
    resultLocation = resultsLoc,
    cohortDefinitions = data.frame(
      cohortName = c("blank1", "blank2", "blank3"),
      cohortId = c(1, 2, 3),
      json = rep("bla", 3)
    ),
    databaseList = createDatabaseList(
      cdmDatabaseSchemas = c("test")
    ),
    sqliteLocation = file.path(resultsLoc, "sqlite")
  )

  # expect the database to exist
  expect_true(file.exists(sqliteLocation))

  cdmDatabaseSchema <- "main"
  ohdsiDatabaseSchema <- "main"
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "sqlite",
    server = sqliteLocation
  )
  conn <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  targetDialect <- "sqlite"

  # check the results table is populated
  sql <- "select count(*) as n from main.performances;"
  res <- DatabaseConnector::querySql(conn, sql, snakeCaseToCamelCase = TRUE)
  expect_true(res$n[1] > 0)

  # check the diagnostic table is populated
  sql <- "select count(*) as n from main.diagnostics;"
  res <- DatabaseConnector::querySql(conn, sql, snakeCaseToCamelCase = TRUE)
  expect_true(res$n[1] > 0)

  # check the models table is populated and intercept is stored correctly
  sql <- paste(
    "select",
    "  model_type as model_type,",
    "  intercept as intercept,",
    "  plp_model_file as plp_model_file,",
    "  train_details as train_details",
    "from main.models;"
  )
  res <- DatabaseConnector::querySql(conn, sql, snakeCaseToCamelCase = TRUE)
  expect_true(nrow(res) > 0)
  expect_true(all(!is.na(res$plpModelFile)))
  expect_true(dir.exists(res$plpModelFile[1]))
  expect_true(length(list.files(res$plpModelFile[1], all.files = TRUE, recursive = TRUE)) > 0)
  expect_true(nzchar(res$modelType[1]))
  expect_true(nzchar(res$trainDetails[1]))
  trainDetails <- ParallelLogger::convertJsonToSettings(res$trainDetails[1])
  expect_true("hyperParamSearch" %in% names(trainDetails))
  expect_true("hyperparameterSettings" %in% names(trainDetails))
  expect_equal(trainDetails$hyperparameterSettings$search, "grid")
  expect_equal(trainDetails$hyperparameterSettings$tuningMetric$name, "AUC")

  expectedIntercept <- PatientLevelPrediction:::getModelIntercept(plpResult$model)
  expect_equal(res$intercept[1], expectedIntercept, tolerance = 1e-4)

  # disconnect
  DatabaseConnector::disconnect(conn)
})

test_that("list hyperParamSearch is flattened for sqlite uploads", {
  skip_if_not_installed("ResultModelManager")
  skip_if_not_installed("Eunomia")
  skip_if_no_eunomia()

  runPlpListHyper <- plpResult
  runPlpListHyper$model$trainDetails$hyperParamSearch <- list(
    list(
      metric = "AUC",
      param = list(numIterations = 30, learningRate = 0.1),
      cvPerformance = 0.60,
      cvPerformancePerFold = c(0.59, 0.61),
      hyperSummary = data.frame(
        metric = "AUC",
        fold = c("CV", "Fold1", "Fold2"),
        value = c(0.60, 0.59, 0.61),
        numIterations = c(30, 30, 30),
        learningRate = c(0.1, 0.1, 0.1),
        stringsAsFactors = FALSE
      )
    ),
    list(
      metric = "AUC",
      param = list(numIterations = 50, learningRate = 0.05),
      cvPerformance = 0.58,
      cvPerformancePerFold = c(0.57, 0.59),
      hyperSummary = data.frame(
        metric = "AUC",
        fold = c("CV", "Fold1", "Fold2"),
        value = c(0.58, 0.57, 0.59),
        numIterations = c(50, 50, 50),
        learningRate = c(0.05, 0.05, 0.05),
        stringsAsFactors = FALSE
      )
    )
  )

  sqliteLocation <- insertRunPlpToSqlite(
    runPlp = runPlpListHyper,
    externalValidatePlp = NULL
  )
  expect_true(file.exists(sqliteLocation))

  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "sqlite",
    server = sqliteLocation
  )
  conn <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn), add = TRUE)

  res <- DatabaseConnector::querySql(
    conn,
    "select train_details as train_details from main.models;",
    snakeCaseToCamelCase = TRUE
  )
  expect_true(nrow(res) > 0)
  trainDetails <- ParallelLogger::convertJsonToSettings(res$trainDetails[1])

  expect_true("hyperParamSearch" %in% names(trainDetails))
  expect_true(is.data.frame(trainDetails$hyperParamSearch))
  expect_true(nrow(trainDetails$hyperParamSearch) >= 6)
  expect_true(all(c("metric", "fold", "value") %in% colnames(trainDetails$hyperParamSearch)))
})

test_that("normalizeHyperParamSearchForDatabase handles all supported shapes", {
  baseDf <- data.frame(
    metric = "AUC",
    fold = "CV",
    value = 0.7,
    stringsAsFactors = FALSE
  )

  normalizedNull <- PatientLevelPrediction:::normalizeHyperParamSearchForDatabase(NULL)
  expect_true(is.data.frame(normalizedNull))
  expect_equal(nrow(normalizedNull), 0)

  normalizedDf <- PatientLevelPrediction:::normalizeHyperParamSearchForDatabase(baseDf)
  expect_equal(normalizedDf, baseDf)

  normalizedFromSummary <- PatientLevelPrediction:::normalizeHyperParamSearchForDatabase(
    list(
      list(hyperSummary = baseDf),
      list(hyperSummary = transform(baseDf, fold = "Fold1", value = 0.69))
    )
  )
  expect_true(is.data.frame(normalizedFromSummary))
  expect_equal(nrow(normalizedFromSummary), 2)
  expect_true(all(c("metric", "fold", "value") %in% colnames(normalizedFromSummary)))

  normalizedFromDirect <- PatientLevelPrediction:::normalizeHyperParamSearchForDatabase(
    list(
      baseDf,
      transform(baseDf, fold = "Fold2", value = 0.68)
    )
  )
  expect_true(is.data.frame(normalizedFromDirect))
  expect_equal(nrow(normalizedFromDirect), 2)
  expect_true(all(c("CV", "Fold2") %in% normalizedFromDirect$fold))

  normalizedUnknown <- PatientLevelPrediction:::normalizeHyperParamSearchForDatabase(
    list(list(metric = "AUC", value = 0.7))
  )
  expect_true(is.data.frame(normalizedUnknown))
  expect_equal(nrow(normalizedUnknown), 0)
})

test_that("sanitizeHyperparameterSettingsForDatabase stores metric metadata only", {
  capturedData <- seq_len(10000)
  metric <- createTuningMetric(
    fun = function(prediction) mean(prediction$value) + length(capturedData),
    maximize = FALSE,
    name = "CustomLargeClosure",
    funArgs = list(cutoff = 0.75)
  )
  settings <- createHyperparameterSettings(
    tuningMetric = metric,
    generator = function(definition, expanded, settings) expanded[1]
  )

  sanitized <- PatientLevelPrediction:::sanitizeHyperparameterSettingsForDatabase(settings)
  json <- as.character(ParallelLogger::convertSettingsToJson(sanitized))

  expect_equal(sanitized$search, "custom")
  expect_equal(sanitized$tuningMetric$name, "CustomLargeClosure")
  expect_false(sanitized$tuningMetric$maximize)
  expect_equal(sanitized$tuningMetric$funArgs$cutoff, 0.75)
  expect_null(sanitized$tuningMetric[["fun", exact = TRUE]])
  expect_equal(sanitized$generator$type, "function")
  expect_lt(nchar(json), 1000)
})

test_that("sanitizeHyperparameterSettingsForDatabase handles optional and non-json fields", {
  env <- new.env(parent = emptyenv())
  env$value <- 1
  object <- structure(list(x = 1), class = "customThing")
  generator <- list(
    initialize = function(definition, settings) invisible(NULL),
    getNext = function(history) NULL
  )

  expect_null(PatientLevelPrediction:::sanitizeObjectForDatabaseJson(NULL))
  expect_equal(
    PatientLevelPrediction:::sanitizeObjectForDatabaseJson(function() NULL),
    list(type = "function")
  )
  expect_equal(
    PatientLevelPrediction:::sanitizeObjectForDatabaseJson(env),
    list(type = "environment")
  )
  expect_equal(
    PatientLevelPrediction:::sanitizeObjectForDatabaseJson(data.frame(
      keep = 1,
      drop = I(list(function() NULL))
    )),
    data.frame(
      keep = 1,
      type = "function"
    )
  )
  expect_equal(PatientLevelPrediction:::sanitizeObjectForDatabaseJson(object), list(x = 1))
  expect_equal(
    PatientLevelPrediction:::sanitizeObjectForDatabaseJson(stats::as.formula("~ x")),
    c("~", "x")
  )
  expect_null(PatientLevelPrediction:::sanitizeTuningMetricForDatabase(NULL))
  expect_null(PatientLevelPrediction:::sanitizeGeneratorForDatabase(NULL))
  expect_equal(
    PatientLevelPrediction:::sanitizeGeneratorForDatabase(generator),
    list(type = "list", methods = c("initialize", "getNext"))
  )
  expect_null(PatientLevelPrediction:::sanitizeHyperparameterSettingsForDatabase(NULL))
})

test_that("insertModelDesignInDatabase handles missing hyperparameterSettings in runPlp model", {
  skip_if_not_installed("ResultModelManager")
  skip_if_not_installed("Eunomia")
  skip_if_no_eunomia()

  sqliteLocation <- file.path(tempdir(), "issue620-modeldesign-fallback.sqlite")
  if (file.exists(sqliteLocation)) {
    file.remove(sqliteLocation)
  }
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "sqlite",
    server = sqliteLocation
  )

  createPlpResultTables(
    connectionDetails = connectionDetails,
    targetDialect = "sqlite",
    resultSchema = "main",
    deleteTables = TRUE,
    createTables = TRUE,
    tablePrefix = ""
  )

  conn <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn), add = TRUE)

  runPlpNoHyper <- plpResult
  runPlpNoHyper$model$modelDesign$hyperparameterSettings <- NULL

  modelDesignId <- insertModelDesignInDatabase(
    object = runPlpNoHyper,
    conn = conn,
    databaseSchemaSettings = createDatabaseSchemaSettings(resultSchema = "main"),
    cohortDefinitions = data.frame(
      cohortName = c("blank1", "blank2", "blank3"),
      cohortId = c(1, 2, 3),
      json = rep("bla", 3)
    )
  )

  expect_false(is.null(modelDesignId))
  expect_true(is.numeric(modelDesignId) || is.integer(modelDesignId))

  countRes <- DatabaseConnector::querySql(
    conn,
    "select count(*) as n from main.model_designs;",
    snakeCaseToCamelCase = TRUE
  )
  expect_true(countRes$n[1] > 0)
})

# SQL lite test
test_that("temporary sqlite with results works", {
  skip_if_not_installed("ResultModelManager")
  skip_if_not_installed("Eunomia")
  skip_if_no_eunomia()
  externalVal <- plpResult
  externalVal$model$model <- "none"
  externalVal$model$trainDetails <- NULL
  externalVal$model$validationDetails <- list(
    targetId = 1,
    outcomeId = 3,
    developmentDatabase = "test",
    validationDatabase = "test",
    populationSettings = plpResult$model$modelDesign$populationSettings,
    restrictPlpDataSettings = plpResult$model$modelDesign$restrictPlpDataSettings
  )

  sqliteLocation <- insertRunPlpToSqlite(
    runPlp = plpResult,
    externalValidatePlp = NULL
  )

  # expect the database to exist
  expect_true(file.exists(sqliteLocation))

  cdmDatabaseSchema <- "main"
  ohdsiDatabaseSchema <- "main"
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "sqlite",
    server = sqliteLocation
  )
  conn <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  targetDialect <- "sqlite"

  # check the results table is populated
  sql <- "select count(*) as n from main.performances;"
  res <- DatabaseConnector::querySql(conn, sql, snakeCaseToCamelCase = TRUE)
  expect_true(res$n[1] > 0)

  # models table exists and has expected intercept
  sql <- "select intercept as intercept, plp_model_file as plp_model_file from main.models;"
  res <- DatabaseConnector::querySql(conn, sql, snakeCaseToCamelCase = TRUE)
  expect_true(nrow(res) > 0)
  expect_true(dir.exists(res$plpModelFile[1]))
  expect_equal(
    res$intercept[1],
    PatientLevelPrediction:::getModelIntercept(plpResult$model),
    tolerance = 1e-4
  )


  # check export to csv
  extractDatabaseToCsv(
    connectionDetails = connectionDetails,
    databaseSchemaSettings = createDatabaseSchemaSettings(resultSchema = "main"),
    csvFolder = file.path(saveLoc, "csvFolder")
  )

  expect_true(dir.exists(file.path(saveLoc, "csvFolder")))
  expect_true(length(dir(file.path(saveLoc, "csvFolder"))) > 0)
  expect_true(dir.exists(file.path(saveLoc, "csvFolder", "models"))) # new
  expect_true(length(dir(file.path(saveLoc, "csvFolder", "models"))) > 0) # new
  # disconnect
  DatabaseConnector::disconnect(conn)
})

test_that("external validation with no outcomes uploads to sqlite", {
  skip_if_not_installed("ResultModelManager")
  skip_if_not_installed("Eunomia")
  skip_if_no_eunomia()

  validationPopulation <- population
  validationPopulation$outcomeCount <- 0

  externalVal <- externalValidatePlp(
    plpModel = plpResult$model,
    plpData = plpData,
    population = validationPopulation,
    settings = createValidationSettings(
      recalibrate = NULL,
      runCovariateSummary = FALSE
    )
  )

  sqliteLocation <- insertRunPlpToSqlite(
    runPlp = plpResult,
    externalValidatePlp = list(externalVal),
    diagnosePlp = NULL
  )

  expect_true(file.exists(sqliteLocation))

  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "sqlite",
    server = sqliteLocation
  )
  conn <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn), add = TRUE)

  res <- DatabaseConnector::querySql(
    conn,
    "select count(*) as n from main.performances;",
    snakeCaseToCamelCase = TRUE
  )
  expect_gte(res$n[1], 2)

  expect_true(DatabaseConnector::existsTable(
    connection = conn,
    databaseSchema = "main",
    tableName = "evaluation_statistics"
  ))

  perf <- DatabaseConnector::querySql(
    conn,
    "select max(performance_id) as performance_id from main.performances;",
    snakeCaseToCamelCase = TRUE
  )
  evalRows <- DatabaseConnector::querySql(
    conn,
    paste0(
      "select count(*) as n from main.evaluation_statistics where performance_id = ",
      perf$performanceId[1],
      ";"
    ),
    snakeCaseToCamelCase = TRUE
  )
  expect_gt(evalRows$n[1], 0)
})

# importFromCsv test here as can use previous csv saving
test_that("import from csv", {
  # TODO remove dependancy on previous test
  skip_if_not_installed("ResultModelManager")
  skip_if_not_installed("Eunomia")
  skip_if_no_eunomia()
  cohortDef <- extractCohortDefinitionsCSV(
    csvFolder = file.path(saveLoc, "csvFolder")
  )
  expect_true(inherits(cohortDef, "data.frame"))
  expect_true(ncol(cohortDef) == 4)

  databaseList <- extractDatabaseListCSV(
    csvFolder = file.path(saveLoc, "csvFolder")
  )
  expect_true(inherits(databaseList, "list"))
  expect_true(!is.null(databaseList[[1]]$databaseDetails))
  expect_true(!is.null(databaseList[[1]]$databaseMetaData))

  # model designs work
  modeldesignsRow <- data.frame(
    target_id = 1, outcome_id = 2, population_setting_id = 1,
    plp_data_setting_id = 1, model_setting_id = 1,
    covariate_setting_id = 1, sample_setting_id = 1,
    split_setting_id = 1, feature_engineering_setting_id = 1,
    tidy_covariates_setting_id = 1
  )
  res <- getModelDesignSettingTable(modeldesignsRow)
  # expect res to be a data.frame, check values?
  expect_true(inherits(res, "data.frame"))

  modelDesign <- getModelDesignCsv(
    modelDesignSettingTable = res,
    csvFolder = file.path(saveLoc, "csvFolder")
  )
  expect_true(inherits(modelDesign, "modelDesign"))

  # performance works
  res <- getPerformanceEvaluationCsv(
    performanceId = 1,
    csvFolder = file.path(saveLoc, "csvFolder")
  )
  expect_true(inherits(res, "list"))
  expect_true(
    sum(names(res) %in%
      c(
        "evaluationStatistics", "thresholdSummary",
        "calibrationSummary", "demographicSummary",
        "predictionDistribution"
      )) == 5
  )


  # test object extracts
  obj <- extractObjectFromCsv(
    performanceId = 1,
    csvFolder = file.path(saveLoc, "csvFolder")
  )
  expect_true(inherits(obj, "externalValidatePlp") | inherits(obj, "runPlp"))

  # test diagnostic extracted
  diag <- extractDiagnosticFromCsv(
    diagnosticId = 1,
    csvFolder = file.path(saveLoc, "csvFolder")
  )
  expect_true(inherits(diag, "diagnosePlp") | is.null(diag))



  # Testing everything together
  csvServerLoc <- file.path(tempdir(), "newCsvDatabase")
  if (!dir.exists(file.path(tempdir(), "newCsvDatabase"))) {
    dir.create(file.path(tempdir(), "newCsvDatabase"), recursive = TRUE)
  }
  newResultConnDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "sqlite",
    server = file.path(csvServerLoc, "newCsv.sqlite")
  )
  newResultConn <- DatabaseConnector::connect(newResultConnDetails)
  csvDatabaseSchemaSettings <- PatientLevelPrediction::createDatabaseSchemaSettings(
    resultSchema = "main",
    tablePrefix = "",
    targetDialect = "sqlite",
    tempEmulationSchema = NULL
  )

  # create empty tables to insert csv into
  PatientLevelPrediction::createPlpResultTables(
    connectionDetails = newResultConnDetails,
    targetDialect = "sqlite",
    resultSchema = "main",
    createTables = TRUE,
    deleteTables = TRUE,
    tablePrefix = "",
    tempEmulationSchema = NULL
  )

  res <- insertCsvToDatabase(
    csvFolder = file.path(saveLoc, "csvFolder"),
    connectionDetails = newResultConnDetails,
    databaseSchemaSettings = csvDatabaseSchemaSettings,
    modelSaveLocation = file.path(csvServerLoc, "models"),
    csvTableAppend = ""
  )
  expect_true(res)

  # check some of the tables
})


# new - check null model just reports message
test_that("message if model is null", {
  skip_if_not_installed("ResultModelManager")
  skip_if_not_installed("Eunomia")
  skip_if_no_eunomia()
  model2 <- list(noModel = TRUE)
  model2$modelDesign$modelSettings$settings$predict <- "noModel"
  model2$modelDesign$modelSettings$settings$saveType <- "RtoJson"
  class(model2) <- "plpModel"

  plpResult2 <- plpResult
  plpResult2$model <- model2

  savePlpResult(plpResult2, file.path(tempdir(), "null_model", "Analysis_1", "plpResult"))

  nullModelServerLoc <- file.path(tempdir(), "nullModelDatabase")
  if (!dir.exists(file.path(tempdir(), "nullModelDatabase"))) {
    dir.create(file.path(tempdir(), "nullModelDatabase"), recursive = TRUE)
  }
  nullModelResultConnDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "sqlite",
    server = file.path(nullModelServerLoc, "sqlite.sqlite")
  )
  nullModelDatabaseSchema <- createDatabaseSchemaSettings(
    resultSchema = "main",
    tablePrefix = "",
    targetDialect = "sqlite",
    tempEmulationSchema = NULL
  )

  createPlpResultTables(
    connectionDetails = nullModelResultConnDetails,
    targetDialect = "sqlite",
    resultSchema = "main",
    deleteTables = TRUE,
    createTables = TRUE,
    tablePrefix = ""
  )

  expect_message(
    addMultipleRunPlpToDatabase(
      connectionDetails = nullModelResultConnDetails,
      databaseSchemaSettings = nullModelDatabaseSchema,
      resultLocation = file.path(tempdir(), "null_model"),
      modelSaveLocation = file.path(tempdir(), "null_model", "models")
    )
  )
})


newUploadDb <- function(env = parent.frame()) {
  sqliteFile <- tempfile(fileext = ".sqlite")
  connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "sqlite", server = sqliteFile)
  createPlpResultTables(
    connectionDetails = connectionDetails,
    targetDialect = "sqlite",
    resultSchema = "main",
    deleteTables = TRUE,
    createTables = TRUE,
    tablePrefix = ""
  )
  withr::defer(unlink(sqliteFile), envir = env)
  connectionDetails
}

test_that("insertRunPlpToSqlite stores a single externalValidatePlp result", {
  skip_if_not_installed("ResultModelManager")
  skip_if_not_installed("Eunomia")
  skip_if_no_eunomia()
  externalVal <- plpResult
  externalVal$model$model <- "none"
  externalVal$model$trainDetails <- NULL
  externalVal$model$validationDetails <- list(
    targetId = 1,
    outcomeId = outcomeId,
    developmentDatabase = "test",
    validationDatabase = "test",
    populationSettings = plpResult$model$modelDesign$populationSettings,
    restrictPlpDataSettings = plpResult$model$modelDesign$restrictPlpDataSettings
  )
  class(externalVal) <- "externalValidatePlp"

  # insertRunPlpToSqlite reuses one file per session
  unlink(file.path(tempdir(), "databaseFile.sqlite"))
  sqliteLocation <- insertRunPlpToSqlite(runPlp = plpResult, externalValidatePlp = externalVal)
  conn <- DatabaseConnector::connect(
    DatabaseConnector::createConnectionDetails(dbms = "sqlite", server = sqliteLocation)
  )
  on.exit(DatabaseConnector::disconnect(conn), add = TRUE)
  res <- DatabaseConnector::querySql(
    conn,
    "select model_development as dev, count(*) as n from main.performances group by model_development;",
    snakeCaseToCamelCase = TRUE
  )
  expect_equal(res$n[res$dev == 1], 1)
  expect_equal(res$n[res$dev == 0], 1)
})

test_that("createPlpResultTables with deleteTables removes rows from complete tables", {
  skip_if_not_installed("ResultModelManager")
  connectionDetails <- newUploadDb()
  conn <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn), add = TRUE)
  DatabaseConnector::executeSql(conn, "insert into main.tars(tar_start_day, tar_start_anchor, tar_end_day, tar_end_anchor) values (1,'cohort start',365,'cohort start');")
  countTars <- function() DatabaseConnector::querySql(conn, "select count(*) as n from main.tars;", snakeCaseToCamelCase = TRUE)$n

  # control: deleteTables = FALSE keeps rows and migrates again
  createPlpResultTables(
    connectionDetails = connectionDetails, targetDialect = "sqlite", resultSchema = "main",
    deleteTables = FALSE, createTables = TRUE, tablePrefix = ""
  )
  expect_equal(countTars(), 1)

  createPlpResultTables(
    connectionDetails = connectionDetails, targetDialect = "sqlite", resultSchema = "main",
    deleteTables = TRUE, createTables = TRUE, tablePrefix = ""
  )
  expect_equal(countTars(), 0)

  # a reset must reapply migrations
  columns <- tolower(colnames(DatabaseConnector::querySql(conn, "select * from main.model_designs where 1 = 0;")))
  expect_true("hyperparameter_setting_id" %in% columns)
  migrations <- DatabaseConnector::querySql(conn, "select * from main.migration;", snakeCaseToCamelCase = TRUE)
  expect_gt(nrow(migrations), 0)
})

test_that("createPlpResultTables keeps existing results by default", {
  skip_if_not_installed("ResultModelManager")
  connectionDetails <- newUploadDb()
  conn <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn), add = TRUE)
  DatabaseConnector::executeSql(conn, "insert into main.tars(tar_start_day, tar_start_anchor, tar_end_day, tar_end_anchor) values (1,'cohort start',365,'cohort start');")

  createPlpResultTables(connectionDetails = connectionDetails, targetDialect = "sqlite", resultSchema = "main")
  n <- DatabaseConnector::querySql(conn, "select count(*) as n from main.tars;", snakeCaseToCamelCase = TRUE)$n
  expect_equal(n, 1)
})

test_that("createPlpResultTables explains how to recreate a schema with only some tables", {
  skip_if_not_installed("ResultModelManager")
  connectionDetails <- newUploadDb()
  conn <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn), add = TRUE)
  DatabaseConnector::executeSql(conn, "drop table main.tars;")

  expect_error(
    createPlpResultTables(connectionDetails = connectionDetails, targetDialect = "sqlite", resultSchema = "main"),
    "Use deleteTables = TRUE"
  )
  createPlpResultTables(
    connectionDetails = connectionDetails, targetDialect = "sqlite", resultSchema = "main",
    deleteTables = TRUE
  )
  expect_true("tars" %in% tolower(DatabaseConnector::getTableNames(conn, databaseSchema = "main")))
})

test_that("createPlpResultTables deleteTables without createTables removes all tables", {
  skip_if_not_installed("ResultModelManager")
  connectionDetails <- newUploadDb()
  createPlpResultTables(
    connectionDetails = connectionDetails, targetDialect = "sqlite", resultSchema = "main",
    deleteTables = TRUE, createTables = FALSE, tablePrefix = ""
  )
  conn <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn), add = TRUE)
  remaining <- tolower(DatabaseConnector::getTableNames(conn, databaseSchema = "main"))
  expect_false(any(c("performances", "models", "migration") %in% remaining))
})

test_that("createPlpResultTables deleteTables honours a table prefix", {
  skip_if_not_installed("ResultModelManager")
  sqliteFile <- tempfile(fileext = ".sqlite")
  on.exit(unlink(sqliteFile), add = TRUE)
  connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "sqlite", server = sqliteFile)
  args <- list(
    connectionDetails = connectionDetails, targetDialect = "sqlite", resultSchema = "main",
    deleteTables = TRUE, createTables = TRUE, tablePrefix = "test"
  )
  do.call(createPlpResultTables, args)
  conn <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn), add = TRUE, after = FALSE)
  DatabaseConnector::executeSql(conn, "insert into main.test_tars(tar_start_day, tar_start_anchor, tar_end_day, tar_end_anchor) values (1,'cohort start',365,'cohort start');")
  do.call(createPlpResultTables, args)
  res <- DatabaseConnector::querySql(conn, "select count(*) as n from main.test_tars;", snakeCaseToCamelCase = TRUE)
  expect_equal(res$n, 0)
})

test_that("insertResultsToSqlite replaces results by default and can add to them", {
  skip_if_not_installed("ResultModelManager")
  skip_if_not_installed("Eunomia")
  skip_if_no_eunomia()
  resultsLoc <- tempfile("insertResultsDeleteTables")
  on.exit(unlink(resultsLoc, recursive = TRUE), add = TRUE)
  savePlpResult(plpResult, file.path(resultsLoc, "Analysis_1", "plpResult"))
  insert <- function(...) {
    insertResultsToSqlite(
      resultLocation = resultsLoc,
      sqliteLocation = file.path(resultsLoc, "sqlite"),
      skipDiagnostics = TRUE,
      ...
    )
  }
  countMarkers <- function(db) {
    conn <- DatabaseConnector::connect(DatabaseConnector::createConnectionDetails(dbms = "sqlite", server = db))
    on.exit(DatabaseConnector::disconnect(conn))
    DatabaseConnector::querySql(conn, "select count(*) as n from main.tars where tar_start_day = -999;", snakeCaseToCamelCase = TRUE)$n
  }
  addMarker <- function(db) {
    conn <- DatabaseConnector::connect(DatabaseConnector::createConnectionDetails(dbms = "sqlite", server = db))
    on.exit(DatabaseConnector::disconnect(conn))
    DatabaseConnector::executeSql(conn, "insert into main.tars(tar_start_day, tar_start_anchor, tar_end_day, tar_end_anchor) values (-999,'cohort start',365,'cohort start');")
  }

  db <- insert()
  addMarker(db)
  insert(deleteTables = FALSE)
  expect_equal(countMarkers(db), 1)

  insert()
  expect_equal(countMarkers(db), 0)
})

test_that("addModel replaces the models row of a re-uploaded fit", {
  skip_if_not_installed("ResultModelManager")
  connectionDetails <- newUploadDb()
  conn <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn), add = TRUE)
  upload <- function(intercept, trainDetails, day) {
    PatientLevelPrediction:::addModel(
      conn = conn, resultSchema = "main", targetDialect = "sqlite", tablePrefix = "",
      analysisId = "Analysis_1", modelDesignId = 1, databaseId = 1, modelType = "lasso",
      plpModelFile = "folder-1-1", trainDetails = trainDetails, preprocessing = "",
      executionDateTime = day, trainingTime = "1 secs", intercept = intercept,
      tempEmulationSchema = NULL
    )
  }
  firstId <- upload(-1.5, '{"first": true}', "2026-01-01")
  secondId <- upload(-3.25, '{"first": false}', "2026-02-02")
  otherDatabase <- PatientLevelPrediction:::addModel(
    conn = conn, resultSchema = "main", targetDialect = "sqlite", tablePrefix = "",
    analysisId = "Analysis_2", modelDesignId = 1, databaseId = 2, modelType = "lasso",
    plpModelFile = "folder-1-2", trainDetails = "{}", preprocessing = "",
    executionDateTime = "2026-03-03", trainingTime = "1 secs", intercept = 9, tempEmulationSchema = NULL
  )
  expect_equal(firstId, secondId)
  expect_false(identical(firstId, otherDatabase))

  row <- DatabaseConnector::querySql(
    conn, "select * from main.models where database_id = 1;",
    snakeCaseToCamelCase = TRUE
  )
  expect_equal(nrow(row), 1)
  expect_equal(row$intercept, -3.25)
  expect_equal(row$trainDetails, '{"first": false}')
  expect_equal(as.character(row$executionDateTime), "2026-02-02")

  other <- DatabaseConnector::querySql(
    conn, "select * from main.models where database_id = 2;",
    snakeCaseToCamelCase = TRUE
  )
  expect_equal(other$intercept, 9)
})

test_that("addCohort keeps full cohort JSON on PostgreSQL and SQL Server style platforms", {
  skip_if_not_installed("ResultModelManager")
  connectionDetails <- newUploadDb()
  conn <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn), add = TRUE)
  prefix <- paste0('{"criteria":"', strrep("a", 4100))
  add <- function(id, tail, dialect) {
    PatientLevelPrediction:::addCohort(
      conn = conn, resultSchema = "main", targetDialect = dialect, cgTablePrefix = "",
      plpTablePrefix = "", tempEmulationSchema = NULL,
      cohortDefinition = data.frame(cohortId = id, cohortName = "shared", json = paste0(prefix, tail))
    )
  }
  first <- add(101, 'early"}', "postgresql")
  second <- add(202, 'late"}', "postgresql")
  expect_false(first == second)
  stored <- DatabaseConnector::querySql(conn, "select length(json) as len from main.cohort_definition order by cohort_definition_id;", snakeCaseToCamelCase = TRUE)
  expect_equal(stored$len, c(nchar(paste0(prefix, 'early"}')), nchar(paste0(prefix, 'late"}'))))

  # same definition reuses its cohort
  expect_equal(add(101, 'early"}', "postgresql"), first)
})

test_that("addCohort does not merge truncated definitions across cohort ids", {
  skip_if_not_installed("ResultModelManager")
  connectionDetails <- newUploadDb()
  conn <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn), add = TRUE)
  prefix <- paste0('{"criteria":"', strrep("a", 4100))
  add <- function(id, tail) {
    PatientLevelPrediction:::addCohort(
      conn = conn, resultSchema = "main", targetDialect = "oracle", cgTablePrefix = "",
      plpTablePrefix = "", tempEmulationSchema = NULL,
      cohortDefinition = data.frame(cohortId = id, cohortName = "shared", json = paste0(prefix, tail))
    )
  }
  first <- add(101, 'early"}')
  second <- add(202, 'late"}')
  expect_false(first == second)
  expect_equal(add(101, 'early"}'), first)
  stored <- DatabaseConnector::querySql(conn, "select length(json) as len from main.cohort_definition;", snakeCaseToCamelCase = TRUE)
  expect_true(all(stored$len == 4000))
})

test_that("addEvaluation clears a summary that has no replacement", {
  skip_if_not_installed("ResultModelManager")
  connectionDetails <- newUploadDb()
  conn <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn), add = TRUE)
  columns <- PatientLevelPrediction:::getColumnNames(conn, "main", "sqlite", tableName = "threshold_summary")
  threshold <- as.data.frame(lapply(setNames(columns, columns), function(x) 0.5))
  threshold$evaluation <- "Test"
  threshold$performanceId <- NULL
  upload <- function(thresholdSummary, auc) {
    PatientLevelPrediction:::addEvaluation(
      conn = conn, resultSchema = "main", targetDialect = "sqlite", tablePrefix = "", performanceId = 1,
      performanceEvaluation = list(
        thresholdSummary = thresholdSummary,
        evaluationStatistics = data.frame(evaluation = "Test", metric = "AUC", value = auc)
      ),
      overWriteIfExists = TRUE, tempEmulationSchema = NULL
    )
  }
  countRows <- function(id = 1) {
    DatabaseConnector::querySql(conn, sprintf("select count(*) as n from main.threshold_summary where performance_id = %s;", id), snakeCaseToCamelCase = TRUE)$n
  }

  upload(threshold, 0.7)
  expect_equal(countRows(), 1)

  # another performance is not touched
  PatientLevelPrediction:::addThresholdSummary(
    conn = conn, resultSchema = "main", targetDialect = "sqlite", tablePrefix = "", performanceId = 2,
    performanceEvaluation = list(thresholdSummary = threshold), overWriteIfExists = TRUE,
    tempEmulationSchema = NULL
  )

  upload(NULL, 0.6)
  expect_equal(countRows(), 0)
  expect_equal(countRows(id = 2), 1)
  auc <- DatabaseConnector::querySql(conn, "select value from main.evaluation_statistics where performance_id = 1;")
  expect_equal(as.numeric(auc[[1]]), 0.6)

  # keeping existing rows stays possible
  upload(threshold, 0.7)
  PatientLevelPrediction:::addThresholdSummary(
    conn = conn, resultSchema = "main", targetDialect = "sqlite", tablePrefix = "", performanceId = 1,
    performanceEvaluation = list(thresholdSummary = NULL), overWriteIfExists = FALSE,
    tempEmulationSchema = NULL
  )
  expect_equal(countRows(), 1)
})

test_that("insertPerformanceInDatabase clears attrition and covariate summary when absent", {
  skip_if_not_installed("ResultModelManager")
  connectionDetails <- newUploadDb()
  conn <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn), add = TRUE)
  schema <- createDatabaseSchemaSettings(resultSchema = "main", targetDialect = "sqlite", tempEmulationSchema = NULL)
  attritionCols <- PatientLevelPrediction:::getColumnNames(conn, "main", "sqlite", tableName = "attrition")
  covariateCols <- PatientLevelPrediction:::getColumnNames(conn, "main", "sqlite", tableName = "covariate_summary")
  attrition <- as.data.frame(lapply(setNames(attritionCols, attritionCols), function(x) 1))
  attrition$performanceId <- NULL
  attrition$description <- "start"
  covariateSummary <- as.data.frame(lapply(setNames(covariateCols, covariateCols), function(x) 1))
  covariateSummary$performanceId <- NULL
  covariateSummary$covariateValue <- 2

  upload <- function(attr, cov) {
    PatientLevelPrediction:::insertPerformanceInDatabase(
      performanceEvaluation = list(evaluationStatistics = data.frame(evaluation = "Test", metric = "AUC", value = 0.5)),
      covariateSummary = cov, attrition = attr,
      executionDateTime = "2026-01-01", plpVersion = "6.7.0", conn = conn,
      databaseSchemaSettings = schema, modelDesignId = 1, developmentDatabaseId = 1,
      validationDatabaseId = 1, validationTarId = 1, validationPopulationId = 1,
      validationPlpDataId = 1, validationTargetId = 1, validationOutcomeId = 1, modelDevelopment = 1
    )
  }
  count <- function(table) DatabaseConnector::querySql(conn, paste0("select count(*) as n from main.", table, ";"), snakeCaseToCamelCase = TRUE)$n

  upload(attrition, covariateSummary)
  expect_equal(count("attrition"), 1)
  expect_equal(count("covariate_summary"), 1)
  upload(NULL, NULL)
  expect_equal(count("attrition"), 0)
  expect_equal(count("covariate_summary"), 0)
})

test_that("addCalibrationSummary warns about calibration summaries it cannot store", {
  skip_if_not_installed("ResultModelManager")
  skip_if_not_installed("survival")
  connectionDetails <- newUploadDb()
  conn <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn), add = TRUE)
  upload <- function(calibration, performanceId) {
    capturedWarnings(PatientLevelPrediction:::addCalibrationSummary(
      conn = conn, resultSchema = "main", targetDialect = "sqlite", tablePrefix = "",
      performanceId = performanceId,
      performanceEvaluation = list(calibrationSummary = calibration), overWriteIfExists = TRUE,
      tempEmulationSchema = NULL
    ))
  }
  stored <- function(performanceId) {
    DatabaseConnector::querySql(conn, paste(
      "select count(*) as n from main.calibration_summary where performance_id =", performanceId
    ))[1, 1]
  }

  set.seed(1)
  prediction <- data.frame(
    rowId = 1:300, value = stats::runif(300, 0.01, 0.5),
    outcomeCount = stats::rbinom(300, 1, 0.3), survivalTime = stats::rexp(300, 0.002),
    evaluationType = "Test"
  )
  survivalCalibration <- PatientLevelPrediction:::getCalibrationSummary_survival(
    prediction = prediction, evalColumn = "evaluationType", numberOfStrata = 10,
    truncateFraction = 0.01, timepoint = 365
  )
  output <- upload(survivalCalibration, performanceId = 1)
  expect_true(any(grepl("Calibration summary for performance 1 was not uploaded", output)))
  expect_true(any(grepl("personCountWithOutcome", output)))
  expect_equal(stored(1), 0)

  binaryCalibration <- data.frame(
    evaluation = "Test", predictionThreshold = 0.1, PersonCountAtRisk = 10, PersonCountWithOutcome = 1,
    averagePredictedProbability = 0.1, StDevPredictedProbability = 0.1, MinPredictedProbability = 0.1,
    P25PredictedProbability = 0.1, MedianPredictedProbability = 0.1, P75PredictedProbability = 0.1,
    MaxPredictedProbability = 0.1, observedIncidence = 0.1
  )
  output <- upload(binaryCalibration, performanceId = 2)
  expect_false(any(grepl("was not uploaded", output)))
  expect_equal(stored(2), 1)
})

test_that("addCalibrationSummary stores survival calibration summaries", {
  testthat::skip("Storing survival calibration needs a results schema migration, see issue #691")
  skip_if_not_installed("ResultModelManager")
  skip_if_not_installed("survival")
  connectionDetails <- newUploadDb()
  conn <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn), add = TRUE)
  prediction <- data.frame(
    rowId = 1:300, value = stats::runif(300, 0.01, 0.5),
    outcomeCount = stats::rbinom(300, 1, 0.3), survivalTime = stats::rexp(300, 1 / 300) + 1,
    evaluationType = "Test"
  )
  calibration <- PatientLevelPrediction:::getCalibrationSummary_survival(
    prediction = prediction, evalColumn = "evaluationType", numberOfStrata = 10,
    truncateFraction = 0.01, timepoint = 365
  )
  PatientLevelPrediction:::addCalibrationSummary(
    conn = conn, resultSchema = "main", targetDialect = "sqlite", tablePrefix = "", performanceId = 1,
    performanceEvaluation = list(calibrationSummary = calibration), overWriteIfExists = TRUE,
    tempEmulationSchema = NULL
  )
  stored <- DatabaseConnector::querySql(conn, "select count(*) as n from main.calibration_summary;", snakeCaseToCamelCase = TRUE)$n
  expect_equal(stored, nrow(calibration))
})
