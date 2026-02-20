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

# only run this during CI in main repo
if (Sys.getenv("CI") == "true" &&
  Sys.getenv("GITHUB_REPOSITORY") == "OHDSI/PatientLevelPrediction") {
  cdmDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
  ohdsiDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_OHDSI_SCHEMA")
  cdmDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
  ohdsiDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_OHDSI_SCHEMA")
  connectionRedshift <- DatabaseConnector::createConnectionDetails(
    dbms = "postgresql",
    user = Sys.getenv("CDM5_POSTGRESQL_USER"),
    password = URLdecode(Sys.getenv("CDM5_POSTGRESQL_PASSWORD")),
    server = Sys.getenv("CDM5_POSTGRESQL_SERVER"),
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
  skip_if(Sys.getenv("CI") != "true", "only run on CI")
  skip_if(Sys.getenv("GITHUB_REPOSITORY") != "OHDSI/PatientLevelPrediction", "not run in fork")
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
  skip_if(Sys.getenv("CI") != "true", "only run on CI")
  skip_if(Sys.getenv("GITHUB_REPOSITORY") != "OHDSI/PatientLevelPrediction", "not run in fork")
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
  skip_if(Sys.getenv("CI") != "true", "only run on CI")
  skip_if(Sys.getenv("GITHUB_REPOSITORY") != "OHDSI/PatientLevelPrediction", "not run in fork")
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
  sql <- "select count(*) as N from @resultSchema.@appendperformances;"
  sql <- SqlRender::render(sql, resultSchema = ohdsiDatabaseSchema, append = appendRandom("test_"))
  res <- DatabaseConnector::querySql(conn, sql, snakeCaseToCamelCase = TRUE)
  expect_true(res$n[1] > 0)

  # add test: check model location has result?
})

test_that("database deletion", {
  skip_if(Sys.getenv("CI") != "true", "only run on CI")
  skip_if(Sys.getenv("GITHUB_REPOSITORY") != "OHDSI/PatientLevelPrediction", "not run in fork")
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
if (Sys.getenv("CI") == "true" && Sys.getenv("GITHUB_REPOSITORY") == "OHDSI/PatientLevelPrediction") {
  DatabaseConnector::disconnect(conn)
}

# code to test sqlite creation, result and diagnostic upload all in one
test_that("temporary sqlite with results works", {
  skip_if_not_installed(c("ResultModelManager", "Eunomia"))
  skip_if_offline()

  resultsLoc <- file.path(saveLoc, "sqliteTest")

  savePlpResult(plpResult, file.path(resultsLoc, "Analysis_1", "plpResult"))
  # save diagnostic
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
  sql <- "select count(*) as N from main.performances;"
  res <- DatabaseConnector::querySql(conn, sql)
  expect_true(res$N[1] > 0)

  # check the diagnostic table is populated
  sql <- "select count(*) as N from main.diagnostics;"
  res <- DatabaseConnector::querySql(conn, sql)
  expect_true(res$N[1] > 0)

  # check the models table is populated and intercept is stored correctly
  sql <- paste(
    "select",
    "  model_type as modelType,",
    "  intercept as intercept,",
    "  plp_model_file as plpModelFile,",
    "  train_details as trainDetails",
    "from main.models;"
  )
  res <- DatabaseConnector::querySql(conn, sql)
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
  skip_if_not_installed(c("ResultModelManager", "Eunomia"))
  skip_if_offline()

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

  res <- DatabaseConnector::querySql(conn, "select train_details as trainDetails from main.models;")
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

test_that("insertModelDesignInDatabase handles missing hyperparameterSettings in runPlp model", {
  skip_if_not_installed(c("ResultModelManager", "Eunomia"))
  skip_if_offline()

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

  countRes <- DatabaseConnector::querySql(conn, "select count(*) as n from main.model_designs;")
  expect_true(countRes$n[1] > 0)
})

# SQL lite test
test_that("temporary sqlite with results works", {
  skip_if_not_installed(c("ResultModelManager", "Eunomia"))
  skip_if_offline()
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
  sql <- "select count(*) as N from main.performances;"
  res <- DatabaseConnector::querySql(conn, sql)
  expect_true(res$N[1] > 0)

  # models table exists and has expected intercept
  sql <- "select intercept as intercept, plp_model_file as plpModelFile from main.models;"
  res <- DatabaseConnector::querySql(conn, sql)
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
  skip_if_not_installed(c("ResultModelManager", "Eunomia"))
  skip_if_offline()

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

  res <- DatabaseConnector::querySql(conn, "select count(*) as N from main.performances;")
  expect_gte(res$N[1], 2)

  expect_true(DatabaseConnector::existsTable(
    connection = conn,
    databaseSchema = "main",
    tableName = "evaluation_statistics"
  ))

  perf <- DatabaseConnector::querySql(conn, "select max(performance_id) as performanceId from main.performances;")
  evalRows <- DatabaseConnector::querySql(
    conn,
    paste0(
      "select count(*) as N from main.evaluation_statistics where performance_id = ",
      perf$performanceId[1],
      ";"
    )
  )
  expect_gt(evalRows$N[1], 0)
})

# importFromCsv test here as can use previous csv saving
test_that("import from csv", {
  # TODO remove dependancy on previous test
  skip_if_not_installed(c("ResultModelManager", "Eunomia"))
  skip_if_offline()
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
  skip_if_not_installed(c("ResultModelManager", "Eunomia"))
  skip_if_offline()
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
