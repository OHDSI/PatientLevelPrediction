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

library("testthat")

context("UploadToDatabase")

# only run this during CI in main repo
if (Sys.getenv("CI") == "true" && 
  Sys.getenv("GITHUB_REPOSITORY") == "OHDSI/PatientLevelPrediction") {
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
  skip_if(Sys.getenv("CI") != "true", "not run on CI")
  skip_if(Sys.getenv("GITHUB_REPOSITORY") != "ohdsi/PatientLevelPrediction", "not run in fork")
  databaseSchemaSettings <- createDatabaseSchemaSettings(
    resultSchema = ohdsiDatabaseSchema,
    tablePrefix = "",
    targetDialect = targetDialect
  )

  # check inputs as expected
  testthat::expect_true(databaseSchemaSettings$resultSchema == ohdsiDatabaseSchema)
  testthat::expect_true(databaseSchemaSettings$tablePrefix == "")
  testthat::expect_true(databaseSchemaSettings$targetDialect == targetDialect)
  testthat::expect_true(databaseSchemaSettings$cohortDefinitionSchema == ohdsiDatabaseSchema)
  testthat::expect_true(databaseSchemaSettings$databaseDefinitionSchema == ohdsiDatabaseSchema)
  testthat::expect_true(databaseSchemaSettings$tablePrefixCohortDefinitionTables == "")
  testthat::expect_true(databaseSchemaSettings$tablePrefixDatabaseDefinitionTables == "")

  databaseSchemaSettings <- createDatabaseSchemaSettings(
    resultSchema = ohdsiDatabaseSchema,
    tablePrefix = "",
    targetDialect = targetDialect,
    cohortDefinitionSchema = "test 123",
    tablePrefixCohortDefinitionTables = "a",
    databaseDefinitionSchema = "test234",
    tablePrefixDatabaseDefinitionTables = "b"
  )

  testthat::expect_true(databaseSchemaSettings$cohortDefinitionSchema == "test 123")
  testthat::expect_true(databaseSchemaSettings$databaseDefinitionSchema == "test234")
  testthat::expect_true(databaseSchemaSettings$tablePrefixCohortDefinitionTables == "A_")
  testthat::expect_true(databaseSchemaSettings$tablePrefixDatabaseDefinitionTables == "B_")


  testthat::expect_true(class(databaseSchemaSettings) == "plpDatabaseResultSchema")
})


test_that("test createDatabaseDetails works", {
  databaseList <- createDatabaseList(
    cdmDatabaseSchemas = paste0("database", 1:5)
  )

  testthat::expect_true(length(databaseList) == length(paste0("database", 1:5)))
  testthat::expect_true(class(databaseList) == "list")
  testthat::expect_true(!is.null(databaseList$database1$databaseDetails))
  testthat::expect_true(!is.null(databaseList$database1$databaseMetaData))

  testthat::expect_equal(
    databaseList$database1$databaseDetails$databaseMetaDataId,
    databaseList$database1$databaseMetaData$databaseId
  )
})


test_that("database creation", {
  skip_if(Sys.getenv("CI") != "true", "not run on CI")
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
  testthat::expect_true(DatabaseConnector::existsTable(
    connection = conn,
    databaseSchema = ohdsiDatabaseSchema,
    tableName = paste0(appendRandom("test"), "_PERFORMANCES")
  ))
})


test_that("results uploaded to database", {
  skip_if(Sys.getenv("CI") != "true", "not run on CI")
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
  res <- DatabaseConnector::querySql(conn, sql)
  testthat::expect_true(res$N[1] > 0)

  # add test: check model location has result?
})

test_that("database deletion", {
  skip_if(Sys.getenv("CI") != "true", "not run on CI")
  skip_if(Sys.getenv("GITHUB_REPOSITORY") != "ohdsi/PatientLevelPrediction", "not run in fork")
  createPlpResultTables(
    connectionDetails = connectionRedshift,
    resultSchema = ohdsiDatabaseSchema,
    targetDialect = targetDialect,
    deleteTables = TRUE,
    createTables = FALSE,
    tablePrefix = appendRandom("test")
  )

  # check the results table is then deleted
  testthat::expect_false(DatabaseConnector::existsTable(
    connection = conn,
    databaseSchema = ohdsiDatabaseSchema,
    tableName = paste0(appendRandom("test"), "_PERFORMANCES")
  ))
})

# disconnect
if (Sys.getenv("CI") == "true" && Sys.getenv("GITHUB_REPOSITORY") == "OHDSI/PatientLevelPrediction") {
  DatabaseConnector::disconnect(conn)
}

# code to test sqlite creation, result and diagnostic upload all in one
test_that("temporary sqlite with results works", {
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
  testthat::expect_true(file.exists(sqliteLocation))

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
  testthat::expect_true(res$N[1] > 0)

  # check the diagnostic table is populated
  sql <- "select count(*) as N from main.diagnostics;"
  res <- DatabaseConnector::querySql(conn, sql)
  testthat::expect_true(res$N[1] > 0)

  # disconnect
  DatabaseConnector::disconnect(conn)
})

# SQL lite test
test_that("temporary sqlite with results works", {
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
  testthat::expect_true(file.exists(sqliteLocation))

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
  testthat::expect_true(res$N[1] > 0)


  # check export to csv
  extractDatabaseToCsv(
    connectionDetails = connectionDetails,
    databaseSchemaSettings = createDatabaseSchemaSettings(resultSchema = "main"),
    csvFolder = file.path(saveLoc, "csvFolder")
  )

  testthat::expect_true(dir.exists(file.path(saveLoc, "csvFolder")))
  testthat::expect_true(length(dir(file.path(saveLoc, "csvFolder"))) > 0)
  testthat::expect_true(dir.exists(file.path(saveLoc, "csvFolder", "models"))) # new
  testthat::expect_true(length(dir(file.path(saveLoc, "csvFolder", "models"))) > 0) # new
  # disconnect
  DatabaseConnector::disconnect(conn)
})

# importFromCsv test here as can use previous csv saving
test_that("import from csv", {
  cohortDef <- extractCohortDefinitionsCSV(
    csvFolder = file.path(saveLoc, "csvFolder")
  )
  testthat::expect_true(inherits(cohortDef, "data.frame"))
  testthat::expect_true(ncol(cohortDef) == 4)

  databaseList <- extractDatabaseListCSV(
    csvFolder = file.path(saveLoc, "csvFolder")
  )
  testthat::expect_true(inherits(databaseList, "list"))
  testthat::expect_true(!is.null(databaseList[[1]]$databaseDetails))
  testthat::expect_true(!is.null(databaseList[[1]]$databaseMetaData))

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
  testthat::expect_true(inherits(res, "data.frame"))

  modelDesign <- getModelDesignCsv(
    modelDesignSettingTable = res,
    csvFolder = file.path(saveLoc, "csvFolder")
  )
  testthat::expect_true(inherits(modelDesign, "modelDesign"))

  # performance works
  res <- getPerformanceEvaluationCsv(
    performanceId = 1,
    csvFolder = file.path(saveLoc, "csvFolder")
  )
  testthat::expect_true(inherits(res, "list"))
  testthat::expect_true(
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
  testthat::expect_true(inherits(obj, "externalValidatePlp") | inherits(obj, "runPlp"))

  # test diagnostic extracted
  diag <- extractDiagnosticFromCsv(
    diagnosticId = 1,
    csvFolder = file.path(saveLoc, "csvFolder")
  )
  testthat::expect_true(inherits(diag, "diagnosePlp") | is.null(diag))



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
  testthat::expect_true(res)

  # check some of the tables
})


# new - check null model just reports message
test_that("message if model is null", {
  model2 <- list(noModel = TRUE)
  attr(model2, "predictionFunction") <- "noModel"
  attr(model2, "saveType") <- "RtoJson"
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

  testthat::expect_message(
    addMultipleRunPlpToDatabase(
      connectionDetails = nullModelResultConnDetails,
      databaseSchemaSettings = nullModelDatabaseSchema,
      resultLocation = file.path(tempdir(), "null_model"),
      modelSaveLocation = file.path(tempdir(), "null_model", "models")
    )
  )
})
