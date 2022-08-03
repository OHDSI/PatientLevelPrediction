# Copyright 2021 Observational Health Data Sciences and Informatics
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

library("testthat")

context("UploadToDatabase")

cdmDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
ohdsiDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_OHDSI_SCHEMA")
connectionRedshift <- DatabaseConnector::createConnectionDetails(
  dbms = "postgresql",
  user = Sys.getenv("CDM5_POSTGRESQL_USER"),
  password = URLdecode(Sys.getenv("CDM5_POSTGRESQL_PASSWORD")),
  server = Sys.getenv("CDM5_POSTGRESQL_SERVER"), 
  )
conn <- DatabaseConnector::connect(connectionRedshift)
targetDialect <- 'postgresql'

randVar <- rawToChar(as.raw(sample(c(65:90,97:122), 5, replace=T)))
appendRandom <- function(x, rand = randVar){
  return(paste(rand, x, sep=''))
}


test_that("test createDatabaseSchemaSettings works", {
  
  databaseSchemaSettings <- createDatabaseSchemaSettings(
    resultSchema = ohdsiDatabaseSchema, 
    stringAppendToResultSchemaTables = '',
    targetDialect = targetDialect
  )
  
  # check inputs as expected
  testthat::expect_true(databaseSchemaSettings$resultSchema == ohdsiDatabaseSchema)
  testthat::expect_true(databaseSchemaSettings$stringAppendToResultSchemaTables == '')
  testthat::expect_true(databaseSchemaSettings$targetDialect == targetDialect)
  testthat::expect_true(databaseSchemaSettings$cohortDefinitionSchema == ohdsiDatabaseSchema)
  testthat::expect_true(databaseSchemaSettings$databaseDefinitionSchema == ohdsiDatabaseSchema)
  testthat::expect_true(databaseSchemaSettings$stringAppendToCohortDefinitionTables == '')
  testthat::expect_true(databaseSchemaSettings$stringAppendToDatabaseDefinitionTables == '')
  
  databaseSchemaSettings <- createDatabaseSchemaSettings(
    resultSchema = ohdsiDatabaseSchema, 
    stringAppendToResultSchemaTables = '',
    targetDialect = targetDialect,
    cohortDefinitionSchema = 'test 123',
    stringAppendToCohortDefinitionTables = 'a',
    databaseDefinitionSchema = 'test234',
    stringAppendToDatabaseDefinitionTables = 'b'
  )
  
  testthat::expect_true(databaseSchemaSettings$cohortDefinitionSchema == 'test 123')
  testthat::expect_true(databaseSchemaSettings$databaseDefinitionSchema == 'test234')
  testthat::expect_true(databaseSchemaSettings$stringAppendToCohortDefinitionTables == 'A_')
  testthat::expect_true(databaseSchemaSettings$stringAppendToDatabaseDefinitionTables == 'B_')
  
  
  testthat::expect_true(class(databaseSchemaSettings) == 'plpDatabaseResultSchema')
  
}
)


test_that("test createDatabaseDetails works", {
  
  databaseList <- createDatabaseList(
    cdmDatabaseSchemas = paste0('database', 1:5)
  )
  
  testthat::expect_true(length(databaseList) == length(paste0('database', 1:5)))
  testthat::expect_true(class(databaseList) == 'list')
  
}
)

test_that("test getDatabaseDetail works", {
  
  databaseList <- createDatabaseList(
    cdmDatabaseSchemas = paste0('database', 1:5)
  )
  
  databaseValue <- getDatabaseDetail(
    databaseList = databaseList ,
    databaseSchema = 'database3'
  )
  testthat::expect_true(class(databaseValue) == 'list')
  testthat::expect_true(databaseValue$name == 'database3')
  
  databaseValue <- getDatabaseDetail(
    databaseList = databaseList ,
    databaseSchema = 'none'
  )
  testthat::expect_true(class(databaseValue) == 'list')
  testthat::expect_true(databaseValue$name == 'none')
  
}
)

test_that("getCohortDefinitionFromDefinitions", {
  
  cohortDefinitions <- list(list(name = 'blank1', id = 1, cohort_json = 'bla'),
                            list(name = 'blank2', id = 2, cohort_json = 'bla'),
                            list(name = 'blank3', id = 3, cohort_json = 'bla'))
  
  res <-  getCohortDefinitionJson(cohortDefinitions, cohortId =  2)
  
  testthat::expect_true(res$name == 'blank2')
}
)


test_that("database creation", {
  
  createPlpResultTables(
    conn = conn, 
    resultSchema = ohdsiDatabaseSchema, 
    targetDialect = targetDialect,
    deleteTables = T, 
    createTables = T,
    stringAppendToTables = appendRandom('test')
  )
  
  tableNames <- DatabaseConnector::getTableNames(connection = conn, databaseSchema = ohdsiDatabaseSchema)
  # check the results table is created
  testthat::expect_true(paste0(toupper(appendRandom('test')),'_PERFORMANCES') %in% tableNames)

})


test_that("results uploaded to database", {
  
  resultsLoc <- file.path(saveLoc,'dbUp')
  
  plpResult$model$trainDetails$developmentDatabase <- 'test' 
  savePlpResult(plpResult, file.path(resultsLoc, 'Analysis_1','plpResult'))
  # save validation
  if(!dir.exists(file.path(resultsLoc,'Validation','test', 'Analysis_1'))){
    dir.create(file.path(resultsLoc,'Validation','test', 'Analysis_1'), recursive = T)
  }
  plpResult$model$validationDetails <- list(
    targetId = 1, 
    outcomeId = 3,
    developmentDatabase = 'test',
    validationDatabase = 'test',
    populationSettings = plpResult$model$modelDesign$populationSettings, 
    restrictPlpDataSettings = plpResult$model$modelDesign$restrictPlpDataSettings
    )
  savePlpResult(plpResult, file.path(resultsLoc,'Validation','test', 'Analysis_1', 'validationResult'))
  
  # add results:
  addMultipleRunPlpToDatabase(
    conn = conn, 
    databaseSchemaSettings = createDatabaseSchemaSettings(
      resultSchema = ohdsiDatabaseSchema, 
      stringAppendToResultSchemaTables = appendRandom('test'),
      targetDialect = targetDialect
    ), 
    cohortDefinitions = list(list(name = 'blank1', id = 1, cohort_json = 'bla'),
                             list(name = 'blank2', id = 2, cohort_json = 'bla'),
                             list(name = 'blank3', id = 3, cohort_json = 'bla')),
    databaseList = createDatabaseList(
      cdmDatabaseSchema = c('test'), 
      acronym = c('test'), 
      version = c(1),
      description = c(1),
      type = c('claims')
    ),
    resultLocation = resultsLoc
  )
  
  # check the results table is populated
  sql <- 'select count(*) as N from @resultSchema.@appendperformances;'
  sql <- SqlRender::render(sql, resultSchema = ohdsiDatabaseSchema, append = appendRandom('test_'))
  res <- DatabaseConnector::querySql(conn, sql)
  testthat::expect_true(res$N[1]>0)

})


test_that("database deletion", {

  createPlpResultTables(
    conn = conn, 
    resultSchema = ohdsiDatabaseSchema, 
    targetDialect = targetDialect,
    deleteTables = T, 
    createTables = F,
    stringAppendToTables = appendRandom('test')
  )
  
  tableNames <- DatabaseConnector::getTableNames(connection = conn, databaseSchema = ohdsiDatabaseSchema)
  # check the results table is then deleted
  testthat::expect_false(paste0(toupper(appendRandom('test')),'_PERFORMANCES') %in% tableNames)
  
  
})

# disconnect
DatabaseConnector::disconnect(conn)

# code to test sqlite creation, result and diagnostic upload all in one
test_that("temporary sqlite with results works", {
  
  resultsLoc <- file.path(saveLoc,'sqliteTest')
  
  savePlpResult(plpResult, file.path(resultsLoc, 'Analysis_1','plpResult'))
  # save diagnostic
  saveRDS(diagnoseResult, file.path(resultsLoc,'Analysis_1','diagnosePlp.rds'))
  
  sqliteLocation <- insertResultsToSqlite(
    resultLocation = resultsLoc, 
    cohortDefinitions = list(list(name = 'blank1', id = 1, cohort_json = 'bla'),
                             list(name = 'blank2', id = 2, cohort_json = 'bla'),
                             list(name = 'blank3', id = 3, cohort_json = 'bla')),
    databaseList = createDatabaseList(
      cdmDatabaseSchema = c('test'), 
      acronym = c('test'), 
      version = c(1),
      description = c(1),
      type = c('claims')
    ),
    sqliteLocation = file.path(resultsLoc, 'sqlite')
  )
  
  # expect the database to exist
  testthat::expect_true(file.exists(sqliteLocation))
  
  cdmDatabaseSchema <- 'main'
  ohdsiDatabaseSchema <- 'main'
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = 'sqlite',
    server = sqliteLocation
  )
  conn <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  targetDialect <- 'sqlite'
  
  # check the results table is populated
  sql <- 'select count(*) as N from main.performances;'
  res <- DatabaseConnector::querySql(conn, sql)
  testthat::expect_true(res$N[1]>0)
  
  # check the diagnostic table is populated
  sql <- 'select count(*) as N from main.diagnostics;'
  res <- DatabaseConnector::querySql(conn, sql)
  testthat::expect_true(res$N[1]>0)
  
  # disconnect
  DatabaseConnector::disconnect(conn)

})

# SQL lite test
test_that("temporary sqlite with results works", {
  
  externalVal <- plpResult
  externalVal$model$model <- 'none'
  externalVal$model$trainDetails <- NULL
  externalVal$model$validationDetails <- list(
    targetId = 1, 
    outcomeId = 3,
    developmentDatabase = 'test',
    validationDatabase = 'test',
    populationSettings = plpResult$model$modelDesign$populationSettings, 
    restrictPlpDataSettings = plpResult$model$modelDesign$restrictPlpDataSettings
  )
  
sqliteLocation <- insertRunPlpToSqlite(
  runPlp = plpResult, 
  externalValidatePlp = NULL
  )

# expect the database to exist
testthat::expect_true(file.exists(sqliteLocation))

cdmDatabaseSchema <- 'main'
ohdsiDatabaseSchema <- 'main'
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = 'sqlite',
  server = sqliteLocation
)
conn <- DatabaseConnector::connect(connectionDetails = connectionDetails)
targetDialect <- 'sqlite'

# check the results table is populated
sql <- 'select count(*) as N from main.performances;'
res <- DatabaseConnector::querySql(conn, sql)
testthat::expect_true(res$N[1]>0)


# check export to csv
extractDatabaseToCsv(
  conn = conn,
  databaseSchemaSettings = createDatabaseSchemaSettings(resultSchema = 'main'),
  csvFolder = file.path(saveLoc, 'csvFolder')
)

testthat::expect_true(dir.exists(file.path(saveLoc, 'csvFolder')))
testthat::expect_true(length(dir(file.path(saveLoc, 'csvFolder'))) > 0 )

# disconnect
DatabaseConnector::disconnect(conn)


})
  



