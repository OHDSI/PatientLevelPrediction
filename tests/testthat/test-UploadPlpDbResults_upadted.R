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

context("UploadPlpDbResults")

cdmDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
ohdsiDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_OHDSI_SCHEMA")
connectionRedshift <- DatabaseConnector::createConnectionDetails(
  dbms = "postgresql",
  user = Sys.getenv("CDM5_POSTGRESQL_USER"),
  password = URLdecode(Sys.getenv("CDM5_POSTGRESQL_PASSWORD")),
  server = Sys.getenv("CDM5_POSTGRESQL_SERVER")
  )
conn <- DatabaseConnector::connect(connectionRedshift)


randVar <- rawToChar(as.raw(sample(c(65:90,97:122), 5, replace=T)))
appendRandom <- function(x, rand = randVar){
  return(paste(rand, x, sep=''))
}


test_that("database creation", {

  createPlpResultTables(conn = conn, 
                        resultSchema = ohdsiDatabaseSchema, 
                        targetDialect = 'postgresql',
                        deleteExistingTables = T, 
                        createTables = T,
                        stringAppendToTables = appendRandom('test'))
  
  tableNames <- DatabaseConnector::getTableNames(connection = conn, databaseSchema = ohdsiDatabaseSchema)
  # check the results table is created
  testthat::expect_true(paste0(toupper(appendRandom('test')),'_RESULTS') %in% tableNames)

})


test_that("results uploaded to database", {
  
  resultsLoc <- file.path(saveLoc,'dbUp')
  
  savePlpResult(plpResult, file.path(resultsLoc, 'Analysis_1','plpResult'))
  # save validation
  if(!dir.exists(file.path(resultsLoc,'Validation','test', 'Analysis_1'))){
    dir.create(file.path(resultsLoc,'Validation','test', 'Analysis_1'), recursive = T)
  }
  plpResult$model$validationDetails <- list(
    cohortId = 1, 
    outcomeId = 3,
    populationSettings = plpResult$model$settings$populationSettings, 
    plpDataSettings = plpResult$model$settings$plpDataSettings
    )
  savePlpResult(plpResult, file.path(resultsLoc,'Validation','test', 'Analysis_1', 'validationResult'))

  # add results:
  populatePlpResultTables(conn = conn, 
                          resultSchema = ohdsiDatabaseSchema, 
                          stringAppendToTables = appendRandom('test'),
                          targetDialect = 'postgresql',
                          studyJsonList = list(list(cohort_name = 'blank1', cohort_id = 1, cohort_json = 'bla'),
                                               list(cohort_name = 'blank2', cohort_id = 2, cohort_json = 'bla'),
                                               list(cohort_name = 'blank3', cohort_id = 3, cohort_json = 'bla')),
                          studyName = 'test',
                          studyDescription = 'testing',
                          researcherName = 'jane doe',
                          researcherEmail = 'none',
                          researcherOrg = 'none',
                          databaseName = 'test',
                          databaseAcronym = 'test',
                          databaseVersion = 1,
                          databaseDescription = 'test',
                          databaseType = 'claims',
                          valDatabases = list(test = list(name = 'test', 
                                                          description = 'test',
                                                          version = 1,
                                                          type = 'claims')),
                          resultLocation = resultsLoc,
                          resultPattern = 'Analysis',
                          validationLocation = file.path(resultsLoc,'Validation'),
                          addInternalValidation = T,
                          addExternalValidation = T,
                          gsubVal = NULL,
                          removePattern = NULL
  )
  
  
  # check the results table is populated
  sql <- 'select count(*) as N from @resultSchema.@appendresults;'
  sql <- SqlRender::render(sql, resultSchema = ohdsiDatabaseSchema, append = appendRandom('test_'))
  res <- DatabaseConnector::querySql(conn, sql)
  testthat::expect_true(res$N[1]>0)

  
})


test_that("database deletion", {

  createPlpResultTables(conn = conn, 
                        resultSchema = ohdsiDatabaseSchema, 
                        targetDialect = 'postgresql',
                        deleteExistingTables = T, 
                        createTables = F,
                        stringAppendToTables = appendRandom('test'))
  
  tableNames <- DatabaseConnector::getTableNames(connection = conn, databaseSchema = ohdsiDatabaseSchema)
  # check the results table is then deleted
  testthat::expect_false(paste0(toupper(appendRandom('test')),'_RESULTS') %in% tableNames)
  
  
})

# disconnect
DatabaseConnector::disconnect(conn)
