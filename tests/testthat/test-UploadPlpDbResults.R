# Copyright 2020 Observational Health Data Sciences and Informatics
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
conn <- DatabaseConnector::connect(connectionDetails)


randVar <- rawToChar(as.raw(sample(c(65:90,97:122), 5, replace=T)))
appendRandom <- function(x, rand = randVar){
  return(paste(rand, x, sep='_'))
}


test_that("database creation", {

  createPlpResultTables(conn = conn, 
                        resultSchema = ohdsiDatabaseSchema, 
                        targetDialect = 'postgresql',
                        deleteExistingTables = T, 
                        createTables = T,
                        stringAppendToTables = appendRandom('test_'))
  
  tableNames <- DatabaseConnector::getTableNames(connection = conn, databaseSchema = ohdsiDatabaseSchema)
  DatabaseConnector::executeSql(conn, sql)
  # check the results table is created
  testthat::expect_true(paste0(toupper(appendRandom('test_')),'_RESULTS') %in% tableNames)

})


test_that("results uploaded to database", {
  
  resultsLoc <- file.path(saveLoc,'dbUp')
  
  # save main result
  savePlpResult(plpResult, file.path(resultsLoc, 'Analysis_1','plpResult'))
  # save validation
  saveRDS(plpResult, file.path(resultsLoc,'Validation','test', 'Analysis_1'))

  # add results:
  populatePlpResultTables(conn = conn, 
                          resultSchema = ohdsiDatabaseSchema, 
                          stringAppendToTables = appendRandom('test_'),
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
                          resultPattern = '',
                          validationLocation = file.path(resultLocation,'Validation'),
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
                        stringAppendToTables = appendRandom('test_'))
  
  tableNames <- DatabaseConnector::getTableNames(connection = conn, databaseSchema = ohdsiDatabaseSchema)
  DatabaseConnector::executeSql(conn, sql)
  # check the results table is then deleted
  testthat::expect_false(paste0(toupper(appendRandom('test_')),'_RESULTS') %in% tableNames)
  
  
})

# disconnect
DatabaseConnector::disconnect(conn)
