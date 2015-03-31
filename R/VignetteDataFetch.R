# @file VignetteDataFetch.R
#
# Copyright 2014 Observational Health Data Sciences and Informatics
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

#' @keywords internal
.vignetteDataFetch <- function(){
  # This function should be used to fetch the data that is used in the vignettes.
  #library(SqlRender)
  #library(DatabaseConnector)
  #library(PatientLevelPrediction)
  setwd("s:/temp")
  
  options("fftempdir" = "s:/temp")
  
  pw <- NULL
  dbms <- "sql server"
  user <- NULL
  server <- "RNDUSRDHIT07.jnj.com"
  cdmDatabaseSchema <- "cdm_truven_mdcd.dbo"
  resultsDatabaseSchema <- "scratch.dbo"
  port <- NULL
  
  dbms = "postgresql"
  server = "localhost/enar"
  user = "postgres"
  password = pw
  cdmDatabaseSchema = "cdm_sim"
  resultsDatabaseSchema = "scratch"
  port <- NULL
  
  connectionDetails <- DatabaseConnector::createConnectionDetails(dbms=dbms, server = server, user = user, password = pw, port=port)
  sql <- SqlRender::loadRenderTranslateSql("HospitalizationCohorts.sql",
                                           packageName = "PatientLevelPrediction",
                                           dbms = dbms,
                                           cdmDatabaseSchema = cdmDatabaseSchema,
                                           resultsDatabaseSchema = resultsDatabaseSchema,
                                           post_time = 30,
                                           pre_time = 365)
  connection <- DatabaseConnector::connect(connectionDetails)
  DatabaseConnector::executeSql(connection, sql)
  
  # Check number of subjects per cohort:
  sql <- "SELECT cohort_concept_id, COUNT(*) AS count FROM @resultsDatabaseSchema.rehospitalization GROUP BY cohort_concept_id"
  sql <- SqlRender::renderSql(sql, resultsDatabaseSchema = resultsDatabaseSchema)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
  DatabaseConnector::querySql(connection, sql)
  
  cohortData <- getDbCohortData(connectionDetails, 
                                cdmDatabaseSchema = cdmDatabaseSchema, 
                                cohortDatabaseSchema = resultsDatabaseSchema, 
                                cohortTable = "rehospitalization", 
                                cohortConceptIds = 1)
  
  covariateSettings <- createCovariateSettings(useCovariateDemographics = TRUE,
                                               useCovariateConditionOccurrence = TRUE)
  covariateData <- getDbCovariateData(connectionDetails, 
                                      cdmDatabaseSchema = cdmDatabaseSchema, 
                                      cohortDatabaseSchema = resultsDatabaseSchema, 
                                      cohortTable = "rehospitalization", 
                                      cohortConceptIds = 1, 
                                      covariateSettings = covariateSettings)
  
  outcomeData <- getDbOutcomeData(connectionDetails, 
                                  cdmDatabaseSchema = cdmDatabaseSchema, 
                                  cohortDatabaseSchema = resultsDatabaseSchema, 
                                  cohortTable = "rehospitalization", 
                                  cohortConceptIds = 1,
                                  outcomeDatabaseSchema = resultsDatabaseSchema,
                                  outcomeTable = "rehospitalization",
                                  outcomeConceptIds = 2)
  
  saveCohortData(cohortData, "s:/temp/PatientLevelPrediction/cohortData")
  saveCovariateData(covariateData, "s:/temp/PatientLevelPrediction/covariateData")
  saveOutcomeData(outcomeData, "s:/temp/PatientLevelPrediction/outcomeData")
  
  #
  cohortData <- loadCohortData("s:/temp/PatientLevelPrediction/cohortData")
  covariateData <- loadCovariateData("s:/temp/PatientLevelPrediction/covariateData") 
  outcomeData <- loadOutcomeData("s:/temp/PatientLevelPrediction/outcomeData")
  
  parts <- splitData(cohortData, covariateData, outcomeData, c(0.75,0.25))

  saveCohortData(parts[[2]]$cohortData, "s:/temp/PatientLevelPrediction/cohortData_Test")
  saveCovariateData(parts[[2]]$covariateData, "s:/temp/PatientLevelPrediction/covariateData_Test")
  saveOutcomeData(parts[[2]]$outcomeData, "s:/temp/PatientLevelPrediction/outcomeData_Test") 
  
  model <- fitPredictiveModel(parts[[1]]$cohortData, 
                              parts[[1]]$covariateData, 
                              parts[[1]]$outcomeData, 
                              modelType = "logistic",
                              prior = createPrior("laplace", 0.1, exclude = c(0), useCrossValidation = FALSE)) 
  
  saveRDS(model, file = "s:/temp/PatientLevelPrediction/model.rda")
  
 # model <- readRDS("s:/temp/PatientLevelPrediction/model.rda")

  prediction <- predictProbabilities(model,
                                     parts[[2]]$cohortData, 
                                     parts[[2]]$covariateData)
  saveRDS(prediction, file = "s:/temp/PatientLevelPrediction/prediction.rda")
  
  computeAuc(prediction, parts[[2]]$outcomeData)
  plotRoc(prediction, parts[[2]]$outcomeData)
  plotCalibration(prediction, parts[[2]]$outcomeData, numberOfStrata = 10)
  
  modelDetails <- getModelDetails(model, covariateData)
  head(modelDetails)
}

