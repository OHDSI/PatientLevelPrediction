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
.vignetteDataFetch <- function() {
  # This function should be used to fetch the data that is used in the vignettes.  library(SqlRender);
  # library(DatabaseConnector); library(PatientLevelPrediction)
  setwd("s:/temp")
  options(fftempdir = "s:/FFtemp")
  
  pw <- NULL
  dbms <- "sql server"
  user <- NULL
  server <- "RNDUSRDHIT07.jnj.com"
  cdmDatabaseSchema <- "cdm_truven_mdcd.dbo"
  resultsDatabaseSchema <- "scratch.dbo"
  port <- NULL
  
  dbms <- "postgresql"
  server <- "localhost/ohdsi"
  user <- "postgres"
  pw <- "F1r3starter"
  cdmDatabaseSchema <- "cdm4_sim"
  resultsDatabaseSchema <- "scratch"
  port <- NULL
  
  pw <- NULL
  dbms <- "pdw"
  user <- NULL
  server <- "JRDUSAPSCTL01"
  cdmDatabaseSchema <- "cdm_truven_mdcd.dbo"
  resultsDatabaseSchema <- "scratch.dbo"
  oracleTempSchema <- NULL
  port <- 17001
  cdmVersion <- "4"
  
  connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                  server = server,
                                                                  user = user,
                                                                  password = pw,
                                                                  port = port)
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
  dbDisconnect(connection)
  
  covariateSettings <- createCovariateSettings(useCovariateDemographics = TRUE,
                                               useCovariateDemographicsGender = TRUE,
                                               useCovariateDemographicsRace = TRUE,
                                               useCovariateDemographicsEthnicity = TRUE,
                                               useCovariateDemographicsAge = TRUE,
                                               useCovariateDemographicsYear = TRUE,
                                               useCovariateDemographicsMonth = TRUE,
                                               useCovariateConditionOccurrence = TRUE,
                                               useCovariateConditionOccurrence365d = TRUE,
                                               useCovariateConditionOccurrence30d = TRUE,
                                               useCovariateConditionOccurrenceInpt180d = TRUE,
                                               useCovariateConditionEra = TRUE,
                                               useCovariateConditionEraEver = TRUE,
                                               useCovariateConditionEraOverlap = TRUE,
                                               useCovariateConditionGroup = TRUE,
                                               useCovariateConditionGroupMeddra = TRUE,
                                               useCovariateConditionGroupSnomed = TRUE,
                                               useCovariateDrugExposure = TRUE,
                                               useCovariateDrugExposure365d = TRUE,
                                               useCovariateDrugExposure30d = TRUE,
                                               useCovariateDrugEra = TRUE,
                                               useCovariateDrugEra365d = TRUE,
                                               useCovariateDrugEra30d = TRUE,
                                               useCovariateDrugEraOverlap = TRUE,
                                               useCovariateDrugEraEver = TRUE,
                                               useCovariateDrugGroup = TRUE,
                                               useCovariateProcedureOccurrence = TRUE,
                                               useCovariateProcedureOccurrence365d = TRUE,
                                               useCovariateProcedureOccurrence30d = TRUE,
                                               useCovariateProcedureGroup = TRUE,
                                               useCovariateObservation = TRUE,
                                               useCovariateObservation365d = TRUE,
                                               useCovariateObservation30d = TRUE,
                                               useCovariateObservationCount365d = TRUE,
                                               useCovariateMeasurement = TRUE,
                                               useCovariateMeasurement365d = TRUE,
                                               useCovariateMeasurement30d = TRUE,
                                               useCovariateMeasurementCount365d = TRUE,
                                               useCovariateMeasurementBelow = TRUE,
                                               useCovariateMeasurementAbove = TRUE,
                                               useCovariateConceptCounts = TRUE,
                                               useCovariateRiskScores = TRUE,
                                               useCovariateRiskScoresCharlson = TRUE,
                                               useCovariateRiskScoresDCSI = TRUE,
                                               useCovariateRiskScoresCHADS2 = TRUE,
                                               useCovariateRiskScoresCHADS2VASc = TRUE,
                                               useCovariateInteractionYear = FALSE,
                                               useCovariateInteractionMonth = FALSE,
                                               excludedCovariateConceptIds = c(),
                                               includedCovariateConceptIds = c(),
                                               deleteCovariatesSmallCount = 100)
  
  plpData <- getDbPlpData(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          oracleTempSchema = oracleTempSchema,
                          cohortDatabaseSchema = resultsDatabaseSchema,
                          cohortTable = "rehospitalization",
                          cohortIds = 1,
                          useCohortEndDate = TRUE,
                          windowPersistence = 0,
                          covariateSettings = covariateSettings,
                          outcomeDatabaseSchema = resultsDatabaseSchema,
                          outcomeTable = "rehospitalization",
                          outcomeIds = 2,
                          firstOutcomeOnly = FALSE, 
                          cdmVersion = cdmVersion)
  
  savePlpData(plpData, "s:/temp/PlpVignette/plpData")
  
  # plpData <- loadPlpData('s:/temp/PlpVignette/plpData')
  
  means <- computeCovariateMeans(plpData = plpData, outcomeId = 2)
  
  saveRDS(means, "s:/temp/PlpVignette/means.rds")
  
  #plotCovariateDifferenceOfTopVariables(means)
  
  parts <- splitData(plpData, c(0.75, 0.25))
  
  savePlpData(parts[[2]], "s:/temp/PlpVignette/plpData_test")
  
  model <- fitPredictiveModel(parts[[1]],
                              modelType = "logistic",
                              prior = createPrior("laplace",
                                                  exclude = c(0),
                                                  useCrossValidation = TRUE),
                              control = createControl(noiseLevel = "quiet",
                                                      cvType = "auto",
                                                      startingVariance = 0.01,
                                                      threads = 10))
  
  saveRDS(model, file = "s:/temp/PlpVignette/model.rds")
  
  # model <- readRDS('s:/temp/PlpVignette/model.rds')
  
  # parts <- list(); parts[[2]] <- loadPlpData('s:/temp/PlpVignette/plpData_test')
  
  prediction <- predictProbabilities(model, parts[[2]])
  
  saveRDS(prediction, file = "s:/temp/PlpVignette/prediction.rds")
  
  # prediction <- readRDS('s:/temp/PlpVignette/prediction.rds')
  
  computeAuc(prediction, parts[[2]])
  plotRoc(prediction, parts[[2]])
  plotCalibration(prediction, parts[[2]], numberOfStrata = 10)
  
  modelDetails <- getModelDetails(model, parts[[2]])
  head(modelDetails)
}

