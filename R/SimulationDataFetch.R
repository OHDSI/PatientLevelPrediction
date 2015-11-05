# @file SimulationDataFetch.R
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
.simulationDataFetch <- function() {
  # This function should be used to create the simulation profile used in some of the unit tests
  # library(PatientLevelPrediction); options('fftempdir' = 's:/fftemp');
  
  pw <- NULL
  dbms <- "sql server"
  user <- NULL
  server <- "RNDUSRDHIT07.jnj.com"
  cdmDatabaseSchema <- "cdm_truven_mdcd.dbo"
  port <- NULL
  
  pw <- NULL
  dbms <- "pdw"
  user <- NULL
  server <- "JRDUSAPSCTL01"
  cdmDatabaseSchema <- "cdm_truven_mdcd_v5.dbo"
  resultsDatabaseSchema <- "scratch.dbo"
  port <- 17001
  cdmVersion <- 5
  
  connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                  server = server,
                                                                  user = user,
                                                                  password = pw,
                                                                  port = port)
  # Create cohorts:
  conn <- connect(connectionDetails)
  
  sql <- "
  IF OBJECT_ID('@resultsDatabaseSchema.mschuemi_stroke', 'U') IS NOT NULL
  DROP TABLE @resultsDatabaseSchema.mschuemi_stroke;
  
  SELECT temp.person_id AS subject_id,
  cohort_start_date, 
  DATEADD(DAY, 365, cohort_start_date) AS cohort_end_date,
  1 AS cohort_definition_id
  INTO @resultsDatabaseSchema.mschuemi_stroke
  FROM (
  SELECT person_id,
  MIN(condition_start_date) AS cohort_start_date
  FROM @cdmDatabaseSchema.condition_occurrence 
  WHERE condition_concept_id = 313217 /* Atrial fibrillation */
  GROUP BY person_id
  ) temp 
  INNER JOIN @cdmDatabaseSchema.observation_period
  ON observation_period.person_id = temp.person_id
  WHERE DATEDIFF(DAY, observation_period_start_date, cohort_start_date) >= 365
  AND  DATEDIFF(DAY, cohort_start_date, observation_period_end_date ) >= 365;
  
  INSERT INTO @resultsDatabaseSchema.mschuemi_stroke
  SELECT condition_occurrence.person_id AS subject_id,
  condition_start_date AS cohort_start_date,
  condition_end_date AS cohort_end_date,
  2 AS cohort_concept_id
  FROM  @cdmDatabaseSchema.condition_occurrence
  WHERE condition_concept_id IN (4108356, 4110189, 4110190, 4110192, 45767658, 45772786) /* Cerebral infarct */
  AND condition_type_concept_id  IN (38000183,38000199,44786627,38000184,38000200);

  INSERT INTO @resultsDatabaseSchema.mschuemi_stroke
  SELECT condition_occurrence.person_id AS subject_id,
  condition_start_date AS cohort_start_date,
  condition_end_date AS cohort_end_date,
  3 AS cohort_concept_id
  FROM  @cdmDatabaseSchema.condition_occurrence
  WHERE condition_concept_id IN (312327, 434376, 436706, 438170, 438438, 438447, 439693, 441579, 444406) /* Myocardial infarction */
  AND condition_type_concept_id IN (38000183,38000199,44786627,38000184,38000200);
  "
  sql <- SqlRender::renderSql(sql, 
                              cdmDatabaseSchema = cdmDatabaseSchema,
                              resultsDatabaseSchema = resultsDatabaseSchema)$sql
  
  sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
  
  executeSql(conn, sql)
  
  sql <- "SELECT cohort_definition_id, COUNT(*) AS count FROM @resultsDatabaseSchema.mschuemi_stroke GROUP BY cohort_definition_id"
  sql <- SqlRender::renderSql(sql, resultsDatabaseSchema = resultsDatabaseSchema)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
  DatabaseConnector::querySql(conn, sql)
  
  sql <- "SELECT COUNT(*) FROM @resultsDatabaseSchema.mschuemi_stroke afib 
  INNER JOIN @resultsDatabaseSchema.mschuemi_stroke stroke ON afib.subject_id = stroke.subject_id 
  WHERE afib.cohort_definition_id = 1 and stroke.cohort_definition_id = 2
  AND stroke.cohort_start_date >= afib.cohort_start_date
  AND stroke.cohort_start_date >= afib.cohort_end_date"
  sql <- SqlRender::renderSql(sql, resultsDatabaseSchema = resultsDatabaseSchema)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
  DatabaseConnector::querySql(conn, sql)
  
  RJDBC::dbDisconnect(conn)
  
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
                          cohortDatabaseSchema = resultsDatabaseSchema,
                          cohortTable = "mschuemi_stroke",
                          cohortIds = 1,
                          useCohortEndDate = TRUE,
                          windowPersistence = 0,
                          covariateSettings = covariateSettings,
                          outcomeDatabaseSchema = resultsDatabaseSchema,
                          outcomeTable = "mschuemi_stroke",
                          outcomeIds = c(2,3),
                          firstOutcomeOnly = TRUE,
                          cdmVersion = cdmVersion)
  summary(plpData)
  
  savePlpData(plpData, "s:/temp/plpStrokeData")
  #plpData <- loadPlpData("s:/temp/plpStrokeData")
  #plpData$outcomes
  # Delete all but first outcome for this example:
#   plpData$metaData$outcomeIds <- c(10)
#   t <- plpData$outcomes$outcomeId == 10
#   plpData$outcomes <- plpData$outcomes[ffbase::ffwhich(t, t == TRUE),]
#   t <- plpData$exclude$outcomeId == 10
#   plpData$exclude <- plpData$exclude[ffbase::ffwhich(t, t == TRUE),]
#   
  
  plpDataSimulationProfile <- createPlpSimulationProfile(plpData)
  save(plpDataSimulationProfile,
       file = "C:/Users/mschuemi/git/PatientLevelPrediction/data/plpDataSimulationProfile.rda",
       compress = "xz")
  
  #plpDataSimulationProfile$outcomeModels[[]][1]
  
  
  # load('C:/Users/mschuemi/git/PatientLevelPrediction/data/plpDataSimulationProfile.rda')
  plpData <- simulatePlpData(plpDataSimulationProfile, n = 1000)
  
  plpData
  summary(plpData)
  
  model <- fitPredictiveModel(plpData, modelType = "poisson", outcomeId = 2)
  
  model <- fitPredictiveModel(plpData, modelType = "logistic", outcomeId = 2)
  prediction <- predictProbabilities(model, plpData)
  
  computeAuc(prediction, plpData)
  plotCalibration(prediction, plpData)
  plotRoc(prediction, plpData)
  means <- computeCovariateMeans(plpData, outcomeId = 2)
  plotCovariateDifferenceOfTopVariables(means)
}
