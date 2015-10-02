.smallTtestCode <- function() {
  # library(PatientLevelPrediction); library(SqlRender)
  options(fftempdir = "s:/fftemp")
  
  dbms <- "postgresql"
  server <- "localhost/enar"
  user <- "postgres"
  pw <- pw
  cdmDatabaseSchema <- "cdm_sim"
  resultsDatabaseSchema <- "scratch"
  port <- NULL
  
  pw <- NULL
  dbms <- "pdw"
  user <- NULL
  server <- "JRDUSAPSCTL01"
  cdmDatabaseSchema <- "cdm_truven_mdcd_v5.dbo"
  resultsDatabaseSchema <- "scratch.dbo"
  port <- 17001
  
  details <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                        server = server,
                                                        user = user,
                                                        password = pw,
                                                        port = port)
  
  # Create cohorts:
  conn <- connect(details)
  
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
  AND condition_type_concept_id  = 38000183;
  "
  sql <- SqlRender::renderSql(sql, 
                              cdmDatabaseSchema = cdmDatabaseSchema,
                              resultsDatabaseSchema = resultsDatabaseSchema)$sql
  
  sql <- SqlRender::translateSql(sql, targetDialect = details$dbms)$sql
  
  executeSql(conn, sql)
  
  sql <- "SELECT cohort_definition_id, COUNT(*) AS count FROM @resultsDatabaseSchema.mschuemi_stroke GROUP BY cohort_definition_id"
  sql <- SqlRender::renderSql(sql, resultsDatabaseSchema = resultsDatabaseSchema)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = details$dbms)$sql
  DatabaseConnector::querySql(conn, sql)
  
  sql <- "SELECT COUNT(*) FROM @resultsDatabaseSchema.mschuemi_stroke afib 
  INNER JOIN @resultsDatabaseSchema.mschuemi_stroke stroke ON afib.subject_id = stroke.subject_id 
  WHERE afib.cohort_definition_id = 1 and stroke.cohort_definition_id = 2
  AND stroke.cohort_start_date >= afib.cohort_start_date
  AND stroke.cohort_start_date >= afib.cohort_end_date"
  sql <- SqlRender::renderSql(sql, resultsDatabaseSchema = resultsDatabaseSchema)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = details$dbms)$sql
  DatabaseConnector::querySql(conn, sql)
  
  
  dbDisconnect(conn)
  
  # Test package functions:
  cohortData <- getDbCohortData(details,
                                cdmDatabaseSchema = cdmDatabaseSchema,
                                cohortDatabaseSchema = resultsDatabaseSchema,
                                cohortTable = "mschuemi_stroke",
                                cohortIds = 1,
                                cdmVersion = "5")
  # saveCohortData(cohortData, 's:/temp/smallPlp/CohortData')
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
  
  covariateData <- getDbCovariateData(details,
                                      cdmDatabaseSchema = cdmDatabaseSchema,
                                      cohortDatabaseSchema = resultsDatabaseSchema,
                                      cohortTable = "mschuemi_stroke",
                                      cohortIds = 1,
                                      covariateSettings = covariateSettings,
                                      cdmVersion = "5")
  
  # saveCovariateData(covariateData, 's:/temp/smallPlp/covariateData')
  
  outcomeData <- getDbOutcomeData(details,
                                  cdmDatabaseSchema = cdmDatabaseSchema,
                                  cohortDatabaseSchema = resultsDatabaseSchema,
                                  cohortTable = "mschuemi_stroke",
                                  cohortIds = 1,
                                  outcomeDatabaseSchema = resultsDatabaseSchema,
                                  outcomeTable = "mschuemi_stroke",
                                  outcomeIds = 2,
                                  firstOutcomeOnly = TRUE,
                                  cdmVersion = "5")
  
  # saveOutcomeData(outcomeData, 's:/temp/smallPlp/outcomeData')
  
  # cohortData <- loadCohortData('s:/temp/smallPlp/cohortData')
  # covariateData <- loadCovariateData('s:/temp/smallPlp/covariateData')
  # outcomeData <- loadOutcomeData('s:/temp/smallPlp/outcomeData')
  
  model <- fitPredictiveModel(cohortData,
                              covariateData,
                              outcomeData,
                              modelType = "logistic",
                              control = createControl(noiseLevel = "quiet", cvType = "auto",
                                                      startingVariance = 0.01, threads = 10))
  
  modelDetails <- getModelDetails(model, covariateData)
  
  # saveRDS(model, file = "s:/temp/smallPlp/model.rds")
  
  # model <- readRDS("s:/temp/smallPlp/model.rds")
  xLr <- predictProbabilities(model, cohortData, covariateData)
  simLr <- runif(nrow(xLr)) < xLr$value
  sum(simLr)
  
  computeAuc(xLr, outcomeData)
  computeAuc(xLr, outcomeData, confidenceInterval = TRUE)
  plotCalibration(xLr, outcomeData, fileName = "c:/temp/Calibration.png")
  plotRoc(xLr, outcomeData, fileName = "c:/temp/Roc.png")
  
  model <- fitPredictiveModel(cohortData,
                              covariateData,
                              outcomeData,
                              modelType = "poisson",
                              prior = createPrior("laplace",
                                                  0.1,
                                                  exclude = c(0),
                                                  useCrossValidation = FALSE))
  
  xPr <- predictProbabilities(model, cohortData, covariateData)
  simPr <- rpois(nrow(xPr), xPr$value)
  sum(simPr)
  
  model <- fitPredictiveModel(cohortData,
                              covariateData,
                              outcomeData,
                              modelType = "survival",
                              prior = createPrior("laplace",
                                                  0.1,
                                                  exclude = c(0),
                                                  useCrossValidation = FALSE))
  
  xSr <- predictProbabilities(model, cohortData, covariateData)
  simSr <- rexp(nrow(xSr), xSr$value) < 30
  sum(simSr)
  
  # Functions: fitPredictiveModel(covariateData, outcomeData) Creates predictiveModel (CyclopsFit?)
  # predict(model, covariateData) Creates data.frame:personId, cohortStartDate, prediction
  # computeAuc(prediction, outcomeData) plotCalibration(prediction, outcomeData)
  # crossValidation(covariateData, outcomeData, folds)
  
}
