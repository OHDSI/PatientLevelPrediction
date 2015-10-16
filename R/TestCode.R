.testCode <- function() {
  library(PatientLevelPrediction)
  options(fftempdir = "s:/FFtemp")
  
  dbms <- "pdw"
  user <- NULL
  pw <- NULL
  server <- "JRDUSAPSCTL01"
  port <- 17001
  connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                  server = server,
                                                                  user = user,
                                                                  password = pw,
                                                                  port = port)
  cdmDatabaseSchema <- "cdm_truven_mdcd_v5.dbo"
  workDatabaseSchema <- "scratch.dbo"
  studyCohortTable <- "ohdsi_celecoxib_prediction"
  oracleTempSchema <- NULL
  cdmVersion <- "5"
  outputFolder <- "S:/temp/CelecoxibPredictiveModels"
  
  
  ### Create covariateSettings ###
  conn <- DatabaseConnector::connect(connectionDetails)
  sql <- "SELECT descendant_concept_id FROM @cdm_database_schema.concept_ancestor WHERE ancestor_concept_id = 1118084"
  sql <- SqlRender::renderSql(sql, cdm_database_schema = cdmDatabaseSchema)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
  celecoxibDrugs <- DatabaseConnector::querySql(conn, sql)
  celecoxibDrugs <- celecoxibDrugs[,1]
  RJDBC::dbDisconnect(conn)
  
  covariateSettings <- PatientLevelPrediction::createCovariateSettings(useCovariateDemographics = TRUE,
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
                                                                       excludedCovariateConceptIds = celecoxibDrugs,
                                                                       includedCovariateConceptIds = c(),
                                                                       deleteCovariatesSmallCount = 100)
  
  
  plpData <- getDbPlpData(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          oracleTempSchema = oracleTempSchema,
                          cohortDatabaseSchema = workDatabaseSchema,
                          cohortTable = studyCohortTable,
                          cohortIds = 1,
                          useCohortEndDate = FALSE,
                          windowPersistence = 365,
                          covariateSettings = covariateSettings,
                          outcomeDatabaseSchema = workDatabaseSchema,
                          outcomeTable = studyCohortTable,
                          outcomeIds = 10:16,
                          firstOutcomeOnly = TRUE, 
                          cdmVersion = cdmVersion)
  
  savePlpData(plpData, "s:/temp/plpData")
  
  plpData <- loadPlpData("s:/temp/plpData")
  
  plpData
  summary(plpData)
  
  splits <- splitData(plpData, splits = c(0.75,0.25))
  
  summary(splits[[1]])
  summary(splits[[2]])
  
  model <- fitPredictiveModel(plpData = splits[[1]],
                              modelType = "logistic",
                              removeDropoutsForLr = TRUE,
                              cohortId = 1,
                              outcomeId = 10,
                              prior = createPrior("laplace",
                                                  exclude = c(0),
                                                  variance = 0.007))
  
  saveRDS(model, file = "s:/temp/plpTestmodel.rds")
  
  # model <- readRDS('s:/temp/plpTestmodel.rds')
  
  prediction <- predictProbabilities(model, splits[[2]])
  
  
  saveRDS(prediction, file = "s:/temp/plpTestPredicition.rds")

  prediction <- readRDS('s:/temp/plpTestPredicition.rds')

  plotCalibration(prediction, splits[[2]], numberOfStrata = 10, truncateFraction = 0.01, fileName = "s:/temp/calibration.png")  
}

