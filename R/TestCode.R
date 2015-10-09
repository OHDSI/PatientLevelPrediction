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
                          firstOutcomeOnly = FALSE, 
                          cdmVersion = cdmVersion)
  
  savePlpData(plpData, "s:/temp/plpData")
  
  plpData <- loadPlPData("s:/temp/plpData")
  
  plpData
  summary(plpData)
  
  x <- aggregate(rowId ~ covariateId, data = plpData$covariates, length)
  x <- x[order(-x$rowId),]
  format(head(x$covariateId))
  
  t <- plpData$covariateRef$covariateId == 1001
  plpData$covariateRef[ffbase::ffwhich(t, t == TRUE),]
  
  
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
  
  prediction <- predictProbabilities(model, parts[[2]])
  
  connectionDetails = connectionDetails
  cdmDatabaseSchema = cdmDatabaseSchema
  oracleTempSchema = oracleTempSchema
  cohortDatabaseSchema = workDatabaseSchema
  cohortTable = studyCohortTable
  cohortIds = 1
  useCohortEndDate = FALSE
  windowPersistence = 365
  covariateSettings = covariateSettings
  outcomeDatabaseSchema = workDatabaseSchema
  outcomeTable = studyCohortTable
  outcomeIds = 10:16
  firstOutcomeOnly = FALSE
  cdmVersion = cdmVersion
  outcomeConditionTypeConceptIds = c()
  
  
  
  
  
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
  cdmDatabaseSchema <- "cdm_truven_mdcd.dbo"
  resultsDatabaseSchema <- "scratch.dbo"
  port <- 17001
  
  details <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                        server = server,
                                                        user = user,
                                                        password = pw,
                                                        port = port)
  
  # Create cohorts:
  conn <- connect(details)
  sql <- loadRenderTranslateSql("HospitalizationCohorts.sql",
                                packageName = "PatientLevelPrediction",
                                dbms = details$dbms,
                                cdmDatabaseSchema = cdmDatabaseSchema,
                                resultsDatabaseSchema = resultsDatabaseSchema,
                                post_time = 30,
                                pre_time = 365)
  executeSql(conn, sql)
  
  # Test package functions:
  cohortData <- getDbCohortData(details,
                                cdmDatabaseSchema = cdmDatabaseSchema,
                                cohortDatabaseSchema = resultsDatabaseSchema,
                                cohortTable = "rehospitalization",
                                cohortIds = 1)
  # savecohortData(cohortData, 's:/temp/cohortData')
  covariateSettings <- createCovariateSettings(useCovariateDemographics = TRUE,
                                               useCovariateConditionOccurrence = TRUE)
  covariateData <- getDbCovariateData(details,
                                      cdmDatabaseSchema = cdmDatabaseSchema,
                                      cohortDatabaseSchema = resultsDatabaseSchema,
                                      cohortTable = "rehospitalization",
                                      cohortIds = 1,
                                      covariateSettings = covariateSettings)
  
  # saveCovariateData(covariateData, 's:/temp/covariateData')
  
  outcomeData <- getDbOutcomeData(details,
                                  cdmDatabaseSchema = cdmDatabaseSchema,
                                  cohortDatabaseSchema = resultsDatabaseSchema,
                                  cohortTable = "first_hospitalization",
                                  cohortIds = 1,
                                  outcomeDatabaseSchema = resultsDatabaseSchema,
                                  outcomeTable = "rehospitalization",
                                  outcomeConceptIds = 1,
                                  firstOutcomeOnly = TRUE)
  
  # saveOutcomeData(outcomeData, 's:/temp/outcomeData')
  
  # cohortData <- loadcohortData('s:/temp/cohortData') covariateData <-
  # loadCovariateData('s:/temp/covariateData') outcomeData <- loadOutcomeData('s:/temp/outcomeData')
  
  model <- fitPredictiveModel(cohortData,
                              covariateData,
                              outcomeData,
                              modelType = "logistic",
                              prior = createPrior("laplace",
                                                  0.1,
                                                  exclude = c(0),
                                                  useCrossValidation = FALSE))
  
  modelDetails <- getModelDetails(model, covariateData)
  
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
