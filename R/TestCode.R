.testCode <- function() {
  library(PatientLevelPrediction)
  library(SqlRender)
  options(fftempdir = "s:/temp")

  dbms <- "postgresql"
  server <- "localhost/enar"
  user <- "postgres"
  password <- pw
  cdmDatabaseSchema <- "cdm_sim"
  resultsDatabaseSchema <- "scratch"
  port <- NULL

  details <- createConnectionDetails(dbms, user, password, server, port, cdmDatabaseSchema)

  # Create cohorts:
  conn <- connect(details)
  sql <- loadRenderTranslateSql("HospitalizationCohorts.sql",
                                packageName = "PatientLevelPrediction",
                                dbms = details$dbms,
                                resultsDatabaseSchema = resultsDatabaseSchema,
                                post_time = 30,
                                pre_time = 365)
  executeSql(conn, sql)

  # Test package functions:
  cohortData <- getDbCohortData(details,
                                cdmDatabaseSchema = cdmDatabaseSchema,
                                cohortDatabaseSchema = resultsDatabaseSchema,
                                cohortTable = "first_hospitalization",
                                cohortConceptIds = 1)
  # savecohortData(cohortData, 's:/temp/cohortData')
  covariateSettings <- createCovariateSettings(useCovariateDemographics = TRUE,
                                               useCovariateConditionOccurrence = TRUE)
  covariateData <- getDbCovariateData(details,
                                      cdmDatabaseSchema = cdmDatabaseSchema,
                                      cohortDatabaseSchema = resultsDatabaseSchema,
                                      cohortTable = "first_hospitalization",
                                      cohortConceptIds = 1,
                                      covariateSettings = covariateSettings)

  # saveCovariateData(covariateData, 's:/temp/covariateData')

  outcomeData <- getDbOutcomeData(details,
                                  cdmDatabaseSchema = cdmDatabaseSchema,
                                  cohortDatabaseSchema = resultsDatabaseSchema,
                                  cohortTable = "first_hospitalization",
                                  cohortConceptIds = 1,
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
