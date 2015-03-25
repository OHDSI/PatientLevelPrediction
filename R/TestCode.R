.testCode <- function(){
  library(PatientLevelPrediction)
  library(SqlRender)
  options("fftempdir" = "s:/temp")
  
  dbms = "postgresql"
  server = "localhost/enar"
  user = "postgres"
  password = pw
  cdmDatabaseSchema = "cdm_sim"
  resultsDatabaseSchema = "scratch"
  port <- NULL
  
  details <- createConnectionDetails(dbms, user, password, server, port, cdmDatabaseSchema)
  
  #Create cohorts:
  conn <- connect(details)
  sql <- loadRenderTranslateSql("HospitalizationCohorts.sql",
                                packageName = "PatientLevelPrediction",
                                dbms = details$dbms,
                                resultsDatabaseSchema = resultsDatabaseSchema,
                                post_time = 30,
                                pre_time = 365)
  executeSql(conn, sql)
  
  #Test package functions:
  cohorts <- getDbCohortsData(details, 
                              cdmDatabaseSchema = cdmDatabaseSchema, 
                              cohortDatabaseSchema = resultsDatabaseSchema, 
                              cohortTable = "first_hospitalization", 
                              cohortConceptIds = 1)
  saveCohortsData(cohorts, "s:/temp/cohortsData")
  covariates <- getDbCovariateData(details, 
                                   cdmDatabaseSchema = cdmDatabaseSchema, 
                                   cohortDatabaseSchema = resultsDatabaseSchema, 
                                   cohortTable = "first_hospitalization", 
                                   cohortConceptIds = 1, 
                                   useCovariateConditionOccurrence = TRUE, 
                                   deleteCovariatesSmallCount = 1)
  
  saveCovariateData(covariates, "s:/temp/covariateData")
  
  outcomes <- getDbOutcomeData(details, 
                               cdmDatabaseSchema = cdmDatabaseSchema, 
                               cohortDatabaseSchema = resultsDatabaseSchema, 
                               cohortTable = "first_hospitalization", 
                               cohortConceptIds = 1,
                               outcomeDatabaseSchema = resultsDatabaseSchema,
                               outcomeTable = "rehospitalization",
                               outcomeConceptIds = 1)
  
  saveOutcomeData(outcomes, "s:/temp/outcomeData")
 
  #cohorts <- loadCohortsData("s:/temp/cohortsData")
  #covariates <- loadCovariateData("s:/temp/covariateData") 
  #outcomes <- loadOutcomeData("s:/temp/outcomeData")
  
  model <- fitPredictiveModel(cohorts, covariates, outcomes, modelType = "logistic", prior = createPrior("laplace", 0.1, exclude = c(0), useCrossValidation = FALSE))
  
  modelDetails <- getPredictiveModel(model, covariates)
  
  x <- predict(model, cohorts, covariates)
  
  model <- fitPredictiveModel(cohorts, covariates, outcomes, modelType = "poisson", prior = createPrior("laplace", 0.1, exclude = c(0), useCrossValidation = FALSE))
  
  x <- predict(model, cohorts, covariates)
  
  
  #   Functions:
  #   
  #   fitPredictiveModel(covariateData, outcomeData)            Creates predictiveModel (CyclopsFit?)
  #   predict(model, covariateData)   Creates data.frame:personId, cohortStartDate, prediction
  #   computeAuc(prediction, outcomeData)
  #   plotCalibration(prediction, outcomeData)
  #   crossValidation(covariateData, outcomeData, folds)
  #   
  
}