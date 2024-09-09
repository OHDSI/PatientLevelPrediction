
# this files contains the objects used in the tests:
if (Sys.getenv('GITHUB_ACTIONS') == 'true') {
  # Download the PostreSQL driver ---------------------------
  # If DATABASECONNECTOR_JAR_FOLDER exists, assume driver has been downloaded
  jarFolder <- Sys.getenv("DATABASECONNECTOR_JAR_FOLDER", unset = "")
  if (jarFolder == "") {
    tempJarFolder <- tempfile("jdbcDrivers")
    dir.create(tempJarFolder)
    Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = tempJarFolder)
    DatabaseConnector::downloadJdbcDrivers("postgresql")
    
    withr::defer({
      unlink(tempJarFolder, recursive = TRUE, force = TRUE)
      Sys.unsetenv("DATABASECONNECTOR_JAR_FOLDER")
    }, testthat::teardown_env())
  }
  
  # configure and activate python
  PatientLevelPrediction::configurePython(envname = 'r-reticulate', envtype = "conda")
  PatientLevelPrediction::setPythonEnvironment(envname = 'r-reticulate', envtype = "conda")
  
  # if mac install nomkl -- trying to fix github actions
  if (ifelse(is.null(Sys.info()), F, Sys.info()['sysname'] == 'Darwin')){
    reticulate::conda_install(envname = 'r-reticulate', packages = c('nomkl'), 
                              forge = TRUE, pip = FALSE, pip_ignore_installed = TRUE, 
                              conda = "auto")
  }
}


saveLoc <- tempfile("saveLoc")
dir.create(saveLoc)


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# simulated data Tests
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
data("plpDataSimulationProfile")

# PLPDATA
connectionDetails <- Eunomia::getEunomiaConnectionDetails()
Eunomia::createCohorts(connectionDetails)
outcomeId <- 3 # GIbleed

databaseDetails <- createDatabaseDetails(
  connectionDetails = connectionDetails, 
  cdmDatabaseSchema = "main", 
  cdmDatabaseName = "main",
  cohortDatabaseSchema = "main", 
  cohortTable = "cohort", 
  outcomeDatabaseSchema = "main", 
  outcomeTable =  "cohort",
  targetId = 1, 
  outcomeIds = outcomeId,
  cdmVersion = 5)

covariateSettings <- FeatureExtraction::createCovariateSettings(
  useDemographicsAge = TRUE,
  useDemographicsGender = TRUE,
  useConditionOccurrenceAnyTimePrior = TRUE
)

plpData <- getPlpData(databaseDetails = databaseDetails,
                      covariateSettings = covariateSettings,
                      restrictPlpDataSettings = createRestrictPlpDataSettings()) 

# POPULATION
populationSettings <- createStudyPopulationSettings(
  firstExposureOnly = FALSE,
  washoutPeriod = 0,
  removeSubjectsWithPriorOutcome = FALSE,
  priorOutcomeLookback = 99999,
  requireTimeAtRisk = T,
  minTimeAtRisk = 10,
  riskWindowStart = 0,
  startAnchor = 'cohort start',
  riskWindowEnd = 365,
  endAnchor = 'cohort start'
  )


# MODEL SETTINGS
lrSet <- setLassoLogisticRegression(seed = 42)

# RUNPLP - LASSO LR
plpResult <- runPlp(
  plpData = plpData, 
  outcomeId = outcomeId, 
  analysisId = 'Test', 
  analysisName = 'Testing analysis',
  populationSettings = populationSettings, 
  splitSettings = createDefaultSplitSetting(splitSeed = 12),
  preprocessSettings = createPreprocessSettings(), 
  modelSettings = lrSet, 
  logSettings = createLogSettings(verbosity = 'ERROR'),
  executeSettings = createDefaultExecuteSettings(), 
  saveDirectory = saveLoc
  )


# now diagnose
diagnoseResult <- diagnosePlp(
  plpData = plpData,
  outcomeId = outcomeId,
  analysisId = 'Test', 
  populationSettings = populationSettings,
  splitSettings = createDefaultSplitSetting(splitSeed = 12),
  saveDirectory = saveLoc,
  modelSettings = lrSet,
  logSettings =  createLogSettings(
    verbosity = 'DEBUG',
    timeStamp = T,
    logName = 'diagnosePlp Log'
  ),
  preprocessSettings = createPreprocessSettings(), 
  sampleSettings = NULL, 
  featureEngineeringSettings = NULL
)

#


population <- createStudyPopulation(
  plpData = plpData,
  outcomeId = outcomeId, 
  populationSettings = populationSettings
  )

createTrainData <- function(plpData, population){
  data <- PatientLevelPrediction::splitData(plpData = plpData, 
                                            population = population,
                                            splitSettings = PatientLevelPrediction::createDefaultSplitSetting(splitSeed = 12))
  trainData <- data$Train
  return(trainData)
}

trainData <- createTrainData(plpData, population)

createTestData <- function(plpData, population){
  data <- PatientLevelPrediction::splitData(plpData = plpData,
                                            population = population,
                                            splitSettings = PatientLevelPrediction::createDefaultSplitSetting(splitSeed = 12))
  testData <- data$Test
  return(testData)
}

testData <- createTestData(plpData, population)

# reduced trainData to only use n most important features (as decided by LR)
reduceTrainData <- function(trainData, n=20) {
  covariates <- plpResult$model$covariateImportance %>% 
    dplyr::slice_max(order_by = abs(.data$covariateValue),n = n, with_ties = F) %>% 
    dplyr::pull(.data$covariateId)
  
  reducedTrainData <- list(labels = trainData$labels,
                           folds = trainData$folds,
                           covariateData = Andromeda::andromeda(
                             analysisRef = trainData$covariateData$analysisRef
                           ))
  
  
  reducedTrainData$covariateData$covariates <- trainData$covariateData$covariates %>% 
    dplyr::filter(covariateId %in% covariates)
  reducedTrainData$covariateData$covariateRef <- trainData$covariateData$covariateRef %>% 
    dplyr::filter(covariateId %in% covariates)
  
  attributes(reducedTrainData$covariateData)$metaData <- attributes(trainData$covariateData)$metaData
  class(reducedTrainData$covariateData) <- class(trainData$covariateData)
  attributes(reducedTrainData)$metaData <- attributes(trainData)$metaData
  return(reducedTrainData)  
}

tinyTrainData <- reduceTrainData(trainData)

tinyPlpData <- createTinyPlpData(plpData, plpResult)

nanoData <- createTinyPlpData(plpData, plpResult, n = 2)
tinyResults <- runPlp(plpData = nanoData,
                      populationSettings = populationSettings,
                      outcomeId = outcomeId,
                      analysisId = 'tinyFit',
                      executeSettings = createExecuteSettings(
                        runSplitData = T,
                        runSampleData = F,
                        runfeatureEngineering = F,
                        runPreprocessData = T,
                        runModelDevelopment = T,
                        runCovariateSummary = F
                      ),
                      saveDirectory = file.path(saveLoc, 'tinyResults'))
