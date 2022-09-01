
# this files contains the objects used in the tests:
if(Sys.getenv('GITHUB_ACTIONS') == 'true'){
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
  
  if(ifelse(is.null(Sys.info()), T, Sys.info()['sysname'] != 'Windows')){
    # configure and activate python
    PatientLevelPrediction::configurePython(envname = 'r-reticulate', envtype = "conda")
    PatientLevelPrediction::setPythonEnvironment(envname = 'r-reticulate', envtype = "conda")
    
    # if mac install nomkl -- trying to fix github actions
    if(ifelse(is.null(Sys.info()), F, Sys.info()['sysname'] == 'Darwin')){
      reticulate::conda_install(envname = 'r-reticulate', packages = c('nomkl'), 
                                forge = TRUE, pip = FALSE, pip_ignore_installed = TRUE, 
                                conda = "auto")
    }
  }
}


saveLoc <- tempfile("saveLoc")
dir.create(saveLoc)


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# simulated data Tests
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

data(plpDataSimulationProfile, envir = environment())

# PLPDATA
sampleSize <- 2000+sample(300,1)
plpData <- simulatePlpData(plpDataSimulationProfile, n = sampleSize)

# POPULATION
populationSettings <- createStudyPopulationSettings(
  firstExposureOnly = FALSE,
  washoutPeriod = 0,
  removeSubjectsWithPriorOutcome = FALSE,
  priorOutcomeLookback = 99999,
  requireTimeAtRisk = T,
  minTimeAtRisk=10,
  riskWindowStart = 0,
  startAnchor = 'cohort start',
  riskWindowEnd = 365,
  endAnchor = 'cohort start'
  )


# MODEL SETTINGS
lrSet <- setLassoLogisticRegression()

# RUNPLP - LASSO LR
plpResult <- runPlp(
  plpData = plpData, 
  outcomeId = 2, 
  analysisId = 'Test', 
  analysisName = 'Testing analysis',
  populationSettings = populationSettings, 
  splitSettings = createDefaultSplitSetting(splitSeed = 12),
  preprocessSettings = createPreprocessSettings(), 
  modelSettings = lrSet, 
  logSettings = createLogSettings(verbosity = 'TRACE'),
  executeSettings = createDefaultExecuteSettings(), 
  saveDirectory = saveLoc
  )


# now diagnose
diagnoseResult <- diagnosePlp(
  plpData = plpData,
  outcomeId = 2,
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
  outcomeId = 2, 
  populationSettings = populationSettings
  )

createTrainData <- function(plpData, population){
  data <- PatientLevelPrediction::splitData(plpData = plpData, population=population,
                                            splitSettings = PatientLevelPrediction::createDefaultSplitSetting(splitSeed = 12))
  trainData <- data$Train
  return(trainData)
}

createTestData <- function(plpData, population){
  data <- PatientLevelPrediction::splitData(plpData = plpData, population=population,
                                            splitSettings = PatientLevelPrediction::createDefaultSplitSetting(splitSeed = 12))
  testData <- data$Test
  return(testData)
}

connectionDetails <- Eunomia::getEunomiaConnectionDetails()
Eunomia::createCohorts(connectionDetails)

covSet <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T,
                                                     useDemographicsAge = T,
                                                     useDemographicsRace = T,
                                                     useDemographicsEthnicity = T,
                                                     useDemographicsAgeGroup = T,
                                                     useConditionGroupEraLongTerm = T,
                                                     useDrugEraStartLongTerm  = T,
                                                     endDays = -1
)

