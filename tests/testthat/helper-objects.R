
# Download the PostreSQL driver ---------------------------
# If DATABASECONNECTOR_JAR_FOLDER exists, assume driver has been downloaded
jarFolder <- Sys.getenv("DATABASECONNECTOR_JAR_FOLDER", unset = "")
if (jarFolder == "") {
  tempJarFolder <- tempfile("jdbcDrivers")
  dir.create(tempJarFolder)
  Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = tempJarFolder)
  downloadJdbcDrivers("postgresql")
  
  withr::defer({
    unlink(tempJarFolder, recursive = TRUE, force = TRUE)
    Sys.unsetenv("DATABASECONNECTOR_JAR_FOLDER")
  }, testthat::teardown_env())
}


# this files contains the objects used in the tests:
travis <- T

saveLoc <- tempfile("saveLoc")
dir.create(saveLoc)


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

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# simulated data Tests
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

data(plpDataSimulationProfile, envir = environment())

# PLPDATA
sampleSize <- 2500+sample(1000,1)
plpData <- simulatePlpData(plpDataSimulationProfile, n = sampleSize)
#plpData$metaData$cohortId <- plpData$metaData$cohortIds

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
  splitSettings = createDefaultSplitSetting(),
  preprocessSettings = createPreprocessSettings(), 
  modelSettings = lrSet, 
  logSettings = createLogSettings(verbosity = 'TRACE'),
  executeSettings = createDefaultExecuteSettings(), 
  saveDirectory = saveLoc
  )

#


population <- createStudyPopulation(
  plpData = plpData,
  outcomeId = 2, 
  populationSettings = populationSettings
  )

createTrainData <- function(plpData, population){
  trainData <- list()
  trainData$covariateData <- Andromeda::copyAndromeda(plpData$covariateData)
  attr(trainData$covariateData, "metaData") <- attr(plpData$covariateData, "metaData")
  trainData$labels <- population
  trainData$folds <- data.frame(
    rowId = population$rowId,
    index = sample(3, nrow(population), replace = T)
  )
  
  attr(trainData, "metaData")$outcomeId <- 2
  attr(trainData, "metaData")$cohortId <- 1

  class(trainData$covariateData) <- 'CovariateData'
  
  return(trainData)
}


sampleSize2 <- 1000+sample(1000,1)
plpData2 <- simulatePlpData(plpDataSimulationProfile, n = sampleSize2)

population2 <- createStudyPopulation(
  plpData = plpData2,
  outcomeId = 2, 
  populationSettings = populationSettings
)

sampleSizeBig <- 10000
plpDataBig <- simulatePlpData(plpDataSimulationProfile, n = sampleSizeBig)

populationBig <- createStudyPopulation(
  plpData = plpDataBig,
  outcomeId = 2, 
  populationSettings = populationSettings
)

