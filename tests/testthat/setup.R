# this files contains the objects used in the tests:
if (Sys.getenv("GITHUB_ACTIONS") == "true") {
  # Download the PostreSQL driver ---------------------------
  # If DATABASECONNECTOR_JAR_FOLDER exists, assume driver has been downloaded
  jarFolder <- Sys.getenv("DATABASECONNECTOR_JAR_FOLDER", unset = "")
  if (jarFolder == "") {
    tempJarFolder <- tempfile("jdbcDrivers")
    dir.create(tempJarFolder)
    Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = tempJarFolder)
    DatabaseConnector::downloadJdbcDrivers("postgresql")
  }
}

if (rlang::is_installed("curl")) {
  internet <- curl::has_internet()
  message("Internet: ", internet)
} else {
  internet <- FALSE
  message("Internet: ", internet)
}

saveLoc <- tempfile("saveLoc")
dir.create(saveLoc)

if (internet && rlang::is_installed("Eunomia")) {
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
    outcomeTable = "cohort",
    targetId = 1,
    outcomeIds = outcomeId,
    cdmVersion = 5
  )

  covariateSettings <- FeatureExtraction::createCovariateSettings(
    useDemographicsAge = TRUE,
    useDemographicsGender = TRUE,
    useConditionOccurrenceAnyTimePrior = TRUE
  )

  plpData <- getPlpData(
    databaseDetails = databaseDetails,
    covariateSettings = covariateSettings,
    restrictPlpDataSettings = createRestrictPlpDataSettings()
  )

  # POPULATION
  populationSettings <- createStudyPopulationSettings(
    firstExposureOnly = FALSE,
    washoutPeriod = 0,
    removeSubjectsWithPriorOutcome = FALSE,
    priorOutcomeLookback = 99999,
    requireTimeAtRisk = TRUE,
    minTimeAtRisk = 10,
    riskWindowStart = 0,
    startAnchor = "cohort start",
    riskWindowEnd = 365,
    endAnchor = "cohort start"
  )

  # MODEL SETTINGS
  lrSet <- setLassoLogisticRegression(seed = 42)

  # RUNPLP - LASSO LR
  plpResult <- runPlp(
    plpData = plpData,
    outcomeId = outcomeId,
    analysisId = "Test",
    analysisName = "Testing analysis",
    populationSettings = populationSettings,
    splitSettings = createDefaultSplitSetting(splitSeed = 12),
    preprocessSettings = createPreprocessSettings(),
    modelSettings = lrSet,
    logSettings = createLogSettings(verbosity = "ERROR"),
    executeSettings = createDefaultExecuteSettings(),
    saveDirectory = saveLoc
  )


  # now diagnose
  diagnoseResult <- diagnosePlp(
    plpData = plpData,
    outcomeId = outcomeId,
    analysisId = "Test",
    populationSettings = populationSettings,
    splitSettings = createDefaultSplitSetting(splitSeed = 12),
    saveDirectory = saveLoc,
    modelSettings = lrSet,
    logSettings = createLogSettings(
      verbosity = "DEBUG",
      timeStamp = TRUE,
      logName = "diagnosePlp Log"
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

  createSplitData <- function(plpData, population, split = "train") {
    data <- PatientLevelPrediction::splitData(
      plpData = plpData,
      population = population,
      splitSettings = PatientLevelPrediction::createDefaultSplitSetting(splitSeed = 12)
    )
    if (split == "train") {
      return(data$Train)
    } else {
      return(data$Test)
    }
  }

  trainData <- createSplitData(plpData, population)
  testData <- createSplitData(plpData, population, split = "test")


  # reduced Data to only use n most important features (as decided by LR)
  reduceData <- function(data, n = 20) {
    covariates <- plpResult$model$covariateImportance %>%
      dplyr::slice_max(order_by = abs(.data$covariateValue), n = n, with_ties = FALSE) %>%
      dplyr::pull(.data$covariateId)

    reducedData <- list(
      labels = data$labels,
      folds = data$folds,
      covariateData = Andromeda::andromeda(
        analysisRef = data$covariateData$analysisRef
      )
    )


    reducedData$covariateData$covariates <- trainData$covariateData$covariates %>%
      dplyr::filter(.data$covariateId %in% covariates)
    reducedData$covariateData$covariateRef <- trainData$covariateData$covariateRef %>%
      dplyr::filter(.data$covariateId %in% covariates)

    attributes(reducedData$covariateData)$metaData <- attributes(trainData$covariateData)$metaData
    class(reducedData$covariateData) <- class(trainData$covariateData)
    attributes(reducedData)$metaData <- attributes(trainData)$metaData
    class(reducedData) <- class(data)
    return(reducedData)
  }

  tinyTrainData <- reduceData(trainData)
  tinyTestData <- reduceData(testData)

  tinyPlpData <- createTinyPlpData(plpData, plpResult)

  nanoData <- createTinyPlpData(plpData, plpResult, n = 2)
  tinyResults <- runPlp(
    plpData = nanoData,
    populationSettings = populationSettings,
    outcomeId = outcomeId,
    analysisId = "tinyFit",
    executeSettings = createExecuteSettings(
      runSplitData = TRUE,
      runSampleData = FALSE,
      runfeatureEngineering = FALSE,
      runPreprocessData = TRUE,
      runModelDevelopment = TRUE,
      runCovariateSummary = FALSE
    ),
    saveDirectory = file.path(saveLoc, "tinyResults")
  )
}

withr::defer(
  {
    if (Sys.getenv("GITHUB_ACTIONS") == "true") {
      # Remove the JDBC driver folder
      jarFolder <- Sys.getenv("DATABASECONNECTOR_JAR_FOLDER", unset = "")
      if (jarFolder != "") {
        unlink(jarFolder, recursive = TRUE)
      }
    }
    unlink(saveLoc, recursive = TRUE)
    if (internet && rlang::is_installed("Eunomia")) {
      unlink(connectionDetails$server())
    }
  },
  envir = teardown_env()
)
