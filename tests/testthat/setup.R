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

# helper so manual sourcing works without testthat
get_defer_env <- function(default = .GlobalEnv) {
  if (rlang::is_installed("testthat")) {
    env <- tryCatch(testthat::teardown_env(), error = function(...) NULL)
    if (!is.null(env)) {
      return(env)
    }
  }
  default
}

register_cleanup <- function(action) {
  stopifnot(is.function(action))
  env <- get_defer_env(default = NULL)
  if (!is.null(env) && !identical(env, .GlobalEnv)) {
    withr::defer(action(), envir = env)
  } else {
    cleanupName <- ".plp_system_file_teardown"
    existing <- get0(cleanupName, envir = .GlobalEnv, ifnotfound = list())
    assign(cleanupName, c(existing, list(action)), envir = .GlobalEnv)
  }
}

run_system_file_cleanups <- function() {
  cleanupName <- ".plp_system_file_teardown"
  if (exists(cleanupName, envir = .GlobalEnv, inherits = FALSE)) {
    cleanups <- get(cleanupName, envir = .GlobalEnv)
    rm(list = cleanupName, envir = .GlobalEnv)
    lapply(rev(cleanups), function(fn) {
      try(fn(), silent = TRUE)
    })
    invisible(TRUE)
  } else {
    invisible(FALSE)
  }
}

resetSystemFileShim <- function() {
  run_system_file_cleanups()
}

# shim system.file to point to inst/ when in dev mode:
if (requireNamespace("pkgload", quietly = TRUE) &&
      pkgload::is_dev_package("PatientLevelPrediction")) {

  devRoot <- tryCatch(
    normalizePath(pkgload::pkg_path(), mustWork = TRUE),
    error = function(err) NA_character_
  )

  if (!is.na(devRoot) && nzchar(devRoot)) {
    run_system_file_cleanups()

    devInst <- file.path(devRoot, "inst")
    baseEnv <- baseenv()

    makeShim <- function(original) {
      force(original)
      function(..., package = "base", lib.loc = NULL, mustWork = FALSE) {
        parts <- list(...)

        if (!missing(package) && identical(package, "PatientLevelPrediction") && is.null(lib.loc)) {
          target <- if (!length(parts)) devRoot else do.call(file.path, c(list(devInst), parts))
          if (!mustWork || (length(target) && all(file.exists(target)))) return(target)
        }

        original(..., package = package, lib.loc = lib.loc, mustWork = mustWork)
      }
    }

    patchEnv <- function(env) {
      original <- get("system.file", envir = env)
      bindingWasLocked <- bindingIsLocked("system.file", env)
      if (bindingWasLocked) unlockBinding("system.file", env)
      assign("system.file", makeShim(original), envir = env)
      if (bindingWasLocked) lockBinding("system.file", env)
      register_cleanup(function() {
        if (bindingIsLocked("system.file", env)) unlockBinding("system.file", env)
        assign("system.file", original, envir = env)
        if (bindingWasLocked) lockBinding("system.file", env)
      })
    }

    patchEnv(baseEnv)
    for (packageName in c("ResultModelManager", "SqlRender")) {
      if (rlang::is_installed(packageName)) {
        importsEnv <- parent.env(getNamespace(packageName))
        if (!identical(importsEnv, baseEnv) && exists("system.file", envir = importsEnv, inherits = FALSE)) {
          patchEnv(importsEnv)
        }
      }
    }
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
  reduceData <- function(data, n = 20, includeAge = FALSE) {
    covariates <- plpResult$model$covariateImportance %>%
      dplyr::slice_max(order_by = abs(.data$covariateValue), n = n, with_ties = FALSE) %>%
      dplyr::pull(.data$covariateId)
    if (includeAge) {
      covariates <- unique(c(covariates, 1002))
    }


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

  tinyTrainData <- reduceData(trainData, includeAge = TRUE)  
  tinyTestData <- reduceData(testData, includeAge = TRUE)

  oneTrainData <- copyTrainData(tinyTrainData)
  oneTrainData$covariateData$covariates <- oneTrainData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == 1002)
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
      runFeatureEngineering = FALSE,
      runPreprocessData = TRUE,
      runModelDevelopment = TRUE,
      runCovariateSummary = FALSE
    ),
    saveDirectory = file.path(saveLoc, "tinyResults")
  )
}

register_cleanup(function() {
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
})
