# @file RunMultiplePlp.R
#
# Copyright 2025 Observational Health Data Sciences and Informatics
#
# This file is part of PatientLevelPrediction
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


#' Run a list of predictions analyses
#'
#' @details
#' This function will run all specified predictions as defined using .
#'
#' @param databaseDetails               The database settings created using \code{createDatabaseDetails()}
#' @param modelDesignList                A list of model designs created using \code{createModelDesign()}
#' @param onlyFetchData                  Only fetches and saves the data object to the output folder without running the analysis.
#' @param cohortDefinitions               A list of cohort definitions for the target and outcome cohorts
#' @param logSettings                    The setting specifying the logging for the analyses created using \code{createLogSettings()}
#' @param saveDirectory                   Name of the folder where all the outputs will written to.
#' @param sqliteLocation                 (optional) The location of the sqlite database with the results
#'
#' @return
#' A data frame with the following columns: \tabular{ll}{ \verb{analysisId} \tab The unique identifier
#' for a set of analysis choices.\cr \verb{targetId} \tab The ID of the target cohort populations.\cr
#' \verb{outcomeId} \tab The ID of the outcomeId.\cr \verb{dataLocation} \tab The location where the plpData was saved
#'  \cr \verb{the settings ids} \tab The ids for all other settings used for model development.\cr }
#'
#' @examplesIf rlang::is_installed("Eunomia") && rlang::is_installed("curl") && curl::has_internet()
#' \donttest{ \dontshow{ # takes too long }
#' connectionDetails <- Eunomia::getEunomiaConnectionDetails()
#' databaseDetails <- createDatabaseDetails(connectionDetails = connectionDetails,
#'                                          cdmDatabaseSchema = "main",
#'                                          cohortDatabaseSchema = "main",
#'                                          cohortTable = "cohort",
#'                                          outcomeDatabaseSchema = "main",
#'                                          outcomeTable = "cohort",
#'                                          targetId = 1,
#'                                          outcomeIds = 2)
#' Eunomia::createCohorts(connectionDetails = connectionDetails)
#' covariateSettings <- 
#'  FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE,
#'                                             useDemographicsAge = TRUE,
#'                                             useConditionOccurrenceLongTerm = TRUE)
#' # GI Bleed in users of celecoxib
#' modelDesign <- createModelDesign(targetId = 1, 
#'                                  outcomeId = 3, 
#'                                  modelSettings = setLassoLogisticRegression(seed = 42),
#'                                  populationSettings = createStudyPopulationSettings(),
#'                                  restrictPlpDataSettings = createRestrictPlpDataSettings(),
#'                                  covariateSettings = covariateSettings,
#'                                  splitSettings = createDefaultSplitSetting(splitSeed = 42),
#'                                  preprocessSettings = createPreprocessSettings())
#' # GI Bleed in users of NSAIDs
#' modelDesign2 <- createModelDesign(targetId = 4,
#'                                   outcomeId = 3,
#'                                   modelSettings = setLassoLogisticRegression(seed = 42),
#'                                   populationSettings = createStudyPopulationSettings(),
#'                                   restrictPlpDataSettings = createRestrictPlpDataSettings(),
#'                                   covariateSettings = covariateSettings,
#'                                   splitSettings = createDefaultSplitSetting(splitSeed = 42),
#'                                   preprocessSettings = createPreprocessSettings())
#' saveLoc <- file.path(tempdir(), "runMultiplePlp")
#' multipleResults <- runMultiplePlp(databaseDetails = databaseDetails,
#'                                   modelDesignList = list(modelDesign, modelDesign2),
#'                                   saveDirectory = saveLoc)
#' # You should see results for two developed models in the ouutput. The output is as well
#' # uploaded to a sqlite database in the saveLoc/sqlite folder, 
#' dir(saveLoc)
#' # The dir output should show two Analysis_ folders with the results, 
#' # two targetId_ folders with th extracted data, and a sqlite folder with the database
#' # The results can be explored in the shiny app by calling viewMultiplePlp(saveLoc)
#'
#' # clean up (viewing the results in the shiny app is won't work after this)
#' unlink(saveLoc, recursive = TRUE)
#' }
#' @export
runMultiplePlp <- function(
    databaseDetails = createDatabaseDetails(),
    modelDesignList = list(
      createModelDesign(targetId = 1, outcomeId = 2, modelSettings = setLassoLogisticRegression()),
      createModelDesign(targetId = 1, outcomeId = 3, modelSettings = setLassoLogisticRegression())
    ),
    onlyFetchData = FALSE,
    cohortDefinitions = NULL,
    logSettings = createLogSettings(
      verbosity = "DEBUG",
      timeStamp = TRUE,
      logName = "runPlp Log"
    ),
    saveDirectory = NULL,
    sqliteLocation = file.path(saveDirectory, "sqlite")) {
  # input checks
  if (is.null(saveDirectory)) {
    stop("saveDirectory must be specified")
  }

  checkIsClass(databaseDetails, c("databaseDetails"))
  checkIsClass(modelDesignList, c("list", "modelDesign"))
  checkIsClass(onlyFetchData, "logical")
  checkIsClass(logSettings, "logSettings")
  checkIsClass(saveDirectory, "character")
  if (!dir.exists(saveDirectory)) {
    dir.create(saveDirectory, recursive = TRUE)
  }

  settingstable <- convertToJson(modelDesignList, cohortDefinitions)

  if (nrow(settingstable) != length(modelDesignList)) {
    stop("Error in settingstable")
  }

  # save the settings - TODO change this to save jsons in csv
  utils::write.csv(
    x = settingstable %>% dplyr::select(
      "analysisId",
      "targetId",
      "targetName",
      "outcomeId",
      "outcomeName",
      "dataLocation"
    ),
    file.path(saveDirectory, "settings.csv"),
    row.names = FALSE
  )

  # group the outcomeIds per combination of data extraction settings
  dataSettings <- settingstable %>%
    dplyr::group_by(
      .data$targetId,
      .data$covariateSettings,
      .data$restrictPlpDataSettings,
      .data$dataLocation
    ) %>%
    dplyr::summarise(
      outcomeIds = paste(unique(.data$outcomeId), collapse = ",")
    )

  # extract data
  for (i in 1:nrow(as.data.frame(dataSettings))) {
    dataExists <- length(dir(file.path(saveDirectory, dataSettings$dataLocation[i]))) > 0
    if (!dataExists) {
      ParallelLogger::logInfo(paste("Extracting data for cohort", dataSettings$targetId[i], "to", file.path(saveDirectory, dataSettings$dataLocation[i])))

      databaseDetails$targetId <- dataSettings$targetId[i]
      databaseDetails$outcomeIds <- strsplit(dataSettings$outcomeIds[i], ",")[[1]]

      plpDataSettings <- list(
        databaseDetails = databaseDetails,
        covariateSettings = ParallelLogger::convertJsonToSettings(dataSettings$covariateSettings[i]),
        restrictPlpDataSettings = ParallelLogger::convertJsonToSettings(dataSettings$restrictPlpDataSettings[i])
      )

      plpData <- tryCatch(
        {
          do.call(getPlpData, plpDataSettings)
        },
        error = function(e) {
          ParallelLogger::logInfo(e)
          return(NULL)
        }
      )
      if (!is.null(plpData)) {
        savePlpData(plpData, file.path(saveDirectory, dataSettings$dataLocation[i]))
      }
    } else {
      ParallelLogger::logInfo(paste("Data for target", dataSettings$targetId[i], "exists at", file.path(saveDirectory, dataSettings$dataLocation[i])))
    }
  }

  # runDiagnosis - NEW
  if (!onlyFetchData) {
    for (i in 1:nrow(as.data.frame(settingstable))) {
      modelDesign <- modelDesignList[[i]]
      settings <- settingstable[i, ] # just the data locations?

      dataExists <- length(dir(file.path(saveDirectory, settings$dataLocation))) > 0

      if (dataExists) {
        analysisExists <- file.exists(file.path(saveDirectory, settings$analysisId, "diagnosePlp.rds"))

        if (!analysisExists) {
          plpData <- PatientLevelPrediction::loadPlpData(file.path(saveDirectory, settings$dataLocation))
          diagnosePlpSettings <- list(
            plpData = plpData,
            outcomeId = modelDesign$outcomeId,
            analysisId = settings$analysisId,
            populationSettings = modelDesign$populationSettings,
            splitSettings = modelDesign$splitSettings,
            sampleSettings = modelDesign$sampleSettings,
            featureEngineeringSettings = modelDesign$featureEngineeringSettings,
            preprocessSettings = modelDesign$preprocessSettings,
            modelSettings = modelDesign$modelSettings,
            logSettings = logSettings,
            saveDirectory = saveDirectory
          )

          diagnose <- tryCatch(
            {
              do.call(diagnosePlp, diagnosePlpSettings)
            },
            error = function(e) {
              ParallelLogger::logInfo(e)
              return(NULL)
            }
          )
        } else {
          ParallelLogger::logInfo(paste("Diagnosis ", settings$analysisId, "exists at", file.path(saveDirectory, settings$analysisId)))
        }
      }
    }
  }

  # runPlp
  if (!onlyFetchData) {
    for (i in 1:nrow(as.data.frame(settingstable))) {
      modelDesign <- modelDesignList[[i]]
      settings <- settingstable[i, ] # just the data locations?

      dataExists <- length(dir(file.path(saveDirectory, settings$dataLocation))) > 0

      if (dataExists) {
        analysisExists <- file.exists(file.path(saveDirectory, settings$analysisId, "plpResult", "runPlp.rds"))

        if (!analysisExists) {
          plpData <- PatientLevelPrediction::loadPlpData(file.path(saveDirectory, settings$dataLocation))
          runPlpSettings <- list(
            plpData = quote(plpData),
            outcomeId = modelDesign$outcomeId,
            analysisId = settings$analysisId,
            populationSettings = modelDesign$populationSettings,
            splitSettings = modelDesign$splitSettings,
            sampleSettings = modelDesign$sampleSettings,
            featureEngineeringSettings = modelDesign$featureEngineeringSettings,
            preprocessSettings = modelDesign$preprocessSettings,
            modelSettings = modelDesign$modelSettings,
            logSettings = logSettings,
            executeSettings = modelDesign$executeSettings,
            saveDirectory = saveDirectory
          )

          result <- tryCatch(
            {
              do.call(runPlp, runPlpSettings)
            },
            error = function(e) {
              ParallelLogger::logInfo(e)
              return(NULL)
            }
          )
        } else {
          ParallelLogger::logInfo(paste("Analysis ", settings$analysisId, "exists at", file.path(saveDirectory, settings$analysisId)))
        }
      }
    } # end run per setting
  }

  # [TODO] add code to create sqlite database and populate with results...
  if (!onlyFetchData) {
    insertResultsToSqlite(
      resultLocation = saveDirectory,
      cohortDefinitions = cohortDefinitions,
      databaseList = createDatabaseList(
        cdmDatabaseSchemas = databaseDetails$cohortDatabaseSchema,
        cdmDatabaseNames = databaseDetails$cdmDatabaseName,
        databaseRefIds = databaseDetails$cdmDatabaseId
      ),
      sqliteLocation = sqliteLocation
    )
  }


  return(invisible(settingstable))
}


#' Specify settings for developing a single model
#'
#' @details
#' This specifies a single analysis for developing as single model
#'
#' @param targetId              The id of the target cohort that will be used for data extraction (e.g., the ATLAS id)
#' @param outcomeId              The id of the outcome that will be used for data extraction (e.g., the ATLAS id)
#' @param restrictPlpDataSettings       The settings specifying the extra restriction settings when extracting the data created using \code{createRestrictPlpDataSettings()}.
#' @param populationSettings             The population settings specified by \code{createStudyPopulationSettings()}
#' @param covariateSettings              The covariate settings, this can be a list or a single \code{'covariateSetting'} object.
#' @param featureEngineeringSettings      Either NULL or an object of class \code{featureEngineeringSettings} specifying any feature engineering used during model development
#' @param sampleSettings                  Either NULL or an object of class \code{sampleSettings} with the over/under sampling settings used for model development
#' @param preprocessSettings              Either NULL or an object of class \code{preprocessSettings} created using \code{createPreprocessingSettings()}
#' @param modelSettings                   The model settings such as \code{setLassoLogisticRegression()}
#' @param splitSettings                  The train/validation/test splitting used by all analyses created using \code{createDefaultSplitSetting()}
#' @param runCovariateSummary             Whether to run the covariateSummary
#'
#' @return
#' A list with analysis settings used to develop a single prediction model
#' 
#' @examples
#' # L1 logistic regression model to predict the outcomeId 2 using the targetId 2
#' # with with default population, restrictPlp, split, and covariate settings
#' createModelDesign(
#'   targetId = 1,
#'   outcomeId = 2,
#'   modelSettings = setLassoLogisticRegression(seed=42),
#'   populationSettings = createStudyPopulationSettings(),
#'   restrictPlpDataSettings = createRestrictPlpDataSettings(),
#'   covariateSettings = FeatureExtraction::createDefaultCovariateSettings(),
#'   splitSettings = createDefaultSplitSetting(splitSeed = 42),
#'   runCovariateSummary = TRUE
#' )
#' @export
createModelDesign <- function(
    targetId = NULL,
    outcomeId = NULL,
    restrictPlpDataSettings = createRestrictPlpDataSettings(),
    populationSettings = createStudyPopulationSettings(),
    covariateSettings = FeatureExtraction::createDefaultCovariateSettings(),
    featureEngineeringSettings = NULL,
    sampleSettings = NULL,
    preprocessSettings = NULL,
    modelSettings = NULL,
    splitSettings = createDefaultSplitSetting(),
    runCovariateSummary = TRUE) {
  checkIsClass(targetId, c("numeric", "integer", "NULL"))
  checkIsClass(outcomeId, c("numeric", "integer", "NULL"))

  checkIsClass(populationSettings, c("populationSettings", "NULL"))
  checkIsClass(restrictPlpDataSettings, c("restrictPlpDataSettings", "NULL"))
  checkIsClass(covariateSettings, c("covariateSettings", "list", "NULL"))
  checkIsClass(splitSettings, c("splitSettings", "NULL"))

  useFE <- FALSE
  if (!is.null(featureEngineeringSettings)) {
    if (inherits(featureEngineeringSettings, "featureEngineeringSettings")) {
      featureEngineeringSettings <- list(featureEngineeringSettings)
    }
    lapply(featureEngineeringSettings, function(x) checkIsClass(x, c("featureEngineeringSettings")))
    useFE <- TRUE
  } else {
    featureEngineeringSettings <- list(createFeatureEngineeringSettings(type = "none"))
  }

  useSample <- FALSE
  if (!is.null(sampleSettings)) {
    if (inherits(sampleSettings, "sampleSettings")) {
      sampleSettings <- list(sampleSettings)
    }
    lapply(sampleSettings, function(x) checkIsClass(x, c("sampleSettings")))

    useSample <- TRUE
  } else {
    sampleSettings <- list(createSampleSettings(type = "none"))
  }

  usePreprocess <- FALSE
  if (!is.null(preprocessSettings)) {
    checkIsClass(preprocessSettings, c("preprocessSettings"))
    usePreprocess <- TRUE
  } else {
    preprocessSettings <- createPreprocessSettings(
      minFraction = 0.001,
      normalize = TRUE
    )
  }

  checkIsClass(modelSettings, c("modelSettings"))

  settings <- list(
    targetId = targetId,
    outcomeId = outcomeId,
    restrictPlpDataSettings = restrictPlpDataSettings,
    covariateSettings = covariateSettings,
    populationSettings = populationSettings,
    sampleSettings = sampleSettings,
    featureEngineeringSettings = featureEngineeringSettings,
    preprocessSettings = preprocessSettings,
    modelSettings = modelSettings,
    splitSettings = splitSettings,
    executeSettings = createExecuteSettings(
      runSplitData = TRUE,
      runSampleData = useSample,
      runFeatureEngineering = useFE,
      runPreprocessData = usePreprocess,
      runModelDevelopment = !is.null(modelSettings),
      runCovariateSummary = runCovariateSummary
    )
  )

  class(settings) <- "modelDesign"
  return(settings)
}


#' Save the modelDesignList to a json file
#'
#' @details
#' This function creates a json file with the modelDesignList saved
#'
#' @param modelDesignList          A list of modelDesigns created using \code{createModelDesign()}
#' @param cohortDefinitions        A list of the cohortDefinitions (generally extracted from ATLAS)
#' @param saveDirectory            The directory to save the modelDesignList settings
#'
#' @return The json string of the ModelDesignList
#'
#' @examples
#' modelDesign <- createModelDesign(targetId = 1, 
#'                                  outcomeId = 2,
#'                                  modelSettings = setLassoLogisticRegression())
#' saveLoc <- file.path(tempdir(), "loadPlpAnalysesJson")
#' jsonFile <- savePlpAnalysesJson(modelDesignList = modelDesign, saveDirectory = saveLoc)
#' # clean up
#' unlink(saveLoc, recursive = TRUE)
#' @export
savePlpAnalysesJson <- function(
    modelDesignList = list(
      createModelDesign(targetId = 1, outcomeId = 2, modelSettings = setLassoLogisticRegression()),
      createModelDesign(targetId = 1, outcomeId = 3, modelSettings = setLassoLogisticRegression())
    ),
    cohortDefinitions = NULL,
    # add cohortDefinitions
    saveDirectory = NULL) {
  if (inherits(modelDesignList, "modelDesign")) {
    modelDesignList <- list(modelDesignList)
  }

  lapply(modelDesignList, function(x) {
    checkIsClass(x, "modelDesign")
  })

  if (!is.null(saveDirectory)) {
    checkIsClass(saveDirectory, "character")

    if (!dir.exists(saveDirectory)) {
      dir.create(saveDirectory, recursive = TRUE)
    }

    ParallelLogger::saveSettingsToJson(
      object = list(
        plpVersion = as.character(utils::packageVersion("PatientLevelPrediction")),
        analyses = modelDesignList,
        cohortDefinitions = cohortDefinitions
      ),
      fileName = file.path(saveDirectory, "predictionAnalysisList.json")
    )

    return(file.path(saveDirectory, "predictionAnalysisList.json"))
  }

  return(
    ParallelLogger::convertSettingsToJson(
      object = list(
        plpVersion = as.character(utils::packageVersion("PatientLevelPrediction")),
        analyses = modelDesignList,
        cohortDefinitions = cohortDefinitions
      )
    )
  )
}


#' Load the multiple prediction json settings from a file
#'
#' @details
#' This function interprets a json with the multiple prediction settings and creates a list
#' that can be combined with connection settings to run a multiple prediction study
#'
#' @param jsonFileLocation    The location of the file 'predictionAnalysisList.json' with the modelDesignList
#' @return A list with the modelDesignList and cohortDefinitions
#' @examples
#' modelDesign <- createModelDesign(targetId = 1, outcomeId = 2, 
#'                                  modelSettings = setLassoLogisticRegression())
#' saveLoc <- file.path(tempdir(), "loadPlpAnalysesJson")
#' savePlpAnalysesJson(modelDesignList = modelDesign, saveDirectory = saveLoc)
#' loadPlpAnalysesJson(file.path(saveLoc, "predictionAnalysisList.json"))
#' # clean use
#' unlink(saveLoc, recursive = TRUE)
#' @export
loadPlpAnalysesJson <- function(
    jsonFileLocation) {
  checkIsClass(jsonFileLocation, "character")
  if (!file.exists(jsonFileLocation)) {
    ParallelLogger::logError("Invalid directory - does not exist")
  }

  rList <- ParallelLogger::loadSettingsFromJson(fileName = jsonFileLocation)

  return(rList)
}





#' externally validate the multiple plp models across new datasets
#' @description
#' This function loads all the models in a multiple plp analysis folder and
#' validates the models on new data
#' @details
#' Users need to input a location where the results of the multiple plp analyses
#' are found and the connection and database settings for the new data
#'
#' @param analysesLocation                The location where the multiple plp analyses are
#' @param validationDatabaseDetails       A single or list of validation database settings created using \code{createDatabaseDetails()}
#' @param validationRestrictPlpDataSettings  The settings specifying the extra restriction settings when extracting the data created using \code{createRestrictPlpDataSettings()}.
#' @param recalibrate                      A vector of recalibration methods (currently supports 'RecalibrationintheLarge' and/or 'weakRecalibration')
#' @param cohortDefinitions           A list of cohortDefinitions
#' @param saveDirectory               The location to save to validation results
#' @return Nothing. The results are saved to the saveDirectory
#'
#' @examplesIf rlang::is_installed("Eunomia") && rlang::is_installed("curl") && curl::has_internet()
#' \donttest{ \dontshow{ # takes too long to run }
#' # first develop a model using runMultiplePlp
#' connectionDetails <- Eunomia::getEunomiaConnectionDetails()
#' Eunomia::createCohorts(connectionDetails = connectionDetails)
#' databaseDetails <- createDatabaseDetails(connectionDetails = connectionDetails,
#'                                          cdmDatabaseId = "1",
#'                                          cdmDatabaseName = "Eunomia",
#'                                          cdmDatabaseSchema = "main",
#'                                          targetId = 1,
#'                                          outcomeIds = 3)
#' covariateSettings <- 
#'  FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE,
#'    useDemographicsAge = TRUE, useConditionOccurrenceLongTerm = TRUE)
#' modelDesign <- createModelDesign(targetId = 1, 
#'                                  outcomeId = 3,
#'                                  modelSettings = setLassoLogisticRegression(seed = 42),
#'                                  covariateSettings = covariateSettings)
#' saveLoc <- file.path(tempdir(), "valiateMultiplePlp", "development")
#' results <- runMultiplePlp(databaseDetails = databaseDetails,
#'                modelDesignList = list(modelDesign),
#'                saveDirectory = saveLoc)
#' # now validate the model on a Eunomia but with a different target
#' analysesLocation <- saveLoc
#' validationDatabaseDetails <- createDatabaseDetails(connectionDetails = connectionDetails,
#'                                                    cdmDatabaseId = "2",
#'                                                    cdmDatabaseName = "EunomiaNew",
#'                                                    cdmDatabaseSchema = "main",
#'                                                    targetId = 4,
#'                                                    outcomeIds = 3)
#' newSaveLoc <- file.path(tempdir(), "valiateMultiplePlp", "validation")
#' validateMultiplePlp(analysesLocation = analysesLocation,
#'                     validationDatabaseDetails = validationDatabaseDetails,
#'                     saveDirectory = newSaveLoc)
#' # the results could now be viewed in the shiny app with viewMultiplePlp(newSaveLoc)
#' }
#' @export
validateMultiplePlp <- function(
    analysesLocation,
    validationDatabaseDetails,
    validationRestrictPlpDataSettings = createRestrictPlpDataSettings(),
    recalibrate = NULL,
    cohortDefinitions = NULL,
    saveDirectory = NULL) {
  # add input checks
  checkIsClass(analysesLocation, "character")

  if (inherits(validationDatabaseDetails, "databaseDetails")) {
    validationDatabaseDetails <- list(validationDatabaseDetails)
  }
  lapply(
    validationDatabaseDetails,
    function(x) {
      checkIsClass(x, "databaseDetails")
    }
  )

  checkIsClass(validationRestrictPlpDataSettings, "restrictPlpDataSettings")

  checkIsClass(recalibrate, c("character", "NULL"))
  checkIsClass(saveDirectory, c("character", "NULL"))

  # for each model run externalValidateDbPlp()
  analyses <- dir(analysesLocation, recursive = FALSE, full.names = FALSE)

  # now fine all analysis folders..
  analyses <- analyses[grep("Analysis_", analyses)]

  for (i in 1:length(analyses)) {
    if (is.null(saveDirectory)) {
      saveLocation <- file.path(analysesLocation, "Validation")
    } else {
      saveLocation <- saveDirectory
    }

    analysis <- analyses[i]
    modelSettings <- file.path(analysesLocation, analysis)

    ParallelLogger::logInfo(paste0("Evaluating model in ", modelSettings))

    if (dir.exists(file.path(modelSettings, "plpResult"))) {
      ParallelLogger::logInfo(paste0("plpResult found in ", modelSettings))

      plpModel <- loadPlpModel(file.path(modelSettings, "plpResult", "model"))

      validations <- tryCatch(
        {
          externalValidateDbPlp(
            plpModel = plpModel,
            validationDatabaseDetails = validationDatabaseDetails,
            validationRestrictPlpDataSettings = validationRestrictPlpDataSettings,
            settings = createValidationSettings(
              recalibrate = recalibrate,
              runCovariateSummary = FALSE
            ),
            outputFolder = saveLocation
          )
        },
        error = function(cont) {
          ParallelLogger::logInfo(paste0("Error: ", cont))
          return(NULL)
        }
      )
    }
  }

  # add to sqlite database - needed for shiny app
  # =======================

  if (saveLocation == file.path(analysesLocation, "Validation")) {
    ParallelLogger::logInfo("Saving validation results into the development sqlite database")
    sqliteLocation <- file.path(analysesLocation, "sqlite")
  } else {
    ParallelLogger::logInfo("Saving validation results into validation sqlite")
    sqliteLocation <- file.path(saveDirectory, "sqlite")
  }

  for (validationDatabaseDetail in validationDatabaseDetails) {
    tryCatch({
      insertResultsToSqlite(
        resultLocation = file.path(saveLocation, validationDatabaseDetail$cdmDatabaseName),
        cohortDefinitions = cohortDefinitions,
        databaseList = createDatabaseList(
          cdmDatabaseSchemas = validationDatabaseDetail$cdmDatabaseSchema,
          cdmDatabaseNames = validationDatabaseDetail$cdmDatabaseName,
          databaseRefIds = validationDatabaseDetail$cdmDatabaseId
        ),
        sqliteLocation = sqliteLocation
      )
    })
  }
}

convertToJson <- function(
    modelDesignList,
    cohortDefinitions = NULL) {
  convertToJsonString <- function(x) {
    as.character(ParallelLogger::convertSettingsToJson(x))
  }

  if (is.null(cohortDefinitions)) {
    cohortIds <- unlist(
      lapply(
        X = 1:length(modelDesignList),
        FUN = function(i) {
          c(
            modelDesignList[[i]]$targetId,
            modelDesignList[[i]]$outcomeId
          )
        }
      )
    )
    cohortIds <- unique(cohortIds)

    cohortDefinitions <- data.frame(
      cohortId = cohortIds,
      cohortName = paste0("Cohort: ", cohortIds)
    )
  } else {
    cohortDefinitions <- cohortDefinitions %>%
      dplyr::select(
        "cohortId",
        "cohortName"
      )
  }

  result <- data.frame(
    analysisId = paste0("Analysis_", 1:length(modelDesignList)),
    targetId = unlist(lapply(modelDesignList, function(x) ifelse(is.null(x$targetId), x$cohortId, x$targetId))),
    outcomeId = unlist(lapply(modelDesignList, function(x) x$outcomeId)),
    covariateSettings = unlist(lapply(modelDesignList, function(x) convertToJsonString(x$covariateSettings))),
    restrictPlpDataSettings = unlist(lapply(modelDesignList, function(x) convertToJsonString(x$restrictPlpDataSettings))),
    populationSettings = unlist(lapply(modelDesignList, function(x) convertToJsonString(x$populationSettings))),
    sampleSettings = unlist(lapply(modelDesignList, function(x) convertToJsonString(x$sampleSettings))),
    splitSettings = unlist(lapply(modelDesignList, function(x) convertToJsonString(x$splitSettings))),
    featureEngineeringSettings = unlist(lapply(modelDesignList, function(x) convertToJsonString(x$featureEngineeringSettings))),
    preprocessSettings = unlist(lapply(modelDesignList, function(x) convertToJsonString(x$preprocessSettings))),
    modelSettings = unlist(lapply(modelDesignList, function(x) convertToJsonString(x$modelSettings))),
    executeSettings = unlist(lapply(modelDesignList, function(x) convertToJsonString(x$executeSettings)))
  )

  result <- result %>%
    dplyr::left_join(cohortDefinitions, by = c("outcomeId" = "cohortId")) %>%
    dplyr::rename(outcomeName = "cohortName") %>%
    dplyr::left_join(cohortDefinitions, by = c("targetId" = "cohortId")) %>%
    dplyr::rename(targetName = "cohortName") # new

  # get the names
  uniqueSettings <- result %>%
    dplyr::distinct(
      .data$targetId,
      .data$covariateSettings,
      .data$restrictPlpDataSettings
    ) %>%
    dplyr::group_by(.data$targetId) %>%
    dplyr::mutate(dataLocation = paste0("targetId_", .data$targetId, "_L", dplyr::row_number()))

  # add the data names
  result <- result %>%
    dplyr::left_join(
      uniqueSettings,
      by = c(
        "targetId" = "targetId",
        "covariateSettings" = "covariateSettings",
        "restrictPlpDataSettings" = "restrictPlpDataSettings"
      )
    )

  return(result)
}
