# @file ExternalValidatePlp.R
#
# Copyright 2021 Observational Health Data Sciences and Informatics
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


externalValidatePlp <- function(plpModel,
                                plpData,
                                population,
                                settings = list(# add covariateSummary option?
                                  recalibrate = 'weakRecalibration')) {
  # Apply model
  #=======
  prediction <- tryCatch({
    predictPlp(plpModel = plpModel,
               plpData = plpData,
               population = population)
  },
  error = function(e) {
    ParallelLogger::logError(e)
  })
  
  prediction$evaluationType <- 'Validation'
  
  # Recalibrate
  #=======
  if (!is.null(settings$recalibrate)) {
    for (recalMethod in settings$recalibrate) {
      prediction <-
        tryCatch({
          recalibratePlp(prediction = prediction, method = recalMethod)
        },
        error = function(e) {
          ParallelLogger::logError(e)
        })
    }
  }
  
  # Evaluate
  #=======
  performance <- tryCatch({
    evaluatePlp(prediction = prediction, typeColumn = 'evaluationType')
  },
  error = function(e) {
    ParallelLogger::logError(e)
  })
  
  # step 6: covariate summary
  labels <- tryCatch({
    population %>% dplyr::select("rowId", "outcomeCount")
  },
  error = function(e) {
    return(NULL)
  })
  
  if (settings$runCovariateSummary) {
    covariateSum <- tryCatch({
      covariateSummary(
        covariateData = plpData$covariateData,
        cohort = population[, colnames(population) != 'outcomeCount'],
        labels = labels,
        variableImportance = plpModel$covariateImportance %>% dplyr::select("covariateId", "covariateValue")
      )
    },
    error = function(e) {
      ParallelLogger::logInfo(e)
      return(NULL)
    })
  } else{
    covariateSum <- NULL
  }
  
  executionSummary <- list(
    ExecutionDateTime = Sys.Date(),
    PackageVersion = list(
      rVersion = R.Version()$version.string,
      packageVersion = utils::packageVersion("PatientLevelPrediction")
    ),
    PlatformDetails = list(
      platform = R.Version()$platform,
      cores = Sys.getenv('NUMBER_OF_PROCESSORS'),
      RAM = memuse::Sys.meminfo()[1]
    ) #  test for non-windows needed
  )
  
  model = list(
    model = 'external validation of model',
    modelDesign = plpModel$modelDesign,
    # was settings
    validationDetails   = list(
      analysisId = '',
      #TODO add from model
      analysisSource = '',
      #TODO add from model
      developmentDatabase = plpModel$trainDetails$developmentDatabase,
      developmentDatabaseId = plpModel$trainDetails$developmentDatabaseId,
      validationDatabase = plpData$metaData$databaseDetails$cdmDatabaseSchema,
      validationDatabaseId = plpData$metaData$databaseDetails$cdmDatabaseId,
      populationSettings = attr(population, 'metaData')$populationSettings,
      restrictPlpDataSettings = plpData$metaData$restrictPlpDataSettings,
      outcomeId = attr(population, 'metaData')$outcomeId,
      targetId = plpData$metaData$databaseDetails$targetId,
      attrition = attr(population, 'metaData')$attrition,
      validationDate = Sys.Date() # is this needed?
    )
  )
  attr(model, "predictionFunction") <- 'none'
  attr(model, "saveType") <- 'RtoJson'
  class(model) <- 'plpModel'
  
  result <- list(
    model = model,
    executionSummary = executionSummary,
    prediction = prediction,
    performanceEvaluation = performance,
    covariateSummary = covariateSum
  )
  
  class(result) <- 'externalValidatePlp'
  return(result)
  
}


#' externalValidateDbPlp - Validate a model on new databases
#'
#' @description
#' This function extracts data using a user specified connection and cdm_schema, applied the model and then calcualtes the performance
#' @details
#' Users need to input a trained model (the output of runPlp()) and new database connections. The function will return a list of length equal to the
#' number of cdm_schemas input with the performance on the new data
#'
#' @param plpModel                    The model object returned by runPlp() containing the trained model
#' @param validationDatabaseDetails   A list of objects of class \code{databaseDetails} created using \code{createDatabaseDetails}
#' @param validationRestrictPlpDataSettings   A list of population restriction settings created by \code{createRestrictPlpDataSettings()}
#' @param settings                    A settings object of class \code{validationSettings} created using \code{createValidationSettings}
#' @param logSettings                 An object of \code{logSettings} created using \code{createLogSettings}
#'                                    specifying how the logging is done
#' @param outputFolder                The directory to save the validation results to (subfolders are created per database in validationDatabaseDetails)
#'
#' @return
#' A list containing the performance for each validation_schema
#'
#'
#' @export
externalValidateDbPlp <- function(plpModel,
                                  validationDatabaseDetails = createDatabaseDetails(),
                                  validationRestrictPlpDataSettings = createRestrictPlpDataSettings(),
                                  settings = createValidationSettings(recalibrate = 'weakRecalibration'),
                                  logSettings = createLogSettings(verbosity = 'INFO', logName = 'validatePLP'),
                                  outputFolder = getwd()) {
  # Input checks
  #=======
  
  checkIsClass(plpModel, 'plpModel')
  
  # check the class and make a list if a single database setting
  if (inherits(validationDatabaseDetails, 'list')) {
    lapply(validationDatabaseDetails, function(x)
      checkIsClass(x, 'databaseDetails'))
  } else{
    checkIsClass(validationDatabaseDetails, 'databaseDetails')
    validationDatabaseDetails <- list(validationDatabaseDetails)
  }
  checkIsClass(validationRestrictPlpDataSettings,
               'restrictPlpDataSettings')
  checkIsClass(settings, 'validationSettings')
  
  # create results list with the names of the databases to validate across
  result <- list()
  length(result) <- length(validationDatabaseDetails)
  names(result) <-
    unlist(lapply(validationDatabaseDetails, function(x)
      attr(x, 'cdmDatabaseName')))
  
  for (databaseDetails in validationDatabaseDetails) {
    databaseName <- attr(databaseDetails, 'cdmDatabaseName')
    
    # initiate log
    logSettings$saveDirectory <-
      file.path(outputFolder,
                databaseName,
                plpModel$trainDetails$analysisId)
    logSettings$logFileName <- 'validationLog'
    logger <- do.call(createLog, logSettings)
    ParallelLogger::registerLogger(logger)
    
    ParallelLogger::logInfo(paste('Validating model on', databaseName))
    
    # Step 1: get data
    #=======
    
    getPlpDataSettings <- list(databaseDetails = databaseDetails,
                               restrictPlpDataSettings = validationRestrictPlpDataSettings)
    if (is.null(getPlpDataSettings$databaseDetails$targetId)) {
      ParallelLogger::logInfo("targetId not in databaseSettings so using model's")
      getPlpDataSettings$databaseDetails$targetId <-
        plpModel$modelDesign$targetId
    }
    if (is.null(getPlpDataSettings$databaseDetails$outcomeIds)) {
      ParallelLogger::logInfo("outcomeId not in databaseSettings  so using model's")
      getPlpDataSettings$databaseDetails$outcomeIds <-
        plpModel$modelDesign$outcomeId
    }
    
    if (is.null(getPlpDataSettings$restrictPlpDataSettings$firstExposureOnly)) {
      ParallelLogger::logInfo("firstExposureOnly not in restrictPlpDataSettings  so using model's")
      getPlpDataSettings$restrictPlpDataSettings$firstExposureOnly <-
        plpModel$modelDesign$restrictPlpDataSettings$firstExposureOnly
    }
    if (is.null(getPlpDataSettings$restrictPlpDataSettings$washoutPeriod)) {
      ParallelLogger::logInfo("washoutPeriod not in restrictPlpDataSettings so using model's")
      getPlpDataSettings$restrictPlpDataSettings$washoutPeriod <-
        plpModel$modelDesign$restrictPlpDataSettings$washoutPeriod
    }
    
    # TODO: we need to update this to restrict to model covariates and update custom features
    getPlpDataSettings$covariateSettings <-
      plpModel$modelDesign$covariateSettings
    
    plpData <- tryCatch({
      do.call(getPlpData, getPlpDataSettings)
    },
    error = function(e) {
      ParallelLogger::logError(e)
      return(NULL)
    })
    
    if (is.null(plpData)) {
      closeLog(logger)
    }
    
    # Step 2: create population
    #=======
    
    population <- tryCatch({
      do.call(
        createStudyPopulation,
        list(
          plpData = plpData,
          outcomeId = getPlpDataSettings$databaseDetails$outcomeIds,
          populationSettings = plpModel$modelDesign$populationSettings
        )
      )
    },
    error = function(e) {
      ParallelLogger::logError(e)
      return(NULL)
    })
    
    if (is.null(population)) {
      closeLog(logger)
    }
    
    # Step 3: Apply model to plpData and population
    #=======
    
    result[[databaseName]] <- tryCatch({
      externalValidatePlp(plpModel,
                          plpData,
                          #databaseName = databaseName,
                          population,
                          settings = settings)
    },
    error = function(e) {
      ParallelLogger::logInfo(e)
      return(NULL)
    })
    
    if (is.null(result[[databaseName]])) {
      closeLog(logger)
    } else{
      if (!dir.exists(file.path(
        outputFolder,
        databaseName,
        plpModel$trainDetails$analysisId
      ))) {
        dir.create(
          file.path(
            outputFolder,
            databaseName,
            plpModel$trainDetails$analysisId
          ),
          recursive = T
        )
      }
      
      savePlpResult(
        result[[databaseName]],
        dirPath = file.path(
          outputFolder,
          databaseName,
          plpModel$trainDetails$analysisId,
          'validationResult'
        )
      )
    }
  }
  
  # Now return results
  return(invisible(result))
}


#' createValidationSettings define optional settings for performing external validation
#'
#' @description
#' This function creates the settings required by externalValidatePlp
#' @details
#' Users need to specify whether they want to sample or recalibate when performing external validation
#'
#' @param recalibrate                      A vector of characters specifying the recalibration method to apply
#' @param runCovariateSummary              Whether to run the covariate summary for the validation data
#' @return
#' A setting object of class \code{validationSettings} containing a list of settings for externalValidatePlp
#'
#' @export
createValidationSettings <- function(recalibrate = NULL,
                                     runCovariateSummary = T) {
  checkIsClass(recalibrate, c('character', 'NULL'))
  if (!is.null(recalibrate)) {
    if (sum(recalibrate %in% c('recalibrationInTheLarge', 'weakRecalibration')) !=
        length(recalibrate)) {
      ParallelLogger::logError(
        'Incorrect recalibrate options used.  Must be recalibrationInTheLarge or weakRecalibration'
      )
    }
  }
  
  result <- list(recalibrate = recalibrate,
                 runCovariateSummary = runCovariateSummary)
  class(result) <- 'validationSettings'
  return(result)
}

#' createValidationDesign - Define the validation design for external validation
#'
#' @param targetId The targetId of the target cohort to validate on
#' @param outcomeId The outcomeId of the outcome cohort to validate on
#' @param populationSettings A list of population restriction settings created 
#' by \code{createPopulationSettings}. Default is NULL and then this is taken
#' from the model
#' @param restrictPlpDataSettings A list of plpData restriction settings
#' created by \code{createRestrictPlpDataSettings}. Default is NULL and then
#' this is taken from the model.
#' @param plpModelList A list of plpModels objects created by \code{runPlp} or a path to such objects
#' @param recalibrate A vector of characters specifying the recalibration method to apply,
#' @param runCovariateSummary whether to run the covariate summary for the validation data
#' @return A validation design object of class \code{validationDesign} or a list of such objects
#' @export
createValidationDesign <-
  function(targetId,
           outcomeId,
           plpModelList,
           populationSettings = NULL,
           restrictPlpDataSettings = NULL,
           recalibrate = NULL,
           runCovariateSummary = TRUE) {
    checkIsClass(targetId, c("numeric", "integer"))
    checkIsClass(outcomeId, c("numeric", "integer"))
    if (!is.null(populationSettings)) {
      checkIsClass(populationSettings, c("populationSettings"))
    }
    if (!is.null(restrictPlpDataSettings)) {  
      checkIsClass(restrictPlpDataSettings, "restrictPlpDataSettings")
    }
    checkIsClass(plpModelList, "list")
    lapply(plpModelList, function(x) {
      checkIsClass(x, c("plpModel", "character"))
    })
    checkIsClass(recalibrate, c("character", "NULL"))
    checkIsClass(runCovariateSummary, "logical")
    
    design <- list(
      targetId = targetId,
      outcomeId = outcomeId,
      populationSettings = populationSettings,
      plpModelList = plpModelList,
      restrictPlpDataSettings = restrictPlpDataSettings,
      recalibrate = recalibrate,
      runCovariateSummary = runCovariateSummary
    )
    class(design) <- "validationDesign"
    return(design)
  }


#' externalValidatePlp - Validate model performance on new data
#'
#' @param validationDesignList A list of objects created with \code{createValidationDesign}
#' @param databaseDetails A list of objects of class
#' \code{databaseDetails} created using \code{createDatabaseDetails}
#' @param logSettings               An object of \code{logSettings} created
#' using \code{createLogSettings}
#' @param outputFolder        The directory to save the validation results to
#' (subfolders are created per database in validationDatabaseDetails)
#' @export
validateExternal <- function(validationDesignList,
                             databaseDetails,
                             logSettings,
                             outputFolder) {
  # Input checks
  #=======
  changedInputs <- checkValidateExternalInputs(validationDesignList,
                                               databaseDetails,
                                               logSettings,
                                               outputFolder)
  validationDesignList <- changedInputs[["validationDesignList"]]
  databaseDetails <- changedInputs[["databaseDetails"]]
  # create results list with the names of the databases to validate across
  result <- list()
  length(result) <- length(databaseDetails)
  names(result) <-
    unlist(lapply(databaseDetails, function(x) {
      attr(x, "cdmDatabaseName")}))
  
  # Need to keep track of incremental analysisId's for each database
  databaseNames <- unlist(lapply(databaseDetails, function(x) {
    x$cdmDatabaseName}))
  analysisInfo <- list()
  for (name in databaseNames) {
    analysisInfo[name] <- 1
  }
  
  # initiate log
  logSettings$saveDirectory <- outputFolder
  logSettings$logFileName <- "validationLog"
  logger <- do.call(createLog, logSettings)
  ParallelLogger::registerLogger(logger)
  on.exit(closeLog(logger))
  
  # create download tasks
  extractUniqueCombinations <- function(validationDesignList) {
    j <- 1
    restrictContentMap <- list()
    uniqueCombinations <- do.call(rbind, lapply(seq_along(validationDesignList), function(i) {
      design <- validationDesignList[[i]]
      restrictContent <- paste0(design$restrictPlpDataSettings, collapse = "|")
      if (!(restrictContent %in% names(restrictContentMap))) {
       restrictContentMap[[restrictContent]] <<- j
        j <<- j + 1
      }
      data.frame(
        targetId = design$targetId,
        outcomeId = design$outcomeId,
        restrictPlpIndex = restrictContentMap[[restrictContent]],
        restrictPlpDataSettings = restrictContent
      )
    }))
    uniqueCombinations <- uniqueCombinations %>% 
      dplyr::group_by(.data$targetId, .data$restrictPlpDataSettings) %>%
        dplyr::summarise(outcomeIds = list(unique(.data$outcomeId)),
                         restrictPlpIndex = dplyr::first(.data$restrictPlpIndex), .groups = "drop") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(restrictPlpDataSettings = list(validationDesignList[[.data$restrictPlpIndex]]$restrictPlpDataSettings))
    return(uniqueCombinations)
  }
  downloadTasks <- extractUniqueCombinations(validationDesignList)




  results <- NULL
  for (design in validationDesignList) {
    for (database in databaseDetails) {
      databaseName <- database$cdmDatabaseName
      
      ParallelLogger::logInfo(paste("Validating model on", database$cdmDatabaseName))
      
      modelDesigns <- extractModelDesigns(design$plpModelList)
      allCovSettings <- lapply(modelDesigns, function(x) x$covariateSettings)
      design <- fromDesignOrModel(design, modelDesigns, "restrictPlpDataSettings")
      checkAllSameInModels(allCovSettings, "covariateSettings")
      
      # get plpData
      plpData <- getData(design, database, outputFolder, allCovSettings, downloadTasks) 
      if (is.null(plpData)) {
        ParallelLogger::logInfo("Couldn't extract plpData for the given design and database, proceeding to the next one.")
        next
      }
      # create study population
      population <- getPopulation(design, modelDesigns, plpData)

      results <- lapply(design$plpModelList, function(model) {
        analysisName <- paste0("Analysis_", analysisInfo[databaseName])
        analysisDone <- file.exists(
          file.path(
            outputFolder,
            databaseName,
            analysisName,
            "validationResult",
            "runPlp.rds"
          )
        )
        if (!analysisDone) {
          validateModel(
            plpModel = model,
            plpData = plpData,
            population = population,
            recalibrate = design$recalibrate,
            runCovariateSummary = design$runCovariateSummary,
            outputFolder = outputFolder,
            databaseName = databaseName,
            analysisName = analysisName)
        } else {
            ParallelLogger::logInfo(paste0("Analysis ", analysisName, " already done",
                                           ", Proceeding to the next one."))
        }
        analysisInfo[[databaseName]] <<- analysisInfo[[databaseName]] + 1
      })
    }
  }
  for (database in databaseDetails) {
    databaseName <- database$cdmDatabaseName
    sqliteLocation <-
      file.path(outputFolder, "sqlite")
    tryCatch({
      insertResultsToSqlite(
        resultLocation = file.path(outputFolder, databaseName),
        cohortDefinitions = NULL,
        databaseList = createDatabaseList(
          cdmDatabaseSchemas = database$cdmDatabaseSchema,
          cdmDatabaseNames = database$cdmDatabaseName,
          databaseRefIds = database$cdmDatabaseId
        ),
        sqliteLocation = sqliteLocation
      )
    },
    error = function(e) ParallelLogger::logInfo(e)
    )
  }
  return(invisible(results))
}

validateModel <-
  function(plpModel,
           plpData,
           population,
           recalibrate,
           runCovariateSummary,
           outputFolder,
           databaseName,
           analysisName) {
    if (is.character(plpModel)) {
      plpModel <- loadPlpModel(plpModel)
    }
    result <- externalValidatePlp(
      plpModel = plpModel,
      plpData = plpData,
      population = population,
      settings = list(recalibrate = recalibrate,
                      runCovariateSummary = runCovariateSummary)
    )
    savePlpResult(result,
                  dirPath = file.path(
                    outputFolder,
                    databaseName,
                    analysisName,
                    "validationResult"
                  ))
    return(result)
}

#' checkAllSameInModels - Check if all settings are the same across models
#' @param settingsList A list of settings to check
#' @param settingName The name of the setting to check
#' @keywords internal
checkAllSameInModels <- function(settingsList, settingName) {
  if (!Reduce(function(x, y) {
    x &&
    identical(y, settingsList[[1]])},
    settingsList[-1],
    init = TRUE)) {
    stop(paste0(settingName, "are not the same across models which is not supported yet"))
  }
}

#' extractModelDesigns - Extract all modelDesigns from a list of plpModels
#' @param plpModelList A list of plpModels
#' @return A list of modelDesigns
#' @keywords internal
extractModelDesigns <- function(plpModelList) {
  lapply(plpModelList, function(plpModel) {
    if (is.character(plpModel)) {
      modelDesign <- ParallelLogger::loadSettingsFromJson(
        normalizePath(file.path(plpModel, "modelDesign.json"))
      )
      return(modelDesign)
    } else {
    plpModel$modelDesign
    }
  })
}

#' checkValidateExternalInputs - Check the inputs for validateExternal
#' @param validationDesignList A list of validationDesign objects
#' @param databaseDetails A list of databaseDetails objects
#' @param logSettings An object of logSettings
#' @param outputFolder The directory to save the validation results to
#' @return A list of inputs that were modified
#' @keywords internal
checkValidateExternalInputs <- function(validationDesignList,
                                        databaseDetails,
                                        logSettings,
                                        outputFolder) {
  if (inherits(validationDesignList, "list")) {
    lapply(validationDesignList, function(x) {
      checkIsClass(x, "validationDesign")})
  } else {
    checkIsClass(validationDesignList, "validationDesign")
    validationDesignList <- list(validationDesignList)
  }
 
  # check the class and make a list if a single database setting
  if (inherits(databaseDetails, "list")) {
    lapply(databaseDetails, function(x) {
      checkIsClass(x, "databaseDetails")})
  } else {
    checkIsClass(databaseDetails, "databaseDetails")
    databaseDetails <- list(databaseDetails)
  }
  results <- list(validationDesignList = validationDesignList,
                  databaseDetails = databaseDetails)
  return(results)
}

#' fromDesignOrModel - Check if the design has the setting, if not use the model's
#' @param validationDesign The validationDesign object
#' @param modelDesigns A list of modelDesign objects
#' @param settingName The name of the setting to check
#' @return The updated design
#' @keywords internal
fromDesignOrModel <- function(validationDesign, modelDesigns, settingName) {
  settingsFromModel <- lapply(modelDesigns, function(x) x[[settingName]])
  if (is.null(validationDesign[[settingName]])) {
    checkAllSameInModels(settingsFromModel, settingName)
    validationDesign[[settingName]] <- settingsFromModel[[1]]
    ParallelLogger::logInfo(paste0(settingName, " not set in design, using model's"))
  } else {
    if (any(unlist(lapply(modelDesigns, function(x) {
            !identical(x[[settingName]], validationDesign[[settingName]])
          })))) {
      ParallelLogger::logWarn(settingName, " are not the same in models and validationDesign") 
    }
  }
  return(validationDesign)
}

#' getData - Get the plpData for the validation
#' @param design The validationDesign object
#' @param database The databaseDetails object
#' @param outputFolder The directory to save the validation results to
#' @param allCovSettings A list of covariateSettings from the models
#' @param downloadTasks A list of download tasks determined by unique
#' combinations of targetId and restrictPlpDataSettings
#' @return The plpData object
#' @keywords internal
getData <- function(design, database, outputFolder, allCovSettings, downloadTasks) {
  # find task associated with design and the index of the task in downloadTasks
  task <- downloadTasks %>%
    dplyr::mutate(taskId = dplyr::row_number()) %>%
    dplyr::filter(.data$targetId == design$targetId,
                  paste0(.data$restrictPlpDataSettings, collapse = "|") == 
                  paste0(design$restrictPlpDataSettings, collapse = "|")) 
  
  databaseName <- database$cdmDatabaseName
  database$targetId <- task$targetId
  database$outcomeIds <- task$outcomeIds
  plpDataName <-
    paste0("targetId_", design$targetId, "_L", task$taskId)
  plpDataLocation <-
    file.path(outputFolder, databaseName, plpDataName)
  if (!dir.exists(plpDataLocation)) {
    plpData <- tryCatch({
      do.call(
        getPlpData,
        list(
          databaseDetails = database,
          restrictPlpDataSettings = task$restrictPlpDataSettings,
          covariateSettings = allCovSettings[[1]]
        )
      )
    },
    error = function(e) {
      ParallelLogger::logError(e)
      return(NULL)
    })   
    if (!is.null(plpData)) {
      if (!dir.exists(file.path(outputFolder, databaseName))) {
        dir.create(file.path(outputFolder, databaseName), recursive = TRUE)
      }
      savePlpData(plpData, file = plpDataLocation)
    }
  } else {
    ParallelLogger::logInfo(paste0("Data already extracted for ",
                            plpDataName, ": Loading from disk"))
    plpData <- loadPlpData(plpDataLocation)
  }
  return(plpData)
}

#' getPopulation - Get the population for the validationDesign
#' @param validationDesign The validationDesign objects
#' @param modelDesigns A list of modelDesign objects
#' @param plpData The plpData object
#' @return The population dataframe
#' @keywords internal
getPopulation <- function(validationDesign, modelDesigns, plpData) {
  design <- fromDesignOrModel(validationDesign, modelDesigns, "populationSettings")
  population <- tryCatch({
    do.call(
      createStudyPopulation,
      list(
        plpData = plpData,
        outcomeId = design$outcomeId,
        populationSettings = design$populationSettings
      )
    )
  },
  error = function(e) {
    ParallelLogger::logError(e)
    return(NULL)
  })
  return(population)
}
