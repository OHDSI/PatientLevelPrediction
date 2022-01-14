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


externalValidatePlp <- function(
  plpModel,
  plpData,
  databaseName = 'database 1',
  population,
  settings = createValidationSettings(  # add covariateSummary option?
    recalibrate = 'weakRecalibration'
  )
){
  
  # Apply model 
  #=======
  prediction <- tryCatch({
    predictPlp(
      plpModel = plpModel, 
      plpData = plpData, 
      population = population
    )},
    error = function(e){ParallelLogger::logError(e)}
  )
  
  prediction$evaluationType <- 'Validation'
  
  # Recalibrate
  #=======
  if(!is.null(settings$recalibrate)){
    for(recalMethod in settings$recalibrate){
      prediction <- tryCatch({recalibratePlp(prediction = prediction, method = recalMethod)
      },
        error = function(e){ParallelLogger::logError(e)}
      )
    }
  }
  
  # Evaluate 
  #=======
  performance <- tryCatch({
    evaluatePlp(prediction = prediction, typeColumn = 'evaluationType')
  },
    error = function(e){ParallelLogger::logError(e)}
  )
  
  # step 6: covariate summary
  labels <- tryCatch({
    population %>% dplyr::select(.data$rowId, .data$outcomeCount)
  },
    error = function(e){ return(NULL) }
  )
  covariateSum <- tryCatch({
    covariateSummary(
      covariateData = plpData$covariateData, 
      cohort = population, 
      labels = labels, 
      variableImportance = plpModel$covariateImportance %>% dplyr::select(.data$covariateId, .data$covariateValue)
    )},
    error = function(e){ return(NULL) }
  )
  
  executionSummary <- list(
    ExecutionDateTime = Sys.Date(),
    PackageVersion = list(
      rVersion= R.Version()$version.string,
      packageVersion = utils::packageVersion("PatientLevelPrediction")
    ),
    PlatformDetails= list(
      platform = R.Version()$platform,
      cores = Sys.getenv('NUMBER_OF_PROCESSORS'),
      RAM = memuse::Sys.meminfo()[1]
    ) #  test for non-windows needed
  )
  
  model = list(
    model = 'external validation of model',
    settings = plpModel$settings,
    validationDetails   = list(
      analysisId = '', #TODO add from model
      analysisSource = '', #TODO add from model
      developmentDatabase = plpModel$trainDetails$cdmDatabaseSchema,
      cdmDatabaseSchema = databaseName,
      populationSettings = attr(population, 'metaData')$populationSettings,
      outcomeId = attr(population, 'metaData')$outcomeId,
      cohortId = attr(plpData, 'metaData')$cohortId,
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
externalValidateDbPlp <- function(
  plpModel,
  validationDatabaseDetails = createDatabaseDetails(),
  validationRestrictPlpDataSettings = createRestrictPlpDataSettings(),
  settings = createValidationSettings(
    recalibrate = 'weakRecalibration'
    ),
  logSettings = createLogSettings(verbosity = 'INFO', logName = 'validatePLP'),
  outputFolder = getwd()
){
  
  # Input checks 
  #=======
  
  checkIsClass(plpModel, 'plpModel')
  
  # check the class and make a list if a single database setting
  if(class(validationDatabaseDetails) == 'list'){
    lapply(validationDatabaseDetails, function(x) checkIsClass(x, 'databaseDetails'))
  } else{
    checkIsClass(validationDatabaseDetails, 'databaseDetails')
    validationDatabaseDetails <- list(validationDatabaseDetails)
  }
  checkIsClass(validationRestrictPlpDataSettings, 'restrictPlpDataSettings')
  checkIsClass(settings, 'validationSettings')

  # create results list with the names of the databases to validate across
  result <- list()
  length(result) <- length(validationDatabaseDetails)
  names(result) <- unlist(lapply(validationDatabaseDetails, function(x) attr(x, 'cdmDatabaseName')))
  
  for(databaseDetails in validationDatabaseDetails){
    
    databaseName <- attr(databaseDetails, 'cdmDatabaseName')
    
    # initiate log
    logSettings$saveDirectory <- file.path(outputFolder, databaseName, plpModel$trainDetails$analysisId)
    logSettings$logFileName <- 'validationLog'
    logger <- do.call(createLog,logSettings)
    ParallelLogger::registerLogger(logger)
    
    ParallelLogger::logInfo(paste('Validating model on', databaseName))
    
    # Step 1: get data
    #=======
    
    getPlpDataSettings <- list(
      databaseDetails = databaseDetails,
      restrictPlpDataSettings = validationRestrictPlpDataSettings
      )
    if(is.null(getPlpDataSettings$databaseDetails$cohortId)){
      ParallelLogger::logInfo("cohortId not in databaseSettings so using model's")
      getPlpDataSettings$databaseDetails$cohortId <- plpModel$trainDetails$cohortId
    }
    if(is.null(getPlpDataSettings$databaseDetails$outcomeIds)){
      ParallelLogger::logInfo("cohortId not in databaseSettings  so using model's")
      getPlpDataSettings$databaseDetails$outcomeIds <- plpModel$trainDetails$outcomeId
    }
    
    if(is.null(getPlpDataSettings$restrictPlpDataSettings$firstExposureOnly)){
      ParallelLogger::logInfo("firstExposureOnly not in restrictPlpDataSettings  so using model's")
      getPlpDataSettings$restrictPlpDataSettings$firstExposureOnly <- plpModel$settings$plpDataSettings$firstExposureOnly
    }
    if(is.null(getPlpDataSettings$restrictPlpDataSettings$washoutPeriod)){
      ParallelLogger::logInfo("washoutPeriod not in restrictPlpDataSettings so using model's")
      getPlpDataSettings$restrictPlpDataSettings$washoutPeriod <- plpModel$settings$plpDataSettings$washoutPeriod
    }
    
    # we need to update this to restrict to model covariates and update custom features
    getPlpDataSettings$covariateSettings <- plpModel$settings$covariateSettings
    
    plpData <- tryCatch({
      do.call(getPlpData, getPlpDataSettings)
    },
      error = function(e){ParallelLogger::logError(e); return(NULL)}
    )
    
    if(is.null(plpData)){
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
          populationSettings = plpModel$settings$populationSettings
        )
      )
    },
      error = function(e){ParallelLogger::logError(e); return(NULL)}
    )
    
    if(is.null(population)){
      closeLog(logger)
    }
    
    # Step 3: Apply model to plpData and population
    #=======

    result[[databaseName]] <- tryCatch({
      externalValidatePlp(
        plpModel,
        plpData,
        databaseName = databaseName,
        population,
        settings = settings
      )},
      error = function(e){ParallelLogger::logInfo(e); return(NULL)}
    )
    
    if(is.null(result[[databaseName]])){
      closeLog(logger)
    } else{
      
      if(!dir.exists(file.path(outputFolder, databaseName, plpModel$trainDetails$analysisId))){
        dir.create(file.path(outputFolder, databaseName, plpModel$trainDetails$analysisId), recursive = T)
      }  
      
      savePlpResult(result[[databaseName]], dirPath = file.path(outputFolder, databaseName, plpModel$trainDetails$analysisId, 'validationResult'))
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
#' @return
#' A setting object of class \code{validationSettings} containing a list of settings for externalValidatePlp
#'
#' @export
createValidationSettings <- function(
  recalibrate = NULL
){
  
  checkIsClass(recalibrate, c('character','NULL'))
  if(!is.null(recalibrate)){
    if(sum(recalibrate %in% c('recalibrationInTheLarge', 'weakRecalibration'))!=length(recalibrate)){
      ParallelLogger::logError('Incorrect recalibrate options used.  Must be recalibrationInTheLarge or weakRecalibration')
    }
  }

  result = list(
    recalibrate = recalibrate
  )
  class(result) <- 'validationSettings'
  return(result)
}
