# @file PlpSaveLoad.R
#
# Copyright 2021 Observational Health Data Sciences and Informatics
#
# This file is part of CohortMethod
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

#' Save the cohort data to folder
#'
#' @description
#' \code{savePlpData} saves an object of type plpData to folder.
#'
#' @param plpData   An object of type \code{plpData} as generated using
#'                           \code{getPlpData}.
#' @param file               The name of the folder where the data will be written. The folder should
#'                           not yet exist.
#' @param envir              The environment for to evaluate variables when saving
#' @param overwrite          Whether to force overwrite an existing file
#' @details
#' The data will be written to a set of files in the folder specified by the user.
#'
#' @examples
#' # todo
#'
#' @export
savePlpData <- function(plpData, file, envir=NULL, overwrite=F) {
  if (missing(plpData))
    stop("Must specify plpData")
  if (missing(file))
    stop("Must specify file")
  if (!class(plpData) %in% c("plpData","plpData.libsvm"  ))
    stop("Data not of class plpData")
  if(dir.exists(file.path(file, "covariates"))){
    stop('Folder to save covariates already exists...')
  }
  
  if(!dir.exists(file)){
    dir.create(file, recursive = T)
  }
  
  # save the actual values in the metaData
  # TODO - only do this if exists in parent or environ
  if(is.null(plpData$metaData$call$sampleSize)){  # fixed a bug when sampleSize is NULL
    plpData$metaData$call$sampleSize <- 'NULL'
  }
  
  Andromeda::saveAndromeda(plpData$covariateData, file = file.path(file, "covariates"), maintainConnection = T)
  saveRDS(plpData$timeRef, file = file.path(file, "timeRef.rds"))
  saveRDS(plpData$cohorts, file = file.path(file, "cohorts.rds"))
  saveRDS(plpData$outcomes, file = file.path(file, "outcomes.rds"))
  saveRDS(plpData$metaData, file = file.path(file, "metaData.rds"))
}

#' Load the cohort data from a folder
#'
#' @description
#' \code{loadPlpData} loads an object of type plpData from a folder in the file
#' system.
#'
#' @param file       The name of the folder containing the data.
#' @param readOnly   If true, the data is opened read only.
#'
#' @details
#' The data will be written to a set of files in the folder specified by the user.
#'
#' @return
#' An object of class plpData.
#'
#' @examples
#' # todo
#'
#' @export
loadPlpData <- function(file, readOnly = TRUE) {
  if (!file.exists(file))
    stop(paste("Cannot find folder", file))
  if (!file.info(file)$isdir)
    stop(paste("Not a folder", file))
  
  result <- list(covariateData = FeatureExtraction::loadCovariateData(file = file.path(file, "covariates")),
                 timeRef = readRDS(file.path(file, "timeRef.rds")),
                 cohorts = readRDS(file.path(file, "cohorts.rds")),
                 outcomes = readRDS(file.path(file, "outcomes.rds")),
                 metaData = readRDS(file.path(file, "metaData.rds")))

  class(result) <- "plpData"

  return(result)
}

#' Saves the plp model
#'
#' @details
#' Saves the plp model to a user specificed folder
#'
#' @param plpModel                   A trained classifier returned by running \code{runPlp()$model}
#' @param dirPath                  A location to save the model to
#'
#' @export
savePlpModel <- function(plpModel, dirPath){
  if (missing(plpModel))
    stop("Must specify plpModel")
  if (missing(dirPath))
    stop("Must specify directory path")
  if (class(plpModel) != "plpModel")
    stop("Not a plpModel")
  
  if(!dir.exists(dirPath)){
    ParallelLogger::logInfo('Creating directory to save model')
    dir.create(dirPath, recursive = T)
  }
  
  # save the covariateImportance
  utils::write.csv(
    x = plpModel$covariateImportance, 
    file = file.path(dirPath, 'covariateImportance.csv'),
    row.names = F
  )
  
  # save the trainDetails
  if(!is.null(plpModel$trainDetails)){
  plpModel$trainDetails$trainingTime <- paste(as.character(plpModel$trainDetails$trainingTime), attr(plpModel$trainDetails$trainingTime,'units'))
  saveJsonFile(
    rObject = plpModel$trainDetails, 
    file = file.path(dirPath, 'trainDetails.json')
  )
  }
  
  # save the validationDetails
  if(!is.null(plpModel$validationDetails)){
    plpModel$validationDetails$validationDate <- paste(as.character(plpModel$validationDetails$validationDate), attr(plpModel$validationDetails$validationDate,'units'))
    saveJsonFile(
      rObject = plpModel$validationDetails, 
      file = file.path(dirPath, 'validationDetails.json')
    )
  }
  
  
  # save the settings
  saveJsonFile(
    rObject = plpModel$settings, 
    file = file.path(dirPath, 'settings.json')
  )
  
  # save the model based on saveType
  if(attr(plpModel, 'saveType') == "xgboost"){
    xgboost::xgb.save(model = plpModel$model, fname = file.path(dirPath, "model.json"))
  } else if(attr(plpModel, 'saveType') == "RtoJson"){
    saveJsonFile(
      rObject = plpModel$model, 
      file = file.path(dirPath, 'model.json')
    )
  } else if(attr(plpModel, 'saveType') == "file"){
    # move the model into model
    if(!dir.exists(file.path(dirPath, 'model'))){
      dir.create(file.path(dirPath, 'model'), recursive = T)
    }
    for(file in dir(plpModel$model)){   
      file.copy(
        file.path(plpModel$model,file), 
        file.path(dirPath,'model'), 
        overwrite = TRUE,  
        recursive = FALSE,
        copy.mode = TRUE, 
        copy.date = FALSE)
    }
  } else{
    ParallelLogger::logWarn('Not sure how to save model - invalid saveType')
  }
  
  # save the attributes of plpModel
  modelAttributes <- attributes(plpModel)
  modelAttributes$names <- NULL
  saveJsonFile(
    rObject = modelAttributes, 
    file = file.path(dirPath, 'attributes.json')
  )
  
  return(dirPath)
}


#' loads the plp model
#'
#' @details
#' Loads a plp model that was saved using \code{savePlpModel()}
#'
#' @param dirPath                  The location of the model
#'
#' @export
loadPlpModel <- function(dirPath) {
  if (!file.exists(dirPath))
    stop(paste("Cannot find folder", dirPath))
  if (!file.info(dirPath)$isdir)
    stop(paste("Not a folder", dirPath))
  
  plpModel <- list()
  modelAttributes <- tryCatch(
    loadJsonFile(file.path(dirPath, 'attributes.json')),
    error = function(e){NULL}
  )
  
  if(is.null(modelAttributes)){
    ParallelLogger::logWarn('Incorrect plpModel object - is this an old model?')
    return(NULL)
  }
  
  attributes(plpModel) <- modelAttributes
  
  plpModel$covariateImportance <- tryCatch(
    utils::read.csv(file.path(dirPath, "covariateImportance.csv")),
    error = function(e){NULL}
  )
  
  if(file.exists(file.path(dirPath, "trainDetails.json"))){
    plpModel$trainDetails <- tryCatch(
      loadJsonFile(file.path(dirPath, "trainDetails.json")),
      error = function(e){NULL}
    )
  }
  if(file.exists(file.path(dirPath, "validationDetails.json"))){
    plpModel$validationDetails <- tryCatch(
      loadJsonFile(file.path(dirPath, "validationDetails.json")),
      error = function(e){NULL}
    )
  }
  
  plpModel$settings <- tryCatch(
    loadJsonFile(file.path(dirPath, "settings.json")),
    error = function(e){NULL}
  )
  
  if(attr(plpModel, 'saveType') == "xgboost"){
    ensure_installed("xgboost")
    plpModel$model <- xgboost::xgb.load(file.path(dirPath, "model.json"))
  } else if(attr(plpModel, 'saveType') %in% c("RtoJson")){
    plpModel$model <- loadJsonFile(file.path(dirPath, "model.json"))
  } else{
    plpModel$model <- file.path(dirPath, 'model')
  }

  return(plpModel)
}

saveJsonFile <- function(rObject, file){
  
  jsonObject  <- jsonlite::serializeJSON(rObject, digits = 23)
  write(jsonObject, file)
}

loadJsonFile <- function(fileName) {
  
  jsonObject <- readChar(fileName, file.info(fileName)$size)
  rObject <- jsonlite::unserializeJSON(jsonObject)
  
  return(rObject)
}


#' Saves the prediction dataframe to RDS
#'
#' @details
#' Saves the prediction data frame returned by predict.R to an RDS file and returns the fileLocation where the prediction is saved
#'
#' @param prediction                   The prediciton data.frame
#' @param dirPath                     The directory to save the prediction RDS
#' @param fileName                    The name of the RDS file that will be saved in dirPath
#' 
#' @export
savePrediction <- function(prediction, dirPath, fileName='prediction.rds'){
  #TODO check inupts
  saveJsonFile(prediction, file=file.path(dirPath,fileName))
  
  return(file.path(dirPath,fileName))
}

#' Loads the prediciton dataframe to csv
#'
#' @details
#' Loads the prediciton  RDS file
#'
#' @param fileLocation                     The location with the saved prediction
#' 
#' @export
loadPrediction <- function(fileLocation){
  #TODO check inupts
  prediction <- loadJsonFile(fileName = fileLocation)
  return(prediction)
}

#' Saves the result from runPlp into the location directory
#'
#' @details
#' Saves the result from runPlp into the location directory
#'
#' @param result                      The result of running runPlp()
#' @param dirPath                     The directory to save the csv
#' 
#' @export
savePlpResult <- function(result, dirPath){
  if (missing(result))
    stop("Must specify runPlp output")
  if (missing(dirPath))
    stop("Must specify directory location")
  #if (class(plpModel) != "plpModel")
  #  stop("Not a plpModel")
  
  if(!dir.exists(dirPath)) dir.create(dirPath, recursive = T)
  
  savePlpModel(result$model, dirPath=file.path(dirPath,'model') )
  result$model <- NULL
  saveRDS(result, file = file.path(dirPath, "runPlp.rds"))
  
}

#' Loads the evalaution dataframe
#'
#' @details
#' Loads the evaluation 
#'
#' @param dirPath                     The directory where the evaluation was saved
#' 
#' @export
loadPlpResult <- function(dirPath){
  if (!file.exists(dirPath))
    stop(paste("Cannot find folder", dirPath))
  if (!file.info(dirPath)$isdir)
    stop(paste("Not a folder", dirPath))
  
  result <- readRDS(file.path(dirPath, "runPlp.rds"))
  result$model = loadPlpModel(file.path(dirPath, "model"))

  class(result) <- "runPlp"
  
  return(result)
  
}


#' Save the plp result as json files and csv files for transparent sharing
#'
#' @details
#' Saves the main results json/csv files (these files can be read by the shiny app)
#'
#' @param result                      An object of class runPlp with development or validation results
#' @param saveDirectory                     The directory the save the results as csv files
#' @param minCellCount                Minimum cell count for the covariateSummary and certain evaluation results
#' 
#' @export
savePlpShareable <- function(result, saveDirectory, minCellCount = 10){
  
  if(!dir.exists(saveDirectory)) dir.create(saveDirectory, recursive = T)
  
  #executionSummary
  saveJsonFile(result$executionSummary, file.path(saveDirectory, 'executionSummary.json'))
  
  #save model as json files
  savePlpModel(result$model, file.path(saveDirectory, 'model'))
  
  #performanceEvaluation
  if(!dir.exists(file.path(saveDirectory, 'performanceEvaluation'))){dir.create(file.path(saveDirectory, 'performanceEvaluation'), recursive = T)}
  utils::write.csv(removeList(result$performanceEvaluation$evaluationStatistics), file = file.path(saveDirectory, 'performanceEvaluation','evaluationStatistics.csv'), row.names = F)
  utils::write.csv(result$performanceEvaluation$thresholdSummary, file = file.path(saveDirectory, 'performanceEvaluation','thresholdSummary.csv'), row.names = F)
  utils::write.csv(
    removeCellCount(
      result$performanceEvaluation$demographicSummary, 
      minCellCount = minCellCount, 
      filterColumns = c('PersonCountAtRisk', 'PersonCountWithOutcome')
    ), 
    file = file.path(saveDirectory, 'performanceEvaluation','demographicSummary.csv'), 
    row.names = F
  )
  utils::write.csv(result$performanceEvaluation$calibrationSummary, file = file.path(saveDirectory, 'performanceEvaluation','calibrationSummary.csv'), row.names = F)
  utils::write.csv(result$performanceEvaluation$predictionDistribution, file = file.path(saveDirectory, 'performanceEvaluation','predictionDistribution.csv'), row.names = F)
  
  if(!is.null(result$covariateSummary)){
    #covariateSummary
    utils::write.csv(
      removeCellCount(
        result$covariateSummary,
        minCellCount = minCellCount, 
        filterColumns = c('CovariateCount', 'WithOutcome_CovariateCount', 'WithNoOutcome_CovariateCount'),
        extraCensorColumns = c('WithOutcome_CovariateMean', 'WithNoOutcome_CovariateMean'),
        restrictColumns = c('covariateId','covariateName', 'analysisId', 'conceptId','CovariateCount', 'covariateValue','WithOutcome_CovariateCount','WithNoOutcome_CovariateCount','WithOutcome_CovariateMean','WithNoOutcome_CovariateMean','StandardizedMeanDiff')
      ), 
      file = file.path(saveDirectory,'covariateSummary.csv'), 
      row.names = F
    )
  }
  
  #analysisRef
  saveJsonFile(result$analysisRef, file.path(saveDirectory, 'analysisRef.json'))
  
  return(invisible(saveDirectory))
}

removeList <- function(x){
  
  if(is.null(x)){
    return(x)
  }
  
  for(i in 1:ncol(x)){
    x[,i] <- unlist(x[,i])
  }
  
  if('value' %in% colnames(x)){
    x$value <- as.double(x$value)
  }
  
  return(x)
  
}

#' Loads the plp result saved as json/csv files for transparent sharing
#'
#' @details
#' Load the main results from json/csv files into a runPlp object
#'
#' @param loadDirectory                     The directory with the results as json/csv files
#' 
#' @export
loadPlpShareable <- function(loadDirectory){
  
  result <- list()
  objects <- gsub('.json', '', gsub('.csv','',dir(loadDirectory)))
  if(sum(!c('covariateSummary','executionSummary','performanceEvaluation', 'model', 'analysisRef')%in%objects)>0){
    stop('Incorrect results file')
  }
  
  length(result) <- length(objects)
  names(result) <- objects
  
  # load model settings
  result$model <- loadPlpModel(file.path(loadDirectory,'model'))
  
  #executionSummary
  result$executionSummary <- tryCatch({loadJsonFile(fileName = file.path(loadDirectory, 'executionSummary.json'))}, error = function(e){return(NULL)})
  
  #performanceEvaluation
  result$performanceEvaluation <- list()
  result$performanceEvaluation$evaluationStatistics <- tryCatch({utils::read.csv(file = file.path(loadDirectory, 'performanceEvaluation','evaluationStatistics.csv'))}, error = function(e){return(NULL)})
  result$performanceEvaluation$thresholdSummary <- tryCatch({utils::read.csv(file = file.path(loadDirectory, 'performanceEvaluation','thresholdSummary.csv'))}, error = function(e){return(NULL)})
  result$performanceEvaluation$demographicSummary <- tryCatch({utils::read.csv(file = file.path(loadDirectory, 'performanceEvaluation','demographicSummary.csv'))}, error = function(e){return(NULL)})
  result$performanceEvaluation$calibrationSummary <- tryCatch({utils::read.csv(file = file.path(loadDirectory, 'performanceEvaluation','calibrationSummary.csv'))}, error = function(e){return(NULL)})
  result$performanceEvaluation$predictionDistribution <- tryCatch({utils::read.csv(file = file.path(loadDirectory, 'performanceEvaluation','predictionDistribution.csv'))}, error = function(e){return(NULL)})
  
  #covariateSummary
  result$covariateSummary <- utils::read.csv(file = file.path(loadDirectory,'covariateSummary.csv'))

  #analysisRef
  result$analysisRef <- tryCatch({loadJsonFile(fileName = file.path(loadDirectory, 'analysisRef.json'))}, error = function(e){return(NULL)})
  
  class(result) <- "runPlp"
  return(result)
}


removeCellCount <- function(
  data,
  minCellCount = minCellCount, 
  filterColumns = c('CovariateCount', 'WithOutcome_CovariateCount', 'WithNoOutcome_CovariateCount'),
  extraCensorColumns = c('WithOutcome_CovariateMean', 'WithNoOutcome_CovariateMean'),
  restrictColumns = NULL
){
  
  # first restrict to certain columns if required
  if(!is.null(restrictColumns)){
    data <- data[,restrictColumns]
  }
  
  #next find the rows that need censoring
  ind <- rep(F, nrow(data))
  for(i in 1:length(filterColumns)){
    data[,filterColumns[i]][is.na(data[,filterColumns[i]])] <- 0
    ind <- ind | (data[,filterColumns[i]] < minCellCount)
  }
  
  # now replace these value with -1
  
  removeColumns <- c(filterColumns,extraCensorColumns)[c(filterColumns,extraCensorColumns) %in% colnames(data)]
  
  for(i in 1:length(removeColumns)){
    data[ind,removeColumns[i]] <- NA
  }
  
  return(data)
}

