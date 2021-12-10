# @file PlpSaveLoad.R
#
# Copyright 2020 Observational Health Data Sciences and Informatics
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
  
  if(!dir.exists(dirPath)) dir.create(dirPath)
  
  # If model is saved on hard drive move it...
  #============================================================
  moveFile <- moveModelFile(plpModel, dirPath )

  if(!is.null(moveFile)){
    plpModel$model <- moveFile
  }
  #============================================================

  # if deep (keras) then save hdfs
  if(attr(plpModel, 'predictionFunction') == "predictXgboost"){
    # fixing xgboost save/load issue
    xgboost::xgb.save(model = plpModel$model, fname = file.path(dirPath, "model.json"))
    plpModel$model <- NULL
  } 
  
  saveRDS(plpModel, file = file.path(dirPath, "plpModel.rds"))

}

moveModelFile <- function(plpModel, dirPath ){
  
  if(length(grep('sklearn', tolower(attr(plpModel, 'predictionFunction'))))>0){
    saveName <- 'sklearn_model'
  } else if(length(grep('knn', tolower(attr(plpModel, 'predictionFunction'))))>0){
    saveName <- 'knn_model'
  } else{
    return(NULL)
  }

  if(!dir.exists(file.path(dirPath, saveName))){
    dir.create(file.path(dirPath, saveName))
  }
  
  for(file in dir(plpModel$model)){   
    file.copy(
      file.path(plpModel$model,file), 
      file.path(dirPath,saveName), 
      overwrite=TRUE,  
      recursive = FALSE,
      copy.mode = TRUE, 
      copy.date = FALSE)
  }
  
  return(invisible(file.path(dirPath,saveName)))
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
  
  plpModel <- tryCatch(readRDS(file.path(dirPath, "plpModel.rds")),
                               error=function(e) NULL)
  
  if(attr(plpModel, 'predictionFunction') == "predictXgboost"){
    ensure_installed("xgboost")
    plpModel$model <- xgboost::xgb.load(file.path(dirPath, "model.json"))
  } 
  
  # update the model location to the load dirPath
  if(!is.null(updateModelLocation(plpModel, dirPath))){
    plpModel$model <- updateModelLocation(plpModel, dirPath)
  }
  
  # make this backwrds compatible for ffdf:
  plpModel$predict <- createTransform(plpModel)
  
  return(plpModel)
}

updateModelLocation  <- function(plpModel, dirPath){
  
  if(length(grep('sklearn', tolower(attr(plpModel, 'predictionFunction'))))>0){
    saveName <- 'sklearn_model'
  } else if(length(grep('knn', tolower(attr(plpModel, 'predictionFunction'))))>0){
    saveName <- 'knn_model'
  } else{
    return(NULL)
  }
  
  plpModel$model <- file.path(dirPath,saveName)
  
  return(plpModel) 
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
  saveRDS(prediction, file=file.path(dirPath,fileName))
  
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
  prediction <- readRDS(file=fileLocation)
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


savePlpModelShareable <- function(plpModel, saveDirectory){
  checkIsClass(plpModel, 'plpModel')
  checkIsClass(saveDirectory, 'character')
  
  ParallelLogger::logInfo(paste0('Saving model to ',saveDirectory))
  
  ensure_installed('RJSONIO')
  if(!dir.exists(saveDirectory)){
    dir.create(saveDirectory, recursive = T)
  }
  
  #============================================================
  moveFile <- moveModelFile(plpModel, saveDirectory )
  
  if(!is.null(moveFile)){
    plpModel$model <- saveDirectory
  }
  #============================================================
  
  # if deep (keras) then save hdfs
  if(attr(plpModel, 'predictionFunction') == "predictXgboost"){
    # fixing xgboost save/load issue
    xgboost::xgb.save(model = plpModel$model, fname = file.path(saveDirectory, "model.json"))
  } else{
    model <- RJSONIO::toJSON(plpModel$model, digits = 23) # update this if a location and move model
    write(model, file.path(saveDirectory, "model.json"))
  }
  
  settings <- RJSONIO::toJSON(plpModel$settings, digits = 23)
  write(settings, file.path(saveDirectory, "settings.json"))
  
  attributes <- list(
    predictionFunction = attr(plpModel, 'predictionFunction'), 
    modelType = attr(plpModel, 'modelType') 
  )
  attributes <-  RJSONIO::toJSON(attributes)
  write(attributes, file.path(saveDirectory,"attributes.json"))
  
  trainDetails <- RJSONIO::toJSON(plpModel$trainDetails, digits = 23)
  write(trainDetails, file.path(saveDirectory, "trainDetails.json"))
  
  utils::write.csv(plpModel$covariateImportance %>% dplyr::filter(.data$covariateValue != 0 ), file.path(saveDirectory, "covariateImportance.csv"), row.names = F)
  
  return(invisible(saveDirectory))
  
}

loadJsonFile <- function(fileName) {
  
  ensure_installed('RJSONIO')
  
  result <- readChar(fileName, file.info(fileName)$size)
  result <- RJSONIO::fromJSON(result)
  
  return(result)
}

loadPlpModelShareable <- function(loadDirectory){
  
  checkIsClass(loadDirectory, 'character')
  if(!dir.exists(loadDirectory)){
    ParallelLogger::logError('No model at specified loadDirectory')
  }
  
  ParallelLogger::logInfo(paste0('Loading model from ', loadDirectory))
  
  plpModel <- list()
  
  attributes <- loadJsonFile(file.path(loadDirectory,"attributes.json"))
  # make this automatic for atr in names(attributes){attr(plpModel, atr) <- attributes[atr]} ?
  attr(plpModel, 'predictionFunction') <- attributes$predictionFunction
  attr(plpModel, 'modelType') <- attributes$modelType
  
  plpModel$settings <- loadJsonFile(file.path(loadDirectory,"settings.json"))
  plpModel$trainDetails <- loadJsonFile(file.path(loadDirectory,"trainDetails.json"))
  
  plpModel$covariateImportance <- utils::read.csv(file.path(loadDirectory,"covariateImportance.csv"))
  
  if(attributes$predictionFunction == "predictXgboost"){
    ensure_installed("xgboost")
      plpModel$model <- xgboost::xgb.load(file.path(dirPath, "model.json"))
  }else{
    plpModel$model <- loadJsonFile(file.path(loadDirectory,"model.json"))
  } 
  
  # update the model location to the loadDirectory
  update <- updateModelLocation(plpModel, loadDirectory)
  if(!is.null(update)){
    plpModel <- update
  }
  
  # add the prediction function
  plpModel$predict <- createTransform(plpModel)
  
  class(plpModel) <- "plpModel"
  
  return(plpModel)
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
  
  #save model as json files
  savePlpModelShareable(result$model, file.path(saveDirectory, 'model'))
  
  #executionSummary
  if(!dir.exists(file.path(saveDirectory, 'executionSummary'))){dir.create(file.path(saveDirectory, 'executionSummary'), recursive = T)}
  utils::write.csv(result$executionSummary$PackageVersion, file = file.path(saveDirectory, 'executionSummary','PackageVersion.csv'), row.names = F)
  utils::write.csv(unlist(result$executionSummary$PlatformDetails), file = file.path(saveDirectory, 'executionSummary','PlatformDetails.csv'))
  utils::write.csv(result$executionSummary$TotalExecutionElapsedTime, file = file.path(saveDirectory, 'executionSummary','TotalExecutionElapsedTime.csv'), row.names = F)
  utils::write.csv(result$executionSummary$ExecutionDateTime, file = file.path(saveDirectory, 'executionSummary','ExecutionDateTime.csv'), row.names = F)
  
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
  objects <- gsub('.csv','',dir(loadDirectory))
  if(sum(!c('covariateSummary','executionSummary','performanceEvaluation', 'model')%in%objects)>0){
    stop('Incorrect csv results file')
  }
  
  length(result) <- length(objects)
  names(result) <- objects
  
  #covariateSummary
  result$covariateSummary <- utils::read.csv(file = file.path(loadDirectory,'covariateSummary.csv'))

  #executionSummary
  result$executionSummary <- list()
  result$executionSummary$PackageVersion <- tryCatch({as.list(utils::read.csv(file = file.path(loadDirectory, 'executionSummary','PackageVersion.csv')))}, error = function(e){return(NULL)})
  result$executionSummary$PlatformDetails <- tryCatch({as.list(utils::read.csv(file = file.path(loadDirectory, 'executionSummary','PlatformDetails.csv'))$x)}, error = function(e){return(NULL)})
  names(result$executionSummary$PlatformDetails) <- tryCatch({utils::read.csv(file = file.path(loadDirectory, 'executionSummary','PlatformDetails.csv'))$X}, error = function(e){return(NULL)})
  result$executionSummary$TotalExecutionElapsedTime <- tryCatch({utils::read.csv(file = file.path(loadDirectory, 'executionSummary','TotalExecutionElapsedTime.csv'))$x}, error = function(e){return(NULL)})
  result$executionSummary$ExecutionDateTime <- tryCatch({utils::read.csv(file = file.path(loadDirectory, 'executionSummary','ExecutionDateTime.csv'))$x}, error = function(e){return(NULL)})
  
  #model settings
  
  #performanceEvaluation
  result$performanceEvaluation <- list()
  result$performanceEvaluation$evaluationStatistics <- tryCatch({utils::read.csv(file = file.path(loadDirectory, 'performanceEvaluation','evaluationStatistics.csv'))}, error = function(e){return(NULL)})
  result$performanceEvaluation$thresholdSummary <- tryCatch({utils::read.csv(file = file.path(loadDirectory, 'performanceEvaluation','thresholdSummary.csv'))}, error = function(e){return(NULL)})
  result$performanceEvaluation$demographicSummary <- tryCatch({utils::read.csv(file = file.path(loadDirectory, 'performanceEvaluation','demographicSummary.csv'))}, error = function(e){return(NULL)})
  result$performanceEvaluation$calibrationSummary <- tryCatch({utils::read.csv(file = file.path(loadDirectory, 'performanceEvaluation','calibrationSummary.csv'))}, error = function(e){return(NULL)})
  result$performanceEvaluation$predictionDistribution <- tryCatch({utils::read.csv(file = file.path(loadDirectory, 'performanceEvaluation','predictionDistribution.csv'))}, error = function(e){return(NULL)})
  
  # load model settings
  result$model <- loadPlpModelShareable(file.path(loadDirectory,'model'))
  
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
  
  removeColumns <- c(filterColumns,extraCensorColumns)
  
  for(i in 1:length(removeColumns)){
    data[ind,removeColumns[i]] <- NA
  }
  
  return(data)
}
