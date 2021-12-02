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
    dir.create(file)
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
  moveFile <- moveHdModel(plpModel, dirPath )

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
  
  attributes <- list(
    predictionFunction = attr(plpModel, 'predictionFunction'), 
    modelType = attr(plpModel, 'modelType') 
    )
  saveRDS(attributes, file = file.path(dirPath,  "attributes.rds"))
  
}

moveModelFile <- function(plpModel, dirPath ){
  
  if(grep('sklearn', tolower(attr(plpModel, 'predictionFunction')))){
    saveName <- 'sklearn_model'
  } else if(grep('knn', tolower(attr(plpModel, 'predictionFunction')))){
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
  
  attributes <- readRDS(file.path(dirPath, "attributes.rds"))
  
  if(attributes$predictionFunction == "predictXgboost"){
    ensure_installed("xgboost")
    if('model' %in% dir(dirPath)){
      plpModel$model <- xgboost::xgb.load(file.path(dirPath, "model"))
    } else{
      plpModel$model <- xgboost::xgb.load(file.path(dirPath, "model.json"))
    }
  } 
  
  attr(plpModel, 'modelType') <- attributes$modelType
  attr(plpModel, 'predictionFunction') <- attributes$predictionFunction
  class(plpModel) <- "plpModel"
  
  # update the model location to the load dirPath
  plpModel <- updateModelLocation(plpModel, dirPath)
  
  # make this backwrds compatible for ffdf:
  plpModel$predict <- createTransform(plpModel)
  
  return(plpModel)
}

updateModelLocation  <- function(plpModel, dirPath){
  
  if(grep('sklearn', tolower(attr(plpModel, 'predictionFunction')))){
    saveName <- 'sklearn_model'
  } else if(grep('knn', tolower(attr(plpModel, 'predictionFunction')))){
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


#result$inputSetting$dataExtrractionSettings$covariateSettings
formatCovariateSettings <- function(covariateSettings){
  
  if(class(covariateSettings) == "covariateSettings"){
    return(list(cvs = data.frame(X = names(unlist(covariateSettings)), x= unlist(covariateSettings)), 
                fun = attr(covariateSettings,'fun')))
    
  } else{
    return(list(cvs = do.call(rbind, lapply(1:length(covariateSettings), function(i){
      inds <- which(lapply(covariateSettings[[i]], class) == "function")
      if(length(inds)>0){
        for(j in inds){
          covariateSettings[[i]][[j]] <- paste0(deparse(covariateSettings[[i]][[j]]), collapse = " ")
        }
      }
    tempResult <- data.frame(names = names(unlist(covariateSettings[[i]])),
               values = unlist(covariateSettings[[i]]))
    tempResult$settingsId <- i
    return(tempResult)
    })), 
    fun = unlist(lapply(covariateSettings, function(x) attr(x,'fun')))
    )
    )
  }
  
}

reformatCovariateSettings <- function(covariateSettingsLocation){
  # adding this to stop warnings when files does not exist
  if(!file.exists(covariateSettingsLocation)){
    return(NULL)
  }
  cs <- utils::read.csv(covariateSettingsLocation, stringsAsFactors=FALSE)
  fun <- utils::read.csv(gsub('.csv','_fun.csv',covariateSettingsLocation), stringsAsFactors=FALSE)
  
  if(sum(colnames(cs)%in%c('X','x'))==2){
    covariateSettings <- cs$x
    covariateSettings <- as.list(covariateSettings)
    names(covariateSettings) <- cs$X
    attr(covariateSettings,'fun') <- fun$x
    class(covariateSettings) <- 'covariateSettings'
  } else {
    
    covariateSettings <- list()
    length(covariateSettings) <- max(cs$settingsId)
    
    for(i in 1:max(cs$settingsId)){
      covariateSettings[[i]] <- cs$values[cs$settingsId==i]
      covariateSettings[[i]] <- as.list(covariateSettings[[i]])
      names(covariateSettings[[i]]) <- cs$names[cs$settingsId==i]
      attr(covariateSettings[[i]],'fun') <- fun$x[i]
    }
    
  }
  
return(covariateSettings)
}


#' Save parts of the plp result as a csv for transparent sharing
#'
#' @details
#' Saves the main results as a csv (these files can be read by the shiny app)
#'
#' @param result                      An object of class runPlp with development or validation results
#' @param dirPath                     The directory the save the results as csv files
#' 
#' @export
savePlpToCsv <- function(result, dirPath){
  
  #model settings - save as json
  if(!dir.exists(file.path(dirPath, 'model'))){dir.create(file.path(dirPath, 'model'), recursive = T)}
  utils::write.csv(result$model$settings$plpDataSettings, file = file.path(dirPath, 'model','plpDataSettings.csv'), row.names = F)
  utils::write.csv(result$model$settings$covariateSettings, file = file.path(dirPath, 'model','covariateSettings.csv'), row.names = F)
  utils::write.csv(result$model$settings$populationSettings, file = file.path(dirPath, 'model','populationSettings.csv'), row.names = F)
  utils::write.csv(result$model$settings$featureEngineering, file = file.path(dirPath, 'model','featureEngineering.csv'), row.names = F)
  utils::write.csv(result$model$settings$tidyCovariates, file = file.path(dirPath, 'model','tidyCovariates.csv'), row.names = F)
  utils::write.csv(result$model$settings$requireDenseMatrix, file = file.path(dirPath, 'model','requireDenseMatrix.csv'), row.names = F)
  utils::write.csv(result$model$settings$modelSettings$model, file = file.path(dirPath, 'model','modelSettings_model.csv'), row.names = F)
  utils::write.csv(result$model$settings$modelSettings$param, file = file.path(dirPath, 'model','modelSettings_param.csv'), row.names = F)
  utils::write.csv(result$model$settings$modelSettings$finalModelParameters, file = file.path(dirPath, 'model','modelSettings_finalModelParameters.csv'), row.names = F)
  utils::write.csv(result$model$settings$modelSettings$extraSettings, file = file.path(dirPath, 'model','modelSettings_extraSettings.csv'), row.names = F)
  utils::write.csv(result$model$settings$splitSettings, file = file.path(dirPath, 'model','splitSettings.csv'), row.names = F)
  
  #trainDetails = list(
  #  cdmDatabaseSchema = attr(trainData, "metaData")$cdmDatabaseSchema,
  #  outcomeId = attr(trainData, "metaData")$outcomeId,
  #  cohortId = attr(trainData, "metaData")$cohortId,
  #  attrition = attr(trainData, "metaData")$attrition, 
  #  trainingTime = comp,
  #  trainingDate = Sys.Date(),
  #  hyperParamSearch = hyperSummary
  #)
  
  #executionSummary
  if(!dir.exists(file.path(dirPath, 'executionSummary'))){dir.create(file.path(dirPath, 'executionSummary'), recursive = T)}
  utils::write.csv(result$executionSummary$PackageVersion, file = file.path(dirPath, 'executionSummary','PackageVersion.csv'), row.names = F)
  utils::write.csv(unlist(result$executionSummary$PlatformDetails), file = file.path(dirPath, 'executionSummary','PlatformDetails.csv'))
  utils::write.csv(result$executionSummary$TotalExecutionElapsedTime, file = file.path(dirPath, 'executionSummary','TotalExecutionElapsedTime.csv'), row.names = F)
  utils::write.csv(result$executionSummary$ExecutionDateTime, file = file.path(dirPath, 'executionSummary','ExecutionDateTime.csv'), row.names = F)
  
  #performanceEvaluation
  if(!dir.exists(file.path(dirPath, 'performanceEvaluation'))){dir.create(file.path(dirPath, 'performanceEvaluation'), recursive = T)}
  utils::write.csv(result$performanceEvaluation$evaluationStatistics, file = file.path(dirPath, 'performanceEvaluation','evaluationStatistics.csv'), row.names = F)
  utils::write.csv(result$performanceEvaluation$thresholdSummary, file = file.path(dirPath, 'performanceEvaluation','thresholdSummary.csv'), row.names = F)
  utils::write.csv(result$performanceEvaluation$demographicSummary, file = file.path(dirPath, 'performanceEvaluation','demographicSummary.csv'), row.names = F)
  utils::write.csv(result$performanceEvaluation$calibrationSummary, file = file.path(dirPath, 'performanceEvaluation','calibrationSummary.csv'), row.names = F)
  utils::write.csv(result$performanceEvaluation$predictionDistribution, file = file.path(dirPath, 'performanceEvaluation','predictionDistribution.csv'), row.names = F)
  
  #covariateSummary
  utils::write.csv(result$covariateSummary, file = file.path(dirPath,'covariateSummary.csv'), row.names = F)
}

#' Loads parts of the plp result saved as csv files for transparent sharing
#'
#' @details
#' Load the main results from csv files into a runPlp object
#'
#' @param dirPath                     The directory with the results as csv files
#' 
#' @export
loadPlpFromCsv <- function(dirPath){
  
  result <- list()
  objects <- gsub('.csv','',dir(dirPath))
  if(sum(!c('covariateSummary','executionSummary','performanceEvaluation', 'model')%in%objects)>0){
    stop('Incorrect csv results file')
  }
  
  length(result) <- length(objects)
  names(result) <- objects
  
  #covariateSummary
  result$covariateSummary <- utils::read.csv(file = file.path(dirPath,'covariateSummary.csv'))

  #executionSummary
  result$executionSummary <- list()
  result$executionSummary$PackageVersion <- tryCatch({as.list(utils::read.csv(file = file.path(dirPath, 'executionSummary','PackageVersion.csv')))}, error = function(e){return(NULL)})
  result$executionSummary$PlatformDetails <- tryCatch({as.list(utils::read.csv(file = file.path(dirPath, 'executionSummary','PlatformDetails.csv'))$x)}, error = function(e){return(NULL)})
  names(result$executionSummary$PlatformDetails) <- tryCatch({utils::read.csv(file = file.path(dirPath, 'executionSummary','PlatformDetails.csv'))$X}, error = function(e){return(NULL)})
  result$executionSummary$TotalExecutionElapsedTime <- tryCatch({utils::read.csv(file = file.path(dirPath, 'executionSummary','TotalExecutionElapsedTime.csv'))$x}, error = function(e){return(NULL)})
  result$executionSummary$ExecutionDateTime <- tryCatch({utils::read.csv(file = file.path(dirPath, 'executionSummary','ExecutionDateTime.csv'))$x}, error = function(e){return(NULL)})
  
  #model settings
  
  #performanceEvaluation
  result$performanceEvaluation <- list()
  result$performanceEvaluation$evaluationStatistics <- tryCatch({utils::read.csv(file = file.path(dirPath, 'performanceEvaluation','evaluationStatistics.csv'))}, error = function(e){return(NULL)})
  result$performanceEvaluation$thresholdSummary <- tryCatch({utils::read.csv(file = file.path(dirPath, 'performanceEvaluation','thresholdSummary.csv'))}, error = function(e){return(NULL)})
  result$performanceEvaluation$demographicSummary <- tryCatch({utils::read.csv(file = file.path(dirPath, 'performanceEvaluation','demographicSummary.csv'))}, error = function(e){return(NULL)})
  result$performanceEvaluation$calibrationSummary <- tryCatch({utils::read.csv(file = file.path(dirPath, 'performanceEvaluation','calibrationSummary.csv'))}, error = function(e){return(NULL)})
  result$performanceEvaluation$predictionDistribution <- tryCatch({utils::read.csv(file = file.path(dirPath, 'performanceEvaluation','predictionDistribution.csv'))}, error = function(e){return(NULL)})
  
  # load model settings
  result$model <- list(settings = list())
  ## result$model$settings$plpDataSettings
  ##result$model$settings$modelSettings
  ##result$model$settings$populationSettings
  ##result$model$settings$covariateSettings
  ## result$model$settings$featureEngineering
  ## result$model$settings$tidyCovariates
  ## result$model$settings$requireDenseMatrix
  ## result$model$settings$modelSettings$model
  ## result$model$settings$modelSettings$param
  ##result$model$settings$modelSettings$finalModelParameters
  ##result$model$settings$modelSettings$extraSettings
  ##result$model$settings$splitSettings
  
  # add the model class 
  class(result$model) <- "plpModel"
  attr(result$model, "predictionType") <- 'missing' #save and load these
  attr(result$model, "modelType") <- 'missing' #save and load these
  
  class(result) <- "runPlp"
  return(result)
}
