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
  for(i in 2:length(plpData$metaData$call)){
    if(!is.null(plpData$metaData$call[[i]]))
      plpData$metaData$call[[i]] <- eval(plpData$metaData$call[[i]], envir = envir)
  }
  
  #FeatureExtraction::saveCovariateData(covariateData = plpData$covariateData, file = file.path(file, "covariates"))
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
  if(!moveFile){
    ParallelLogger::logError('Moving model files error')
  }
  #============================================================
    

  # if deep (keras) then save hdfs
  if(attr(plpModel, 'type') == "xgboost"){
    # fixing xgboost save/load issue
    xgboost::xgb.save(model = plpModel$model, fname = file.path(dirPath, "model.json"))
  } else {  
    saveRDS(plpModel$model, file = file.path(dirPath, "model.rds"))
  }
  saveRDS(NULL, file = file.path(dirPath, "transform.rds"))
  saveRDS(plpModel$index, file = file.path(dirPath, "index.rds"))
  saveRDS(plpModel$trainCVAuc, file = file.path(dirPath, "trainCVAuc.rds"))
  saveRDS(plpModel$hyperParamSearch, file = file.path(dirPath, "hyperParamSearch.rds"))
  saveRDS(plpModel$modelSettings, file = file.path(dirPath,  "modelSettings.rds"))
  saveRDS(plpModel$metaData, file = file.path(dirPath, "metaData.rds"))
  saveRDS(plpModel$populationSettings, file = file.path(dirPath, "populationSettings.rds"))
  saveRDS(plpModel$trainingTime, file = file.path(dirPath,  "trainingTime.rds"))
  saveRDS(plpModel$varImp, file = file.path(dirPath,  "varImp.rds"))
  saveRDS(plpModel$dense, file = file.path(dirPath,  "dense.rds"))
  saveRDS(plpModel$cohortId, file = file.path(dirPath,  "cohortId.rds"))
  saveRDS(plpModel$outcomeId, file = file.path(dirPath,  "outcomeId.rds"))
  saveRDS(plpModel$analysisId, file = file.path(dirPath,  "analysisId.rds"))
  #if(!is.null(plpModel$covariateMap))
  saveRDS(plpModel$covariateMap, file = file.path(dirPath,  "covariateMap.rds"))
  
  attributes <- list(type=attr(plpModel, 'type'), predictionType=attr(plpModel, 'predictionType') )
  saveRDS(attributes, file = file.path(dirPath,  "attributes.rds"))
  
}

moveHdModel <- function(plpModel, dirPath ){
  #==================================================================
  # if python then move pickle
  #==================================================================
  if(attr(plpModel, 'type') %in% c('pythonOld','pythonReticulate', 'pythonAuto') ){
    if(!dir.exists(file.path(dirPath,'python_model')))
      dir.create(file.path(dirPath,'python_model'))
    for(file in dir(plpModel$model)){   #DOES THIS CORRECTLY TRANSFER AUTOENCODER BITS?
      file.copy(file.path(plpModel$model,file), 
                file.path(dirPath,'python_model'), overwrite=TRUE,  recursive = FALSE,
                copy.mode = TRUE, copy.date = FALSE)
    }
  }
  
  #==================================================================
  # if knn then move model
  #==================================================================
  if(attr(plpModel, 'type') =='knn'){
    if(!dir.exists(file.path(dirPath,'knn_model')))
      dir.create(file.path(dirPath,'knn_model'))
    for(file in dir(plpModel$model)){
      file.copy(file.path(plpModel$model,file), 
                file.path(dirPath,'knn_model'), overwrite=TRUE,  recursive = FALSE,
                copy.mode = TRUE, copy.date = FALSE)
    }
  }
  
  return(TRUE)
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
  
  hyperParamSearch <- tryCatch(readRDS(file.path(dirPath, "hyperParamSearch.rds")),
                               error=function(e) NULL)
  # add in these as they got dropped
  outcomeId <- tryCatch(readRDS(file.path(dirPath, "outcomeId.rds")),
                        error=function(e) NULL)
  cohortId <- tryCatch(readRDS(file.path(dirPath, "cohortId.rds")),
                       error=function(e) NULL)  
  dense <- tryCatch(readRDS(file.path(dirPath, "dense.rds")),
                    error=function(e) NULL)  
  covariateMap <- tryCatch(readRDS(file.path(dirPath, "covariateMap.rds")),
                           error=function(e) NULL) 
  analysisId <- tryCatch(readRDS(file.path(dirPath, "analysisId.rds")),
                           error=function(e) NULL) 
  
  if(readRDS(file.path(dirPath, "attributes.rds"))$type == "xgboost"){
    ensure_installed("xgboost")
    if('model' %in% dir(dirPath)){
      model <- xgboost::xgb.load(file.path(dirPath, "model"))
    } else{
      model <- xgboost::xgb.load(file.path(dirPath, "model.json"))
    }
  } else {  
    model <- readRDS(file.path(dirPath, "model.rds"))
  }
  
  result <- list(model = model,
                 modelSettings = readRDS(file.path(dirPath, "modelSettings.rds")),
                 hyperParamSearch = hyperParamSearch,
                 trainCVAuc = readRDS(file.path(dirPath, "trainCVAuc.rds")),
                 metaData = readRDS(file.path(dirPath, "metaData.rds")),
                 populationSettings= readRDS(file.path(dirPath, "populationSettings.rds")),
                 outcomeId = outcomeId,
                 cohortId = cohortId,
                 varImp = readRDS(file.path(dirPath, "varImp.rds")),
                 trainingTime = readRDS(file.path(dirPath, "trainingTime.rds")),
                 covariateMap =covariateMap,
                 predict = readRDS(file.path(dirPath, "transform.rds")),
                 index = readRDS(file.path(dirPath, "index.rds")),
                 dense = dense,
                 analysisId = analysisId)
  
  attributes <- readRDS(file.path(dirPath, "attributes.rds"))
  attr(result, 'type') <- attributes$type
  attr(result, 'predictionType') <- attributes$predictionType
  class(result) <- "plpModel"
  
  # update the model location to the load dirPath
  result <- updateModelLocation(result, dirPath)
  
  # make this backwrds compatible for ffdf:
  result$predict <- createTransform(result)
  
  return(result)
}

updateModelLocation  <- function(plpModel, dirPath){
  type <- attr(plpModel, 'type')
  # if python update the location
  if( type %in% c('pythonOld','pythonReticulate', 'pythonAuto')){
    plpModel$model <- file.path(dirPath,'python_model')
    ##plpModel$predict <- createTransform(plpModel)
  }
  # if knn update the locaiton - TODO !!!!!!!!!!!!!!
  if( type =='knn'){
    plpModel$model <- file.path(dirPath,'knn_model')
    ##plpModel$predict <- createTransform(plpModel)
  }
  
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
  saveRDS(result$analysisRef, file = file.path(dirPath, "analysisRef.rds"))
  saveRDS(result$inputSetting, file = file.path(dirPath, "inputSetting.rds"))
  saveRDS(result$executionSummary, file = file.path(dirPath, "executionSummary.rds"))
  saveRDS(result$prediction, file = file.path(dirPath, "prediction.rds"))
  saveRDS(result$performanceEvaluation, file = file.path(dirPath, "performanceEvaluation.rds"))
  #saveRDS(result$performanceEvaluationTrain, file = file.path(dirPath, "performanceEvaluationTrain.rds"))
  saveRDS(result$covariateSummary, file = file.path(dirPath, "covariateSummary.rds"))
  
  
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
  
  
  result <- list(model = loadPlpModel(file.path(dirPath, "model")),
                 analysisRef = readRDS(file.path(dirPath, "analysisRef.rds")),
                 inputSetting = readRDS(file.path(dirPath, "inputSetting.rds")),
                 executionSummary = readRDS(file.path(dirPath, "executionSummary.rds")),
                 prediction = readRDS(file.path(dirPath, "prediction.rds")),
                 performanceEvaluation = readRDS(file.path(dirPath, "performanceEvaluation.rds")),
                 #performanceEvaluationTrain= readRDS(file.path(dirPath, "performanceEvaluationTrain.rds")),
                 covariateSummary = readRDS(file.path(dirPath, "covariateSummary.rds"))
  )
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
  
  #inputSetting
  if(!dir.exists(file.path(dirPath, 'inputSetting'))){dir.create(file.path(dirPath, 'inputSetting'), recursive = T)}
  utils::write.csv(result$inputSetting$modelSettings$model, file = file.path(dirPath, 'inputSetting','modelSettings_model.csv'), row.names = F)
  
  if(!is.null(result$inputSetting$modelSettings$param)){
    utils::write.csv(as.data.frame(t(unlist(result$inputSetting$modelSettings$param))), file = file.path(dirPath, 'inputSetting','modelSettings_param.csv'), row.names = F)
  }else{
    utils::write.csv(NULL, file = file.path(dirPath, 'inputSetting','modelSettings_param.csv'), row.names = F)
  }
  utils::write.csv(result$inputSetting$modelSettings$name, file = file.path(dirPath, 'inputSetting','modelSettings_name.csv'), row.names = F)
  if(!is.null(result$inputSetting$dataExtrractionSettings$covariateSettings)){
    utils::write.csv(formatCovariateSettings(result$inputSetting$dataExtrractionSettings$covariateSettings)$cvs, file = file.path(dirPath, 'inputSetting','dataExtrractionSettings_covariateSettings.csv'), row.names = F)
    utils::write.csv(formatCovariateSettings(result$inputSetting$dataExtrractionSettings$covariateSettings)$fun, file = file.path(dirPath, 'inputSetting','dataExtrractionSettings_covariateSettings_fun.csv'), row.names = F)
  }
  utils::write.csv(result$inputSetting$populationSettings$attrition, file = file.path(dirPath, 'inputSetting','populationSettings_attrition.csv'), row.names = F)
  result$inputSetting$populationSettings$attrition <- NULL
  utils::write.csv(result$inputSetting$populationSettings, file = file.path(dirPath, 'inputSetting','populationSettings.csv'), row.names = F)
  
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
  if(sum(!c('covariateSummary','executionSummary','inputSetting','performanceEvaluation')%in%objects)>0){
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
  
  #inputSetting
  result$inputSetting <- list()
  result$inputSetting$modelSettings$model <- tryCatch({utils::read.csv(file = file.path(dirPath, 'inputSetting','modelSettings_model.csv'))$x}, error = function(e){return(NULL)})
  result$inputSetting$modelSettings$param <- tryCatch({as.list(utils::read.csv(file = file.path(dirPath, 'inputSetting','modelSettings_param.csv')))}, error = function(e){return(NULL)})
  result$inputSetting$modelSettings$name <- tryCatch({utils::read.csv(file = file.path(dirPath, 'inputSetting','modelSettings_name.csv'))$x}, error = function(e){return(NULL)})
  
  result$inputSetting$dataExtrractionSettings$covariateSettings <- tryCatch({reformatCovariateSettings(file.path(dirPath, 'inputSetting','dataExtrractionSettings_covariateSettings.csv'))}, error = function(e){return(NULL)})

  result$inputSetting$populationSettings <- tryCatch({as.list(utils::read.csv(file = file.path(dirPath, 'inputSetting','populationSettings.csv')))}, error = function(e){return(NULL)})
  result$inputSetting$populationSettings$attrition <- tryCatch({utils::read.csv(file = file.path(dirPath, 'inputSetting','populationSettings_attrition.csv'))}, error = function(e){return(NULL)})
  
  #performanceEvaluation
  result$performanceEvaluation <- list()
  result$performanceEvaluation$evaluationStatistics <- tryCatch({utils::read.csv(file = file.path(dirPath, 'performanceEvaluation','evaluationStatistics.csv'))}, error = function(e){return(NULL)})
  result$performanceEvaluation$thresholdSummary <- tryCatch({utils::read.csv(file = file.path(dirPath, 'performanceEvaluation','thresholdSummary.csv'))}, error = function(e){return(NULL)})
  result$performanceEvaluation$demographicSummary <- tryCatch({utils::read.csv(file = file.path(dirPath, 'performanceEvaluation','demographicSummary.csv'))}, error = function(e){return(NULL)})
  result$performanceEvaluation$calibrationSummary <- tryCatch({utils::read.csv(file = file.path(dirPath, 'performanceEvaluation','calibrationSummary.csv'))}, error = function(e){return(NULL)})
  result$performanceEvaluation$predictionDistribution <- tryCatch({utils::read.csv(file = file.path(dirPath, 'performanceEvaluation','predictionDistribution.csv'))}, error = function(e){return(NULL)})
  
  result$model$modelSettings <- result$inputSetting$modelSettings
  result$model$populationSettings <- result$inputSetting$populationSettings
  result$model$metaData$call$covariateSettings <- result$inputSetting$dataExtrractionSettings$covariateSettings
  
  # add the model class 
  class(result$model) <- "plpModel"
  attr(result$model, "type") <- 'missing'
  
  class(result) <- "runPlp"
  return(result)
}
