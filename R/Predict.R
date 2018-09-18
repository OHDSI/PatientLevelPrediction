# @file predict.R
#
# Copyright 2018 Observational Health Data Sciences and Informatics
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

#' predictPlp
#'
#' @description
#' Predict the risk of the outcome using the input plpModel for the input plpData
#' @details
#' The function applied the trained model on the plpData to make predictions
#' @param plpModel                         An object of type \code{plpModel} - a patient level prediction model
#' @param population                       The population created using createStudyPopulation() who will have their risks predicted
#' @param plpData                          An object of type \code{plpData} - the patient level prediction
#'                                         data extracted from the CDM.
#' @param index                            A data frame containing rowId: a vector of rowids and index: a vector of doubles the same length as the rowIds. If used, only the rowIds with a negative index value are used to calculate the prediction.  
#' 
#' @return
#' A dataframe containing the prediction for each person in the population with an attribute metaData containing prediction details.
#'

# parent predict that calls the others
#' @export
predictPlp <- function(plpModel, population, plpData,  index=NULL){
  
  if(is.null(plpModel))
    stop('No model input')
  if(is.null(population))
    stop('No population input')
  if(is.null(plpData))
    stop('No plpData input')
  
  # check logger
  if(length(OhdsiRTools::getLoggers())==0){
    logger <- OhdsiRTools::createLogger(name = "SIMPLE",
                           threshold = "INFO",
                           appenders = list(OhdsiRTools::createConsoleAppender(layout = OhdsiRTools::layoutTimestamp)))
    OhdsiRTools::registerLogger(logger)
  }
  
  # apply the feature transformations
  if(!is.null(index)){
    OhdsiRTools::logTrace(paste0('Calculating prediction for ',sum(index$index<0),' in test set'))
    ind <- population$rowId%in%index$rowId[index$index<0]
  } else{
    OhdsiRTools::logTrace(paste0('Calculating prediction for ',nrow(population),' in dataset'))
    ind <- rep(T, nrow(population))
  }
  
  # do the predction on the new data
  if(class(plpModel)=='plpModel'){
    # extract the classifier type
    prediction <- plpModel$predict(plpData=plpData,population=population[ind,])
    
    if(nrow(prediction)!=nrow(population[ind,]))
      OhdsiRTools::logWarn(paste0('Dimension mismatch between prediction and population test cases.  Population test: ',nrow(population[ind, ]), '-- Prediction:', nrow(prediction) ))
  } else{
    OhdsiRTools::logError('Non plpModel input')
    stop()
  }
  
  metaData <- list(predictionType="binary",
                   cohortId = attr(population,'metaData')$cohortId,
                   outcomeId = attr(population,'metaData')$outcomeId)
  
  attr(prediction, "metaData") <- metaData
  return(prediction)
}

# default patient level prediction prediction  
predict.plp <- function(plpModel,population, plpData, ...){
  covariates <- limitCovariatesToPopulation(plpData$covariates, ff::as.ff(population$rowId))
  prediction <- predictProbabilities(plpModel$model, population, covariates)
  
  return(prediction)
}

# for gxboost
predict.xgboost <- function(plpModel,population, plpData, ...){ 
  result <- toSparseM(plpData, population, map=plpModel$covariateMap)
  data <- result$data[population$rowId,]
  prediction <- data.frame(rowId=population$rowId,
                           value=stats::predict(plpModel$model, data)
                           )
  
  prediction <- merge(population, prediction, by='rowId')
  prediction <- prediction[,colnames(prediction)%in%c('rowId','subjectId','cohortStartDate','outcomeCount','indexes', 'value')] # need to fix no index issue
  attr(prediction, "metaData") <- list(predictionType = "binary") 
  return(prediction)
  
}

# TODO: edit caret methods (or remove as slow - replace with python)
# caret model prediction 
predict.python <- function(plpModel, population, plpData){
  
  # connect to python if not connected
  initiatePython()

  OhdsiRTools::logInfo('Setting inputs...')
  PythonInR::pySet("dense", plpModel$dense)
  PythonInR::pySet("model_loc", plpModel$model)
  
  OhdsiRTools::logInfo('Mapping covariates...')
  #load python model mapping.txt
  # create missing/mapping using plpData$covariateRef
  if (plpModel$modelSettings$model == 'fitCNNTorch' | plpModel$modelSettings$model == 'fitRNNTorch'){
    #covariates <- plpData$covariates
    #covariates$rowIdPython <- covariates$rowId -1 #to account for python/r index difference
    #PythonInR::pySet('covariates', as.matrix(covariates[,c('rowIdPython','covariateId','timeId', 'covariateValue')]))
    result<- toSparseTorchPython(plpData,population,map=plpModel$covariateMap, temporal=T)
    PythonInR::pySet("modeltype", 'temporal')
    PythonInR::pySet("autoencoder", 0)
    python_dir <- system.file(package='PatientLevelPrediction','python')
    PythonInR::pySet("python_dir", python_dir)
  } else{
    newData <- toSparsePython(plpData, population, map=plpModel$covariateMap)
    PythonInR::pySet("modeltype", 'normal')
    PythonInR::pySet("autoencoder", 0)
    if (plpModel$modelSettings$model == 'fitLRTorch' | plpModel$modelSettings$model == 'fitMLPTorch'){
      python_dir <- system.file(package='PatientLevelPrediction','python')
      PythonInR::pySet("python_dir", python_dir)      
      if (plpModel$modelSettings$modelParameters$autoencoder | plpModel$modelSettings$modelParameters$vae){
        PythonInR::pySet("autoencoder", 1)
      }
    }
    included <- plpModel$varImp$covariateId[plpModel$varImp$included>0] # does this include map?
    included <- newData$map$newIds[newData$map$oldIds%in%included]-1 # python starts at 0, r at 1
    PythonInR::pySet("included", as.matrix(sort(included)))
  } 

  # save population
  if('indexes'%in%colnames(population)){
    population$rowIdPython <- population$rowId-1 # -1 to account for python/r index difference
    PythonInR::pySet('population', as.matrix(population[,c('rowIdPython','outcomeCount','indexes')]) )
    
  } else {
    population$rowIdPython <- population$rowId-1 # -1 to account for python/r index difference
    PythonInR::pySet('population', as.matrix(population[,c('rowIdPython','outcomeCount')]) )
  }
  
  # run the python predict code:
  OhdsiRTools::logInfo('Executing prediction...')
  PythonInR::pyExecfile(system.file(package='PatientLevelPrediction','python','python_predict.py'))
  
  #get the prediction from python and reformat:
  OhdsiRTools::logInfo('Returning results...')
  prediction <- PythonInR::pyGet('prediction', simplify = F)
  prediction <-  apply(prediction,1, unlist)
  prediction <- t(prediction)
  prediction <- as.data.frame(prediction)
  attr(prediction, "metaData") <- list(predictionType="binary")
  if(ncol(prediction)==4){
    colnames(prediction) <- c('rowId','outcomeCount','indexes', 'value')
  } else {
    colnames(prediction) <- c('rowId','outcomeCount', 'value')
  }
  
  # add 1 to rowId from python:
  prediction$rowId <- prediction$rowId+1
  
  # add subjectId and date:
  prediction <- merge(prediction,
                      population[,c('rowId','subjectId','cohortStartDate')], 
                      by='rowId')
  
  # TODO delete results
  
  return(prediction)
}

predict.knn <- function(plpData, population, plpModel, ...){
  covariates <- limitCovariatesToPopulation(plpData$covariates, ff::as.ff(population$rowId))
  prediction <- BigKnn::predictKnn(covariates = covariates,
                                   cohorts=ff::as.ffdf(population[,!colnames(population)%in%'cohortStartDate']),
                                   indexFolder = plpModel$model,
                                   k = plpModel$modelSettings$modelParameters$k,
                                   weighted = TRUE)
  
  # return the cohorts as a data frame with the prediction added as 
  # a new column with the column name 'value'
  prediction <- merge(population, prediction, by='rowId', 
                      all.x=T, fill=0)
  prediction$value[is.na(prediction$value)] <- 0
  
  return(prediction)
}


predict.deep <- function(plpModel, population, plpData,   ...){
  temporal <- !is.null(plpData$timeRef)
  OhdsiRTools::logDebug(paste0('timeRef null: ',is.null(plpData$timeRef)))
  if(temporal){
    OhdsiRTools::logTrace('temporal')
    result<-toSparseM(plpData,population,map=plpModel$covariateMap, temporal=T)
    
    data <-result$data[population$rowId,,]
    if(plpModel$useVae==TRUE){
      data<- plyr::aaply(as.array(data), 2, function(x) predict(plpModel$vaeEncoder, x, batch_size = plpModel$vaeBatchSize))
      data<-aperm(data, perm = c(2,1,3))#rearrange of dimension
    }
    
    batch_size <- min(2000, length(population$rowId))
    maxVal <- length(population$rowId)
    batches <- lapply(1:ceiling(maxVal/batch_size), function(x) ((x-1)*batch_size+1):min((x*batch_size),maxVal))
    prediction <- population
    prediction$value <- 0
    for(batch in batches){
      pred <- keras::predict_on_batch(plpModel$model, as.array(data[batch,,]))
      if(is.null(dim(pred))){
        prediction$value[batch] <- pred
      } else{
        prediction$value[batch] <- pred[,2]
      }
    }
    
    prediction <- prediction[,colnames(prediction)%in%c('rowId','subjectId','cohortStartDate','outcomeCount','indexes', 'value')] # need to fix no index issue
    return(prediction)
  } else{
    result<-toSparseM(plpData,population,map=plpModel$covariateMap, temporal=F)
    data <-result$data[population$rowId,]
    
    batch_size <- min(2000, length(population$rowId))
    maxVal <- length(population$rowId)
    batches <- lapply(1:ceiling(maxVal/batch_size), function(x) ((x-1)*batch_size+1):min((x*batch_size),maxVal))
    prediction <- population
    prediction$value <- 0
    for(batch in batches){
      pred <- keras::predict_on_batch(plpModel$model, as.array(data[batch,]))
      prediction$value[batch] <- pred
    }
    
    prediction <- prediction[,colnames(prediction)%in%c('rowId','subjectId','cohortStartDate','outcomeCount','indexes', 'value')] # need to fix no index issue
    return(prediction)
    
  }
}


predict.deepMulti <- function(plpModel, population, plpData,   ...){
  
  repeats <- attr(plpModel, 'inputs')
  
  temporal <- !is.null(plpData$timeRef)
  OhdsiRTools::logDebug('timeRef null: ',paste0(is.null(plpData$timeRef)))
  if(temporal){
    OhdsiRTools::logTrace('temporal')
    result<-toSparseM(plpData,population,map=plpModel$covariateMap, temporal=T)
    data <-result$data[population$rowId,,]
    
    batch_size <- min(2000, length(population$rowId))
    maxVal <- length(population$rowId)
    batches <- lapply(1:ceiling(maxVal/batch_size), function(x) ((x-1)*batch_size+1):min((x*batch_size),maxVal))
    prediction <- population
    prediction$value <- 0
    for(batch in batches){
      dat <- list()
      length(dat) <- repeats
      for( i in 1:repeats) {dat[[i]] <- as.array(data[batch,,])}

      pred <- keras::predict_on_batch(plpModel$model, dat)
      if(is.null(dim(pred))){
        prediction$value[batch] <- pred
      } else{
        prediction$value[batch] <- pred[,2]
      }
    }
    
    prediction <- prediction[,colnames(prediction)%in%c('rowId','subjectId','cohortStartDate','outcomeCount','indexes', 'value')] # need to fix no index issue
    return(prediction)
  } else{
    result<-toSparseM(plpData,population,map=plpModel$covariateMap, temporal=F)
    data <-result$data[population$rowId,]
    
    batch_size <- min(2000, length(population$rowId))
    maxVal <- length(population$rowId)
    batches <- lapply(1:ceiling(maxVal/batch_size), function(x) ((x-1)*batch_size+1):min((x*batch_size),maxVal))
    prediction <- population
    prediction$value <- 0
    for(batch in batches){
      dat <- list()
      length(dat) <- repeats
      for( i in 1:repeats) {dat[[i]] <- as.array(data[batch,,])}
      
      pred <- keras::predict_on_batch(plpModel$model, dat)
      prediction$value[batch] <- pred
    }
    
    prediction <- prediction[,colnames(prediction)%in%c('rowId','subjectId','cohortStartDate','outcomeCount','indexes', 'value')] # need to fix no index issue
    return(prediction)
    
  }
}


#' Create predictive probabilities
#'
#' @details
#' Generates predictions for the population specified in plpData given the model.
#'
#' @return
#' The value column in the result data.frame is: logistic: probabilities of the outcome, poisson:
#' Poisson rate (per day) of the outome, survival: hazard rate (per day) of the outcome.
#'
#' @param predictiveModel   An object of type \code{predictiveModel} as generated using
#'                          \code{\link{fitPlp}}.
#' @param population        The population to calculate the prediction for
#' @param covariates        The covariate part of PlpData containing the covariates for the population
#' @export
predictProbabilities <- function(predictiveModel, population, covariates) {
  start <- Sys.time()

  prediction <- predictFfdf(predictiveModel$coefficients,
                            population,
                            covariates,
                            predictiveModel$modelType)
  prediction$time <- NULL
  attr(prediction, "modelType") <- predictiveModel$modelType
  attr(prediction, "cohortId") <- attr(population, "metadata")$cohortId
  attr(prediction, "outcomeId") <- attr(population, "metadata")$outcomeId

  delta <- Sys.time() - start
  OhdsiRTools::logInfo("Prediction took", signif(delta, 3), attr(delta, "units"))
  return(prediction)
}

#' Generated predictions from a regression model
#'
#' @param coefficients   A names numeric vector where the names are the covariateIds, except for the
#'                       first value which is expected to be the intercept.
#' @param population       A data frame containing the population to do the prediction for
#' @param covariates     A data frame or ffdf object containing the covariates with predefined columns
#'                       (see below).
#' @param modelType      Current supported types are "logistic", "poisson", or "survival".
#'
#' @details
#' These columns are expected in the outcome object: \tabular{lll}{ \verb{rowId} \tab(integer) \tab
#' Row ID is used to link multiple covariates (x) to a single outcome (y) \cr \verb{time} \tab(real)
#' \tab For models that use time (e.g. Poisson or Cox regression) this contains time \cr \tab
#' \tab(e.g. number of days) \cr } These columns are expected in the covariates object: \tabular{lll}{
#' \verb{rowId} \tab(integer) \tab Row ID is used to link multiple covariates (x) to a single outcome
#' (y) \cr \verb{covariateId} \tab(integer) \tab A numeric identifier of a covariate \cr
#' \verb{covariateValue} \tab(real) \tab The value of the specified covariate \cr }
#'
#' @export
predictFfdf <- function(coefficients, population, covariates, modelType = "logistic") {
  if (!(modelType %in% c("logistic", "poisson", "survival"))) {
    stop(paste("Unknown modelType:", modelType))
  }
  if (class(covariates) != "ffdf") {
    stop("Covariates should be of type ffdf")
  }
  intercept <- coefficients[names(coefficients)%in%'(Intercept)']
  if(length(intercept)==0) intercept <- 0
  coefficients <- coefficients[!names(coefficients)%in%'(Intercept)']
  coefficients <- data.frame(beta = as.numeric(coefficients),
                             covariateId = as.numeric(names(coefficients)))
  coefficients <- coefficients[coefficients$beta != 0, ]
  if(sum(coefficients$beta != 0)>0){
    prediction <- merge(covariates, ff::as.ffdf(coefficients), by = "covariateId")
    prediction$value <- prediction$covariateValue * prediction$beta
    prediction <- bySumFf(prediction$value, prediction$rowId)
    colnames(prediction) <- c("rowId", "value")
   # prediction <- merge(population, ff::as.ram(prediction), by = "rowId", all.x = TRUE)
    prediction <- merge(ff::as.ram(population), prediction, by ="rowId", all.x = TRUE)
    prediction$value[is.na(prediction$value)] <- 0
    prediction$value <- prediction$value + intercept
  } else{
    warning('Model had no non-zero coefficients so predicted same for all population...')
    prediction <- population
    prediction$value <- rep(0, nrow(population)) + intercept
  }
  if (modelType == "logistic") {
    link <- function(x) {
      return(1/(1 + exp(0 - x)))
    }
    prediction$value <- link(prediction$value)
  } else if (modelType == "poisson" || modelType == "survival") {
    prediction$value <- exp(prediction$value)
  }
  return(prediction)
}

#' Compute sum of values binned by a second variable
#'
#' @param values   An ff object containing the numeric values to be summed
#' @param bins     An ff object containing the numeric values to bin by
#'
#' @examples
#' values <- ff::as.ff(c(1, 1, 2, 2, 1))
#' bins <- ff::as.ff(c(1, 1, 1, 2, 2))
#' bySumFf(values, bins)
#'
#' @export
bySumFf <- function(values, bins) {
  bySum(values, bins)
}

