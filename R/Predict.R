# @file predict.R
#
# Copyright 2020 Observational Health Data Sciences and Informatics
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
  if(length(ParallelLogger::getLoggers())==0){
    logger <- ParallelLogger::createLogger(name = "SIMPLE",
                                           threshold = "INFO",
                                           appenders = list(ParallelLogger::createConsoleAppender(layout = ParallelLogger::layoutTimestamp)))
    ParallelLogger::registerLogger(logger)
  }
  
  # apply the feature transformations
  if(!is.null(index)){
    ParallelLogger::logTrace(paste0('Calculating prediction for ',sum(index$index<0),' in test set'))
    ind <- population$rowId%in%index$rowId[index$index<0]
  } else{
    ParallelLogger::logTrace(paste0('Calculating prediction for ',nrow(population),' in dataset'))
    ind <- rep(T, nrow(population))
  }
  
  # do the predction on the new data
  if(class(plpModel)=='plpModel'){
    # extract the classifier type
    prediction <- plpModel$predict(plpData=plpData,population=population[ind,])
    
    if(nrow(prediction)!=nrow(population[ind,]))
      ParallelLogger::logWarn(paste0('Dimension mismatch between prediction and population test cases.  Population test: ',nrow(population[ind, ]), '-- Prediction:', nrow(prediction) ))
  } else{
    ParallelLogger::logError('Non plpModel input')
    stop()
  }
  
  metaData <- list(predictionType = attr(plpModel, 'predictionType'), #"binary", 
                   cohortId = attr(population,'metaData')$cohortId,
                   outcomeId = attr(population,'metaData')$outcomeId,
                   timepoint = attr(population,'metaData')$riskWindowEnd)
  
  attr(prediction, "metaData") <- metaData
  return(prediction)
}

# default patient level prediction prediction  
predict.plp <- function(plpModel,population, plpData, ...){
  ## done in transform covariateData <- limitCovariatesToPopulation(plpData$covariateData, population$rowId)
  ParallelLogger::logTrace('predict.plp - predictingProbabilities start')
  prediction <- predictProbabilities(plpModel$model, population, 
                                     plpData$covariateData)
  ParallelLogger::logTrace('predict.plp - predictingProbabilities end')
  
  # add baselineHazard function
  if(!is.null(plpModel$model$baselineHazard)){
    time <- attr(population,'metaData')$riskWindowEnd
    bhind <- which.min(abs(plpModel$model$baselineHazard$time-time))
    prediction$value <- 1-plpModel$model$baselineHazard$surv[bhind]^prediction$value
    
    attr(prediction, "baselineHazard") <- plpModel$model$baselineHazard$surv[bhind]
    attr(prediction, "timePoint") <- time
    attr(prediction, "offset") <- 0
  }
  
  return(prediction)
}

# for gxboost
predict.xgboost <- function(plpModel,population, plpData, ...){ 
  result <- toSparseM(plpData, population, map=plpModel$covariateMap)
  data <- result$data[population$rowId,, drop = F]
  prediction <- data.frame(rowId=population$rowId,
                           value=stats::predict(plpModel$model, data)
  )
  
  prediction <- merge(population, prediction, by='rowId', all.x=T, fill=0)
  prediction <- prediction[,colnames(prediction)%in%c('rowId','subjectId','cohortStartDate','outcomeCount','indexes', 'value','ageYear', 'gender')] # need to fix no index issue
  attr(prediction, "metaData") <- list(predictionType = "binary") 
  return(prediction)
  
}

predict.pythonReticulate <- function(plpModel, population, plpData){
  
  
  python_predict <- python_predict_temporal <- function(){return(NULL)}
  
  e <- environment()
  reticulate::source_python(system.file(package='PatientLevelPrediction','python','predictFunctions.py'), envir = e)
  
  
  ParallelLogger::logInfo('Mapping covariates...')
  if(!is.null(plpData$timeRef)){
    pdata <- toSparseTorchPython(plpData,population, map=plpModel$covariateMap, temporal=T)
    pdata <- pdata$data
    fun_predict <- python_predict_temporal
  } else {  
    newData <- toSparseM(plpData, population, map=plpModel$covariateMap)
    included <- plpModel$varImp$covariateId[plpModel$varImp$included>0] # does this include map?
    included <- newData$map$newCovariateId[newData$map$oldCovariateId%in%included] 
    pdata <- reticulate::r_to_py(newData$data[,included, drop = F])
    fun_predict <- python_predict
  }
  
  population$rowIdPython <- population$rowId-1 # -1 to account for python/r index difference
  namesInd <- c('rowIdPython','rowId')%in%colnames(population)
  namesInd <- c('rowIdPython','rowId')[namesInd]
  pPopulation <- as.matrix(population[,namesInd])
  
  # run the python predict code:
  ParallelLogger::logInfo('Executing prediction...')
  result <- fun_predict(population = pPopulation, 
                        plpData = pdata, 
                        model_loc = plpModel$model,
                        dense = ifelse(is.null(plpModel$dense),0,plpModel$dense),
                        autoencoder = F)
  
  #get the prediction from python and reformat:
  ParallelLogger::logInfo('Returning results...')
  prediction <- result
  prediction <- as.data.frame(prediction)
  attr(prediction, "metaData") <- list(predictionType="binary")
  colnames(prediction) <- c(namesInd, 'value')
  
  # add 1 to rowId from python:
  ##prediction$rowId <- prediction$rowId+1
  
  # add subjectId and date:
  prediction <- merge(prediction[,colnames(prediction)!='rowIdPython'],
                      population, 
                      by='rowId')
  prediction <- prediction[,colnames(prediction)%in%c('rowId','subjectId','cohortStartDate','outcomeCount','indexes', 'value','ageYear', 'gender')]
  return(prediction)
}

predict.pythonAuto <- function(plpModel, population, plpData){
  
  python_predict <- function(){return(NULL)}
  
  ParallelLogger::logInfo('Mapping covariates...')
  if(!is.null(plpData$timeRef)){
    pdata <- toSparseTorchPython(plpData,population, map=plpModel$covariateMap, temporal=T)
    pdata <- pdata$data
  } else {  
    newData <- toSparseM(plpData, population, map=plpModel$covariateMap)
    included <- plpModel$varImp$covariateId[plpModel$varImp$included>0] # does this include map?
    included <- newData$map$newCovariateId[newData$map$oldCovariateId%in%included] 
    pdata <- reticulate::r_to_py(newData$data[,included])
  }
  
  population$rowIdPython <- population$rowId-1 # -1 to account for python/r index difference
  namesInd <- c('rowIdPython','rowId')%in%colnames(population)
  namesInd <- c('rowIdPython','rowId')[namesInd]
  pPopulation <- as.matrix(population[,namesInd])
  
  # run the python predict code:
  ParallelLogger::logInfo('Executing prediction...')
  e <- environment()
  reticulate::source_python(system.file(package='PatientLevelPrediction','python','predictFunctions.py'), envir = e)
  
  result <- python_predict(population = pPopulation, 
                           plpData = pdata, 
                           model_loc = plpModel$model,
                           dense = ifelse(is.null(plpModel$dense),0,plpModel$dense),
                           autoencoder = T)
  #get the prediction from python and reformat:
  ParallelLogger::logInfo('Returning results...')
  prediction <- result
  prediction <- as.data.frame(prediction)
  attr(prediction, "metaData") <- list(predictionType="binary")
  colnames(prediction) <- c(namesInd, 'value')
  
  prediction <- merge(prediction[,colnames(prediction)!='rowIdPython'],
                      population, 
                      by='rowId')
  prediction <- prediction[,colnames(prediction)%in%c('rowId','subjectId','cohortStartDate','outcomeCount','indexes', 'value','ageYear', 'gender')]
  return(prediction)
}



predict.knn <- function(plpData, population, plpModel, ...){
  ##covariateData <- limitCovariatesToPopulation(plpData$covariateData, population$rowId)
  prediction <- BigKnn::predictKnn(covariates = plpData$covariateData$covariates,
                                   cohorts= population[,!colnames(population)%in%'cohortStartDate'],
                                   indexFolder = plpModel$model,
                                   k = plpModel$modelSettings$modelParameters$k,
                                   weighted = TRUE,
                                   threads = plpModel$modelSettings$threads)
  
  # can add: threads = 1 in the future
  
  # return the cohorts as a data frame with the prediction added as 
  # a new column with the column name 'value'
  prediction <- merge(population, prediction[,c('rowId','value')], by='rowId', 
                      all.x=T, fill=0)
  prediction$value[is.na(prediction$value)] <- 0
  
  return(prediction)
}


predict.deep <- function(plpModel, population, plpData,   ...){
  ensure_installed("plyr")
  
  temporal <- !is.null(plpData$timeRef)
  ParallelLogger::logDebug(paste0('timeRef null: ',is.null(plpData$timeRef)))
  if(temporal){
    ParallelLogger::logTrace('temporal')
    result<-toSparseM(plpData,population,map=plpModel$covariateMap, temporal=T)
    
    data <-result$data[population$rowId,,]
    if(!is.null(plpModel$useVae)){
      if(plpModel$useVae==TRUE){
        data<- plyr::aaply(as.array(data), 2, function(x) stats::predict(plpModel$vaeEncoder, x, batch_size = plpModel$vaeBatchSize))
        data<-aperm(data, perm = c(2,1,3))#rearrange of dimension
      }}
    
    batch_size <- min(2000, length(population$rowId))
    maxVal <- length(population$rowId)
    batches <- lapply(1:ceiling(maxVal/batch_size), function(x) ((x-1)*batch_size+1):min((x*batch_size),maxVal))
    prediction <- population
    prediction$value <- 0
    for(batch in batches){
      pred <- keras::predict_on_batch(plpModel$model, as.array(data[batch,,]))
      if(is.null(dim(pred))){
        prediction$value[batch] <- as.double(pred)
      } else{
        prediction$value[batch] <- as.double(pred[,2])
      }
    }
    
    prediction <- prediction[,colnames(prediction)%in%c('rowId','subjectId','cohortStartDate','outcomeCount','indexes', 'value','ageYear', 'gender')] # need to fix no index issue
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
      prediction$value[batch] <- as.double(pred[,2])
    }
    
    prediction <- prediction[,colnames(prediction)%in%c('rowId','subjectId','cohortStartDate','outcomeCount','indexes', 'value','ageYear', 'gender')] # need to fix no index issue
    return(prediction)
    
  }
}

predict.BayesianDeep <- function(plpModel, population, plpData,   ...){
  ensure_installed("plyr")
  
  temporal <- !is.null(plpData$timeRef)
  ParallelLogger::logDebug(paste0('timeRef null: ',is.null(plpData$timeRef)))
  if(temporal){
    ParallelLogger::logTrace('temporal')
    result<-toSparseM(plpData,population,map=plpModel$covariateMap, temporal=T)
    
    data <-result$data[population$rowId,,]
    if(!is.null(plpModel$useVae)){
      if(plpModel$useVae==TRUE){
        data<- plyr::aaply(as.array(data), 2, function(x) stats::predict(plpModel$vaeEncoder, x, batch_size = plpModel$vaeBatchSize))
        data<-aperm(data, perm = c(2,1,3))#rearrange of dimension
      }}
    
    batch_size <- min(2000, length(population$rowId))
    maxVal <- length(population$rowId)
    batches <- lapply(1:ceiling(maxVal/batch_size), function(x) ((x-1)*batch_size+1):min((x*batch_size),maxVal))
    prediction <- population
    prediction$value <- 0
    prediction$epistemicUncertainty <- 0
    prediction$aleatoricUncertainty <- 0
    for(batch in batches){
      num_MC_samples = 100
      output_dim = 2
      pred <- keras::predict_on_batch(plpModel$model, as.array(data[batch,,]))
      MC_samples <- array(0, dim = c(num_MC_samples, length(batch), 2 * output_dim))
      for (k in 1:num_MC_samples){
        MC_samples[k,, ] = stats::predict(plpModel$model, as.array(data[batch,,]))
        #keras::predict_proba(model, as.array(plpData[population$rowId[population$indexes==index],,][batch,,]))
      }
      pred <- apply(MC_samples[,,output_dim], 2, mean)
      epistemicUncertainty <- apply(MC_samples[,,output_dim], 2, stats::var)
      logVar = MC_samples[, , output_dim * 2]
      if(length(dim(logVar))<=1){
        aleatoricUncertainty = exp(mean(logVar))
      }else{
        aleatoricUncertainty = exp(colMeans(logVar))
        
      }
      prediction$value[batch] <- pred
      prediction$epistemicUncertainty[batch] = epistemicUncertainty
      prediction$aleatoricUncertainty[batch] = aleatoricUncertainty
      #writeLines(paste0(dim(pred[,2]), collapse='-'))
      #writeLines(paste0(pred[1,2], collapse='-'))
      
    }
    prediction$value[prediction$value>1] <- 1
    prediction$value[prediction$value<0] <- 0
    prediction <- prediction[,colnames(prediction)%in%c('rowId','subjectId','cohortStartDate','outcomeCount','indexes', 'value','ageYear', 'gender',
                                                        'epistemicUncertainty', 'aleatoricUncertainty')] # need to fix no index issue
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
      num_MC_samples = 100
      output_dim =2
      MC_samples <- array(0, dim = c(num_MC_samples, length(batch), 2 * output_dim))
      for (k in 1:num_MC_samples){
        MC_samples[k,, ] = stats::predict(plpModel$model, as.array(data[batch,,]))
        #keras::predict_proba(model, as.array(plpData[population$rowId[population$indexes==index],,][batch,,]))
      }
      pred <- apply(MC_samples[,,output_dim], 2, mean)
      epistemicUncertainty <- apply(MC_samples[,,output_dim], 2, stats::var)
      logVar = MC_samples[, , output_dim * 2]
      if(length(dim(logVar))<=1){
        aleatoricUncertainty = exp(mean(logVar))
      }else{
        aleatoricUncertainty = exp(colMeans(logVar))
        
      }
      prediction$value[batch] <- pred
      prediction$epistemicUncertainty[batch] = epistemicUncertainty
      prediction$aleatoricUncertainty[batch] = aleatoricUncertainty
      #writeLines(paste0(dim(pred[,2]), collapse='-'))
      #writeLines(paste0(pred[1,2], collapse='-'))
    }
    prediction$value[prediction$value>1] <- 1
    prediction$value[prediction$value<0] <- 0
    prediction <- prediction[,colnames(prediction)%in%c('rowId','subjectId','cohortStartDate','outcomeCount','indexes', 'value','ageYear', 'gender',
                                                        'epistemicUncertainty', 'aleatoricUncertainty')] # need to fix no index issue
    return(prediction)
    
  }
}

predict.deepEnsemble <- function(plpModel, population, plpData,   ...){
  ensure_installed("plyr")
  
  mu <- function(){return(NULL)}
  sigma <- function(){return(NULL)}
  
  temporal <- !is.null(plpData$timeRef)
  ParallelLogger::logDebug(paste0('timeRef null: ',is.null(plpData$timeRef)))
  if(temporal){
    ParallelLogger::logTrace('temporal')
    result<-toSparseM(plpData,population,map=plpModel$covariateMap, temporal=T)
    
    data <-result$data[population$rowId,,]
    if(!is.null(plpModel$useVae)){
      if(plpModel$useVae==TRUE){
        data<- plyr::aaply(as.array(data), 2, function(x) stats::predict(plpModel$vaeEncoder, x, batch_size = plpModel$vaeBatchSize))
        data<-aperm(data, perm = c(2,1,3))#rearrange of dimension
      }}
    
    batch_size <- min(2000, length(population$rowId))
    maxVal <- length(population$rowId)
    batches <- lapply(1:ceiling(maxVal/batch_size), function(x) ((x-1)*batch_size+1):min((x*batch_size),maxVal))
    prediction <- population
    prediction$value <- 0
    
    prediction$sigmas <- 0
    for(batch in batches){
      for (i in seq(plpModel$modelSettings$modelParameters$numberOfEnsembleNetwork)){
        if(i==1){
          muMatrix <- data.frame()
          sigmaMatrix <-data.frame()
        }
        c(mu,sigma) %<-% plpModel$model[[i]](inputs=list(as.array(data[batch,,])))
        muMatrix<-rbind(muMatrix,t(as.data.frame(mu[,2])))
        sigmaMatrix<-rbind(sigmaMatrix,t(as.data.frame(sigma[,2])))
      }
      
      muMean <- apply(muMatrix,2,mean)
      muSq <- muMatrix^2
      sigmaSq <- sigmaMatrix^2
      sigmaMean <- apply(sigmaMatrix,2,mean)
      sigmaResult=apply(muSq+sigmaSq,2, mean)- muMean^2
      
      prediction$value[batch] <- c(muMean)
      prediction$sigmas[batch] <- c(sigmaResult)
    }
    prediction <- prediction[,colnames(prediction)%in%c('rowId','subjectId','cohortStartDate','outcomeCount','indexes', 'value', 'sigmas')] # need to fix no index issue
    #If the prediction value is negative, please add values to make all the values to positive. 
    if(min(prediction$value)<0){prediction$value = prediction$value+ (min(prediction$value)* (-1))  }
    return(prediction)
    
  } else{
    result<-toSparseM(plpData,population,map=plpModel$covariateMap, temporal=F)
    data <-result$data[population$rowId,]
    
    batch_size <- min(2000, length(population$rowId))
    maxVal <- length(population$rowId)
    batches <- lapply(1:ceiling(maxVal/batch_size), function(x) ((x-1)*batch_size+1):min((x*batch_size),maxVal))
    prediction <- population
    prediction$value <- 0
    
    prediction$sigmas <- 0
    for(batch in batches){
      for (i in seq(plpModel$modelSettings$modelParameters$numberOfEnsembleNetwork)){
        if(i==1){
          muMatrix <- data.frame()
          sigmaMatrix <-data.frame()
        }
        c(mu,sigma) %<-% plpModel$model[[i]](inputs=list(as.array(data[batch,,])))
        muMatrix<-rbind(muMatrix,t(as.data.frame(mu[,2])))
        sigmaMatrix<-rbind(sigmaMatrix,t(as.data.frame(sigma[,2])))
      }
      
      muMean <- apply(muMatrix,2,mean)
      muSq <- muMatrix^2
      sigmaSq <- sigmaMatrix^2
      sigmaMean <- apply(sigmaMatrix,2,mean)
      sigmaResult=apply(muSq+sigmaSq,2, mean)- muMean^2
      
      prediction$value[batch] <- c(muMean)
      prediction$sigmas[batch] <- c(sigmaResult)
    }
    
    prediction <- prediction[,colnames(prediction)%in%c('rowId','subjectId','cohortStartDate','outcomeCount','indexes', 'value', 'sigmas','ageYear', 'gender')] # need to fix no index issue
    if(min(prediction$value)<0){prediction$value = prediction$value+ (min(prediction$value)* (-1))  }
    return(prediction)
    
  }
}

predict.deepMulti <- function(plpModel, population, plpData,   ...){
  
  repeats <- attr(plpModel, 'inputs')
  
  temporal <- !is.null(plpData$timeRef)
  ParallelLogger::logDebug('timeRef null: ',paste0(is.null(plpData$timeRef)))
  if(temporal){
    ParallelLogger::logTrace('temporal')
    result<-toSparseM(plpData,population,map=plpModel$covariateMap, temporal=T)
    ParallelLogger::logDebug('result$data dim: ',paste0(dim(result$data), collapse = '-'))
    ParallelLogger::logDebug('population dim: ',paste0(dim(population), collapse = '-'))
    data <-result$data[population$rowId,,]
    
    ParallelLogger::logInfo('running batch prediction')
    batch_size <- min(2000, length(population$rowId))
    maxVal <- length(population$rowId)
    batches <- lapply(1:ceiling(maxVal/batch_size), function(x) ((x-1)*batch_size+1):min((x*batch_size),maxVal))
    prediction <- population
    prediction$value <- 0
    for(batch in batches){
      ParallelLogger::logDebug('batch length: ', length(batch))
      dat <- list()
      length(dat) <- repeats
      for( i in 1:repeats) {dat[[i]] <- as.array(data[batch,,])}
      
      pred <- keras::predict_on_batch(plpModel$model, dat)
      if(is.null(dim(pred))){
        ParallelLogger::logDebug('Pred length: ', length(pred))
        prediction$value[batch] <- as.double(pred)
      } else{
        ParallelLogger::logDebug('Pred dim: ', paste0(dim(pred), collapse = '-'))
        prediction$value[batch] <- as.double(pred[,2])
      }
    }
    
    prediction <- prediction[,colnames(prediction)%in%c('rowId','subjectId','cohortStartDate','outcomeCount','indexes', 'value','ageYear', 'gender')] # need to fix no index issue
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
      prediction$value[batch] <- as.double(pred)
    }
    
    prediction <- prediction[,colnames(prediction)%in%c('rowId','subjectId','cohortStartDate','outcomeCount','indexes', 'value','ageYear', 'gender')] # need to fix no index issue
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
#' @param covariateData        The covariateData containing the covariates for the population
#' @export
predictProbabilities <- function(predictiveModel, population, covariateData) {
  start <- Sys.time()
  
  ParallelLogger::logTrace('predictProbabilities - predictAndromeda start')
  prediction <- predictAndromeda(predictiveModel$coefficients,
                            population,
                            covariateData,
                            predictiveModel$modelType)
  ParallelLogger::logTrace('predictProbabilities - predictAndromeda end')
  prediction$time <- NULL
  attr(prediction, "modelType") <- predictiveModel$modelType
  attr(prediction, "cohortId") <- attr(population, "metadata")$cohortId
  attr(prediction, "outcomeId") <- attr(population, "metadata")$outcomeId
  
  if (predictiveModel$modelType %in% c("survival", "cox")) {
    attr(prediction, "timepoint") <- attr(population, "metadata")$riskWindowEnd
  }
  
  delta <- Sys.time() - start
  ParallelLogger::logInfo("Prediction took ", signif(delta, 3), " ", attr(delta, "units"))
  return(prediction)
}

#' Generated predictions from a regression model
#'
#' @param coefficients   A names numeric vector where the names are the covariateIds, except for the
#'                       first value which is expected to be the intercept.
#' @param population       A data frame containing the population to do the prediction for
#' @param covariateData     An andromeda object containing the covariateData with predefined columns
#'                       (see below).
#' @param modelType      Current supported types are "logistic", "poisson", "cox" or "survival".
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
predictAndromeda <- function(coefficients, population, covariateData, modelType = "logistic") {
  if (!(modelType %in% c("logistic", "poisson", "survival","cox"))) {
    stop(paste("Unknown modelType:", modelType))
  }
  if (!FeatureExtraction::isCovariateData(covariateData)){
    stop("Needs correct covariateData")
  }
  
  intercept <- coefficients[names(coefficients)%in%'(Intercept)']
  if(length(intercept)==0) intercept <- 0
  coefficients <- coefficients[!names(coefficients)%in%'(Intercept)']
  coefficients <- data.frame(beta = as.numeric(coefficients),
                             covariateId = bit64::as.integer64(names(coefficients)) #!@ modified 
                             )
  coefficients <- coefficients[coefficients$beta != 0, ]
  if(sum(coefficients$beta != 0)>0){
    covariateData$coefficients <- coefficients
    on.exit(covariateData$coefficients <- NULL, add = TRUE)
    
    prediction <- covariateData$covariates %>% 
      dplyr::inner_join(covariateData$coefficients, by= 'covariateId') %>% 
      dplyr::mutate(values = .data$covariateValue*.data$beta) %>%
      dplyr::group_by(.data$rowId) %>%
      dplyr::summarise(value = sum(.data$values, na.rm = TRUE)) %>%
      dplyr::select(.data$rowId, .data$value)
    
    prediction <- as.data.frame(prediction)
    prediction <- merge(population, prediction, by ="rowId", all.x = TRUE, fill = 0)
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
  } else if (modelType == "poisson" || modelType == "survival" || modelType == "cox") {
    prediction$value <- exp(prediction$value)
    #if(max(prediction$value)>1){
    #  prediction$value <- prediction$value/max(prediction$value)
    #}
  }
  return(prediction)
}

