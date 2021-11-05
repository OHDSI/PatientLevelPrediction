# @file IterativeHardThresholding.R
#
# Copyright 2019 Observational Health Data Sciences and Informatics
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

#' Create setting for lasso logistic regression
#'  
#' @param K              The maximum number of non-zero predictors
#' @param penalty        Specifies the IHT penalty; possible values are `BIC` or `AIC` or a numeric value
#' @param seed           An option to add a seed when training the model
#' @param exclude        A vector of numbers or covariateId names to exclude from prior
#' @param fitBestSubset  Logical: Fit final subset with no regularization 
#'
#' @examples
#' model.lr <- setLassoLogisticRegression()
#' @export
setIterativeHardThresholding<- function(K, penalty = "bic", seed = NULL, exclude = NULL, fitBestSubset = FALSE){
  if(K<1)
    stop('Invalid maximum number of predictors')
  if(!(penalty %in% c("aic", "bic") || is.numeric(penalty)))
    stop('Penalty must be "aic", "bic" or numeric')
  if(!is.logical(fitBestSubset))
    stop("fitBestSubset must be of type: logical")
  if(!class(seed)%in%c('numeric','NULL','integer'))
    stop('Invalid seed')
  
  # set seed
  if(is.null(seed[1])){
    seed <- as.integer(sample(100000000,1))
  }
  
  result <- list(model='fitIterativeHardThresholding', param=list(K = K, penalty = penalty, seed=seed[1], exclude = exclude), name="Iterative Hard Thresholding")
  class(result) <- 'modelSettings' 
  
  return(result)
}

fitIterativeHardThresholding<- function(population, plpData, param, search='adaptive', 
                                      outcomeId, cohortId, trace=F,...){
  
  # check logger
  if(length(ParallelLogger::getLoggers())==0){
    logger <- ParallelLogger::createLogger(name = "SIMPLE",
                                           threshold = "INFO",
                                           appenders = list(ParallelLogger::createConsoleAppender(layout = ParallelLogger::layoutTimestamp)))
    ParallelLogger::registerLogger(logger)
  }
  
  # check plpData is coo format:
  if(!'ffdf'%in%class(plpData$covariates)){
    ParallelLogger::logError('Iterative Hard Thresholding requires plpData in coo format')
    stop()
  }
  
  metaData <- attr(population, 'metaData')
  if(!is.null(population$indexes))
    population <- population[population$indexes>0,]
  attr(population, 'metaData') <- metaData

  start <- Sys.time()
  modelTrained <- fitGLMModel(population = population,
                              plpData = plpData,
                              modelType = "logistic", 
                              prior = IterativeHardThresholding::createIhtPrior(K  = param$K, 
                                                                                penalty = param$penalty, 
                                                                                exclude = param$exclude),
                              control = Cyclops::createControl()
                              
                              )
                              
  
  # TODO get optimal lambda value
  ParallelLogger::logTrace('Returned from fitting to LassoLogisticRegression')
  comp <- Sys.time() - start
  varImp <- data.frame(covariateId=names(modelTrained$coefficients)[names(modelTrained$coefficients)!='(Intercept)'], 
                       value=modelTrained$coefficients[names(modelTrained$coefficients)!='(Intercept)'])
  if(sum(abs(varImp$value)>0)==0){
    ParallelLogger::logWarn('No non-zero coefficients')
    varImp <- NULL
  } else {
    ParallelLogger::logInfo('Creating variable importance data frame')
    #varImp <- varImp[abs(varImp$value)>0,]
    varImp <- merge(ff::as.ram(plpData$covariateRef), varImp, 
                    by='covariateId',all=T)
    varImp$value[is.na(varImp$value)] <- 0
    varImp <- varImp[order(-abs(varImp$value)),]
    colnames(varImp)[colnames(varImp)=='value'] <- 'covariateValue'
  }
  
  #get prediction on test set:
  ParallelLogger::logInfo('Getting predictions on train set')
  prediction <- predict.plp(plpModel=list(model = modelTrained),
                            population = population, 
                            plpData = plpData)
  
  result <- list(model = modelTrained,
                 modelSettings = list(model='iht', modelParameters=param), #todo get lambda as param
                 metaData = plpData$metaData,
                 populationSettings = attr(population, 'metaData'),
                 outcomeId=outcomeId,# can use populationSettings$outcomeId?
                 cohortId=cohortId,
                 varImp = varImp,
                 trainingTime=comp,
                 covariateMap = NULL,
                 predictionTrain = prediction
  )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'plp'
  attr(result, 'predictionType') <- 'binary'
  return(result)
}
