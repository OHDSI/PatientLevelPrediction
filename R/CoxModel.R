# @file lassoCoxModel.R
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

#' Create setting for lasso Cox model
#'
#' @param variance   a single value used as the starting value for the automatic lambda search
#' @param seed       An option to add a seed when training the model
#'
#' @examples
#' model.lr <- setCoxModel()
#' @export
setCoxModel<- function(variance=0.01, seed=NULL){
  if(!class(seed)%in%c('numeric','NULL','integer'))
    stop('Invalid seed')
  if(!class(variance) %in% c("numeric", "integer"))
    stop('Variance must be numeric')
  if(variance<0)
    stop('Variance must be >= 0')
  
  result <- list(model='fitCoxModel', param=list(variance=variance, seed=seed), name="Lasso Cox Regression")
  class(result) <- 'modelSettings' 
  
  return(result)
}

fitCoxModel<- function(population, plpData, param, search='adaptive', 
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
    ParallelLogger::logError('Cox regression requires plpData in coo format')
    stop()
  }
  
  metaData <- attr(population, 'metaData')
  if(!is.null(population$indexes))
    population <- population[population$indexes>0,]
  attr(population, 'metaData') <- metaData
  #TODO - how to incorporate indexes?
  variance <- 0.003
  if(!is.null(param$variance )) variance <- param$variance
  start <- Sys.time()
  modelTrained <- fitGLMModel(population,
                              plpData = plpData,
                              modelType = "cox",
                              prior = createPrior("laplace",useCrossValidation = TRUE),
                              control = createControl(noiseLevel = ifelse(trace,"quiet","silent"), cvType = "auto",
                                                      startingVariance = variance,
                                                      tolerance  = 2e-07,
                                                      cvRepetitions = 1, fold=ifelse(!is.null(population$indexes),max(population$indexes),1),
                                                      selectorType = "byRow",
                                                      threads=-1,
                                                      maxIterations = 3000,
                                                      seed=param$seed))
  
  # TODO get optimal lambda value
  
  comp <- Sys.time() - start
  varImp <- data.frame(covariateId=names(modelTrained$coefficients)[names(modelTrained$coefficients)!='(Intercept)'], 
                       value=modelTrained$coefficients[names(modelTrained$coefficients)!='(Intercept)'])
  if(sum(abs(varImp$value)>0)==0){
    ParallelLogger::logWarn('No non-zero coefficients')
    varImp <- NULL
  } else {
    #varImp <- varImp[abs(varImp$value)>0,]
    varImp <- merge(ff::as.ram(plpData$covariateRef), varImp, 
                    by='covariateId',all=T)
    varImp$value[is.na(varImp$value)] <- 0
    varImp <- varImp[order(-abs(varImp$value)),]
    colnames(varImp)[colnames(varImp)=='value'] <- 'covariateValue'
  }
  
  #get prediction on test set:
  prediction <- predict.plp(plpModel=list(model = modelTrained),
                            population = population, 
                            plpData = plpData)
  
  result <- list(model = modelTrained,
                 modelSettings = list(model='cox_lasso', modelParameters=param), #todo get lambda as param
                 hyperParamSearch = c(priorVariance=modelTrained$priorVariance, 
                                      seed=ifelse(is.null(param$seed), 'NULL', param$seed  ), 
                                      log_likelihood = modelTrained$log_likelihood),
                 trainCVAuc = NULL,
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
