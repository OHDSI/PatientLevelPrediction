# @file CompetingRiskModel.R
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

#' Create setting for competing risk model (uses Fine-Gray model in Cyclops)
#'
#' @param seed       An option to add a seed when training the model
#'
#' @examples
#' model.lr <- setCompetingRiskModel()
#' @export
setCompetingRiskModel<- function(seed=NULL){
  if(!class(seed)%in%c('numeric','NULL','integer'))
    stop('Invalid seed')
  
  result <- list(model='fitCompetingRiskModel', param=list(seed=seed), name="Competing Risk Model")
  class(result) <- 'modelSettings' 
  
  return(result)
}

fitCompetingRiskModell<- function(population, plpData, param, search='adaptive', 
                       outcomeId, cohortId, trace=F,...){
  
  # check logger
  if(length(ParallelLogger::getLoggers())==0){
    logger <- ParallelLogger::createLogger(name = "SIMPLE",
                                           threshold = "INFO",
                                           appenders = list(ParallelLogger::createConsoleAppender(layout = ParallelLogger::layoutTimestamp)))
    ParallelLogger::registerLogger(logger)
  }
  
  # check plpData is coo format:
  if (!FeatureExtraction::isCovariateData(plpData$covariateData)){
    ParallelLogger::logError('Competing risk model requires plpData in coo format')
    stop()
  }
  
  metaData <- attr(population, 'metaData')
  if(!is.null(population$indexes))
    population <- population[population$indexes>0,]
  attr(population, 'metaData') <- metaData
  #TODO - how to incorporate indexes?
  
  start <- Sys.time()
  
  fgDat <- Cyclops::getFineGrayWeights(population$survivalTime, population$outcomeCount)
  population$time <- population$survivalTime
  population$y <- population$outcomeCount
  population$censorWeights <- fgDat$weights 

  plpData$covariateData$outcomes <- population[,c('rowId','time','y','censorWeights')]
  
  dataPtr <-  Cyclops::convertToCyclopsData(plpData$covariateData$outcomes, 
                                  plpData$covariateData$covariates, 
                                  modelType = "fgr")
  sparseFit <-  Cyclops::fitCyclopsModel(dataPtr)
  
  modelTrained <- list(model = sparseFit,
                       coefficients = stats::coef(sparseFit),
                       modelType = 'competingRisk',
                       timeAtRisk = data.frame(sum(population$timeAtRisk)))
  class(modelTrained) <- 'plpModel'

  comp <- Sys.time() - start
  varImp <- data.frame(covariateId=names(modelTrained$coefficients)[names(modelTrained$coefficients)!='(Intercept)'], 
                       value=modelTrained$coefficients[names(modelTrained$coefficients)!='(Intercept)'])
  if(sum(abs(varImp$value)>0)==0){
    ParallelLogger::logWarn('No non-zero coefficients')
    varImp <- NULL
  } else {
    #varImp <- varImp[abs(varImp$value)>0,]
    varImp <- merge(as.data.frame(plpData$covariateData$covariateRef), varImp, 
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
                 modelSettings = list(model='competing_risk', modelParameters=NULL),
                 hyperParamSearch = NULL,
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
  attr(result, 'predictionType') <- 'competingRisk'
  return(result)
}



