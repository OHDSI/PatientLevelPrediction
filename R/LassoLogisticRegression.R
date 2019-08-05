# @file lassoLogisticRegression.R
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
#' @param variance   a single value used as the starting value for the automatic lambda search
#' @param seed       An option to add a seed when training the model
#'
#' @examples
#' model.lr <- setLassoLogisticRegression()
#' @export
setLassoLogisticRegression<- function(variance=0.01, seed=NULL){
  if(!class(seed)%in%c('numeric','NULL','integer'))
    stop('Invalid seed')
  if(!class(variance) %in% c("numeric", "integer"))
    stop('Variance must be numeric')
  if(variance<0)
    stop('Variance must be >= 0')
  
  # set seed
  if(is.null(seed[1])){
    seed <- as.integer(sample(100000000,1))
  }
  
  result <- list(model='fitLassoLogisticRegression', param=list(variance=variance, seed=seed[1]), name="Lasso Logistic Regression")
  class(result) <- 'modelSettings' 
  
  return(result)
}

fitLassoLogisticRegression<- function(population, plpData, param, search='adaptive', 
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
    ParallelLogger::logError('Lasso Logistic regression requires plpData in coo format')
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
                                     modelType = "logistic",
                                     prior = createPrior("laplace",exclude = c(0),useCrossValidation = TRUE),
                                     control = createControl(noiseLevel = ifelse(trace,"quiet","silent"), cvType = "auto",
                                                             startingVariance = variance,
                                                             tolerance  = 2e-07,
                                                             cvRepetitions = 1, fold=ifelse(!is.null(population$indexes),max(population$indexes),1),
                                                             selectorType = "byPid",
                                                             threads=-1,
                                                             maxIterations = 3000,
                                                             seed=param$seed))
  
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
                 modelSettings = list(model='lr_lasso', modelParameters=param), #todo get lambda as param
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



# Code to do variable importance by looking at AUC decrease when the variable is removed from model
inverseLog <- function(x){
  return(-log(1/x-1))
}
revisedAUC <- function(i, coefficients, prediction, mat){
  if(names(coefficients)[i]=='(Intercept)'){
    return(AUC::auc(AUC::roc(prediction$value, factor(prediction$outcomeCount))))
  }
  if(coefficients[i]==0){
    return(AUC::auc(AUC::roc(prediction$value, factor(prediction$outcomeCount))))
  }
  ind <- mat$data[prediction$rowId,mat$map$newIds[mat$map$oldIds==as.double(names(coefficients)[i])]]
  revisedPred <- inverseLog(prediction$value)-coefficients[i]*ind
  
  auc <- tryCatch({AUC::auc(AUC::roc(revisedPred, factor(prediction$outcomeCount)))}, 
                  error = function(e){return(-1)}, warning = function(w){return(-1)})
  return(auc)
}

variableImportanceLR <- function(coefficients, prediction, plpData,preprocessSettings){
  plpData <- applyTidyCovariateData(plpData,preprocessSettings)
  mat <- toSparseM(plpData, prediction)
  auc <- AUC::auc(AUC::roc(prediction$value, factor(prediction$outcomeCount)))
  
  return(auc-sapply(1:length(coefficients), function(i) revisedAUC(i,coefficients, prediction, mat)))
}
