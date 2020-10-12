# @file lassoLogisticRegression.R
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

#' Create setting for lasso logistic regression
#'
#' @param variance   a single value used as the starting value for the automatic lambda search
#' @param seed       An option to add a seed when training the model
#' @param includeCovariateIds a set of covariate IDS to limit the analysis to
#' @param noShrinkage a set of covariates whcih are to be forced to be included in the final model. default is the intercept 
#' @param threads    An option to set number of threads when training model
#' @param useCrossValidation  Set this to FALSE if you want to train a LR with a preset varience
#' 
#' @examples
#' model.lr <- setLassoLogisticRegression()
#' @export
setLassoLogisticRegression<- function(variance=0.01, seed=NULL, includeCovariateIds = c(), noShrinkage = c(0), threads = -1, useCrossValidation = TRUE){

  if(!class(seed)%in%c('numeric','NULL','integer'))
    stop('Invalid seed')
  if(!class(threads)%in%c('numeric','NULL','integer'))
    stop('Invalid threads')
  if(!class(variance) %in% c("numeric", "integer"))
    stop('Variance must be numeric')
  if(variance<0)
    stop('Variance must be >= 0')
  
  # set seed
  if(is.null(seed[1])){
    seed <- as.integer(sample(100000000,1))
  }
  
  result <- list(model='fitLassoLogisticRegression', param=list(variance=variance, seed=seed[1], includeCovariateIds = includeCovariateIds, noShrinkage = noShrinkage, threads = threads[1], useCrossValidation=useCrossValidation[1]), name="Lasso Logistic Regression")

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
  if (!FeatureExtraction::isCovariateData(plpData$covariateData)){
    stop("Needs correct covariateData")
  }

  metaData <- attr(population, 'metaData')
  if(!is.null(population$indexes))
    population <- population[population$indexes>0,]
  attr(population, 'metaData') <- metaData
  #TODO - how to incorporate indexes?
  variance <- 0.003
  if(!is.null(param$variance )) variance <- param$variance
  includeCovariateIds <- param$includeCovariateIds
  start <- Sys.time()
  noShrinkage <- param$noShrinkage
  modelTrained <- fitGLMModel(population,
                                     plpData = plpData, 
                                     includeCovariateIds = includeCovariateIds,
                                     modelType = "logistic",
                                     prior = createPrior("laplace",exclude = c(0),useCrossValidation = param$useCrossValidation, variance = variance),
                                     control = createControl(noiseLevel = ifelse(trace,"quiet","silent"), cvType = "auto",
                                                             startingVariance = variance,
                                                             tolerance  = 2e-07,
                                                             cvRepetitions = 1, fold=ifelse(!is.null(population$indexes),max(population$indexes),1),
                                                             selectorType = "byPid",
                                                             threads= param$threads,
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
    varImp <- merge(as.data.frame(plpData$covariateData$covariateRef), varImp, 
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
  
  # get cv AUC
  cvPrediction  <- do.call(rbind, lapply(modelTrained$cv, function(x){x$predCV}))
  cvPerFold <-  unlist(lapply(modelTrained$cv, function(x){x$out_sample_auc}))
  if(length(cvPerFold)>0){
    names(cvPerFold) <- paste0('fold_auc', 1:length(cvPerFold))
  }
  
  result <- list(model = modelTrained,
                 modelSettings = list(model='lr_lasso', modelParameters=param), #todo get lambda as param
                 hyperParamSearch = c(priorVariance=modelTrained$priorVariance, 
                                      seed=ifelse(is.null(param$seed), 'NULL', param$seed  ), 
                                      log_likelihood = modelTrained$log_likelihood,
                                      cvPerFold,
                                      auc = tryCatch({aucWithoutCi(cvPrediction$value, cvPrediction$y)}, error = function(e) return(NULL))),
                 trainCVAuc = list(value = cvPerFold, prediction = cvPrediction),
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


