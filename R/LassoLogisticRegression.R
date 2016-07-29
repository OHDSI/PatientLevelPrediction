# @file lassoLogisticRegression.R
#
# Copyright 2016 Observational Health Data Sciences and Informatics
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
#' @param variance   a single value or vector of values to be used to train multiple models and the model with the
#'                   best performance on the cross validation set is choosen
#'
#' @examples
#' model.lr <- lassoLogisticRegression.set()
#' @export
lassoLogisticRegression.set<- function(variance=0.01){
  result <- list(model='fitLassoLogisticRegression', param=list(val=variance), name="Lasso Logistic Regression")
  class(result) <- 'modelSettings' 
  attr(result, 'libSVM') <- F
  
  return(result)
}

fitLassoLogisticRegression<- function(population, plpData, param, search='adaptive', 
                     outcomeId, cohortId, ...){
  
  # check plpData is coo format:
  if(!'ffdf'%in%class(plpData$covariates) || class(plpData)=='plpData.libsvm'){
    flog.error('Lasso Logistic regression requires plpData in coo format')
    stop()
  }

  metaData <- attr(population, 'metaData')
  if(!is.null(population$indexes))
    population <- population[population$indexes>0,]
  attr(population, 'metaData') <- metaData
  #TODO - how to incorporate indexes?
  val <- 0.003
  if(!is.null(param$val )) val <- param$val
  start <- Sys.time()
  modelTrained <- fitGLMModel(population,
                                     plpData = plpData,
                                     modelType = "logistic",
                                     prior = createPrior("laplace",exclude = c(0),useCrossValidation = TRUE),
                                     control = createControl(noiseLevel = "quiet", cvType = "auto",
                                                             startingVariance = val,
                                                             tolerance  = 2e-06,
                                                             cvRepetitions = 1, fold=ifelse(!is.null(index$index),max(index$index),1),
                                                             selectorType = "byPid",
                                                             threads=-1,
                                                             maxIterations = 3000))
  
  # TODO get optimal lambda value
  
  comp <- Sys.time() - start
  varImp <- data.frame(covariateId=names(modelTrained$coefficients)[names(modelTrained$coefficients)!='(Intercept)'], 
                       value=modelTrained$coefficients[names(modelTrained$coefficients)!='(Intercept)'])
  if(sum(abs(varImp$value)>0)==0){
    flog.warn('No non-zero coefficients')
    varImp <- NULL
  } else {
    #varImp <- varImp[abs(varImp$value)>0,]
    varImp <- merge(ff::as.ram(plpData$covariateRef), varImp, 
                    by='covariateId',all=T)
    varImp$value[is.na(varImp$value)] <- 0
    varImp <- varImp[order(-abs(varImp$value)),]
  }
  
  result <- list(model = modelTrained,
                 modelSettings = list(model='lr_lasso', modelParameters=param), #todo get lambda as param
                 trainCVAuc = NULL,
                 metaData = plpData$metaData,
                 populationSettings = attr(population, 'metaData'),
                 outcomeId=outcomeId,# can use populationSettings$outcomeId?
                 cohortId=cohortId,
                 varImp = varImp,
                 trainingTime=comp
  )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'plp'
  attr(result, 'predictionType') <- 'binary'
  return(result)
}