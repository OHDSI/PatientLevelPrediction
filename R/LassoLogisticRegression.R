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
#' model.lr <- logisticRegressionModel()
#' @export
lassoLogisticRegression.set<- function(variance=0.01){
  result <- list(model='lassoLogisticRegression.fit', param=list(val=variance))
  class(result) <- 'modelSettings' 
  attr(result, 'libSVM') <- F
  
  return(result)
}

lassoLogisticRegression.fit<- function(population, plpData, param,index, search='adaptive', quiet=F,
                     outcomeId, cohortId, ...){
  
  # check plpData is coo format:
  if(!'ffdf'%in%class(plpData$covariates) || class(plpData)=='plpData.libsvm')
    stop('Lasso Logistic regression requires plpData in coo format')
  
  metaData <- attr(population, 'metaData')
  if(!is.null(population$indexes))
    population <- population[population$indexes>0,]
  attr(population, 'metaData') <- metaData
  #TODO - how to incorporate indexes?
  val <- 0.003
  if(!is.null(param$val )) val <- param$val
  if(!quiet)
    writeLines(paste0('Training lasso logistic regression model on train set containing ', nrow(population), ' people with ',sum(population$outcomeCount>0), ' outcomes'))
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
                                                             maxIterations = 3000),
                                     silent=quiet)
  
  # TODO get optimal lambda value
  
  comp <- Sys.time() - start
  if(!quiet)
    writeLines(paste0('Model Logistic Regression with Lasso regularisation trained - took:',  format(comp, digits=3)))
  
  varImp <- data.frame(covariateId=names(modelTrained$coefficients)[names(modelTrained$coefficients)!='(Intercept)'], 
                       value=modelTrained$coefficients[names(modelTrained$coefficients)!='(Intercept)'])
  if(sum(abs(varImp$value)>0)==0){
    warning('No non-zero coefficients')
    varImp <- NULL
  } else {
    varImp <- varImp[abs(varImp$value)>0,]
    varImp <- merge(varImp, ff::as.ram(plpData$covariateRef))
    varImp<-varImp[order(-abs(varImp$value)),]
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