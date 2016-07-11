# @file Fit.R
#
# Copyright 2016 Observational Health Data Sciences and Informatics
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

#' fitPlp
#'
#' @description
#' Train various models using a default parameter gird search or user specified parameters
#'
#' @details
#' The user can define the machine learning model to train (regularised logistic regression, random forest,
#' gradient boosting machine, neural network and )
#' 
#' @param population                       The population created using createStudyPopulation() who will have their risks predicted
#' @param data                             An object of type \code{plpData} - the patient level prediction
#'                                         data extracted from the CDM.
#' @param index                            A data frame containing rowId: a vector of rowids and index: a vector of doubles the same length as the 
#'                                         rowIds. If used, only the rowIds with a negative index value are used to calculate the prediction.  
#' @param modelSettings                    An object of class \code{modelSettings} created using one of the function:
#'                                         \itemize{
#'                                         \item{logisticRegressionModel()}{ A lasso logistic regression model}
#'                                         \item{GBMclassifier()}{ A gradient boosting machine}
#'                                         \item{RFclassifier()}{ A random forest model}
#'                                         \item{GLMclassifier ()}{ A generalised linear model}
#'                                         \item{KNNclassifier()}{ A KNN model}
#'                                         }
#' @return
#' An object of class \code{plpModel} containing:
#' 
#' \item{model}{The trained prediction model}
#' \item{modelLoc}{The path to where the model is saved (if saved)}
#' \item{trainAuc}{The AUC obtained on the training set}
#' \item{trainCalibration}{The calibration obtained on the training set}
#' \item{modelSettings}{A list specifiying the model, preprocessing, outcomeId and cohortId}
#' \item{metaData}{The model meta data}
#' \item{trainingTime}{The time taken to train the classifier}
#'
#'

#' @export
fitPlp <- function(population, data, index,  modelSettings,#featureSettings, 
                   quiet,
                   cohortId, outcomeId){
  silent <- quiet
  if('ffdf'%in%class(data$covariates)){
    plpData <- list(outcomes =data$outcomes,
                    cohorts = data$cohorts,
                    covariates =ff::clone(data$covariates),
                    covariateRef=ff::clone(data$covariateRef),
                    metaData=data$metaData
    )} else{
      plpData <- data
    }
  
  
  #=========================================================
  # run through pipeline list and apply:
  #=========================================================
  # Now apply the classifier:
  fun <- modelSettings$model
  args <- list(plpData =plpData,param =modelSettings$param, index=index,
               population=population, quiet=quiet, cohortId=cohortId, outcomeId=outcomeId)
  plpModel <- do.call(fun, args)
  
  plpModel$transform <- createTransform(plpModel)
  
  return(plpModel)
  
}


# create transformation function
createTransform <- function(plpModel){
  
  transform <- function(plpData=NULL, population=NULL, silent=F){
    #check model fitting makes sense:
    if(ifelse(!is.null(attr(population, "metaData")$cohortId),attr(population, "metaData")$cohortId,-1)!=plpModel$cohortId)
      warning('cohortId of new data does not match training data')
    if(ifelse(!is.null(attr(population, "metaData")$outcomeId),attr(population, "metaData")$outcomeId,-1)!=plpModel$outcomeId)
      warning('outcomeId of new data does not match training data or does not exist')
    
    #TODO: recalibrate
    
    if(!silent) writeLines('Applying model to calculate predictions...')
    pred <- do.call(paste0('predict.',attr(plpModel, 'type')), list(plpModel=plpModel,
                                                                    plpData=plpData, 
                                                                    population=population, 
                                                                    silent=silent))
    metaData <- list(trainDatabase = strsplit(do.call(paste, list(plpModel$metaData$call$cdmDatabaseSchema)),'\\.')[[1]][1],
                     testDatabase = strsplit(do.call(paste, list(plpData$metaData$call$cdmDatabaseSchema)),'\\.')[[1]][1],
                     studyStartDate = do.call(paste,list(plpModel$metaData$call$studyStartDate)), 
                     studyEndDate = do.call(paste,list(plpModel$metaData$call$studyEndDate)),
                     cohortId = plpModel$cohortId,
                     outcomeId = plpModel$outcomeId,
                     predictionType ='binary'
    )
    attr(pred, 'metaData') <- metaData
    return(pred)
  }
  return(transform)
}