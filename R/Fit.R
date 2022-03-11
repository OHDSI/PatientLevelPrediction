# @file Fit.R
#
# Copyright 2021 Observational Health Data Sciences and Informatics
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
#' @param trainData                        An object of type \code{TrainData} created using \code{splitData}
#'                                         data extracted from the CDM.
#' @param modelSettings                    An object of class \code{modelSettings} created using one of the function:
#'                                         \itemize{
#'                                         \item{logisticRegressionModel()}{ A lasso logistic regression model}
#'                                         \item{GBMclassifier()}{ A gradient boosting machine}
#'                                         \item{RFclassifier()}{ A random forest model}
#'                                         \item{GLMclassifier ()}{ A generalised linear model}
#'                                         \item{KNNclassifier()}{ A KNN model}
#'                                         }
#' @param search                           The search strategy for the hyper-parameter selection (currently not used)                                        
#' @param analysisId                       The id of the analysis
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
fitPlp <- function(
  trainData,   
  modelSettings,
  search = "grid",
  analysisId
  )
  {
  
  if(is.null(trainData))
    stop('trainData is NULL')
  if(is.null(trainData$covariateData))
    stop('covariateData is NULL')
  checkIsClass(trainData$covariateData, 'CovariateData')
  if(is.null(modelSettings$fitFunction))
    stop('No model specified')
  checkIsClass(modelSettings, 'modelSettings')
  
  #=========================================================
  # run through pipeline list and apply:
  #=========================================================

  # Now apply the classifier:
  fun <- eval(parse(text = modelSettings$fitFunction))
  args <- list(
    trainData = trainData,
    param = modelSettings$param,
    search = search,
    analysisId = analysisId
    )
  plpModel <- do.call(fun, args)
  ParallelLogger::logTrace('Returned from classifier function')
  
  class(plpModel) <- 'plpModel'
  
  return(plpModel)
  
}
