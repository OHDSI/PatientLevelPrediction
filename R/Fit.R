# @file Fit.R
#
# Copyright 2020 Observational Health Data Sciences and Informatics
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
#' @param modelSettings                    An object of class \code{modelSettings} created using one of the function:
#'                                         \itemize{
#'                                         \item{logisticRegressionModel()}{ A lasso logistic regression model}
#'                                         \item{GBMclassifier()}{ A gradient boosting machine}
#'                                         \item{RFclassifier()}{ A random forest model}
#'                                         \item{GLMclassifier ()}{ A generalised linear model}
#'                                         \item{KNNclassifier()}{ A KNN model}
#'                                         }
#' @param cohortId                         Id of study cohort
#' @param outcomeId                        Id of outcome cohort
#' @param minCovariateFraction             The minimum fraction of the target popualtion who have a variable for it to be included in the model training 
#' @param normalizeData                    Whether to normalise the data before model fitting
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
fitPlp <- function(population, data,   modelSettings,#featureSettings, 
                   cohortId, outcomeId, minCovariateFraction=0.001, normalizeData=T){
  
  if(is.null(population))
    stop('Population is NULL')
  if(is.null(data))
    stop('plpData is NULL')
  if(is.null(modelSettings$model))
    stop('No model specified')
  
  plpData <- data
  #=========================================================
  # run through pipeline list and apply:
  #=========================================================
  
  # normalise the data:
  removeRedundancy <- ifelse("timeId" %in%colnames(plpData$covariateData$covariates), F, T)
  plpData$covariateData <- tryCatch({
    suppressWarnings(FeatureExtraction::tidyCovariateData(covariateData=data$covariateData, 
                                                          minFraction = minCovariateFraction,
                                                          normalize = normalizeData,
                                                          removeRedundancy = removeRedundancy))
  })
  
  # get the pre-processing settings
  ##preprocessSettings <- attr(newCovariateData, "metaData")  
  #   normFactors, deletedRedundantCovariateIds, deletedInfrequentCovariateIds 
  
  # Now apply the classifier:
  fun <- modelSettings$model
  args <- list(plpData =plpData,param =modelSettings$param, 
               population=population, cohortId=cohortId, outcomeId=outcomeId)
  plpModel <- do.call(fun, args)
  ParallelLogger::logTrace('Returned from classifier function')
  # add pre-processing details
  plpModel$metaData$preprocessSettings <- attr(plpData$covariateData, "metaData") 
  
  ParallelLogger::logTrace('Creating prediction function')
  plpModel$predict <- createTransform(plpModel)
  ParallelLogger::logTrace('Adding index')
  plpModel$index <- population$indexes  ##?- dont think we need this, just the seed instead
  class(plpModel) <- 'plpModel'
  
  return(plpModel)
  
}

#NEED TO UPDATE....
# fucntion for implementing the pre-processing (normalisation and redundant features removal)
applyTidyCovariateData <- function(covariateData,preprocessSettings){

  if (!FeatureExtraction::isCovariateData(covariateData))
    stop("Data not of class CovariateData")
  
  newCovariateData <- Andromeda::andromeda(covariateRef = covariateData$covariateRef,
                                           analysisRef = covariateData$analysisRef)
  
  maxs <- preprocessSettings$normFactors
  deleteRedundantCovariateIds <- preprocessSettings$deletedRedundantCovariateIds
  deletedInfrequentCovariateIds <- preprocessSettings$deletedInfrequentCovariateIds
 
  writeLines("Removing infrequent and redundant covariates and normalizing")
  start <- Sys.time()       
  
  if('bins'%in%colnames(maxs)){
    covariateData$maxes <- tibble::as_tibble(maxs)  %>% dplyr::rename(covariateId = bins) %>% 
      dplyr::rename(maxValue = maxs)
  } else{
  covariateData$maxes <- maxs #tibble::as_tibble(maxs)  %>% dplyr::rename(covariateId = bins)
  }
  on.exit(covariateData$maxes <- NULL, add = TRUE)
  
  newCovariateData$covariates <- covariateData$covariates %>%  
    dplyr::filter(!covariateId %in%deletedInfrequentCovariateIds) %>%
    dplyr::filter(!covariateId %in%deleteRedundantCovariateIds) %>%
    dplyr::inner_join(covariateData$maxes, by = 'covariateId') %>%
    dplyr::mutate(value = 1.0*covariateValue/maxValue) %>%
    dplyr::select(-covariateValue) %>%
    dplyr::rename(covariateValue = value)
  
  
  delta <- Sys.time() - start
  writeLines(paste("Removing infrequent and redundant covariates covariates and normalizing took", signif(delta, 3), attr(delta, "units")))
  
  # return processed data
  return(newCovariateData)
}

# create transformation function
createTransform <- function(plpModel){
  #=============== edited this in last run
  # remove index to save space 
  plpModel$index <- NULL
  ##plpModel$varImp <- NULL
  # remove connection details for privacy
  plpModel$metaData$call$connectionDetails <- NULL
  #=====================
  
  transform <- function(plpData=NULL, population=NULL){
    #check model fitting makes sense:
    if(ifelse(!is.null(attr(population, "metaData")$cohortId),attr(population, "metaData")$cohortId,-1)!=plpModel$cohortId)
      warning('cohortId of new data does not match training data')
    if(ifelse(!is.null(attr(population, "metaData")$outcomeId),attr(population, "metaData")$outcomeId,-1)!=plpModel$outcomeId)
      warning('outcomeId of new data does not match training data or does not exist')
    
    # apply normalsation to new data
    plpData$covariateData <- limitCovariatesToPopulation(plpData$covariateData, population$rowId)
    if(!is.null(plpModel$metaData$preprocessSettings)){
      plpData$covariateData <- applyTidyCovariateData(plpData$covariateData,plpModel$metaData$preprocessSettings)
    }
    pred <- do.call(paste0('predict.',attr(plpModel, 'type')), list(plpModel=plpModel,
                                                                    plpData=plpData, 
                                                                    population=population))

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
