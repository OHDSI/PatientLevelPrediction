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
  search = "grid")
  {
  
  if(is.null(trainData))
    stop('trainData is NULL')
  if(is.null(trainData$covariateData))
    stop('covariateData is NULL')
  checkIsClass(trainData$covariateData, 'covariateData')
  if(is.null(modelSettings$model))
    stop('No model specified')
  checkIsClass(modelSettings, 'modelSettings')
  
  #=========================================================
  # run through pipeline list and apply:
  #=========================================================

  # add index here or somewhere else?
  #Andromeda::createIndex(trainData$covariateData$covariates, c('rowId'),
  #                       indexName = 'restrict_pop_rowId') # is this needed now?
  
  # Now apply the classifier:
  fun <- modelSettings$fitFunction
  args <- list(
    trainData = trainData,
    param = modelSettings$param,
    search = search
    )
  plpModel <- do.call(fun, args)
  ParallelLogger::logTrace('Returned from classifier function')
  
  # add pre-processing details TODO expand this
  plpModel$metaData$preprocessSettings <- attr(trainData$covariateData, "metaData") 
  
  #plpModel$settings$dataExtraction <- list(cohortId, outcomeId, covariateSettings)
  #plpModel$settings$population <- populationSettings
  #plpModel$settings$featureEngineering <- ...
  #plpModel$settings$preprocess <- ...
  #plpModel$dataInfo$attrition <- ...
  # save all seeds
  
  ParallelLogger::logTrace('Creating prediction function')
  plpModel$predict <- createTransform(plpModel)
  class(plpModel) <- 'plpModel'
  
  return(plpModel)
  
}

#NEED TO UPDATE....
# fucntion for implementing the pre-processing (normalisation and redundant features removal)
applyTidyCovariateData <- function(
  covariateData,
  preprocessSettings
)
{

  if(!FeatureExtraction::isCovariateData(covariateData)){stop("Data not of class CovariateData")}
  
  newCovariateData <- Andromeda::andromeda(covariateRef = covariateData$covariateRef,
                                           analysisRef = covariateData$analysisRef)
  
  maxs <- preprocessSettings$normFactors
  deleteRedundantCovariateIds <- preprocessSettings$deletedRedundantCovariateIds
  deletedInfrequentCovariateIds <- preprocessSettings$deletedInfrequentCovariateIds
  
  # --- added for speed
  deleteCovariateIds <- c(deleteRedundantCovariateIds,deletedInfrequentCovariateIds)
  temp <- covariateData$covariateRef %>% dplyr::collect()
  allCovariateIds <- temp$covariateId
  covariateData$includeCovariates <- data.frame(covariateId = allCovariateIds[!allCovariateIds%in%deleteCovariateIds])
  Andromeda::createIndex(covariateData$includeCovariates, c('covariateId'),
                         indexName = 'includeCovariates_covariateId')
  on.exit(covariateData$includeCovariates <- NULL, add = TRUE)
  # ---

  ParallelLogger::logInfo("Removing infrequent and redundant covariates and normalizing")
  start <- Sys.time()       
  
  if(!is.null(maxs)){
    if('bins'%in%colnames(maxs)){
      covariateData$maxes <- tibble::as_tibble(maxs)  %>% dplyr::rename(covariateId = .data$bins) %>% 
        dplyr::rename(maxValue = .data$maxs)
    } else{
      covariateData$maxes <- maxs #tibble::as_tibble(maxs)  %>% dplyr::rename(covariateId = bins)
    }
    on.exit(covariateData$maxes <- NULL, add = TRUE)
    
    # --- added for speed
    Andromeda::createIndex(covariateData$maxes, c('covariateId'),
                           indexName = 'maxes_covariateId')
    # ---
    
    newCovariateData$covariates <- covariateData$covariates %>%  
      dplyr::inner_join(covariateData$includeCovariates, by='covariateId') %>% # added as join
      dplyr::inner_join(covariateData$maxes, by = 'covariateId') %>%
      dplyr::mutate(value = 1.0*.data$covariateValue/.data$maxValue) %>%
      dplyr::select(- .data$covariateValue) %>%
      dplyr::rename(covariateValue = .data$value)
  } else{
    newCovariateData$covariates <- covariateData$covariates %>% 
      dplyr::inner_join(covariateData$includeCovariates, by='covariateId')
  }
  
  # adding index for restrict to pop
  Andromeda::createIndex(
    newCovariateData$covariates, 
    c('rowId'),
    indexName = 'ncovariates_rowId'
    )
  
  
  class(newCovariateData) <- "CovariateData"
  
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
    if(!is.null(plpModel$metaData$preprocessSettings)){
      plpData$covariateData <- applyTidyCovariateData(plpData$covariateData,plpModel$metaData$preprocessSettings)
    }
    
    if(length(population$rowId)<200000){
      plpData$covariateData <- limitCovariatesToPopulation(plpData$covariateData, 
                                                           population$rowId)
    } else{
      plpData$covariateData <- batchRestrict(plpData$covariateData, 
                                             data.frame(rowId = population$rowId), 
                                             sizeN = 10000000)
    }
    
    
    pred <- do.call(paste0('predict_',attr(plpModel, 'type')), list(plpModel=plpModel,
                                                                    plpData=plpData, 
                                                                    population=population))

    metaData <- list(trainDatabase = strsplit(do.call(paste, list(plpModel$metaData$call$cdmDatabaseSchema)),'\\.')[[1]][1],
                     testDatabase = strsplit(do.call(paste, list(plpData$metaData$call$cdmDatabaseSchema)),'\\.')[[1]][1],
                     studyStartDate = do.call(paste,list(plpModel$metaData$call$studyStartDate)), 
                     studyEndDate = do.call(paste,list(plpModel$metaData$call$studyEndDate)),
                     cohortId = plpModel$cohortId,
                     outcomeId = plpModel$outcomeId,
                     predictionType = attr(plpModel, 'predictionType') #'binary'
    )
    attr(pred, 'metaData') <- metaData
    return(pred)
  }
  return(transform)
}
