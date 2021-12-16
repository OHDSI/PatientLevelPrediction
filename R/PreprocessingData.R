# @file PreprocessingData.R
# Copyright 2021 Observational Health Data Sciences and Informatics
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

#' Create the settings for preprocessing the trainData using \code{ }.
#'
#' @details
#' Returns an object of class \code{preprocessingSettings} that specifies how to preprocess the training data
#'
#' @param minFraction             The minimum fraction of target population who must have a covariate for it to be included in the model training                            
#' @param normalize                    Whether to normalise the covariates before training (Default: TRUE)
#' @param removeRedundancy                 Whether to remove redundant features (Default: TRUE)
#' @return
#' An object of class \code{preprocessingSettings}
#' @export
createPreprocessSettings <- function(
  minFraction = 0.001,
  normalize = TRUE,
  removeRedundancy = TRUE
  ){
  
    checkIsClass(minFraction, c('numeric','integer'))
    checkHigherEqual(minFraction,0)
  
    checkIsClass(normalize, c("logical"))

    checkIsClass(removeRedundancy, c("logical"))
  
    preprocessingSettings <- list(
      minFraction = minFraction,
      normalize = normalize,
      removeRedundancy = removeRedundancy
      )
  
  class(preprocessingSettings) <- "preprocessSettings"
  return(preprocessingSettings)
  
}


#' A function that wraps around FeatureExtraction::tidyCovariateData to normalise the data
#' and remove rare or redundant features
#'
#' @details
#' Returns an object of class \code{covariateData} that has been processed
#'
#' @param covariateData         The covariate part of the training data created by \code{splitData} after being sampled and having 
#'                              any required feature engineering                           
#' @param preprocessSettings    The settings for the preprocessing created by \code{createPreprocessSettings}                    
#' @return
#' The data processed 
preprocessData <- function (covariateData, 
                            preprocessSettings){
  
  metaData <- attr(covariateData, "metaData")
  
  checkIsClass(covariateData, c("CovariateData"))
  checkIsClass(preprocessSettings, c("preprocessSettings"))
  
  ParallelLogger::logDebug(paste0('minFraction: ', preprocessSettings$minFraction))
  ParallelLogger::logDebug(paste0('normalize: ', preprocessSettings$normalize))
  ParallelLogger::logDebug(paste0('removeRedundancy: ', preprocessSettings$removeRedundancy))
  
  preprocessSettings$covariateData <- covariateData
  covariateData <- do.call(FeatureExtraction::tidyCovariateData, preprocessSettings)
  
  #update covariateRed
  removed <- unique(
    attr(covariateData, "metaData")$deletedInfrequentCovariateIds,
    attr(covariateData, "metaData")$deletedRedundantCovariateIds
    )
  covariateData$covariateRef <- covariateData$covariateRef %>% 
    dplyr::filter(!.data$covariateId  %in% removed)
  
  metaData$tidyCovariateDataSettings <- attr(covariateData, "metaData")
  attr(covariateData, "metaData") <- metaData
  
  return(covariateData)
}
