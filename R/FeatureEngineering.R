# @file FeatureEngineering.R
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



#' Create the settings for defining any feature engineering that will be done
#'
#' @details
#' Returns an object of class \code{featureEngineeringSettings} that specifies the sampling function that will be called and the settings
#'
#' @param type              (character) Choice of:  \itemize{
#'                                         \item{'none'}{ No feature engineering - this is the default }
#'                                         } 
#'
#' @return
#' An object of class \code{featureEngineeringSettings}
#' @export
createFeatureEngineeringSettings <- function(type = 'none'){
  
  featureEngineeringSettings <- list()
  
  if(type == 'none'){
    attr(featureEngineeringSettings, "fun") <- "sameData"
  }
  class(featureEngineeringSettings) <- "featureEngineeringSettings"
  return(featureEngineeringSettings)
  
}


#' Create the settings for defining any feature selection that will be done
#'
#' @details
#' Returns an object of class \code{featureEngineeringSettings} that specifies the sampling function that will be called and the settings
#'
#' @param k              This function returns the K features most associated (univariately) to the outcome
#'
#' @return
#' An object of class \code{featureEngineeringSettings}
#' @export
createUnivariateFeatureSelection <- function(k = 100){
  
  checkIsClass(k, c('numeric','integer'))
  checkHigherEqual(k, 0)
  
  featureEngineeringSettings <- list(k = k) 
  
  attr(featureEngineeringSettings, "fun") <- "univariateFeatureSelection"
  class(featureEngineeringSettings) <- "featureEngineeringSettings"
  
  return(featureEngineeringSettings)
  
}

#' Create the settings for random foreat based feature selection
#'
#' @details
#' Returns an object of class \code{featureEngineeringSettings} that specifies the sampling function that will be called and the settings
#'
#' @param ntrees              number of tree in forest
#' @param maxDepth            MAx depth of each tree
#'
#' @return
#' An object of class \code{featureEngineeringSettings}
#' @export
createRandomForestFeatureSelection <- function(ntrees = 2000, maxDepth = 17){
  
  checkIsClass(ntrees, c('numeric','integer'))
  checkIsClass(maxDepth, c('numeric','integer'))
  checkHigher(ntrees, 0)
  checkHigher(maxDepth, 0)
  
  featureEngineeringSettings <- list(
    ntrees = ntrees,
    max_depth = maxDepth
    )
  
  attr(featureEngineeringSettings, "fun") <- "randomForestFeatureSelection"
  class(featureEngineeringSettings) <- "featureEngineeringSettings"
  
  return(featureEngineeringSettings)
}

univariateFeatureSelection <- function(
  trainData, 
  featureEngineeringSettings,
  covariateIdsInclude = NULL){
  
  if(is.null(covariateIdsInclude)){
    #convert data into matrix:
    mappedData <- toSparseM(trainData, trainData$labels)
    
    matrixData <- mappedData$dataMatrix
    labels <- mappedData$labels
    covariateMap <- mappedData$covariateMap
    
    X <- reticulate::r_to_py(matrixData)
    y <- reticulate::r_to_py(labels[,'outcomeCount'])
    
    np <- reticulate::import('numpy')
    os <- reticulate::import('os')
    sys <- reticulate::import('sys')
    math <- reticulate::import('math')
    scipy <- reticulate::import('scipy')
    
    sklearn <- reticulate::import('sklearn')
    
    SelectKBest <- sklearn$feature_selection$SelectKBest
    chi2 <- sklearn$feature_selection$chi2
    
    kbest <- SelectKBest(chi2, k = featureEngineeringSettings$k)$fit(X, y)
    kbest$scores_ <- np$nan_to_num(kbest$scores_)
    threshold <- -np$sort(-kbest$scores_)[(featureEngineeringSettings$k-1)]
    
    inc <- kbest$scores_ >= threshold
    
    covariateIdsInclude <- covariateMap[inc,]$covariateId
  }
  
  trainData$covariateData$covariates <- trainData$covariateData$covariates %>% 
    dplyr::filter(.data$covariateId %in% covariateIdsInclude)
  
  trainData$covariateData$covariateRef <- trainData$covariateData$covariateRef %>% 
    dplyr::filter(.data$covariateId %in% covariateIdsInclude)
  
  featureEngeering <- list(
    funct = 'univariateFeatureSelection',
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings,
      covariateIdsInclude = covariateIdsInclude
    )
  )
  
  attr(trainData, 'metaData')$featureEngineering = listAppend(
    attr(trainData, 'metaData')$featureEngineering,
    featureEngeering
  )
  
  return(trainData)
  
}


randomForestFeatureSelection <- function(
  trainData, 
  featureEngineeringSettings,
  covariateIdsInclude = NULL
){
  
  if(is.null(covariateIdsInclude)){
    #convert data into matrix:
    mappedData <- toSparseM(trainData)
    
    matrixData <- mappedData$dataMatrix
    labels <- mappedData$labels
    covariateMap <- mappedData$covariateMap
    
    X <- reticulate::r_to_py(matrixData)
    y <- reticulate::r_to_py(matrix(labels$outcomeCount, ncol=1))
    
    np <- reticulate::import('numpy')
    os <- reticulate::import('os')
    sys <- reticulate::import('sys')
    math <- reticulate::import('math')
    scipy <- reticulate::import('scipy')
    
    sklearn <- reticulate::import('sklearn')
    
    ntrees = featureEngineeringSettings$ntrees #2000
    max_depth = featureEngineeringSettings$max_depth #17
    
    rf = sklearn$ensemble$RandomForestClassifier(
      max_features = 'auto', 
      n_estimators = as.integer(ntrees),
      max_depth = as.integer(max_depth),
      min_samples_split = as.integer(2), 
      random_state = as.integer(10), # make this an imput for consistency
      n_jobs = as.integer(-1), 
      bootstrap = F
    )
    
    rf = rf$fit(X, y$ravel())
    
    inc <- rf$feature_importances_ > 0 
    
    covariateIdsInclude <- covariateMap$covariateId[inc]
  } 
  
  trainData$covariateData$covariates <- trainData$covariateData$covariates %>% 
    dplyr::filter(.data$covariateId %in% covariateIdsInclude)
  
  trainData$covariateData$covariateRef <- trainData$covariateData$covariateRef %>% 
    dplyr::filter(.data$covariateId %in% covariateIdsInclude)
  
  
  featureEngeering <- list(
    funct = 'randomForestFeatureSelection',
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings,
      covariateIdsInclude = covariateIdsInclude
    )
  )
  
  attr(trainData, 'metaData')$featureEngineering = listAppend(
    attr(trainData, 'metaData')$featureEngineering,
    featureEngeering
  )
  
  return(trainData)
  
}



featureEngineer <- function(data, featureEngineeringSettings){
  
  ParallelLogger::logInfo('Starting Feature Engineering')
  
  # if a single setting, make it a list
  if(class(featureEngineeringSettings) == 'featureEngineeringSettings'){
    featureEngineeringSettings <- list(featureEngineeringSettings)
  }
  
  for(featureEngineeringSetting in featureEngineeringSettings){
    fun <- attr(featureEngineeringSetting, "fun")
    args <- list(trainData = data,
                 featureEngineeringSettings = featureEngineeringSetting)
    ParallelLogger::logInfo(paste0('Applying ',fun))
    data <- do.call(eval(parse(text = fun)), args)
  }
  
  ParallelLogger::logInfo('Done Feature Engineering')
  
  return(data)
  
}
