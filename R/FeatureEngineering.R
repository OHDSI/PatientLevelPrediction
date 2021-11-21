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
#' @param K              This function returns the K features most associated (univariately) to the outcome
#'
#' @return
#' An object of class \code{featureEngineeringSettings}
#' @export
createUnivariateFeatureSelection <- function(K = 100){
  
  featureEngineeringSettings <- list(K = K)
  
  attr(featureEngineeringSettings, "fun") <- "univariateFeatureSelection"
  class(featureEngineeringSettings) <- "featureEngineeringSettings"
  
  return(featureEngineeringSettings)
  
}

univariateFeatureSelection <- function(trainData, featureEngineeringSettings){
  
  #convert data into matrix:
  mappedData <- createPythonData(trainData)
  
  matrixData <- mappedData$pythonMatrixData
  labels <- mappedData$pythonLabels
  covariateMap <- mappedData$covariateMap
  
  X <- Reticulate::r_to_py(matrixData)
  y <- Reticulate::r_to_py(labels[,'outcomeCount'])
  
  np <- reticulate::import('numpy')
  os <- reticulate::import('os')
  sys <- reticulate::import('sys')
  math <- reticulate::import('math')
  scipy <- reticulate::import('scipy')
  
  sklearn <- reticulate::import('sklearn')
  
  SelectKBest <- sklearn$feature_selection$SelectKBest
  chi2 <- sklearn$feature_selection$chi2
  
  kbest = SelectKBest(chi2, k= featureEngineeringSettings$K)$fit(X, y)
  kbest$scores_ = np$nan_to_num(kbest$scores_)
  threshold = -np$sort(-kbest$scores_)[featureEngineeringSettings$K-1]

  inc <- reticulate::py_to_r(kbest$scores_ >= threshold)
  
  covariateIdsInclude <- covariateMap$oldCovariateId[]
  
  trainData$covariateData$covariates <- trainData$covariateData$covariates %>% 
    dplyr::filter(.data$covariateId %in% covariateIdsInclude)
  
  trainData$covariateData$covariateRef <- trainData$covariateData$covariateRef %>% 
    dplyr::filter(.data$covariateId %in% covariateIdsInclude)
  
  trainData$metaData$includeCovariateIds = c(covariateIdsInclude)
  trainData$metaData$removeCovariateIds = c(trainData$metaData$removeCovariateIds,covariateIdsRemove)
  
  return(trainData)
  
}


randomForestFeatureSelection <- function(trainData, featureEngineeringSettings){
  
  #convert data into matrix:
  mappedData <- createPythonData(trainData)
  
  matrixData <- mappedData$pythonMatrixData
  labels <- mappedData$pythonLabels
  covariateMap <- mappedData$covariateMap
  
  X <- Reticulate::r_to_py(matrixData)
  y <- Reticulate::r_to_py(labels[,'outcomeCount'])
  
  np <- reticulate::import('numpy')
  os <- reticulate::import('os')
  sys <- reticulate::import('sys')
  math <- reticulate::import('math')
  scipy <- reticulate::import('scipy')
  
  sklearn <- reticulate::import('sklearn')
  
  ntrees = featureEngineeringSettings$ntrees #2000
  max_depth = featureEngineeringSettings$maxDepth #17

  mtry = int(np$round(np$sqrt(X$shape[1])))
  
  rf = RandomForestClassifier(
    max_features = mtry, 
    n_estimators = ntrees,
    max_depth = max_depth,
    min_samples_split = 2, 
    random_state = 0, 
    n_jobs = -1, 
    bootstrap = False
    )
  
  rf = rf$fit(X, Y)

  inc <- reticualte::py_to_r(rf$feature_importances_ > 0 )
  
  covariateIdsInclude <- covariateMap$oldCovariateId[]
  
  trainData$covariateData$covariates <- trainData$covariateData$covariates %>% 
    dplyr::filter(.data$covariateId %in% covariateIdsInclude)
  
  trainData$covariateData$covariateRef <- trainData$covariateData$covariateRef %>% 
    dplyr::filter(.data$covariateId %in% covariateIdsInclude)
  
  trainData$metaData$includeCovariateIds = c(covariateIdsInclude)
  trainData$metaData$removeCovariateIds = c(trainData$metaData$removeCovariateIds,covariateIdsRemove)
  
  return(trainData)
  
}



featureEngineer <- function(data, featureEngineeringSettings){
  
  ParallelLogger::logInfo('Starting Feature Engineering')
  
  metaData <- attr(data, "metaData")
  
  # if a single setting, make it a list
  if(class(featureEngineeringSettings) == 'featureEngineeringSettings'){
    featureEngineeringSettings <- list(featureEngineeringSettings)
  }
  
  for(featureEngineeringSetting in featureEngineeringSettings){
    fun <- attr(featureEngineeringSetting, "fun")
    args <- list(data = data,
                 featureEngineeringSettings = featureEngineeringSetting)
    ParallelLogger::logInfo(paste0('Applying ',fun))
    data <- do.call(eval(parse(text = fun)), args)
  }
  
  ParallelLogger::logInfo('Done Feature Engineering')
  
  metaData$featureEngineering <- list(
    settings = featureEngineeringSettings,
    application = attr(data, "metaData")$application
    )
  
  attr(data, "metaData") <- metaData
  return(data)
  
}