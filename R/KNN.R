# @file knn.R
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

#' Create setting for knn model
#'
#' @param k         The number of neighbors to consider 
#' @param indexFolder The directory where the results and intermediate steps are output
#' @param threads   The number of threads to use when applying big knn
#'
#' @examples
#' \dontrun{
#' model.knn <- setKNN(k=10000)
#' }
#' @export
setKNN <- function(k=1000, indexFolder=file.path(getwd(),'knn'), threads = 1  ){
  ensure_installed("BigKnn")
  
  checkIsClass(indexFolder, c('character'))
  
  checkIsClass(k, c('numeric','integer'))
  checkHigher(k, 0)
  
  if(length(k)>1){
    ParallelLogger::logWarning('k can only be a single value - using first value only')
    k <- k[1]
  }
  
  
  param <- list(
    k = k,
    indexFolder = indexFolder,
    threads = threads
    )
  
  attr(param, 'settings') <- list(
    modelName = 'K Nearest Neighbors'
  )
  
  result <- list(
    fitFunction = "fitKNN",
    param = param
  )

  class(result) <- 'modelSettings' 
  
  return(result)
}

fitKNN <- function(trainData, param, search = 'none', analysisId ){

  if (!FeatureExtraction::isCovariateData(trainData$covariateData)){
    stop("Needs correct covariateData")
  }
  
  settings <- attr(param, 'settings')
  ParallelLogger::logInfo(paste0('Training ', settings$modelName))

  start <- Sys.time()
  k <- param$k
  if(is.null(k))
    k <- 10
  if(is.null(param$threads)){
    param$threads<- 1
  }
  indexFolder <- param$indexFolder
  
  #removed as done in BigKnn
  labels <- trainData$labels %>% 
    dplyr::mutate(
      y = sapply(.data$outcomeCount, function(x) min(1,x))
    )
  
  BigKnn::buildKnnFromPlpData(
    plpData = trainData,
    population = labels,
    indexFolder = indexFolder,
    overwrite = TRUE
  )
  
  comp <- Sys.time() - start
  
  ParallelLogger::logInfo(paste0('Model knn trained - took:',  format(comp, digits=3)))

  variableImportance <- as.data.frame(trainData$covariateData$covariateRef)
  variableImportance$covariateValue <- rep(0, nrow(variableImportance))
  
  prediction <- predict_knn(
    trainData = trainData, 
    population = labels, 
    plpModel = list(
      model = indexFolder,
      modelSettings = list(
        model = 'knn',
        modelParameters = list(k = k),
        indexFolder = indexFolder,
        threads = param$threads
        )
      )
    )
  
  prediction$evaluationType <- 'Train'
  
  result <- list(
    model = indexFolder,
    
    prediction = prediction,
    
    settings = list(
      covariateSettings = attr(trainData, "metaData")$call$covariateSettings,
      featureEngineering = attr(trainData$covariateData, "metaData"),
      covariateMap = covariateMap,
      requireDenseMatrix = F,
      populationSettings = attr(trainData, "metaData")$populationSettings,
      modelSettings = list(
        model = 'KNN', 
        param = param,
        finalModelParameters = list(),
        extraSettings = attr(param, 'settings')
      )
    ),
    
    trainDetails = list(
      analysisId = analysisId,
      outcomeId = attr(trainData, "metaData")$populationSettings$outcomeId,
      cohortId = attr(trainData, "metaData")$call$cohortId,
      trainingTime = comp,
      trainingDate = Sys.Date(),
      hyperParamSearch =c()
    ),
    
    covariateImportance = variableImportance
  )
  
  
  class(result) <- 'plpModel'
  attr(result, 'predictionFunction') <- 'predictKnn'
  attr(result, 'modelType') <- 'binary'
  return(result)
}


predictKnn <- function(
  plpModel, 
  covariateData, 
  cohort
){
  
  prediction <- BigKnn::predictKnn(
    covariates = covaraiteData$covariates,
    cohort = cohort[,!colnames(cohort)%in%'cohortStartDate'],
    indexFolder = model$model,
    k = model$modelSettings$modelParameters$k,
    weighted = TRUE,
    threads = plpModel$modelSettings$threads
  )
  
  # can add: threads = 1 in the future
  
  # return the cohorts as a data frame with the prediction added as 
  # a new column with the column name 'value'
  prediction <- merge(cohort, prediction[,c('rowId','value')], by='rowId', 
    all.x=T, fill=0)
  prediction$value[is.na(prediction$value)] <- 0
  
  return(prediction)
  
}


