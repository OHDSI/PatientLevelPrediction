# @file knn.R
#
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
    ParallelLogger::logWarn('k can only be a single value - using first value only')
    k <- k[1]
  }
  
  
  param <- list(
    k = k,
    indexFolder = indexFolder,
    threads = threads
    )
  
  attr(param, 'settings') <- list(
    modelType = 'knn',
    modelName = 'K Nearest Neighbors'
  )
  
  attr(param, 'saveType') <- 'file'
  
  result <- list(
    fitFunction = "fitKNN",
    param = param
  )

  class(result) <- 'modelSettings' 
  
  return(result)
}

fitKNN <- function(trainData, modelSettings, search = 'none', analysisId, ...){
  
  param <- modelSettings$param

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
  
  
  BigKnn::buildKnnFromPlpData(
    plpData = trainData,
    population = trainData$labels %>% 
      dplyr::mutate(
        y = sapply(.data$outcomeCount, function(x) min(1,x))
      ),
    indexFolder = indexFolder,
    overwrite = TRUE
  )
  
  comp <- Sys.time() - start
  
  ParallelLogger::logInfo(paste0('Model knn trained - took:',  format(comp, digits=3)))

  variableImportance <- as.data.frame(trainData$covariateData$covariateRef)
  variableImportance$covariateValue <- rep(1, nrow(variableImportance))
  
  prediction <- predictKnn(
    data = trainData, 
    cohort = trainData$labels, 
    plpModel = list(
      model = indexFolder,
      trainDetails = list(
        finalModelParameters = list(
            k = k,
            threads = param$threads
          )
      )
    )
  )
      
  
  prediction$evaluationType <- 'Train'
  
  result <- list(
    model = indexFolder,
    
    preprocessing = list(
      featureEngineering = attr(trainData, "metaData")$featureEngineering,#learned mapping
      tidyCovariates = attr(trainData$covariateData, "metaData")$tidyCovariateDataSettings,  #learned mapping
      requireDenseMatrix = F
    ),
    
    prediction = prediction,
    
    modelDesign = PatientLevelPrediction::createModelDesign(
      targetId = attr(trainData, "metaData")$targetId,
      outcomeId = attr(trainData, "metaData")$outcomeId,
      restrictPlpDataSettings = attr(trainData, "metaData")$restrictPlpDataSettings,
      covariateSettings = attr(trainData, "metaData")$covariateSettings,
      populationSettings = attr(trainData, "metaData")$populationSettings,
      featureEngineeringSettings = attr(trainData$covariateData, "metaData")$featureEngineeringSettings,
      preprocessSettings = attr(trainData$covariateData, "metaData")$preprocessSettings, 
      modelSetting = modelSettings,
      splitSettings = attr(trainData, "metaData")$splitSettings,
      sampleSettings = attr(trainData, "metaData")$sampleSettings
    ),
    
    trainDetails = list(
      analysisId = analysisId,
      developmentDatabase = attr(trainData, "metaData")$cdmDatabaseName,
      developmentDatabaseSchema = attr(trainData, "metaData")$cdmDatabaseSchema, 
      attrition = attr(trainData, "metaData")$attrition, 
      trainingTime = paste(as.character(abs(comp)), attr(comp,'units')),
      trainingDate = Sys.Date(),
      modelName = 'KNN',
      hyperParamSearch = data.frame(),
      finalModelParameters = list(
        k = k,
        threads = param$threads
          )
    ),
    
    covariateImportance = variableImportance
  )
  
  
  class(result) <- 'plpModel'
  attr(result, 'predictionFunction') <- 'predictKnn'
  attr(result, 'modelType') <- 'binary'
  attr(result, 'saveType') <- attr(param, 'saveType')
  return(result)
}


predictKnn <- function(
  plpModel, 
  data, 
  cohort
){
  
  prediction <- BigKnn::predictKnn(
    covariates = data$covariateData$covariates,
    cohort = cohort[,!colnames(cohort)%in%'cohortStartDate'],
    indexFolder = plpModel$model,
    k = plpModel$trainDetails$finalModelParameters$k,
    weighted = TRUE,
    threads = plpModel$trainDetails$finalModelParameters$threads
  )
  
  # can add: threads = 1 in the future
  
  # return the cohorts as a data frame with the prediction added as 
  # a new column with the column name 'value'
  prediction <- merge(cohort, prediction[,c('rowId','value')], by='rowId', 
    all.x=T, fill=0)
  prediction$value[is.na(prediction$value)] <- 0
  
  attr(prediction, "metaData")$modelType <- 'binary'
  
  return(prediction)
  
}


