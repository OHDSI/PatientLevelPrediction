# @file knn.R
#
# Copyright 2019 Observational Health Data Sciences and Informatics
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
  if(class(indexFolder)!='character')
    stop('IndexFolder must be a character')
  if(!class(k) %in% c("numeric", "integer"))
    stop('k must be a numeric value >0 ')
  if(k<1)
    stop('k must be a numeric value >0 ')
  
  if(length(k)>1)
    stop('k can only be a single value')
  
  result <- list(model='fitKNN', param=list(k=k, indexFolder=indexFolder, threads=threads),
                 name='KNN'
  )
  class(result) <- 'modelSettings' 
  
  return(result)
}
fitKNN <- function(plpData,population, param, quiet=T, cohortId, outcomeId, ...){

  if (!FeatureExtraction::isCovariateData(plpData$covariateData)){
    stop("Needs correct covariateData")
  }
  metaData <- attr(population, 'metaData')
  if(!is.null(population$indexes))
    population <- population[population$indexes>0,]
  attr(population, 'metaData') <- metaData
  
  start <- Sys.time()
  k <- param$k
  if(is.null(k))
    k <- 10
  if(is.null(param$threads)){
    param$threads<- 1
  }
  indexFolder <- param$indexFolder
  
  #removed as done in BigKnn
  #covariateData <- limitCovariatesToPopulation(plpData$covariateData, population$rowId)
  
  population$y <- population$outcomeCount
  population$y[population$y>0] <- 1
  
  # create the model in indexFolder
  #BigKnn::buildKnn(population = population[,c('rowId','y')],
  #                 covariateData = covariateData,
  #                 indexFolder = indexFolder)
  
  BigKnn::buildKnnFromPlpData(plpData = plpData,
                                  population = population,
                                  indexFolder = indexFolder,
                                  overwrite = TRUE)
  
  
  comp <- Sys.time() - start
  if(!quiet)
    writeLines(paste0('Model knn trained - took:',  format(comp, digits=3)))
  
  varImp<- as.data.frame(plpData$covariateData$covariateRef)
  varImp$covariateValue <- rep(0, nrow(varImp))
  
  prediction <- predict.knn(plpData=plpData, 
                            population = population, 
                            plpModel=list(model=indexFolder,
                                          modelSettings = list(model='knn',
                                                               modelParameters=list(k=k),
                                                               indexFolder=indexFolder,
                                                               threads = param$threads
                                          )))
  
  
  result <- list(model = indexFolder,
                 trainCVAuc = NULL,    # did I actually save this!?
                 modelSettings = list(model='knn',
                                      modelParameters=list(k=k),
                                      indexFolder=indexFolder,
                                      threads = param$threads
                 ),
                 hyperParamSearch = unlist(param),
                 metaData = plpData$metaData,
                 populationSettings = attr(population, 'metaData'),
                 outcomeId=outcomeId,
                 cohortId=cohortId,
                 varImp = varImp,
                 trainingTime =comp,
                 predictionTrain = prediction
  )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'knn'
  attr(result, 'predictionType') <- 'binary'
  return(result)
}



