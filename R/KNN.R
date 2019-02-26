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
#'
#' @examples
#' \dontrun{
#' model.knn <- setKNN(k=10000)
#' }
#' @export
setKNN <- function(k=1000, indexFolder=file.path(getwd(),'knn')  ){
  ensure_installed("BigKnn")
  if(class(indexFolder)!='character')
    stop('IndexFolder must be a character')
  if(!class(k) %in% c("numeric", "integer"))
    stop('k must be a numeric value >0 ')
  if(k<1)
    stop('k must be a numeric value >0 ')
  
  if(length(k)>1)
    stop('k can only be a single value')
  
  result <- list(model='fitKNN', param=list(k=k, indexFolder=indexFolder),
                 name='KNN'
  )
  class(result) <- 'modelSettings' 
  
  return(result)
}
fitKNN <- function(plpData,population, param, quiet=T, cohortId, outcomeId, ...){
  # check plpData is coo format:
  if(!'ffdf'%in%class(plpData$covariates) )
    stop('KNN requires plpData in coo format')
  
  metaData <- attr(population, 'metaData')
  if(!is.null(population$indexes))
    population <- population[population$indexes>0,]
  attr(population, 'metaData') <- metaData
  
  start <- Sys.time()
  k <- param$k
  if(is.null(k))
    k <- 10
  indexFolder <- param$indexFolder
  
  #clone data to prevent accidentally deleting plpData 
  covariates <- ff::clone(plpData$covariates)
  covariates <- limitCovariatesToPopulation(covariates, ff::as.ff(population$rowId))
  
  population$y <- population$outcomeCount
  population$y[population$y>0] <- 1
  
  # create the model in indexFolder
  BigKnn::buildKnn(outcomes = ff::as.ffdf(population[,c('rowId','y')]),
                   covariates = ff::as.ffdf(covariates),
                   indexFolder = indexFolder)
  
  comp <- Sys.time() - start
  if(!quiet)
    writeLines(paste0('Model knn trained - took:',  format(comp, digits=3)))
  
  varImp<- ff::as.ram(plpData$covariateRef)
  varImp$covariateValue <- rep(0, nrow(varImp))
  
  prediction <- predict.knn(plpData=plpData, 
                            population = population, 
                            plpModel=list(model=indexFolder,
                                          modelSettings = list(model='knn',
                                                               modelParameters=list(k=k),
                                                               indexFolder=indexFolder
                                          )))
  
  result <- list(model = indexFolder,
                 trainCVAuc = NULL,    # did I actually save this!?
                 modelSettings = list(model='knn',
                                      modelParameters=list(k=k),
                                      indexFolder=indexFolder
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



