# @file MLP.R
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

#' Create setting for neural network model with python 
#' @param size       The number of hidden nodes
#' @param alpha      The l2 regularisation
#' @param seed       A seed for the model 
#'
#' @examples
#' \dontrun{
#' model.mlp <- setMLP(size=4, alpha=0.00001, seed=NULL)
#' }
#' @export
setMLP <- function(size=4, alpha=0.00001, seed=NULL){
  
  if(!class(seed)%in%c('numeric','NULL','integer'))
    stop('Invalid seed')
  if(!class(size) %in% c("numeric", "integer"))
    stop('size must be a numeric value >0 ')
  if(min(size) < 1)
    stop('size must be greater that 0')
  if(!class(alpha) %in% c("numeric", "integer"))
    stop('alpha must be a numeric value >0')
  if(min(alpha) <= 0)
    stop('alpha must be greater that 0')
  
  # test python is available and the required dependancies are there:
  ##checkPython()
  
  # set seed
  if(is.null(seed[1])){
    seed <- as.integer(sample(100000000,1))
  }
  
  result <- list(model='fitMLP', 
                 param= split(expand.grid(size=size, 
                                          alpha=alpha,
                                          seed=seed[1]),
                              1:(length(size)*length(alpha))  ),
                 name='Neural network')
  class(result) <- 'modelSettings' 
  
  return(result)
}

fitMLP <- function(population, plpData, param, search='grid', quiet=F,
                      outcomeId, cohortId, ...){
  
  # check plpData is libsvm format or convert if needed
  if(!'ffdf'%in%class(plpData$covariates))
    stop('Needs plpData')
  
  if(colnames(population)[ncol(population)]!='indexes'){
    warning('indexes column not present as last column - setting all index to 1')
    population$indexes <- rep(1, nrow(population))
  }
  
  # connect to python if not connected
  ##initiatePython()
  
  start <- Sys.time()
  
  population$rowIdPython <- population$rowId-1 # -1 to account for python/r index difference
  pPopulation <- as.matrix(population[,c('rowIdPython','outcomeCount','indexes')])
  
  # convert plpData in coo to python:
  x <- toSparseM(plpData,population, map=NULL)
  pydata <- reticulate::r_to_py(x$data)
  
  # save the model to outLoc  TODO: make this an input or temp location?
  outLoc <- createTempModelLoc()
  # clear the existing model pickles
  for(file in dir(outLoc))
    file.remove(file.path(outLoc,file))


  # do cross validation to find hyperParameter
  hyperParamSel <- lapply(param, function(x) do.call(trainMLP, listAppend(x,
                                                                          list(plpData = pydata,
                                                                          population = pPopulation,
                                                                          train=TRUE,
                                                                          quiet = quiet,
                                                                          modelOutput = outLoc)  )))

  
  hyperSummary <- cbind(do.call(rbind, param), unlist(hyperParamSel))
  
  #now train the final model and return coef
  bestInd <- which.max(abs(unlist(hyperParamSel)-0.5))[1]
  finalModel <- do.call(trainMLP, listAppend(param[[bestInd]], 
                                             list(plpData = pydata,
                                                  population = pPopulation,
                                                  train=FALSE,
                                                  quiet = quiet,
                                                  modelOutput = outLoc)
                                             ))
  
  # get the coefs and do a basic variable importance:
  lev1 <- finalModel[[2]]
  lev2 <- finalModel[[3]]
  vals <- abs(lev1)%*%abs(lev2)
  varImp <- apply(vals, 1, function(x) sum(abs(x)))
  
  covariateRef <- ff::as.ram(plpData$covariateRef)
  incs <- rep(1, nrow(covariateRef))
  covariateRef$included <- incs
  covariateRef$covariateValue <- unlist(varImp)
  
    
  # select best model and remove the others  (!!!NEED TO EDIT THIS)
  modelTrained <- file.path(outLoc) 
  param.best <- param[[bestInd]]
  
  comp <- start-Sys.time()
  
  # train prediction
  pred <- finalModel[[1]]
  pred[,1] <- pred[,1] + 1 # converting from python to r index
  colnames(pred) <- c('rowId','outcomeCount','indexes', 'value')
  pred <- as.data.frame(pred)
  attr(pred, "metaData") <- list(predictionType="binary")
  prediction <- merge(population, pred[,c('rowId', 'value')], by='rowId')
  
  
  # return model location (!!!NEED TO ADD CV RESULTS HERE)
  result <- list(model = modelTrained,
                 trainCVAuc = hyperParamSel,
                 hyperParamSearch = hyperSummary,
                 modelSettings = list(model='fitMLP',modelParameters=param.best),
                 metaData = plpData$metaData,
                 populationSettings = attr(population, 'metaData'),
                 outcomeId=outcomeId,
                 cohortId=cohortId,
                 varImp = covariateRef,
                 trainingTime =comp,
                 dense=0,
                 covariateMap=x$map,
                 predictionTrain = prediction
  )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'pythonReticulate'
  attr(result, 'predictionType') <- 'binary'
  
  return(result)
}


trainMLP <- function(plpData, population, size=1, alpha=0.001, seed=NULL, train=TRUE, quiet=F, modelOutput){

  e <- environment()
  reticulate::source_python(system.file(package='PatientLevelPrediction','python','mlpFunctions.py'), envir = e)
  
  result <- train_mlp(population = population, 
                      plpData = plpData, 
                      alpha =alpha, 
                      size = as.integer(size), 
                      seed = as.integer(seed), 
                      quiet = quiet, 
                      modelOutput = modelOutput,
                      train = train)
  
  if(train){
    # then get the prediction 
    pred <- result
    colnames(pred) <- c('rowId','outcomeCount','indexes', 'value')
    pred <- as.data.frame(pred)
    attr(pred, "metaData") <- list(predictionType="binary")
    auc <- PatientLevelPrediction::computeAuc(pred)
    writeLines(paste0('Model obtained CV AUC of ', auc))
    return(auc)
  }
  
  return(result)
}
