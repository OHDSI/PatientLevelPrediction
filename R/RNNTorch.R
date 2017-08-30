# @file RNNTorch.R
#
# Copyright 2017 Observational Health Data Sciences and Informatics
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

#' Create setting for RNN model with python 
#' @param hidden_size  The hidden size
#' @param epochs     The number of epochs
#' @param seed       A seed for the model
#' @param time_window       A time window to get temporal data
#'
#' @examples
#' \dontrun{
#' model.rnnTorch <- setRNNTorch()
#' }
#' @export
setRNNTorch <- function(hidden_size=c(50, 100), epochs=c(20, 50), seed=0, time_window = 12, class_weight = 0){
  
  # test python is available and the required dependancies are there:
  if (!PythonInR::pyIsConnected()){
    tryCatch({
      python.test <- PythonInR::autodetectPython(pythonExePath = NULL)
    }, error = function(err){
      stop('Python was not found on your system. See the vignette for instructions.')
    }  
    )
  }
  result <- list(model='fitRNNTorch', param= expand.grid(hidden_size=hidden_size,
                                                         epochs=epochs, seed=ifelse(is.null(seed),'NULL', seed), 
                                                         time_window = time_window, class_weight = class_weight),
                 name='RNN Torch')
  
  class(result) <- 'modelSettings' 
  
  return(result)
}

#' @export
fitRNNTorch <- function(population, plpData, param, search='grid', quiet=F,
                        outcomeId, cohortId, ...){
  
  # check plpData is libsvm format or convert if needed
  if(!'ffdf'%in%class(plpData$covariates))
    stop('Needs plpData')
  
  if(colnames(population)[ncol(population)]!='indexes'){
    warning('indexes column not present as last column - setting all index to 1')
    population$indexes <- rep(1, nrow(population))
  }
  
  # connect to python if not connected
  if ( !PythonInR::pyIsConnected() ){ 
    PythonInR::pyConnect()
  }
  
  # return error if we can't connect to python
  if ( !PythonInR::pyIsConnected() )
    stop('Python not connect error')
  
  PythonInR::pyOptions("numpyAlias", "np")
  PythonInR::pyOptions("useNumpy", TRUE)
  PythonInR::pyImport("numpy", as='np')
  
  start <- Sys.time()
  
  population$rowIdPython <- population$rowId-1  #to account for python/r index difference #subjectId
  #idx <- ffbase::ffmatch(x = population$subjectId, table = ff::as.ff(plpData$covariates$rowId))
  #idx <- ffbase::ffwhich(idx, !is.na(idx))
  #population <- population[idx, ]
  
  PythonInR::pySet('population', as.matrix(population[,c('rowIdPython','outcomeCount','indexes')]) )
  
  # convert plpData in coo to python:
  covariates <- plpData$covariates
  covariates$rowIdPython <- covariates$rowId -1 #to account for python/r index difference
  PythonInR::pySet('covariates', as.matrix(covariates[,c('rowIdPython','covariateId','timeId', 'covariateValue')]))
  
  covariateRef <- ff::as.ram(plpData$covariateRef)
  inc <- 1:ncol(covariateRef)  
  # save the model to outLoc  TODO: make this an input or temp location?
  outLoc <- file.path(getwd(),'python_models')
  # clear the existing model pickles
  for(file in dir(outLoc))
    file.remove(file.path(outLoc,file))
  
  covariateRef <- ff::as.ram(plpData$covariateRef)
  incs <- rep(1, nrow(covariateRef))
  covariateRef$included <- incs
  #covariateRef$value <- unlist(varImp)
  all_auc <- c()
  
  for(i in 1:nrow(param)){
    
    # do inc-1 to go to python index as python starts at 0, R starts at 1
    PythonInR::pySet('included', as.matrix(inc-1), 
                     namespace = "__main__", useNumpy = TRUE)
    
    # then run standard python code
    auc <- do.call(trainRNNTorch,list(epochs=as.character(param$epochs[i]), hidden_size = as.character(param$hidden_size[i]), 
                                      seed = as.character(param$seed[i]), time_window = as.character(param$time_window[i]), 
                                      class_weight = as.character(param$class_weight[i]), train = TRUE))
    
    all_auc <- c(all_auc, auc)
    writeLines(paste0('Model with settings: epochs: ',param$epochs[i], 
                      'hidden_size: ', param$hidden_size[i], 'seed: ', param$seed[i], 'time_window:', param$time_window[i], ' obtained AUC of ', auc))
  }
  
  hyperSummary <- cbind(param, cv_auc=all_auc)
  
  # run model:
  outLoc <- file.path(getwd(),'python_models')
  PythonInR::pySet("modelOutput",outLoc)
  
  
  # ToDo: I do not like this list creation
  finalModel <- do.call(trainRNNTorch,list(epochs=as.character(param$epochs[which.max(all_auc)]), 
                                           hidden_size=as.character(param$hidden_size[which.max(all_auc)]), 
                                           seed = as.character(param$seed[which.max(all_auc)]), 
                                           time_window = as.character(param$time_window[which.max(all_auc)]),
                                           class_weight = as.character(param$class_weight[which.max(all_auc)]),
                                           train = FALSE))
  
  
  modelTrained <- file.path(outLoc) 
  param.best <- NULL
  
  comp <- start-Sys.time()
  
  # return model location 
  result <- list(model = modelTrained,
                 trainCVAuc = -1, # ToDo decide on how to deal with this
                 hyperParamSearch = hyperSummary,
                 modelSettings = list(model='fitRNNTorch',modelParameters=param.best),
                 metaData = plpData$metaData,
                 populationSettings = attr(population, 'metaData'),
                 outcomeId=outcomeId,
                 cohortId=cohortId,
                 varImp = covariateRef, 
                 trainingTime =comp,
                 dense=1
  )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'python'
  attr(result, 'predictionType') <- 'binary'
  
  return(result)
}


trainRNNTorch <- function(epochs=50, hidden_size = 100, seed=0, time_window = 12, class_weight= 0, train=TRUE){
  #PythonInR::pyExec(paste0("size = ",size))
  PythonInR::pyExec(paste0("epochs = ",epochs))
  PythonInR::pyExec(paste0("hidden_size = ",hidden_size))
  PythonInR::pyExec(paste0("seed = ",seed))
  PythonInR::pyExec(paste0("time_window = ",time_window))
  PythonInR::pyExec(paste0("class_weight = ",class_weight))
  PythonInR::pyExec("model_type = 'RNN'")
  if(train)
    PythonInR::pyExec("train = True")
  if(!train)
    PythonInR::pyExec("train = False")
  
  # then run standard python code
  PythonInR::pyExecfile(system.file(package='PatientLevelPrediction','python','deepTorch.py'))
  
  if(train){
    # then get the prediction 
    pred <- PythonInR::pyGet('prediction', simplify = FALSE)
    pred <-  apply(pred,1, unlist)
    pred <- t(pred)
    colnames(pred) <- c('rowId','outcomeCount','indexes', 'value')
    pred <- as.data.frame(pred)
    attr(pred, "metaData") <- list(predictionType="binary")
    
    pred$value <- 1-pred$value
    auc <- PatientLevelPrediction::computeAuc(pred)
    writeLines(paste0('Model obtained CV AUC of ', auc))
    return(auc)
  }
  
  return(T)
  
}
