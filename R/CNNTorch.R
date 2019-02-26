# @file CNNTorch.R
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

#' Create setting for CNN model with python 
#' @param nbfilters  The number of filters
#' @param epochs     The number of epochs
#' @param seed       A seed for the model
#' @param class_weight   The class weight used for imbalanced data: 
#'                           0: Inverse ratio between positives and negatives
#'                          -1: Focal loss
#' @param type      It can be normal 'CNN', 'CNN_LSTM', CNN_MLF' with multiple kernels with different kernel size, 
#'                      'CNN_MIX', 'ResNet' and 'CNN_MULTI'
#' 
#' @examples
#' \dontrun{
#' model.cnnTorch <- setCNNTorch()
#' }
#' @export
setCNNTorch <- function(nbfilters=c(16, 32), epochs=c(20, 50), seed=0, class_weight = 0, type = 'CNN'){
  
  ensure_installed("PythonInR")
  # test python is available and the required dependancies are there:
  if ( !PythonInR::pyIsConnected() ){
    python.test <- PythonInR::autodetectPython(pythonExePath = NULL)
    
    if(is.null(python.test$pythonExePath))
      stop('You need to install python for this method - please see ...')
  }
  
  result <- list(model='fitCNNTorch', param=split(expand.grid(nbfilters=nbfilters,
                                            epochs=epochs, seed=ifelse(is.null(seed),'NULL', seed), 
											class_weight = class_weight, type = type),
											1:(length(nbfilters)*length(epochs)) ),
                 					  name='CNN Torch')
  
  class(result) <- 'modelSettings' 
  
  return(result)
}


fitCNNTorch <- function(population, plpData, param, search='grid', quiet=F,
                        outcomeId, cohortId, ...){
  
  # check plpData is libsvm format or convert if needed
  if(!'ffdf'%in%class(plpData$covariates))
    stop('Needs plpData')
  
  if(colnames(population)[ncol(population)]!='indexes'){
    warning('indexes column not present as last column - setting all index to 1')
    population$indexes <- rep(1, nrow(population))
  }
  
  # connect to python if not connected
  if(!PythonInR::pyIsConnected()){ 
    ParallelLogger::logTrace('Connecting to python')
    PythonInR::pyConnect()
  }
  if ( !PythonInR::pyIsConnected() ){
    stop('Python not connecting error')
  }
  
  # then set numpy options
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
  #covariates <- plpData$covariates
  #covariates$rowIdPython <- covariates$rowId -1 #to account for python/r index difference
  #PythonInR::pySet('covariates', as.matrix(covariates[,c('rowIdPython','covariateId','timeId', 'covariateValue')]))
  
  result <- toSparseTorchPython(plpData,population,map=NULL, temporal=T)
  #result<- toSparsePython(plpData,population,map=NULL, temporal=T)
  # save the model to outLoc  TODO: make this an input or temp location?
  outLoc <- file.path(getwd(),'python_models')
  # clear the existing model pickles
  for(file in dir(outLoc))
    file.remove(file.path(outLoc,file))

  outLoc <- file.path(getwd(),'python_models')
  PythonInR::pySet("modelOutput",outLoc)

  # do cross validation to find hyperParameter
  hyperParamSel <- lapply(param, function(x) do.call(trainCNNTorch, c(x, train=TRUE)  ))
 
  hyperSummary <- cbind(do.call(rbind, param), unlist(hyperParamSel))
  
  #now train the final model and return coef
  bestInd <- which.max(abs(unlist(hyperParamSel)-0.5))[1]
  finalModel <- do.call(trainCNNTorch, c(param[[bestInd]], train=FALSE))

  covariateRef <- ff::as.ram(plpData$covariateRef)
  incs <- rep(1, nrow(covariateRef)) 
  covariateRef$included <- incs
  covariateRef$covariateValue <- rep(0, nrow(covariateRef))
  
  modelTrained <- file.path(outLoc) 
  param.best <- param[[bestInd]]
  
  comp <- start-Sys.time()
  
  # train prediction
  pred <- PythonInR::pyGet('prediction', simplify = F)
  pred <-  apply(pred,1, unlist)
  pred <- t(pred)
  pred[,1] <- pred[,1] + 1 # converting from python to r index
  colnames(pred) <- c('rowId','outcomeCount','indexes', 'value')
  pred <- as.data.frame(pred)
  attr(pred, "metaData") <- list(predictionType="binary")
  prediction <- merge(population, pred[,c('rowId', 'value')], by='rowId')
  
  
  # return model location 
  result <- list(model = modelTrained,
                 trainCVAuc = -1, # ToDo decide on how to deal with this
                 hyperParamSearch = hyperSummary,
                 modelSettings = list(model='fitCNNTorch',modelParameters=param.best),
                 metaData = plpData$metaData,
                 populationSettings = attr(population, 'metaData'),
                 outcomeId=outcomeId,
                 cohortId=cohortId,
                 varImp = covariateRef, 
                 trainingTime =comp,
                 dense=1,
                 covariateMap=result$map, # I think this is need for new data to map the same?
                 predictionTrain = prediction
                 )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'python'
  attr(result, 'predictionType') <- 'binary'
  
  return(result)
}


trainCNNTorch <- function(epochs=50, nbfilters = 16, seed=0, class_weight= 0, type = 'CNN', train=TRUE){
  #PythonInR::pyExec(paste0("size = ",size))
  PythonInR::pyExec(paste0("epochs = ",epochs))
  PythonInR::pyExec(paste0("nbfilters = ",nbfilters))
  PythonInR::pyExec(paste0("seed = ",seed))
  #PythonInR::pyExec(paste0("time_window = ",time_window))
  PythonInR::pyExec(paste0("class_weight = ",class_weight))
  if (type == 'CNN'){
    PythonInR::pyExec("model_type = 'CNN'")
  } else if (type == 'CNN_LSTM'){
    PythonInR::pyExec("model_type = 'CNN_LSTM'")
  }
  else if (type == 'CNN_MLF'){
    PythonInR::pyExec("model_type = 'CNN_MLF'")
  }
  else if (type == 'CNN_MIX'){
    PythonInR::pyExec("model_type = 'CNN_MIX'")
  } else if (type == 'CNN_MULTI'){
    PythonInR::pyExec("model_type = 'CNN_MULTI'")
  } else if (type == 'ResNet'){
    PythonInR::pyExec("model_type = 'ResNet'")
  }
  #PythonInR::pyExec(paste0("model_type = ",type))
  if(train)
    PythonInR::pyExec("train = True")
  if(!train)
    PythonInR::pyExec("train = False")
  python_dir <- system.file(package='PatientLevelPrediction','python')
  PythonInR::pySet("python_dir", python_dir)
  # then run standard python code #learningcurve.py #deepTorch.py
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
