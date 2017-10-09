# @file CNNTorch.R
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

#' Create setting for CNN model with python 
#' @param nbfilters  The number of filters
#' @param epochs     The number of epochs
#' @param seed       A seed for the model
#' @param time_window       A time window to get temporal data
#' @param class_weight      weight the class in imblanced data
#' @param cnn_type      It can be normal 'CNN', 'CNN_MLF' with multiple kernels with different kernel size, 'CNN_MIX' and 'CNN_MULTI'
#' 
#' @examples
#' \dontrun{
#' model.cnnTorch <- setCNNTorch()
#' }
#' @export
setCNNTorch <- function(nbfilters=c(16, 32), epochs=c(20, 50), seed=0, time_window = 12, class_weight = 0, cnn_type = 'CNN'){
  
  # test python is available and the required dependancies are there:
  if (!PythonInR::pyIsConnected()){
    tryCatch({
      python.test <- PythonInR::autodetectPython(pythonExePath = NULL)
    }, error = function(err){
      stop('Python was not found on your system. See the vignette for instructions.')
    }  
    )
  }
  result <- list(model='fitCNNTorch', param= expand.grid(nbfilters=nbfilters,
                                                         epochs=epochs, seed=ifelse(is.null(seed),'NULL', seed), 
											time_window = time_window, class_weight = class_weight, cnn_type = cnn_type),
                 name='CNN Torch')
  
  class(result) <- 'modelSettings' 
  
  return(result)
}

#' @export
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
    auc <- do.call(trainCNNTorch,list(epochs=as.character(param$epochs[i]), nbfilters = as.character(param$nbfilters[i]), 
                                      seed = as.character(param$seed[i]), time_window = as.character(param$time_window[i]), 
									  class_weight = as.character(param$class_weight[i]), cnn_type = as.character(param$cnn_type[i]),
									  train = TRUE))
    
    all_auc <- c(all_auc, auc)
    writeLines(paste0('Model with settings: epochs: ',param$epochs[i], 
                      'nbfilters: ', param$nbfilters[i], 'seed: ', param$seed[i], 'time_window:', param$time_window[i], ' obtained AUC of ', auc))
  }
  
  hyperSummary <- cbind(param, cv_auc=all_auc)
  
  # run model:
  outLoc <- file.path(getwd(),'python_models')
  PythonInR::pySet("modelOutput",outLoc)
  
  
  # ToDo: I do not like this list creation
  finalModel <- do.call(trainCNNTorch,list(epochs=as.character(param$epochs[which.max(all_auc)]), 
                                           nbfilters=as.character(param$nbfilters[which.max(all_auc)]), 
                                           seed = as.character(param$seed[which.max(all_auc)]), 
                                           time_window = as.character(param$time_window[which.max(all_auc)]),
										                       class_weight = as.character(param$class_weight[which.max(all_auc)]),
										                       cnn_type = as.character(param$cnn_type[which.max(all_auc)]),
                                           train = FALSE))
  
  
  modelTrained <- file.path(outLoc) 
  param.best <- NULL
  
  comp <- start-Sys.time()
  
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
                 dense=1
  )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'python'
  attr(result, 'predictionType') <- 'binary'
  
  return(result)
}


trainCNNTorch <- function(epochs=50, nbfilters = 16, seed=0, time_window = 12, class_weight= 0, cnn_type = 'CNN', train=TRUE){
  #PythonInR::pyExec(paste0("size = ",size))
  PythonInR::pyExec(paste0("epochs = ",epochs))
  PythonInR::pyExec(paste0("nbfilters = ",nbfilters))
  PythonInR::pyExec(paste0("seed = ",seed))
  PythonInR::pyExec(paste0("time_window = ",time_window))
  PythonInR::pyExec(paste0("class_weight = ",class_weight))
  if (cnn_type == 'CNN'){
    PythonInR::pyExec("model_type = 'CNN'")
  } else if (cnn_type == 'CNN_MLF'){
    PythonInR::pyExec("model_type = 'CNN_MLF'")
  }
  else if (cnn_type == 'CNN_MIX'){
    PythonInR::pyExec("model_type = 'CNN_MIX'")
  } else if (cnn_type == 'CNN_MULTI'){
    PythonInR::pyExec("model_type = 'CNN_MULTI'")
  }
  #PythonInR::pyExec(paste0("model_type = ",cnn_type))
  if(train)
    PythonInR::pyExec("train = True")
  if(!train)
    PythonInR::pyExec("train = False")
  
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
