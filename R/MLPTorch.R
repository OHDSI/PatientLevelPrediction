# @file MLPTorch.R
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

#' Create setting for neural network model with python 
#' @param size       The number of hidden nodes
#' @param w_decay      The l2 regularisation
#' @param seed       A seed for the model
#' @param epochs     The number of epochs 
#' @param class_weight   weight the class in imblanced data
#'
#' @examples
#' \dontrun{
#' model.mlpTorch <- setMLPTorch()
#' }
#' @export
setMLPTorch <- function(size=c(500, 1000), w_decay=c(0.0005, 0.005), epochs=c(20, 50), seed=0, class_weight = 0){
  
  # test python is available and the required dependancies are there:
  if (!PythonInR::pyIsConnected()){
    tryCatch({
      python.test <- PythonInR::autodetectPython(pythonExePath = NULL)
    }, error = function(err){
      stop('Python was not found on your system. See the vignette for instructions.')
    }  
    )
  }
  result <- list(model='fitMLPTorch', param= expand.grid(size=size, w_decay=w_decay,
                 epochs=epochs, seed=ifelse(is.null(seed),'NULL', seed), class_weight = ifelse(is.null(class_weight),'NULL', class_weight)), 
                 name='MLP Torch')
  
  #result <- list(model='fitMLPTorch', 
  #               param= c(size,epochs,seed),
  #               name='MLP Torch')
  
  class(result) <- 'modelSettings' 
  
  return(result)
}

#' @export
fitMLPTorch <- function(population, plpData, param, search='grid', quiet=F,
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
  
  population$rowIdPython <- population$rowId-1 # -1 to account for python/r index difference
  PythonInR::pySet('population', as.matrix(population[,c('rowIdPython','outcomeCount','indexes')]) )
  
  # convert plpData in coo to python:
  x <- toSparsePython(plpData,population, map=NULL)
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
    # set variable params - do loop  
    #PythonInR::pyExec(paste0("size = int(",param$size[i],")"))
    #PythonInR::pyExec(paste0("w_decay = int(",param$w_decay[i],")"))
    #PythonInR::pyExec(paste0("epochs = int(",param$epochs[i],")"))
    #PythonInR::pySet("epochs",param$epochs[i])
    #PythonInR::pySet("train", FALSE)
    ##PythonInR::pySet("dataLocation" ,plpData$covariates)
    
    # do inc-1 to go to python index as python starts at 0, R starts at 1
    ##PythonInR::pyImport("numpy", as="np")
    PythonInR::pySet('included', as.matrix(inc-1), 
                     namespace = "__main__", useNumpy = TRUE)
    
    #mapping = sys.argv[5] # this contains column selection/ordering 
    #missing = sys.argv[6] # this contains missing
    
    # then run standard python code
    #PythonInR::pyExecfile(system.file(package='PatientLevelPrediction','python','mlp_torch.py'))
    auc <- do.call(trainMLPTorch,list(size=as.character(param$size[i]), epochs=as.character(param$epochs[i]), w_decay = as.character(param$w_decay[i]), 
                                                        seed = as.character(param$seed[i]), class_weight = as.character(param$class_weight[i]), train = TRUE))

    # close python
    
    ##pred <- read.csv(file.path(outLoc,i,'prediction.txt'), header=F)
    ##colnames(pred) <- c('rowId','outcomeCount','indexes', 'value')
    ##auc <- PatientLevelPrediction::computeAuc(pred)
    all_auc <- c(all_auc, auc)
    writeLines(paste0('Model with settings: size:',param$size[i],' epochs: ',param$epochs[i], 
                      'w_decay: ', param$w_decay[i], 'seed: ', param$seed[i], 'class_weight: ', param$class_weight[i], ' obtained AUC of ', auc))
  }
  
  hyperSummary <- cbind(param, cv_auc=all_auc)
  
  # run model:
  outLoc <- file.path(getwd(),'python_models')
  PythonInR::pySet("modelOutput",outLoc)

  
  # ToDo: I do not like this list creation
  finalModel <- do.call(trainMLPTorch,list(size=as.character(param$size[which.max(all_auc)]), 
                                           epochs=as.character(param$epochs[which.max(all_auc)]), 
                                           w_decay=as.character(param$w_decay[which.max(all_auc)]), 
                                           seed = as.character(param$seed[which.max(all_auc)]), 
                                           class_weight = as.character(param$class_weight[which.max(all_auc)]), train = FALSE))
  
  
  modelTrained <- file.path(outLoc) 
  param.best <- NULL
  
  comp <- start-Sys.time()
  
  # return model location 
  result <- list(model = modelTrained,
                 trainCVAuc = -1, # ToDo decide on how to deal with this
                 hyperParamSearch = hyperSummary,
                 modelSettings = list(model='fitMLPTorch',modelParameters=param.best),
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


trainMLPTorch <- function(size=200, epochs=100, w_decay = 0.001, seed=0, class_weight = 0, train=TRUE){
  PythonInR::pyExec(paste0("size = ",size))
  PythonInR::pyExec(paste0("epochs = ",epochs))
  PythonInR::pyExec(paste0("w_decay = ",w_decay))
  PythonInR::pyExec(paste0("seed = ",seed))
  PythonInR::pyExec(paste0("class_weight = ",class_weight))
  PythonInR::pyExec("model_type = 'MLP'")
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
