# @file naiveBayes.R
#
# Copyright 2016 Observational Health Data Sciences and Informatics
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

#' Create setting for naive bayes model with python 
#'
#' @examples
#' \dontrun{
#' model.nb <- setNaiveBayes()
#' }
#' @export
setNaiveBayes <- function(){
  
  # test python is available and the required dependancies are there:
  if (!PythonInR::pyIsConnected()){
    tryCatch({
      python.test <- PythonInR::autodetectPython(pythonExePath = NULL)
    }, error = function(err){
        stop('Python was not found on your system. See the vignette for instructions.')
       }  
    )
  }
  result <- list(model='fitNaiveBayes', name='Naive Bayes', param= '')
  class(result) <- 'modelSettings' 
  
  return(result)
}

fitNaiveBayes <- function(population, plpData, param, search='grid', quiet=F,
                      outcomeId, cohortId, ...){
  
  # check plpData is libsvm format or convert if needed
  if(!'ffdf'%in%class(plpData$covariates))
    stop('Need plpData')
  
  if(colnames(population)[ncol(population)]!='indexes'){
    warning('indexes column not present as last column - setting all index to 1')
    population$indexes <- rep(1, nrow(population))
  }
  
  # connect to python if not connected
  if ( !PythonInR::pyIsConnected() ){ 
    PythonInR::pyConnect()
    PythonInR::pyOptions("numpyAlias", "np")
    PythonInR::pyOptions("useNumpy", TRUE)
    PythonInR::pyImport("numpy", as='np')}
  
  
  # return error if we can't connect to python
  if ( !PythonInR::pyIsConnected() )
    stop('Python not connect error')
  
  start <- Sys.time()
  
  # make sure population is ordered?
  population$rowIdPython <- population$rowId-1 # -1 to account for python/r index difference
  PythonInR::pySet('population', as.matrix(population[,c('rowIdPython','outcomeCount','indexes')]) )
  
  # convert plpData in coo to python:
  x <- toSparsePython(plpData,population, map=NULL)
  
  # save the model to outLoc
  outLoc <- file.path(getwd(),'python_models')
  # clear the existing model pickles
  for(file in dir(outLoc))
    file.remove(file.path(outLoc,file))

  
  # run model:
  outLoc <- file.path(getwd(),'python_models')
  PythonInR::pySet("modelOutput",outLoc)
  

  # then run standard python code
  PythonInR::pyExecfile(system.file(package='PatientLevelPrediction','python','naive_bayes.py '))
  
  # then get the prediction 
  pred <- PythonInR::pyGet('prediction', simplify = FALSE)
  pred <-  apply(pred,1, unlist)
  pred <- t(pred)
  colnames(pred) <- c('rowId','outcomeCount','indexes', 'value')
  pred <- as.data.frame(pred)
  attr(pred, "metaData") <- list(predictionType="binary")
  
  # add 1 to rowId from python:
  pred$rowId <- pred$rowId+1
  
  pred$value <- 1-pred$value
  auc <- PatientLevelPrediction::computeAuc(pred)
  writeLines(paste0('Model obtained CV AUC of ', auc))
  
  # get the univeriate selected features (nb requires dense so need feat sel)
  #varImp <- read.csv(file.path(outLoc,1, 'varImp.txt'), header=F)[,1]
  varImp <- PythonInR::pyGet('kbest.scores_', simplify = F)[,1]
  varImp[is.na(varImp)] <- 0
  if(mean(varImp)==0)
    stop('No important variables - seems to be an issue with the data')
  
  top2000 <- varImp[order(-varImp)][2000]
  inc <- which(varImp>=top2000, arr.ind=T)
  covariateRef <- ff::as.ram(plpData$covariateRef)
  incs <- rep(0, nrow(covariateRef))
  incs[inc] <- 1
  covariateRef$included <- incs
  covariateRef$covariateValue <- varImp
  
  # select best model and remove the others
  modelTrained <- file.path(outLoc) 
  param.best <- ''
  
  comp <- start-Sys.time()
  
  # return model location
  result <- list(model = modelTrained,
                 trainCVAuc = auc,
                 modelSettings = list(model='naiveBayes_python',modelParameters=param.best),
                 hyperParamSearch = NULL,
                 metaData = plpData$metaData,
                 populationSettings = attr(population, 'metaData'),
                 outcomeId=outcomeId,
                 cohortId=cohortId,
                 varImp = covariateRef,
                 trainingTime =comp,
                 dense=1,
                 covariateMap=x$map
  )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'python'
  attr(result, 'predictionType') <- 'binary'
  
  
  return(result)
}