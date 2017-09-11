# @file randomForest.R
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

#' Create setting for random forest model with python (very fast)
#'
#' @param mtries     The number of features to include in each tree (-1 defaults to square root of total features)
#' @param ntrees     The number of trees to build 
#' @param max_depth  Maximum number of interactions - a large value will lead to slow model training
#' @param varImp     Perform an initial variable selection prior to fitting the model to select the useful variables
#' @param seed       An option to add a seed when training the final model
#'
#' @examples
#' \dontrun{
#' model.rf <- setRandomForest(mtries=c(-1,5,20),  ntrees=c(10,100), 
#'                            max_depth=c(5,20))
#' }                           
#' @export
setRandomForest<- function(mtries=-1,ntrees=c(10,500),max_depth=17, varImp=T, seed=NULL){
  # check seed is int
  if(!class(seed)%in%c('numeric','NULL'))
    stop('Invalid seed')
  if(class(ntrees)!='numeric')
    stop('ntrees must be a numeric value >0')
  if(sum(ntrees < 0)>0)
    stop('mtries must be greater that 0')
  if(class(mtries)!='numeric')
    stop('mtries must be a numeric value >1 or -1')
  if(sum(mtries < -1)>0)
    stop('mtries must be greater that 0 or -1')
  if(class(max_depth)!='numeric')
    stop('max_depth must be a numeric value >0')
  if(sum(max_depth < 1)>0)
    stop('max_depth must be greater that 0')
  if(class(varImp)!="logical")
    stop('varImp must be boolean')
  
  # test python is available and the required dependancies are there:
  if ( !PythonInR::pyIsConnected() ){
    python.test <- PythonInR::autodetectPython(pythonExePath = NULL)
    
    if(is.null(python.test$pythonExePath))
      stop('You need to install python for this method - please see ...')
  }
  
  result <- list(model='fitRandomForest', param= expand.grid(ntrees=ntrees, mtries=mtries,
                                                       max_depth=max_depth, varImp=varImp, 
                                                       seed=ifelse(is.null(seed),'NULL', seed)),
                 name='Random forest')
  class(result) <- 'modelSettings' 
  
  return(result)
}



fitRandomForest <- function(population, plpData, param, search='grid', quiet=F,
                      outcomeId, cohortId, ...){
  
  # check plpData is libsvm format:
  if(!'ffdf'%in%class(plpData$covariates))
    stop('Random forest requires plpData')
  
  if(colnames(population)[ncol(population)]!='indexes'){
    warning('indexes column not present as last column - setting all index to 1')
    population$indexes <- rep(1, nrow(population))
  }
  
  # connect to python if not connected
  if ( !PythonInR::pyIsConnected() || .Platform$OS.type=="unix"){ 
    PythonInR::pyConnect()
    PythonInR::pyOptions("numpyAlias", "np")
    PythonInR::pyOptions("useNumpy", TRUE)
    PythonInR::pyImport("numpy", as='np')}
  
  # return error if we can't connect to python
  if ( !PythonInR::pyIsConnected() )
    stop('Python not connect error')
  
  PythonInR::pyExec('quiet = True')
  if(quiet==F){
    writeLines(paste0('Training random forest model...' ))
    PythonInR::pyExec('quiet = False')
  }
  start <- Sys.time()
  
  # make sure population is ordered?
  population$rowIdPython <- population$rowId-1 # -1 to account for python/r index difference
  PythonInR::pySet('population', as.matrix(population[,c('rowIdPython','outcomeCount','indexes')]) )
  
  
  # set seed
  if(param$seed[1] == 'NULL')
    PythonInR::pyExec('seed = None')
  if(param$seed[1]!='NULL'){
    PythonInR::pySet('seed', as.matrix(param$seed[1]) )
    PythonInR::pyExec('seed = int(seed)')
  }

  # convert plpData in coo to python:
  x <- toSparsePython(plpData,population, map=NULL)
    
  #do var imp
  if(param$varImp[1]==T){
  
    # python checked in .set 
    PythonInR::pyExecfile(system.file(package='PatientLevelPrediction','python','rf_var_imp.py'))
    
    
    #load var imp and create mapping/missing
    varImp <-PythonInR::pyGet("rf.feature_importances_", simplify = FALSE)[,1]
    
    if(!quiet)
      writeLines('Variable importance completed')  
    if(mean(varImp)==0)
      stop('No important variables - seems to be an issue with the data')
    
    inc <- which(varImp>mean(varImp), arr.ind=T)
    covariateRef <- ff::as.ram(plpData$covariateRef) 
    
    # save mapping, missing, indexes
  } else{
    covariateRef <- ff::as.ram(plpData$covariateRef)
    inc <- 1:ncol(covariateRef)
  }
  
  # write the include covariates to file (gets read by python)
  ##write.table(inc-1, file.path(plpData$covariates, 'included.txt'), row.names=F, col.names = F)
  # above now is loaded threw pythoninR
  
  
  # save the model to outLoc
  outLoc <- file.path(getwd(),'python_models')
  # clear the existing model pickles
  for(file in dir(outLoc))
    file.remove(file.path(outLoc,file))
  
  # run rf_plp for each grid search:
  all_auc <- c()
  
  for(i in 1:nrow(param)){
    
    # set variable params - do loop  
    PythonInR::pyExec(paste0("ntrees = int(",param$ntree[i],")"))
    PythonInR::pyExec(paste0("max_depth = int(",param$max_depth[i],")"))
    PythonInR::pySet("mtry",param$mtries[i])
    
    ##PythonInR::pySet("dataLocation" ,plpData$covariates)
    
    # do inc-1 to go to python index as python starts at 0, R starts at 1
    ##PythonInR::pyImport("numpy", as="np")
    PythonInR::pySet('included', as.matrix(inc-1), 
                     namespace = "__main__", useNumpy = TRUE)
    
    #mapping = sys.argv[5] # this contains column selection/ordering 
    #missing = sys.argv[6] # this contains missing
    
    # then run standard python code
    PythonInR::pyExecfile(system.file(package='PatientLevelPrediction','python','randomForestCV.py'))
    
    # then get the prediction 
    pred <- PythonInR::pyGet('prediction', simplify = FALSE)
    pred <-  apply(pred,1, unlist)
    pred <- t(pred)
    colnames(pred) <- c('rowId','outcomeCount','indexes', 'value')
    pred <- as.data.frame(pred)
    attr(pred, "metaData") <- list(predictionType="binary")
    # close python
    
    ##pred <- read.csv(file.path(outLoc,i,'prediction.txt'), header=F)
    ##colnames(pred) <- c('rowId','outcomeCount','indexes', 'value')
    auc <- PatientLevelPrediction::computeAuc(pred)
    all_auc <- c(all_auc, auc)
    if(!quiet)
      writeLines(paste0('Model with settings: ntrees:',param$ntrees[i],' max_depth: ',param$max_depth[i], 
                      'mtry: ', param$mtry[i] , ' obtained AUC of ', auc))
  }
  
  hyperSummary <- cbind(param, cv_auc=all_auc)
  
  # now train the final model for the best hyper-parameters previously found
  PythonInR::pyExec(paste0("ntrees = int(",param$ntree[which.max(all_auc)],")"))
  PythonInR::pyExec(paste0("max_depth = int(",param$max_depth[which.max(all_auc)],")"))
  PythonInR::pySet("mtry",param$mtries[which.max(all_auc)])
  PythonInR::pySet("modelOutput",outLoc)
  
  PythonInR::pyExecfile(system.file(package='PatientLevelPrediction','python','finalRandomForest.py'))
  
  modelTrained <- file.path(outLoc) # location 
  param.best <- param[which.max(all_auc),]
  varImp <- PythonInR::pyGet('rf.feature_importances_', simplify = F)[,1]

  variableImportance <- rep(0, nrow(covariateRef))
  variableImportance[inc] <- varImp
  incs <- rep(0, nrow(covariateRef))
  incs[inc] <- 1
  covariateRef <- cbind(covariateRef, incs, variableImportance)
  colnames(covariateRef) <- c('covariateId','covariateName','analysisId','conceptId','included','covariateValue')
  ##write.table(covariateRef, file.path(outLoc, 'covs.txt'), row.names=F, col.names=T) # might not need?
  
  pred <- PythonInR::pyGet('prediction', simplify = F)
  pred <-  apply(pred,1, unlist)
  pred <- t(pred)
  colnames(pred) <- c('rowId','outcomeCount','indexes', 'value')
  pred <- as.data.frame(pred)
  attr(pred, "metaData") <- list(predictionType="binary")
  
  auc <- PatientLevelPrediction::computeAuc(pred)
  writeLines(paste0('Final model with ntrees:',param$ntrees[which.max(all_auc)],' max_depth: ',param$max_depth[which.max(all_auc)], 
                    'mtry: ', param$mtry[which.max(all_auc)] , ' obtained AUC of ', auc))
  
  # close python:
  ##PythonInR::pyExit()
  
  comp <- start-Sys.time()
  
  # return model location
  result <- list(model = modelTrained,
                 trainCVAuc = all_auc[which.max(all_auc)],
                 modelSettings = list(model='randomForest_python',modelParameters=param.best),
                 hyperParamSearch = hyperSummary,
                 metaData = plpData$metaData,
                 populationSettings = attr(population, 'metaData'),
                 outcomeId=outcomeId,
                 cohortId=cohortId,
                 varImp = covariateRef,
                 trainingTime =comp,
                 dense=0,
                 covariateMap=x$map
  )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'python'
  attr(result, 'predictionType') <- 'binary'
  
  
  return(result)
}
