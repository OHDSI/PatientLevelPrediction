# @file randomForest.R
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

#' Create setting for random forest model with python (very fast)
#'
#' @param mtries     The number of features to include in each tree (-1 defaults to square root of total features)
#' @param ntrees     The number of trees to build 
#' @param maxDepth  Maximum number of interactions - a large value will lead to slow model training
#' @param varImp     Perform an initial variable selection prior to fitting the model to select the useful variables
#' @param seed       An option to add a seed when training the final model
#'
#' @examples
#' \dontrun{
#' model.rf <- setRandomForest(mtries=c(-1,5,20),  ntrees=c(10,100), 
#'                            maxDepth=c(5,20))
#' }                           
#' @export
setRandomForest <- function(mtries=-1,ntrees=500,maxDepth=c(4,10,17), varImp=T, seed=NULL){
  # check seed is int
  if(!class(seed)%in%c('numeric','NULL','integer'))
    stop('Invalid seed')
  if(!class(ntrees) %in% c("numeric", "integer"))
    stop('ntrees must be a numeric value >0')
  if(sum(ntrees < 0)>0)
    stop('mtries must be greater that 0')
  if(!class(mtries) %in% c("numeric", "integer"))
    stop('mtries must be a numeric value >1 or -1')
  if(sum(mtries < -1)>0)
    stop('mtries must be greater that 0 or -1')
  if(!class(maxDepth) %in% c("numeric", "integer"))
    stop('maxDepth must be a numeric value >0')
  if(sum(maxDepth < 1)>0)
    stop('maxDepth must be greater that 0')
  if(class(varImp)!="logical")
    stop('varImp must be boolean')
  
  # set seed
  if(is.null(seed[1])){
    seed <- as.integer(sample(100000000,1))
  }
  
  result <- list(model='fitRandomForest', param= expand.grid(ntrees=ntrees, mtries=mtries,
                                                              maxDepth=maxDepth, varImp=varImp, 
                                                              seed= seed[1]),
                 name='Random forest')
  class(result) <- 'modelSettings' 
  
  return(result)
}



fitRandomForest <- function(population, plpData, param, search='grid', quiet=F,
                             outcomeId, cohortId, ...){
  
  covariateRef <- ff::as.ram(plpData$covariateRef)
  e <- environment()
  
  # check logger
  if(length(ParallelLogger::getLoggers())==0){
    logger <- ParallelLogger::createLogger(name = "SIMPLE",
                                        threshold = "INFO",
                                        appenders = list(ParallelLogger::createConsoleAppender(layout = ParallelLogger::layoutTimestamp)))
    ParallelLogger::registerLogger(logger)
  }
  
  # check plpData is libsvm format:
  if(!'ffdf'%in%class(plpData$covariates)){
    ParallelLogger::logError('class plpData$covariates: ', class(plpData$covariates))
    stop('Random forest requires plpData')
  }
  
  if(colnames(population)[ncol(population)]!='indexes'){
    ParallelLogger::logWarn(paste0('population columns: ', paste0(colnames(population), collapse='-')))
    warning('indexes column not present as last column - setting all index to 1')
    population$indexes <- rep(1, nrow(population))
  }

  pQuiet <- 'True'
  if(quiet==F){
    ParallelLogger::logTrace(paste0('Training random forest model...' ))
    #PythonInR::pyExec('quiet = False')
    pQuiet <- 'False'
  }
  start <- Sys.time()
  
  # make sure population is ordered?
  population$rowIdPython <- population$rowId-1 # -1 to account for python/r index difference
  #PythonInR::pySet('population', as.matrix(population[,c('rowIdPython','outcomeCount','indexes')]) )
  pPopulation <- as.matrix(population[,c('rowIdPython','outcomeCount','indexes')])
  
  # set seed
  if(param$seed[1] == 'NULL'){
    pSeed <- as.integer(sample(100000000,1))
  }
  if(param$seed[1]!='NULL'){
    pSeed <- as.integer(param$seed[1])
  }
  
  # convert plpData in coo to python:
  ParallelLogger::logTrace('Mapping R data to python')
  #x <- toSparsePython2(plpData,population, map=NULL)
  prediction <- population
  x <- PatientLevelPrediction::toSparseM(plpData,population,map=NULL, temporal = F)

  reticulate::source_python(system.file(package='PatientLevelPrediction','python','randomForestFunctions.py'), envir = e)
  data <- reticulate::r_to_py(x$data)
  
  #do var imp
  if(param$varImp[1]==T){
    
    # python checked in .set 
    varImp <- rf_var_imp(population = pPopulation,
                             plpData = data, 
                             quiet=pQuiet)
    
    if(!quiet)
      ParallelLogger::logTrace('Variable importance completed')  
    if(mean(varImp)==0)
      stop('No important variables - seems to be an issue with the data')
    
    incRInd <- which(varImp>mean(varImp), arr.ind=T)
    
    # save mapping, missing, indexes
  } else{
    incRInd <- 1:nrow(covariateRef) 
  }
  
  # save the model to outLoc
  outLoc <- createTempModelLoc()
  # clear the existing model pickles
  for(file in dir(outLoc))
    file.remove(file.path(outLoc,file))
  
  # run rf_plp for each grid search:
  all_auc <- c()

  for(i in 1:nrow(param)){
    
    # then run standard python code
    pred <- train_rf(population=pPopulation, 
                         plpData = data,
                         ntrees = as.integer(param$ntree[i]),
                         max_depth = as.integer(param$maxDepth[i]),
                         mtry = param$mtries[i],
                         included = as.matrix(incRInd-1),
                         seed = pSeed,
                         quiet='False')

    colnames(pred) <- c('rowId','outcomeCount','indexes', 'value')
    pred <- as.data.frame(pred)
    attr(pred, "metaData") <- list(predictionType="binary")
    
    auc <- PatientLevelPrediction::computeAuc(pred)
    all_auc <- c(all_auc, auc)
    if(!quiet)
      ParallelLogger::logInfo(paste0('Model with settings: ntrees:',param$ntrees[i],' maxDepth: ',param$maxDepth[i], 
                                  'mtry: ', param$mtry[i] , ' obtained AUC of ', auc))
  }
  
  hyperSummary <- cbind(param, cv_auc=all_auc)
  
  # now train the final model for the best hyper-parameters previously found
  #reticulate::source_python(system.file(package='PatientLevelPrediction','python','finalRandomForest.py'), envir = e)
  result <- final_rf(population=pPopulation, 
                         plpData = data,
                         ntrees = as.integer(param$ntree[which.max(all_auc)]),
                         max_depth = as.integer(param$maxDepth[which.max(all_auc)]),
                         mtry = as.integer(param$mtries[which.max(all_auc)]),
                         included = as.matrix(incRInd-1),
                         modelOutput = outLoc,
                         seed = pSeed,
                         quiet='False')
  
  pred <- result[[1]]
  varImp <- result[[2]]
  
  modelTrained <- file.path(outLoc) # location 
  param.best <- param[which.max(all_auc),]

  variableImportance <- rep(0, nrow(covariateRef))
  variableImportance[incRInd] <- varImp
  incs <- rep(0, nrow(covariateRef))
  incs[incRInd] <- 1
  covariateRef <- cbind(covariateRef, incs, variableImportance)
  colnames(covariateRef) <- c('covariateId','covariateName','analysisId','conceptId','included','covariateValue')
  
  comp <- start-Sys.time()
  
  pred[,1] <- pred[,1] + 1 # converting from python to r index
  colnames(pred) <- c('rowId','outcomeCount','indexes', 'value')
  pred <- as.data.frame(pred)
  attr(pred, "metaData") <- list(predictionType="binary")
  prediction <- merge(prediction, pred[,c('rowId', 'value')], by='rowId')
  
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
                 covariateMap=x$map,
                 predictionTrain = prediction
  )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'pythonReticulate'
  attr(result, 'predictionType') <- 'binary'

  return(result)
}
