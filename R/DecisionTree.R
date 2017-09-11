# @file DecisionTree.R
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

#' Create setting for DecisionTree with python 
#' @param max_depth    The maximum depth of the tree
#' @param min_samples_split    The minimum samples per split
#' @param min_samples_leaf    The minimum number of samples per leaf
#' @param min_impurity_split  Threshold for early stopping in tree growth. A node will split if its impurity is above the threshold, otherwise it is a leaf. 
#' @param class_weight        Balance or None
#' @param seed                The random state seed
#' @param plot                A mysterious parameter
#'
#' @examples
#' \dontrun{
#' model.decisionTree <- setDecisionTree(max_depth=10,min_samples_leaf=10, seed=NULL )
#' }
#' @export
setDecisionTree <- function(max_depth=10 ,min_samples_split=2 ,min_samples_leaf=10,
                            min_impurity_split=10^-7,seed =NULL, class_weight='None', plot=NULL  ){
  if(!class(seed)%in%c('numeric','NULL'))
    stop('Invalid seed')
  if(class(max_depth)!='numeric')
    stop('max_depth must be a numeric value >0 ')
  if(max_depth < 1)
    stop('max_depth must be greater that 0 or -1')
  if(class(min_samples_split)!='numeric')
    stop('min_samples_split must be a numeric value >1')
  if(min_samples_split < 2)
    stop('min_samples_split must be greater that 1')
  if(class(min_samples_leaf)!='numeric')
    stop('min_samples_leaf must be a numeric value >0')
  if(min_samples_leaf < 1)
    stop('min_samples_leaf must be greater that 0')
  if(class(min_impurity_split)!='numeric')
    stop('min_impurity_split must be a numeric value >0 ')
  if(min_impurity_split <= 0)
    stop('min_impurity_split must be greater that 0')
  if(class(class_weight) !='character')
    stop('class_weight must be a character of either None or balanced')
  if(!class_weight%in%c('None','balanced'))
    stop('class_weight must be a character of either None or balanced')
  if(class(plot) !='character')
    stop('Plot must be a character refering to a directory to save the decision tree plot')
  
  # test python is available and the required dependancies are there:
  if (!PythonInR::pyIsConnected()){
    tryCatch({
      python.test <- PythonInR::autodetectPython(pythonExePath = NULL)
    }, error = function(err){
      stop('Python was not found on your system. See the vignette for instructions.')
    }  
    )
  }
  result <- list(model='fitDecisionTree', 
                 param= split(expand.grid(max_depth=max_depth, 
                                          min_samples_split=min_samples_split,
                                          min_samples_leaf=min_samples_leaf,
                                          min_impurity_split=min_impurity_split,
                                          class_weight=class_weight,
                                          seed=ifelse(is.null(seed),'NULL', seed),
                                          plot=ifelse(is.null(plot),'NULL', plot)),
                              1:(length(class_weight)*length(max_depth)*length(min_samples_split)*length(min_samples_leaf)*length(min_impurity_split))  ),
                 name='DecisionTree')
  class(result) <- 'modelSettings' 
  
  return(result)
}

fitDecisionTree <- function(population, plpData, param, search='grid', quiet=F,
                        outcomeId, cohortId, ...){
  
  # check plpData is libsvm format or convert if needed
  if(!'ffdf'%in%class(plpData$covariates))
    stop('Needs plpData')
  
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
    writeLines(paste0('Training decision tree model...' ))
    PythonInR::pyExec('quiet = False')
  }
  start <- Sys.time()
  
  population$rowIdPython <- population$rowId-1 # -1 to account for python/r index difference
  PythonInR::pySet('population', as.matrix(population[,c('rowIdPython','outcomeCount','indexes')]) )
  
  # convert plpData in coo to python:
  x <- toSparsePython(plpData,population, map=NULL)
  
  # save the model to outLoc  TODO: make this an input or temp location?
  outLoc <- file.path(getwd(),'python_models')
  # clear the existing model pickles
  for(file in dir(outLoc))
    file.remove(file.path(outLoc,file))
  
  # run model:
  outLoc <- file.path(getwd(),'python_models')
  PythonInR::pySet("modelOutput",outLoc)
  
  # feed into variable names for tree plot...
  var <- suppressWarnings(ff::as.ram(plpData$covariateRef$covariateName))
  PythonInR::pySet('varnames', as.matrix(as.character(var)  ))
  
  
  # send the column names for the plot:
  ##PythonInR::pySet('variables', as.matrix(ff::as.ram(plpData$covariateRef$covariateName)) )
  
  # do cross validation to find hyperParameter
  # do cross validation to find hyperParameter
  hyperParamSel <- lapply(param, function(x) do.call(trainDecisionTree, c(x, train=TRUE)  ))
  
  hyperSummary <- cbind(do.call(rbind, param), unlist(hyperParamSel))
  
  #now train the final model and return coef
  bestInd <- which.max(abs(unlist(hyperParamSel)-0.5))[1]
  finalModel <- do.call(trainDecisionTree, c(param[[bestInd]], train=FALSE))
  
  #now train the final model and return coef

  # get the coefs and do a basic variable importance:
  varImp <- PythonInR::pyGet('dt.feature_importances_', simplify = F)[,1]
  varImp[is.na(varImp)] <- 0
  #varImp <- PythonInR::pyGet('mlp.coefs_[0]', simplify = F)[,1]
  #varImp[is.na(varImp)] <- 0
  
  covariateRef <- ff::as.ram(plpData$covariateRef)
  incs <- rep(1, nrow(covariateRef))
  covariateRef$included <- incs
  covariateRef$value <- unlist(varImp)
  
  
  # select best model and remove the others  (!!!NEED TO EDIT THIS)
  modelTrained <- file.path(outLoc) 
  param.best <- NULL
  
  comp <- start-Sys.time()
  
  # return model location (!!!NEED TO ADD CV RESULTS HERE)
  result <- list(model = modelTrained,
                 trainCVAuc = hyperParamSel,
                 hyperParamSearch = hyperSummary,
                 modelSettings = list(model='fitDecisionTree',modelParameters=param.best),
                 metaData = plpData$metaData,
                 populationSettings = attr(population, 'metaData'),
                 outcomeId=outcomeId,
                 cohortId=cohortId,
                 varImp = covariateRef,
                 trainingTime =comp,
                 dense=0
  )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'python'
  attr(result, 'predictionType') <- 'binary'
  
  
  return(result)
}


trainDecisionTree <- function(max_depth=10 ,min_samples_split=2 ,min_samples_leaf=10,
                              min_impurity_split=10^-7,class_weight='None',seed =NULL,
                              train=TRUE, plot=getwd(), quiet=F){
  #PythonInR::pySet('size', as.matrix(size) )
  #PythonInR::pySet('alpha', as.matrix(alpha) )
  PythonInR::pyExec(paste0("max_depth = ", max_depth))
  PythonInR::pyExec(paste0("min_samples_split = ", min_samples_split))
  PythonInR::pyExec(paste0("min_samples_leaf = ", min_samples_leaf))
  PythonInR::pyExec(paste0("min_impurity_split = ", min_impurity_split))
  ifelse(class_weight=='None', PythonInR::pyExec(paste0("class_weight = ", class_weight)), 
         PythonInR::pyExec(paste0("class_weight = '", class_weight,"'")))
  PythonInR::pyExec(paste0("seed = ", ifelse(is.null(seed),'None',seed)))
  PythonInR::pyExec(paste0("plot = ", ifelse(is.null(seed),getwd(),plot)))
  if(train)
    PythonInR::pyExec("train = True")
  if(!train)
    PythonInR::pyExec("train = False")
  
  # then run standard python code
  PythonInR::pyExecfile(system.file(package='PatientLevelPrediction','python','decisionTree.py'))
  
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
    if(!quiet)
      writeLines(paste0('Model obtained CV AUC of ', auc))
    return(auc)
  }
  
}