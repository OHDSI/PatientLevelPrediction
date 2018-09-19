# @file DecisionTree.R
#
# Copyright 2018 Observational Health Data Sciences and Informatics
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
#' @param maxDepth    The maximum depth of the tree
#' @param minSamplesSplit    The minimum samples per split
#' @param minSamplesLeaf    The minimum number of samples per leaf
#' @param minImpurityDecrease  Threshold for early stopping in tree growth. A node will split if its impurity is above the threshold, otherwise it is a leaf. 
#' @param classWeight        Balance or None
#' @param seed                The random state seed
#' @param plot                Boolean whether to plot the tree (requires python pydotplus module)
#'
#' @examples
#' \dontrun{
#' model.decisionTree <- setDecisionTree(maxDepth=10,minSamplesLeaf=10, seed=NULL )
#' }
#' @export
setDecisionTree <- function(maxDepth=10 ,minSamplesSplit=2 ,minSamplesLeaf=10,
                            minImpurityDecrease=10^-7,seed =NULL, classWeight='None', 
                            plot=F  ){
  if(!class(seed)%in%c('numeric','NULL', 'integer'))
    stop('Invalid seed')
  if(!class(maxDepth) %in% c("numeric", "integer"))
    stop('maxDepth must be a numeric value >0 ')
  if(min(maxDepth) < 1)
    stop('maxDepth must be greater that 0 or -1')
  if(!class(minSamplesSplit) %in% c("numeric", "integer") )
    stop('minSamplesSplit must be a numeric value >1')
  if(min(minSamplesSplit) < 2)
    stop('minSamplesSplit must be greater that 1')
  if(!class(minSamplesLeaf) %in% c("numeric", "integer"))
    stop('minSamplesLeaf must be a numeric value >0')
  if(min(minSamplesLeaf) < 1)
    stop('minSamplesLeaf must be greater that 0')
  if(class(minImpurityDecrease)!='numeric')
    stop('minImpurityDecrease must be a numeric value >0 ')
  if(min(minImpurityDecrease) <= 0)
    stop('minImpurityDecrease must be greater that 0')
  if(class(classWeight) !='character')
    stop('classWeight must be a character of either None or balanced')
  if(!classWeight%in%c('None','balanced'))
    stop('classWeight must be a character of either None or balanced')
  if(class(plot) !='logical')
    stop('Plot must be logical')

  # test python is available and the required dependancies are there:
  checkPython()
  
  result <- list(model='fitDecisionTree', 
                 param= split(expand.grid(maxDepth=maxDepth, 
                                          minSamplesSplit=minSamplesSplit,
                                          minSamplesLeaf=minSamplesLeaf,
                                          minImpurityDecrease=minImpurityDecrease,
                                          classWeight=classWeight,
                                          seed=ifelse(is.null(seed),'NULL', seed),
                                          plot=plot[1]),
                              1:(length(classWeight)*length(maxDepth)*length(minSamplesSplit)*length(minSamplesLeaf)*length(minImpurityDecrease))  )
                              ,
                 name='DecisionTree')
  class(result) <- 'modelSettings' 
  
  return(result)
}

fitDecisionTree <- function(population, plpData, param, search='grid', quiet=F,
                        outcomeId, cohortId , ...){
  
  # check plpData is libsvm format or convert if needed
  if(!'ffdf'%in%class(plpData$covariates))
    stop('Needs plpData')
  
  if(colnames(population)[ncol(population)]!='indexes'){
    warning('indexes column not present as last column - setting all index to 1')
    population$indexes <- rep(1, nrow(population))
  }
  
  # connect to python if not connected
  initiatePython()
  
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
  covariateRef$covariateValue <- unlist(varImp)
  
  
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
                 dense=0,
                 covariateMap=x$map
  )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'python'
  attr(result, 'predictionType') <- 'binary'
  
  
  return(result)
}


trainDecisionTree <- function(maxDepth=10 ,minSamplesSplit=2 ,minSamplesLeaf=10,
                              minImpurityDecrease=10^-7,classWeight='None',seed =NULL,
                              train=TRUE, plot=F,quiet=F){
  #PythonInR::pySet('size', as.matrix(size) )
  #PythonInR::pySet('alpha', as.matrix(alpha) )
  PythonInR::pyExec(paste0("max_depth = ", maxDepth))
  PythonInR::pyExec(paste0("min_samples_split = ", minSamplesSplit))
  PythonInR::pyExec(paste0("min_samples_leaf = ", minSamplesLeaf))
  PythonInR::pyExec(paste0("min_impurity_decrease = ", minImpurityDecrease))
  ifelse(classWeight=='None', PythonInR::pyExec(paste0("class_weight = ", classWeight)), 
         PythonInR::pyExec(paste0("class_weight = '", classWeight,"'")))
  PythonInR::pyExec(paste0("seed = ", ifelse(is.null(seed),'None',seed)))
  #==== editied
  # set the plotting variable
  PythonInR::pyExec("plot= False")
  if(plot)
    PythonInR::pyExec("plot= True")
  #=========
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