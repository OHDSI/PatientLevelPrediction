# @file DecisionTree.R
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
  if(sum(!classWeight%in%c('None','balanced'))!=0)
    stop('classWeight must be a character of either None or balanced')
  if(class(plot) !='logical')
    stop('Plot must be logical')
  
  # test python is available and the required dependancies are there:
  ##checkPython()
  
  # set seed
  if(is.null(seed[1])){
    seed <- as.integer(sample(100000000,1))
  }
  
  result <- list(model='fitDecisionTree', 
                 param= split(expand.grid(maxDepth=maxDepth, 
                                          minSamplesSplit=minSamplesSplit,
                                          minSamplesLeaf=minSamplesLeaf,
                                          minImpurityDecrease=minImpurityDecrease,
                                          classWeight=classWeight,
                                          seed=seed[1],
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
  ##initiatePython()
  
  if(quiet==F){
    writeLines(paste0('Training decision tree model...' ))
  }
  start <- Sys.time()
  
  population$rowIdPython <- population$rowId-1 # -1 to account for python/r index difference
  pPopulation <- as.matrix(population[,c('rowIdPython','outcomeCount','indexes')])
  
  # convert plpData in coo to python:
  x <- toSparseM(plpData,population, map=NULL)
  
  # save the model to outLoc
  outLoc <- createTempModelLoc()
  # clear the existing model pickles
  for(file in dir(outLoc))
    file.remove(file.path(outLoc,file))
  
  pydata <- reticulate::r_to_py(x$data)
  
  # feed into variable names for tree plot...
  var <- suppressWarnings(ff::as.ram(plpData$covariateRef$covariateName))
  
  hyperParamSel <- lapply(param, function(x) do.call(trainDecisionTree, 
                                                     listAppend(x, list(train=TRUE,
                                                                        population = pPopulation, 
                                                                        plpData = pydata, 
                                                                        quiet=quiet, 
                                                                        var=var,
                                                                        modelOutput=outLoc))  ))
  
  hyperSummary <- cbind(do.call(rbind, param), unlist(hyperParamSel))
  
  #now train the final model and return coef
  bestInd <- which.max(abs(unlist(hyperParamSel)-0.5))[1]
  finalModel <- do.call(trainDecisionTree, listAppend(param[[bestInd]], list(train=FALSE,
                                                                             population = pPopulation, 
                                                                             plpData = pydata, 
                                                                             quiet=quiet, 
                                                                             var=var,
                                                                             modelOutput=outLoc)))
  
  #now train the final model and return coef
  
  # get the coefs and do a basic variable importance:
  varImp <- finalModel[[2]]
  varImp[is.na(varImp)] <- 0
  
  covariateRef <- ff::as.ram(plpData$covariateRef)
  incs <- rep(1, nrow(covariateRef))
  covariateRef$included <- incs
  covariateRef$covariateValue <- varImp
  
  
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
                 modelSettings = list(model='fitDecisionTree',modelParameters=param.best),
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


trainDecisionTree <- function(population, plpData, 
                              maxDepth=10 ,minSamplesSplit=2 ,minSamplesLeaf=10,
                              minImpurityDecrease=10^-7,classWeight='None',
                              seed =NULL,
                              train=TRUE, plot=F,quiet=F, var, modelOutput){
  
  e <- environment()
  reticulate::source_python(system.file(package='PatientLevelPrediction','python','decisionTreeFunctions.py'), envir = e)
  
  result <- train_decision_tree(population = population, 
                                train = train,
                                plpData = plpData, 
                                plot = plot, 
                                max_depth = as.integer(maxDepth), 
                                min_samples_split = as.integer(minSamplesSplit), 
                                min_samples_leaf = as.integer(minSamplesLeaf), 
                                min_impurity_decrease = minImpurityDecrease, 
                                class_weight = as.character(classWeight), 
                                seed = as.integer(seed), 
                                quiet = quiet,
                                varNames = var, 
                                modelOutput = modelOutput)
  
  
  if(train){
    # then get the prediction 
    pred <- result
    colnames(pred) <- c('rowId','outcomeCount','indexes', 'value')
    pred <- as.data.frame(pred)
    attr(pred, "metaData") <- list(predictionType="binary")
    
    pred$value <- 1-pred$value
    auc <- PatientLevelPrediction::computeAuc(pred)
    if(!quiet)
      writeLines(paste0('Model obtained CV AUC of ', auc))
    return(auc)
  }
  
  return(result)
}
