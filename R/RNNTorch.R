# @file RNNTorch.R
#
# Copyright 2020 Observational Health Data Sciences and Informatics
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
#' @param class_weight   The class weight used for imbalanced data: 
#'                           0: Inverse ratio between positives and negatives
#'                          -1: Focal loss
#' @param type      It can be normal 'RNN', 'BiRNN' (bidirectional RNN) and 'GRU'
#'
#' @examples
#' \dontrun{
#' model.rnnTorch <- setRNNTorch()
#' }
#' @export
setRNNTorch <- function(hidden_size=c(50, 100), epochs=c(20, 50), seed=0, class_weight = 0, type = 'RNN'){
  # check inputs
  
  # add warning about dense data..
  
  
  # set seed
  if(is.null(seed[1])){
    seed <- as.integer(sample(100000000,1))
  }
  
  result <- list(model='fitRNNTorch', param=split(expand.grid(hidden_size=hidden_size,
                                            epochs=epochs, seed=seed[1], 
                                            class_weight = class_weight, type = type),
									        1:(length(hidden_size)*length(epochs)) ),
                                      name='RNN Torch')
  
  class(result) <- 'modelSettings' 
  
  return(result)
}

fitRNNTorch <- function(population, plpData, param, search='grid', quiet=F,
                        outcomeId, cohortId, ...){
  
  # check plpData is libsvm format or convert if needed
  if (!FeatureExtraction::isCovariateData(plpData$covariateData))
    stop("Needs correct covariateData")
  
  if(colnames(population)[ncol(population)]!='indexes'){
    warning('indexes column not present as last column - setting all index to 1')
    population$indexes <- rep(1, nrow(population))
  }
  
  start <- Sys.time()
  
  population$rowIdPython <- population$rowId-1  #to account for python/r index difference #subjectId
  pPopulation <- as.matrix(population[,c('rowIdPython','outcomeCount','indexes')])
  
  result <- toSparseTorchPython(plpData,population, map=NULL, temporal=T)
    
  outLoc <- createTempModelLoc()
  # clear the existing model pickles
  for(file in dir(outLoc))
    file.remove(file.path(outLoc,file))
  
  # do cross validation to find hyperParameter
  hyperParamSel <- lapply(param, function(x) do.call(trainRNNTorch, listAppend(x, 
                                                                               list(plpData = result$data,
                                                                                    population = pPopulation,
                                                                                    train=TRUE,
                                                                                    modelOutput=outLoc))  ))
 
  hyperSummary <- cbind(do.call(rbind, param), unlist(hyperParamSel))
  
  #now train the final model and return coef
  bestInd <- which.max(abs(unlist(hyperParamSel)-0.5))[1]
  finalModel <- do.call(trainRNNTorch, listAppend(param[[bestInd]], 
                                                  list(plpData = result$data,
                                                       population = pPopulation,
                                                       train=FALSE,
                                                       modelOutput=outLoc)))

  covariateRef <- as.data.frame(plpData$covariateData$covariateRef)
  incs <- rep(1, nrow(covariateRef)) 
  covariateRef$included <- incs
  covariateRef$covariateValue <- rep(0, nrow(covariateRef))
  
  modelTrained <- file.path(outLoc) 
  param.best <- param[[bestInd]]
  
  comp <- start-Sys.time()
  
  # prediction on train set:
  pred <- finalModel
  pred[,1] <- pred[,1] + 1 # adding one to convert from python to r indexes
  colnames(pred) <- c('rowId','outcomeCount','indexes', 'value')
  pred <- as.data.frame(pred)
  attr(pred, "metaData") <- list(predictionType="binary")
  
  pred$value <- 1-pred$value
  prediction <- merge(population, pred[,c('rowId','value')], by='rowId')
  
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
                 dense=1,
                 covariateMap=result$map, # I think this is need for new data to map the same?
                 predictionTrain = prediction
  )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'pythonReticulate'
  attr(result, 'predictionType') <- 'binary'
  
  return(result)
}


trainRNNTorch <- function(plpData, population, epochs=50, hidden_size = 100, seed=0, class_weight= 0, type = 'RNN', train=TRUE, modelOutput, quiet=F){

  train_deeptorch <- function(){return(NULL)}
  python_dir <- system.file(package='PatientLevelPrediction','python')
  
  e <- environment()
  reticulate::source_python(system.file(package='PatientLevelPrediction','python','deepTorchFunctions.py'), envir = e)
  
  result <- train_deeptorch(population = population, 
                                train = train,
                                plpData = plpData, 
                                model_type = as.character(type),
                                epochs = as.integer(epochs),
                                hidden_size = as.integer(hidden_size),
                                class_weight = class_weight,
                                seed = seed, 
                                quiet = quiet,
                                modelOutput = modelOutput)
  
  if(train){
    # then get the prediction 
    pred <- result
    colnames(pred) <- c('rowId','outcomeCount','indexes', 'value')
    pred <- as.data.frame(pred)
    attr(pred, "metaData") <- list(predictionType="binary")
    
    pred$value <- 1-pred$value
    auc <- computeAuc(pred)
    writeLines(paste0('Model obtained CV AUC of ', auc))
    return(auc)
  }
  
  return(result)
  
}
