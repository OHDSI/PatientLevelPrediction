# @file CNNTorch.R
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
  
  ParallelLogger::logWarn('This model has broken - please use setCNN() or setCNN2() instead ')
  
  # set seed
  if(is.null(seed[1])){
    seed <- as.integer(sample(100000000,1))
  }
  
  result <- list(model='fitCNNTorch', param=split(expand.grid(nbfilters=nbfilters,
                                            epochs=epochs, seed=seed[1], 
											class_weight = class_weight, type = type),
											1:(length(nbfilters)*length(epochs)) ),
                 					  name='CNN Torch')
  
  class(result) <- 'modelSettings' 
  
  return(result)
}


fitCNNTorch <- function(population, plpData, param, search='grid', quiet=F,
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
  hyperParamSel <- lapply(param, function(x) do.call(trainCNNTorch, listAppend(x, 
                                                                                list(plpData = result$data,
                                                                                     population = pPopulation,
                                                                                     train=TRUE,
                                                                                     modelOutput=outLoc))  ))
  
  hyperSummary <- cbind(do.call(rbind, param), unlist(hyperParamSel))
  
  #now train the final model and return coef
  bestInd <- which.max(abs(unlist(hyperParamSel)-0.5))[1]
  finalModel <- do.call(trainCNNTorch, listAppend(param[[bestInd]], 
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
  
  # train prediction
  pred <- as.matrix(finalModel)
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
  attr(result, 'type') <- 'pythonReticulate'
  attr(result, 'predictionType') <- 'binary'
  
  return(result)
}


trainCNNTorch <- function(plpData, population, epochs=50, nbfilters = 16, seed=0, class_weight= 0, type = 'CNN', train=TRUE, modelOutput, quiet=F){
  
  train_deeptorch <- function(){return(NULL)}
  
  python_dir <- system.file(package='PatientLevelPrediction','python')
  e <- environment()
  reticulate::source_python(system.file(package='PatientLevelPrediction','python','deepTorchFunctions.py'), envir = e)
  
  
  result <- train_deeptorch(population = population, 
                            plpData = plpData, 
                            epochs = as.integer(epochs),
                            nbfilters = as.integer(nbfilters),
                            seed = as.integer(seed), 
                            class_weight = as.double(class_weight),
                            model_type = as.character(type),
                            train = train,
                            modelOutput = modelOutput,
                            quiet = quiet
                            )
  
  if(train){
    # then get the prediction 
    pred <- as.matrix(result)
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
