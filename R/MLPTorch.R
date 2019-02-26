# @file MLPTorch.R
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

#' Create setting for neural network model with python 
#' @param size           The number of hidden nodes
#' @param w_decay        The l2 regularisation
#' @param seed           A seed for the model
#' @param epochs         The number of epochs 
#' @param class_weight   The class weight used for imbalanced data: 
#'                           0: Inverse ratio between positives and negatives
#'                          -1: Focal loss
#' @param mlp_type        The type of multiple layer network, inlcuding MLP and SNN (self-normalizing neural network) 
#' @param autoencoder     First learn stakced autoencoder for input features, then train MLP on the encoded features.
#' @param vae     First learn stakced varational autoencoder for input features, then train MLP on the encoded features.
#' @examples
#' \dontrun{
#' model.mlpTorch <- setMLPTorch()
#' }
#' @export
setMLPTorch <- function(size=c(500, 1000), w_decay=c(0.0005, 0.005), 
                        epochs=c(20, 50), seed=0, class_weight = 0, mlp_type = 'MLP', autoencoder = FALSE, vae = FALSE){
  
  # set seed
  if(is.null(seed[1])){
    seed <- as.integer(sample(100000000,1))
  }
  
  result <- list(model='fitMLPTorch', param=split(expand.grid(size=size, w_decay=w_decay,
                                            epochs=epochs, seed=seed[1], 
											class_weight = class_weight, mlp_type = mlp_type, autoencoder = autoencoder, vae = vae),
				                            1:(length(size)*length(w_decay)*length(epochs)) ),
                                      name='MLP Torch')
  
  class(result) <- 'modelSettings' 
  
  return(result)
}

fitMLPTorch <- function(population, plpData, param, search='grid', quiet=F,
                   outcomeId, cohortId, ...){
  
  # check plpData is libsvm format or convert if needed
  if(!'ffdf'%in%class(plpData$covariates))
    stop('Needs plpData')
  
  # check population has indexes column, which is used to split training with different folds and testing set
  if(colnames(population)[ncol(population)]!='indexes'){
    warning('indexes column not present as last column - setting all index to 1')
    population$indexes <- rep(1, nrow(population))
  }
  
  start <- Sys.time()
  
  population$rowIdPython <- population$rowId-1 # -1 to account for python/r index difference
  pPopulation <-  as.matrix(population[,c('rowIdPython','outcomeCount','indexes')])
  
  # convert plpData in coo to python:
  x <- toSparseM(plpData,population, map=NULL)
  #x <- toSparseTorchPython2(plpData,population, map=NULL, temporal=F)
  
  outLoc <- createTempModelLoc()
  # clear the existing model pickles
  for(file in dir(outLoc))
    file.remove(file.path(outLoc,file))
  
  pydata <- reticulate::r_to_py(x$data)

  #do cross validation to find hyperParameter
  hyperParamSel <- lapply(param, function(x) do.call(trainMLPTorch, 
                                                     listAppend(x, list(plpData=pydata,
                                                                        population = pPopulation,
                                                                        train=TRUE,
                                                                        modelOutput=outLoc,
                                                                        quiet = quiet))  ))

  
  hyperSummary <- cbind(do.call(rbind, param), unlist(hyperParamSel))
  
  #now train the final model
  bestInd <- which.max(abs(unlist(hyperParamSel)-0.5))[1]
  finalModel <- do.call(trainMLPTorch, listAppend(param[[bestInd]], 
                                                   list(plpData=pydata,
                                                             population = pPopulation,
                                                             train=FALSE,
                                                             modelOutput=outLoc,
                                                             quiet = quiet)))

  covariateRef <- ff::as.ram(plpData$covariateRef)
  incs <- rep(1, nrow(covariateRef)) 
  covariateRef$included <- incs
  covariateRef$covariateValue <- rep(0, nrow(covariateRef))
  
  modelTrained <- file.path(outLoc) 
  param.best <- param[[bestInd]]
  
  comp <- start-Sys.time()
  
  # train prediction
  pred <- finalModel
  pred[,1] <- pred[,1] + 1 # converting from python to r index
  colnames(pred) <- c('rowId','outcomeCount','indexes', 'value')
  pred <- as.data.frame(pred)
  attr(pred, "metaData") <- list(predictionType="binary")
  prediction <- merge(population, pred[,c('rowId', 'value')], by='rowId')
  
  
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
                 dense=1,
                 covariateMap=x$map, # I think this is need for new data to map the same?
                 predictionTrain = prediction
  )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'pythonReticulate'
  attr(result, 'predictionType') <- 'binary'

  return(result)
}


trainMLPTorch <- function(population, plpData, modelOutput, size=200, epochs=100, w_decay = 0.001, seed=0, class_weight = 0, train=TRUE, 
                          mlp_type = 'MLP', autoencoder = FALSE, vae = FALSE, quiet= FALSE){

  e <- environment()
  reticulate::source_python(system.file(package='PatientLevelPrediction','python','deepTorchFunctions.py'), envir = e)
  
  result <- train_deeptorch(population = population, 
                            train = train,
                            plpData = plpData, 
                            model_type = as.character(mlp_type),
                            autoencoder = autoencoder,
                            vae = vae,
                            epochs = as.integer(epochs),
                            size = as.integer(size),
                            w_decay = w_decay,
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
    auc <- PatientLevelPrediction::computeAuc(pred)
    writeLines(paste0('Model obtained CV AUC of ', auc))
    return(auc)
  }
  
  return(result)
  
}
