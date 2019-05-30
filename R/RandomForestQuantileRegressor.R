# @file RandomForestQuantileRegressor.R
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

#' Create setting for RandomForestQuantileRegressor with python scikit-garden (skgarden.quantile.RandomForestQuantileRegressor)
#' #' @description
#' This creates a setting for fitting a RandomForestQuantileRegressor model.  You need skgarden python install.  To install this open your command line and type: conda install -c conda-forge scikit-garden 
#' @details
#' Pick the hyper-parameters you want to do a grid search for
#'
#' @param nEstimators    (int default:100) The number of trees in the forest.
#' @param criterion      (string default="mse")) The function to measure the quality of a split. Supported criteria are "mse" for the mean squared error, which is equal to variance reduction as feature selection criterion, and "mae" for the mean absolute error.
#' @param maxFeatures    (int default: -1) The number of features to consider when looking for the best split.  If -1 then use sqrt of total number of features.
#' @param maxDepth       (int default:4) The maximum depth of the tree. If None, then nodes are expanded until all leaves are pure or until all leaves contain less than minSamplesSplit samples.
#' @param minSamplesSplit  An integer specifying min samples per tree split (complexity)
#' @param minSamplesLeaf   An integer specifying min samples per leaf (complexity)
#' @param minWeightFractionLeaf Lookup
#' @param maxLeafNodes (int) Grow trees with maxLeafNodes in best-first fashion. Best nodes are defined as relative reduction in impurity. If None then unlimited number of leaf nodes.
#' @param bootstrap (boolean default:TRUE) Whether bootstrap samples are used when building trees.
#' @param oobScore (boolean default:FALSE) Whether to use out-of-bag samples to estimate the R^2 on unseen data.
#' @param warmStart (boolean default:FALSE) When set to True, reuse the solution of the previous call to fit and add more estimators to the ensemble, otherwise, just fit a whole new forest.
#' @param seed  will add
#' @param quiet will add
#'
#' @examples
#' \dontrun{
#' rfQR <- setRandomForestQuantileRegressor(nEstimators =c(10,50,100),
#'  maxDepth=c(4,10,17), seed = 2)
#' }
#' @export
setRandomForestQuantileRegressor <- function(
  nEstimators =  c(100),
  criterion = 'mse',
  maxFeatures = -1,
  maxDepth = 4,
  minSamplesSplit= 2,
  minSamplesLeaf =1 ,
  minWeightFractionLeaf = 0, 
  maxLeafNodes = NULL,
  bootstrap = TRUE,
  oobScore = FALSE,
  warmStart = FALSE,
  seed = NULL,
  quiet = F) {
  
  if (!class(seed) %in% c("numeric", "NULL", "integer"))
    stop("Invalid seed")
  if (!class(nEstimators) %in% c("numeric", "integer"))
    stop("nEstimators must be a numeric value >0 ")
  if (min(nEstimators) < 1)
    stop("nEstimators must be greater than or equal to 1")
  
  # add check and warn if dependancy not available...
  ParallelLogger::logInfo('To use RandomForestQuantileRegressorl models you need scikit-garden python library.  To set up open the command line and enter: "conda install -c conda-forge scikit-garden"')
  
  if(is.null(maxFeatures)){
    maxFeatures <- 'NULL'
  }
  if(is.null(maxLeafNodes)){
    maxLeafNodes <- 'NULL'
  }
  
  # set seed
  if(is.null(seed[1])){
    seed <- as.integer(sample(100000000,1))
  }
  
  result <- list(model = "fitRandomForestQuantileRegressor",
                 param = split(expand.grid(nEstimators =  nEstimators,
                                           criterion = criterion,
                                           maxFeatures = maxFeatures,
                                           maxDepth = maxDepth,
                                           minSamplesSplit= minSamplesSplit,
                                           minSamplesLeaf = minSamplesLeaf ,
                                           minWeightFractionLeaf = minWeightFractionLeaf, 
                                           maxLeafNodes = maxLeafNodes,
                                           bootstrap = bootstrap,
                                           oobScore = oobScore,
                                           warmStart = warmStart,
                                           seed = seed[1]), 1:(length(nEstimators) * length(criterion) *
                                                                 length(maxFeatures) * length(maxDepth)*length(minSamplesSplit)*
                                                                 length(minSamplesLeaf) * length(minWeightFractionLeaf)*length(maxLeafNodes)*
                                                                 length(bootstrap) * length(oobScore)*length(warmStart))),
                 name = "RandomForestQuantileRegressor")
  class(result) <- "modelSettings"
  
  return(result)
}

fitRandomForestQuantileRegressor <- function(population,
                           plpData,
                           param,
                           search = "grid",
                           quiet = F,
                           outcomeId,
                           cohortId,
                           ...) {
  
  # check plpData is libsvm format or convert if needed
  if (!"ffdf" %in% class(plpData$covariates))
    stop("Needs plpData")
  
  if (colnames(population)[ncol(population)] != "indexes") {
    warning("indexes column not present as last column - setting all index to 1")
    population$indexes <- rep(1, nrow(population))
  }
  
  start <- Sys.time()
  
  population$rowIdPython <- population$rowId - 1  # -1 to account for python/r index difference
  pPopulation <- as.matrix(population[,c('rowIdPython','outcomeCount','indexes')])
  
  # convert plpData in coo to python:
  x <- toSparseM(plpData, population, map = NULL)
  data <- reticulate::r_to_py(x$data)
  
  # save the model to outLoc TODO: make this an input or temp location?
  outLoc <- createTempModelLoc()
  # clear the existing model pickles
  for(file in dir(outLoc))
    file.remove(file.path(outLoc,file))
  
  # do cross validation to find hyperParameter
  hyperParamSel <- lapply(param, function(x) do.call(trainRandomForestQuantileRegressor, listAppend(x, 
                                                                                  list(train = TRUE, 
                                                                                       population=pPopulation, 
                                                                                       plpData=data,
                                                                                       quiet=quiet))))
  hyperSummary <- cbind(do.call(rbind, param), unlist(hyperParamSel))
  
  writeLines('Training Final')
  # now train the final model and return coef
  bestInd <- which.max(abs(unlist(hyperParamSel) - 0.5))[1]
  finalModel <- do.call(trainRandomForestQuantileRegressor, listAppend(param[[bestInd]], 
                                                     list(train = FALSE, 
                                                          modelLocation=outLoc, 
                                                          population=pPopulation, 
                                                          plpData=data,
                                                          quiet=quiet)))
  
  # get the coefs and do a basic variable importance:
  varImp <- finalModel[[2]]
  varImp[is.na(varImp)] <- 0
  
  covariateRef <- ff::as.ram(plpData$covariateRef)
  incs <- rep(1, nrow(covariateRef))
  covariateRef$included <- incs
  covariateRef$covariateValue <- unlist(varImp)
  
  
  # select best model and remove the others (!!!NEED TO EDIT THIS)
  modelTrained <- file.path(outLoc)
  param.best <- param[[bestInd]]
  
  comp <- start - Sys.time()
  
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
                 modelSettings = list(model = "fitRandomForestQuantileRegressor", modelParameters = param.best),
                 metaData = plpData$metaData,
                 populationSettings = attr(population, "metaData"),
                 outcomeId = outcomeId,
                 cohortId = cohortId,
                 varImp = covariateRef,
                 trainingTime = comp,
                 dense = 0,
                 covariateMap = x$map,
                 predictionTrain=prediction)
  class(result) <- "plpModel"
  attr(result, "type") <- "pythonGarden"
  attr(result, "predictionType") <- "binary"
  
  
  return(result)
}


trainRandomForestQuantileRegressor <- function(population, plpData, seed = NULL, train = TRUE, 
                             modelLocation=NULL, quiet=FALSE,
                             nEstimators =  100,
                             criterion = 'mse',
                             maxFeatures = -1,
                             maxDepth = 4,
                             minSamplesSplit= 2,
                             minSamplesLeaf =1 ,
                             minWeightFractionLeaf = 0, 
                             maxLeafNodes = NULL,
                             bootstrap = TRUE,
                             oobScore = FALSE,
                             warmStart = FALSE) {
  
  e <- environment()
  # then run standard python code
  reticulate::source_python(system.file(package='PatientLevelPrediction','python','gardenFunctions.py'), envir = e)
  
  if(maxFeatures=='NULL'){
    maxFeatures <- NULL
  }
  if(maxLeafNodes=='NULL'){
    maxLeafNodes <- NULL
  }
  if(maxFeatures == -1){
    maxFeatures = 'sqrt'
  }
  
  result <- train_RandomForestQuantileRegressor(population=population, 
                          plpData=plpData, 
                          train = train,
                          modelOutput = modelLocation,
                          seed = as.integer(seed), 
                          quiet = quiet,
                          n_estimators = as.integer(nEstimators),
                          criterion = as.character(criterion),
                          max_features = maxFeatures,
                          max_depth = as.integer(maxDepth),
                          min_samples_split = as.integer(minSamplesSplit), 
                          min_samples_leaf = as.integer(minSamplesLeaf),
                          min_weight_fraction_leaf = minWeightFractionLeaf, 
                          max_leaf_nodes = maxLeafNodes,
                          bootstrap =  bootstrap,
                          oob_score = oobScore,
                          warm_start = warmStart)
  
  if (train) {
    # then get the prediction
    pred <- result
    colnames(pred) <- c("rowId", "outcomeCount", "indexes", "value")
    pred <- as.data.frame(pred)
    attr(pred, "metaData") <- list(predictionType = "binary")
    
    auc <- PatientLevelPrediction::computeAuc(pred)
    writeLines(paste0("CV model obtained CV AUC of ", auc))
    return(auc)
  }
  
  return(result)
}



predict.pythonGarden <- function(plpModel, population, plpData){
  
  e <- environment()
  reticulate::source_python(system.file(package='PatientLevelPrediction','python','predictFunctions.py'), envir = e)
  
  
  ParallelLogger::logInfo('Mapping covariates...')
  newData <- toSparseM(plpData, population, map=plpModel$covariateMap)
  pdata <- reticulate::r_to_py(newData$data)
  
  # save population
  if('indexes'%in%colnames(population)){
    population$rowIdPython <- population$rowId-1 # -1 to account for python/r index difference
    pPopulation <- as.matrix(population[,c('rowIdPython','outcomeCount','indexes')])
  } else {
    population$rowIdPython <- population$rowId-1 # -1 to account for python/r index difference
    pPopulation <- as.matrix(population[,c('rowIdPython','outcomeCount')])
  }
  
  # run the python predict code:
  ParallelLogger::logInfo('Executing prediction...')
  result <- python_predict_garden(population = pPopulation, 
                                    plpData = pdata, 
                                    model_loc = plpModel$model)
  
  #get the prediction from python and reformat:
  ParallelLogger::logInfo('Returning results...')
  prediction <- result
  prediction <- as.data.frame(prediction)
  attr(prediction, "metaData") <- list(predictionType="binary")
  if(ncol(prediction)==4){
    colnames(prediction) <- c('rowId','outcomeCount','indexes', 'value')
  } else {
    colnames(prediction) <- c('rowId','outcomeCount', 'value')
  }
  
  # add 1 to rowId from python:
  prediction$rowId <- prediction$rowId+1
  
  # add subjectId and date:
  prediction <- merge(prediction,
                      population[,c('rowId','subjectId','cohortStartDate')], 
                      by='rowId')
  return(prediction)
}