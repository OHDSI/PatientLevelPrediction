# @file GBMSurvival.R
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

#' Create setting for GBM Survival with python
#' #' @description
#' This creates a setting for fitting  GBM surivial model.  You need sksurv python install.  To install this open your command line and type: conda install -c sebp scikit-survival
#' @details
#' Pick the hyper-parameters you want to do a grid search for
#'
#' @param loss           A string specifying the loss function to minimise (default: 'coxph' )
#' @param learningRate   A double specifying the learning rate (controls convergence speed)
#' @param nEstimators    An integer specifying how many trees to build
#' @param criterion      Default: 'friedman_mse'
#' @param minSamplesSplit  An integer specifying min samples per tree split (complexity)
#' @param minSamplesLeaf   An integer specifying min samples per leaf (complexity)
#' @param minWeightFractionLeaf Lookup
#' @param maxDepth      An integer specifying the max depth of trees (complexity)
#' @param minImpuritySplit  A double or NULL specifying the minimum impurity split
#' @param minImpurityDecrease will add
#' @param maxFeatures  will add
#' @param maxLeafNodes will add
#' @param presort will add
#' @param subsample will add
#' @param dropoutRate will add
#' @param seed  will add
#' @param quiet will add
#'
#' @examples
#' \dontrun{
#' gbmSurv <- setGBMSurvival(learningRate=c(0.1,0.01), nEstimators =c(10,50,100),
#'  maxDepth=c(4,10,17), seed = 2)
#' }
#' @export
setGBMSurvival <- function(loss = 'coxph', 
                           learningRate = 0.1,
                           nEstimators =  c(100),
                           criterion = 'friedman_mse',
                           minSamplesSplit= 2,
                           minSamplesLeaf =1 ,
                           minWeightFractionLeaf = 0, 
                           maxDepth = c(3,10,17),
                           minImpuritySplit = NULL,
                           minImpurityDecrease = 0,
                           maxFeatures = NULL,
                           maxLeafNodes = NULL,
                           presort = 'auto',
                           subsample = 1,
                           dropoutRate = 0,
                           seed = NULL,
                           quiet = F) {

  if (!class(seed) %in% c("numeric", "NULL", "integer"))
    stop("Invalid seed")
  if (!class(learningRate) %in% c("numeric", "integer"))
    stop("learningRate must be a numeric value >0 and <=1")
  if (max(learningRate) > 1)
    stop("learningRate must be less that or equal to 1")
  if (min(learningRate) < 0)
    stop("learningRate must be a numeric value >0")
  if (!class(nEstimators) %in% c("numeric", "integer"))
    stop("nEstimators must be a numeric value >0 ")
  if (min(nEstimators) < 1)
    stop("nEstimators must be greater than or equal to 1")
  
  # add check and warn if dependancy not available...
  ParallelLogger::logInfo('To use GBM survival models you need scikit-survival python library.  To set up open the command line and enter: "conda install -c sebp scikit-survival"')
  
  if(is.null(minImpuritySplit)){
    minImpuritySplit <- 'NULL'
  }
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
  
  result <- list(model = "fitGBMSurvival",
                 param = split(expand.grid(nEstimators = nEstimators,
                                           learningRate = learningRate,
                                           loss = loss, criterion = criterion,
                                           minSamplesSplit = minSamplesSplit, 
                                           minSamplesLeaf = minSamplesLeaf,
                                           minWeightFractionLeaf = minWeightFractionLeaf, 
                                           maxDepth = maxDepth,
                                           minImpuritySplit = minImpuritySplit,
                                           minImpurityDecrease = minImpurityDecrease,
                                           maxFeatures = maxFeatures,
                                           maxLeafNodes = maxLeafNodes,
                                           presort = presort,
                                           subsample = subsample,
                                           dropoutRate = dropoutRate ,
                                           seed = seed[1]), 1:(length(nEstimators) * length(learningRate) *
                                                                 length(loss) * length(criterion)*length(minSamplesSplit)*
                                                                 length(minSamplesLeaf) * length(minWeightFractionLeaf)*length(maxDepth)*
                                                                 length(minImpuritySplit) * length(minImpurityDecrease)*length(maxFeatures)*
                                                                 length(maxLeafNodes) * length(presort)*length(subsample)*
                                                                 length(dropoutRate))),
                 name = "GBMSurvival")
  class(result) <- "modelSettings"

  return(result)
}

fitGBMSurvival <- function(population,
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
  population$outcomeBoolean <- population$outcomeCount>0
  pPopulation <- as.matrix(population[,c('rowIdPython','outcomeBoolean','survivalTime','indexes')])
  
  # convert plpData in coo to python:
  x <- toSparseM(plpData, population, map = NULL)
  data <- reticulate::r_to_py(x$data)
  
  # save the model to outLoc TODO: make this an input or temp location?
  outLoc <- createTempModelLoc()
  # clear the existing model pickles
  for(file in dir(outLoc))
    file.remove(file.path(outLoc,file))

  # do cross validation to find hyperParameter
  hyperParamSel <- lapply(param, function(x) do.call(trainGBMSurvival, listAppend(x, 
                                                                      list(train = TRUE, 
                                                                      population=pPopulation, 
                                                                      plpData=data,
                                                                      quiet=quiet))))
  hyperSummary <- cbind(do.call(rbind, param), unlist(hyperParamSel))

  writeLines('Training Final')
  # now train the final model and return coef
  bestInd <- which.max(abs(unlist(hyperParamSel) - 0.5))[1]
  finalModel <- do.call(trainGBMSurvival, listAppend(param[[bestInd]], 
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
  colnames(pred) <- c('rowId','outcomeBoolean','survivalTime','indexes', 'value')
  pred <- as.data.frame(pred)
  attr(pred, "metaData") <- list(predictionType="binary")
  prediction <- merge(population, pred[,c('rowId', 'value')], by='rowId')
  # scale the value
  prediction$value <- prediction$value - min(prediction$value)
  prediction$value <- prediction$value/max(prediction$value)
  
  # return model location (!!!NEED TO ADD CV RESULTS HERE)
  result <- list(model = modelTrained,
                 trainCVAuc = hyperParamSel,
                 hyperParamSearch = hyperSummary,
                 modelSettings = list(model = "fitGBMSurvival", modelParameters = param.best),
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
  attr(result, "type") <- "pythonSurvival"
  attr(result, "predictionType") <- "binary"


  return(result)
}


trainGBMSurvival <- function(population, plpData, seed = NULL, train = TRUE, 
                             modelLocation=NULL, quiet=FALSE,
                             loss = 'coxph', 
                             learningRate = 0.1,
                             nEstimators =  100,
                             criterion = 'friedman_mse',
                             minSamplesSplit= 2,
                             minSamplesLeaf =1 ,
                             minWeightFractionLeaf = 0, 
                             maxDepth = 4,
                             minImpuritySplit = NULL,
                             minImpurityDecrease = 0,
                             maxFeatures = NULL,
                             maxLeafNodes = NULL,
                             presort = 'auto',
                             subsample = 1,
                             dropoutRate = 0) {

  e <- environment()
  # then run standard python code
  reticulate::source_python(system.file(package='PatientLevelPrediction','python','gbmSurvivalFunctions.py'), envir = e)
  
  if(minImpuritySplit=='NULL'){
    minImpuritySplit <- NULL
  }
  if(maxFeatures=='NULL'){
    maxFeatures <- NULL
  }
  if(maxLeafNodes=='NULL'){
    maxLeafNodes <- NULL
  }

  result <- train_gbmsurv(population=population, 
                           plpData=plpData, 
                           train = train,
                           modelOutput = modelLocation,
                           seed = as.integer(seed), 
                           quiet = quiet,
                          n_estimators = as.integer(nEstimators),
                          learning_rate = learningRate,
                          loss = as.character(loss), 
                          criterion = as.character(criterion),
                          min_samples_split = as.integer(minSamplesSplit), 
                          min_samples_leaf = as.integer(minSamplesLeaf),
                          min_weight_fraction_leaf = minWeightFractionLeaf, 
                          max_depth = as.integer(maxDepth),
                          min_impurity_split = minImpuritySplit,
                          min_impurity_decrease = minImpurityDecrease,
                          max_features = maxFeatures,
                          max_leaf_nodes = maxLeafNodes,
                          presort = as.character(presort),
                          subsample = subsample,
                          dropout_rate = dropoutRate)
  
  if (train) {
    # then get the prediction
    pred <- result
    colnames(pred) <- c("rowId", "outcomeBoolean","survivalTime", "indexes", "value")
    pred <- as.data.frame(pred)
    pred$outcomeCount <- rep(0, nrow(pred))
    pred$outcomeCount[pred$outcomeBoolean==T ] <- 1
    attr(pred, "metaData") <- list(predictionType = "binary")

    auc <- PatientLevelPrediction::computeAuc(pred)
    writeLines(paste0("CV model obtained CV AUC of ", auc))
    return(auc)
  }

  return(result)
}



predict.pythonSurvival <- function(plpModel, population, plpData){
  
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
  result <- python_predict_survival(population = pPopulation, 
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
  
  # scale the value 
  prediction$value <- prediction$value - min(prediction$value)
  prediction$value <- prediction$value/max(prediction$value)
  
  # add subjectId and date:
  prediction <- merge(prediction,
                      population[,c('rowId','subjectId','cohortStartDate')], 
                      by='rowId')
  return(prediction)
}