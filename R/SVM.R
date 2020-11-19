# @file SVM.R
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

#' Create setting for SVM with python
#' @param kernal        Specifies the kernel type to be used in the algorithm. one of ‘linear’, ‘poly’, ‘rbf’, ‘sigmoid’, ‘precomputed’. If none is given ‘rbf’ will be used.
#' @param C             penalty parameter C of the error term.        
#' @param degree        degree of kernel function is significant only in poly, rbf, sigmoid
#' @param gamma         kernel coefficient for rbf and poly, by default 1/n_features will be taken.
#' @param shrinking     wether to use the shrinking heuristic.
#' @param coef0         independent term in kernel function. It is only significant in poly/sigmoid.
#' @param classWeight   Class weight based on imbalance either 'balanced' or 'none'
#' @param varImp        Whether to calculate the variable importance using PFI
#' @param seed           A seed for the model
#'
#' @examples
#' \dontrun{
#' model.svm <- setSVM(kernel='rbf', seed = NULL)
#' }
#' @export
setSVM <- function(kernel='rbf', C=c(1,0.9,2,0.1), degree=c(1,3,5), 
                   gamma=c(1e-04, 3e-05, 0.001, 0.01,0.25),
                   shrinking = T, coef0=0.0,
                   classWeight = 'balanced', varImp = F, seed = NULL) {
  
  if (!class(seed) %in% c("numeric", "NULL", "integer"))
    stop("Invalid seed")
  if (!kernel %in% c("rbf", 'linear', 'poly', 'sigmoid', 'precomputed'))
    stop("Invalid kernel")
  if (!class(C) %in% c("numeric", "integer"))
    stop("C must be a numeric value >0 ")
  if (min(C) < 0)
    stop("C must be greater than 0")
  if (!class(degree) %in% c("numeric", "integer"))
    stop("degree must be an integer")
  if (!class(gamma) %in% c("numeric", "integer"))
    stop("gamma must be a numeric value >0 ")
  if (min(gamma) < 0)
    stop("gamma must be greater than 0")
  if (!class(shrinking) %in% c("logical"))
    stop("shrinking must be T or F ")
  if (!class(coef0) %in% c("numeric", "integer"))
    stop("coef0 must be a numeric value ")
  if(sum(classWeight%in%c('none','balanced'))!=length(classWeight)){
    stop("classWeight must be 'balanced' or 'none'  ")
  }
  

  # set seed
  if(is.null(seed[1])){
    seed <- as.integer(sample(100000000,1))
  }
  
  result <- list(model = "fitSVM",
                 param = split(expand.grid(kernel = kernel,
                                           C = C,
                                           degree = degree,
                                           gamma = gamma,
                                           shrinking = shrinking,
                                           coef0 = coef0,
                                           classWeight = classWeight,
                                           varImp = varImp[1],
                                           seed = seed[1]), 1:(length(kernel) * length(C) * length(degree) * length(gamma) * length(shrinking) * length(coef0) * length(classWeight) )),
                 name = "SVM")
  class(result) <- "modelSettings"
  
  return(result)
}

fitSVM <- function(population,
                        plpData,
                        param,
                        search = "grid",
                        quiet = F,
                        outcomeId,
                        cohortId,
                        ...) {
  
  # check covariate data
  if (!FeatureExtraction::isCovariateData(plpData$covariateData))
    stop("Needs correct covariateData")
  
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
  hyperParamSel <- lapply(param, function(x) do.call(trainSVM, listAppend(x, 
                                                                               list(train = TRUE, 
                                                                                    population=pPopulation, 
                                                                                    plpData=data,
                                                                                    quiet=quiet))))
  
  
  cvAuc <- do.call(rbind, lapply(hyperParamSel, function(x) x$aucCV))
  colnames(cvAuc) <- paste0('fold_auc', 1:ncol(cvAuc))
  auc <- unlist(lapply(hyperParamSel, function(x) x$auc))
  
  cvPrediction <- lapply(hyperParamSel, function(x) x$prediction )
  cvPrediction <- cvPrediction[[which.max(auc)[1]]]
  
  hyperSummary <- cbind(do.call(rbind, param), cvAuc, auc= auc)
  
  writeLines('Training Final')
  # now train the final model and return coef
  bestInd <- which.max(abs(auc - 0.5))[1]
  finalModel <- do.call(trainSVM, listAppend(param[[bestInd]], 
                                                  list(train = FALSE, 
                                                       modelLocation=outLoc, 
                                                       population=pPopulation, 
                                                       plpData=data,
                                                       quiet=quiet)))
  
  covariateRef <- as.data.frame(plpData$covariateData$covariateRef)
  incs <- rep(1, nrow(covariateRef))
  covariateRef$included <- incs
  
  # get the coefs and do a basic variable importance:
  if(param[[1]]$varImp){
  varImp <- finalModel[[2]]  #rep(1, nrow(covariateRef))# SVC doesnt calculate var imp
  varImp[is.na(varImp)] <- 0
  covariateRef$covariateValue <- unlist(varImp)
  pred <- finalModel[[1]]
  } else{
    covariateRef$covariateValue <- rep(1, nrow(covariateRef))
    pred <- finalModel
  }
  
  
  # select best model and remove the others (!!!NEED TO EDIT THIS)
  modelTrained <- file.path(outLoc)
  param.best <- param[[bestInd]]
  
  comp <- start - Sys.time()
  
  # train prediction
  pred[,1] <- pred[,1] + 1 # converting from python to r index
  colnames(pred) <- c('rowId','outcomeCount','indexes', 'value')
  pred <- as.data.frame(pred)
  attr(pred, "metaData") <- list(predictionType="binary")
  prediction <- merge(population, pred[,c('rowId', 'value')], by='rowId')
  
  # return model location (!!!NEED TO ADD CV RESULTS HERE)
  result <- list(model = modelTrained,
                 trainCVAuc = list(value = unlist(cvAuc[bestInd,]),
                                   prediction = cvPrediction),#hyperParamSel,
                 hyperParamSearch = hyperSummary,
                 modelSettings = list(model = "fitSVM", modelParameters = param.best),
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
  attr(result, "type") <- "pythonReticulate"
  attr(result, "predictionType") <- "binary"
  
  
  return(result)
}


trainSVM <- function(population, plpData,
                     kernel='rbf', C= 1, degree= 3, gamma=3e-05, shrinking = T, coef0=0.0,classWeight = T, varImp = T,
                     seed = NULL, train = TRUE, modelLocation=NULL, quiet=FALSE) {
  
  e <- environment()
  # then run standard python code
  reticulate::source_python(system.file(package='PatientLevelPrediction','python','svmFunctions.py'), envir = e)
  
  result <- train_svm(population=population, 
                           plpData=plpData, 
                           train = train,
                      kernel = kernel,
                      C = as.numeric(C),
                      degree = as.integer(degree),
                      gamma =  as.numeric(gamma),
                      shrinking = shrinking,
                      coef0 = as.numeric(coef0),
                      classWeight = classWeight, 
                           modelOutput = modelLocation,
                           seed = as.integer(seed), 
                      varImp = varImp,
                           quiet = quiet)
  
  if (train) {
    # then get the prediction
    pred <- result
    colnames(pred) <- c("rowId", "outcomeCount", "indexes", "value")
    pred <- as.data.frame(pred)
    attr(pred, "metaData") <- list(predictionType = "binary")
    
    aucCV <- lapply(1:max(pred$indexes), function(i){computeAuc(pred[pred$indexes==i,])})
    
    auc <- computeAuc(pred)
    writeLines(paste0("CV model obtained CV AUC of ", auc))
    return(list(auc = auc, aucCV = aucCV, prediction = pred[pred$indexes>0,]))
  }
  
  return(result)
}
