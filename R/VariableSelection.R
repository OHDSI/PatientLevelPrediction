# @file VariableSelection.R
#
# Copyright 2021 Observational Health Data Sciences and Informatics
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


#' @export
setUnivariateSelection <- function(modelSettings = PatientLevelPrediction::setLassoLogisticRegression(),
                                   corMethod = "pearson",
                                   nVariables = 50) { # TODO: set dynamic number based on elbow
  
  checkIsClass(nVariables, c('numeric','integer'))
  # TODO: add class checks input (modelSettings, corMethod)?
  
  param <- list(
    modelSettings = modelSettings,
    param = list(
      corMethod = corMethod,
      nVariables = nVariables
    )
  )
  
  result <- list(
    fitFunction = "fitUnivariateSelection",
    param = param
  )
  class(result) <- "modelSettings"
  
  return(result)
}

fitUnivariateSelection <- function(
    trainData,
    modelSettings,
    search = 'none',
    analysisId,
    stop = F,
    ...) {
  
  # Settings variable selection
  param <- modelSettings$param$param
  
  covariates <- as.data.frame(trainData$covariateData$covariates)
  
  if (length(unique(covariates$covariateId)) > param$nVariables) {
    outcomes <- trainData$labels[, c("rowId", "outcomeCount")]
    
    # Select features based on univariate association with outcome
    correlation <- sapply(unique(covariates$covariateId), function(covId) {
      cor(ifelse(outcomes$rowId %in% covariates$rowId[covariates$covariateId == covId], 1, 0), # TODO: can this be done smarter for sparse data
          outcomes$outcomeCount, 
          method = param$corMethod)
    })
    names(correlation) <- unique(covariates$covariateId)
    
    # Order from high to low absolute correlation
    correlation <-  correlation[order(abs(correlation), decreasing = TRUE)]
    
    # Select variables
    selected <- names(correlation)[1:min(param$nVariables, length(correlation))]
    
    # Update covariate data
    trainData$covariateData <- updateCovariateData(trainData$covariateData, covIds=selected, update="select")
    
    ParallelLogger::logTrace('Finished variable selection.')
  } else {
    ParallelLogger::logTrace('No variable selection, number of covariates less than or equal to nVariables.')
  }
  
  # Fit final PLP model
  fun <- eval(parse(text = modelSettings$param$modelSettings$fitFunction))
  args <- list(
    trainData = trainData,
    modelSettings = modelSettings$param$modelSettings,
    search = search,
    analysisId = analysisId
  )
  plpModel <- do.call(fun, args)
  
  return(plpModel)
}

#' @export
setStepwiseSelection <- function(modelSettings,
                                 selectMethod = "backward",
                                 nInitialVariables = 50, # TODO: set dynamic number based on elbow
                                 nVariables = 20, # TODO: set dynamic number based on elbow
                                 stepSize = 1) { 
  
  checkIsClass(nInitialVariables, c('numeric','integer'))
  checkIsClass(nVariables, c('numeric','integer'))
  checkIsClass(stepSize, c('numeric','integer'))
  # TODO: add class checks input (modelSettings, selectMethod)?
  
  param <- list(
    modelSettings = PatientLevelPrediction::setLassoLogisticRegression(),
    param = list(
      selectMethod = selectMethod,
      nInitialVariables = nInitialVariables, # For classifiers with initial selection based on variable importance
      nVariables = nVariables,
      stepSize = stepSize # Number of variables removed at the same time
    )
  )
  
  result <- list(
    fitFunction = "fitStepwiseSelection",
    param = param
  )
  class(result) <- "modelSettings"
  
  return(result)
}


fitStepwiseSelection <- function(
    trainData,
    modelSettings,
    search = 'none',
    analysisId,
    stop = F,
    ...) {
  
  # Initial fit PLP model
  fun <- eval(parse(text = modelSettings$param$modelSettings$fitFunction))
  args <- list(
    trainData = trainData,
    modelSettings = modelSettings$param$modelSettings,
    search = search,
    analysisId = analysisId
  )
  plpModel <- do.call(fun, args)
  
  if (modelSettings$param$param$selectMethod == "backward") {
    # Initial selection
    # Select non-zero coefficients for LASSO
    covIds <- plpModel$model$coefficients$covariateIds[plpModel$model$coefficients$betas != 0] 
    
    # TODO: make generic across algorithms based on var importance (e.g. randomForest)
    # covIds <- TODO
    
    fullCovariates <- NULL
    updateIteration <- "remove"
    
  } else if (modelSettings$param$param$selectMethod == "forward") {
    # Initial selection (empty)
    covIds <- NULL
    
    fullCovariates <- as.data.frame(trainData$covariateData$covariates)
    updateIteration <- "add"
    
  } else {
    stop("Variable selection stopped: selectMethod not implemented.")
  }
  
  update <- "select" # Only first update of covariates
  while(!is.null(covIds) | update == "select") { # Stop when selected is NULL
    # Update covariate data
    trainData$covariateData <- updateCovariateData(trainData$covariateData, covIds=covIds, update=update, fullCovariates=fullCovariates)
    
    # Backward or forward select variables
    update <- updateIteration 
    covIds <- selectVariables(modelSettings, trainData, fullCovariates, search, analysisId)
  }
  
  ParallelLogger::logTrace('Finished variable selection.')
  
  # Fit final PLP model
  fun <- eval(parse(text = modelSettings$param$modelSettings$fitFunction))
  args <- list(
    trainData = trainData,
    modelSettings = modelSettings$param$modelSettings,
    search = search,
    analysisId = analysisId
  )
  plpModel <- do.call(fun, args)
  
  return(plpModel)
}

selectVariables <- function(modelSettings, trainData, fullCovariates, search, analysisId) {
  
  # Settings variable selection
  param <- modelSettings$param$param
  
  covariates <- as.data.frame(trainData$covariateData$covariates)
  
  if (param$selectMethod == "backward") {
    update <- "remove"
    start <- (length(unique(covariates$covariateId)) > param$nVariables) # Too many variables
    covariateList <- unique(covariates$covariateId)
  } else if (param$selectMethod == "forward") {
    update <- "add"
    start <- (length(unique(covariates$covariateId)) < param$nVariables) # Not enough variables
    covariateList <- unique(fullCovariates$covariateId)[!(unique(fullCovariates$covariateId) %in% unique(covariates$covariateId))]
  }
  # TODO: make correction for param$stepSize to come to exactly the right number of variables
  
  if (start) {
    performance <- sapply(covariateList, function(covId) {
      # Temporarily update covarite data
      tempData <- trainData
      tempData$covariateData <- updateCovariateData(tempData$covariateData, covIds=covId, update=update, fullCovariates=fullCovariates)
      
      # Re-fit PLP model
      fun <- eval(parse(text = modelSettings$param$modelSettings$fitFunction))
      args <- list(
        trainData = tempData,
        modelSettings = modelSettings$param$modelSettings,
        search = search,
        analysisId = analysisId
      )
      tempModel <- do.call(fun, args)
      
      # Return performance for current covariate data
      # TODO: extend to other selection criteria? 
      metric <- tempModel$model$log_likelihood
      
      return(metric)
    })
    names(performance) <- covariateList
    
    if (param$selectMethod == "backward") {
      # Order from low to high performance (minimum negative log likelihood is high)
      performance <- performance[order(performance, decreasing = TRUE)]
      
      # Select variables to remove
      covIds <- names(performance)[1:min(param$stepSize, length(performance))]
      
    } else if (param$selectMethod == "forward") {
      # Order from high to low performance (minimum negative log likelihood is high)
      performance <- performance[order(performance, decreasing = FALSE)]
      
      # Select variables to add
      covIds <- names(performance)[1:min(param$stepSize, length(performance))]
    }
    
    return(covIds)
  }
  
  return(NULL) # Return NULL to initiate stop
}

updateCovariateData <- function(covariateData, covIds, update="select", fullCovariates=NULL) {
  # FeatureExtraction -> excludedCovariateConceptIds: A list of concept IDs that should NOT be used to construct covariates.
  newCovariates <- as.data.frame(covariateData$covariates) # TODO: try without
  print(paste0("Rows covariates - before: ", nrow(newCovariates)))
  
  if (update == "select") {
    newCovariates <- newCovariates[newCovariates$covariateId %in% covIds,]
  } else if (update == "remove") {
    # print(paste0("Remove covIds: ", paste0(covIds, collapse = ", ")))
    newCovariates <- newCovariates[!(newCovariates$covariateId %in% covIds),]
  } else if (update == "add") {
    # print(paste0("Add covIds: ", paste0(covIds, collapse = ", ")))
    
    if (is.null(fullCovariates)) {
      stop("fullCovariates missing, needed for adding covariates.")
    }
    newCovariates <- fullCovariates[fullCovariates$covariateId %in% c(unique(newCovariates$covariateId), covIds),]
  }
  print(paste0("Rows covariates - after: ", nrow(newCovariates)))
  
  # metaData <- list(call = match.call())
  result <- Andromeda::andromeda(covariates = newCovariates,
                                 labels=covariateData$labels,
                                 covariateRef = covariateData$covariateRef,
                                 analysisRef = covariateData$analysisRef)
  # attr(result, "metaData") <- metaData
  class(result) <- "CovariateData"
  
  return(result)
}


