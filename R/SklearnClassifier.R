# @file PythonClassifier.R
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

fitSklearn <- function(
  trainData,
  param,
  search = "grid",
  analysisId,
  ...) {
  
  # check covariate data
  if(!FeatureExtraction::isCovariateData(trainData$covariateData)){stop("Needs correct covariateData")}
  
  # get the settings from the param
  pySettings <- attr(param, 'settings')
  
  # make sure the inputs are valid
  checkPySettings(pySettings)
  
  start <- Sys.time()
  
  if(!is.null(trainData$folds)){
    trainData$labels <- merge(trainData$labels, trainData$fold, by = 'rowId')
  }
  
  # convert the data to a sparse R Matrix and then use reticulate to convert to python sparse
  # need to save the covariateMap so we know the covariateId to columnId when applying model
  mappedData <- toSparseM(trainData)
  
  matrixData <- mappedData$dataMatrix
  labels <- mappedData$labels
  covariateRef <- mappedData$covariateRef
  
  # save the model to outLoc
  outLoc <- createTempModelLoc()

  # functions does CV and fits final models
  # returns: prediction (Train/CV),
  #          finalParam (optimal hyper-parameters)
  #          variableImportance (final model) 
  #          paramGridSearch list with performance and params for complete grid search
  # at the moment it uses AUC as performance but this could be modified to let user
  # specify the performance metric to optimise
  cvResult <- do.call( 
    what = gridCvPython,
    args = list(
      matrixData = matrixData,
      labels = labels,
      seed = pySettings$seed,
      requiresDenseMatrix = pySettings$requiresDenseMatrix,
      modelName = pySettings$name,
      pythonImport = pySettings$pythonImport,
      pythonImportSecond = pySettings$pythonImportSecond,
      pythonClassifier = pySettings$pythonClassifier,
      modelLocation = outLoc,
      paramSearch = param,
      saveToJson = pySettings$saveToJson
      )
    )
  
  hyperSummary <- do.call(rbind, lapply(cvResult$paramGridSearch, function(x) x$hyperSummary))

  prediction <- cvResult$prediction
 
  variableImportance <- cvResult$variableImportance
  variableImportance[is.na(variableImportance)] <- 0
  
  incs <- rep(1, nrow(covariateRef))
  covariateRef$included <- incs
  covariateRef$covariateValue <- unlist(variableImportance) # check this is correct order
  
  comp <- start - Sys.time()
  
  result <- list(
    model = file.path(outLoc),
    
    prediction = prediction,
    
    settings = list(
      plpDataSettings = attr(trainData, "metaData")$plpDataSettings,
      covariateSettings = attr(trainData, "metaData")$covariateSettings,
      populationSettings = attr(trainData, "metaData")$populationSettings,
      featureEngineering = attr(trainData$covariateData, "metaData")$featureEngineering,
      tidyCovariates = attr(trainData$covariateData, "metaData")$tidyCovariateDataSettings, 
      requireDenseMatrix = attr(param, 'settings')$requiresDenseMatrix,
      modelSettings = list(
        model = pySettings$name, 
        param = param,
        finalModelParameters = cvResult$finalParam,
        extraSettings = attr(param, 'settings')
      ),
      splitSettings = attr(trainData, "metaData")$splitSettings,
      sampleSettings = attr(trainData, "metaData")$sampleSettings
    ),
    
    trainDetails = list(
      analysisId = analysisId,
      cdmDatabaseSchema = attr(trainData, "metaData")$cdmDatabaseSchema,
      outcomeId = attr(trainData, "metaData")$outcomeId,
      cohortId = attr(trainData, "metaData")$cohortId,
      attrition = attr(trainData, "metaData")$attrition, 
      trainingTime = comp,
      trainingDate = Sys.Date(),
      hyperParamSearch = hyperSummary
    ),
    
    covariateImportance = covariateRef
  )
  
  class(result) <- "plpModel"
  attr(result, "predictionFunction") <- "predictPythonSklearn"
  attr(result, "modelType") <- "binary"
  attr(result, "saveType") <- attr(param, 'saveType')
  
  return(result)
}


predictPythonSklearn <- function(
  plpModel, 
  data, 
  cohort
  ){
  
  if(class(data) == 'plpData'){
    # convert
    matrixObjects <- toSparseM(
      plpData = data, 
      cohort = cohort,
      map = plpModel$covariateImportance %>% 
        dplyr::select(.data$columnId, .data$covariateId)
    )
    
    newData <- matrixObjects$dataMatrix
    cohort <- matrixObjects$labels
    
  }else{
    newData <- data
  }

  # import
  os <- reticulate::import('os')
  
  # load model
  if(plpModel$settings$modelSettings$extraSettings$saveToJson){
    skljson <- reticulate::import('sklearn_json')
    modelLocation <- reticulate::r_to_py(paste0(plpModel$model,"\\model.json"))
    model <- skljson$from_json(modelLocation)
  } else{
    joblib <- reticulate::import('joblib')
    modelLocation <- reticulate::r_to_py(paste0(plpModel$model,"\\model.pkl"))
    model <- joblib$load(os$path$join(modelLocation)) 
  }
  
  included <- plpModel$covariateImportance$columnId[plpModel$covariateImportance$included>0] # does this include map?
  pythonData <- reticulate::r_to_py(newData[,included, drop = F])

  # make dense if needed
  if(plpModel$settings$requireDenseMatrix){
    pythonData <- pythonData$toarray()
  }
  
  cohort <- predictValues(
    model = model, 
    data = pythonData, 
    cohort = cohort, 
    type = attr(plpModel, 'modelType')
  )

  return(cohort)
}

predictValues <- function(model, data, cohort, type = 'binary'){
  
  predictionValue  <- model$predict_proba(data)
  cohort$value <- predictionValue[,2]
  
  cohort <- cohort %>% 
    dplyr::select(-.data$rowId) %>%
    dplyr::rename(rowId = .data$originalRowId)
  
  attr(cohort, "metaData")$modelType <-  type
  
  return(cohort)
}

checkPySettings <- function(settings){
  checkIsClass(settings$seed, c('numeric', 'integer'))
  ParallelLogger::logDebug(paste0('classifier seed: ', settings$seed))
  
  checkIsClass(settings$requiresDenseMatrix, c('logical'))
  ParallelLogger::logDebug(paste0('requiresDenseMatrix: ', settings$requiresDenseMatrix))
  
  checkIsClass(settings$name, c('character'))
  ParallelLogger::logDebug(paste0('name: ', settings$name))
  
  checkIsClass(settings$saveToJson, c('logical'))
  ParallelLogger::logDebug(paste0('saveToJson: ', settings$saveToJson))
  
  checkIsClass(settings$pythonImport, c('character'))
  ParallelLogger::logDebug(paste0('pythonImport: ', settings$pythonImport))

  if(!is.null(settings$pythonImportSecond)){
    checkIsClass(settings$pythonImportSecond, c('character'))
    ParallelLogger::logDebug(paste0('pythonImportSecond: ', settings$pythonImportSecond))
  } else{
    ParallelLogger::logDebug(paste0('pythonImportSecond: NULL'))
  }
  
  checkIsClass(settings$pythonClassifier, c('character'))
  ParallelLogger::logDebug(paste0('pythonClassifier: ', settings$pythonClassifier))
}

gridCvPython <- function(
  matrixData, 
  labels, 
  seed, 
  requiresDenseMatrix, 
  modelName,
  pythonImport,
  pythonImportSecond,
  pythonClassifier,
  modelLocation,
  paramSearch,
  saveToJson
  )
  {
  
  ParallelLogger::logInfo(paste0("Rnning CV for ",modelName," model"))
 
  np <- reticulate::import('numpy')
  os <- reticulate::import('os')
  sys <- reticulate::import('sys')
  math <- reticulate::import('math')
  scipy <- reticulate::import('scipy')
  joblib <- reticulate::import('joblib')
  
  firstImport <- reticulate::import(pythonImport)
  
  if(!is.null(pythonImportSecond)){
    classifier <- firstImport[[pythonImportSecond]][[pythonClassifier]]
  } else{
    classifier <- firstImport[[pythonClassifier]]
  }
  
  ###########################################################################
  
  gridSearchPredictons <- list()
  length(gridSearchPredictons) <- length(paramSearch)
  
  for(gridId in 1:length(paramSearch)){
    
    # initiate prediction
    prediction <- c()
    
    fold <- labels$index
    ParallelLogger::logInfo(paste0('Max fold: ', max(fold)))
    
    for( i in 1:max(fold)){
      
      ParallelLogger::logInfo(paste0('Fold ',i))
      trainY <- reticulate::r_to_py(labels$outcomeCount[fold != i])
      trainX <- reticulate::r_to_py(matrixData[fold != i,])
      testX <- reticulate::r_to_py(matrixData[fold == i,])
      
      if(requiresDenseMatrix){
        ParallelLogger::logInfo('Converting sparse martix to dense matrix (CV)')
        trainX <- trainX$toarray()
        testX <- testX$toarray()
      }
      
      model <- fitPythonModel(classifier, paramSearch[[gridId]], seed, trainX, trainY, np, pythonClassifier)
      
      ParallelLogger::logInfo("Calculating predictions on left out fold set...")
      prediction <- rbind(prediction, predictValues(model = model, data = testX, cohort = labels[fold == i,], type = 'binary'))
      
    }
    
    gridSearchPredictons[[gridId]] <- list(
      prediction = prediction,
      param = paramSearch[[gridId]]
    )
  }
  
  # get best para (this could be modified to enable any metric instead of AUC, just need metric input in function)
  
  paramGridSearch <- lapply(gridSearchPredictons, function(x){do.call(computeGridPerformance, x)})  # cvAUCmean, cvAUC, param
  
  optimalParamInd <- which.max(unlist(lapply(paramGridSearch, function(x) x$cvPerformance)))
  
  finalParam <- paramGridSearch[[optimalParamInd]]$param
  
  cvPrediction <- gridSearchPredictons[[optimalParamInd]]$prediction
  cvPrediction$evaluationType <- 'CV'
  
  ParallelLogger::logInfo('Training final model using optimal parameters')
  
  trainY <- reticulate::r_to_py(labels$outcomeCount)
  trainX <- reticulate::r_to_py(matrixData)
  
  if(requiresDenseMatrix){
    ParallelLogger::logInfo('Converting sparse martix to dense matrix (final model)')
    trainX <- trainX$toarray()
  }
  
  model <- fitPythonModel(classifier, finalParam , seed, trainX, trainY, np, pythonClassifier)
  
  ParallelLogger::logInfo("Calculating predictions on all train data...")
  prediction <- predictValues(model = model, data = trainX, cohort = labels, type = 'binary')
  prediction$evaluationType <- 'Train'
  
  prediction <- rbind(
    prediction,
    cvPrediction
  )
  
  # saving model
  if(!dir.exists(file.path(modelLocation))){
    dir.create(file.path(modelLocation), recursive = T)
  }
 # joblib$dump(model, os$path$join(modelLocation,"model.pkl"), compress = T) 
  if(saveToJson){
    skljson <- reticulate::import('sklearn_json')
    skljson$to_json(model = model, model_name =  file.path(modelLocation,"model.json"))
  } else{
    joblib$dump(model, file.path(modelLocation,"model.pkl"), compress = T) 
  }
  
  # feature importance
  variableImportance <- tryCatch({model$feature_importances_}, error = function(e){ParallelLogger::logInfo(e);return(rep(1,ncol(matrixData)))})

  return(
    list(
      prediction = prediction,
      finalParam = finalParam,
      paramGridSearch = paramGridSearch,
      variableImportance = variableImportance
    )
  )
  
}


fitPythonModel <- function(classifier, param, seed, trainX, trainY, np, pythonClassifier){
  ParallelLogger::logInfo(paste0('data X dim: ', trainX$shape[0], 'x', trainX$shape[1]))
  ParallelLogger::logInfo(paste0('data Y length: ', np$shape(trainY)[[1]], ' with ',np$sum(trainY), ' outcomes'))
  
  timeStart <- Sys.time()
  
  # print parameters
  # convert NULL to NA values
  paramString <- param
  for(ind in 1:length(paramString)){
    if(is.null(paramString[[ind]])){
      paramString[[ind]] <- 'null'
    }
  }
  ParallelLogger::logInfo(paste(names(param), unlist(paramString), sep = ':', collapse = '    '))
  
  if(!is.null(param)){
    model <- do.call(paste0(pythonClassifier,'Inputs'), list(classifier = classifier, param = param))
  } else{
    model <- classifier()
  }
  model <- model$fit(trainX, trainY)
  timeEnd <- Sys.time()
  
  ParallelLogger::logInfo(paste0("Training model took (mins): ",difftime(timeEnd, timeStart, units='mins') ))
  
  return(model)
}




computeGridPerformance <- function(prediction, param, performanceFunct = 'computeAuc'){
  
  performance <- do.call(
    what = eval(parse(text = performanceFunct)),
    args = list(prediction = prediction)
  )
  
  performanceFold <- c()
  for(i in 1:max(prediction$index)){
    performanceFold <- c(
      performanceFold, 
      do.call(
        what = eval(parse(text = performanceFunct)),
        args = list(prediction = prediction[prediction$index == i,])
      )
    )
  }
  
  paramString <- param
  for(ind in 1:length(paramString)){
    if(is.null(paramString[[ind]])){
      paramString[[ind]] <- 'null'
    }
  }
  
  hyperSummary <- c(performanceFunct, performance, performanceFold, unlist(paramString))
  names(hyperSummary) <- c(
    'Metric', 
    'cvPerformance', 
    paste0('cvPerformanceFold',1:length(performanceFold)),
    names(param)
  )
  
  return(
    list(
      metric = performanceFunct,
      cvPerformance = performance,
      cvPerformancePerFold = performanceFold,
      param = param,
      hyperSummary = hyperSummary
    )
  )
  
}
