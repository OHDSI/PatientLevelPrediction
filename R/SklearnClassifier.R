# @file PythonClassifier.R
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

fitSklearn <- function(trainData,
  classifierFunction = 'trainAdaBoost',
  param,
  search = "grid",
  ...) {
  
  # check covariate data
  if(!FeatureExtraction::isCovariateData(trainData$covariateData)){stop("Needs correct covariateData")}
  
  # get the settings from the param
  pySettings <- attr(param, 'settings')
  
  # make sure the inputs are valid
  checkPySettings(pySettings)
  
  start <- Sys.time()
  
  # convert the data to a sparse R Matrrix and then use reticulate to convert to python sparse
  # need to save the covariateMap so we know the covariateId to columnId when applying model
  mappedData <- createPythonData(trainData)
  
  matrixData <- mappedData$pythonMatrixData
  labels <- mappedData$pythonLabels
  covariateMap <- mappedData$covariateMap
  
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
      pythonImport = pythonImport,
      pythonImportSecond = pythonImportSecond,
      pythonClassifier = pythonClassifier,
      modelLocation = outLoc,
      paramSearch = param
      )
    )
  
  hyperSummary <- do.call(rbind, lapply(cvResult$paramGridSearch, function(x) x$hyperSummary))

  prediction <- cvResult$prediction
  prediction <- merge(population, prediction[,c('rowId', 'value')], by='rowId', all = T)
  
  variableImportance <- cvResult$variableImportance
  variableImportance[is.na(variableImportance)] <- 0
  
  covariateRef <- as.data.frame(trainData$covariateData$covariateRef)
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
      featureEngineering = attr(trainData$covariateData, "metaData")$featureEngineering,
      tidyCovariates = attr(trainData$covariateData, "metaData")$tidyCovariateDataSettings, 
      covariateMap = covariateMap,
      requireDenseMatrix = F,
      populationSettings = attr(trainData, "metaData")$populationSettings,
      modelSettings = list(
        model = classifierFunction, 
        param = param,
        finalModelParameters = cvResult$finalParam,
        extraSettings = attr(param, 'settings')
      ),
      splitSettings = attr(trainData, "metaData")$splitSettings
    ),
    
    trainDetails = list(
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
  attr(result, "predictionType") <- "pythonReticulate"
  attr(result, "modelType") <- "binary"
  
  return(result)
}

checkPySettings <- function(settings){
  checkIsClass(settings$seed, c('numeric', 'integer'))
  ParallelLogger::logDebug(paste0('classifier seed: ', settings$seed))
  
  checkIsClass(settings$requiresDenseMatrix, c('logical'))
  ParallelLogger::logDebug(paste0('requiresDenseMatrix: ', settings$requiresDenseMatrix))
  
  checkIsClass(settings$name, c('character'))
  ParallelLogger::logDebug(paste0('name: ', settings$name))
  
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
  paramSearch)
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
    
    # initiate all predictions to 0
    labels$value <- 0
    
    fold <- labels$index
    ParallelLogger::logInfo(paste0('Max fold: ', max(fold)))
    
    for( i in 1:max(fold)){
      
      ParallelLogger::logInfo(paste0('Fold ',i))
      trainY <- reticulate::r_to_py(labels$outcomeCount[fold != i])
      trainX <- reticulate::r_to_py(matrixData[fold != i,])
      testX <- reticulate::r_to_py(matrixData[fold == i,])
      
      ParallelLogger::logDebug(paste0('fold X dim: ', trainX$shape[0], 'x', trainX$shape[1]))
      ParallelLogger::logDebug(paste0('fold Y length: ', np$shape(trainY)[[1]], ' with ',np$sum(trainY)[[1]], ' outcomes'))
    
      if(requiresDenseMatrix){
        ParallelLogger::logInfo('Converting sparse martix to dense matrix (CV)')
        trainX <- trainX$toarray()
        testX <- testX$toarray()
      }
      
      model <- fitPythonModel(trainX,trainY)
      
      ParallelLogger::logInfo(paste0("Training fold took (mins): ",difftime(timeEnd, timeStart, units='mins') ))
      
      ParallelLogger::logInfo("Calculating predictions on left out fold set...")
      predFold <- predictValues(model = model, newX = testX)

      labels$value[fold == i] <- predFold[,1]
      
    }
    
    gridSearchPredictons[[gridId]] <- list(
      prediction = labels,
      param = paramSearch[[gridId]]
    )
  }
  
  # get best para (this could be modified to enable any metric instead of AUC, just need metric input in function)
  
  paramGridSearch <- lapply(gridSearchPredictons, computeGridPerformance)  # cvAUCmean, cvAUC, param
  
  optimalParamInd < which.max(unlist(lapply(paramGridSearch, function(x) x$cvPerformance)))
  
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
  
  model <- fitPythonModel(trainX,trainY)
  
  ParallelLogger::logInfo("Calculating predictions on all train data...")
  predictionValue <- predictValues(model = model, newX = trainX)
  
  labels$value <- predictionValue[,1]
  labels$evaluationType <- 'Train'
  
  prediction <- rbind(
    labels,
    cvPrediction
  )
  
  # saving model
  joblib$dump(model, os$path$join(modelOutput,"model.pkl"), compress = True) 
  
  # feature importance
  variableImportance <- reticulate::py_to_r(model$feature_importances_)

  return(
    list(
      prediction = prediction,
      finalParam = finalParam,
      paramGridSearch = paramGridSearch,
      variableImportance = variableImportance
    )
  )
  
}


fitPythonModel <- function(trainX,trainY){
  ParallelLogger::logDebug(paste0('fold X dim: ', trainX$shape[0], 'x', trainX$shape[1]))
  ParallelLogger::logDebug(paste0('fold Y length: ', np$shape(trainY)[[1]], ' with ',np$sum(trainY)[[1]], ' outcomes'))
  
  timeStart <- Sys.time()
  model = classifier(n_estimators = n_estimators, 
    learning_rate = learning_rate, 
    algorithm = 'SAMME.R', 
    random_state = seed)  # paramSearch[[gridId]]
  model = model$fit(trainX, trainY)
  timeEnd <- Sys.time()
  
  ParallelLogger::logInfo(paste0("Training final model took (mins): ",difftime(timeEnd, timeStart, units='mins') ))
  
  return(model)
}

predictValues <- function(model = model, newX = trainX){
  
  newPredY <- model$predict_proba(newX)
  predictionValue <- reticulate::py_to_r(newPredY)
  
  return(predictionValue)
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
  
  hyperSummary <- cbind(metric, performance, performanceFold, unlist(param))
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
