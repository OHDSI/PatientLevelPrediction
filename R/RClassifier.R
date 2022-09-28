# this is a generic wrapper for training models using classifiers in R
fitRclassifier <- function(
  trainData, 
  modelSettings, 
  search = 'grid', 
  analysisId
  ){
  
  param <- modelSettings$param
  
  if (!FeatureExtraction::isCovariateData(trainData$covariateData)){
    stop("Needs correct covariateData")
  }
  
  
  # add folds to labels if present:
  if(!is.null(trainData$folds)){
    trainData$labels <- merge(trainData$labels, trainData$folds, by = 'rowId')
  }
  
  settings <- attr(param, 'settings')
  ParallelLogger::logInfo(paste0('Training ', settings$modelName))
  
  set.seed(settings$seed)
  
  # convert data into sparse Matrix:
  result <- toSparseM(
    trainData,
    map=NULL
  )
  
  dataMatrix <- result$dataMatrix
  labels <- result$labels
  covariateRef <- result$covariateRef
  
  # set test/train sets (for printing performance as it trains)
  start <- Sys.time()
  
  # use the new R CV wrapper
  cvResult <- applyCrossValidationInR(
    dataMatrix, 
    labels, 
    hyperparamGrid = param, 
    covariateMap = result$covariateMap
    )
  
  hyperSummary <- do.call(rbind, lapply(cvResult$paramGridSearch, function(x) x$hyperSummary))
  
  prediction <- cvResult$prediction

  variableImportance <- cvResult$variableImportance

  covariateRef <- merge(covariateRef, variableImportance, all.x = T, by = 'covariateId')
  covariateRef$covariateValue[is.na(covariateRef$covariateValue)] <- 0
  covariateRef$included[is.na(covariateRef$included)] <- 0
  
  comp <- start - Sys.time()
  
  result <- list(
    model = cvResult$model,
    
    preprocessing = list(
      featureEngineering = attr(trainData, "metaData")$featureEngineering,
      tidyCovariates = attr(trainData$covariateData, "metaData")$tidyCovariateDataSettings, 
      requireDenseMatrix = F
    ),
    
    prediction = prediction,
    
    modelDesign = PatientLevelPrediction::createModelDesign(
      targetId = attr(trainData, "metaData")$targetId,
      outcomeId = attr(trainData, "metaData")$outcomeId,
      restrictPlpDataSettings = attr(trainData, "metaData")$restrictPlpDataSettings,
      covariateSettings = attr(trainData, "metaData")$covariateSettings,
      populationSettings = attr(trainData, "metaData")$populationSettings,
      featureEngineeringSettings = attr(trainData$covariateData, "metaData")$featureEngineeringSettings,
      preprocessSettings = attr(trainData$covariateData, "metaData")$preprocessSettings,
      modelSettings = modelSettings,
      splitSettings = attr(trainData, "metaData")$splitSettings,
      sampleSettings = attr(trainData, "metaData")$sampleSettings
    ),
    
    trainDetails = list(
      analysisId = analysisId,
      analysisSource = '', #TODO add from model
      developmentDatabase = attr(trainData, "metaData")$cdmDatabaseSchema,
      attrition = attr(trainData, "metaData")$attrition, 
      trainingTime = paste(as.character(abs(comp)), attr(comp,'units')),
      trainingDate = Sys.Date(),
      modelName = attr(param, 'settings')$trainRFunction, 
      finalModelParameters = cvResult$finalParam,
      hyperParamSearch = hyperSummary
    ),
    
    covariateImportance = covariateRef
  )
  
  class(result) <- "plpModel"
  attr(result, "predictionFunction") <- settings$predictRFunction
  attr(result, "modelType") <- "binary"
  attr(result, "saveType") <- attr(param, 'saveType')
  
  return(result)
}



applyCrossValidationInR <- function(dataMatrix, labels, hyperparamGrid, covariateMap){
  
  gridSearchPredictions <- list()
  length(gridSearchPredictions) <- length(hyperparamGrid)
  
  for(gridId in 1:length(hyperparamGrid)){
    
    param <- hyperparamGrid[[gridId]]
    
    cvPrediction <- c()
    for(i in unique(labels$index)){
      
      ind <- labels$index != i
      
      model <- do.call(
        attr(hyperparamGrid, 'settings')$trainRFunction, 
        list(
          dataMatrix = dataMatrix[ind,],
          labels = labels[ind,],
          hyperParameters = param,
          settings = attr(hyperparamGrid, 'settings')
        )  
        )
      
      cvPrediction <- rbind(
        cvPrediction,
        do.call(
          attr(hyperparamGrid, 'settings')$predictRFunction, 
          list(
            plpModel = model,
            data = dataMatrix[!ind,],
            cohort = labels[!ind,]
          )  
        )
      )
      
    }
    
    attr(cvPrediction, "metaData") <- list(modelType = "binary") # make this some attribute of model

    # save hyper-parameter cv prediction
    gridSearchPredictions[[gridId]] <- list(
      prediction = cvPrediction,
      param = param
    )
  }
  
  # computeGridPerformance function is currently in SklearnClassifier.R
  paramGridSearch <- lapply(gridSearchPredictions, function(x) do.call(computeGridPerformance, x))  # cvAUCmean, cvAUC, param

  optimalParamInd <- which.max(unlist(lapply(paramGridSearch, function(x) x$cvPerformance)))
  
  finalParam <- paramGridSearch[[optimalParamInd]]$param
  
  cvPrediction <- gridSearchPredictions[[optimalParamInd]]$prediction
  cvPrediction$evaluationType <- 'CV'
  
  # fit final model
  
  finalModel <- do.call(
    attr(hyperparamGrid, 'settings')$trainRFunction, 
    list(
      dataMatrix = dataMatrix,
      labels = labels,
      hyperParameters = finalParam,
      settings = attr(hyperparamGrid, 'settings')
    )  
  )
  
  prediction <- do.call(
    attr(hyperparamGrid, 'settings')$predictRFunction, 
    list(
      plpModel = finalModel,
      data = dataMatrix,
      cohort = labels
    )  
  )
  
  prediction$evaluationType <- 'Train'
  
  prediction <- rbind(
    prediction,
    cvPrediction
  )
  
  # variable importance - how to mak sure this just returns a vector?
  variableImportance <- tryCatch(
    {do.call(
      attr(hyperparamGrid, 'settings')$varImpRFunction, 
      list(model = finalModel, covariateMap = covariateMap)
    )}
    ,
    error = function(e){
      ParallelLogger::logInfo('Error calculating variableImportance');
      ParallelLogger::logInfo(e);
      return(NULL)
    })
  
  result <- list(
    model = finalModel,
    prediction = prediction,
    finalParam = finalParam,
    paramGridSearch = paramGridSearch,
    variableImportance = variableImportance
  )
  
  return(result)
}
