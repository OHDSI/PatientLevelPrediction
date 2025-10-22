fitBinaryClassifier <- function(
    trainData,
    modelSettings,
    search = "grid", # TODO: remove this?
    analysisId,
    hyperparamSettings,
    ...) {
  
  # get the settings from the param
  param <- modelSettings$param
  
  # TODO: shall we finally move away from attr?
  settings <- modelSettings$settings
  if(is.null(settings)){
    settings <- attr(param, "settings")
  }
  
  ParallelLogger::logInfo(paste0("Training ", settings$name))
  start <- Sys.time()
  
  
  if (!FeatureExtraction::isCovariateData(trainData$covariateData)) {
    stop("Needs correct covariateData")
  }
  
  # make sure the inputs are valid
  #TODO: make this generic
  checkPySettings(settings)
  
  # add folds to labels if present:
  if (!is.null(trainData$folds)) {
    trainData$labels <- merge(trainData$labels, trainData$folds, by = "rowId")
  }
  
  set.seed(settings$seed)
  
  # convert data into sparse Matrix:
  # TODO: make this a settings functions?
  mappedData <- toSparseM(
    trainData,
    map = NULL
  )
  
  matrixData <- mappedData$dataMatrix
  labels <- mappedData$labels
  covariateRef <- mappedData$covariateRef
  

  # TODO: create tune with input/output
  # input: matrixData, labels, settings, hyperparamSettings
  # output: model, prediction, finalParam, hyperSummary
  result <- do.call(
    what = tune,
    args = list(
      matrixData = matrixData,
      labels = labels,
      settings = modelSettings, # contains the fit and settings
      hyperparamSettings = hyperparamSettings
    )
  )

  # add code for var imp here - model and settings
  
  
  
  # Process this inside 'tune'?
  variableImportance <- result$variableImportance
  variableImportance[is.na(variableImportance)] <- 0
  
  covariateRef <- merge(covariateRef, variableImportance, all.x = TRUE,
                        by = "covariateId")
  covariateRef$covariateValue[is.na(covariateRef$covariateValue)] <- 0
  covariateRef$included[is.na(covariateRef$included)] <- 0
  
  comp <- start - Sys.time()
  
  result <- list(
    model = result$model,
    preprocessing = list(
      featureEngineering = attr(trainData$covariateData, "metaData")$featureEngineering,
      tidyCovariates = attr(trainData$covariateData, "metaData")$tidyCovariateDataSettings,
      requireDenseMatrix = FALSE
    ),
    prediction = result$prediction,
    modelDesign = PatientLevelPrediction::createModelDesign(
      targetId = attr(trainData, "metaData")$targetId,
      outcomeId = attr(trainData, "metaData")$outcomeId,
      restrictPlpDataSettings = attr(trainData, "metaData")$restrictPlpDataSettings,
      # ADDING hyperparamSearch
      hyperparamSettings = hyperparamSettings,
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
      analysisSource = "", # TODO add from model
      developmentDatabase = attr(trainData, "metaData")$cdmDatabaseName,
      developmentDatabaseSchema = attr(trainData, "metaData")$cdmDatabaseSchema,
      attrition = attr(trainData, "metaData")$attrition,
      trainingTime = paste(as.character(abs(comp)), attr(comp, "units")),
      trainingDate = Sys.Date(),
      modelName = modelSettings$fitFunction, 
      finalModelParameters = result$finalParam,
      hyperParamSearch = result$hyperSummary
    ),
    covariateImportance = covariateRef #,
    #attributes = list(
    # predictionFunction = settings$predictFunction,
    # modelType = "binary",
    # saveType = modelSettings$saveType
    #)
  )
  
  class(result) <- "plpModel"
  # remove attr and put this into attributes in result?
  attr(result, "predictionFunction") <- settings$predictFunction
  attr(result, "modelType") <- "binary"
  attr(result, "saveType") <- attr(param, "saveType")
  
  return(result)
}
