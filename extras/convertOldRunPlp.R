# functions to load old PLP results and convert to new structure
loadOldPlpModel <- function(dirPath) {
  if (!file.exists(dirPath))
    stop(paste("Cannot find folder", dirPath))
  if (!file.info(dirPath)$isdir)
    stop(paste("Not a folder", dirPath))
  
  hyperParamSearch <- tryCatch(readRDS(file.path(dirPath, "hyperParamSearch.rds")),
    error=function(e) NULL)
  # add in these as they got dropped
  outcomeId <- tryCatch(readRDS(file.path(dirPath, "outcomeId.rds")),
    error=function(e) NULL)
  cohortId <- tryCatch(readRDS(file.path(dirPath, "cohortId.rds")),
    error=function(e) NULL)  
  dense <- tryCatch(readRDS(file.path(dirPath, "dense.rds")),
    error=function(e) NULL)  
  covariateMap <- tryCatch(readRDS(file.path(dirPath, "covariateMap.rds")),
    error=function(e) NULL) 
  analysisId <- tryCatch(readRDS(file.path(dirPath, "analysisId.rds")),
    error=function(e) NULL) 
  
  if(file.exists(file.path(dirPath, "keras_model"))){
    warning('Keras models no longer supported in new PLP')
    ensure_installed("keras")
    model <- keras::load_model_hdf5(file.path(dirPath, "keras_model"))
  } else if(readRDS(file.path(dirPath, "attributes.rds"))$type == "xgboost"){
    ensure_installed("xgboost")
    if('model' %in% dir(dirPath)){
      model <- xgboost::xgb.load(file.path(dirPath, "model"))
    } else{
      model <- xgboost::xgb.load(file.path(dirPath, "model.json"))
    }
  } else {  
    model <- readRDS(file.path(dirPath, "model.rds"))
  }
  
  result <- list(model = model,
    modelSettings = readRDS(file.path(dirPath, "modelSettings.rds")),
    hyperParamSearch = hyperParamSearch,
    trainCVAuc = readRDS(file.path(dirPath, "trainCVAuc.rds")),
    metaData = readRDS(file.path(dirPath, "metaData.rds")),
    populationSettings= readRDS(file.path(dirPath, "populationSettings.rds")),
    outcomeId = outcomeId,
    cohortId = cohortId,
    varImp = readRDS(file.path(dirPath, "varImp.rds")),
    trainingTime = readRDS(file.path(dirPath, "trainingTime.rds")),
    covariateMap =covariateMap,
    predict = readRDS(file.path(dirPath, "transform.rds")),
    index = readRDS(file.path(dirPath, "index.rds")),
    dense = dense,
    analysisId = analysisId)
  
  #attributes <- readRDS(file.path(dirPath, "attributes.rds"))
  attributes <- readRDS(file.path(dirPath, "attributes.rds"))
  attr(result, 'type') <- attributes$type
  attr(result, 'predictionType') <- attributes$predictionType
  class(result) <- "plpModel"
  
  # update the model location to the load dirPath
  result <- updateModelLocation(result, dirPath)
  
  return(result)
}

updateModelLocation  <- function(plpModel, dirPath){
  type <- attr(plpModel, 'type')
  if( type %in% c('pythonOld','pythonReticulate', 'pythonAuto')){
    plpModel$model <- file.path(dirPath,'python_model')
  }
  if( type =='sagemaker'){
    plpModel$model$loc <- file.path(dirPath,'sagemaker_model')
  }
  if( type =='knn'){
    plpModel$model <- file.path(dirPath,'knn_model')
  }
  return(plpModel) 
}

loadOldPlpResult <- function(dirPath){
  if (!file.exists(dirPath))
    stop(paste("Cannot find folder", dirPath))
  if (!file.info(dirPath)$isdir)
    stop(paste("Not a folder", dirPath))
  
  result <- list(model = loadOldPlpModel(file.path(dirPath, "model")),
    analysisRef = readRDS(file.path(dirPath, "analysisRef.rds")),
    inputSetting = readRDS(file.path(dirPath, "inputSetting.rds")),
    executionSummary = readRDS(file.path(dirPath, "executionSummary.rds")),
    prediction = readRDS(file.path(dirPath, "prediction.rds")),
    performanceEvaluation = readRDS(file.path(dirPath, "performanceEvaluation.rds")),
    #performanceEvaluationTrain= readRDS(file.path(dirPath, "performanceEvaluationTrain.rds")),
    covariateSummary = readRDS(file.path(dirPath, "covariateSummary.rds"))
  )
  class(result) <- "runPlp"
  
  return(result)
  
}


renameCovSum <- function(
  oldRunPlp, 
  newColNames = c(
    "covariateId","covariateName","analysisId","conceptId",                        
    "CovariateCount","CovariateMean","CovariateStDev","WithNoOutcome_CovariateCount",     
    "WithNoOutcome_CovariateMean","WithNoOutcome_CovariateStDev",
    "WithOutcome_CovariateCount","WithOutcome_CovariateMean",      
    "WithOutcome_CovariateStDev","StandardizedMeanDiff",             
    "TestWithNoOutcome_CovariateCount","TestWithNoOutcome_CovariateMean",  
    "TestWithNoOutcome_CovariateStDev","TestWithOutcome_CovariateCount",   
    "TestWithOutcome_CovariateMean","TestWithOutcome_CovariateStDev",   
    "TrainWithNoOutcome_CovariateCount","TrainWithNoOutcome_CovariateMean", 
    "TrainWithNoOutcome_CovariateStDev","TrainWithOutcome_CovariateCount",  
    "TrainWithOutcome_CovariateMean","TrainWithOutcome_CovariateStDev",  
    "covariateValue")
){
  #newColNames <- colnames(newPlp$covariateSummary)  
  oldColNames <- colnames(oldRunPlp$covariateSummary)
  newtoOld <- unlist(lapply(strsplit(newColNames, '_'), function(x) paste0(ifelse(is.na(x[2]),'', x[2]),x[1])))
  
  testIds <- grep('Test', newtoOld)
  trainIds <- grep('Train', newtoOld)
  
  newtoOld <- gsub('Test','', gsub('Train','', newtoOld))
  newtoOld[testIds] <- paste0('Test', newtoOld[testIds])
  newtoOld[trainIds] <- paste0('Train', newtoOld[trainIds])
  
  reorderOld <- sapply(oldColNames, function(oldcn) which(oldcn == newtoOld))
  
  colnames(oldRunPlp$covariateSummary) <- newColNames[reorderOld]
  
  return(oldRunPlp)
}

updatePerformanceEvaluation <- function(performanceEvaluation){
  
  performanceEvaluation$predictionDistribution$analysisId <- NULL
  ind <- colnames(performanceEvaluation$predictionDistribution) == "Eval"
  colnames(performanceEvaluation$predictionDistribution)[ind] <- 'evaluation'
  
  performanceEvaluation$calibrationSummary$analysisId <- NULL
  ind <- colnames(performanceEvaluation$calibrationSummary) == "Eval"
  colnames(performanceEvaluation$calibrationSummary)[ind] <- 'evaluation'
  
  performanceEvaluation$demographicSummary$analysisId <- NULL
  ind <- colnames(performanceEvaluation$demographicSummary) == "Eval"
  colnames(performanceEvaluation$demographicSummary)[ind] <- 'evaluation'
  
  performanceEvaluation$thresholdSummary$analysisId <- NULL
  ind <- colnames(performanceEvaluation$thresholdSummary) == "Eval"
  colnames(performanceEvaluation$thresholdSummary)[ind] <- 'evaluation'
  
  performanceEvaluation$evaluationStatistics <- performanceEvaluation$evaluationStatistics[,-1]
  colnames(performanceEvaluation$evaluationStatistics) <- c('evaluation', 'metric', 'value')
  
  return(performanceEvaluation)
}

# only works for logistic regression currently
convertOldToNew <- function(runPlpOld){
  
  # same: executionSummary, analysisRef
  
  # update covariateSummary
  runPlpOld <- renameCovSum(oldRunPlp = runPlpOld)
  
  # update performanceEvaluation
  runPlpOld$performanceEvaluation <- updatePerformanceEvaluation(runPlpOld$performanceEvaluation)
  
  # remove inputSetting
  inputSet <- runPlpOld$inputSetting
  runPlpOld$inputSetting <- NULL
  
  # update model
  settings <- list(
    plpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings( 
      sampleSize = inputSet$dataExtrractionSettings$sampleSize, 
      washoutPeriod = inputSet$dataExtrractionSettings$washoutPeriod, 
      studyStartDate = inputSet$dataExtrractionSettings$studyStartDate, 
      studyEndDate = inputSet$dataExtrractionSettings$studyEndDate, 
      firstExposureOnly = inputSet$dataExtrractionSettings$firstExposureOnly
    ),
    covariateSettings  = inputSet$dataExtrractionSettings$covariateSettings,
    featureEngineering = NULL, 
    tidyCovariates = list(
      populationSize = runPlpOld$model$metaData$preprocessSettings$populationSize,
      deletedRedundantCovariateIds = runPlpOld$model$metaData$preprocessSettings$deletedRedundantCovariateIds,
      deletedInfrequentCovariateIds = runPlpOld$model$metaData$preprocessSettings$deletedInfrequentCovariateIds,
      normFactors = runPlpOld$model$metaData$preprocessSettings$normFactors
      ),    
    covariateMap = runPlpOld$model$covariateMap, #does this need updating columnames?
    requireDenseMatrix = runPlpOld$model$dense, 
    populationSettings = inputSet$populationSettings, 
    modelSettings = inputSet$modelSettings,
    splitSettings = list(
      testSplit = inputSet$testSplit, 
      testFraction = inputSet$testFraction,
      nfold = inputSet$nfold, 
      splitSeed = inputSet$splitSeed
      ),
    sampleSettings = NULL
  )
  
  # new plpModel: "covariateImportance" "trainDetails"        "settings"            "model" 
  plpModel <- list(
    model = runPlpOld$model$model,
    trainDetails = list(
      analysisId = runPlpOld$model$analysisId,
      cdmDatabaseSchema = runPlpOld$inputSetting$dataExtrractionSettings$cdmDatabaseSchema,
      outcomeId = runPlpOld$model$outcomeId,
      cohortId = runPlpOld$model$cohortId,
      attrition = inputSet$populationSettings$attrition,
      trainingTime = runPlpOld$model$trainingTime,
      trainingDate = runPlpOld$executionSummary$ExecutionDateTime,
      hyperParamSearch = runPlpOld$model$hyperParamSearch
    ),
    settings = settings,
    covariateImportance = runPlpOld$model$varImp
    )
  
  class(plpModel) <- 'plpModel'
  attr(plpModel, 'predictionFunction') <- "predictCyclops" # only works for cyclops models
  attr(plpModel, 'modelType') <- attr(runPlpOld$model, 'predictionType')
  attr(plpModel, 'saveType') <- ifelse(class(runPlpOld$model$model)=='character', 'file', 'RtoJson')
  
  runPlpOld$model <- plpModel
  
  
  return(runPlpOld)
}
