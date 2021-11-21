# @file lassoLogisticRegression.R
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


fitCyclopsModel <- function(
  trainData, 
  param, 
  search='adaptive'){
  
  # check plpData is coo format:
  if (!FeatureExtraction::isCovariateData(trainData$covariateData)){
    stop("Needs correct covariateData")
  }
  
  settings <- attr(param, 'settings')
  
  trainData$covariateData$labels <- trainData$labels %>% 
    dplyr::mutate(
      y = sapply(.data$outcomeCount, function(x) min(1,x)),
      time <- .data$survivalTime
    )
    
  covariates <- filterCovariateIds(param, trainData$covariateData)
  
  start <- Sys.time() 
  
  cyclopsData <- Cyclops::convertToCyclopsData(outcomes = trainData$covariateData$labels,
    covariates = covariates,
    addIntercept = settings$addIntercept,
    modelType = modelTypeToCyclopsModelType(settings$modelType),
    checkRowIds = FALSE,
    normalize = NULL,
    quiet = TRUE
    )

  prior <- Cyclops::createPrior(
    priorType =  param$priorType, 
    forceIntercept = param$forceIntercept,
    useCrossValidation = max(trainData$labels$index)>1, 
    variance = param$startingVariance,
    exclude = param$exclude
    )
  
  if(settings$useControl){
    
    control <- Cyclops::createControl(
      cvType = "auto",
      fold = max(trainData$labels$index),
      startingVariance = param$startingVariance,
      lowerLimit = param$lowerLimit,
      upperLimit = param$upperLimit,
      tolerance  = settings$tolerance,
      cvRepetitions = 1, # make an option?
      selectorType = "byPid",
      noiseLevel = "silent",
      threads = settingd$threads,
      maxIterations = settings$maxIterations)
    
    fit <- tryCatch({
      ParallelLogger::logInfo('Running Cyclops')
      Cyclops::fitCyclopsModel(cyclopsData, prior = prior, control = control)}, 
      finally = ParallelLogger::logInfo('Done.'))
  } else{
    fit <- tryCatch({
      ParallelLogger::logInfo('Running Cyclops with fixed varience')
      Cyclops::fitCyclopsModel(cyclopsData, prior = prior)}, 
      finally = ParallelLogger::logInfo('Done.')) 
  }
  
  modelTrained <- createCyclopsModel(
    fit = fit, 
    modelType = modelTypeToCyclopsModelType(settings$modelType), 
    useCrossValidation = max(trainData$labels$index)>1, 
    cyclopsData = cyclopsData, 
    labels = trainData$labels
    )

  # TODO get optimal lambda value
  ParallelLogger::logTrace('Returned from fitting to LassoLogisticRegression')
  comp <- Sys.time() - start

  ParallelLogger::logTrace('Getting variable importance')
  variableImportance <- getVariableImportance(modelTrained, trainData)
  
  #get prediction on test set:
  ParallelLogger::logTrace('Getting predictions on train set')
  prediction <- predict_plp(
    plpModel = list(model = modelTrained),
    population = trainData$labels, 
    plpData = plpData
    )
  prediction$evalType <- 'Train'
  
  # get cv AUC
  cvPrediction  <- do.call(rbind, lapply(modelTrained$cv, function(x){x$predCV}))
  cvPrediction$evalType <- 'CV'
  
  prediction <- rbind(prediction, cvPrediction)
  
  cvPerFold <-  unlist(lapply(modelTrained$cv, function(x){x$out_sample_auc}))
  if(length(cvPerFold)>0){
    names(cvPerFold) <- paste0('fold_auc', 1:length(cvPerFold))
  }
  
  result <- list(
    model = modelTrained,
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
        model = settings$modelType, 
        param = param,
        finalModelParameters = list(
          variance = modelTrained$priorVariance,
          log_likelihood = modelTrained$log_likelihood
        ),
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
      hyperParamSearch = cvPerFold
    ),
    
    covariateImportance = variableImportance
  )
  
  
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'plp'
  attr(result, 'predictionType') <- 'binary'
  return(result)
}


createCyclopsModel <- function(fit, modelType, useCrossValidation, cyclopsData, labels){
  
  if (is.character(fit)) {
    coefficients <- c(0)
    status <- fit
  } else if (fit$return_flag == "ILLCONDITIONED") {
    coefficients <- c(0)
    status <- "ILL CONDITIONED, CANNOT FIT"
    ParallelLogger::logWarn(paste("GLM fitting issue: ", status))
  } else if (fit$return_flag == "MAX_ITERATIONS") {
    coefficients <- c(0)
    status <- "REACHED MAXIMUM NUMBER OF ITERATIONS, CANNOT FIT"
    ParallelLogger::logWarn(paste("GLM fitting issue: ", status))
  } else {
    status <- "OK"
    coefficients <- stats::coef(fit) # not sure this is stats??
    ParallelLogger::logInfo(paste("GLM fit status: ", status))
  }
  
  outcomeModel <- list(
    coefficients = coefficients, 
    priorVariance = fit$variance,
    log_likelihood = fit$log_likelihood,
    modelType = modelType,
    modelStatus = status
  )

  if(modelType == "cox" || modelType == "survival") {
    baselineHazard <- tryCatch({survival::survfit(fit, type = "aalen")},
      error = function(e) {ParallelLogger::logInfo(e); return(NULL)})
    if(is.null(baselineHazard)){
      ParallelLogger::logInfo('No baseline hazard function returned')
    }
    outcomeModel$baselineHazard <- baselineHazard
  }
  class(outcomeModel) <- "plpModel"
  
  #get CV
  if(modelType == "logistic" && useCrossValidation){
    outcomeModel$cv <- getCV(cyclopsData, labels, cvVariance = fit$variance)
  }
  
  return(outcomeModel)

}

modelTypeToCyclopsModelType <- function(modelType, stratified=F) {
  if (modelType == "logistic") {
    if (stratified)
      return("clr")
    else
      return("lr")
  } else if (modelType == "poisson") {
    if (stratified)
      return("cpr")
    else
      return("pr")
  } else if (modelType == "cox") {
    return("cox")
  } else {
    ParallelLogger::logError(paste("Unknown model type:", modelType))
    stop()
  }
  
}



getCV <- function(
  cyclopsData, 
  labels,
  cvVariance
)
{
  fixed_prior <- Cyclops::createPrior("laplace", variance = cvVariance, useCrossValidation = FALSE)
  
  result <- lapply(1:max(labels$index), function(i) {
    hold_out <- labels$index==i
    weights <- rep(1.0, Cyclops::getNumberOfRows(cyclopsData))
    weights[hold_out] <- 0.0
    subset_fit <- suppressWarnings(Cyclops::fitCyclopsModel(cyclopsData,
      prior = fixed_prior,
      weights = weights))
    predict <- stats::predict(subset_fit)
    
    auc <- aucWithoutCi(predict[hold_out], labels$y[hold_out])
    
    predCV <- cbind(labels[hold_out,c('rowId','index','y')], 
      value = predict[hold_out])
    predCV$outcomeCount <- predCV$y
    
    return(list(out_sample_auc = auc,
      predCV = predCV,
      log_likelihood = subset_fit$log_likelihood,
      log_prior = subset_fit$log_prior,
      coef = stats::coef(subset_fit)))
  })
  
  return(result) 
  
}

getVariableImportance <- function(modelTrained, trainData){
  varImp <- data.frame(
    covariateId = names(modelTrained$coefficients)[names(modelTrained$coefficients)!='(Intercept)'],
    value = modelTrained$coefficients[names(modelTrained$coefficients)!='(Intercept)']
  )

if(sum(abs(varImp$value)>0)==0){
  ParallelLogger::logWarn('No non-zero coefficients')
  varImp <- NULL
} else {
  ParallelLogger::logInfo('Creating variable importance data frame')
  
  trainData$covariateData$varImp <- varImp
  on.exit(trainData$covariateData$varImp <- NULL, add = T)
  
  varImp <- trainData$covariateData$covariateRef %>% 
    dplyr::left_join(trainData$covariateData$varImp) %>%
    dplyr::mutate(covariateValue = ifelse(is.na(.data$value), 0, .data$value)) %>%
    dplyr::select(-.data$value) %>%
    dplyr::arrange(-abs(.data$covariateValue)) %>%
    dplyr::collect()
}
  
  return(varImp)
}


filterCovariateIds <- function(param, covariateData){
  if ( (length(param$includeCovariateIds) != 0) & (length(param$excludeCovariateIds) != 0)) {
    covariates <- covariateData$covariates %>% 
      dplyr::filter(.data$covariateId %in% param$includeCovariateIds)  %>% 
      dplyr::filter(!.data$covariateId %in% param$excludeCovariateIds)
  } else if ( (length(param$includeCovariateIds) == 0) & (param$length(excludeCovariateIds) != 0)) { 
    covariates <- covariateData$covariates %>% 
      dplyr::filter(!.data$covariateId %in% param$excludeCovariateIds)
  } else if ( (length(param$includeCovariateIds) != 0) & (length(param$excludeCovariateIds) == 0)) {
    covariates <- covariateData$covariates %>% 
      dplyr::filter(.data$covariateId %in% param$includeCovariateIds)
  } else {
    covariates <- covariateData$covariates
  }
  return(covariates)
}
