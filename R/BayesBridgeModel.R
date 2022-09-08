# @file BayesBridge.R
#
# Copyright 2022 Observational Health Data Sciences and Informatics
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

#' setBayesBridge
#' @param nIter          total number of MCMC iterations i.e. burn-ins + saved 
#' posterior draws
#' @param burnIter      number of burn-in iterations
#' @param bridgeExponent exponent for bridge prior
#' @param thin           Number of iterations per saved samples for “thinning” 
#' MCMC to reduce the output size.
#' @param regularizingSlabSize Numeric: Standard deviation of the Gaussian 
#' tail-regularizer on the bridge prior.
#' @param globalScale Numeric: Reference prior for a scale parameter
#' @param coefSamplerType An object to specify the sampling method to update regression coefficients:
#'  \itemize{
#'                                         \item{None}{ Chooses a method via a crude heuristic based on model type}
#'                                         \item{cholesky}{ Cholesky decomposition based sampler}
#'                                         \item{cg}{ Conjugate gradient sampler preferred over Cholesky for linear and logistic models with large+sparse design matrix}
#'                                         \item{hmc}{ Hamilton Monte Carlo for other models}
#'                                         } 
#' @param useGPU        if model should be fit using GPU and cupy
#' 
#' @export
setBayesBridge <- function(nIter=1000,
                           burnIter=250,
                           bridgeExponent=0.5,
                           regularizingSlabSize=1,
                           thin=1,
                           globalScale=0.1,
                           coefSamplerType='cg',
                           seed = NULL,
                           useGPU=FALSE) {
  if(!inherits(bridgeExponent,c("numeric", "integer")))
    stop('alpha exponent must be a numeric value >0 ')
  
  
  if(is.null(seed[1])){
    seed <- as.integer(sample(100000000,1))
  }
  
  param <- list(
    bridgeExponent=bridgeExponent,
    regularizingSlabSize=1
  )
  
  attr(param, 'settings') <- list(
    modelType = 'logistic',
    modelName = "BayesBridge",
    nIter=nIter,
    burnIter=burnIter,
    thin=1,
    globalScale=0.1,
    coefSamplerType=coefSamplerType,
    useGPU=useGPU,
    seed=seed
  )
  
  attr(param, 'saveType') <- 'RtoJson'
  attr(param, 'modelType') <- 'binary'
  
  result <- list(
    fitFunction = "fitBayesBridge",
    param = param
  )
  
  class(result) <- 'modelSettings' 
  
  return(result)
  
}


fitBayesBridge <- function(trainData,
                           modelSettings,
                           search='grid',
                           analysisId) {
  
  param <- modelSettings$param
  
  # add folds to labels if present:
  if(!is.null(trainData$folds)){
    trainData$labels <- merge(trainData$labels, trainData$folds, by = 'rowId')
  }
  
  settings <- attr(param, 'settings')
  ParallelLogger::logInfo(paste0('Training ', settings$modelName))
  
  # convert data into sparse Matrix:
  result <- toSparseM(
    trainData,
    map=NULL
  )
  
  dataMatrix <- result$dataMatrix
  labels <- result$labels
  covariateRef <- result$covariateRef
  
  
  labels <- labels %>% dplyr::arrange(rowId)
  settings <- attr(param, 'settings')
  y <- labels$outcomeCount
  
  bayesbridge <- reticulate::import('bayesbridge')
  np <- reticulate::import('numpy', convert = FALSE)
  
  X <- reticulate::r_to_py(dataMatrix)
  y <- np$array(reticulate::r_to_py(y))
  trials <- np$ones(np$shape(y))
  outcome <- reticulate::tuple(y, trials)

  if (settings$useGPU) {
    cp <- reticulate::import('cupy', convert = FALSE)
    X <- cp$sparse$csr_matrix(X)
  }


  ParallelLogger::logInfo('Running BayesBridge')
  start <- Sys.time()
  model <- bayesbridge$RegressionModel(outcome, X, family='logit', 
                                       add_intercept=T, center_predictor=F)
  prior <- bayesbridge$RegressionCoefPrior(bridge_exponent=param$bridgeExponent,
                                           regularizing_slab_size = param$regularizingSlabSize
  )
  bridge = bayesbridge$BayesBridge(model, prior)

  gibbsOutput = bridge$gibbs(
    n_iter=as.integer(settings$nIter), 
    n_burnin=as.integer(settings$burnIter), 
    thin=as.integer(settings$thin), 
    init=list('global_scale'=settings$globalScale),
    coef_sampler_type=settings$coefSamplerType,
    seed=as.integer(settings$seed)
  )
  mcmcSamples <- gibbsOutput[[1]]
  mcmcInfo <- gibbsOutput[[2]]
  comp <- Sys.time() - start
  ParallelLogger::logInfo("MCMC took ", signif(comp, 3), " ", attr(comp, "units"))
  browser()
  modelTrained <- list()
  modelTrained$coefRaw <- mcmcSamples$coef
  modelTrained$coefficients <- apply(mcmcSamples$coef, 1, median)
  names(modelTrained$coefficients) <- c("(Intercept)", covariateRef$covariateId)
  modelTrained$logLikelihood <- tail(mcmcSamples$logp, 1)
  modelTrained$modelType <- "BayesBridge logistic"
  modelTrained$modelStatus <- "OK"
  attr(modelTrained, "class") <- "plpModel"
  
  ParallelLogger::logTrace('Getting predictions on train set')
  start2 <- Sys.time()
  link <- function(x){
    return(1/(1 + exp(0 - x)))
  }
  betas <- reticulate::r_to_py(modelTrained$coefRaw[-1,])
  intercepts <- np$array(reticulate::r_to_py(modelTrained$coefRaw[1,]))
  if (settings$useGPU) {
    X <- X$get()
  }
  values <- X$dot(betas)
  values <- np$add(values, intercepts)

  values <- link(reticulate::py_to_r(values))
  
  # #get posterior median predictive values
  # for(i in 1:ncol(modelTrained$coefRaw)){
  #   coefficients <- modelTrained$coefRaw[,i]
  #   names(coefficients) <- c("(Intercept)", covariateRef$covariateId)
  #   intercept <- coefficients[names(coefficients)%in%'(Intercept)']
  #   if(length(intercept)==0) intercept <- 0
  #   coefficients <- coefficients[!names(coefficients)%in%'(Intercept)']
  #   beta <- as.numeric(coefficients)
  #   value <- dataMatrix %*% beta
  #   value[is.na(value)] <- 0
  #   value <- value + intercept
  #   link <- function(x){
  #     return(1/(1 + exp(0 - x)))
  #   }
  #   value <- link(value)
  #   out <- cbind(out, value)
  # }
  valueMed <- apply(values, 1, median)
  labels$value <- valueMed
  prediction <- labels
  comp <- Sys.time() - start2
  ParallelLogger::logInfo("Prediction for training set took ", signif(comp, 3), " ", attr(comp, "units"))
  
  
  
  attr(prediction, "metaData")$modelType <- 'binary'
  prediction$evaluationType <- 'Train'
  
  #variable importance
  ParallelLogger::logTrace('Getting variable importance')
  varImp <- data.frame(
    covariateId = as.double(names(modelTrained$coefficients)[names(modelTrained$coefficients)!='(Intercept)']),
    value = modelTrained$coefficients[names(modelTrained$coefficients)!='(Intercept)']
  )
  if(sum(abs(varImp$value)>0)==0){
    ParallelLogger::logWarn('No non-zero coefficients')
    varImp <- NULL
  } else {
    ParallelLogger::logInfo('Creating variable importance data frame')
    
    variableImportance <- covariateRef %>% 
      dplyr::collect()  %>%
      dplyr::left_join(varImp, by = 'covariateId') %>%
      dplyr::mutate(covariateValue = ifelse(is.na(.data$value), 0, .data$value)) %>%
      dplyr::select(-.data$value) %>%
      dplyr::arrange(-abs(.data$covariateValue)) %>%
      dplyr::collect()
  } 
  
  variableImportance[is.na(variableImportance)] <- 0
  covariateRef$covariateValue <- variableImportance %>% 
    dplyr::arrange(covariateId) %>% 
    dplyr::select(covariateValue)
  covariateRef <- covariateRef %>% dplyr::arrange(-abs(.data$covariateValue))
  
  #output result
  comp <- Sys.time() - start
  result <- list(
    model = modelTrained,
    prediction = prediction,
    
    preprocessing = list(
      featureEngineering = attr(trainData$covariateData, "metaData")$featureEngineering,
      tidyCovariates = attr(trainData$covariateData, "metaData")$tidyCovariateDataSettings,
      requireDenseMatrix = F
    ),
    
    modelDesign = PatientLevelPrediction::createModelDesign(
      targetId = attr(trainData, "metaData")$targetId, # added
      outcomeId = attr(trainData, "metaData")$outcomeId, # added
      restrictPlpDataSettings = attr(trainData, "metaData")$restrictPlpDataSettings, # made this restrictPlpDataSettings
      covariateSettings = attr(trainData, "metaData")$covariateSettings,
      populationSettings = attr(trainData, "metaData")$populationSettings, 
      featureEngineeringSettings = attr(trainData, "metaData")$featureEngineeringSettings,
      preprocessSettings = attr(trainData$covariateData, "metaData")$preprocessSettings,
      modelSettings = modelSettings, #modified
      splitSettings = attr(trainData, "metaData")$splitSettings,
      sampleSettings = attr(trainData, "metaData")$sampleSettings
    ),
    
    trainDetails = list(
      analysisId = analysisId, 
      analysisSource = '', #TODO add from model
      developmentDatabase = attr(trainData, "metaData")$cdmDatabaseSchema,
      attrition = attr(trainData, "metaData")$attrition, 
      trainingTime =  paste(as.character(abs(comp)), attr(comp,'units')),
      trainingDate = Sys.Date(),
      modelName = settings$modelType,
      finalModelParameters = list(
        bridgeExponent = param$bridgeExponent, 
        regularizingSlabSize = param$regularizingSlabSize
      )
    ),
    covariateImportance = covariateRef
  )
  
  
  class(result) <- 'plpModel'
  attr(result, 'predictionFunction') <- 'predictBayesBridge'
  attr(result, 'modelType') <- attr(param, 'modelType')
  attr(result, 'saveType') <- attr(param, 'saveType')
  return(result)
}

predictBayesBridge <- function(plpModel, data, cohort, train = FALSE){
    ParallelLogger::logInfo('Starting prediction for test set')
    start <- Sys.time()
    settings <- attr(plpModel$modelDesign$modelSettings$param, 'settings')
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
      covariateRef <- matrixObjects$covariateRef
    } else{
      newData <- data
    }
    
    if(class(plpModel) == 'plpModel'){
      model <- plpModel$model
    } else{
      model <- plpModel
    }
    
    link <- function(x){
      return(1/(1 + exp(0 - x)))
    }
    
    np <- reticulate::import('numpy', convert = FALSE)
    betas <- reticulate::r_to_py(plpModel$model$coefRaw[-1,])
    X <- reticulate::r_to_py(newData)
    intercepts <- np$array(reticulate::r_to_py(plpModel$model$coefRaw[1,]))
    values <- X$dot(betas)
    values <- np$add(values, intercepts)
    values <- link(reticulate::py_to_r(values))
    valueMed <- apply(values, 1, median)
    
    #output
    cohort$value <- valueMed
    prediction <- cohort
    
    attr(prediction, "metaData")$modelType <- 'binary'

    #system time + wrapup
    delta <- Sys.time() - start
    ParallelLogger::logInfo("Prediction took ", signif(delta, 3), " ", attr(delta, "units"))
    return(prediction)
  }
  


