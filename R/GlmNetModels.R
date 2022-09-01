# @file GlmNetModels.R
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

#' setLassoGlmNet
#' 
#' @description
#' Create setting for lasso logistic regression model using glmnet implementation
#'
#' @param nlambda           The number of lambda values
#' @param lambda.min.ratio  Smallest value of lambda, as fraction of lambda.max which is data derived 
#' to have the first estimated model have zero coefficients.
#' @param nfolds            How many cross validation folds to perform, default=3
#' @param parallel          TRUE to use parallelization across folds (cores=nfolds)
#'
#'
#' @export
setLassoGlmNet <- function(nlambda=100,
                           nfolds=3,
                           lambda.min.ratio=0.01,
                           parallel=TRUE){
  if(!inherits(nlambda,c("numeric", "integer")))
    stop('ntrees must be a numeric value >0 ')
  if(sum(nlambda < 1)>0)
    stop('ntrees must be greater that 0 or -1')
  
  param <- list(
    nlambda=nlambda,
    lambda.min.ratio=lambda.min.ratio,
    alpha = 1
  )
  
  attr(param, 'settings') <- list(
    modelType = 'logistic',
    modelName = "Lasso Logistic Regression GLMNet",
    nfolds = nfolds,
    parallel = parallel
  )
  
  attr(param, 'saveType') <- 'RtoJson'
  attr(param, 'modelType') <- 'binary'
  
  result <- list(
    fitFunction = "fitGlmNet",
    param = param
  )
  
  class(result) <- 'modelSettings' 
  
  return(result)
}

#' setRidgeGlmNet
#' 
#' @param lambda.min.ratio  Smallest value of lambda, as fraction of lambda.max which is data derived 
#' to have the first estimated model have zero coefficients.
#' @param nfolds            How many cross validation folds to perform, default=3
#' @param parallel          TRUE to use parallelization across folds (cores=nfolds)
#'
#'
#' @export
setRidgeGlmNet <- function(nlambda=100,
                           nfolds=3,
                           lambda.min.ratio=0.01,
                           parallel=TRUE){
  if(!inherits(nlambda,c("numeric", "integer")))
    stop('ntrees must be a numeric value >0 ')
  if(sum(nlambda < 1)>0)
    stop('ntrees must be greater that 0 or -1')
  
  param <- list(
    nlambda=nlambda,
    lambda.min.ratio=lambda.min.ratio,
    alpha = 0
  )
  
  attr(param, 'settings') <- list(
    modelType = 'logistic',
    modelName = "Ridge Logistic Regression GLMNet",
    nfolds = nfolds,
    parallel = parallel
  )
  
  attr(param, 'saveType') <- 'RtoJson'
  attr(param, 'modelType') <- 'binary'
  
  result <- list(
    fitFunction = "fitGlmNet",
    param = param
  )
  
  class(result) <- 'modelSettings' 
  
  return(result)
}

#' setElasticNet
#' 
#' @description
#' Create setting for ElasticNet regression model using glmnet
#'
#' @param nlambda           The number of lambda values
#' @param lambda.min.ratio  Smallest value of lambda, as fraction of lambda.max which is data derived 
#' to have the first estimated model have zero coefficients.
#' @param nfolds            How many cross validation folds to perform, default=3
#' @param parallel          TRUE to use parallelization across folds (cores=nfolds)
#'
#'
#' @export
setElasticNet <- function(nlambda=100,
                          nfolds=3,
                          lambda.min.ratio=0.01,
                          parallel=TRUE){
  if(!inherits(nlambda,c("numeric", "integer")))
    stop('ntrees must be a numeric value >0 ')
  if(sum(nlambda < 1)>0)
    stop('ntrees must be greater that 0 or -1')
  
  param <- list(
    nlambda=nlambda,
    lambda.min.ratio=lambda.min.ratio,
    alpha = 0.5
  )
  
  attr(param, 'settings') <- list(
    modelType = 'logistic',
    modelName = "Elastic Net",
    nfolds = nfolds,
    parallel = parallel
  )
  
  attr(param, 'saveType') <- 'RtoJson'
  attr(param, 'modelType') <- 'binary'
  
  result <- list(
    fitFunction = "fitGlmNet",
    param = param
  )
  
  class(result) <- 'modelSettings' 
  
  return(result)
}

#' setAdaptiveLasso
#' 
#' @description
#' Create setting for an adaptive Lasso regression model using glmnet
#'
#' @param nlambda           The number of lambda values
#' @param lambda.min.ratio  Smallest value of lambda, as fraction of lambda.max which is data derived 
#' to have the first estimated model have zero coefficients.
#' @param nfolds            How many cross validation folds to perform, default=3
#' @param parallel          TRUE to use parallelization across folds (cores=nfolds)
#'
#'
#' @export
setAdaptiveLasso <- function(nlambda=100,
                             nfolds=3,
                             lambda.min.ratio=0.01,
                             parallel=TRUE){
  if(!inherits(nlambda,c("numeric", "integer")))
    stop('ntrees must be a numeric value >0 ')
  if(sum(nlambda < 1)>0)
    stop('ntrees must be greater that 0 or -1')
  
  param <- list(
    nlambda=nlambda,
    lambda.min.ratio=lambda.min.ratio,
    alpha = 1
  )
  
  attr(param, 'settings') <- list(
    modelType = 'logistic',
    modelName = "Adaptive Lasso",
    nfolds = nfolds,
    parallel = parallel,
    adaptive = TRUE
  )
  
  attr(param, 'saveType') <- 'RtoJson'
  attr(param, 'modelType') <- 'binary'
  
  result <- list(
    fitFunction = "fitGlmNet",
    param = param
  )
  
  class(result) <- 'modelSettings' 
  
  return(result)
}

#' setAdaptiveElasticNet
#' 
#' @description
#' Create setting for an adaptive Lasso regression model using glmnet
#'
#' @param nlambda           The number of lambda values
#' @param lambda.min.ratio  Smallest value of lambda, as fraction of lambda.max which is data derived 
#' to have the first estimated model have zero coefficients.
#' @param nfolds            How many cross validation folds to perform, default=3
#' @param parallel          TRUE to use parallelization across folds (cores=nfolds)
#'
#'
#' @export
setAdaptiveElasticNet <- function(nlambda=100,
                                  nfolds=3,
                                  lambda.min.ratio=0.01,
                                  parallel=TRUE){
  if(!inherits(nlambda,c("numeric", "integer")))
    stop('ntrees must be a numeric value >0 ')
  if(sum(nlambda < 1)>0)
    stop('ntrees must be greater that 0 or -1')
  
  param <- list(
    nlambda=nlambda,
    lambda.min.ratio=lambda.min.ratio,
    alpha = 0.5
  )
  
  attr(param, 'settings') <- list(
    modelType = 'logistic',
    modelName = "Adaptive Elastic Net",
    nfolds = nfolds,
    parallel = parallel,
    adaptive = TRUE
  )
  
  attr(param, 'saveType') <- 'RtoJson'
  attr(param, 'modelType') <- 'binary'
  
  result <- list(
    fitFunction = "fitGlmNet",
    param = param
  )
  
  class(result) <- 'modelSettings' 
  
  return(result)
}


#' fitGlmNet
#' 
#' @description 
#' fits a glmnet model
#' 
#' @param trainData       The training data received through fitPlp
#' @param modelSettings   The model settings objects
#' @param search          Hyperparameter search strategy
#' @param analysisId      Analysis Id
#' 
fitGlmNet <- function(trainData,
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
  
  start <- Sys.time()
  
  cvResult <- cvGlmNet(
    dataMatrix, 
    labels, 
    param = param, 
    covariateMap = result$covariateMap
  )
  comp <- Sys.time() - start

  variableImportance <- data.frame(
    covariateId = as.double(cvResult$model$coefficients$covariateIds[cvResult$model$coefficients$covariateIds!='(Intercept)']),
    covariateValue = cvResult$model$coefficients$betas[cvResult$model$coefficients$covariateIds!='(Intercept)']
  )
  covariateRef <- merge(covariateRef, variableImportance, all.x = T, by = 'covariateId')
  covariateRef$included <- 0
  covariateRef$included[covariateRef$covariateValue != 0.0] <- 1

  result <- list(
    model = cvResult$model,
    
    preprocess = list(
      featureEngineering = attr(trainData$covariateData, "metaData")$featureEngineering,#learned mapping
      tidyCovariates = attr(trainData$covariateData, "metaData")$tidyCovariateDataSettings,  #learned mapping
      requireDenseMatrix = F
    ),
    
    prediction = cvResult$prediction,
    
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
        lambda = cvResult$model$lambda
      ),
      hyperParamSearch = cvResult$hyperParamSearch
    ),
    
    covariateImportance = covariateRef
  )
  
  class(result) <- 'plpModel'
  attr(result, 'predictionFunction') <- 'predictGlmNet'
  attr(result, 'modelType') <- attr(param, 'modelType')
  attr(result, 'saveType') <- attr(param, 'saveType')
  return(result)
}

#' cvGlmNet
#' 
#' @description 
#' performs hyperparameter search using cross validation for glmnet models
#' 
#' @param dataMatrix       R sparse matrix object
#' @param labels           population dataframe with the labels
#' @param hyperParamGrid   Hyperparameters to try
#' @param covariateMap     Dataframe that maps covariates to columns in the sparse matrix
#' 
cvGlmNet <- function(dataMatrix,
                     labels,
                     param,
                     covariateMap) {
  
  settings <- attr(param, 'settings')
  y <- labels$outcomeCount
  
  
  if (settings$parallel) {
    doParallel::registerDoParallel(cores = settings$nfolds)
  }
  dataMatrix@Dimnames[[2]] <- as.character(covariateMap$covariateId)
  nvars <- dim(dataMatrix)[[2]]
  if (settings$adaptive) {
    ridgeCv <- glmnet::cv.glmnet(dataMatrix, y=y, alpha=0, family='binomial',
                                  trace.it=1, nfolds=settings$nfolds, 
                                  lambda.min.ratio=param$lambda.min.ratio,
                                  foldId=labels$index, parallel=settings$parallel,
                                  type.measure = 'auc')
    ridgeCoefs <- as.numeric(coef(ridgeCv, s = ridgeCv$lambda.min))[-1] # no intercept
    penaltyFactor <- 1/abs(ridgeCoefs)
  } else {penaltyFactor <- rep(1, nvars)}
  glmFit <- glmnet::cv.glmnet(dataMatrix, y=y, alpha=param$alpha, family='binomial',
                    trace.it = 1, nfolds=settings$nfolds, lambda.min.ratio=param$lambda.min.ratio,
                    foldId=labels$index,
                    parallel=settings$parallel,
                    type.measure = 'auc',
                    keep = TRUE,
                    penalty.factor = penaltyFactor
                    )  
  
  bestLambdaPos <- which(glmFit$lambda == glmFit$lambda.min)
  coefficient <- as.matrix(coef(glmFit))
  coefDf <- data.frame(betas=as.numeric(coefficient),
                       covariateIds=attr(coefficient, 'dimnames')[[1]])
  
  preds <- predict(glmFit, newx=dataMatrix, type='response', s='lambda.min')
  
  prediction <- labels[, !names(labels) %in% c('index', 'rowId')]
  prediction <- prediction %>% dplyr::mutate(rowId=.data$originalRowId) %>%
    dplyr::select(-c('originalRowId'))
  prediction$value <- as.numeric(preds)
  prediction$evaluationType <- 'Train'  
  #extract CV predictions for optimal lambda
  cvPrediction <- labels[, names(labels) != 'index']
  cvPrediction$value <- glmFit$fit.preval[,bestLambdaPos]
  cvPrediction$value <- as.numeric(1/(1+exp(-cvPrediction$value))) # convert to probability
  cvPrediction$evaluationType <- 'CV'
  
  prediction <- rbind(prediction, cvPrediction[,colnames(prediction)])
  
  attr(prediction, 'modelType') <- attr(param, 'modelType')

  results <- list(
    model = list(
      modelObj=glmFit,
      lambda = glmFit$lambda.min,
      coefficients = coefDf
    ),
    prediction=prediction,
    hyperParamSearch = list(
      lambdas = glmFit$lambda,
      auc = glmFit$cvm,
      nzero = glmFit$nzero
    )
  )
  return(results)
}

#' predictGlmNet
#' 
#' @description 
#' predicts a glmnet model
#' 
#' @param plpModel      An object of type \code{predictiveModel} as generated using
#'                          \code{\link{fitPlp}}.
#' @param data         The new plpData containing the covariateData for the new population                       
#' @param cohort       The cohort to calculate the prediction for
#'
#'
predictGlmNet <- function(plpModel,
                          data,
                          cohort) { 
  
  sparseMatrix <- toSparseM(plpData=data,
                           cohort=cohort,
                           map=plpModel$covariateImportance %>% 
                             dplyr::select(.data$columnId, .data$covariateId))

  preds <- predict(plpModel$model$modelObj, newx=sparseMatrix$dataMatrix, type='response',
                   s='lambda.min')
  prediction <- sparseMatrix$labels
  prediction$value <- as.numeric(preds)
  prediction <- prediction %>% 
    dplyr::select(-.data$rowId) %>%
    dplyr::rename(rowId = .data$originalRowId)
  
  attr(prediction, "metaData") <- list(modelType = attr(plpModel, "modelType"))

  return(prediction)
}


