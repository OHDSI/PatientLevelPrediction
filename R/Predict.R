# @file Predict.R
#
# Copyright 2016 Observational Health Data Sciences and Informatics
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


# default patient level prediction prediction  
predict.plp <- function(plpModel,population, plpData, dirPath,silent=F, ...){
  covariates <- limitCovariatesToPopulation(plpData$covariates, ff::as.ff(population$rowId))
  prediction <- predictProbabilities(plpModel$model, population, covariates, silent=silent)
  
  return(prediction)
}

# TODO: edit caret methods (or remove as slow - replace with python)
# caret model prediction 
predict.caret <- function(plpModel, population, plpData, dirPath,silent=F, ...){
  if(!silent) writeLines('Predicting on validation set...')
  # convert plpData to matrix:
  covariates <- ff::clone(plpData$covariates)
  # if plpModel$features in not null filter non features from covariates
  
  # now convert into matrix using reshape2::dcast
  if(!silent)  writeLines('Converting data into matrix...')
  plpData.matrix <- reshape2::dcast(ff::as.ram(covariates), rowId~covariateId, value.var='covariateValue', fill=0)
  
  # add people with no covarites:
  ppl <- ff::as.ram(plpData$cohorts$rowId)
  miss.ppl <- ppl[!ppl%in%plpData.matrix$rowId]
  if(length(miss.ppl)>0){
    cov.add <- matrix(rep(0, (ncol(plpData.matrix)-1)*length(miss.ppl)   ), ncol=(ncol(plpData.matrix)-1))
    cov.add <- data.frame(miss.ppl,cov.add)
    colnames(cov.add) <- colnames(plpData.matrix)
    plpData.matrix <-  rbind(plpData.matrix, cov.add)
  }
  
  # convert the columns to have X
  colnames(plpData.matrix)[!colnames(plpData.matrix)%in%c('rowId')] <- paste0('X',colnames(plpData.matrix)[!colnames(plpData.matrix)%in%c('rowId')])
  
  # remove features not in model:
  if(!"ksvm"%in%class(plpModel$model$finalModel))
    modelCoef <-plpModel$model$finalModel$xNames
  if("ksvm"%in%class(plpModel$model$finalModel))
    modelCoef <- colnames(plpData.matrix) #ifelse(length(plpModel$model$coefnames)==0, colnames(plpData.matrix), plpModel$model$coefnames)
  
  if(!silent)  (paste0('model: ', length(modelCoef), ' -- data: ', ncol(plpData.matrix)))
  plpData.matrix <- plpData.matrix[,colnames(plpData.matrix)%in%c(modelCoef,'rowId')]
  
  missCoef <- modelCoef[!modelCoef%in%colnames(plpData.matrix)]
  if(length( missCoef )>0){
    writeLines(paste0('missing these covariates: ', paste(missCoef, collapse=',', sep=',')))
    addval <- matrix(rep(0, length( missCoef )*nrow(plpData.matrix)), ncol=length( missCoef))
    colnames(addval) <- missCoef
    plpData.matrix <- cbind(plpData.matrix, addval)
  }
  if(!silent)  writeLines(paste0('model: ', length(modelCoef), ' -- data: ', ncol(plpData.matrix[,-1])))
  
  
  #writeLines(paste(plpModel$model$finalModel$xNames, collapse=',',sep=','))
  
  #writeLines(paste(colnames((plpData.matrix[,-1])), collapse=',',sep=','))
  
  resp <- 'raw'
  if(class(plpModel$model$finalModel)=="ksvm")
    resp <- 'probabilities'
  prediction <- predict(plpModel$model$finalModel, plpData.matrix[,-1], type=resp)
  ##prediction <- ffdf(rowId = as.ff(plpData.matrix$rowId), value = as.ff(prediction))
  prediction <- data.frame(rowId = plpData.matrix$rowId, value = prediction[,1])
  ##writeLines(paste(colnames(prediction), sep='', collapse='_'))
  if(!silent)  writeLines('Prediction complete...')
  
  return(prediction)
}


# default h2o prediction
predict.h2o <- function(plpModel, population, plpData, dirPath, silent=F){
  covariates <- ff::clone(plpData$covariates)
  
  #load libSvm file and use index to extract test data?
  cov.h2o <- h2o::h2o.importFile(path = file.path(dirPath,'libSVM','plpData.txt'))
  rowIds <- read.table(file.path(dirPath,'libSVM','rowId.txt'))[,1]
  if(nrow(cov.h2o)!=length(rowIds)) writeLines('dimension mismatch')

  value <- h2o::h2o.predict(plpModel$model, cov.h2o[,-1])
  #writeLines(paste(colnames(value), sep='-',collapse='-'))
  #writeLines(paste(as.data.frame(value)[1,], sep='-',collapse='-'))
  pred <- data.frame(rowId=rowIds, value=as.data.frame(value)[,3])
  #writeLines(paste(pred[1,], sep='-',collapse='-'))
  #writeLines(paste(colnames(ff::as.ram(plpData$cohorts)), sep='-',collapse='-'))
  prediction <- merge(population, pred, all.x=T)
  prediction$value[is.na(prediction$value)] <- 0
  #writeLines(paste(prediction[1,], sep='-',collapse='-'))
  return(prediction)
}



predict.knn <- function(plpData, population, plpModel,silent=T, ...){
  covariates <- limitCovariatesToPopulation(plpData$covariates, ff::as.ff(population$rowId))
  prediction <- BigKnn::predictKnn(covariates = covariates,
                                   indexFolder = plpModel$model,
                                   k = plpModel$modelSettings$modelParameters$k,
                                   weighted = TRUE)
  
  # return the cohorts as a data frame with the prediction added as 
  # a new column with the column name 'value'
  prediction <- merge(population, prediction, by='rowId', 
                      all.x=T, fill=0)
  prediction$value[is.na(prediction$value)] <- 0
  
  return(prediction)
}





#' Create predictive probabilities
#'
#' @details
#' Generates predictions for the population specified in plpData given the model.
#'
#' @return
#' The value column in the result data.frame is: logistic: probabilities of the outcome, poisson:
#' Poisson rate (per day) of the outome, survival: hazard rate (per day) of the outcome.
#'
#' @param predictiveModel   An object of type \code{predictiveModel} as generated using
#'                          \code{\link{fitPredictiveModel}}.
#' @param plpData           An object of type \code{plpData} as generated using
#'                          \code{\link{getDbPlpData}}.
#' @export
predictProbabilities <- function(predictiveModel, population, covariates, silent=F) {
  start <- Sys.time()


  prediction <- predictFfdf(predictiveModel$coefficients,
                            population,
                            covariates,
                            predictiveModel$modelType)
  prediction$time <- NULL
  attr(prediction, "modelType") <- predictiveModel$modelType
  attr(prediction, "cohortId") <- attr(population, "metadata")$cohortId
  attr(prediction, "outcomeId") <- attr(population, "metadata")$outcomeId

  delta <- Sys.time() - start
  if(!silent) writeLines(paste("Prediction took", signif(delta, 3), attr(delta, "units")))
  return(prediction)
}

#' Generated predictions from a regression model
#'
#' @param coefficients   A names numeric vector where the names are the covariateIds, except for the
#'                       first value which is expected to be the intercept.
#' @param outcomes       A data frame or ffdf object containing the outcomes with predefined columns
#'                       (see below).
#' @param covariates     A data frame or ffdf object containing the covariates with predefined columns
#'                       (see below).
#' @param modelType      Current supported types are "logistic", "poisson", or "survival".
#'
#' @details
#' These columns are expected in the outcome object: \tabular{lll}{ \verb{rowId} \tab(integer) \tab
#' Row ID is used to link multiple covariates (x) to a single outcome (y) \cr \verb{time} \tab(real)
#' \tab For models that use time (e.g. Poisson or Cox regression) this contains time \cr \tab
#' \tab(e.g. number of days) \cr } These columns are expected in the covariates object: \tabular{lll}{
#' \verb{rowId} \tab(integer) \tab Row ID is used to link multiple covariates (x) to a single outcome
#' (y) \cr \verb{covariateId} \tab(integer) \tab A numeric identifier of a covariate \cr
#' \verb{covariateValue} \tab(real) \tab The value of the specified covariate \cr }
#'
#' @export
predictFfdf <- function(coefficients, population, covariates, modelType = "logistic") {
  if (!(modelType %in% c("logistic", "poisson", "survival"))) {
    stop(paste("Unknown modelType:", modelType))
  }
  if (class(covariates) != "ffdf") {
    stop("Covariates should be of type ffdf")
  }
  intercept <- coefficients[names(coefficients)%in%'(Intercept)']
  if(length(intercept)==0) intercept <- 0
  coefficients <- coefficients[!names(coefficients)%in%'(Intercept)']
  coefficients <- data.frame(beta = as.numeric(coefficients),
                             covariateId = as.numeric(names(coefficients)))
  coefficients <- coefficients[coefficients$beta != 0, ]
  if(sum(coefficients$beta != 0)>0){
    prediction <- merge(covariates, ff::as.ffdf(coefficients), by = "covariateId")
    prediction$value <- prediction$covariateValue * prediction$beta
    prediction <- bySumFf(prediction$value, prediction$rowId)
    colnames(prediction) <- c("rowId", "value")
    prediction <- merge(population, ff::as.ram(prediction), by = "rowId", all.x = TRUE)
    prediction$value[is.na(prediction$value)] <- 0
    prediction$value <- prediction$value + intercept
  } else{
    warning('Model had no non-zero coefficients so predicted same for all population...')
    prediction <- population
    prediction$value <- rep(0, nrow(population)) + intercept
  }
  if (modelType == "logistic") {
    link <- function(x) {
      return(1/(1 + exp(0 - x)))
    }
    prediction$value <- link(prediction$value)
  } else if (modelType == "poisson" || modelType == "survival") {
    prediction$value <- exp(prediction$value)
  }
  return(prediction)
}

#' Compute sum of values binned by a second variable
#'
#' @param values   An ff object containing the numeric values to be summed
#' @param bins     An ff object containing the numeric values to bin by
#'
#' @examples
#' values <- ff::as.ff(c(1, 1, 2, 2, 1))
#' bins <- ff::as.ff(c(1, 1, 1, 2, 2))
#' bySumFf(values, bins)
#'
#' @export
bySumFf <- function(values, bins) {
  .bySum(values, bins)
  # .Call('PatientLevelPrediction_bySum', PACKAGE = 'PatientLevelPrediction', values, bins)
}
