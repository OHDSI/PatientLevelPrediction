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
predict.plp <- function(plpModel,population, plpData, silent=F, ...){
  covariates <- limitCovariatesToPopulation(plpData$covariates, ff::as.ff(population$rowId))
  prediction <- predictProbabilities(plpModel$model, population, covariates, silent=silent)
  
  return(prediction)
}

# for gxboost
predict.xgboost <- function(plpModel,population, plpData, silent=F, ...){ 
  result <- toSparseM(plpData, population, map=plpModel$covariateMap,silent=silent)
  data <- result$data[population$rowId,]
  prediction <- data.frame(rowId=population$rowId, 
                           value=xgboost::predict(plpModel$model, data)
                           )
  
  prediction <- merge(population, prediction, by='rowId')
  prediction <- prediction[,colnames(prediction)%in%c('rowId','outcomeCount','indexes', 'value')] # need to fix no index issue
  attr(prediction, "metaData") <- list(predictionType = "binary") 
  return(prediction)
  
}

# TODO: edit caret methods (or remove as slow - replace with python)
# caret model prediction 
predict.python <- function(plpModel, population, plpData,  silent=F){
  
  dense <- plpModel$dense
  
  # restrict plpdata to populaiton:
  # create vector of 1s and 0s indicating whether the plpData row is in the populaiton
  rowIds <- read.table(file.path(plpData$covariates,'rowId.txt'))[,1]
  rowData <- rep(0, length(rowIds))
  rowData[rowIds%in%population$rowId] <- 1
  write.table(rowData, file.path(plpData$covariates,'dataRows.txt'), col.names=F, row.names = F)
  
  
  #load python model mapping.txt
  # create missing/mapping using plpData$covariateRef
  originalCovs <- read.table(file.path(plpModel$model,'covs.txt'), header = T) 
  originalCovs$rowInd.org <- (1:nrow(originalCovs))-1 # python index starts at 0
  newCovs <- ff::as.ram(plpData$covariateRef)
  newCovs$rowInd.new <- (1:nrow(newCovs))-1 # python index starts at 0
  
  map <- merge(newCovs, originalCovs, by='covariateId', all.x=T, all.y=T)
  map$rowInd.new[is.na(map$rowInd.new)] <- -1
  map$rowInd.org[is.na(map$rowInd.org)] <- -1
  
  write.table(map[, c('rowInd.new','rowInd.org')], file.path(plpData$covariates, 'mapping.txt'), 
              row.names=F, col.names=F)

  # save popualtion
  if('indexes'%in%colnames(population)){
    write.table(population[,c('rowId','outcomeCount','indexes')], file.path(plpData$covariates, 'population.txt'), 
                row.names=F, col.names=F)
  } else {
    write.table(population[,c('rowId','outcomeCount')], file.path(plpData$covariates, 'population.txt'), 
                row.names=F, col.names=F)
  }
  
  # data, model, output
  output <- file.path(getwd(), 'results')
  if(!dir.exists(output)) dir.create(output)
  system(paste(system.file(package='PatientLevelPrediction', 'executionables',
         'win64','python','python_predict.exe'), gsub('/','\\\\',plpData$covariates), 
         gsub('/','\\\\',plpModel$model), gsub('/','\\\\',output), dense )  )
  
  prediction <- read.csv(file.path(output, 'new_pred.txt'), header=F)
  if(ncol(prediction)==4){
    colnames(prediction) <- c('rowId','outcomeCount','indexes', 'value')
  } else {
    colnames(prediction) <- c('rowId','outcomeCount', 'value')
  }
  
  # TODO delete results
  
  return(prediction)
}


# default h2o prediction
predict.h2o <- function(plpModel, population, plpData,  silent=F){
  # check plpData is libsvm format:
  if('ffdf'%in%class(plpData$covariates) || class(plpData)!='plpData.libsvm')
    stop('H2o models require plpData in libsvm format')
  if(!file.exists(file.path(plpData$covariates,'covariate.txt')))
    stop('Cannot find libsvm file')
  
  
  #load libSvm file and use index to extract test data?
  h2oData <- restrictLibsvmToPopulation(plpData, population)
  
  # any missing covariates will be filled with mean values from train set
  # TODO add option of replacing with zeros?
  
  value <- h2o::h2o.predict(plpModel$model, h2oData)
  #writeLines(paste(colnames(value), sep='-',collapse='-'))
  #writeLines(paste(as.data.frame(value)[1,], sep='-',collapse='-'))
  pred <- data.frame(rowId=as.data.frame(h2oData[,'rowId']), value=as.data.frame(value)[,3])
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



#' Apply train model on new data
#' 
#' Apply a Patient Level Prediction model on Patient Level Prediction Data
#' and get the predicted risk in [0,1] for each person in the popualtion.
#' If the user inputs a population with an outcomeCount column then the 
#' function also returns the evaluation of the prediction (AUC, brier score, calibration)
#'
#' @param population   The population of people who you want to predict the risk for
#' @param plpData    The plpData for the population
#' @param plpModel   The trained PatientLevelPrediction model
#' @param logConnection        A connection to output any logging during the process
#' @param databaseOutput Whether to save the details into the prediction database
#'
#' @examples
#' # load the model and data
#' plpData <- loadPlpData('C:/plpdata')
#' plpModel <- loadPlpModel('C:/plpmodel')
#' 
#' # use the same population settings as the model:
#' populationSettings <- plpModel$populationSettings
#' populationSettings$plpData <- plpData
#' population <- do.call(createStudyPopulation, populationSettings)
#' 
#' # get the prediction:
#' prediction <- applyModel(population, plpData, plpModel)$prediction
#'
#' @export
applyModel <- function(population, plpData, plpModel,
                       logConnection=NULL,
                       databaseOutput=NULL,
                       silent=F){
#check input:
  if(is.null(population))
    stop('NULL popualtion')
  if(class(plpData)!='plpData')
    stop('Incorrect plpData class')
  if(class(plpModel)!='plpModel')
    stop('Incorrect plpModel class')
  if(!ifelse(is.null(logConnection),TRUE,"connection"%in%class(logConnection)))
    stop('logConnection not NULL or a connection')
  
  # log the trained model details
  # TODO
  
  # get prediction counts:
  peopleCount <- nrow(population)
    
  start.pred <- Sys.time()
  if(!is.null(logConnection))
    cat('Starting Prediction at ', Sys.time(), 'for ', peopleCount, ' people', file=logConnection)
  if(!silent)
    writeLines(paste('Starting Prediction ', Sys.time(), 'for ', peopleCount, ' people') )  

  prediction <- plpModel$transform(plpData=plpData, population=population)  
  
  if(!is.null(logConnection)){
    cat('Prediction completed at ', Sys.time(), file=logConnection)
    cat('Took: ', start.pred - Sys.time(), file=logConnection)
  }
  if(!silent)
    writeLines(paste('Prediction completed at ', Sys.time(), ' taking ', start.pred-Sys.time()) )  
  
  
  if(!'outcomeCount'%in%colnames(prediction))
    return(list(prediction=prediction))
  
  if(!is.null(logConnection)){
    cat('Starting evaluation at ', Sys.time(), file=logConnection)
    }
  if(!silent)
    writeLines(paste('Starting evaulation at ', Sys.time()) )  
  
  performance <- evaluatePlp(prediction)
  
  if(!is.null(logConnection)){
    cat('Evaluation completed at ', Sys.time(), file=logConnection)
    cat('Took: ', start.pred - Sys.time(), file=logConnection)
  }
  if(!silent)
    writeLines(paste('Evaluation completed at ', Sys.time(), ' taking ', start.pred-Sys.time()) )  
  
  result <- list(prediction=prediction,
                 performance = performance)
}