# @file predict.R
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

#' predictPlp
#'
#' @description
#' Predict the risk of the outcome using the input plpModel for the input plpData
#' @details
#' The function applied the trained model on the plpData to make predictions
#' @param plpModel                         An object of type \code{plpModel} - a patient level prediction model
#' @param plpData                          An object of type \code{plpData} - the patient level prediction
#'                                         data extracted from the CDM.
#' @param population                       The population created using createStudyPopulation() who will have their risks predicted or a cohort without the outcome known
#' @param timepoint                        The timepoint to predict risk (survival models only)
#' @return
#' A dataframe containing the prediction for each person in the population with an attribute metaData containing prediction details.
#'

# parent predict that calls the others
#' @export
predictPlp <- function(plpModel, plpData, population, timepoint){
  
  if(is.null(plpModel))
    stop('No model input')
  if(is.null(population))
    stop('No population input')
  if(is.null(plpData))
    stop('No plpData input')
  
  
  # do feature engineering/selection
  if(!is.null(plpModel$preprocessing$featureEngineering)){
    plpData <- do.call(
      applyFeatureengineering, 
      list(
        plpData = plpData,
        settings = plpModel$preprocessing$featureEngineering
      )
    )
    featureEngineering <- T
  } else{
    featureEngineering <- F
  }
  
  ParallelLogger::logTrace('did FE')
  
  if(!is.null(plpModel$preprocessing$tidyCovariates)){
    # do preprocessing
    plpData$covariateData <- do.call(
      applyTidyCovariateData, 
      list(
        covariateData = plpData$covariateData,
        preprocessSettings = plpModel$preprocessing$tidyCovariates
      )
    )
    tidyCovariates <- T
  } else{
    tidyCovariates <- F
  }
  
  ParallelLogger::logTrace('did tidy')
  
  # add timepoint if not missing to population attribute
  if(!missing(timepoint)){
    attr(population, 'timepoint') <- timepoint
  } else{
    timepoint <- attr(population,'metaData')$populationSettings$riskWindowEnd
  }
  
  # apply prediction function
  prediction <- do.call(
    eval(parse(text = attr(plpModel, "predictionFunction"))), 
    list(
      plpModel = plpModel, 
      data = plpData, 
      cohort = population
      )
    )
  
  if(!is.null(attr(prediction, "metaData"))){
    metaData <- attr(prediction, "metaData")
  } else{
    metaData <- list()
  }
  
  # add metaData
  metaData$modelType <- attr(plpModel, 'modelType') #"binary", 
  metaData$targetId <- attr(population,'metaData')$targetId
  metaData$outcomeId <- attr(population,'metaData')$outcomeId
  metaData$timepoint <- timepoint
  
  # added information about running preprocessing/FE
  metaData$tidyCovariates <- tidyCovariates
  metaData$featureEngineering <- featureEngineering
  
  attr(prediction, "metaData") <- metaData
  return(prediction)
}



applyFeatureengineering <- function(
  plpData, 
  settings
){
  
  # if a single setting make it into a list
  if(!is.null(settings$funct)){
    settings <- list(settings)
  }
  
  # add code for implementing the feature engineering
  for(set in settings){
    set$settings$trainData <- plpData
    plpData <- do.call(eval(parse(text = set$funct)), set$settings)
  }
  
  # dont do anything for now
  return(plpData)
  
}


#NEED TO UPDATE....
# fucntion for implementing the pre-processing (normalisation and redundant features removal)
applyTidyCovariateData <- function(
  covariateData,
  preprocessSettings
)
{
  
  if(!FeatureExtraction::isCovariateData(covariateData)){stop("Data not of class CovariateData")}
  
  newCovariateData <- Andromeda::andromeda(covariateRef = covariateData$covariateRef,
    analysisRef = covariateData$analysisRef)
  
  maxs <- preprocessSettings$normFactors
  deleteRedundantCovariateIds <- preprocessSettings$deletedRedundantCovariateIds
  deletedInfrequentCovariateIds <- preprocessSettings$deletedInfrequentCovariateIds
  
  # --- added for speed
  deleteCovariateIds <- c(deleteRedundantCovariateIds,deletedInfrequentCovariateIds)
  temp <- covariateData$covariateRef %>% dplyr::collect()
  allCovariateIds <- temp$covariateId
  covariateData$includeCovariates <- data.frame(covariateId = allCovariateIds[!allCovariateIds%in%deleteCovariateIds])
  Andromeda::createIndex(covariateData$includeCovariates, c('covariateId'),
    indexName = 'includeCovariates_covariateId')
  on.exit(covariateData$includeCovariates <- NULL, add = TRUE)
  # ---
  
  ParallelLogger::logInfo("Removing infrequent and redundant covariates and normalizing")
  start <- Sys.time()       
  
  if(!is.null(maxs)){
    if('bins'%in%colnames(maxs)){
      covariateData$maxes <- dplyr::as_tibble(maxs)  %>% dplyr::rename(covariateId = "bins") %>% 
        dplyr::rename(maxValue = "maxs")
    } else{
      covariateData$maxes <- maxs 
    }
    on.exit(covariateData$maxes <- NULL, add = TRUE)
    
    # --- added for speed
    Andromeda::createIndex(covariateData$maxes, c('covariateId'),
      indexName = 'maxes_covariateId')
    # ---
    
    newCovariateData$covariates <- covariateData$covariates %>%  
      dplyr::inner_join(covariateData$includeCovariates, by='covariateId') %>% # added as join
      dplyr::inner_join(covariateData$maxes, by = 'covariateId') %>%
      dplyr::mutate(value = 1.0*.data$covariateValue/.data$maxValue) %>%
      dplyr::select(-"covariateValue") %>%
      dplyr::rename(covariateValue = "value")
  } else{
    newCovariateData$covariates <- covariateData$covariates %>% 
      dplyr::inner_join(covariateData$includeCovariates, by='covariateId')
  }
  
  # reduce covariateRef
  newCovariateData$covariateRef <- covariateData$covariateRef %>% 
    dplyr::inner_join(covariateData$includeCovariates, by='covariateId')
  
  # adding index for restrict to pop
  Andromeda::createIndex(
    newCovariateData$covariates, 
    c('rowId'),
    indexName = 'ncovariates_rowId'
  )
  
  
  class(newCovariateData) <- "CovariateData"
  
  delta <- Sys.time() - start
  writeLines(paste("Removing infrequent and redundant covariates covariates and normalizing took", signif(delta, 3), attr(delta, "units")))
  
  # return processed data
  return(newCovariateData)
}

#' predict using a logistic regression model
#' 
#' @description
#' Predict risk with a given plpModel containing a generalized linear model.
#' 
#' @param plpModel An object of type \code{plpModel} - a patient level 
#' prediction model
#' @param data An object of type \code{plpData} - the patient level prediction
#' data extracted from the CDM.
#' @param cohort The population dataframe created using
#' /code{createStudyPopulation} who will have their risks predicted or a cohort
#' without the outcome known
#' @export
#' @return A dataframe containing the prediction for each person in the 
#' population
#' @export
predictGlm <- function(plpModel, data, cohort) {
  start <- Sys.time()
  
  ParallelLogger::logInfo("predict risk probabilities using predictGlm")
  
  data$covariateData$coefficients <- plpModel$model$coefficients
  on.exit(data$covariateData$coefficients <- NULL)
  
  prediction <- data$covariateData$covariates %>%
    dplyr::inner_join(data$covariateData$coefficients, by = "covariateId") %>%
    dplyr::mutate(values = .data$covariateValue * .data$coefficient) %>%
    dplyr::group_by(.data$rowId) %>%
    dplyr::summarise(value = sum(.data$values, na.rm = TRUE)) %>%
    dplyr::select("rowId", "value")
  
  prediction <- as.data.frame(prediction)
  prediction <- merge(cohort, prediction, by = "rowId", all.x = TRUE, fill = 0)
  prediction$value[is.na(prediction$value)] <- 0
  prediction$value <- prediction$value + plpModel$model$intercept
  
  if (plpModel$model$finalMapping == "linear") {
    prediction$value <- prediction$value
  } else if (plpModel$model$finalMapping == "logistic") {
    prediction$value <- 1 / (1 + exp(-prediction$value))
  } else if (plpModel$model$finalMapping == "square") {
    prediction$value <- prediction$value^2
  } else if (plpModel$model$finalMapping == "exponential") {
    prediction$value <- exp(prediction$value)
  }
  
  attr(prediction, "metaData")$modelType <- "binary"
  
  delta <- Sys.time() - start
  ParallelLogger::logInfo("Prediction took ", signif(delta, 3), " ", attr(delta, "units"))
  return(prediction)
}

#' createGlmModel
#'
#' @description
#' Create a generalized linear model that can be used in the 
#' PatientLevelPrediction package.
#' @param coefficients A dataframe containing two columns, coefficients and
#' covariateId, both of type numeric. The covariateId column must contain
#' valid covariateIds that match those used in the /code{FeatureExtraction}
#' package.
#' @param intercept A numeric value representing the intercept of the model.
#' @param finalMapping A string representing the final mapping from the 
#' linear predictors to outcome probabilities. For generalized linear models
#' this is the inverse of the link function. Supported values is only
#' "logistic" for logistic regression model at the moment. 
#' @return A model object containing the model and the prediction function.
#' @export
createGlmModel <- function(coefficients,
                           intercept = 0,
                           finalMapping = "logistic") {
  checkIsClass(coefficients, c("data.frame"))
  if (!all(c("covariateId", "coefficient") %in% colnames(coefficients))) {
    stop("coefficients must contain columns covariateId and coefficient")
  }
  checkIsClass(coefficients$covariateId, c("numeric"))
  checkIsClass(coefficients$coefficient, c("numeric"))
  checkHigherEqual(coefficients$covariateId, 0)
  checkIsClass(intercept, c("numeric"))

  checkIsClass(finalMapping, c("character"))
  if (finalMapping != "logistic") {
    stop("finalMapping must be 'logistic'")
  }

  plpModel <- list(
    intercept = intercept,
    coefficients = coefficients,
    finalMapping = finalMapping,
    predictionFunction = "predictGlm"
  )
  plpModel$modelType <- "GLM"
  return(plpModel)
}
