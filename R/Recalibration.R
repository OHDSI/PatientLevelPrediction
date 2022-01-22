# @file Recalibration.R
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


#' recalibratePlpRefit
#'
#' @description
#' Train various models using a default parameter gird search or user specified parameters
#'
#' @details
#' The user can define the machine learning model to train (regularised logistic regression, random forest,
#' gradient boosting machine, neural network and )
#' 
#' @param plpModel                         The trained plpModel (runPlp$model)
#' @param newPopulation                    The population created using createStudyPopulation() who will have their risks predicted
#' @param newData                          An object of type \code{plpData} - the patient level prediction
#'                                         data extracted from the CDM.
#' @return
#' An object of class \code{runPlp} that is recalibrated on the new data
#'
#' @export
recalibratePlpRefit <- function(
  plpModel,
  newPopulation, 
  newData
){
  if (is.null(newPopulation))
    stop("NULL population")
  if (class(newData) != "plpData")
    stop("Incorrect plpData class")
  if (class(plpModel) != "plpModel")
    stop("plpModel is not of class plpModel")
  
  #get selected covariates
  includeCovariateIds <- plpModel$covariateImportance %>% 
    dplyr::filter(.data$covariateValue != 0) %>% 
    dplyr::select(.data$covariateId) %>% 
    dplyr::pull()
  
  # check which covariates are included in new data
  containedIds <- newData$covariateData$covariateRef %>% dplyr::collect()
  noShrinkage <- intersect(includeCovariateIds, containedIds$covariateId)
  
  # add intercept
  noShrinkage <- append(noShrinkage, 0, 0)
  
  setLassoRefit <- setLassoLogisticRegression(
    includeCovariateIds = includeCovariateIds,
    noShrinkage = noShrinkage
  )
  
  newData$labels <- newPopulation #%>% 
    #dplyr::select(
    #  .data$rowId, 
    #  .data$cohortStartDate,
    #  .data$outcomeCount, 
     # .data$survivalTime
      #)
  
  newData$folds <- data.frame(
    rowId = newData$labels$rowId, 
    index = sample(2, length(newData$labels$rowId), replace = T)
    )
  
  newModel <- fitPlp(
    trainData = newData, 
    modelSettings = setLassoRefit,
    analysisId = 'recalibrationRefit'
    )
  
  newModel$prediction$evaluationType <- 'recalibrationRefit'

  oldPred <- predictPlp(
    plpModel = plpModel, 
    plpData = newData, 
    population = newPopulation, 
    timepoint = 0
    )
  
  oldPred$evaluationType <- 'validation'
  
  prediction <- rbind(
    oldPred, 
    newModel$prediction[, colnames(oldPred)]
    )

  if(!is.null(newModel$covariateImportance)){
    adjust <- newModel$covariateImportance %>% 
      dplyr::filter(.data$covariateValue != 0) %>% 
      dplyr::select(
        .data$covariateId, 
        .data$covariateValue
      )
  } else{
    adjust <- c()
  }
  
  newIntercept <- newModel$model$coefficients[names(newModel$model$coefficients) == '(Intercept)']
  
  attr(prediction, "metaData")$recalibratePlpRefit <- list(adjust = adjust, newIntercept = newIntercept)
  
  return(prediction)
}


#' recalibratePlp
#'
#' @description
#' Train various models using a default parameter gird search or user specified parameters
#'
#' @details
#' The user can define the machine learning model to train (regularised logistic regression, random forest,
#' gradient boosting machine, neural network and )
#' 
#' @param prediction                      A prediction dataframe
#' @param analysisId                      The model analysisId
#' @param typeColumn                      The column name where the strata types are specified
#' @param method                          Method used to recalibrate ('recalibrationInTheLarge' or 'weakRecalibration' )
#' @return
#' An object of class \code{runPlp} that is recalibrated on the new data
#'

#' @export
recalibratePlp <- function(prediction, analysisId, typeColumn = 'evaluationType',
                           method = c('recalibrationInTheLarge', 'weakRecalibration')){
  # check input:
    if (class(prediction) != 'data.frame')
      stop("Incorrect prediction") 
  
  if(!method  %in% c('recalibrationInTheLarge', 'weakRecalibration'))
    stop("Unknown recalibration method type. must be of type: recalibrationInTheLarge, weakRecalibration")
  
  
  prediction <- do.call(method, list(prediction = prediction, columnType = typeColumn))
  
  return(prediction)
  
}



recalibrationInTheLarge <- function(prediction, columnType = 'evaluationType'){
  
  if(attr(prediction, "metaData")$modelType == 'binary'){
    misCal <- calibrationInLarge(prediction)
    obsOdds <- misCal$observedRisk/ (1-misCal$observedRisk)
    predOdds <- misCal$meanPredictionRisk/ (1 -  misCal$meanPredictionRisk)
    correctionFactor <- log(obsOdds / predOdds)
    
    recalibrated <- prediction
    recalibrated$value = logFunct(inverseLog(recalibrated$value) + correctionFactor)
    
    recalibrated[,columnType] <- 'recalibrationInTheLarge'
    prediction <- rbind(prediction, recalibrated)
    attr(prediction, 'metaData')$recalibrationInTheLarge = list(correctionFactor = correctionFactor)
    
    return(prediction)
  }
  
  if(attr(prediction, "metaData")$modelType == 'survival'){

    ParallelLogger::logError('Survival recal in the large not currently available')
  }
  
  
}


weakRecalibration <- function(prediction, columnType = 'evaluationType'){
  
  # if binary:
  if(attr(prediction, "metaData")$modelType == 'binary'){
    recalibrated <- prediction
    recalibrated$value[recalibrated$value==0] <- 0.000000000000001
    recalibrated$value[recalibrated$value==1] <- 1-0.000000000000001
    
    y <- ifelse(recalibrated$outcomeCount>0, 1, 0)
    inverseLog <- inverseLog(recalibrated$value)
    refit <- suppressWarnings(stats::glm(y ~ inverseLog, family = 'binomial'))
    
    recalibrated$value <- logFunct((inverseLog * refit$coefficients[2]) + refit$coefficients[1])
    
    recalibrated[,columnType] <- 'weakRecalibration'
    prediction <- rbind(prediction, recalibrated)
    attr(prediction, 'metaData')$weakRecalibration = list(
      adjustGradient = refit$coefficients[2], 
      adjustIntercept = refit$coefficients[1]
      )
    
    return(prediction)
  } 
  
  # add if survival
  if(attr(prediction, "metaData")$modelType == 'survival'){
    
    recalibrated <- prediction
    
    baseline <- ifelse(is.null(attr(recalibrated, "baselineHazard")), 0.9, attr(recalibrated, "baselineHazard"))
    ParallelLogger::logInfo(paste0('recal initial baseline hazard: ',baseline))
    
    offset <- ifelse(is.null(attr(recalibrated, "offset")), 0, attr(recalibrated, "offset"))
    ParallelLogger::logInfo(paste0('recal initial offset: ',offset))
    
    timepoint <- ifelse(is.null(attr(recalibrated, "timePoint")), 365, attr(recalibrated, "timePoint"))
    ParallelLogger::logInfo(paste0('recal initial timepoint: ',timepoint))
    
    if(!is.null(baseline)){
      lp <- log(log(1-recalibrated$value)/log(baseline)) + offset
    } else{
      lp <- log(recalibrated$value)
    }
    
    
    t <- apply(cbind(recalibrated$daysToCohortEnd, recalibrated$survivalTime), 1, min)
    y <- ifelse(recalibrated$outcomeCount>0,1,0)  # observed outcome
    y[t>timepoint] <- 0
    t[t>timepoint] <- timepoint
    S<- survival::Surv(t, y) 
    #### Intercept + Slope recalibration
    f.slope <- survival::coxph(S~lp)
    h.slope <- max(survival::basehaz(f.slope)$hazard)  # maximum OK because of prediction_horizon
    lp.slope <- stats::predict(f.slope)
    recalibrated$value <- 1-exp(-h.slope*exp(lp.slope))
    # 1-h.slope^exp(lp.slope)
    
    
    recalibrated[,columnType] <- 'weakRecalibration'
    prediction <- rbind(prediction, recalibrated)
    attr(prediction, 'metaData')$weakRecalibration = list(
      adjustGradient = f.slope$coefficients['lp'], 
      adjustIntercept = h.slope
    )
    
    return(prediction)
    
  } 
  
} 

logFunct <- function(values){
  return(1/(1 + exp(0 - values)))
}

inverseLog <- function(values){
  res <- log(values/(1-values))
  return(res)
}

