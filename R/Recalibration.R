# @file Recalibration.R
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
#' @param testFraction                    Fraction of data to used for internal validation
#' @return
#' An object of class \code{runPlp} that is recalibrated on the new data
#'

#' @export
recalibratePlpRefit <- function(plpModel,
                                     newPopulation, 
                                     newData, 
                                     testFraction = 0.25){
  if (is.null(newPopulation))
    stop("NULL population")
  if (class(newData) != "plpData")
    stop("Incorrect plpData class")
  if (class(plpModel) != "plpModel")
    stop("plpModel is not of class plpModel")
  
  #get selected covariates
  includeCovariateIds <- plpModel$varImp$covariateId[plpModel$varImp$covariateValue!=0]
  
  # check which covariates are included in new data
  containedIds <- newData$covariateData$covariateRef %>% dplyr::collect()
  noShrinkage <- intersect(includeCovariateIds, containedIds$covariateId)
  # add intercept
  noShrinkage <- append(noShrinkage,0, 0)
  setLassoRefit <- setLassoLogisticRegression(includeCovariateIds = includeCovariateIds,
                                              noShrinkage = noShrinkage)
    
  result <- runPlp(population = newPopulation, analysisId = plpModel$analysisId,
                   plpData = newData, 
                   modelSettings = setLassoRefit, 
                   testFraction = testFraction, 
                   savePlpData = F, savePlpResult = F, savePlpPlots = F, saveEvaluation = F)

  
  recalibrateResult <- evaluatePlp(result$prediction)
  
  recalibrateResult <- formatEvaluation(recalibrateResult = recalibrateResult, 
                                        analysisId = plpModel$analysisId, 
                                        eval = 'recalibrationRefit')
  
  metaData <- attr(result$prediction, "metaData") # new code
  
  prediction <- result$prediction[,c('rowId', 'value')]
  colnames(prediction)[2] <- 'reestimateValue'
  oldPred <- applyModel(population = newPopulation, plpData = newData, 
                        plpModel = plpModel, calculatePerformance = F)
  prediction <- merge(oldPred, prediction, by = 'rowId')
  attr(prediction, "metaData") <- metaData # new code
  
  adjust <- result$covariateSummary[,c('covariateValue', 'covariateId')]
  adjust <- adjust[adjust$covariateValue != 0, ]
  newIntercept <- result$model$model$coefficients[names(result$model$model$coefficients) == '(Intercept)']
  
  
  recalibrateResult$evaluationStatistics <- as.data.frame(recalibrateResult$evaluationStatistics)
  recalibrateResult$evaluationStatistics$Metric <- as.character(recalibrateResult$evaluationStatistics$Metric)
  recalibrateResult$evaluationStatistics$Value <- as.character(recalibrateResult$evaluationStatistics$Value)
  recalibrateResult$evaluationStatistics <- rbind(recalibrateResult$evaluationStatistics,
                                                  data.frame(analysisId = plpModel$analysisId,
                                                             Eval = 'recalibrationRefit',
                                                             Metric = c(as.character(adjust$covariateId),0),
                                                             Value = c(adjust$covariateValue,newIntercept)))
  
  return(list(prediction = prediction,
              performanceEvalution = recalibrateResult)
         )
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
#' @param method                          Method used to recalibrate ('recalibrationInTheLarge' or 'weakRecalibration' )
#' @return
#' An object of class \code{runPlp} that is recalibrated on the new data
#'

#' @export
recalibratePlp <- function(prediction, analysisId,
                           method = c('recalibrationInTheLarge', 'weakRecalibration')){
  # check input:
    if (class(prediction) != 'data.frame')
      stop("Incorrect prediction") 
  
  if(!method  %in% c('recalibrationInTheLarge', 'weakRecalibration'))
    stop("Unknown recalibration method type. must be of type: recalibrationInTheLarge, weakRecalibration")
  
  
  result <- do.call(method, list(prediction = prediction))
  
  recalibrateResult <- evaluatePlp(result$prediction)
  recalibrateResult <- formatEvaluation(recalibrateResult = recalibrateResult, 
                                        analysisId = analysisId, 
                                        eval = method)
  
  result$prediction <- result$prediction[,c('rowId', 'value')]
  colnames(result$prediction)[2] <- paste0(result$type, 'Value')

  metaDataTemp <- attr(prediction, "metaData")
  prediction <- merge(prediction, result$prediction, by = 'rowId')
  attr(prediction, "metaData") <- metaDataTemp
  
  recalibrateResult$evaluationStatistics <- rbind(recalibrateResult$evaluationStatistics,
                                                  data.frame(analysisId = analysisId,
                                                             Eval = result$type,
                                                             Metric = unlist(names(result)[!names(result)%in%c('prediction','type')]),
                                                             Value = unlist(result[!names(result)%in%c('prediction','type')]))
                                                  )
  
  return(list(prediction = prediction,
              performanceEvaluation = recalibrateResult)
  )
  
}



recalibrationInTheLarge <- function(prediction){
  
  if(attr(prediction, "metaData")$predictionType == 'binary'){
    misCal <- calibrationInLarge(prediction)
    obsOdds <- misCal$observedRisk/ (1-misCal$observedRisk)
    predOdds <- misCal$meanPredictionRisk/ (1 -  misCal$meanPredictionRisk)
    correctionFactor <- log(obsOdds / predOdds)
    
    prediction$value = logFunct(inverseLog(prediction$value) + correctionFactor)
    
    return(list(prediction = prediction,
                type = 'recalibrationInTheLarge',
                correctionFactor = correctionFactor))
  }
  
  if(attr(prediction, "metaData")$predictionType == 'survival'){

    ParallelLogger::logError('Survival recal in the large not currently available')
  }
  
  
}


weakRecalibration <- function(prediction){
  
  # if binary:
  if(attr(prediction, "metaData")$predictionType == 'binary'){
    prediction$value[prediction$value==0] <- 0.000000000000001
    prediction$value[prediction$value==1] <- 1-0.000000000000001
    
    y <- ifelse(prediction$outcomeCount>0, 1, 0)
    inverseLog <- inverseLog(prediction$value)
    refit <- suppressWarnings(stats::glm(y ~ inverseLog, family = 'binomial'))
    
    prediction$value <- logFunct((inverseLog * refit$coefficients[2]) + refit$coefficients[1])
    
    return(list(prediction = prediction, 
                type = 'weakRecalibration',
                adjustGradient = refit$coefficients[2], 
                adjustIntercept = refit$coefficients[1]) 
    )
  } 
  
  # add if survival
  if(attr(prediction, "metaData")$predictionType == 'survival'){
    
    baseline <- ifelse(is.null(attr(prediction, "baselineHazard")), 0.9, attr(prediction, "baselineHazard"))
    ParallelLogger::logInfo(paste0('recal initial baseline hazard: ',baseline))
    
    offset <- ifelse(is.null(attr(prediction, "offset")), 0, attr(prediction, "offset"))
    ParallelLogger::logInfo(paste0('recal initial offset: ',offset))
    
    timepoint <- ifelse(is.null(attr(prediction, "timePoint")), 365, attr(prediction, "timePoint"))
    ParallelLogger::logInfo(paste0('recal initial timepoint: ',timepoint))
    
    if(!is.null(baseline)){
      lp <- log(log(1-prediction$value)/log(baseline)) + offset
    } else{
      lp <- log(prediction$value)
    }
    
    
    t <- apply(cbind(prediction$daysToCohortEnd, prediction$survivalTime), 1, min)
    y <- ifelse(prediction$outcomeCount>0,1,0)  # observed outcome
    y[t>timepoint] <- 0
    t[t>timepoint] <- timepoint
    S<- survival::Surv(t, y) 
    #### Intercept + Slope recalibration
    f.slope <- survival::coxph(S~lp)
    h.slope <- max(survival::basehaz(f.slope)$hazard)  # maximum OK because of prediction_horizon
    lp.slope <- stats::predict(f.slope)
    prediction$value <- 1-exp(-h.slope*exp(lp.slope))
    # 1-h.slope^exp(lp.slope)
    
    return(list(prediction = prediction, 
                type = 'weakRecalibration',
                adjustGradient = f.slope$coefficients['lp'], 
                adjustIntercept = h.slope) 
    )
    
  } 
  
} 

logFunct <- function(values){
  return(1/(1 + exp(0 - values)))
}

inverseLog <- function(values){
  res <- log(values/(1-values))
  return(res)
}

#' addRecalibration
#'
#' @description
#' Adds the recalibration results to the main results
#'
#' @details
#' Append the recalibration results into the main results
#' 
#' @param performanceEvaluation           The main result performanceEvaluation
#' @param recalibration                   The recalibration result
#' @return
#' An object of class \code{runPlp} that is recalibrated on the new data
#'
#' @export
addRecalibration <- function(performanceEvaluation, recalibration){
  
  if(!is.null(recalibration$demographicSummary)){
    ParallelLogger::logInfo('Appending recalibration demographicSummary')
    performanceEvaluation$demographicSummary <- rbind(performanceEvaluation$demographicSummary,
                                                      recalibration$demographicSummary)
  }
  
  if(!is.null(recalibration$calibrationSummary )){
    ParallelLogger::logInfo('Appending recalibration calibrationSummary ')
    performanceEvaluation$calibrationSummary  <- rbind(performanceEvaluation$calibrationSummary ,
                                                      recalibration$calibrationSummary )
  }
  
  if(!is.null(recalibration$thresholdSummary )){
    ParallelLogger::logInfo('Appending recalibration thresholdSummary ')
    performanceEvaluation$thresholdSummary  <- rbind(performanceEvaluation$thresholdSummary ,
                                                       recalibration$thresholdSummary )
  }
  
  if(!is.null(recalibration$evaluationStatistics )){
    ParallelLogger::logInfo('Appending recalibration evaluationStatistics ')
    
    performanceEvaluation$evaluationStatistics <- as.data.frame(performanceEvaluation$evaluationStatistics)
    performanceEvaluation$evaluationStatistics$Metric <- as.character(performanceEvaluation$evaluationStatistics$Metric)
    performanceEvaluation$evaluationStatistics$Value <- as.character(performanceEvaluation$evaluationStatistics$Value)
    performanceEvaluation$evaluationStatistics <- rbind(performanceEvaluation$evaluationStatistics ,
                                                       recalibration$evaluationStatistics )
  }
  
  return(performanceEvaluation)
}



formatEvaluation <- function(recalibrateResult, analysisId, eval){
  if(!is.null(recalibrateResult$demographicSummary)){
    demoNames <- colnames(recalibrateResult$demographicSummary)
    recalibrateResult$demographicSummary$analysisId  <- analysisId
    recalibrateResult$demographicSummary$Eval <- eval
    recalibrateResult$demographicSummary <- recalibrateResult$demographicSummary[,c("analysisId","Eval", demoNames )]
  }
  
  if(!is.null(recalibrateResult$calibrationSummary)){
    calNames <- colnames(recalibrateResult$calibrationSummary)
    recalibrateResult$calibrationSummary$analysisId  <- analysisId
    recalibrateResult$calibrationSummary$Eval <- eval
    recalibrateResult$calibrationSummary <- recalibrateResult$calibrationSummary[,c("analysisId","Eval", calNames )]
  }
  
  if(!is.null(recalibrateResult$thresholdSummary)){
    thresNames <- colnames(recalibrateResult$thresholdSummary)
    recalibrateResult$thresholdSummary$analysisId  <- analysisId
    recalibrateResult$thresholdSummary$Eval <- eval
    recalibrateResult$thresholdSummary <- recalibrateResult$thresholdSummary[,c("analysisId","Eval", thresNames )]
  }
  
  recalibrateResult$evaluationStatistics$analysisId <- NULL
  recalibrateResult$evaluationStatistics <- data.frame(analysisId = analysisId,
                                                       Eval = eval,
                                                       Metric = names(unlist(recalibrateResult$evaluationStatistics)),
                                                       Value = unlist(recalibrateResult$evaluationStatistics))
  return(recalibrateResult)
}
