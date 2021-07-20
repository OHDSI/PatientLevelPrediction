# @file ImportExport.R
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


#'Transports a plpResult to a new location and removed sensitive data
#'
#' @details
#' This function is used to
#'
#' @param plpResult      An object returned by running \code{runPlp}.
#' @param modelName      A string of the name of the model
#' @param dataName       A string of the name of the data
#' @param outputFolder   The folder on the file system where the CSV files will be created. If the
#'                       folder does not yet exist it will be created.
#' @param n              The minimum number of people required for each result summary to be included
#' @param includeEvaluationStatistics  Whether to include the evaluationStatistics
#' @param includeThresholdSummary      Whether to include the thresholdSummary
#' @param includeDemographicSummary    Whether to include the demographicSummary
#' @param includeCalibrationSummary    Whether to include the calibrationSummary
#' @param includePredictionDistribution  Whether to include the predictionDistribution
#' @param includeCovariateSummary      Whether to include the covariateSummary
#' @param save                         Whether to save the result or just return the transportable object
#' @param reduceSize                   Remove parts of runPlp object that are not needed but take up space
#'
#' @examples
#' \dontrun{
#' transportPlp(plpResult, "s:/temp/exportTest", n=10)
#' }
#' @export
#'
#'
transportPlp <- function(plpResult,modelName=NULL, dataName=NULL,
                         outputFolder, n=NULL,includeEvaluationStatistics=T,
                         includeThresholdSummary=T, includeDemographicSummary=T,
                         includeCalibrationSummary =T, includePredictionDistribution=T,
                         includeCovariateSummary=T, save=T,
                         reduceSize = F){

  # remove any sensitive data:
  plpResult$inputSetting$dataExtrractionSettings <- NULL 
  plpResult$executionSummary$Log <- NULL
  plpResult$model$metaData$call$connectionDetails <- NULL
  plpResult$model$metaData$call$cdmDatabaseSchema <- dataName
  plpResult$model$metaData$call$cohortDatabaseSchema <- NULL
  plpResult$model$metaData$call$outcomeDatabaseSchema <- NULL
  plpResult$model$metaData$call$oracleTempSchema <- NULL
  plpResult$model$metaData$call$baseUrl <- NULL
  plpResult$model$metaData$modelName <- modelName
  plpResult$model$index <- NULL
  if(!is.null(plpResult$model$trainCVAuc)){
    plpResult$model$trainCVAuc <- NULL # newly added 
  }
  if(class(plpResult$model$model)%in%c('list',"plpModel")){
    if(!is.null(plpResult$model$model$cv)){
      plpResult$model$model$cv <- NULL # newly added 
    }
  }
  plpResult$prediction <- NULL
  if(!is.null(plpResult$model$predict)){
    mod <- get("plpModel", envir = environment(plpResult$model$predict))
    mod$index <- NULL
    mod$metaData$call$connectionDetails <- NULL
    mod$metaData$call$oracleTempSchema <- NULL
    mod$metaData$call$outcomeDatabaseSchema <-'Missing'
    mod$metaData$call$cdmDatabaseSchema <- 'Missing'
    mod$metaData$call$cohortDatabaseSchema <- 'Missing'
    mod$metaData$call$baseUrl <- NULL
    if(!is.null(mod$trainCVAuc)){
      mod$trainCVAuc <- NULL
    }
    if(class(mod$model)=='list'){
      if(!is.null(mod$model$cv)){
        mod$model$cv <- NULL
      }
    }
    mod$varImp <- mod$varImp[mod$varImp$covariateValue!=0,]  #remove non-zero
    
    assign("plpModel", mod, envir = environment(plpResult$model$predict))
  }
  
  if(reduceSize){
    plpResult$model$covariateMap <- NULL
    plpResult$model$varImp <- NULL
    plpResult$model$metaData$preprocessSettings <- NULL
  }

  if(!includeEvaluationStatistics)
   plpResult$performanceEvaluation$evaluationStatistics <- NULL
  if(!includeThresholdSummary)
    plpResult$performanceEvaluation$thresholdSummary <- NULL
  if(!includeDemographicSummary)
    plpResult$performanceEvaluation$demographicSummary <- NULL
  if(!includeCalibrationSummary)
    plpResult$performanceEvaluation$calibrationSummary <- NULL
  if(!includePredictionDistribution)
    plpResult$performanceEvaluation$predictionDistribution <- NULL
  if(!includeCovariateSummary)
    plpResult$covariateSummary <- NULL

  # remove things less than n
  if(!is.null(n)){
    # remove less than n counts from demographicSummary

    if(!is.null(plpResult$performanceEvaluation$demographicSummary)){
      
      plpResult$performanceEvaluation$demographicSummary$PersonCountAtRisk[is.na(plpResult$performanceEvaluation$demographicSummary$PersonCountAtRisk)] <- 0
      plpResult$performanceEvaluation$demographicSummary$PersonCountWithOutcome[is.na(plpResult$performanceEvaluation$demographicSummary$PersonCountWithOutcome)] <- 0
      
      removeInd <- plpResult$performanceEvaluation$demographicSummary$PersonCountAtRisk< n |
        plpResult$performanceEvaluation$demographicSummary$PersonCountWithOutcome < n 
      plpResult$performanceEvaluation$demographicSummary$PersonCountAtRisk[removeInd] <- -1
      plpResult$performanceEvaluation$demographicSummary$PersonCountWithOutcome[removeInd] <- -1
    }
    
    if(!is.null(plpResult$covariateSummary)){
      plpResult$covariateSummary <- plpResult$covariateSummary[,colnames(plpResult$covariateSummary)%in%c('covariateId','covariateName', 'analysisId', 'conceptId','CovariateCount', 'covariateValue','CovariateCountWithOutcome','CovariateCountWithNoOutcome','CovariateMeanWithOutcome','CovariateMeanWithNoOutcome','StandardizedMeanDiff')]
      
      plpResult$covariateSummary$CovariateCount[is.na(plpResult$covariateSummary$CovariateCount)] <- 0
      plpResult$covariateSummary$CovariateCountWithOutcome[is.na(plpResult$covariateSummary$CovariateCountWithOutcome)] <- 0
      plpResult$covariateSummary$CovariateCountWithNoOutcome[is.na(plpResult$covariateSummary$CovariateCountWithNoOutcome)] <- 0
      
      removeInd <- plpResult$covariateSummary$CovariateCount < n |
        plpResult$covariateSummary$CovariateCountWithOutcome < n | 
        plpResult$covariateSummary$CovariateCountWithNoOutcome < n 
      plpResult$covariateSummary$CovariateCount[removeInd] <- -1
      plpResult$covariateSummary$CovariateCountWithOutcome[removeInd] <- -1
      plpResult$covariateSummary$CovariateCountWithNoOutcome[removeInd] <- -1
      plpResult$covariateSummary$CovariateMeanWithOutcome[removeInd] <- -1
      plpResult$covariateSummary$CovariateMeanWithNoOutcome[removeInd] <- -1
    }
    
    }

  #save to the output location
  if(save){
    savePlpResult(plpResult, outputFolder)
    return(NULL)
  }
  
  return(plpResult)

}

#'Transports a plpModel to a new location and removes sensitive data
#'
#' @details
#' This function is used to
#'
#' @param plpModel      A trianed model.
#' @param outputFolder   The folder on the file system where the CSV files will be created. If the
#'                       folder does not yet exist it will be created.
#'
#' @examples
#' \dontrun{
#' transportModel(plpModel, "s:/temp/exportTest")
#' }
#' @export
#'
#'
transportModel <- function(plpModel,outputFolder){

  plpModel$index <- NULL
  plpModel$metaData$call$connectionDetails <- NULL
  plpModel$metaData$call$cdmDatabaseSchema <- NULL
  plpModel$metaData$call$cohortDatabaseSchema <- NULL
  plpModel$metaData$call$outcomeDatabaseSchema <- NULL
  plpModel$metaData$call$oracleTempSchema <- NULL

  # remove any sensitive data:
  if(!is.null(plpModel$predict)){
    mod <- get("plpModel", envir = environment(plpModel$predict))
    mod$index <- NULL
    mod$metaData$call$connectionDetails <- NULL
    mod$metaData$call$oracleTempSchema <- NULL
    mod$metaData$call$outcomeDatabaseSchema <-'Missing'
    mod$metaData$call$cdmDatabaseSchema <- 'Missing'
    mod$metaData$call$cohortDatabaseSchema <- 'Missing'
    assign("plpModel", mod, envir = environment(plpModel$predict))
  }

  #save to the output location
  savePlpModel(plpModel, outputFolder)

}





