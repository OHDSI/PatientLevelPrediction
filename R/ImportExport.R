# @file ImportExport.R
#
# Copyright 2017 Observational Health Data Sciences and Informatics
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


#' Export all data in a plpData object to CSV files
#'
#' @details
#' Created a set of CSV files in the output folder with all the data in the plplData object. This
#' function is intended to be used for research into prediction methods. The following files will be
#' created: \describe{ \item{cohort.csv}{Listing all persons and their prediction periods. This file
#' will have these fields: row_id (a unique ID per period), person_id, cohort_start_date, cohort_id,
#' time (number of days in the window).} \item{outcomes.csv}{Listing all outcomes per period. This
#' file will have these fields: row_id, outcome_id, outcome_count, time_to_event.}
#' \item{exclude.csv}{Either not exported or a file listing per outcome ID which windows had the
#' outcome prior to the window and should therefore be removed prior to fitting the model. This object
#' will have these fields: rowId, outcomeId.} \item{covariates.csv}{Listing the baseline covariates
#' per person in the cohorts. This is done using a sparse representation: covariates with a value of 0
#' are omitted to save space. The covariates file will have three columns: rowId, covariateId, and
#' covariateValue. } \item{covariateRef.csv}{A file describing the covariates that have been
#' extracted.} \item{metaData}{Some information on how the plpData object was constructed.} }
#'
#'
#' @param plpData        An object of type \code{plpData}.
#' @param outputFolder   The folder on the file system where the CSV files will be created. If the
#'                       folder does not yet exist it will be created.
#'
#' @examples
#' \dontrun{
#' exportPlpDataToCsv(plpData, "s:/temp/exportTest")
#' }
#' @export
exportPlpDataToCsv <- function(plpData, outputFolder) {
  start <- Sys.time()
  if (!file.exists(outputFolder)) {
    dir.create(outputFolder)
  }
  writeLines("Exporting cohorts.csv")
  ff::write.csv.ffdf(plpData$cohorts, file = file.path(outputFolder, "cohorts.csv"))
  writeLines("Exporting outcomes.csv")
  ff::write.csv.ffdf(plpData$outcomes, file = file.path(outputFolder, "outcomes.csv"))
  if (!is.null(plpData$exclude)) {
    writeLines("Exporting exclude.csv")
    ff::write.csv.ffdf(plpData$exclude, file = file.path(outputFolder, "exclude.csv"))
  }
  writeLines("Exporting covariates.csv")
  ff::write.csv.ffdf(plpData$covariates, file = file.path(outputFolder, "covariates.csv"))
  writeLines("Exporting covariateRef.csv")
  ff::write.csv.ffdf(plpData$covariateRef, file = file.path(outputFolder, "covariateRef.csv"))
  writeLines("Exporting metaData.csv")
  metaData <- data.frame(cohortIds = paste(plpData$metaData$cohortIds, collapse = ","),
                         outcomeIds = paste(plpData$metaData$outcomeIds, collapse = ","),
                         useCohortEndDate = plpData$metaData$useCohortEndDate,
                         windowPersistence = plpData$metaData$windowPersistence)
  write.csv(metaData, file = file.path(outputFolder, "metaData.csv"), row.names = FALSE)
  writeLines("Done exporting")
  delta <- Sys.time() - start
  writeLines(paste("Exporting data to CSV took", signif(delta, 3), attr(delta, "units")))
}




#'Transports a plpResult to a new location and removed sensitive data
#'
#' @details
#' This function is used to 
#'
#' @param plpResult      An object returned by running \code{RunPlp}.
#' @param outputFolder   The folder on the file system where the CSV files will be created. If the
#'                       folder does not yet exist it will be created.
#' @param n              The minimum number of people required for each result summary to be included 
#' @param includeEvaluationStatistics  Whether to include the evaluationStatistics
#' @param includeThresholdSummary      Whether to include the thresholdSummary
#' @param includeDemographicSummary    Whether to include the demographicSummary
#' @param includeCalibrationSummary    Whether to include the calibrationSummary
#' @param includePredictionDistribution  Whether to include the predictionDistribution  
#' @param includeCovariateSummary      Whether to include the covariateSummary             
#'
#' @examples
#' \dontrun{
#' transportPlp(plpResult, "s:/temp/exportTest", n=10)
#' }
#' @export
#' 
#' 
transportPlp <- function(plpResult,outputFolder, n=NULL,includeEvaluationStatistics=T,
                         includeThresholdSummary=T, includeDemographicSummary=T, 
                         includeCalibrationSummary =T, includePredictionDistribution=T,
                         includeCovariateSummary=T){
  
  # remove any sensitive data:
  plpResult$inputSetting$dataExtrractionSettings$connectionDetails <- NULL
  plpResult$model$metaData$call$connectionDetails <- NULL
  plpResult$model$metaData$call$cdmDatabaseSchema <- NULL
  plpResult$model$metaData$call$cohortDatabaseSchema <- NULL
  plpResult$model$metaData$call$outcomeDatabaseSchema <- NULL
  plpResult$model$index <- NULL
  plpResult$prediction <- NULL
  mod <- get("plpModel", envir = environment(plpResult$model$predict))
  mod$index <- NULL
  mod$metaData$call$connectionDetails <- NULL
  assign("plpModel", mod, envir = environment(plpResult$model$predict))
  
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
    
    includeInd <- plpResult$performanceEvaluation$demographicSummary$PersonCountAtRisk>=n &
                  plpResult$performanceEvaluation$demographicSummary$PersonCountWithOutcome >= n
    plpResult$performanceEvaluation$demographicSummary <- plpResult$performanceEvaluation$demographicSummary[includeInd,]
   
    plpResult$covariateSummary <- plpResult$covariateSummary[,colnames(plpResult$covariateSummary)%in%c('covariateId','covariateName', 'analysisId', 'conceptId', 'covariateValue')]  
    
    }
  
  #save to the output location
  PatientLevelPrediction::savePlpResult(plpResult, outputFolder)
  
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
  
  # remove any sensitive data:
  mod <- get("plpModel", envir = environment(plpModel$predict))
  mod$index <- NULL
  mod$metaData$call$connectionDetails <- NULL
  assign("plpModel", mod, envir = environment(plpModel$predict))
  
  
  #save to the output location
  PatientLevelPrediction::savePlpModel(plpModel, outputFolder)
  
}