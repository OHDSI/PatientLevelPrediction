# @file Diagnostics.R
#
# Copyright 2019 Observational Health Data Sciences and Informatics
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

#' diagnostic - Investigates the prediction problem settings - use before training a model
#'
#' @description
#' This function runs a set of prediction diagnoses to help pick a suitable T, O, TAR and determine 
#' whether the prediction problem is worth executing.
#' 
#' @details
#' Users can define set of Ts, Os, databases and population settings.  A list of data.frames containing details such as
#' follow-up time distribution, time-to-event information, characteriszation details, time from last prior event, 
#' observation time distribution. 
#'
#' @param plpData                      The data object to do the diagnostic on - if NULL you need to specify the connection settings below
#' @param cdmDatabaseName            Name of the database
#' @param connectionDetails            An R object of type\cr\code{connectionDetails} created using the
#'                                     function \code{createConnectionDetails} in the
#'                                     \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema            The name of the database schema that contains the OMOP CDM
#'                                     instance.  Requires read permissions to this database. On SQL
#'                                     Server, this should specifiy both the database and the schema,
#'                                     so for example 'cdm_instance.dbo'.
#' @param oracleTempSchema             For Oracle only: the name of the database schema where you want
#'                                     all temporary tables to be managed. Requires create/insert
#'                                     permissions to this database.
#' @param cohortId                     A unique identifier to define the at risk cohorts. CohortId is
#'                                     used to select the cohort_concept_id in the cohort-like table.
#' @param outcomeIds                   A list of cohort_definition_ids used to define outcomes.
#' @param cohortDatabaseSchema         The name of the database schema that is the location where the
#'                                     cohort data used to define the at risk cohort is available.
#'                                     If cohortTable = DRUG_ERA, cohortDatabaseSchema is not used
#'                                     by assumed to be cdmSchema.  Requires read permissions to this
#'                                     database.
#' @param cohortTable                  The tablename that contains the at risk cohort.  If
#'                                     cohortTable <> DRUG_ERA, then expectation is cohortTable has
#'                                     format of COHORT table: cohort_concept_id, SUBJECT_ID,
#'                                     COHORT_START_DATE, COHORT_END_DATE.
#' @param outcomeDatabaseSchema            The name of the database schema that is the location where
#'                                         the data used to define the outcome cohorts is available. If
#'                                         cohortTable = CONDITION_ERA, exposureDatabaseSchema is not
#'                                         used by assumed to be cdmSchema.  Requires read permissions
#'                                         to this database.
#' @param outcomeTable                     The tablename that contains the outcome cohorts.  If
#'                                         outcomeTable <> CONDITION_OCCURRENCE, then expectation is
#'                                         outcomeTable has format of COHORT table:
#'                                         COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE,
#'                                         COHORT_END_DATE.
#' @param cdmVersion                   Define the OMOP CDM version used: currently support "4" and "5".
#' @param riskWindowStart        The start of the risk window (in days) relative to the \code{startAnchor}.
#' @param startAnchor	           The anchor point for the start of the risk window. Can be "cohort start" or "cohort end".
#' @param riskWindowEnd          The end of the risk window (in days) relative to the \code{endAnchor} parameter
#' @param endAnchor              The anchor point for the end of the risk window. Can be "cohort start" or "cohort end".
#' @param outputFolder           Location to save results for shiny app
#' @param sampleSize             Sample from the target population
#' @param minCellCount           The minimum count that will be displayed
#' 
#' @return
#' An object containing the model or location where the model is save, the data selection settings, the preprocessing
#' and training settings as well as various performance measures obtained by the model.
#'
#' \item{distribution}{list for each O of a data.frame containing: i) Time to observation end distribution, ii) Time from observation start distribution, iii) Time to event distribution and iv) Time from last prior event to index distribution (only for patients in T who have O before index) }
#' \item{incident}{list for each O of incidence of O in T during TAR}
#' \item{characterization}{list for each O of Characterization of T, TnO, Tn~O}
#'
#'
#' @export
#' @examples
#' \dontrun{
#' #******** EXAMPLE 1 ********* 
#' } 
diagnostic <- function(plpData = NULL,
                       cdmDatabaseName,
                       connectionDetails,
                       cdmDatabaseSchema,
                       oracleTempSchema = NULL,
                       cohortId,
                       outcomeIds,
                       cohortDatabaseSchema,
                       cohortTable = 'cohort',
                       outcomeDatabaseSchema = cohortDatabaseSchema,
                       outcomeTable = cohortTable,
                       cdmVersion = 5,
                       riskWindowStart = 1,
                       startAnchor = 'cohort start',
                       riskWindowEnd = 365,
                       endAnchor = 'cohort start',
                       outputFolder = NULL,
                       sampleSize = NULL,
                       minCellCount = 5){
  
  if(!is.null(outputFolder)){
    if(!dir.exists(file.path(outputFolder, cdmDatabaseName))){
      dir.create(file.path(outputFolder, cdmDatabaseName), recursive = T)
    }
  }
  
  if(is.null(plpData)){
    # get outcome and cohort data - dont need covariates
    
    if (!is.null(getOption("fftempdir")) && !file.exists(getOption("fftempdir"))) {
      warning("fftempdir '", getOption("fftempdir"), "' not found. Attempting to create folder")
      dir.create(getOption("fftempdir"), recursive = TRUE)
    }
    
    ParallelLogger::logInfo('Extracting data')
    settings <- list(connectionDetails = connectionDetails,
                     cdmDatabaseSchema = cdmDatabaseSchema,
                     oracleTempSchema = oracleTempSchema,
                     cohortId = cohortId,
                     outcomeIds = outcomeIds,
                     cohortDatabaseSchema = cohortDatabaseSchema,
                     cohortTable = cohortTable,
                     outcomeDatabaseSchema = outcomeDatabaseSchema,
                     outcomeTable = outcomeTable,
                     cdmVersion = cdmVersion,
                     sampleSize = sampleSize,
                     covariateSettings = FeatureExtraction::createDefaultCovariateSettings())
    data <- do.call(getPlpData, settings)
  } else {
    data <- plpData
  } 
  
  outcomeIds <- unique(data$outcomes$outcomeId)
  
  ParallelLogger::logInfo('Calculating distributions')
  distribution <- getDistribution(cohort = data$cohorts,
                                  outcomes = data$outcomes,
                                  outputFolder = outputFolder, 
                                  databaseName = cdmDatabaseName)
  
  # do characterisation - needs TAR
  ParallelLogger::logInfo('Calculating incidence and characterizations')
  
  incidence <- list()
  length(incidence) <- length(outcomeIds)
  names(incidence) <- outcomeIds
  
  characterization <- list()
  length(characterization) <- length(outcomeIds)
  names(characterization) <- outcomeIds
  for(i in 1:length(outcomeIds)){
    oi <- outcomeIds[i]
    population <- createStudyPopulation(plpData = data, 
                                        outcomeId = oi, 
                                        firstExposureOnly = F,
                                        includeAllOutcomes = F, 
                                        removeSubjectsWithPriorOutcome = F, 
                                        requireTimeAtRisk = F, 
                                        washoutPeriod = 0,
                                        riskWindowStart = riskWindowStart, 
                                        startAnchor = startAnchor, 
                                        riskWindowEnd = riskWindowEnd, 
                                        endAnchor = endAnchor)
    
    
    if(sum(population$outcomeCount>0) < minCellCount){
      incidence[[i]] <-data.frame(Opercent = paste0('< ',round(minCellCount/nrow(population)*100, digits = 2)),
                                  Tcount = nrow(population),
                                  Ocount = paste0('< ',minCellCount))
    } else {
      incidence[[i]] <- data.frame(Opercent = sum(population$outcomeCount>0)/nrow(population)*100,
                                   Tcount = nrow(population),
                                   Ocount = sum(population$outcomeCount>0))
    }
    
    characterization[[i]] <- covariateSummary(plpData = data, 
                                              population = population)
    
    ind <- (characterization[[i]]$CovariateCount < minCellCount)  
    ind2 <- (characterization[[i]]$CovariateCountWithOutcome < minCellCount) | (characterization[[i]]$CovariateCountWithNoOutcome < minCellCount)
    
    characterization[[i]][ind,'CovariateCount'] <- -1
    characterization[[i]][ind,'CovariateCountWithOutcome'] <- -1
    characterization[[i]][ind,'CovariateCountWithNoOutcome'] <- -1
    characterization[[i]][ind,'CovariateMeanWithOutcome'] <- -1
    characterization[[i]][ind,'CovariateMeanWithNoOutcome'] <- -1
    
    characterization[[i]][ind2,'CovariateCountWithOutcome'] <- -1
    characterization[[i]][ind2,'CovariateCountWithNoOutcome'] <- -1
    characterization[[i]][ind2,'CovariateMeanWithOutcome'] <- -1
    characterization[[i]][ind2,'CovariateMeanWithNoOutcome'] <- -1
    
    if(!is.null(outputFolder)){
      saveRDS(incidence[[i]], file.path(outputFolder, cdmDatabaseName, paste('incidence',oi,startAnchor,endAnchor,riskWindowStart,paste0(riskWindowEnd,'.rds'), sep='_')))
      saveRDS(characterization[[i]], file.path(outputFolder, cdmDatabaseName, paste('characterization_',oi,startAnchor,endAnchor,riskWindowStart,paste0(riskWindowEnd,'.rds'), sep='_') ))
    }
    
  }
  
  result <- list(distribution = distribution,
                 incidence = incidence,
                 characterization = characterization)
  
  return(result)
}


getDistribution <- function(cohort,
                            outcomes,
                            outputFolder = NULL, 
                            databaseName){
  
  outcomesIds <- unique(outcomes$outcomeId)
  
  result <- list()
  length(result) <- length(outcomesIds)
  names(result) <- outcomesIds
  
  for(i in  1:length(outcomesIds)){
    oi <- outcomesIds[i]
    ind <- outcomes$outcomeId==oi & outcomes$daysToEvent >= 0
    if(sum(ind)>0){
      afterC <- aggregate(x = outcomes$daysToEvent[ind], 
                          by = list(outcomes$rowId[ind]),
                          FUN = min)
      colnames(afterC) <- c('rowId','daysToOutcomeAfterMin')
      afterC$hasOutcomeAfter <- rep(1, nrow(afterC))
      } else {
        afterC <- data.frame(rowId = -1, daysToOutcomeAfterMin = 0, hasOutcomeAfter = 0)
      }
  
    
    ind <- outcomes$outcomeId==oi & outcomes$daysToEvent < 0
    if(sum(ind)>0){
      beforeC <- aggregate(x = outcomes$daysToEvent[ind], 
                           by = list(outcomes$rowId[ind]),
                           FUN = max)
      colnames(beforeC) <- c('rowId','daysToOutcomeBeforeMin')
      beforeC$hasOutcomeBefore <- rep(1, nrow(beforeC))
    } else {
      beforeC <- data.frame(rowId = -1, daysToOutcomeBeforeMin = 0, hasOutcomeBefore = 0)
    }
    
    cohort <- merge(cohort, afterC, by='rowId', all.x = T)
    cohort <- merge(cohort, beforeC, by='rowId', all.x = T)
    if(sum(is.na(cohort$hasOutcomeAfter))>0){
      cohort$hasOutcomeAfter[is.na(cohort$hasOutcomeAfter)] <- rep(0, sum(is.na(cohort$hasOutcomeAfter))) 
    }
    if(sum(is.na(cohort$hasOutcomeBefore))>0){
      cohort$hasOutcomeBefore[is.na(cohort$hasOutcomeBefore)] <- rep(0, sum(is.na(cohort$hasOutcomeBefore)))
    }
    
    result[[i]] <- cohort
    
    if(!is.null(outputFolder)){
      saveRDS(result[[i]], file.path(outputFolder, databaseName, paste0('distribution_',oi,'.rds')))
    }
    
  }
  
  return(result)
}

  