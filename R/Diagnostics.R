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
#' @param cohortName                   A string specifying the name of the target cohort                                    
#' @param outcomeIds                   A vector of cohort_definition_ids used to define outcomes.
#' @param outcomeNames                 A vector of names for each outcome.
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
                       cohortName = cohortId,
                       outcomeIds,
                       outcomeNames = outcomeIds,
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
    if(!dir.exists(file.path(outputFolder))){
      dir.create(file.path(outputFolder), recursive = T)
    }
  }
  
  if(!is.null(plpData)){
    cohortId <- unique(plpData$cohorts$cohortId)
    outcomeIds <- unique(plpData$outcomes$outcomeId)
  }
  
  #create cohort names csv:
  if(file.exists(file.path(outputFolder,'names.csv'))){
    cohortNames <- read.csv(file.path(outputFolder,'names.csv'))
    
    newNames <- data.frame(ids = c(cohortId,outcomeIds), 
                           names = c(cohortName,outcomeNames))
    
    newNames<- newNames[!newNames$ids%in%cohortNames$ids,]
    if(length(newNames$ids)>0){
      cohortNames <- rbind(cohortNames, newNames)
    }
    
  } else {
    cohortNames <- data.frame(ids = c(cohortId,outcomeIds), 
                           names = c(cohortName,outcomeNames))
  }
  ParallelLogger::logInfo('Saving cohort names to csv')
  utils::write.csv(cohortNames, file.path(outputFolder,'names.csv'), row.names = F)
  
  #create settings:
  if(file.exists(file.path(outputFolder,'settings.csv'))){
    settings <- read.csv(file.path(outputFolder,'settings.csv'))
  } else{
    settings <- c()
  }
  maxAnalysis <-  ifelse(is.null(settings$analysisId), 0, max(settings$analysisId))
  for(i in 1:length(riskWindowStart)){
    for( j in 1:length(outcomeIds)){
      maxAnalysis <- maxAnalysis + 1
      settingsTemp <- data.frame(analysisId = maxAnalysis,
                                 cdmDatabaseName = cdmDatabaseName,
                                 cohortId = cohortId,
                                 outcomeId = outcomeIds[j],
                                 riskWindowStart = riskWindowStart[i],
                                 startAnchor = startAnchor[i],
                                 riskWindowEnd = riskWindowEnd[i],
                                 endAnchor = endAnchor[i]
      )
      settings <- rbind(settings, settingsTemp)
    }
  }
  
  ParallelLogger::logInfo('Saving settings to csv')
  write.csv(settings, file.path(outputFolder,'settings.csv'), row.names = F)
  
  
  if(is.null(plpData)){
    # get outcome and cohort data - dont need covariates
    
    if (!is.null(getOption("fftempdir")) && !file.exists(getOption("fftempdir"))) {
      warning("fftempdir '", getOption("fftempdir"), "' not found. Attempting to create folder")
      dir.create(getOption("fftempdir"), recursive = TRUE)
    }
    
    ParallelLogger::logInfo('Extracting data')
    dataSettings <- list(connectionDetails = connectionDetails,
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
    data <- do.call(getPlpData, dataSettings)
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
  
  if(file.exists(file.path(outputFolder, 'incidence.csv'))){
    incidence <- read.csv(file.path(outputFolder, 'incidence.csv')) 
  } else {
    incidence <- c()
  }
  
  if(file.exists(file.path(outputFolder, 'characterization.csv'))){
    characterization <- read.csv(file.path(outputFolder, 'characterization.csv')) 
  } else {
    characterization <- c()
  }
  for(i in 1:length(outcomeIds)){
    oi <- outcomeIds[i]
    for(j in 1:length(riskWindowStart)){
      population <- createStudyPopulation(plpData = data, 
                                          outcomeId = oi, 
                                          firstExposureOnly = F,
                                          includeAllOutcomes = F, 
                                          removeSubjectsWithPriorOutcome = F, 
                                          requireTimeAtRisk = F, 
                                          washoutPeriod = 0,
                                          riskWindowStart = riskWindowStart[j], 
                                          startAnchor = startAnchor[j], 
                                          riskWindowEnd = riskWindowEnd[j], 
                                          endAnchor = endAnchor[j])
      
      analysisId <- getAnalysisId(settings = settings, 
                                  cohortId = cohortId,
                                  outcomeId = oi,
                                  riskWindowStart = riskWindowStart[j], 
                                  startAnchor = startAnchor[j], 
                                  riskWindowEnd = riskWindowEnd[j], 
                                  endAnchor = endAnchor[j])
      
      if(sum(population$outcomeCount>0) < minCellCount){
        incidenceTemp <-data.frame(analysisId = analysisId,
                                   databaseName = cdmDatabaseName,
                                    cohort = cohortId,
                                    outcome = oi,
                                    riskWindowStart = riskWindowStart[j], 
                                    startAnchor = startAnchor[j], 
                                    riskWindowEnd = riskWindowEnd[j], 
                                    endAnchor = endAnchor[j],
                                    Opercent = paste0('< ',round(minCellCount/nrow(population)*100, digits = 2)),
                                    Tcount = nrow(population),
                                    Ocount = paste0('< ',minCellCount))
      } else {
        incidenceTemp <- data.frame(analysisId = analysisId,
                                    databaseName = cdmDatabaseName,
                                     cohort = cohortId,
                                     outcome = oi,
                                     riskWindowStart = riskWindowStart[j], 
                                     startAnchor = startAnchor[j], 
                                     riskWindowEnd = riskWindowEnd[j], 
                                     endAnchor = endAnchor[j],
                                     Opercent = sum(population$outcomeCount>0)/nrow(population)*100,
                                     Tcount = nrow(population),
                                     Ocount = sum(population$outcomeCount>0))
      }
      incidence <- unique(rbind(incidence, incidenceTemp))

      characterizationTemp <- covariateSummary(plpData = data, 
                                                population = population)
      
      
      characterizationTemp <- characterizationTemp[,c('covariateId',
                                                      'covariateName',
                                                      'CovariateCount',
                                                      'CovariateCountWithOutcome',
                                                      'CovariateCountWithNoOutcome',
                                                      'CovariateMeanWithOutcome',
                                                      'CovariateMeanWithNoOutcome')]
      
      ind <- (characterizationTemp$CovariateCount < minCellCount)  
      ind2 <- (characterizationTemp$CovariateCountWithOutcome < minCellCount) | (characterizationTemp$CovariateCountWithNoOutcome < minCellCount)
      
      characterizationTemp[ind,'CovariateCount'] <- -1
      characterizationTemp[ind,'CovariateCountWithOutcome'] <- -1
      characterizationTemp[ind,'CovariateCountWithNoOutcome'] <- -1
      characterizationTemp[ind,'CovariateMeanWithOutcome'] <- -1
      characterizationTemp[ind,'CovariateMeanWithNoOutcome'] <- -1
      
      characterizationTemp[ind2,'CovariateCountWithOutcome'] <- -1
      characterizationTemp[ind2,'CovariateCountWithNoOutcome'] <- -1
      characterizationTemp[ind2,'CovariateMeanWithOutcome'] <- -1
      characterizationTemp[ind2,'CovariateMeanWithNoOutcome'] <- -1
      
      # add analysisId
      characterizationTemp$analysisId <- analysisId
      characterization <- rbind(characterization, characterizationTemp)
    }
    
  }
  
  if(!is.null(outputFolder)){
    write.csv(incidence, file.path(outputFolder, 'incidence.csv'), row.names = F)
    write.csv(characterization, file.path(outputFolder, 'characterization.csv'), row.names = F)
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
  
  cohortId <- unique(cohort$cohortId)
  outcomesIds <- unique(outcomes$outcomeId)
  
  if(file.exists(file.path(outputFolder, 'distribution.csv'))){
    result <- read.csv(file.path(outputFolder, 'distribution.csv'))
  } else{
    result <- c()
  }
  
  for(i in  1:length(outcomesIds)){
    oi <- outcomesIds[i]
    ind <- outcomes$outcomeId==oi & outcomes$daysToEvent >= 0
    if(sum(ind)>0){
      afterC <- aggregate(x = outcomes$daysToEvent[ind], 
                          by = list(outcomes$rowId[ind]),
                          FUN = min)
      colnames(afterC) <- c('rowId','daysToOutcomeAfterMin')
      } else {
        afterC <- data.frame(rowId = -1, daysToOutcomeAfterMin = 0)
      }
  
    
    ind <- outcomes$outcomeId==oi & outcomes$daysToEvent < 0
    if(sum(ind)>0){
      beforeC <- aggregate(x = abs(outcomes$daysToEvent[ind]), 
                           by = list(outcomes$rowId[ind]),
                           FUN = min)
      colnames(beforeC) <- c('rowId','daysToOutcomeBeforeMin')
    } else {
      beforeC <- data.frame(rowId = -1, daysToOutcomeBeforeMin = 0)
    }
    
    tempResult <- merge(cohort, afterC, by='rowId', all.x = T)
    tempResult <- merge(tempResult, beforeC, by='rowId', all.x = T)

    tempResult <- processDistribution(tempResult)
    
    tempResult$databaseName <- databaseName 
    tempResult$outcomeId <- oi
    tempResult$targetId <- cohortId
    
    result <- rbind(result, tempResult)
 
  }
  
  if(!is.null(outputFolder)){
    write.csv(result, file.path(outputFolder, 'distribution.csv'), row.names = F)
  }
  
  return(result)
}


processDistribution <- function(distribution){
  
  distribution$year <- format(as.Date(as.character(distribution$cohortStartDate), format="%Y-%m-%d"),"%Y")
  distribution <- distribution[, c('year','daysFromObsStart','daysToObsEnd','daysToOutcomeAfterMin','daysToOutcomeBeforeMin')]
  results <- do.call(rbind, lapply(c('all',unique(distribution$year)), function(x) getQuantiles(distribution, x) ))
  return(results) 
}

getQuantiles <- function(distribution, year= 'all'){
  
  if(year != 'all'){
    distribution <- distribution[distribution$year==year,]
  } 
  quants <- data.frame(
    year = year,
    daysFromObsStart = quantile(distribution$daysFromObsStart, seq(0,1,0.01)),
    daysToObsEnd = quantile(distribution$daysToObsEnd, seq(0,1,0.01)),
    daysToOutcomeAfterMin = quantile(distribution$daysToOutcomeAfterMin[!is.na(distribution$daysToOutcomeAfterMin)], seq(0,1,0.01)),
    daysToOutcomeBeforeMin = quantile(distribution$daysToOutcomeBeforeMin[!is.na(distribution$daysToOutcomeBeforeMin)], seq(0,1,0.01))
  )
  heading <- data.frame(
    year = year,
    daysFromObsStart =length(distribution$daysFromObsStart),
    daysToObsEnd = length(distribution$daysToObsEnd),
    daysToOutcomeAfterMin = sum(!is.na(distribution$daysToOutcomeAfterMin)),
    daysToOutcomeBeforeMin = sum(!is.na(distribution$daysToOutcomeBeforeMin))
  )
  results <- rbind(N = heading, quants)
  results$type = rownames(results)
  rownames(results) <- NULL
  return(results)
}



getAnalysisId <- function(settings, 
                          cohortId,
                          outcomeId,
                          riskWindowStart, 
                          startAnchor, 
                          riskWindowEnd, 
                          endAnchor){
  
  ind <- (settings$cohortId == cohortId) & (settings$outcomeId == outcomeId) & 
    (settings$riskWindowStart == riskWindowStart) & (settings$riskWindowEnd == riskWindowEnd) &
      (settings$startAnchor == startAnchor) &  (settings$endAnchor == endAnchor)
      if(sum(ind)==0){
        writeLines(paste('cohortId:',cohortId, '-outcomeId:',outcomeId, 
                         '-riskWindowStart:', riskWindowStart, '-riskWindowEnd:', riskWindowEnd,
                         '-startAnchor:', startAnchor, '-endAnchor:',endAnchor))
        print(settings)
        stop('No analysis id found for the settings')
      } else {
        return(settings$analysisId[ind][1])
      }
}
  