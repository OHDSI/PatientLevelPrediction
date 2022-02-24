# @file Diagnostics.R
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
#' @param cdmDatabaseName             The name of the database being diagnosed
#' @param cohortName                  Name of the target cohort
#' @param outcomeNames                Vector of outcome names
#' @param databaseDetails            (only used is plpData is NULL) The database details created using \code{createDatabaseDetails}
#' @param restrictPlpDataSettings    (only used is plpData is NULL) The restrictPlpDataSettings created using \code{createRestrictPlpDataSettings}
#' @param populationSettings         The population setting details created using \code{createPopulationSettings}
#' @param outputFolder           Location to save results for shiny app
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
diagnostic <- function(
  plpData = NULL,
  cdmDatabaseName = 'none',
  cohortName,
  outcomeNames,
  databaseDetails,
  restrictPlpDataSettings,
  populationSettings,
  outputFolder = NULL,
  minCellCount = 5
){
  
  if(is.null(plpData)){
    checkIsClass(databaseDetails, 'databaseDetails')
    cdmDatabaseName <- attr(databaseDetails, 'cdmDatabaseName')
    checkIsClass(restrictPlpDataSettings, 'restrictPlpDataSettings')
  }
  
  if(class(populationSettings) != 'list'){
    populationSettings <- list(populationSettings)
  }
  lapply(populationSettings, function(x) checkIsClass(x, 'populationSettings'))
  
  if(!is.null(outputFolder)){
    if(!dir.exists(file.path(outputFolder))){
      dir.create(file.path(outputFolder), recursive = T)
    }
  }
  
  if(!is.null(plpData)){
    cohortId <- unique(plpData$cohorts$cohortId)
    outcomeIds <- unique(plpData$outcomes$outcomeId)
  } else{
    cohortId <- databaseDetails$cohortId
    outcomeIds <- databaseDetails$outcomeIds
  }
  
  #create cohort names csv:
  if(file.exists(file.path(outputFolder,'namesdetails.csv'))){
    cohortNames <- utils::read.csv(file.path(outputFolder,'namesdetails.csv'))
    
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
  utils::write.csv(cohortNames, file.path(outputFolder,'namesdetails.csv'), row.names = F)
  
  #create settings:
  if(file.exists(file.path(outputFolder,'settings.csv'))){
    settings <- utils::read.csv(file.path(outputFolder,'settings.csv'))
  } else{
    settings <- c()
  }
  maxAnalysis <-  ifelse(is.null(settings$analysisId), 0, max(settings$analysisId))
  for(i in 1:length(populationSettings)){
    for( j in 1:length(outcomeIds)){
      maxAnalysis <- maxAnalysis + 1
      settingsTemp <- data.frame(analysisId = maxAnalysis,
                                 cdmDatabaseName = cdmDatabaseName,
                                 cohortId = cohortId,
                                 outcomeId = outcomeIds[j],
                                 riskWindowStart = populationSettings[[i]]$riskWindowStart,
                                 startAnchor = populationSettings[[i]]$startAnchor,
                                 riskWindowEnd = populationSettings[[i]]$riskWindowEnd,
                                 endAnchor = populationSettings[[i]]$endAnchor
      )
      settings <- unique(rbind(settings, settingsTemp))
    }
  }
  
  ParallelLogger::logInfo('Saving settings to csv')
  utils::write.csv(settings, file.path(outputFolder,'settings.csv'), row.names = F)
  
  
  if(is.null(plpData)){
    # get outcome and cohort data - dont need covariates
    
    ParallelLogger::logInfo('Extracting data')
    data <- do.call(
      getPlpData, 
      list(
        databaseDetails = databaseDetails, 
        covariateSettings = FeatureExtraction::createDefaultCovariateSettings(),
        restrictPlpDataSettings = restrictPlpDataSettings
      )
    )
  } else {
    data <- plpData
  } 
  
  outcomeIds <- unique(data$outcomes$outcomeId)
  
  ParallelLogger::logInfo('Calculating distributions')
  distribution <- getDistribution(cohort = data$cohorts,
                                  outcomes = data$outcomes,
                                  outputFolder = outputFolder, 
                                  databaseName = cdmDatabaseName)
  
  
  # get survival data:
  ParallelLogger::logInfo('Calculating survival data')
  if(file.exists(file.path(outputFolder, 'survival.csv'))){
    surv <- utils::read.csv(file.path(outputFolder, 'survival.csv')) 
  } else {
    surv <- c()
  }
  survTemp <- lapply(outcomeIds, function(oi) getSurvival(plpData = data, 
                                                          outcomeId = oi,
                                                          cohortId = cohortId, 
                                                          cdmDatabaseName  = cdmDatabaseName ))
  surv <- unique(rbind(surv, do.call('rbind', survTemp)))
  if(!is.null(outputFolder)){
    utils::write.csv(surv, file.path(outputFolder, 'survival.csv'), row.names = F)
  }
  
  # do characterisation - needs TAR
  ParallelLogger::logInfo('Calculating proportion and characterizations')
  
  if(file.exists(file.path(outputFolder, 'proportion.csv'))){
    proportion <- utils::read.csv(file.path(outputFolder, 'proportion.csv')) 
  } else {
    proportion <- c()
  }
  
  if(file.exists(file.path(outputFolder, 'characterization.csv'))){
    characterization <- utils::read.csv(file.path(outputFolder, 'characterization.csv')) 
  } else {
    characterization <- c()
  }
  for(i in 1:length(outcomeIds)){
    oi <- outcomeIds[i]
    for(j in 1:length(populationSettings)){
      
      population <- createStudyPopulation(
        plpData = data, 
        outcomeId = oi, 
        populationSettings = populationSettings[[j]]
        )

      analysisId <- getAnalysisId(
        settings = settings, 
        cohortId = cohortId,
        outcomeId = oi,
        riskWindowStart = populationSettings[[j]]$riskWindowStart, 
        startAnchor = populationSettings[[j]]$startAnchor, 
        riskWindowEnd = populationSettings[[j]]$riskWindowEnd, 
        endAnchor = populationSettings[[j]]$endAnchor
      )
      
      proportionTemp <- getProportions(
        population,
        analysisId = analysisId,
        cdmDatabaseName = cdmDatabaseName,
        cohortId = cohortId,
        outcomeId = oi,
        minCellCount = minCellCount
      )
     
      proportion <- unique(rbind(proportion, proportionTemp))

      characterizationTemp <- covariateSummary(
        covariateData = plpData$covariateData, 
        cohort = population %>% dplyr::select(.data$rowId),
        labels = population %>% dplyr::select(.data$rowId, .data$outcomeCount)
        )
      
      
      characterizationTemp <- characterizationTemp[,c('covariateId',
                                                      'covariateName',
                                                      'CovariateCount',
                                                      'WithOutcome_CovariateCount',
                                                      'WithNoOutcome_CovariateCount',
                                                      'WithOutcome_CovariateMean',
                                                      'WithNoOutcome_CovariateMean')]
      
      characterizationTemp[is.na(characterizationTemp)] <- 0
      
      ind <- (characterizationTemp$CovariateCount < minCellCount)  
      ind2 <- (characterizationTemp$WithOutcome_CovariateCount < minCellCount) | (characterizationTemp$WithNoOutcome_CovariateCount < minCellCount)
      
      characterizationTemp[ind,'CovariateCount'] <- -1
      characterizationTemp[ind,'WithOutcome_CovariateCount'] <- -1
      characterizationTemp[ind,'WithNoOutcome_CovariateCount'] <- -1
      characterizationTemp[ind,'WithOutcome_CovariateMean'] <- -1
      characterizationTemp[ind,'WithNoOutcome_CovariateMean'] <- -1
      
      characterizationTemp[ind2,'WithOutcome_CovariateCount'] <- -1
      characterizationTemp[ind2,'WithNoOutcome_CovariateCount'] <- -1
      characterizationTemp[ind2,'WithOutcome_CovariateMean'] <- -1
      characterizationTemp[ind2,'WithNoOutcome_CovariateMean'] <- -1
      
      # add analysisId
      characterizationTemp$analysisId <- analysisId
      characterization <- rbind(characterization, characterizationTemp)
    }
    
  }
  
  if(!is.null(outputFolder)){
    utils::write.csv(proportion, file.path(outputFolder, 'proportion.csv'), row.names = F)
    utils::write.csv(characterization, file.path(outputFolder, 'characterization.csv'), row.names = F)
  }
  
  # Add all to zip file -------------------------------------------------------------------------------
  ParallelLogger::logInfo("Adding results to zip file")
  zipName <- file.path(outputFolder, paste0("Results_", cdmDatabaseName, ".zip"))
  files <- list.files(outputFolder, pattern = ".*\\.csv$")
  oldWd <- setwd(outputFolder)
  on.exit(setwd(oldWd), add = TRUE)
  DatabaseConnector::createZipFile(zipFile = zipName, files = files)
  ParallelLogger::logInfo("Results are ready for sharing at: ", zipName)
  
  result <- list(distribution = distribution,
                 proportion = proportion,
                 characterization = characterization,
                 survival = surv)
  
  return(result)
}


getSurvival <- function(plpData, outcomeId, cohortId, cdmDatabaseName ){

  object <- plpData$outcomes %>% 
    dplyr::filter(.data$outcomeId == !!outcomeId) %>%
    dplyr::right_join(plpData$cohorts, by ='rowId') %>%
    dplyr::group_by(.data$rowId) %>%
    dplyr::summarise(daysToObsEnd = min(.data$daysToObsEnd),
                     daysToEvent = min(.data$daysToEvent))
    
    
  object$censoredTime <- apply(object[,-1], 1, function(x) min(x, na.rm = T))
  object$event <- 0
  object$event[!is.na(object$daysToEvent)] <- ifelse(object$event[!is.na(object$daysToEvent)] <= object$censoredTime[!is.na(object$daysToEvent)], 1,0)
  
  
  result <- object %>% dplyr::group_by(.data$censoredTime) %>%
    dplyr::summarise(events = sum(.data$event),
                     censored = length(.data$event)-sum(.data$event))
  
  totalCensored <- lapply(unique(object$censoredTime), function(i) sum(result %>% dplyr::filter(.data$censoredTime <= i) %>% dplyr::select(.data$censored)))

  totalCensored <- data.frame(censoredTime = unique(object$censoredTime),
                              totalCensored = unlist(totalCensored))
  
  totalLost <- lapply(unique(object$censoredTime), function(i) sum(result %>% dplyr::filter(.data$censoredTime <= i) %>% dplyr::mutate(lost = .data$censored + .data$events) %>% dplyr::select(.data$lost)))
  totalLost <- data.frame(censoredTime = unique(object$censoredTime),
                          nAtRisk = nrow(plpData$cohorts) - unlist(totalLost))
  
  result <- result %>% 
    dplyr::left_join(totalCensored, by ='censoredTime') %>% 
    dplyr::left_join(totalLost, by ='censoredTime')
  
  result$outcomeId <- outcomeId
  result$cohortId <- cohortId
  result$cdmDatabaseName <- cdmDatabaseName 
  return(result)
}


getDistribution <- function(cohort,
                            outcomes,
                            outputFolder = NULL, 
                            databaseName){
  
  cohortId <- unique(cohort$cohortId)
  outcomesIds <- unique(outcomes$outcomeId)
  
  if(file.exists(file.path(outputFolder, 'distribution.csv'))){
    result <- utils::read.csv(file.path(outputFolder, 'distribution.csv'))
  } else{
    result <- c()
  }
  
  for(i in  1:length(outcomesIds)){
    oi <- outcomesIds[i]
    ind <- outcomes$outcomeId==oi & outcomes$daysToEvent >= 0
    if(sum(ind)>0){
      afterC <- stats::aggregate(x = outcomes$daysToEvent[ind], 
                          by = list(outcomes$rowId[ind]),
                          FUN = min)
      colnames(afterC) <- c('rowId','daysToOutcomeAfterMin')
      } else {
        afterC <- data.frame(rowId = -1, daysToOutcomeAfterMin = 0)
      }
  
    
    ind <- outcomes$outcomeId==oi & outcomes$daysToEvent < 0
    if(sum(ind)>0){
      beforeC <- stats::aggregate(x = abs(outcomes$daysToEvent[ind]), 
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
    
    result <- unique(rbind(result, tempResult))
 
  }
  
  if(!is.null(outputFolder)){
    utils::write.csv(result, file.path(outputFolder, 'distribution.csv'), row.names = F)
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
    daysFromObsStart = stats::quantile(distribution$daysFromObsStart, seq(0,1,0.01)),
    daysToObsEnd = stats::quantile(distribution$daysToObsEnd, seq(0,1,0.01)),
    daysToOutcomeAfterMin = stats::quantile(distribution$daysToOutcomeAfterMin[!is.na(distribution$daysToOutcomeAfterMin)], seq(0,1,0.01)),
    daysToOutcomeBeforeMin = stats::quantile(distribution$daysToOutcomeBeforeMin[!is.na(distribution$daysToOutcomeBeforeMin)], seq(0,1,0.01))
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



getProportions <- function(population, 
                           analysisId,
                           cdmDatabaseName,
                           cohortId,
                           outcomeId,
                           minCellCount = NULL){
  
  details <- attr(population, 'metaData')$populationSettings
  
  TAR <- paste0(details$startAnchor, ' + ', details$riskWindowStart, ' days - ',
                details$endAnchor, ' + ', details$riskWindowEnd, ' days')
  
  
  result <- population %>% dplyr::mutate(ageGroup = paste0(floor(.data$ageYear/5)*5 ,' - ', (floor(.data$ageYear/5)+1)*5-1 ),
                               year = substring(.data$cohortStartDate,1,4)) %>%
    dplyr::group_by(.data$year, .data$ageGroup, .data$gender) %>%
    dplyr::summarize(N = length(.data$rowId), 
                     O = sum(.data$outcomeCount>0)
                     ) %>% 
    dplyr::select(.data$year, .data$ageGroup, .data$gender, .data$N, .data$O) 
  
  # add all years:
  allYears <- result %>% dplyr::group_by(.data$ageGroup, .data$gender) %>%
    dplyr::summarize(N = sum(.data$N), 
                     O = sum(.data$O),
                     year = 'all'
    ) %>% dplyr::select(.data$year, .data$ageGroup, .data$gender, .data$N, .data$O) 
  # add all gender:
  allGender <- result %>% dplyr::group_by(.data$year, .data$ageGroup) %>%
    dplyr::summarize(N = sum(.data$N), 
                     O = sum(.data$O),
                     gender = -1
    ) %>% dplyr::select(.data$year, .data$ageGroup, .data$gender, .data$N, .data$O) 
  
  # add all gender:
  allAge <- result %>% dplyr::group_by(.data$year, .data$gender) %>%
    dplyr::summarize(N = sum(.data$N), 
                     O = sum(.data$O),
                     ageGroup = 'all'
    ) %>% dplyr::select(.data$year, .data$ageGroup, .data$gender, .data$N, .data$O) 
  
  result <- rbind(result, allYears, allGender, allAge)
  
  result$opercent <- result$O/result$N*100
  
  # censor
  if(!is.null(minCellCount)){
    result$opercent[result$O < minCellCount] <- -1
    result$N[result$N<minCellCount] <- paste0('<', minCellCount)
    result$O[result$O<minCellCount] <- paste0('<', minCellCount)
    
  }
  
  result$TAR <- TAR 
  result$analysisId <- analysisId
  result$cdmDatabaseName <- cdmDatabaseName
  result$cohortId <- cohortId
  result$outcomeId <- outcomeId
  
  return(result)
}
  
