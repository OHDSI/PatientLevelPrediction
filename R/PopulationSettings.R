# @file StudyPopulation.R
#
# Copyright 2021 Observational Health Data Sciences and Informatics
#
# This file is part of CohortMethod
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


#' create the study population settings
#'
#' @details
#' Takes as input the inputs to create study population
#' @param binary                Forces the outcomeCount to be 0 or 1 (use for binary prediction problems)                              
#' @param includeAllOutcomes    (binary) indicating whether to include people with outcomes who are not observed for the whole at risk period
#' @param firstExposureOnly            Should only the first exposure per subject be included? Note that
#'                                     this is typically done in the \code{createStudyPopulation} function,
#' @param washoutPeriod                The mininum required continuous observation time prior to index
#'                                     date for a person to be included in the cohort.
#' @param removeSubjectsWithPriorOutcome  Remove subjects that have the outcome prior to the risk window start?
#' @param priorOutcomeLookback            How many days should we look back when identifying prior outcomes?
#' @param requireTimeAtRisk      Should subject without time at risk be removed?
#' @param minTimeAtRisk          The minimum number of days at risk required to be included
#' @param riskWindowStart        The start of the risk window (in days) relative to the index date (+
#'                               days of exposure if the \code{addExposureDaysToStart} parameter is
#'                               specified).
#' @param startAnchor	           The anchor point for the start of the risk window. Can be "cohort start" or "cohort end".
#' @param riskWindowEnd          The end of the risk window (in days) relative to the index data (+
#'                               days of exposure if the \code{addExposureDaysToEnd} parameter is
#'                               specified).
#' @param endAnchor              The anchor point for the end of the risk window. Can be "cohort start" or "cohort end".
#' @param restrictTarToCohortEnd If using a survival model and you want the time-at-risk to end at the cohort end date set this to T
#' @return
#' A list containing all the settings required for creating the study population
#' @export
createStudyPopulationSettings <- function(
  binary = T,
  includeAllOutcomes = T,
  firstExposureOnly = FALSE,
  washoutPeriod = 0,
  removeSubjectsWithPriorOutcome = TRUE,
  priorOutcomeLookback = 99999,
  requireTimeAtRisk = T,
  minTimeAtRisk = 364,
  riskWindowStart = 1,
  startAnchor = 'cohort start',
  riskWindowEnd = 365,
  endAnchor = "cohort start",
  restrictTarToCohortEnd = F
){
  
  checkIsClass(binary, "logical")
  checkNotNull(binary)
  
  checkIsClass(includeAllOutcomes, "logical")
  checkNotNull(includeAllOutcomes)
  
  checkIsClass(firstExposureOnly, "logical")
  checkNotNull(firstExposureOnly)
  
  checkIsClass(washoutPeriod, c("numeric", "integer"))
  checkNotNull(washoutPeriod)
  checkHigherEqual(washoutPeriod, 0)
  
  checkIsClass(removeSubjectsWithPriorOutcome, "logical")
  checkNotNull(removeSubjectsWithPriorOutcome)
  
  checkIsClass(priorOutcomeLookback, c("numeric", "integer"))
  checkNotNull(priorOutcomeLookback)
  checkHigherEqual(priorOutcomeLookback, 0)
  
  checkIsClass(requireTimeAtRisk, "logical")
  checkNotNull(requireTimeAtRisk)
  
  if(requireTimeAtRisk){
    checkIsClass(minTimeAtRisk, c("numeric", "integer"))
    checkNotNull(minTimeAtRisk)
    checkHigherEqual(minTimeAtRisk, 0)
  }
  
  checkIsClass(riskWindowStart, c("numeric", "integer"))
  checkNotNull(riskWindowStart)
  
  checkIsClass(startAnchor, c("character"))
  checkNotNull(startAnchor)
  if(!startAnchor%in%c('cohort start', 'cohort end')){
    stop('Incorrect startAnchor')
  }
  
  checkIsClass(riskWindowEnd, c("numeric", "integer"))
  checkNotNull(riskWindowEnd)
  
  checkIsClass(endAnchor, c("character"))
  checkNotNull(endAnchor)
  if(!endAnchor%in%c('cohort start', 'cohort end')){
    stop('Incorrect endAnchor')
  }
  
  if(startAnchor == endAnchor){
    checkHigherEqual(riskWindowEnd, riskWindowStart)
  }

  checkIsClass(restrictTarToCohortEnd, "logical")
  checkNotNull(restrictTarToCohortEnd)
  
  if(requireTimeAtRisk){
    if(startAnchor==endAnchor){
      if(minTimeAtRisk>(riskWindowEnd-riskWindowStart)){
        warning('issue: minTimeAtRisk is greater than max possible time-at-risk')
      }
    }
  }

  result <- list(
    binary = binary,
    includeAllOutcomes = includeAllOutcomes,
    firstExposureOnly = firstExposureOnly,
    washoutPeriod = washoutPeriod,
    removeSubjectsWithPriorOutcome = removeSubjectsWithPriorOutcome,
    priorOutcomeLookback = priorOutcomeLookback,
    requireTimeAtRisk = requireTimeAtRisk, 
    minTimeAtRisk = minTimeAtRisk,
    riskWindowStart = riskWindowStart,
    startAnchor = startAnchor,
    riskWindowEnd = riskWindowEnd,
    endAnchor = endAnchor, 
    restrictTarToCohortEnd = restrictTarToCohortEnd)
  
  class(result) <- 'populationSettings'
  return(result)
  
}

#' Create a study population
#'
#' @details
#' Create a study population by enforcing certain inclusion and exclusion criteria, defining
#' a risk window, and determining which outcomes fall inside the risk window.
#'
#' @param plpData      An object of type \code{plpData} as generated using
#'                              \code{getplpData}.
#' @param outcomeId             The  ID of the outcome.
#' @param populationSettings    An object of class populationSettings created using \code{createPopulationSettings}
#' @param population            If specified, this population will be used as the starting point instead of the
#'                              cohorts in the \code{plpData} object.
#'
#' @return
#' A data frame specifying the study population. This data frame will have the following columns:
#' \describe{
#' \item{rowId}{A unique identifier for an exposure}
#' \item{subjectId}{The person ID of the subject}
#' \item{cohortStartdate}{The index date}
#' \item{outcomeCount}{The number of outcomes observed during the risk window}
#' \item{timeAtRisk}{The number of days in the risk window}
#' \item{survivalTime}{The number of days until either the outcome or the end of the risk window}
#' }
#'
#' @export
createStudyPopulation <- function(
  plpData,
  outcomeId,
  populationSettings,
  population = NULL
  ) {
  
  checkIsClass(populationSettings, 'populationSettings')
  
  binary <- populationSettings$binary
  includeAllOutcomes <- populationSettings$includeAllOutcomes
  firstExposureOnly <- populationSettings$firstExposureOnly
  washoutPeriod <- populationSettings$washoutPeriod 
  removeSubjectsWithPriorOutcome <- populationSettings$removeSubjectsWithPriorOutcome
  priorOutcomeLookback <- populationSettings$priorOutcomeLookback
  requireTimeAtRisk <- populationSettings$requireTimeAtRisk
  minTimeAtRisk <- populationSettings$minTimeAtRisk
  riskWindowStart <- populationSettings$riskWindowStart
  startAnchor <- populationSettings$startAnchor
  riskWindowEnd <- populationSettings$riskWindowEnd
  endAnchor <- populationSettings$endAnchor
  restrictTarToCohortEnd <- populationSettings$restrictTarToCohortEnd
  
  # parameter checks
  if(!class(plpData)%in%c('plpData')){
    ParallelLogger::logError('Check plpData format')
    stop('Wrong plpData input')
  }
  ParallelLogger::logDebug(paste0('outcomeId: ', outcomeId))
  checkNotNull(outcomeId)
  
  ParallelLogger::logDebug(paste0('binary: ', binary))
  ParallelLogger::logDebug(paste0('includeAllOutcomes: ', includeAllOutcomes))
  ParallelLogger::logDebug(paste0('firstExposureOnly: ', firstExposureOnly))
  ParallelLogger::logDebug(paste0('washoutPeriod: ', washoutPeriod))
  ParallelLogger::logDebug(paste0('removeSubjectsWithPriorOutcome: ', removeSubjectsWithPriorOutcome))
  ParallelLogger::logDebug(paste0('priorOutcomeLookback: ', priorOutcomeLookback))
  ParallelLogger::logDebug(paste0('requireTimeAtRisk: ', requireTimeAtRisk))
  ParallelLogger::logDebug(paste0('minTimeAtRisk: ', minTimeAtRisk))
  ParallelLogger::logDebug(paste0('restrictTarToCohortEnd: ', restrictTarToCohortEnd))
  ParallelLogger::logDebug(paste0('riskWindowStart: ', riskWindowStart))
  ParallelLogger::logDebug(paste0('startAnchor: ', startAnchor))
  ParallelLogger::logDebug(paste0('riskWindowEnd: ', riskWindowEnd))
  ParallelLogger::logDebug(paste0('endAnchor: ', endAnchor))
  ParallelLogger::logDebug(paste0('restrictTarToCohortEnd: ', restrictTarToCohortEnd))

  if (is.null(population)) {
    population <- plpData$cohorts
  }
  
  # save the metadata (should have the cohortId, outcomeId, plpDataSettings and population settings)
  metaData <- attr(population, "metaData")
  metaData$plpDataSettings <- plpData$metaData$restrictPlpDataSettings
  metaData$outcomeId <- outcomeId
  metaData$populationSettings <- populationSettings
  
  # set the existing attrition
  if(is.null(metaData$attrition)){
    metaData$attrition <- attr(plpData$cohorts,  'metaData')$attrition
  }
  if(!is.null(metaData$attrition)){
    metaData$attrition <- data.frame(outcomeId = metaData$attrition$outcomeId,
                                     description = metaData$attrition$description,
                                     targetCount = metaData$attrition$targetCount,
                                     uniquePeople = metaData$attrition$uniquePeople,
                                     outcomes = metaData$attrition$outcomes)
    if(sum(metaData$attrition$outcomeId==outcomeId)>0){
      metaData$attrition <- metaData$attrition[metaData$attrition$outcomeId==outcomeId,]
    } else{
      metaData$attrition <- NULL
    }
  }
  
  
  # ADD TAR
  oId <- outcomeId
  population <- population %>% 
    dplyr::mutate(startAnchor = startAnchor, startDay = riskWindowStart,
                  endAnchor = endAnchor, endDay = riskWindowEnd) %>%
    dplyr::mutate(tarStart = ifelse(.data$startAnchor == 'cohort start', .data$startDay, .data$startDay+ .data$daysToCohortEnd),
                  tarEnd = ifelse(.data$endAnchor == 'cohort start', .data$endDay, .data$endDay+ .data$daysToCohortEnd))  %>%
    dplyr::mutate(tarEnd = ifelse(.data$tarEnd>.data$daysToObsEnd, .data$daysToObsEnd,.data$tarEnd ))
    
  
  # censor at cohortEndDate:
  if(max(population$daysToCohortEnd)>0 & restrictTarToCohortEnd){
    ParallelLogger::logInfo('Restricting tarEnd to end of target cohort')
    population <- population %>% dplyr::mutate(tarEnd = ifelse(.data$tarEnd>.data$daysToCohortEnd, .data$daysToCohortEnd,.data$tarEnd ))
  }
    
  
  # get the outcomes during TAR
  outcomeTAR <- population %>% 
    dplyr::inner_join(plpData$outcomes, by ='rowId') %>% 
    dplyr::filter(.data$outcomeId == get('oId'))  %>% 
    dplyr::select(.data$rowId, .data$daysToEvent, .data$tarStart, .data$tarEnd) %>% 
    dplyr::filter(.data$daysToEvent >= .data$tarStart & .data$daysToEvent <= .data$tarEnd)  
  
  # prevent warnings when no results left
  if(nrow(as.data.frame(outcomeTAR))>0){
  outcomeTAR <- outcomeTAR %>%
    dplyr::group_by(.data$rowId) %>%
    dplyr::summarise(first = min(.data$daysToEvent),
                     ocount = length(unique(.data$daysToEvent)))  %>% 
    dplyr::select(.data$rowId, .data$first, .data$ocount)
  } else {
    outcomeTAR <- outcomeTAR %>% 
      dplyr::mutate(first = 0, ocount = 0) %>% 
      dplyr::select(.data$rowId, .data$first, .data$ocount) 
  }
  
  population <- population %>%
    dplyr::left_join(outcomeTAR, by = 'rowId')
  
  
  # get the initial row
  attrRow <- population %>% dplyr::group_by() %>%
    dplyr::summarise(outcomeId = get('oId'),
                     description = 'Initial plpData cohort or population',
                     targetCount = length(.data$rowId),
                     uniquePeople = length(unique(.data$subjectId)),
                     outcomes = sum(!is.na(.data$first)))  
  metaData$attrition <- rbind(metaData$attrition, attrRow)
  
  if (firstExposureOnly) {
    ParallelLogger::logTrace(paste("Restricting to first exposure"))
    
    population <- population %>%
      dplyr::arrange(.data$subjectId,.data$cohortStartDate) %>%
      dplyr::group_by(.data$subjectId) %>%
      dplyr::filter(dplyr::row_number(.data$subjectId)==1)
    
    attrRow <- population %>% dplyr::group_by() %>%
      dplyr::summarise(outcomeId = get('oId'),
                       description = 'First Exposure',
                       targetCount = length(.data$rowId),
                       uniquePeople = length(unique(.data$subjectId)),
                       outcomes = sum(!is.na(.data$first)))  
    metaData$attrition <- rbind(metaData$attrition, attrRow)
  }

  
  if(washoutPeriod) {
    ParallelLogger::logTrace(paste("Requiring", washoutPeriod, "days of observation prior index date"))
    msg <- paste("At least", washoutPeriod, "days of observation prior")
    population <- population %>%
      dplyr::mutate(washoutPeriod = washoutPeriod) %>%
      dplyr::filter(.data$daysFromObsStart >= .data$washoutPeriod)
    
    attrRow <- population %>% dplyr::group_by() %>%
      dplyr::summarise(outcomeId = get('oId'),
                       description = msg,
                       targetCount = length(.data$rowId),
                       uniquePeople = length(unique(.data$subjectId)),
                       outcomes = sum(!is.na(.data$first)))  
    metaData$attrition <- rbind(metaData$attrition, attrRow)
  }
  
  if(removeSubjectsWithPriorOutcome) {
      ParallelLogger::logTrace("Removing subjects with prior outcomes (if any)")
    
    # get the outcomes during TAR
    outcomeBefore <- population %>% 
      dplyr::inner_join(plpData$outcomes, by ='rowId') %>% 
      dplyr::filter(outcomeId == get('oId'))  %>% 
      dplyr::select(.data$rowId, .data$daysToEvent, .data$tarStart) %>% 
      dplyr::filter(.data$daysToEvent < .data$tarStart & .data$daysToEvent > -get('priorOutcomeLookback') ) 
    
    if(nrow(as.data.frame(outcomeBefore))>0){
      outcomeBefore %>%
        dplyr::group_by(.data$rowId) %>%
        dplyr::summarise(first = min(.data$daysToEvent))  %>% 
        dplyr::select(.data$rowId)
    }
      
    population <- population %>%
      dplyr::filter(!.data$rowId %in% outcomeBefore$rowId )
      
      attrRow <- population %>% dplyr::group_by() %>%
        dplyr::summarise(outcomeId = get('oId'),
                         description = "No prior outcome",
                         targetCount = length(.data$rowId),
                         uniquePeople = length(unique(.data$subjectId)),
                         outcomes = sum(!is.na(.data$first)))  
      metaData$attrition <- rbind(metaData$attrition, attrRow)
  }
  

  if (requireTimeAtRisk) {
    if(includeAllOutcomes){
      ParallelLogger::logTrace("Removing non outcome subjects with insufficient time at risk (if any)")
      
      
      population <- population %>%
        dplyr::filter(!is.na(.data$first) | .data$tarEnd >= .data$tarStart + minTimeAtRisk )
      
      attrRow <- population %>% dplyr::group_by() %>%
        dplyr::summarise(outcomeId = get('oId'),
                         description = "Removing non-outcome subjects with insufficient time at risk (if any)",
                         targetCount = length(.data$rowId),
                         uniquePeople = length(unique(.data$subjectId)),
                         outcomes = sum(!is.na(.data$first)))  
      metaData$attrition <- rbind(metaData$attrition, attrRow)

    }
    else {
      ParallelLogger::logTrace("Removing subjects with insufficient time at risk (if any)")
      
      population <- population %>%
        dplyr::filter( .data$tarEnd >= .data$tarStart + minTimeAtRisk )
      
      attrRow <- population %>% dplyr::group_by() %>%
        dplyr::summarise(outcomeId = get('oId'),
                         description = "Removing subjects with insufficient time at risk (if any)",
                         targetCount = length(.data$rowId),
                         uniquePeople = length(unique(.data$subjectId)),
                         outcomes = sum(!is.na(.data$first)))  
      metaData$attrition <- rbind(metaData$attrition, attrRow)
      
    }
  } else {
    # remve any patients with negative timeAtRisk
    ParallelLogger::logTrace("Removing subjects with no time at risk (if any)")
    
    population <- population %>%
      dplyr::filter( .data$tarEnd >= .data$tarStart )
    
    attrRow <- population %>% dplyr::group_by() %>%
      dplyr::summarise(outcomeId = get('oId'),
                       description = "Removing subjects with no time at risk (if any))",
                       targetCount = length(.data$rowId),
                       uniquePeople = length(unique(.data$subjectId)),
                       outcomes = sum(!is.na(.data$first)))  
    metaData$attrition <- rbind(metaData$attrition, attrRow)
  }
  
  # now add columns to pop
  
  if(binary){
    ParallelLogger::logInfo("Outcome is 0 or 1")
  population <- population %>%
    dplyr::mutate(outcomeCount = ifelse(is.na(.data$ocount),0,1))
  } else{
    ParallelLogger::logTrace("Outcome is count")
    population <- population %>%
      dplyr::mutate(outcomeCount = ifelse(is.na(.data$ocount),0,.data$ocount))
  }
  
  population <- population %>%
    dplyr::mutate(timeAtRisk = .data$tarEnd - .data$tarStart + 1 ,
                  survivalTime = ifelse(.data$outcomeCount == 0, .data$tarEnd -.data$tarStart + 1, .data$first - .data$tarStart + 1),
                  daysToEvent = .data$first) %>%
    dplyr::select(.data$rowId, .data$subjectId, .data$cohortId, .data$cohortStartDate, .data$daysFromObsStart,
                  .data$daysToCohortEnd, .data$daysToObsEnd, .data$ageYear, .data$gender,
                  .data$outcomeCount, .data$timeAtRisk, .data$daysToEvent, .data$survivalTime)

    # check outcome still there
    if(sum(!is.na(population$daysToEvent))==0){
      ParallelLogger::logWarn('No outcomes left...')
      return(NULL)
    }
  
  population <- as.data.frame(population)
  
  attr(population, "metaData") <- metaData
  return(population)
}


getCounts <- function(population,description = "") {
  persons <- length(unique(population$subjectId))
  targets <- nrow(population)
  
  counts <- data.frame(description = description,
                       targetCount= targets,
                       uniquePeople = persons)
  return(counts)
}

getCounts2 <- function(cohort,outcomes, description = "") {
  persons <- length(unique(cohort$subjectId))
  targets <- nrow(cohort)
  
  outcomes <- stats::aggregate(cbind(count = outcomeId) ~ outcomeId, 
                        data = outcomes, 
                        FUN = function(x){NROW(x)})
  
  counts <- data.frame(outcomeId = outcomes$outcomeId,
                       description = description,
                       targetCount= targets,
                       uniquePeople = persons,
                       outcomes = outcomes$count)
  return(counts)
}

