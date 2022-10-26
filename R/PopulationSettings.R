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
    restrictTarToCohortEnd = restrictTarToCohortEnd
    )
  
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
#' @importFrom data.table := 
#' @export
createStudyPopulation <- function(
  plpData,
  outcomeId,
  populationSettings,
  population = NULL
  ) {
  startTime <- Sys.time()
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
  if(!inherits(x = plpData, what = c('plpData'))){
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
    population <- rlang::duplicate(plpData$cohorts)
  }
  
  # save the metadata (should have the ?targetId, outcomeId, plpDataSettings and population settings)
  metaData <- attr(population, "metaData")
  metaData$restrictPlpDataSettings <- plpData$metaData$restrictPlpDataSettings
  metaData$outcomeId <- outcomeId
  metaData$populationSettings <- populationSettings # this will overwrite an existing setting
  
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
  
  data.table::setDT(population)
  # ADD TAR
  oId <- outcomeId
  population[, c('startAnchor','startDay', 'endAnchor', 'endDay') := .(startAnchor, riskWindowStart, endAnchor, riskWindowEnd)]
  population[, c('tarStart', 'tarEnd') := .(ifelse(startAnchor == 'cohort start', startDay, startDay + daysToCohortEnd),
                                            ifelse(endAnchor == 'cohort start', endDay, endDay + daysToCohortEnd))]
  population[, c('tarEnd') := .(ifelse(tarEnd > daysToObsEnd, daysToObsEnd, tarEnd))]
    
  
  # censor at cohortEndDate:
  if(max(population$daysToCohortEnd)>0 & restrictTarToCohortEnd){
    ParallelLogger::logInfo('Restricting tarEnd to end of target cohort')
    population[, tarEnd := ifelse(tarEnd > daysToCohortEnd, daysToCohortEnd, tarEnd)]
  }
    
  
  # get the outcomes during TAR
  outcomeTAR <- population[plpData$outcomes, on = 'rowId', nomatch = 0][outcomeId == get('oId')][, .(rowId, daysToEvent, tarStart, tarEnd)][daysToEvent >= tarStart & daysToEvent <= tarEnd]
  
  
  # prevent warnings when no results left
  if(nrow(as.data.frame(outcomeTAR))>0){
    outcomeTAR <- outcomeTAR[, by=rowId,
                             .(first = min(daysToEvent),
                               ocount = data.table::uniqueN(daysToEvent))][, .(rowId, first, ocount)]
  } else {
    outcomeTAR <- outcomeTAR[, c('first', 'ocount') := .(0 ,0)][, .(rowId, first, ocount)] 
  }
  
  population[outcomeTAR, on=.(rowId), c('first', 'ocount') := .(i.first, i.ocount)]
  
  
  # get the initial row
  attrRow <- population[,
                        .(outcomeId = get('oId'),
                          description = 'Initial plpData cohort or population',
                          targetCount = .N,
                          uniquePeople = data.table::uniqueN(subjectId),
                          outcomes = sum(!is.na(first)))]
  metaData$attrition <- rbind(metaData$attrition, attrRow)
  
  if (firstExposureOnly) {
    ParallelLogger::logTrace(paste("Restricting to first exposure"))
    
    population <- population[order(subjectId, cohortStartDate)][!duplicated(subjectId),]
    
    
    attrRow <- population[,  .(outcomeId = get('oId'), 
                               description = 'First Exposure', 
                               targetCount = .N, 
                               uniquePeople = data.table::uniqueN(subjectId),
                               outcomes = sum(!is.na(first)))] 
    metaData$attrition <- rbind(metaData$attrition, attrRow)
  }

  
  if(washoutPeriod) {
    ParallelLogger::logTrace(paste("Requiring", washoutPeriod, "days of observation prior index date"))
    msg <- paste("At least", washoutPeriod, "days of observation prior")
    population <- population[, washoutPeriod := washoutPeriod][daysFromObsStart >= washoutPeriod,]
    
    attrRow <- population[,  .(outcomeId = get('oId'), 
                               description = msg, 
                               targetCount = .N, 
                               uniquePeople = data.table::uniqueN(subjectId),
                               outcomes = sum(!is.na(first)))]
    metaData$attrition <- rbind(metaData$attrition, attrRow)
  }
  
  if(removeSubjectsWithPriorOutcome) {
      ParallelLogger::logTrace("Removing subjects with prior outcomes (if any)")
    
    # get the outcomes during TAR
    outcomeBefore <- population[plpData$outcomes, on='rowId', nomatch=0][outcomeId == get('oId'), .(rowId, daysToEvent, tarStart)][daysToEvent < tarStart & daysToEvent > -get('priorOutcomeLookback')]
    
    if(nrow(as.data.frame(outcomeBefore))>0){
      outcomeBefore <- outcomeBefore[, by='rowId',
                                     .(first=min(daysToEvent))][, .(rowId)]
    }
      
    population <- population[!rowId %in% outcomeBefore$rowId,]
      
    attrRow <- population[,  .(outcomeId = get('oId'), 
                               description = 'No prior outcome', 
                               targetCount = .N, 
                               uniquePeople = data.table::uniqueN(subjectId),
                               outcomes = sum(!is.na(first)))]
    metaData$attrition <- rbind(metaData$attrition, attrRow)
  }
  

  if (requireTimeAtRisk) {
    if(includeAllOutcomes){
      ParallelLogger::logTrace("Removing non outcome subjects with insufficient time at risk (if any)")
      
      
      population <- population[!is.na(first) | tarEnd >= tarStart + minTimeAtRisk,]
      
      
      attrRow <- population[,  .(outcomeId = get('oId'),
                                 description = 'Removing non-outcome subjects with insufficient time at risk (if any)',
                                 targetCount = .N,
                                 uniquePeople = data.table::uniqueN(subjectId),
                                 outcomes = sum(!is.na(first)))]
      metaData$attrition <- rbind(metaData$attrition, attrRow)

    }
    else {
      ParallelLogger::logTrace("Removing subjects with insufficient time at risk (if any)")
      
      population <- population[tarEnd >= tarStart + minTimeAtRisk,]
      
      attrRow <- population[,  .(outcomeId = get('oId'),
                                 description = 'Removing non-outcome subjects with insufficient time at risk (if any)',
                                 targetCount = .N,
                                 uniquePeople = data.table::uniqueN(subjectId),
                                 outcomes = sum(!is.na(first)))]
      metaData$attrition <- rbind(metaData$attrition, attrRow)
      
    }
  } else {
    # remove any patients with negative timeAtRisk
    ParallelLogger::logTrace("Removing subjects with no time at risk (if any)")
    
    population <- population[tarEnd >= tarStart,]
    
    attrRow <- population[,  .(outcomeId = get('oId'),
                               description = 'Removing subjects with no time at risk (if any)',
                               targetCount = .N,
                               uniquePeople = data.table::uniqueN(subjectId),
                               outcomes = sum(!is.na(first)))] 
    metaData$attrition <- rbind(metaData$attrition, attrRow)
  }
  
  # now add columns to pop
  
  if(binary){
    ParallelLogger::logInfo("Outcome is 0 or 1")
    population[, outcomeCount := ifelse(is.na(ocount), 0, 1)]
    
  } else{
    ParallelLogger::logTrace("Outcome is count")
    population[, outcomeCount := ifelse(is.na(ocount), 0, ocount)]
  }
  
  population <- population[, c('timeAtRisk', 'survivalTime', 'daysToEvent') := 
                             .(tarEnd - tarStart + 1,
                               ifelse(outcomeCount ==0, tarEnd - tarStart + 1, first - tarStart + 1),
                               first)][,.(rowId, subjectId, targetId, cohortStartDate, daysFromObsStart,
                                          daysToCohortEnd, daysToObsEnd, ageYear, gender,
                                          outcomeCount, timeAtRisk, daysToEvent, survivalTime)]

    # check outcome still there
    if(sum(!is.na(population$daysToEvent))==0){
      ParallelLogger::logWarn('No outcomes left...')
      return(NULL)
    }
  data.table::setDF(population)
  
  data.table::setDF(metaData$attrition)
  population <- as.data.frame(population)
  
  attr(population, "metaData") <- metaData
  delta <- Sys.time() - startTime
  ParallelLogger::logInfo("Creating study population took ", signif(delta, 3), " ", attr(delta, "units"))
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

