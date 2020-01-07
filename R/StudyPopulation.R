# @file StudyPopulation.R
#
# Copyright 2019 Observational Health Data Sciences and Informatics
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

#' Create a study population
#'
#' @details
#' Create a study population by enforcing certain inclusion and exclusion criteria, defining
#' a risk window, and determining which outcomes fall inside the risk window.
#'
#' @param plpData      An object of type \code{plpData} as generated using
#'                              \code{getDbplpData}.
#' @param population            If specified, this population will be used as the starting point instead of the
#'                              cohorts in the \code{plpData} object.
#' @param binary                Forces the outcomeCount to be 0 or 1 (use for binary prediction problems)                              
#' @param outcomeId             The  ID of the outcome. If not specified, no outcome-specific transformations will
#'                              be performed.
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
#' @param addExposureDaysToStart   Add the length of exposure the start of the risk window?
#' @param riskWindowEnd          The end of the risk window (in days) relative to the index data (+
#'                               days of exposure if the \code{addExposureDaysToEnd} parameter is
#'                               specified).
#' @param addExposureDaysToEnd   Add the length of exposure the risk window?
#' @param verbosity              Sets the level of the verbosity. If the log level is at or higher in priority than the logger threshold, a message will print. The levels are:
#'                               \itemize{
#'                               \item{DEBUG}{Highest verbosity showing all debug statements}
#'                               \item{TRACE}{Showing information about start and end of steps}
#'                               \item{INFO}{Show informative information (Default)}
#'                               \item{WARN}{Show warning messages}
#'                               \item{ERROR}{Show error messages}
#'                               \item{FATAL}{Be silent except for fatal errors} 
#'                               }
#' @param ...                   Other inputs
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
createStudyPopulation <- function(plpData,
                                  population = NULL,
                                  outcomeId,
                                  binary = T,
                                  includeAllOutcomes = T,
                                  firstExposureOnly = FALSE,
                                  washoutPeriod = 0,
                                  removeSubjectsWithPriorOutcome = TRUE,
                                  priorOutcomeLookback = 99999,
                                  requireTimeAtRisk = T,
                                  minTimeAtRisk=365, # outcome nonoutcome
                                  riskWindowStart = 0,
                                  addExposureDaysToStart = FALSE,
                                  riskWindowEnd = 365,
                                  addExposureDaysToEnd = F,
                                  verbosity = "INFO",
                                  ...) {
  
  if(missing(verbosity)){
    verbosity <- "INFO"
  } else{
    if(!verbosity%in%c("DEBUG","TRACE","INFO","WARN","FATAL","ERROR")){
      stop('Incorrect verbosity string')
    }
  }
  # check logger
  if(length(ParallelLogger::getLoggers())==0){
    logger <- ParallelLogger::createLogger(name = "SIMPLE",
                                        threshold = verbosity,
                                        appenders = list(ParallelLogger::createConsoleAppender(layout = ParallelLogger::layoutTimestamp)))
    ParallelLogger::registerLogger(logger)
  }
  
  
  # parameter checks
  if(!class(plpData)%in%c('plpData.libsvm','plpData.coo','plpData')){
    ParallelLogger::logError('Check plpData format')
    stop('Wrong plpData input')
  }
  ParallelLogger::logDebug(paste0('outcomeId: ', outcomeId))
  checkNotNull(outcomeId)
  ParallelLogger::logDebug(paste0('binary: ', binary))
  checkBoolean(binary)
  ParallelLogger::logDebug(paste0('includeAllOutcomes: ', includeAllOutcomes))
  checkBoolean(includeAllOutcomes)
  ParallelLogger::logDebug(paste0('firstExposureOnly: ', firstExposureOnly))
  checkBoolean(firstExposureOnly)
  ParallelLogger::logDebug(paste0('washoutPeriod: ', washoutPeriod))
  checkHigherEqual(washoutPeriod,0)
  ParallelLogger::logDebug(paste0('removeSubjectsWithPriorOutcome: ', removeSubjectsWithPriorOutcome))
  checkBoolean(removeSubjectsWithPriorOutcome)
  if (removeSubjectsWithPriorOutcome){
    ParallelLogger::logDebug(paste0('priorOutcomeLookback: ', priorOutcomeLookback))
    checkHigher(priorOutcomeLookback,0)
  }
  ParallelLogger::logDebug(paste0('requireTimeAtRisk: ', requireTimeAtRisk))
  checkBoolean(requireTimeAtRisk)
  ParallelLogger::logDebug(paste0('minTimeAtRisk: ', minTimeAtRisk))
  checkHigherEqual(minTimeAtRisk,0)
  ParallelLogger::logDebug(paste0('riskWindowStart: ', riskWindowStart))
  checkHigherEqual(riskWindowStart,0)
  ParallelLogger::logDebug(paste0('addExposureDaysToStart: ', addExposureDaysToStart))
  checkBoolean(addExposureDaysToStart)
  ParallelLogger::logDebug(paste0('riskWindowEnd: ', riskWindowEnd))
  checkHigherEqual(riskWindowEnd,0)
  ParallelLogger::logDebug(paste0('addExposureDaysToEnd: ', addExposureDaysToEnd))
  checkBoolean(addExposureDaysToEnd)
  
  if(requireTimeAtRisk){
    if(addExposureDaysToStart==addExposureDaysToEnd){
      if(minTimeAtRisk>(riskWindowEnd-riskWindowStart)){
        warning('issue: minTimeAtRisk is greater than max possible time-at-risk')
      }
    }
  }
  
  if (is.null(population)) {
    population <- plpData$cohorts
  }
  
  # save the metadata
  metaData <- attr(population, "metaData")
  metaData$outcomeId <- outcomeId
  metaData$binary <- binary
  metaData$includeAllOutcomes <- includeAllOutcomes
  metaData$firstExposureOnly = firstExposureOnly
  metaData$washoutPeriod = washoutPeriod
  metaData$removeSubjectsWithPriorOutcome = removeSubjectsWithPriorOutcome
  metaData$priorOutcomeLookback = priorOutcomeLookback
  metaData$requireTimeAtRisk = requireTimeAtRisk
  metaData$minTimeAtRisk=minTimeAtRisk
  metaData$riskWindowStart = riskWindowStart
  metaData$addExposureDaysToStart = addExposureDaysToStart
  metaData$riskWindowEnd = riskWindowEnd
  metaData$addExposureDaysToEnd = addExposureDaysToEnd
  
  # get attriction for outcomeId
  if(!is.null(metaData$attrition$uniquePeople)){
    metaData$attrition <- metaData$attrition[metaData$attrition$outcomeId==outcomeId,c('description', 'targetCount', 'uniquePeople', 'outcomes')]
  } else {
    if(!is.null(attr(plpData$cohorts,  'metaData')$attrition)){
    metaData$attrition <- data.frame(outcomeId=outcomeId,description=metaData$attrition$description, 
                                     targetCount=attr(plpData$cohorts,  'metaData')$attrition$persons, uniquePeople=0,
                                     outcomes= metaData$attrition$outcomes)
    } else {
      metaData$attrition <- c()
    }

  }
  
  if (firstExposureOnly) {
    ParallelLogger::logTrace("Keeping only first exposure per subject")
    population <- population[order(population$subjectId, as.Date(population$cohortStartDate)), ]
    idx <- duplicated(population[, c("subjectId", "cohortId")])
    population <- population[!idx, ]
    
    # get outcome count:
    outCount <- 0
    if(!missing(outcomeId) && !is.null(outcomeId))
      outCount <- sum(plpData$outcomes$rowId%in%population$rowId & plpData$outcomes$outcomeId == outcomeId)
    metaData$attrition <- rbind(metaData$attrition, getCounts(population,outCount, "First exposure only"))
  }
  if (washoutPeriod) {
    ParallelLogger::logTrace(paste("Requiring", washoutPeriod, "days of observation prior index date"))
    population <- population[population$daysFromObsStart >= washoutPeriod,]
    outCount <- 0
    if(!missing(outcomeId) && !is.null(outcomeId))
      outCount <- sum(plpData$outcomes$rowId%in%population$rowId & plpData$outcomes$outcomeId == outcomeId)
    metaData$attrition <- rbind(metaData$attrition, getCounts(population, outCount, paste("At least", washoutPeriod, "days of observation prior")))
  }
  if (removeSubjectsWithPriorOutcome) {
    if (missing(outcomeId) || is.null(outcomeId)){
      ParallelLogger::logTrace("No outcome specified so skipping removing people with prior outcomes")
    } else {
      ParallelLogger::logTrace("Removing subjects with prior outcomes (if any)")
      outcomes <- plpData$outcomes[plpData$outcomes$outcomeId == outcomeId, ]
      if (addExposureDaysToStart) {
        outcomes <- merge(outcomes, population[, c("rowId","daysToCohortEnd")])
        priorOutcomeRowIds <- outcomes$rowId[outcomes$daysToEvent > -priorOutcomeLookback & outcomes$daysToEvent < outcomes$daysToCohortEnd + riskWindowStart]
      } else {
        priorOutcomeRowIds <- outcomes$rowId[outcomes$daysToEvent > -priorOutcomeLookback & outcomes$daysToEvent < riskWindowStart]
      }
      population <- population[!(population$rowId %in% priorOutcomeRowIds), ]
      outCount <- 0
      if(!missing(outcomeId) && !is.null(outcomeId))
        outCount <- sum(plpData$outcomes$rowId%in%population$rowId & plpData$outcomes$outcomeId == outcomeId)
      metaData$attrition <- rbind(metaData$attrition, getCounts(population,outCount, paste("No prior outcome")))
    }
  }
  # Create risk windows:
  population$riskStart <- riskWindowStart
  if (addExposureDaysToStart) {
    population$riskStart <- population$riskStart + population$daysToCohortEnd
  }
  population$riskEnd <- riskWindowEnd
  if (addExposureDaysToEnd) {
    population$riskEnd <- population$riskEnd + population$daysToCohortEnd
  }
  #trancate end if it runs over obs date end
  population$riskEnd[population$riskEnd > population$daysToObsEnd] <- population$daysToObsEnd[population$riskEnd > population$daysToObsEnd]
  
  if (requireTimeAtRisk) {
    if(includeAllOutcomes){
      ParallelLogger::logTrace("Removing non outcome subjects with insufficient time at risk (if any)")
      
      #people with the outcome:
      outcomes <- plpData$outcomes[plpData$outcomes$outcomeId == outcomeId, ]
      outcomes <- merge(outcomes, population[, c("rowId", "riskStart", "riskEnd")])
      outcomes <- outcomes[outcomes$daysToEvent >= outcomes$riskStart & outcomes$daysToEvent <= outcomes$riskEnd, ]
      outcomePpl <- unique(outcomes$rowId)
      
      noAtRiskTimeRowIds <- population$rowId[population$riskEnd < population$riskStart + minTimeAtRisk ]
      noAtRiskTimeRowIds <- noAtRiskTimeRowIds[!noAtRiskTimeRowIds%in%outcomePpl]
      population <- population[!(population$rowId %in% noAtRiskTimeRowIds), ]
    }
    else {
      ParallelLogger::logTrace("Removing subjects with insufficient time at risk (if any)")
      noAtRiskTimeRowIds <- population$rowId[population$riskEnd < population$riskStart + minTimeAtRisk ]
      population <- population[!(population$rowId %in% noAtRiskTimeRowIds), ]
    }
    outCount <- 0
    if(!missing(outcomeId) && !is.null(outcomeId))
      outCount <- sum(plpData$outcomes$rowId%in%population$rowId & plpData$outcomes$outcomeId == outcomeId)
    metaData$attrition <- rbind(metaData$attrition, getCounts(population, outCount, paste("Have time at risk")))
  } else {
    # remve any patients with negative timeAtRisk
    ParallelLogger::logTrace("Removing subjects with no time at risk (if any)")
    noAtRiskTimeRowIds <- population$rowId[population$riskEnd < population$riskStart ]
    population <- population[!(population$rowId %in% noAtRiskTimeRowIds), ]
    outCount <- 0
    if(!missing(outcomeId) && !is.null(outcomeId))
      outCount <- sum(plpData$outcomes$rowId%in%population$rowId & plpData$outcomes$outcomeId == outcomeId)
    metaData$attrition <- rbind(metaData$attrition, getCounts(population, outCount, paste("Have time at risk")))
    
  }
  if (missing(outcomeId) || is.null(outcomeId)){
    ParallelLogger::logTrace("No outcome specified so not creating outcome and time variables")
  } else {
    # Select outcomes during time at risk
    outcomes <- plpData$outcomes[plpData$outcomes$outcomeId == outcomeId, ]
    outcomes <- merge(outcomes, population[, c("rowId", "riskStart", "riskEnd")])
    outcomes <- outcomes[outcomes$daysToEvent >= outcomes$riskStart & outcomes$daysToEvent <= outcomes$riskEnd, ]
    
    # check outcome still there
    if(nrow(outcomes)==0){
      population <- NULL
      ParallelLogger::logWarn('No outcomes left...')
      return(population)
    }
    
    # Create outcome count column
    if(binary){
      ParallelLogger::logInfo("Outcome is 0 or 1")
      one <- function(x) return(1)
      outcomeCount <- stats::aggregate(outcomeId ~ rowId, data = outcomes, one)
    } else {
      ParallelLogger::logTrace("Outcome is count")
      outcomeCount <- stats::aggregate(outcomeId ~ rowId, data = outcomes, length)
    }
    colnames(outcomeCount)[colnames(outcomeCount) == "outcomeId"] <- "outcomeCount"
    population <- merge(population, outcomeCount[, c("rowId", "outcomeCount")], all.x = TRUE)
    population$outcomeCount[is.na(population$outcomeCount)] <- 0
    
    # Create time at risk column
    population$timeAtRisk <- population$riskEnd - population$riskStart + 1
    
    # Create survival time column
    firstOutcomes <- outcomes[order(outcomes$rowId, outcomes$daysToEvent), ]
    firstOutcomes <- firstOutcomes[!duplicated(firstOutcomes$rowId), ]
    population <- merge(population, firstOutcomes[, c("rowId", "daysToEvent")], all.x = TRUE)
    population$survivalTime <- population$timeAtRisk
    population$survivalTime[population$outcomeCount != 0] <- population$daysToEvent[population$outcomeCount != 0] - population$riskStart[population$outcomeCount != 0] + 1
  }
  population$riskStart <- NULL
  population$riskEnd <- NULL
  attr(population, "metaData") <- metaData
  return(population)
}

limitCovariatesToPopulation <- function(covariates, rowIds) {
  idx <- !is.na(ffbase::ffmatch(covariates$rowId, rowIds))
  if(sum(idx)!=0){
    covariates <- covariates[ffbase::ffwhich(idx, idx == TRUE), ]
  }else{
    stop('No covariates')
  }
  return(covariates)
}

#' Get the attrition table for a population
#'
#' @param object   Either an object of type \code{plpData}, a population object generated by functions
#'                     like \code{createStudyPopulation}, or an object of type \code{outcomeModel}.
#'
#' @return
#' A data frame specifying the number of people and exposures in the population after specific steps of filtering.
#'
#'
#' @export
getAttritionTable <- function(object) {
  if (is(object, "plpData")) {
    object = object$cohorts
  }
  if (methods::is(object, "outcomeModel")){
    return(object$attrition)
  } else {
    return(attr(object, "metaData")$attrition)
  }
}

getCounts <- function(population,outCount, description = "") {
  persons <- length(unique(population$subjectId))
  targets <- nrow(population)
  
  counts <- data.frame(description = description,
                       targetCount= targets,
                       uniquePeople = persons,
                       outcomes = outCount)
  return(counts)
}

getCounts2 <- function(cohort,outcomes, description = "") {
  persons <- length(unique(cohort$subjectId))
  targets <- nrow(cohort)
  
  outcomes <- aggregate(cbind(count = outcomeId) ~ outcomeId, 
                        data = outcomes, 
                        FUN = function(x){NROW(x)})
  
  counts <- data.frame(outcomeId = outcomes$outcomeId,
                       description = description,
                       targetCount= targets,
                       uniquePeople = persons,
                       outcomes = outcomes$count)
  return(counts)
}
