# @file CovariateSummary.R
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

#' covariateSummary
#'
#' @description
#' Summarises the covariateData to calculate the mean and standard deviation per covaraite
#' if the labels are input it also stratifies this by class label and if the trainRowIds and testRowIds
#' specifying the patients in the train/test sets respectively are input, these values are also stratified
#' by train and test set
#' @details
#' The function calculates various metrics to measure the performance of the model
#' @param covariateData                      The covariateData part of the plpData that is 
#'                                           extracted using \code{getPlpData}
#' @param cohort                             The patient cohort to calculate the summary                                          
#' @param labels                             A data.frame with the columns rowId and outcomeCount
#' @param strata                             A data.frame containing the columns rowId, strataName
#' @param variableImportance                 A data.frame with the columns covariateId and 
#'                                           value (the variable importance value) 
#' @param featureEngineering                 (currently not used )
#'                                           A function or list of functions specifying any feature engineering
#'                                           to create covariates before summarising                            
#'                                           
#' @return
#' A data.frame containing: CovariateCount	CovariateMean and CovariateStDev plus these values
#' for any specified stratification
#' @export
covariateSummary <- function(
  covariateData,
  cohort,
  labels = NULL, 
  strata = NULL,
  variableImportance = NULL,
  featureEngineering = NULL
){
  
  ParallelLogger::logInfo(paste0('Calculating covariate summary @ ', Sys.time()))
  ParallelLogger::logInfo('This can take a while...')
  
  if(missing(covariateData)){
    stop('Must enter the covariateData')
  }
  
  if(missing(cohort)){
    stop('Must enter the cohort of patients')
  }
  
  subsetList <- createCovariateSubsets(
    cohort = cohort,
    labels = labels, 
    strata = strata
  )
  
  # make this run in parallel for big speed improvements..
  covariateSummariesPerStrata <- lapply(subsetList, 
    function(x){
      do.call(
        covariateSummarySubset, 
        list(
          covariateData = covariateData,
          subset = x$subset$rowId,
          subsetName = x$subsetName,
          restrictCovariateDataToSubsetIds = T
        )
      )}
  )
  
  covariateSummary <- aggregateCovariateSummaries(
    covariateSummariesPerStrata = do.call(rbind, covariateSummariesPerStrata),
    labels = labels, 
    strata = strata
  )
  
  # add variable importance if input
  if(!is.null(variableImportance)){
    covariateSummary <- covariateSummary %>% 
      dplyr::left_join(variableImportance, by = 'covariateId')
  }
  
  # add covariate names 
  covariateSummary <- covariateData$covariateRef %>% 
    dplyr::collect() %>% dplyr::left_join(covariateSummary, by ='covariateId')
  
  ParallelLogger::logInfo(paste0('Finished covariate summary @ ', Sys.time()))
  
  return(covariateSummary)
}


# aggregate the covariateSummaries:
aggregateCovariateSummaries <- function(
  covariateSummariesPerStrata,
  labels, 
  strata
){
  
  # if no labels or strata
  if(is.null(labels) & is.null(strata)){
    ParallelLogger::logInfo('Aggregating with no labels or strata')
    result <- covariateSummariesPerStrata %>% 
      dplyr::select(
        .data$covariateId, 
        .data$CovariateCount, 
        .data$CovariateMean, 
        .data$CovariateStDev, 
      )
  }
  
  # if labels but no strata or strata and no labels
  if( ( !is.null(labels) & is.null(strata) ) | ( is.null(labels) & !is.null(strata) ) ){
    ParallelLogger::logInfo('Aggregating with only labels or strata')
    resultLabels <- covariateSummariesPerStrata %>% 
      dplyr::select(
        .data$group,
        .data$covariateId, 
        .data$CovariateCount, 
        .data$CovariateMean, 
        .data$CovariateStDev, 
      )

    resultLabels <- tidyr::pivot_longer(
      data = resultLabels, 
      cols = colnames(resultLabels)[!colnames(resultLabels) %in% c('covariateId','group')],
      names_to = 'variable', 
      values_to = 'value'
      )
    
    resultLabels <- resultLabels %>% 
      dplyr::mutate(group_variable = paste(.data$group, .data$variable, sep ='_')) %>%
      dplyr::select(-.data$group, -.data$variable)
    
    resultLabels <- tidyr::pivot_wider(
      data = resultLabels, 
      names_from = 'group_variable', 
      values_from = 'value', 
      values_fill = 0
      )
      
    #resultLabels <- reshape2::melt(resultLabels, id.vars = c('covariateId','group'))
    #resultLabels <- reshape2::dcast(resultLabels, covariateId~group+variable, fill = 0)
    
    resultLabels <- resultLabels %>% 
      dplyr::mutate(StandardizedMeanDiff = (.data$WithOutcome_CovariateMean - .data$WithNoOutcome_CovariateMean)/sqrt((.data$WithOutcome_CovariateStDev^2 + .data$WithNoOutcome_CovariateStDev^2)/2) )
    
    
    resultAll <- covariateSummariesPerStrata %>% 
      dplyr::group_by(.data$covariateId) %>% 
      dplyr::summarise(
        CovariateCount = sum(.data$CovariateCount), 
        CovariateMean = sum(.data$sumVal)/sum(.data$N),
        CovariateStDev = sqrt(sum(.data$sumSquares)/sum(.data$N) - (sum(.data$sumVal)/sum(.data$N))^2 )
      )
      
      result <-  resultAll %>% dplyr::left_join(resultLabels, by = 'covariateId')

  }
  
  # if strata and labels
  if( !is.null(labels) & !is.null(strata) ) {
    ParallelLogger::logInfo('Aggregating with labels and strata')
    # labels and strata
    resultLabelStratas <- covariateSummariesPerStrata %>% 
      dplyr::select(
        .data$group,
        .data$covariateId, 
        .data$CovariateCount, 
        .data$CovariateMean, 
        .data$CovariateStDev, 
      )
    
    resultLabelStratas <- tidyr::pivot_longer(
      data = resultLabelStratas, 
      cols = colnames(resultLabelStratas)[!colnames(resultLabelStratas) %in% c('covariateId','group')],
      names_to = 'variable', 
      values_to = 'value'
    )
    
    resultLabelStratas <- resultLabelStratas %>% 
      dplyr::mutate(group_variable = paste(.data$group, .data$variable, sep ='_')) %>%
      dplyr::select(-.data$group, -.data$variable)
    
    resultLabelStratas <- tidyr::pivot_wider(
      data = resultLabelStratas, 
      names_from = 'group_variable', 
      values_from = 'value', 
      values_fill = 0
    )
    
    #resultLabelStratas <- reshape2::melt(resultLabelStratas, id.vars = c('covariateId','group'))
    #resultLabelStratas <- reshape2::dcast(resultLabelStratas, covariateId~group+variable, fill = 0)
    
    # labels only
    resultLabels <- covariateSummariesPerStrata %>% 
      dplyr::mutate(
        groupLabel = sapply(.data$group, function(x){ ifelse(
          length( grep('WithNoOutcome', x))>0, 
          'WithNoOutcome',
          'WithOutcome' 
          )})
      ) %>%
      dplyr::group_by(.data$covariateId, .data$groupLabel) %>%
    dplyr::summarise(
      CovariateCount = sum(.data$CovariateCount), 
      CovariateMean = sum(.data$sumVal)/sum(.data$N),
      CovariateStDev = sqrt(sum(.data$sumSquares)/sum(.data$N) - (sum(.data$sumVal)/sum(.data$N))^2 )
    ) %>% 
      dplyr::select(
        .data$groupLabel,
        .data$covariateId, 
        .data$CovariateCount, 
        .data$CovariateMean, 
        .data$CovariateStDev
      )
    
    resultLabels <- tidyr::pivot_longer(
      data = resultLabels, 
      cols = colnames(resultLabels)[!colnames(resultLabels) %in% c('covariateId','groupLabel')],
      names_to = 'variable', 
      values_to = 'value'
    )
    
    resultLabels <- resultLabels %>% 
      dplyr::mutate(group_variable = paste(.data$groupLabel, .data$variable, sep ='_')) %>%
      dplyr::select(-.data$groupLabel, -.data$variable)
    
    resultLabels <- tidyr::pivot_wider(
      data = resultLabels, 
      names_from = 'group_variable', 
      values_from = 'value', 
      values_fill = 0
    )
    
    #resultLabels <- reshape2::melt(resultLabels, id.vars = c('covariateId','groupLabel'))
    #resultLabels <- reshape2::dcast(resultLabels, covariateId~groupLabel+variable, fill = 0)
    
    resultLabels <- resultLabels %>% 
      dplyr::mutate(StandardizedMeanDiff = (.data$WithOutcome_CovariateMean - .data$WithNoOutcome_CovariateMean)/sqrt((.data$WithOutcome_CovariateStDev^2 + .data$WithNoOutcome_CovariateStDev^2)/2) )
    
 
    # all results
    resultAll <- covariateSummariesPerStrata %>% 
      dplyr::group_by(.data$covariateId) %>% 
    dplyr::summarise(
      CovariateCount = sum(.data$CovariateCount), 
      CovariateMean = sum(.data$sumVal)/sum(.data$N),
      CovariateStDev = sqrt(sum(.data$sumSquares)/sum(.data$N) - (sum(.data$sumVal)/sum(.data$N))^2 )
    )
    
    result <-  resultAll %>% 
      dplyr::left_join(resultLabels, by = 'covariateId')  %>% 
      dplyr::left_join(resultLabelStratas, by = 'covariateId')
    
  }

  return(result)  
}


createCovariateSubsets <- function(
  cohort,
  labels = NULL, 
  strata = NULL
){
  
  if(!is.null(labels)){
    ParallelLogger::logInfo('Creating binary labels')
    cohort <- cohort %>% dplyr::inner_join(labels, by = 'rowId') %>%
      dplyr::mutate(label = ifelse(.data$outcomeCount==0, 'WithNoOutcome','WithOutcome'))
  } else{
    cohort$label <- ''
  }
  
  if(!is.null(strata)){
    ParallelLogger::logInfo('Joining with strata')
    cohort <- cohort %>% dplyr::inner_join(strata, by = 'rowId')
  } else{
    cohort$strataName <- ''
  }
  
  cohort <- cohort %>% 
    dplyr::mutate(finalStrata = paste0(.data$strataName,.data$label))
  
  finalStratas <- unique(cohort$finalStrata)
  
  result <- list()
  length(result) <- length(finalStratas)
  
  for(i in 1:length(finalStratas)){
    ParallelLogger::logInfo(paste0('calculating subset of strata ',i))
    subset <- cohort %>% 
      dplyr::filter(.data$finalStrata == finalStratas[[i]]) %>%
      dplyr::select(.data$rowId)
    
    result[[i]] <- list(
      subset = subset, 
      subsetName = finalStratas[[i]]
    )
      
  }
  
  return(result)
}



covariateSummarySubset <- function(
  covariateData, 
  subset, 
  subsetName = '',
  restrictCovariateDataToSubsetIds = T){
  
  N <- length(subset)
  
  if(restrictCovariateDataToSubsetIds){
    ParallelLogger::logInfo('Restricting to subgroup')
    covariates <- getCovariatesForGroup(
      covariateData, 
      restrictIds = subset
      )
  } else{
    covariates <- covariateData$covariates
  }
  
  ParallelLogger::logInfo(paste0('Calculating summary for subgroup ', subsetName))
  
  result <- covariates %>%
    dplyr::group_by(.data$covariateId) %>%
    dplyr::summarise(
      CovariateCount = dplyr::n(),
      sumVal = sum(.data$covariateValue,na.rm = TRUE),
      sumSquares = sum(.data$covariateValue^2,na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      CovariateMean = 1.0*.data$sumVal/N,
      CovariateStDev = sqrt(.data$sumSquares/N - (.data$sumVal/N)^2 ),
      N = N,
      group = subsetName
      ) %>% 
    dplyr::collect()
  
  return(result)
}

getCovariatesForGroup <- function(covariateData, restrictIds){
  # restrict covaraiteData to specified rowIds
  if(length(restrictIds)<200000){
    
    covariateData$restrictIds <- data.frame(rowId = restrictIds)
    #on.exit(covariateData$restrictIds <- NULL, add = T)
    
    newCovariates <- covariateData$covariates %>% 
      dplyr::inner_join(covariateData$restrictIds, by= 'rowId')
 
  } else{
    newCovariateData <- batchRestrict(
      covariateData, 
      data.frame(rowId = restrictIds), 
      sizeN = 10000000
      )
    
    newCovariates <- newCovariateData$covariates
    
  }
  
  # add index to rowId and covariateId?
  
  return(newCovariates)
}

