printHeader <- function(plpData, cohortId, outcomeId , analysisId, ExecutionDateTime){
  
  ParallelLogger::logInfo(paste0('Patient-Level Prediction Package version ', utils::packageVersion("PatientLevelPrediction")))
  
  ParallelLogger::logInfo(paste0('Study started at: ', ExecutionDateTime))
  
  ParallelLogger::logInfo(sprintf('%-20s%s', 'AnalysisID: ',analysisId))
  
  # add header to analysis log
  ParallelLogger::logInfo(sprintf('%-20s%s', 'CohortID: ', cohortId))
  ParallelLogger::logInfo(sprintf('%-20s%s', 'OutcomeID: ', outcomeId))
  ParallelLogger::logInfo(sprintf('%-20s%s', 'Cohort size: ', nrow(plpData$cohorts)))
  ParallelLogger::logInfo(sprintf('%-20s%s', 'Covariates: ', nrow(plpData$covariateData$covariateRef)))
 ## ParallelLogger::logInfo(sprintf('%-20s%s', 'Population size: ', nrow(population)))
 ## ParallelLogger::logInfo(sprintf('%-20s%s', 'Cases: ', sum(population$outcomeCount>0)))
  
}


checkInputs <- function(plpData, outcomeId, ...) {
  
  inputs <- as.list(match.call())
  inputNames <- names(inputs)
  
  checkIsClass(inputs[['plpData']], c('plpData'))
  checkIsClass(inputs[['outcomeId']], c("numeric"))
  
  for(inputName in inputNames[!inputNames %in% c('plpData', 'outcomeId')]){
    
    # check class is correct
    if(class(inputs[[inputName]]) != inputName){
      
      # print settings
      ParallelLogger::logInfo(paste0(names(inputs[[inputName]]), ' : ',
                                      unlist(lapply(inputs[[inputName]], function(x) paste0(unlist(x), collapse= '-')))
      )
      )
      
      ParallelLogger::logError(paste0('Incorrect ', inputName))
      return(invisible(FALSE))
    }
    
  }
  
  # return all the settings
  return(invisible(TRUE))
}


dataSummary <- function(data){
  #Test/Train: lsit(covariates,covariateRef, label, folds)
  
  ParallelLogger::logInfo('Train Set:')
  result <- data$Train$label %>% 
    dplyr::inner_join(data$Train$folds, by = .data$rowId) %>% 
    dplyr::group_by(.data$index) %>%
    dplyr::summarise(N = length(.data$data$outcomeCount),
                     outcomes = sum(.data$data$outcomeCount)) %>% 
    dplyr::collect()
  
  ParallelLogger::logInfo(paste0('Fold ', result$index ,' ', result$N, ' patients with ', result$outcomes, ' outcomes'))
  
  
  result <- data$Train$covariates %>% 
    dplyr::group_by(.data$covariateId) %>% 
    dplyr::summarise(N = length(.data$covariateValue)) %>% 
    dplyr::collect()
  
  ParallelLogger::logInfo(paste0(nrow(result), ' covariates in train data'))
  
  if('Test' %in% names(data)){
    ParallelLogger::logInfo('Test Set:')
    result <- data$Test$label %>% 
      dplyr::collect()
    
    ParallelLogger::logInfo(paste0(nrow(result), ' patients with ', sum(result$outcomeCount>0), ' outcomes'))
    
  }
  
}







# check parameters
ParallelLogger::logTrace('Parameter Check Started')
ParallelLogger::logDebug(paste0('testSplit: ', testSplit))
checkInStringVector(testSplit, c('person','time', 'stratified','subject'))
ParallelLogger::logDebug(paste0('outcomeCount: ', sum(population[,'outcomeCount']>0)))
checkHigherEqual(sum(population[,'outcomeCount']>0), 25)
ParallelLogger::logDebug(paste0('plpData class: ', class(plpData)))



createDataProcessingSettings(minCovariateFraction = 0.001,
                             normalizeData = T)


createLogSettings(verbosity = 'DEBUG',
                  timeStamp = T,
                  name = 'runPlp Log')


createExecuteSettings(runProcessData = T,
                      runModelDevelopment = T,
                      runCovariateSummary = T)
