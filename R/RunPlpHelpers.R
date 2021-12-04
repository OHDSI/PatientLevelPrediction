printHeader <- function(plpData, cohortId, outcomeId , analysisId, analysisName, ExecutionDateTime){
  
  ParallelLogger::logInfo(paste0('Patient-Level Prediction Package version ', utils::packageVersion("PatientLevelPrediction")))
  
  ParallelLogger::logInfo(paste0('Study started at: ', ExecutionDateTime))
  
  ParallelLogger::logInfo(sprintf('%-20s%s', 'AnalysisID: ',analysisId))
  ParallelLogger::logInfo(sprintf('%-20s%s', 'AnalysisName: ',analysisName))
  
  # add header to analysis log
  ParallelLogger::logInfo(sprintf('%-20s%s', 'CohortID: ', cohortId))
  ParallelLogger::logInfo(sprintf('%-20s%s', 'OutcomeID: ', outcomeId))
  ParallelLogger::logInfo(sprintf('%-20s%s', 'Cohort size: ', nrow(plpData$cohorts)))
  ParallelLogger::logInfo(sprintf('%-20s%s', 'Covariates: ', nrow(plpData$covariateData$covariateRef)))
 ## ParallelLogger::logInfo(sprintf('%-20s%s', 'Population size: ', nrow(population)))
 ## ParallelLogger::logInfo(sprintf('%-20s%s', 'Cases: ', sum(population$outcomeCount>0)))
  
  return(invisible(TRUE))
}


checkInputs <- function(inputs) {
  
  inputNames <- names(inputs)
  
  checkIsClass(inputs[['plpData']], c('plpData'))
  checkIsClass(inputs[['outcomeId']], c("numeric"))
  
  for(inputName in inputNames[!inputNames %in% c('plpData', 'outcomeId')]){
    
    ParallelLogger::logDebug(paste0(names(inputs[[inputName]]), ' : ',
      unlist(lapply(inputs[[inputName]], function(x) paste0(unlist(x), collapse= '-')))
    )
    )
    
    # check class is correct
    if(class(inputs[[inputName]]) != inputName){
      ParallelLogger::logError(paste0('Incorrect ', inputName))
      return(invisible(FALSE))
    }
    
  }
  
  # return all the settings
  return(invisible(TRUE))
}


#' @export
createExecuteSettings <- function(
  runSplitData = F,
  runSampleData = F,
  runfeatureEngineering = F,
  runPreprocessData = F,
  runModelDevelopment = F,
  runCovariateSummary = F
){
  
  checkIsClass(runSplitData, "logical")
  checkIsClass(runSampleData, "logical")
  checkIsClass(runfeatureEngineering, "logical")
  checkIsClass(runPreprocessData, "logical")
  checkIsClass(runModelDevelopment, "logical")
  checkIsClass(runCovariateSummary, "logical")
  
  result <- list(
    runSplitData = runSplitData,
    runSampleData = runSampleData,
    runfeatureEngineering = runfeatureEngineering,
    runPreprocessData = runPreprocessData,
    runModelDevelopment = runModelDevelopment,
    runCovariateSummary = runCovariateSummary
  )
  class(result) <- 'executeSettings'
  return(result)
}

#' @export
createDefaultExecuteSettings <- function(){
  createExecuteSettings(
    runSplitData = T,
    runSampleData = F,
    runfeatureEngineering = F,
    runPreprocessData = T,
    runModelDevelopment = T,
    runCovariateSummary = T
  )
}
