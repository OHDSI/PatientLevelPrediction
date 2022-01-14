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
  checkIsClass(inputs[['outcomeId']], c("numeric", "integer"))
  
  for(inputName in inputNames[!inputNames %in% c('plpData', 'outcomeId')]){
    
    ParallelLogger::logDebug(paste0(names(inputs[[inputName]]), ' : ',
      unlist(lapply(inputs[[inputName]], function(x) paste0(unlist(x), collapse= '-')))
    )
    )
    
    # check class is correct
    if(class(inputs[[inputName]]) != inputName){
      ParallelLogger::logError(paste0('Incorrect ', inputName))
      stop('Bad input')
    }
    
  }
  
  # return all the settings
  return(invisible(TRUE))
}



#' Creates list of settings specifying what parts of runPlp to execute
#'
#' @details
#' define what parts of runPlp to execute
#'
#' @param runSplitData            TRUE or FALSE whether to split data into train/test
#' @param runSampleData           TRUE or FALSE whether to over or under sample
#' @param runfeatureEngineering   TRUE or FALSE whether to do feature engineering
#' @param runPreprocessData       TRUE or FALSE whether to do preprocessing
#' @param runModelDevelopment     TRUE or FALSE whether to develop the model
#' @param runCovariateSummary     TRUE or FALSE whether to create covariate summary           
#'
#' @return
#' list with TRUE/FALSE for each part of runPlp
#'
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


#' Creates default list of settings specifying what parts of runPlp to execute
#'
#' @details
#' runs split, preprocess, model development and covariate summary
#'
#' @return
#' list with TRUE for split, preprocess, model development and covariate summary
#'
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
