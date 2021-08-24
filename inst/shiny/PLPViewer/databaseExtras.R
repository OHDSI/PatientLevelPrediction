# repo files

editColnames <- function(cnames, edits){
  lwcnames <- tolower(cnames)
  
  for(edit in edits){
    if(tolower(edit)%in%lwcnames){
      cnames[tolower(edit)==lwcnames] <- edit
    }
  }
  return(cnames)
  
}

editTar <- function(summaryTable){
  
  summaryTable <- summaryTable %>% dplyr::mutate(TAR = paste0('(',trimws(.data$tarStartAnchor),' + ',.data$tarStartDay, ') - (',trimws(.data$tarEndAnchor),' + ',.data$tarEndDay, ')' )) %>%
    dplyr::select(-c(.data$tarStartAnchor, .data$tarStartDay, .data$tarEndAnchor, .data$tarEndDay))
  
  return(summaryTable)
}


getDbSummary <- function(con, mySchema ){
    ParallelLogger::logInfo("gettingDb summary")

    sql <- "SELECT distinct results.result_id, results.model_id as analysis_id, 
                    results.researcher_id, 
                                    databases.database_acronym AS Dev, 
                                    databases.database_acronym AS Val,
                                    targets.cohort_name AS T, outcomes.cohort_name AS O,
       models.model_name AS model, 
       models.covariate_setting_id, 
       tars.tar_start_day, tars.tar_start_anchor, tars.tar_end_day, tars.tar_end_anchor,
       ROUND(aucResult.auc, 3) as auc,
       ROUND(auprcResult.auprc,4) as auprc,
       nResult.population_size, 
       oResult.outcome_count,
       ROUND(nTest.test_size*100.0/nResult.population_size, 1) as eval_percent,
       ROUND(oResult.outcome_count*100.0/nResult.population_size,4) as outcome_percent
       
       FROM @my_schema.results INNER JOIN @my_schema.models 
        ON results.target_id = models.target_id and 
             results.outcome_id = models.outcome_id and 
             results.tar_id = models.tar_id and
             results.population_setting_id = models.population_setting_id and
             results.database_id = models.database_id
             
        LEFT JOIN (SELECT cohort_id, cohort_name FROM @my_schema.cohorts) AS targets ON results.target_id = targets.cohort_id
        LEFT JOIN (SELECT cohort_id, cohort_name FROM @my_schema.cohorts) AS outcomes ON results.outcome_id = outcomes.cohort_id
        LEFT JOIN @my_schema.databases ON results.database_id = databases.database_id 
        LEFT JOIN @my_schema.tars ON results.tar_id = tars.tar_id
        LEFT JOIN (SELECT result_id, value AS auc FROM @my_schema.evaluation_statistics where metric = 'AUC.auc' and eval in ('test','validation') ) AS aucResult ON results.result_id = aucResult.result_id
        LEFT JOIN (SELECT result_id, value AS auprc FROM @my_schema.evaluation_statistics where metric = 'AUPRC' and eval in ('test','validation') ) AS auprcResult ON results.result_id = auprcResult.result_id
        LEFT JOIN (SELECT result_id, sum(value) AS population_size FROM @my_schema.evaluation_statistics where metric = 'populationSize' group by result_id) AS nResult ON results.result_id = nResult.result_id
        LEFT JOIN (SELECT result_id, sum(value) AS outcome_count FROM @my_schema.evaluation_statistics where metric = 'outcomeCount' group by result_id) AS oResult ON results.result_id = oResult.result_id
        LEFT JOIN (SELECT result_id, value AS test_size FROM @my_schema.evaluation_statistics where metric = 'populationSize' and eval = 'test') AS nTest ON results.result_id = nTest.result_id;"
    
    sql <- SqlRender::render(sql = sql, my_schema = mySchema)
    
    summaryTable <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
    colnames(summaryTable) <- SqlRender::snakeCaseToCamelCase(colnames(summaryTable))
    
    summaryTable$t <- trimws(summaryTable$t)
    summaryTable$o <- trimws(summaryTable$o)
    
    summaryTable <- summaryTable %>% 
      dplyr::rename(`Covariate setting` = covariateSettingId) %>%
      dplyr::rename(`T Size` = populationSize) %>% 
      dplyr::rename(`O Count` = outcomeCount) %>%
      dplyr::rename(`Val (%)` = evalPercent) %>%
      dplyr::rename(`O Incidence (%)` = outcomePercent)
    
    summaryTable <- editTar(summaryTable)
    
    colnames(summaryTable) <- editColnames(cnames = colnames(summaryTable), 
                                           edits = c('AUC','AUPRC', 'T', 'O', 'Dev','Val', 'TAR', 'Model'))
    
    summaryTable$timeStamp <- 0
    summaryTable$Analysis <- summaryTable$analysisId
    ParallelLogger::logInfo("Got db summary")
    return(summaryTable[,c('Dev', 'Val', 'T','O', 'Model','Covariate setting',
                           'TAR', 'AUC', 'AUPRC', 
                           'T Size', 'O Count','Val (%)', 'O Incidence (%)', 'timeStamp', 'analysisId', 'researcherId', 'resultId', 'Analysis')])

}

#' Extract the validations from the predictionLibrary database
#'
#' @details
#' Load the validation results from database.
#' 
#' @param chosenRow  The row from the summaryTable of the selected result
#' 
#' @export# 
getValSummary <- function(con, mySchema, modelId ){
  ParallelLogger::logInfo("getting Val summary")
  
  sql <- "SELECT results.result_id, results.model_id as analysis_id, 
                results.researcher_id, 
                                --databases.database_acronym AS Dev, 
                                databases.database_acronym AS Val,
                                targets.cohort_name AS T, outcomes.cohort_name AS O,
   models.model_name AS model, 
   models.covariate_setting_id, 
   tars.tar_start_day, tars.tar_start_anchor, tars.tar_end_day, tars.tar_end_anchor,
   ROUND(aucResult.auc, 3) as auc,
   ROUND(auprcResult.auprc,4) as auprc,
   nResult.population_size, 
   oResult.outcome_count,
   ROUND(nTest.test_size*100.0/nResult.population_size, 1) as eval_percent,
   ROUND(oResult.outcome_count*100.0/nResult.population_size,4) as outcome_percent,
   ROUND(calibration_in_large, 3) as calibration_in_large
   
   FROM @my_schema.results INNER JOIN @my_schema.models 
    ON 
         --results.target_id = models.target_id and 
         --results.outcome_id = models.outcome_id and 
         results.tar_id = models.tar_id and
         results.population_setting_id = models.population_setting_id and
         models.model_id = @model_id
         
    LEFT JOIN (SELECT cohort_id, cohort_name FROM @my_schema.cohorts) AS targets ON results.target_id = targets.cohort_id
    LEFT JOIN (SELECT cohort_id, cohort_name FROM @my_schema.cohorts) AS outcomes ON results.outcome_id = outcomes.cohort_id
    LEFT JOIN @my_schema.databases ON results.database_id = databases.database_id 
    LEFT JOIN @my_schema.tars ON results.tar_id = tars.tar_id
    LEFT JOIN (SELECT result_id, value AS auc FROM @my_schema.evaluation_statistics where metric = 'AUC.auc' and eval in ('test','validation') ) AS aucResult ON results.result_id = aucResult.result_id
    LEFT JOIN (SELECT result_id, value AS auprc FROM @my_schema.evaluation_statistics where metric = 'AUPRC' and eval in ('test','validation') ) AS auprcResult ON results.result_id = auprcResult.result_id
    
    LEFT JOIN (SELECT result_id, value AS calibration_in_large FROM @my_schema.evaluation_statistics where metric = 'CalibrationInLarge' and eval in ('test','validation') ) AS CalibrationInLargeResult ON results.result_id = CalibrationInLargeResult.result_id

    LEFT JOIN (SELECT result_id, sum(value) AS population_size FROM @my_schema.evaluation_statistics where metric = 'populationSize' group by result_id) AS nResult ON results.result_id = nResult.result_id
    LEFT JOIN (SELECT result_id, sum(value) AS outcome_count FROM @my_schema.evaluation_statistics where metric = 'outcomeCount' group by result_id) AS oResult ON results.result_id = oResult.result_id
    LEFT JOIN (SELECT result_id, value AS test_size FROM @my_schema.evaluation_statistics where metric = 'populationSize' and eval = 'test') AS nTest ON results.result_id = nTest.result_id;"
  
  sql <- SqlRender::render(sql = sql, my_schema = mySchema, model_id = modelId)
  
  valTable <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
  colnames(valTable) <- SqlRender::snakeCaseToCamelCase(colnames(valTable))
  
  valTable$t <- trimws(valTable$t)
  valTable$o <- trimws(valTable$o)
  
  valTable <- valTable %>% 
    dplyr::rename(`Covariate setting` = covariateSettingId) %>%
    dplyr::rename(`T Size` = populationSize) %>% 
    dplyr::rename(`O Count` = outcomeCount) %>%
    dplyr::rename(`Val (%)` = evalPercent) %>%
    dplyr::rename(`O Incidence (%)` = outcomePercent)
  
  valTable <- editTar(valTable)
  
  #colnames(valTable) <- editColnames(cnames = colnames(valTable), 
  #                                   edits = c('AUC','AUPRC', 'T', 'O', 'Dev','Val', 'TAR', 'Model'))
  colnames(valTable) <- editColnames(cnames = colnames(valTable), 
                                     edits = c('AUC','AUPRC', 'T', 'O','Val', 'TAR', 'Model'))
  
  valTable$timeStamp <- 0
  valTable$Analysis <- valTable$analysisId
  ParallelLogger::logInfo("got db summary")
  #return(valTable[,c('Dev', 'Val', 'T','O', 'Model','Covariate setting',
  return(valTable[,c('Val', 'T','O', 'Model','Covariate setting',
                     'TAR', 'AUC', 'AUPRC', 'calibrationInLarge',
                     'T Size', 'O Count','Val (%)', 'O Incidence (%)', 'timeStamp', 'analysisId', 'researcherId', 'resultId', 'Analysis')])
  
}

getResult <- function(con, tableName, resultId, mySchema){
  sql <- "SELECT * FROM @my_schema.@table_name WHERE result_id = @result_id"
  sql <- SqlRender::render(sql = sql, 
                           
                           my_schema = mySchema,
                           table_name = tableName,
                           result_id = resultId)
  
  result <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
  colnames(result) <- SqlRender::snakeCaseToCamelCase(colnames(result))
  return(result)
}

#' Extract a plpResult from the predictionLibrary database
#'
#' @details
#' Load the main results from database into a runPlp object
#'
#' @param chosenRow  The row from the summaryTable of the selected result
#' 
#' @export
  
loadPlpFromDb <- function(chosenRow, mySchema, con, val = F){
  resultId <- chosenRow$resultId
  modelId <- chosenRow$analysisId
  researcherId <- chosenRow$researcherId
  result <- list()
  result$performanceEvaluation <- list()

  if (!val){
    sql <- "SELECT population_setting_id, model_setting_id, covariate_setting_id FROM @my_schema.models WHERE model_id = @model_id;"
    sql <- SqlRender::render(sql = sql, 
                             my_schema = mySchema,
                             model_id = modelId)
    ParallelLogger::logInfo("starting population")

    ids <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
    colnames(ids) <- SqlRender::snakeCaseToCamelCase(colnames(ids))
    
    ParallelLogger::logInfo("finishing population")
    
    modSetId <- ids$populationSettingId
    covSetId <- ids$modelSettingId
    popSetId <- ids$covariateSettingId
   
    #covariateSummary 
    #made this null to speed up programme
    result$covariateSummary <- NULL
   
    #inputSetting
    result$inputSetting <- list()
    
    sql <- "SELECT * FROM @my_schema.model_settings WHERE model_setting_id = @model_setting_id"
    sql <- SqlRender::render(sql = sql, 
                             my_schema = mySchema,
                             model_setting_id = modSetId)
    ParallelLogger::logInfo("start modeSet")
    tempModSettings <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql)
    ParallelLogger::logInfo("end modeSet")
    
    result$inputSetting$modelSettings <- RJSONIO::fromJSON(tempModSettings$model_settings_json)
    
    sql <- "SELECT * FROM @my_schema.covariate_settings WHERE covariate_setting_id = @covariate_setting_id"
    sql <- SqlRender::render(sql = sql, 
                             my_schema = mySchema,
                             covariate_setting_id = covSetId)
    ParallelLogger::logInfo("start covSet")
    tempCovSettings <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql)
    ParallelLogger::logInfo("end covSet")
    result$inputSetting$dataExtrractionSettings$covariateSettings <- RJSONIO::fromJSON(tempCovSettings$covariate_settings_json)
    
    if(!is.null(result$inputSetting$dataExtrractionSettings$covariateSettings$endDays)){
      class(result$inputSetting$dataExtrractionSettings$covariateSettings) <- 'covariateSettings'
    }
    
    
    sql <- "SELECT * FROM @my_schema.population_settings WHERE population_setting_id = @population_setting_id"
    ParallelLogger::logInfo("start popSet")
    sql <- SqlRender::render(sql = sql, 
                             my_schema = mySchema,
                             population_setting_id = popSetId)
    tempPopSettings<- DatabaseConnector::dbGetQuery(conn =  con, statement = sql)
    ParallelLogger::logInfo("end popSet")
    result$inputSetting$populationSettings <- RJSONIO::fromJSON(tempPopSettings$population_settings_json)
    
    result$inputSetting$populationSettings$attrition <- do.call(cbind, result$inputSetting$populationSettings$attrition)
    
    result$performanceEvaluation$demographicSummary <- getResult(con, 'demographic_summary', resultId,mySchema)
    result$performanceEvaluation$demographicSummary$eval <- trimws(result$performanceEvaluation$demographicSummary$eval)
    result$performanceEvaluation$demographicSummary$ageGroup <- trimws(result$performanceEvaluation$demographicSummary$ageGroup)
    result$performanceEvaluation$demographicSummary$genGroup <- trimws(result$performanceEvaluation$demographicSummary$genGroup)
    colnames(result$performanceEvaluation$demographicSummary) <- editColnames(colnames(result$performanceEvaluation$demographicSummary), c('Eval',"PersonCountAtRisk","PersonCountWithOutcome", "StDevPredictedProbability",
                                                                                                                                           "MinPredictedProbability", "P25PredictedProbability", "P50PredictedProbability",
                                                                                                                                           "P75PredictedProbability", "MaxPredictedProbability"))
    result$performanceEvaluation$predictionDistribution <- getResult(con, 'prediction_distribution', resultId,mySchema) 
    result$performanceEvaluation$predictionDistribution$eval <- trimws(result$performanceEvaluation$predictionDistribution$eval)
    result$performanceEvaluation$predictionDistribution$class <- result$performanceEvaluation$predictionDistribution$classLabel
    colnames(result$performanceEvaluation$predictionDistribution) <- editColnames(colnames(result$performanceEvaluation$predictionDistribution), c('Eval', "PersonCount", "StDevPredictedProbability", 
                                                                                                                                                   "MinPredictedProbability", "P05PredictedProbability" , 
                                                                                                                                                   "P25PredictedProbability", "MedianPredictedProbability", 
                                                                                                                                                   "P75PredictedProbability" , "P95PredictedProbability","MaxPredictedProbability"))
    
    result$model$modelSettings <- result$inputSetting$modelSettings
    result$model$populationSettings <- result$inputSetting$populationSettings
    result$model$metaData$call$covariateSettings <- result$inputSetting$dataExtrractionSettings$covariateSettings
  
  }
  
  #performanceEvaluation
  result$performanceEvaluation$evaluationStatistics <- getResult(con, 'evaluation_statistics', resultId, mySchema)
  colnames(result$performanceEvaluation$evaluationStatistics) <- editColnames(colnames(result$performanceEvaluation$evaluationStatistics), c('Eval', 'Metric',"Value" ))
  
  result$performanceEvaluation$thresholdSummary <- getResult(con, 'threshold_summary', resultId,mySchema)
  result$performanceEvaluation$thresholdSummary$eval <- trimws(result$performanceEvaluation$thresholdSummary$eval)
  colnames(result$performanceEvaluation$thresholdSummary) <- editColnames(colnames(result$performanceEvaluation$thresholdSummary), c('Eval'))
  
  result$performanceEvaluation$calibrationSummary <- getResult(con, 'calibration_summary', resultId,mySchema)
  result$performanceEvaluation$calibrationSummary$eval <- trimws(result$performanceEvaluation$calibrationSummary$eval)
  colnames(result$performanceEvaluation$calibrationSummary) <- editColnames(colnames(result$performanceEvaluation$calibrationSummary), c('Eval',"PersonCountAtRisk","PersonCountWithOutcome","StDevPredictedProbability",
                                                                                                                                         "MinPredictedProbability", "P25PredictedProbability", "MedianPredictedProbability",
                                                                                                                                         "P75PredictedProbability", "MaxPredictedProbability"))
  

  sql <- "SELECT researcher_name, researcher_email FROM @my_schema.researchers WHERE researcher_id = @researcher_id"
  ParallelLogger::logInfo("start researchers")
  sql <- SqlRender::render(sql = sql, 
                           my_schema = mySchema,
                           researcher_id = researcherId)   
  
  result$researcherInfo <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
  colnames(result$researcherInfo) <- SqlRender::snakeCaseToCamelCase(colnames(result$researcherInfo))
  
  
  ParallelLogger::logInfo("end researchers")
  result$model_id <- modelId
  
  # add intercept
  ParallelLogger::logInfo("start intercept")
  sql <- "SELECT intercept FROM @my_schema.models WHERE model_id = @model_id"
  sql <- SqlRender::render(sql = sql, 
                           my_schema = mySchema,
                           model_id = modelId)
  coefficient <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
  result$model$model <- list(coefficient = coefficient$intercept[1])
  
  #hack so the check in plot,utlipl.. doesnt break it
  result$analysisRef <- ""
  result$executionSummary <- ""
  class(result) <- "runPlp"
  ParallelLogger::logInfo("end")
  return(result)
}



#' Extract a covariateSummary from the predictionLibrary database
#'
#' @details
#' Load the covariateSummary from database into a runPlp object
#'
#' @param chosenRow  The row from the summaryTable of the selected result
#' 
#' @export

loadCovSumFromDb <- function(chosenRow, mySchema, con){
  ParallelLogger::logInfo("starting covsum")
  resultId <- chosenRow$resultId
  sql <- "SELECT * FROM @my_schema.covariate_summary WHERE result_id = @result_id;" 
  
  sql <- SqlRender::render(sql = sql,
                           my_schema = mySchema,
                           result_id = resultId)

  covariateSummary <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
  print(colnames(covariateSummary))
  colnames(covariateSummary) <- SqlRender::snakeCaseToCamelCase(colnames(covariateSummary))
  
  
  colnames(covariateSummary) <- editColnames(colnames(covariateSummary), c('CovariateCount', "CovariateMean", "CovariateStDev",
                                                                           "CovariateCountWithNoOutcome","CovariateMeanWithNoOutcome",
                                                                           "CovariateStDevWithNoOutcome" ,     "CovariateCountWithOutcome" ,
                                                                           "CovariateMeanWithOutcome" ,        "CovariateStDevWithOutcome",
                                                                           "StandardizedMeanDiff" ,            "TestCovariateCountWithNoOutcome",
                                                                           "TestCovariateMeanWithNoOutcome",   "TestCovariateStDevWithNoOutcome",
                                                                           "TrainCovariateCountWithNoOutcome", "TrainCovariateMeanWithNoOutcome",
                                                                           "TrainCovariateStDevWithNoOutcome", "TestCovariateCountWithOutcome" ,
                                                                           "TestCovariateMeanWithOutcome" ,    "TestCovariateStDevWithOutcome" ,
                                                                           "TrainCovariateCountWithOutcome",   "TrainCovariateMeanWithOutcome" ,
                                                                           "TrainCovariateStDevWithOutcome" ))
  
  ParallelLogger::logInfo("finishing covsum")
  return(covariateSummary)
}




