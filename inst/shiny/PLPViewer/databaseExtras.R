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


getDbSummary <- function(con, mySchema, targetDialect, myTableAppend = '' ){
    ParallelLogger::logInfo("gettingDb summary")

    sql <- "SELECT distinct s.study_id, 
     results.result_id, 
     results.model_id as analysis_id, 
     results.researcher_id, 
     d.database_acronym AS Dev, 
     d.database_acronym AS Val,
     targets.cohort_name AS T, outcomes.cohort_name AS O,
       model_settings.model_type AS model, 
       model_designs.covariate_setting_id, 
       tars.tar_start_day, tars.tar_start_anchor, tars.tar_end_day, tars.tar_end_anchor,
       ROUND(aucResult.auc, 3) as auc,
       ROUND(auprcResult.auprc,4) as auprc,
       nResult.population_size, 
       oResult.outcome_count,
       ROUND(nTest.test_size*100.0/nResult.population_size, 1) as eval_percent,
       ROUND(oResult.outcome_count*100.0/nResult.population_size,4) as outcome_percent
       
       FROM @my_schema.@my_table_appendresults AS results INNER JOIN @my_schema.@my_table_appendmodels AS models 
          ON results.model_id = models.model_id and
             results.database_id = models.database_id
             
    inner join @my_schema.@my_table_appendmodel_designs as model_designs
    on model_designs.model_design_id = models.model_design_id and
    results.target_id = model_designs.target_id and 
             results.outcome_id = model_designs.outcome_id and 
             results.tar_id = model_designs.tar_id and
             results.population_setting_id = model_designs.population_setting_id
         
    inner join @my_schema.@my_table_appendmodel_settings as model_settings
    on model_settings.model_setting_id = model_designs.model_setting_id
    
             
        INNER JOIN @my_schema.@my_table_appendstudy_models AS s on models.model_id = s.model_id
       
             
        LEFT JOIN (SELECT cohort_id, cohort_name FROM @my_schema.@my_table_appendcohorts) AS targets ON results.target_id = targets.cohort_id
        LEFT JOIN (SELECT cohort_id, cohort_name FROM @my_schema.@my_table_appendcohorts) AS outcomes ON results.outcome_id = outcomes.cohort_id
        LEFT JOIN @my_schema.@my_table_appenddatabase_details AS d ON results.database_id = d.database_id 
        LEFT JOIN @my_schema.@my_table_appendtars AS tars ON results.tar_id = tars.tar_id
        LEFT JOIN (SELECT result_id, value AS auc FROM @my_schema.@my_table_appendevaluation_statistics where metric = 'AUROC' and evaluation in ('Test','Validation') ) AS aucResult ON results.result_id = aucResult.result_id
        LEFT JOIN (SELECT result_id, value AS auprc FROM @my_schema.@my_table_appendevaluation_statistics where metric = 'AUPRC' and evaluation in ('Test','Validation') ) AS auprcResult ON results.result_id = auprcResult.result_id
        LEFT JOIN (SELECT result_id, sum(value) AS population_size FROM @my_schema.@my_table_appendevaluation_statistics where metric = 'populationSize' and evaluation in ('Test','Train') group by result_id) AS nResult ON results.result_id = nResult.result_id
        LEFT JOIN (SELECT result_id, sum(value) AS outcome_count FROM @my_schema.@my_table_appendevaluation_statistics where metric = 'outcomeCount' and evaluation in ('Test','Train') group by result_id) AS oResult ON results.result_id = oResult.result_id
        LEFT JOIN (SELECT result_id, value AS test_size FROM @my_schema.@my_table_appendevaluation_statistics where metric = 'populationSize' and evaluation = 'Test') AS nTest ON results.result_id = nTest.result_id;"
    
    sql <- SqlRender::render(sql = sql, 
                             my_schema = mySchema,
                             my_table_append = myTableAppend)
    
    sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
    
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
                           'T Size', 'O Count','Val (%)', 'O Incidence (%)', 'timeStamp', 'analysisId', 'researcherId', 'resultId', 'Analysis', 'studyId')])

}


getValSummary <- function(con, mySchema, modelId, targetDialect, myTableAppend = '' ){
  ParallelLogger::logInfo("getting Val summary")
  
  sql <- "SELECT results.result_id, results.model_id as analysis_id, 
                results.researcher_id, 
                                --databases.database_acronym AS Dev, 
                                d.database_acronym AS Val,
                                targets.cohort_name AS T, outcomes.cohort_name AS O,
   model_settings.model_type AS model, 
   model_designs.covariate_setting_id, 
   tars.tar_start_day, tars.tar_start_anchor, tars.tar_end_day, tars.tar_end_anchor,
   ROUND(aucResult.auc, 3) as auc,
   ROUND(auprcResult.auprc,4) as auprc,
   nResult.population_size, 
   oResult.outcome_count,
   ROUND(nTest.test_size*100.0/nResult.population_size, 1) as eval_percent,
   ROUND(oResult.outcome_count*100.0/nResult.population_size,4) as outcome_percent,
   ROUND(calibration_in_large, 3) as calibration_in_large
   
   FROM @my_schema.@my_table_appendresults AS results INNER JOIN @my_schema.@my_table_appendmodels AS models
    ON 
         results.model_id = models.model_id AND
         results.model_id = @model_id
         
    inner join @my_schema.@my_table_appendmodel_designs as model_designs
    on model_designs.model_design_id = models.model_design_id and
             results.tar_id = model_designs.tar_id and
         results.population_setting_id = model_designs.population_setting_id
         
    inner join @my_schema.@my_table_appendmodel_settings as model_settings
    on model_settings.model_setting_id = model_designs.model_setting_id
    
    LEFT JOIN (SELECT cohort_id, cohort_name FROM @my_schema.@my_table_appendcohorts) AS targets ON results.target_id = targets.cohort_id
    LEFT JOIN (SELECT cohort_id, cohort_name FROM @my_schema.@my_table_appendcohorts) AS outcomes ON results.outcome_id = outcomes.cohort_id
    LEFT JOIN @my_schema.@my_table_appenddatabase_details AS d ON results.database_id = d.database_id 
    LEFT JOIN @my_schema.@my_table_appendtars AS tars ON results.tar_id = tars.tar_id
    LEFT JOIN (SELECT result_id, value AS auc FROM @my_schema.@my_table_appendevaluation_statistics where metric = 'AUROC' and evaluation in ('Test','Validation') ) AS aucResult ON results.result_id = aucResult.result_id
    LEFT JOIN (SELECT result_id, value AS auprc FROM @my_schema.@my_table_appendevaluation_statistics where metric = 'AUPRC' and evaluation in ('Test','Validation') ) AS auprcResult ON results.result_id = auprcResult.result_id
    
    LEFT JOIN (SELECT result_id, value AS calibration_in_large FROM @my_schema.@my_table_appendevaluation_statistics where metric = 'calibrationInLarge intercept' and evaluation in ('Test','Validation') ) AS CalibrationInLargeResult ON results.result_id = CalibrationInLargeResult.result_id

    LEFT JOIN (SELECT result_id, sum(value) AS population_size FROM @my_schema.@my_table_appendevaluation_statistics where metric = 'populationSize' and evaluation in ('Test','Train','Validation') group by result_id) AS nResult ON results.result_id = nResult.result_id
    LEFT JOIN (SELECT result_id, sum(value) AS outcome_count FROM @my_schema.@my_table_appendevaluation_statistics where metric = 'outcomeCount' and evaluation in ('Test','Train','Validation') group by result_id) AS oResult ON results.result_id = oResult.result_id
    LEFT JOIN (SELECT result_id, value AS test_size FROM @my_schema.@my_table_appendevaluation_statistics where metric = 'populationSize' and evaluation in ('Test','Validation')) AS nTest ON results.result_id = nTest.result_id;"
  
  sql <- SqlRender::render(sql = sql, 
                           my_schema = mySchema, 
                           model_id = modelId,
                           my_table_append = myTableAppend)

  sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
  
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

getResult <- function(con, tableName, resultId, mySchema, targetDialect){
  sql <- "SELECT * FROM @my_schema.@table_name WHERE result_id = @result_id"
  sql <- SqlRender::render(sql = sql, 
                           my_schema = mySchema,
                           table_name = tableName,
                           result_id = resultId)
  sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
  
  result <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
  colnames(result) <- SqlRender::snakeCaseToCamelCase(colnames(result))
  return(result)
}


loadPlpFromDb <- function(chosenRow, mySchema, con, val = F, targetDialect, myTableAppend = ''){
  resultId <- chosenRow$resultId
  modelId <- chosenRow$analysisId
  researcherId <- chosenRow$researcherId
  result <- list()
  result$performanceEvaluation <- list()

  if (!val){
    print(paste0('model: ', modelId))
    ## get hyper_param_search plpResult$model$hyperParamSearch <- ...
    #old sql <- "SELECT population_setting_id, model_setting_id, covariate_setting_id, hyper_param_search FROM @my_schema.@my_table_appendmodels AS models WHERE model_id = @model_id;"
    sql <- "SELECT population_setting_id, model_setting_id, covariate_setting_id, hyper_param_search FROM 
    @my_schema.@my_table_appendmodel_designs md inner join 
    @my_schema.@my_table_appendmodels AS models 
    on models.model_design_id = md.model_design_id
    WHERE models.model_id = @model_id;"
    
    sql <- SqlRender::render(sql = sql, 
                             my_schema = mySchema,
                             model_id = modelId,
                             my_table_append = myTableAppend)
    sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
    ParallelLogger::logInfo("starting population, model setting and covariate setting")

    ids <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
    colnames(ids) <- SqlRender::snakeCaseToCamelCase(colnames(ids))
    
    ParallelLogger::logInfo("finishing population, model setting and covariate setting")
    
    popSetId <- ids$populationSettingId
    modSetId <- ids$modelSettingId
    covSetId <- ids$covariateSettingId
    
    hyperParamSearch <- jsonlite::unserializeJSON(ids$hyperParamSearch)
   
    #covariateSummary 
    #made this null to speed up programme
    result$covariateSummary <- NULL
   
    #inputSetting
    result$model <- list(settings = list())
    
    sql <- "SELECT * FROM @my_schema.@my_table_appendmodel_settings AS model_setting WHERE model_setting_id = @model_setting_id"
    sql <- SqlRender::render(sql = sql, 
                             my_schema = mySchema,
                             model_setting_id = modSetId,
                             my_table_append = myTableAppend)
    sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
    ParallelLogger::logInfo("start modeSet")

    tempModSettings <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
    colnames(tempModSettings) <- SqlRender::snakeCaseToCamelCase(colnames(tempModSettings))
    ParallelLogger::logInfo("end modeSet")
    
    if(length(tempModSettings$modelSettingsJson)>0){
      result$model$settings$modelSettings <- jsonlite::unserializeJSON(tempModSettings$modelSettingsJson)
    } else{
      result$model$settings$modelSettings <- list('missing', list(param = 'na'))
    }
    
    sql <- "SELECT * FROM @my_schema.@my_table_appendcovariate_settings AS covariate_setting WHERE covariate_setting_id = @covariate_setting_id"
    sql <- SqlRender::render(sql = sql, 
                             my_schema = mySchema,
                             covariate_setting_id = covSetId,
                             my_table_append = myTableAppend
                             )
    sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
    ParallelLogger::logInfo("start covSet")
    tempCovSettings  <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
    colnames(tempCovSettings) <- SqlRender::snakeCaseToCamelCase(colnames(tempCovSettings))
    ParallelLogger::logInfo("end covSet")
    
    if(length(tempCovSettings$covariateSettingsJson)>0){
      # old result$inputSetting$dataExtrractionSettings$covariateSettings <- jsonlite::unserializeJSON(tempCovSettings$covariateSettingsJson)
      result$model$settings$covariateSettings <- jsonlite::fromJSON(tempCovSettings$covariateSettingsJson, simplifyVector = T, simplifyDataFrame = F, simplifyMatrix = T)
      
      extractAttributes <- function(x){
        
        ind <- grep('attr_', names(x))
        
        if(length(ind)>0){
          attributeValues <- x[ind]
          x <- x[-ind]
          names(attributeValues) <- gsub(pattern = 'attr_',replacement = '',x = names(attributeValues))
          attributeValues$names <- names(x)
          attributes(x) <- attributeValues
        }
        
        return(x)
      }
      
      result$model$settings$covariateSettings <- lapply(result$model$settings$covariateSettings, function(x) extractAttributes(x))
  } else{
    result$model$settings$covariateSettings <- list()
  }
    
    
    sql <- "SELECT * FROM @my_schema.@my_table_appendpopulation_settings AS population_settings WHERE population_setting_id = @population_setting_id"
    ParallelLogger::logInfo("start popSet")
    sql <- SqlRender::render(sql = sql, 
                             my_schema = mySchema,
                             population_setting_id = popSetId,
                             my_table_append = myTableAppend)
    sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
    
    tempPopSettings  <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
    colnames(tempPopSettings) <- SqlRender::snakeCaseToCamelCase(colnames(tempPopSettings))
    ParallelLogger::logInfo("end popSet")
    
    if(length(tempPopSettings$populationSettingsJson)>0){
      result$model$settings$populationSettings <- jsonlite::unserializeJSON(tempPopSettings$populationSettingsJson)
    } else{
      result$model$settings$populationSettings <- NULL
    }
    
    # attrition
    sql <- "SELECT * FROM @my_schema.@my_table_appendattrition WHERE result_id = @result_id"
    ParallelLogger::logInfo("start attrition")
    sql <- SqlRender::render(sql = sql, 
      my_schema = mySchema,
      result_id = resultId,
      my_table_append = myTableAppend)
    sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
    
    tempAttrition  <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
    colnames(tempAttrition) <- SqlRender::snakeCaseToCamelCase(colnames(tempAttrition))
    ParallelLogger::logInfo("end popSet")
    
    result$model$trainDetails$attrition <- as.data.frame(tempAttrition)
    
    result$performanceEvaluation$demographicSummary <- getResult(con, paste0(myTableAppend,'demographic_summary'), resultId, mySchema, targetDialect = targetDialect)
    result$performanceEvaluation$demographicSummary$evaluation <- trimws(result$performanceEvaluation$demographicSummary$evaluation)
    result$performanceEvaluation$demographicSummary$ageGroup <- trimws(result$performanceEvaluation$demographicSummary$ageGroup)
    result$performanceEvaluation$demographicSummary$genGroup <- trimws(result$performanceEvaluation$demographicSummary$genGroup)
    colnames(result$performanceEvaluation$demographicSummary) <- editColnames(colnames(result$performanceEvaluation$demographicSummary), c('evaluation',"PersonCountAtRisk","PersonCountWithOutcome", "StDevPredictedProbability",
                                                                                                                                           "MinPredictedProbability", "P25PredictedProbability", "P50PredictedProbability",
                                                                                                                                           "P75PredictedProbability", "MaxPredictedProbability"))
    result$performanceEvaluation$predictionDistribution <- getResult(con, paste0(myTableAppend,'prediction_distribution'), resultId,mySchema, targetDialect = targetDialect ) 
    result$performanceEvaluation$predictionDistribution$evaluation <- trimws(result$performanceEvaluation$predictionDistribution$evaluation)
    result$performanceEvaluation$predictionDistribution$class <- result$performanceEvaluation$predictionDistribution$classLabel
    colnames(result$performanceEvaluation$predictionDistribution) <- editColnames(colnames(result$performanceEvaluation$predictionDistribution), c('evaluation', "PersonCount", "StDevPredictedProbability", 
                                                                                                                                                   "MinPredictedProbability", "P05PredictedProbability" , 
                                                                                                                                                   "P25PredictedProbability", "MedianPredictedProbability", 
                                                                                                                                                   "P75PredictedProbability" , "P95PredictedProbability","MaxPredictedProbability"))
    
    
    result$model$trainDetails$hyperParamSearch <- hyperParamSearch
  
  }
  
  #performanceEvaluation
  result$performanceEvaluation$evaluationStatistics <- getResult(con, paste0(myTableAppend,'evaluation_statistics'), resultId, mySchema, targetDialect = targetDialect )
  
  result$performanceEvaluation$thresholdSummary <- getResult(con, paste0(myTableAppend,'threshold_summary'), resultId,mySchema, targetDialect = targetDialect)
  result$performanceEvaluation$thresholdSummary$evaluation <- trimws(result$performanceEvaluation$thresholdSummary$evaluation)
  
  result$performanceEvaluation$calibrationSummary <- getResult(con, paste0(myTableAppend,'calibration_summary'), resultId,mySchema, targetDialect = targetDialect)
  result$performanceEvaluation$calibrationSummary$evaluation <- trimws(result$performanceEvaluation$calibrationSummary$evaluation)
  colnames(result$performanceEvaluation$calibrationSummary) <- editColnames(colnames(result$performanceEvaluation$calibrationSummary), c('evaluation',"PersonCountAtRisk","PersonCountWithOutcome","StDevPredictedProbability",
                                                                                                                                         "MinPredictedProbability", "P25PredictedProbability", "MedianPredictedProbability",
                                                                                                                                         "P75PredictedProbability", "MaxPredictedProbability"))
  

  sql <- "SELECT researcher_name, researcher_email FROM @my_schema.@my_table_appendresearchers AS researchers WHERE researcher_id = @researcher_id"
  ParallelLogger::logInfo("start researchers")
  sql <- SqlRender::render(sql = sql, 
                           my_schema = mySchema,
                           researcher_id = researcherId,
                           my_table_append = myTableAppend) 
  sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
  
  result$researcherInfo <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
  colnames(result$researcherInfo) <- SqlRender::snakeCaseToCamelCase(colnames(result$researcherInfo))
  
  ParallelLogger::logInfo("end researchers")
  result$model_id <- modelId
  
  # add intercept
  ParallelLogger::logInfo("start intercept")
  sql <- "SELECT intercept FROM @my_schema.@my_table_appendmodels AS models WHERE model_id = @model_id"
  sql <- SqlRender::render(sql = sql, 
                           my_schema = mySchema,
                           model_id = modelId,
                           my_table_append = myTableAppend)
  sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
  
  coefficient <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
  colnames(coefficient) <- SqlRender::snakeCaseToCamelCase(colnames(coefficient))
  
  result$model$model <- list(coefficient = coefficient$intercept[1])
  
  #hack so the check in plot,utlipl.. doesnt break it
  result$analysisRef <- ""
  result$executionSummary <- ""
  class(result) <- "runPlp"
  ParallelLogger::logInfo("end")
  return(result)
}








