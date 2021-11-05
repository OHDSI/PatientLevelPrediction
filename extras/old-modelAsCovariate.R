#' Convert logistic regression model to sql code...
#'
#' @details
#' This function is used to create custom covariates for a logistic regression model
#' (currently only supports, demographics/conditions/drug/procedures/observations and measurement concepts)
#'
#' @param models          A trianed plp model.
#' @param modelNames      A name used in the covariate function names (no spaces)
#' @param covariateConstructionName     the name used for the create covariate function
#' @param modelTable     The temporary table name storing the model details
#' @param analysisId    The covariate analysis_id
#' @param e              The environment to output the covariate setting functions to
#' @param databaseOutput  If you want to output to go inot a cohort table add the "database.schema.tablename" here

#' @export
#'
createLrSql <- function(models, modelNames, covariateConstructionName='prediction', 
                        modelTable='#model_table',
                        analysisId=111, e=environment(),
                        databaseOutput=NULL){
  
  # test model inputs
  #====================================
  if(!exists("models"))
    stop('Need to input models')
  if(!exists("modelNames"))
    stop('Need to input model names')
  if(class(models)!='plpModel'){
    if(length(models)!=length(modelNames))
      stop('model names not same length as models')
  }
  if(class(models)!='list' & class(models)!='plpModel')
    stop('wrong models class')
  if(class(modelNames)!='character')
    stop('wrong model names class')
  #====================================
  
  # create the settings function
  #====================================
  createCovs <- function() {
    covariateSettings <- list(modelTable=modelTable, modelNames=modelNames,
                              covariateConstructionName = covariateConstructionName,
                              analysisId=analysisId, databaseOutput=databaseOutput)
    attr(covariateSettings, "fun") <- paste0('get',covariateConstructionName,'CovariateSettings')
    class(covariateSettings) <- "covariateSettings"
    return(covariateSettings)
  }
  
  
  getCovs <- function(connection,
                      oracleTempSchema = NULL,
                      cdmDatabaseSchema,
                      cohortTable = "#cohort_person",
                      cohortId = -1,
                      cdmVersion = "5",
                      rowIdField = "subject_id",
                      covariateSettings,
                      aggregated = FALSE){
    
    if (aggregated)
      stop("Aggregation not supported")
    
    if(!is.null(covariateSettings$modelTable)){
      modelTable = covariateSettings$modelTable} else{
        modelTable='#model_table'
      }
    
    if(is.null(covariateSettings$analysisId)){
      analysisId <- 111
    }
    
    # test the covariates use the same time settings so we can do call at the same time
    # use model$metaData$call$covariateSettings to check equality
    #if(class(models)!='plpModel'){
    #  if(sum(unlist(lapply(models, function(x) sameCovs(x, models[[1]]))))!=length(models))
    #    stop('Covariates are inconsistent between models')
    #}
    
    
    # =================================
    #==================================
    #  INSERT THE INTERCEPTS FOR THE MODELS: MODEL_ID, INTERCEPT
    if(class(models)=='plpModel'){
      intercepts <- data.frame(modelId=1,
                               intercept= models$model$coefficients[names(models$model$coefficients)=="(Intercept)"])
      
    } else{
      intercepts <- cbind(modelId=1:length(models),
                          intercept= unlist(lapply(models,
                                                   function(x) x$model$coefficients[names(x$model$coefficients)=="(Intercept)"]))
      )}
    
    colnames(intercepts) <- SqlRender::camelCaseToSnakeCase(colnames(intercepts))
    DatabaseConnector::insertTable(connection, tableName='#intercepts', data = intercepts, tempTable = T)
    
    
    # =================================
    #==================================
    #  CREATE THE COVARIATE SETTINGS
    
    # create the table with: model_id, covariate_id, covariateValue
    if(class(models)=='plpModel'){
      allVars <- cbind(model_id=rep(1, sum(models$varImp$covariateValue!=0) ),
                       models$varImp[models$varImp$covariateValue!=0,])
      allVars <- merge(allVars, models$metaData$normFactors, by.x="covariateId", by.y="bins", all.x=T)
      allVars$maxs[is.na(allVars$maxs)] <- 1
      allVars$covariateValue <- allVars$covariateValue/allVars$maxs
      
    } else{
      allVars <- c()
      for( i in 1:length(models)){
        vars <- cbind(model_id=rep(i, sum(models[[i]]$varImp$covariateValue!=0) ),
                      models[[i]]$varImp[models[[i]]$varImp$covariateValue!=0,])
        
        # update the vars value based on normalisation ((raw value/norm factor)*coef), revised_coef = coef/norm factor
        vars <- merge(vars, models[[i]]$metaData$normFactors, by.x="covariateId", by.y="bins", all.x=T)
        vars$maxs[is.na(vars$maxs)] <- 1
        vars$covariateValue <- vars$covariateValue/vars$maxs
        
        allVars <- rbind(allVars,vars)
      }
    }
    
    # insert the model table
    colnames(allVars) <- SqlRender::camelCaseToSnakeCase(colnames(allVars))
    DatabaseConnector::insertTable(connection, tableName=modelTable, data = allVars, tempTable = T)
    
    # =================================
    #==================================
    #  RUN GETPLPDATA (BUT WITHOUT REMOVING THE COVARIATE TABLE)
    if(class(models)=='plpModel'){
      covSettings <- models$metaData$call$covariateSettings
    } else{
      covSettings <- models[[1]]$metaData$call$covariateSettings
    }
    #update some of the settings and only include variables in the model
    
    covSettings$includedCovariateIds <-  as.double(unique(allVars$covariate_id))
    
    covariateData <- getPredictionCovariateData(connection = connection,
                                                oracleTempSchema = oracleTempSchema,
                                                cdmDatabaseSchema = cdmDatabaseSchema,
                                                cohortTable = cohortTable,#"#cohort_person",
                                                cdmVersion = cdmVersion,
                                                rowIdField = rowIdField,#"row_id",
                                                covariateSettings = covSettings,
                                                analysisId = covariateSettings$analysisId,
                                                databaseOutput= covariateSettings$databaseOutput)
    
    # clean the model_table... [TODO]
    covariateData$covariateRef <- data.frame(covariateId = 1000*(1:length(covariateSettings$modelNames))+covariateSettings$analysisId,
                                             covariateName = covariateSettings$modelNames,
                                             analysisId = rep(covariateSettings$analysisId,length(covariateSettings$modelNames)),
                                             conceptId = rep(0, length(covariateSettings$modelNames)))
    # Construct analysis reference:
    covariateData$analysisRef <- data.frame(analysisId = covariateSettings$analysisId,
                                            analysisName = "Prediction Models",
                                            domainId = "Demographics",
                                            startDay = 0,
                                            endDay = 0,
                                            isBinary = "N",
                                            missingMeansZero = "N")
    # Construct analysis reference:
    ##covariateData$metaData <- list(sql = covariates$sql, call = match.call())
    covariateData$metaData <- list(call = match.call())
    class(covariateData) <- "covariateData"
    
    return(covariateData)
  }
  
  assign(paste0('create',covariateConstructionName,'CovariateSettings'), createCovs,envir = e)
  assign(paste0('get',covariateConstructionName,'CovariateSettings'), getCovs,envir = e)
  
  return(T)
  
}


getPredictionCovariateData <- function(connection,
                                       oracleTempSchema = NULL,
                                       cdmDatabaseSchema,
                                       cohortTable = "#cohort_person",
                                       cohortId = -1,
                                       cdmVersion = "5",
                                       rowIdField = "subject_id",
                                       covariateSettings,
                                       aggregated = FALSE,
                                       analysisId=111,
                                       databaseOutput=NULL) {
  if (!"covariateSettings" %in% class(covariateSettings)) {
    stop("Covariate settings object not of type covariateSettings")
  }
  if (cdmVersion == "4") {
    stop("Common Data Model version 4 is not supported")
  }
  FeatureExtraction::getDbDefaultCovariateData(connection = connection,
                                               oracleTempSchema = oracleTempSchema,
                                               cdmDatabaseSchema = cdmDatabaseSchema,
                                               cohortTable = cohortTable,
                                               cohortId = cohortId,
                                               rowIdField = rowIdField,
                                               covariateSettings = covariateSettings,
                                               targetCovariateTable = "#cov_temp",
                                               aggregated = aggregated)
  
  # =================================
  #==================================
  #  CALCULATE THE RISK
  writeLines("Calculating the risk scores...")
  
  if(is.null(databaseOutput)){
    # now sum the temp_covariates table t get val and then 1/(1+exp(-val))
    sql <- "select #cov_temp.row_id, #model_table.model_id*1000+@analysis_id as covariate_id,
  1/(1+exp(-1.0*(max(#intercepts.intercept)+sum(#cov_temp.covariate_value*#model_table.covariate_value)))) covariate_value
  from #cov_temp inner join #model_table on #cov_temp.covariate_id=#model_table.covariate_id
  inner join #intercepts on #intercepts.model_id=#model_table.model_id
  group by #cov_temp.row_id,  #model_table.model_id"
    sql <- SqlRender::renderSql(sql,
                                analysis_id = analysisId
    )$sql
    sql <- SqlRender::translateSql(sql, targetDialect ='pdw')$sql
    
    # Retrieve the covariate:
    covariateData <- Andromeda::andromeda()
    DatabaseConnector::querySqlToAndromeda(connection = connection, 
                                           sql = sql, 
                                           andromeda = covariateData, 
                                           andromedaTableName = "covariates",
                                           snakeCaseToCamelCase = TRUE)
  } else {
    # TO ADD TO A COHORT TABLE
    covariates <- NULL
    sql <- " select a.@rowIdField as subject_id, a.cohort_start_date, a.cohort_start_date as cohort_end_date,
  b.covariate_id cohort_definition_id, b.covariate_value as risk
  into @databaseOutput
  from @cohortTable a inner join

  (select #cov_temp.row_id, #model_table.model_id*1000+@analysis_id as covariate_id,
  1/(1+exp(-1.0*(max(#intercepts.intercept)+sum(#cov_temp.covariate_value*#model_table.covariate_value)))) covariate_value
  from #cov_temp inner join #model_table on #cov_temp.covariate_id=#model_table.covariate_id
  inner join #intercepts on #intercepts.model_id=#model_table.model_id
  group by #cov_temp.row_id,  #model_table.model_id) b on a.@rowIdField=b.row_id"
    
    sql <- SqlRender::renderSql(sql, 
                                rowIdField = rowIdField,
                                cohortTable =cohortTable,
                                analysis_id = analysisId,
                                databaseOutput=databaseOutput
    )$sql
    sql <- SqlRender::translateSql(sql, targetDialect ='pdw')$sql
    DatabaseConnector::executeSql(connection, sql)
  }
  
  result <- list(covariateData=covariateData, 
                 sql=sql)
  return(result)
}
