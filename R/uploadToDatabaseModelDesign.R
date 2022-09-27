#' Insert a model design into a PLP result schema database
#' @description
#' This function inserts a model design and all the settings into the result schema
#'
#' @details
#' This function can be used to upload a model design into a database
#'
#' @param object                       An object of class modelDesign, runPlp or externalValidatePlp
#' @param conn                         A connection to a database created by using the
#'                                     function \code{connect} in the
#'                                     \code{DatabaseConnector} package.
#' @param databaseSchemaSettings       A object created by \code{createDatabaseSchemaSettings} with all the settings specifying the result tables                              
#' @param cohortDefinitions            A set of one or more cohorts extracted using ROhdsiWebApi::exportCohortDefinitionSet()
#'    
#' @return
#' Returns NULL but uploads the model design into the database schema specified in databaseSchemaSettings
#' 
#' @export
insertModelDesignInDatabase <- function(
  object,
  conn,
  databaseSchemaSettings,
  cohortDefinitions
){
  
  # REMOVE THIS
  if(inherits(object, 'externalValidatePlp') | inherits(object, 'runPlp')){
    
    object <- PatientLevelPrediction::createModelDesign(
      targetId = object$model$modelDesign$targetId,
      outcomeId = object$model$modelDesign$outcomeId,
      restrictPlpDataSettings = object$model$modelDesign$restrictPlpDataSettings,
      populationSettings = object$model$modelDesign$populationSettings,
      covariateSettings = object$model$modelDesign$covariateSettings,
      featureEngineeringSettings = object$model$modelDesign$featureEngineeringSettings,
      sampleSettings = object$model$modelDesign$sampleSettings,
      preprocessSettings = object$model$modelDesign$preprocessSettings,
      modelSettings = object$model$modelDesign$modelSettings,
      runCovariateSummary = T
    )
    
  }
  
  if(inherits(object, 'modelDesign')){
    modelDesignId <- insertModelDesignSettings(
      object = object,
      conn = conn,
      databaseSchemaSettings = databaseSchemaSettings,
      cohortDefinitions = cohortDefinitions
    )
    return(modelDesignId)
  }
  
  return(NULL)
}


# this function inserts all the settings for the model design
# it returns the model_design_id for the database
insertModelDesignSettings <- function(
  object,
  conn,
  databaseSchemaSettings,
  cohortDefinitions
){
  
  if(!inherits(x = object, what = 'modelDesign')){
    stop('object in insertModelDesign() is not a modelDesign')
  }
  

  # add TAR
  tarId <- addTar(
    conn = conn, 
    resultSchema = databaseSchemaSettings$resultSchema, 
    targetDialect = databaseSchemaSettings$targetDialect,
    startDay = object$populationSettings$riskWindowStart, 
    startAnchor = object$populationSettings$startAnchor,
    endDay = object$populationSettings$riskWindowEnd,  
    endAnchor = object$populationSettings$endAnchor, 
    tablePrefix = databaseSchemaSettings$tablePrefix,
    tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
  )
  ParallelLogger::logInfo(paste0('tarId: ', tarId))
  
  tId <- addCohort(
    conn = conn, 
    resultSchema = databaseSchemaSettings$cohortDefinitionSchema, 
    targetDialect = databaseSchemaSettings$targetDialect,
    cohortDefinition = getCohortDef(cohortDefinitions, object$targetId),
    tablePrefix = databaseSchemaSettings$tablePrefixCohortDefinitionTables,
    tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
  )
  ParallelLogger::logInfo(paste0('tId: ', tId))
  
  oId <- addCohort(
    conn = conn, 
    resultSchema = databaseSchemaSettings$cohortDefinitionSchema, 
    targetDialect = databaseSchemaSettings$targetDialect,
    cohortDefinition = getCohortDef(cohortDefinitions, object$outcomeId),
    tablePrefix = databaseSchemaSettings$tablePrefixCohortDefinitionTables,
    tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
  )
  ParallelLogger::logInfo(paste0('oId: ', oId))
  
  popSetId <- addPopulationSetting(
    conn = conn, 
    resultSchema = databaseSchemaSettings$resultSchema, 
    targetDialect = databaseSchemaSettings$targetDialect,
    json = object$populationSettings, 
    tablePrefix = databaseSchemaSettings$tablePrefix,
    tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
  )
  ParallelLogger::logInfo(paste0('popSetId: ', popSetId))
  
  covSetId <- addCovariateSetting(
    conn = conn, 
    resultSchema = databaseSchemaSettings$resultSchema, 
    targetDialect = databaseSchemaSettings$targetDialect,
    json = object$covariateSettings, 
    tablePrefix = databaseSchemaSettings$tablePrefix,
    tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
  )
  ParallelLogger::logInfo(paste0('covSetId: ', covSetId))
  
  modSetId <- addModelSetting(
    conn = conn, 
    resultSchema = databaseSchemaSettings$resultSchema, 
    targetDialect = databaseSchemaSettings$targetDialect,
    modelType = attr(object$modelSettings$param, 'settings')$modelType, # make this the same as model$trainDetails$modelName?
    json = object$modelSettings, 
    tablePrefix = databaseSchemaSettings$tablePrefix,
    tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
  )
  ParallelLogger::logInfo(paste0('modSetId: ', modSetId))
  
  # NEW: add plp_data_settings
  plpDataSetId <- addPlpDataSetting(
    conn = conn, 
    resultSchema = databaseSchemaSettings$resultSchema, 
    targetDialect = databaseSchemaSettings$targetDialect,
    json = object$restrictPlpDataSettings, 
    tablePrefix = databaseSchemaSettings$tablePrefix,
    tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
  )
  ParallelLogger::logInfo(paste0('plpDataSetId: ', plpDataSetId))
  
  # NEW: add FE_settings
  FESetId <- addFESetting(
    conn = conn, 
    resultSchema = databaseSchemaSettings$resultSchema, 
    targetDialect = databaseSchemaSettings$targetDialect,
    json = object$featureEngineeringSettings, 
    tablePrefix = databaseSchemaSettings$tablePrefix,
    tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
  )
  ParallelLogger::logInfo(paste0('FESetId: ', FESetId))
  
  # NEW: add sample_settings
  sampleSetId <- addSampleSetting(
    conn = conn, 
    resultSchema = databaseSchemaSettings$resultSchema, 
    targetDialect = databaseSchemaSettings$targetDialect,
    json = object$sampleSettings, 
    tablePrefix = databaseSchemaSettings$tablePrefix,
    tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
  )
  ParallelLogger::logInfo(paste0('sampleSetId: ', sampleSetId))
  
  # NEW: add tidy_covariate_settings
  tidySetId <- addTidySetting(
    conn = conn, 
    resultSchema = databaseSchemaSettings$resultSchema, 
    targetDialect = databaseSchemaSettings$targetDialect,
    json = object$preprocessSettings, 
    tablePrefix = databaseSchemaSettings$tablePrefix,
    tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
  )
  ParallelLogger::logInfo(paste0('tidySetId: ', tidySetId))
  
  
  # this is now split setting - update this function
  splitId <- addSplitSettings(
    conn = conn, 
    resultSchema = databaseSchemaSettings$resultSchema, 
    targetDialect = databaseSchemaSettings$targetDialect,
    json = object$splitSettings,
    tablePrefix = databaseSchemaSettings$tablePrefix,
    tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
  )
  ParallelLogger::logInfo(paste0('splitId: ', splitId))
  
  # create this function
  modelDesignId <- addModelDesign( # need to create
    conn = conn, 
    resultSchema = databaseSchemaSettings$resultSchema, 
    targetDialect = databaseSchemaSettings$targetDialect,
    targetId = tId,
    outcomeId = oId,
    tarId = tarId,
    plpDataSettingId = plpDataSetId,
    populationSettingId = popSetId,
    modelSettingId = modSetId,
    covariateSettingId = covSetId,
    sampleSettingId = sampleSetId,
    splitSettingId = splitId, # changed from trainingId
    featureEngineeringSettingId = FESetId,
    tidyCovariatesSettingId = tidySetId,
    tablePrefix = databaseSchemaSettings$tablePrefix,
    tempEmulationSchema = databaseSchemaSettings$tempEmulationSchema
  )
  ParallelLogger::logInfo(paste0('modelDesignId: ', modelDesignId))
  
  return(modelDesignId)
}

addModelDesign <- function(
  conn, 
  resultSchema, targetDialect,
  tablePrefix = tablePrefix,
  targetId,
  outcomeId,
  tarId,
  plpDataSettingId,
  populationSettingId,
  modelSettingId,
  covariateSettingId,
  sampleSettingId,
  splitSettingId,
  featureEngineeringSettingId,
  tidyCovariatesSettingId,
  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
){
  
  if(is.null(targetId)){
    stop('targetId is null')
  }
  if(is.null(outcomeId)){
    stop('outcomeId is null')
  }
  if(is.null(tarId)){
    stop('tarId is null')
  }
  
  if(is.null(plpDataSettingId)){
    stop('plpDataSettingId is null')
  }
  if(is.null(populationSettingId)){
    stop('populationSettingId is null')
  }
  if(is.null(modelSettingId)){
    stop('modelSettingId is null')
  }
  if(is.null(covariateSettingId)){
    stop('covariateSettingId is null')
  }
  if(is.null(sampleSettingId)){
    stop('sampleSettingId is null')
  }
  if(is.null(splitSettingId)){
    stop('splitSettingId is null')
  }
  if(is.null(featureEngineeringSettingId)){
    stop('featureEngineeringSettingId is null')
  }
  if(is.null(tidyCovariatesSettingId)){
    stop('tidyCovariatesSettingId is null')
  }
  
  # process json to make it ordered...
  # TODO
  
  result <- checkTable(
    conn = conn, 
    resultSchema = resultSchema, 
    tablePrefix = tablePrefix,
    targetDialect = targetDialect, 
    tableName = 'model_designs',
    columnNames = c(
      'target_id',
      'outcome_id',
      'tar_id',
      'plp_data_setting_id',
      'population_setting_id',
      'model_setting_id',
      'covariate_setting_id',
      'sample_setting_id',
      'split_setting_id',
      'feature_engineering_setting_id',
      'tidy_covariates_setting_id'
    ), 
    values = c(
      targetId,
      outcomeId,
      tarId,
      plpDataSettingId,
      populationSettingId,
      modelSettingId,
      covariateSettingId,
      sampleSettingId,
      splitSettingId,
      featureEngineeringSettingId,
      tidyCovariatesSettingId
    ),
    tempEmulationSchema = tempEmulationSchema
  )
  
  if(nrow(result)==0){
    # model
    sql <- "INSERT INTO @my_schema.@string_to_appendmodel_designs(
    target_id,
    outcome_id,
    tar_id,
    plp_data_setting_id,
    population_setting_id,
    model_setting_id,
    covariate_setting_id,
    sample_setting_id,
    split_setting_id,
    feature_engineering_setting_id,
    tidy_covariates_setting_id
    ) VALUES 
  ( 
  @target_id, 
  @outcome_id, 
  @tar_id, 
  @plp_data_setting_id,
  @population_setting_id, 
  @model_setting_id, 
  @covariate_setting_id, 
  @sample_setting_id, 
  @split_setting_id, 
  @feature_engineering_setting_id, 
  @tidy_covariates_setting_id
    )"
    sql <- SqlRender::render(
      sql, 
      my_schema = resultSchema,
      target_id = targetId,
      outcome_id = outcomeId,
      tar_id = tarId,
      plp_data_setting_id= plpDataSettingId,
      population_setting_id = populationSettingId,
      model_setting_id = modelSettingId,
      covariate_setting_id = covariateSettingId,
      sample_setting_id = sampleSettingId,
      split_setting_id = splitSettingId,
      feature_engineering_setting_id = featureEngineeringSettingId,
      tidy_covariates_setting_id = tidyCovariatesSettingId,
      string_to_append = tablePrefix
    )
    sql <- SqlRender::translate(sql, targetDialect = targetDialect,
                                tempEmulationSchema = tempEmulationSchema)
    DatabaseConnector::executeSql(conn, sql)
    
    #getId of new
    result <- checkTable(conn = conn, 
                         resultSchema = resultSchema, 
                         tablePrefix = tablePrefix,
                         targetDialect = targetDialect, 
                         tableName = 'model_designs',
                         columnNames = c(
                           'target_id',
                           'outcome_id',
                           'tar_id',
                           'plp_data_setting_id',
                           'population_setting_id',
                           'model_setting_id',
                           'covariate_setting_id',
                           'sample_setting_id',
                           'split_setting_id',
                           'feature_engineering_setting_id',
                           'tidy_covariates_setting_id'
                           ), 
                         values = c(targetId,
                                    outcomeId,
                                    tarId,
                                    plpDataSettingId,
                                    populationSettingId,
                                    modelSettingId,
                                    covariateSettingId,
                                    sampleSettingId,
                                    splitSettingId,
                                    featureEngineeringSettingId,
                                    tidyCovariatesSettingId
                                    ),
                         tempEmulationSchema = tempEmulationSchema
    )
    
  } 
  
  return(result$modelDesignId[1])
}


addTar <- function(conn, resultSchema, targetDialect,
                   tablePrefix = '',
                   startDay, 
                   startAnchor,
                   endDay,  
                   endAnchor,
                   tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
){
  
  result <- checkTable(conn = conn, 
                       resultSchema = resultSchema, 
                       tablePrefix = tablePrefix,
                       targetDialect = targetDialect, 
                       tableName = 'tars',
                       columnNames = c('tar_start_day', 'tar_start_anchor',
                                       'tar_end_day', 'tar_end_anchor'), 
                       values = c(startDay, 
                                  paste0("'",startAnchor,"'"),
                                  endDay,
                                  paste0("'",endAnchor,"'")),
                       tempEmulationSchema = tempEmulationSchema
  )
  
  if(nrow(result)==0){
    
    ParallelLogger::logInfo('Adding TAR')
    # tars - id 1
    sql <- "INSERT INTO @my_schema.@string_to_appendtars(tar_start_day, tar_start_anchor,
                           tar_end_day, tar_end_anchor) 
          VALUES (@tar_start_day, @tar_start_anchor, @tar_end_day, @tar_end_anchor);"
    sql <- SqlRender::render(sql, 
                             my_schema = resultSchema,
                             tar_start_day = startDay,
                             tar_start_anchor = paste0("'",startAnchor,"'"),
                             tar_end_day = endDay,
                             tar_end_anchor = paste0("'",endAnchor,"'"),
                             string_to_append = tablePrefix)
    
    sql <- SqlRender::translate(sql, targetDialect = targetDialect, 
                                tempEmulationSchema = tempEmulationSchema)
    
    DatabaseConnector::executeSql(conn, sql)
    
    #getId of new
    result <- checkTable(conn = conn, 
                         resultSchema = resultSchema, 
                         tablePrefix = tablePrefix,
                         targetDialect = targetDialect, 
                         tableName = 'tars',
                         columnNames = c('tar_start_day', 'tar_start_anchor',
                                         'tar_end_day', 'tar_end_anchor'), 
                         values = c(startDay, 
                                    paste0("'",startAnchor,"'"),
                                    endDay,
                                    paste0("'",endAnchor,"'")),
                         tempEmulationSchema = tempEmulationSchema
    )
    
  } else {
    ParallelLogger::logInfo('TAR exists')
  }
  
  
  return(result$tarId[1])
  
}

addPopulationSetting <- function(conn, resultSchema, targetDialect,
                                 tablePrefix = '',
                                 json,
                                 tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
){
  
  # process json to make it ordered...
  # make sure the json has been converted 
  if(!inherits(x = json, what = 'character')){
      json <- orderJson(json) # to ensure attributes are alphabetic order
      json <- ParallelLogger::convertSettingsToJson(json)
      json <- as.character(json) # now convert to character
  }
  
  jsonId <- checkJson(conn = conn,
                      resultSchema = resultSchema, 
                      tablePrefix = tablePrefix,
                      targetDialect = targetDialect, 
                      tableName = 'population_settings',
                      jsonColumnName = 'populationSettingsJson',
                      id = 'populationSettingId',
                      json = json, 
                      tempEmulationSchema = tempEmulationSchema)
  
  if(is.null(jsonId)){
    ParallelLogger::logInfo('Adding new population settings')
    
    data <- data.frame(populationSettingsJson = json)
    DatabaseConnector::insertTable(connection = conn, 
                                   databaseSchema = resultSchema, 
                                   tableName = paste0(tablePrefix, 'population_settings'),
                                   data = data, 
                                   dropTableIfExists = F, 
                                   createTable = F, 
                                   tempTable = F, 
                                   progressBar = T,
                                   camelCaseToSnakeCase = T,
                                   tempEmulationSchema = tempEmulationSchema
    )
    
    #getId of new
    jsonId <- checkJson(conn = conn,
                        resultSchema = resultSchema, 
                        tablePrefix = tablePrefix,
                        targetDialect = targetDialect, 
                        tableName = 'population_settings',
                        jsonColumnName = 'populationSettingsJson',
                        id = 'populationSettingId',
                        json = json,
                        tempEmulationSchema = tempEmulationSchema)
    
  } else{
    ParallelLogger::logInfo('Population settings exists')
  }
  
  return(jsonId)
}


addCovariateSetting <- function(conn, resultSchema, targetDialect,
                                tablePrefix = '',
                                json,
                                tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
){
  
  # process json to make it ordered...
  # make sure the json has been converted 
  if(!inherits(x = json, what = 'character')){
    json <- orderJson(json) # to ensure attributes are alphabetic order
    json <- ParallelLogger::convertSettingsToJson(json)
    json <- as.character(json) # now convert to character
  }
  
  jsonId <- checkJson(
    conn = conn,
    resultSchema = resultSchema, 
    tablePrefix = tablePrefix,
    targetDialect = targetDialect, 
    tableName = 'covariate_settings',
    jsonColumnName = 'covariateSettingsJson',
    id = 'covariateSettingId',
    json = json,
    tempEmulationSchema = tempEmulationSchema
  )
  
  if(is.null(jsonId)){
    
    ParallelLogger::logInfo('Adding new covariate settings')
    
    data <- data.frame(covariateSettingsJson = json)
    DatabaseConnector::insertTable(connection = conn, 
                                   databaseSchema = resultSchema, 
                                   tableName = paste0(tablePrefix, 'covariate_settings'),
                                   data = data, 
                                   dropTableIfExists = F, 
                                   createTable = F, 
                                   tempTable = F, 
                                   progressBar = T,
                                   camelCaseToSnakeCase = T,
                                   tempEmulationSchema = tempEmulationSchema
    )
    
    #getId of new
    jsonId <- checkJson(conn = conn,
                        resultSchema = resultSchema, 
                        tablePrefix = tablePrefix,
                        targetDialect = targetDialect, 
                        tableName = 'covariate_settings',
                        jsonColumnName = 'covariateSettingsJson',
                        id = 'covariateSettingId',
                        json = json,
                        tempEmulationSchema = tempEmulationSchema)
    
  } else{
    ParallelLogger::logInfo('Covariate setting exists')
  }
  
  return(jsonId)
}


addModelSetting <- function(conn, resultSchema, targetDialect,
                            tablePrefix = '',
                            modelType, json,
                            tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
){
  
  # process json to make it ordered...
  # make sure the json has been converted 
  if(!inherits(x = json, what = 'character')){
    json <- orderJson(json) # to ensure attributes are alphabetic order
    json <- ParallelLogger::convertSettingsToJson(json)
    json <- as.character(json) # now convert to character
  }
  
  jsonId <- checkJson(conn = conn,
                      resultSchema = resultSchema, 
                      tablePrefix = tablePrefix,
                      targetDialect = targetDialect, 
                      tableName = 'model_settings',
                      jsonColumnName = 'modelSettingsJson',
                      id = 'modelSettingId',
                      json = json,
                      tempEmulationSchema = tempEmulationSchema)
  
  if(is.null(jsonId)){
    
    ParallelLogger::logInfo('Adding new model settings')
    
    data <- data.frame(modelType = modelType,
                       modelSettingsJson = json)
    DatabaseConnector::insertTable(connection = conn, 
                                   databaseSchema = resultSchema, 
                                   tableName = paste0(tablePrefix, 'model_settings'),
                                   data = data, 
                                   dropTableIfExists = F, 
                                   createTable = F, 
                                   tempTable = F, 
                                   progressBar = T,
                                   camelCaseToSnakeCase = T,
                                   tempEmulationSchema = tempEmulationSchema)
    
    #getId of new
    jsonId <- checkJson(conn = conn,
                        resultSchema = resultSchema, 
                        tablePrefix = tablePrefix,
                        targetDialect = targetDialect, 
                        tableName = 'model_settings',
                        jsonColumnName = 'modelSettingsJson',
                        id = 'modelSettingId',
                        json = json,
                        tempEmulationSchema = tempEmulationSchema)
    
  } else{
    ParallelLogger::logInfo('Model setting exists')
  }
  
  return(jsonId)
}

addTidySetting <- function(
  conn, 
  resultSchema, 
  targetDialect,
  tablePrefix = '',
  json,
  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
){
  
  if(!inherits(x = json, what = 'character')){
    json <- orderJson(json) # to ensure attributes are alphabetic order
    json <- ParallelLogger::convertSettingsToJson(json)
    json <- as.character(json) # now convert to character
  }
  
  jsonId <- checkJson(conn = conn,
                      resultSchema = resultSchema, 
                      tablePrefix = tablePrefix,
                      targetDialect = targetDialect, 
                      tableName = 'tidy_covariates_settings',
                      jsonColumnName = 'tidyCovariatesSettingsJson',
                      id = 'tidyCovariatesSettingId',
                      json = json,
                      tempEmulationSchema = tempEmulationSchema)
  
  if(is.null(jsonId)){
    
    ParallelLogger::logInfo('Adding new tidy covariates settings')
    
    data <- data.frame(
      tidyCovariatesSettingsJson = json
    )
    
    DatabaseConnector::insertTable(
      connection = conn, 
      databaseSchema = resultSchema, 
      tableName = paste0(tablePrefix, 'tidy_covariates_settings'),
      data = data, 
      dropTableIfExists = F, 
      createTable = F, 
      tempTable = F, 
      progressBar = T,
      camelCaseToSnakeCase = T,
      tempEmulationSchema = tempEmulationSchema
    )
    
    #getId of new
    jsonId <- checkJson(
      conn = conn,
      resultSchema = resultSchema, 
      tablePrefix = tablePrefix,
      targetDialect = targetDialect, 
      tableName = 'tidy_covariates_settings',
      jsonColumnName = 'tidyCovariatesSettingsJson',
      id = 'tidyCovariatesSettingId',
      json = json,
      tempEmulationSchema = tempEmulationSchema
    )
    
  } else{
    ParallelLogger::logInfo('tidy covariates setting exists')
  }
  
  return(jsonId)
  
}

addSampleSetting <- function(
  conn, 
  resultSchema, 
  targetDialect,
  tablePrefix = '',
  json,
  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
){
  
  if(!inherits(x = json, what = 'character')){
    json <- orderJson(json) # to ensure attributes are alphabetic order
    json <- ParallelLogger::convertSettingsToJson(json)
    json <- as.character(json) # now convert to character
  }
  
  jsonId <- checkJson(
    conn = conn,
    resultSchema = resultSchema, 
    tablePrefix = tablePrefix,
    targetDialect = targetDialect, 
    tableName = 'sample_settings',
    jsonColumnName = 'sampleSettingsJson',
    id = 'sampleSettingId',
    json = json,
    tempEmulationSchema = tempEmulationSchema
  )
  
  if(is.null(jsonId)){
    
    ParallelLogger::logInfo('Adding new sample settings')
    
    data <- data.frame(
      sampleSettingsJson = json
    )
    
    DatabaseConnector::insertTable(
      connection = conn, 
      databaseSchema = resultSchema, 
      tableName = paste0(tablePrefix, 'sample_settings'),
      data = data, 
      dropTableIfExists = F, 
      createTable = F, 
      tempTable = F, 
      progressBar = T,
      camelCaseToSnakeCase = T,
      tempEmulationSchema = tempEmulationSchema
    )
    
    #getId of new
    jsonId <- checkJson(
      conn = conn,
      resultSchema = resultSchema, 
      tablePrefix = tablePrefix,
      targetDialect = targetDialect, 
      tableName = 'sample_settings',
      jsonColumnName = 'sampleSettingsJson',
      id = 'sampleSettingId',
      json = json,
      tempEmulationSchema = tempEmulationSchema
    )
    
  } else{
    ParallelLogger::logInfo('sample setting exists')
  }
  
  return(jsonId)
  
}

addPlpDataSetting <- function(
  conn, 
  resultSchema, 
  targetDialect,
  tablePrefix = '',
  json,
  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
){
  
  if(!inherits(x = json, what = 'character')){
    json <- orderJson(json) # to ensure attributes are alphabetic order
    json <- ParallelLogger::convertSettingsToJson(json)
    json <- as.character(json) # now convert to character
  }
  
  jsonId <- checkJson(conn = conn,
                      resultSchema = resultSchema, 
                      tablePrefix = tablePrefix,
                      targetDialect = targetDialect, 
                      tableName = 'plp_data_settings',
                      jsonColumnName = 'plpDataSettingsJson',
                      id = 'plpDataSettingId',
                      json = json,
                      tempEmulationSchema = tempEmulationSchema)
  
  if(is.null(jsonId)){
    
    ParallelLogger::logInfo('Adding new plp data settings')
    
    data <- data.frame(
      plpDataSettingsJson = json
    )
    
    DatabaseConnector::insertTable(
      connection = conn, 
      databaseSchema = resultSchema, 
      tableName = paste0(tablePrefix, 'plp_data_settings'),
      data = data, 
      dropTableIfExists = F, 
      createTable = F, 
      tempTable = F, 
      progressBar = T,
      camelCaseToSnakeCase = T,
      tempEmulationSchema = tempEmulationSchema
    )
    
    #getId of new
    jsonId <- checkJson(
      conn = conn,
      resultSchema = resultSchema, 
      tablePrefix = tablePrefix,
      targetDialect = targetDialect, 
      tableName = 'plp_data_settings',
      jsonColumnName = 'plpDataSettingsJson',
      id = 'plpDataSettingId',
      json = json,
      tempEmulationSchema = tempEmulationSchema
    )
    
  } else{
    ParallelLogger::logInfo('Split setting exists')
  }
  
  return(jsonId)
  
}

addFESetting <- function(
  conn, 
  resultSchema, 
  targetDialect,
  tablePrefix = '',
  json,
  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
){
  
  if(!inherits(x = json, what = 'character')){
    json <- orderJson(json) # to ensure attributes are alphabetic order
    json <- ParallelLogger::convertSettingsToJson(json)
    json <- as.character(json) # now convert to character
  }
  
  jsonId <- checkJson(
    conn = conn,
    resultSchema = resultSchema, 
    tablePrefix = tablePrefix,
    targetDialect = targetDialect, 
    tableName = 'feature_engineering_settings',
    jsonColumnName = 'featureEngineeringSettingsJson',
    id = 'featureEngineeringSettingId',
    json = json,
    tempEmulationSchema = tempEmulationSchema
  )
  
  if(is.null(jsonId)){
    
    ParallelLogger::logInfo('Adding new feature_engineering settings')
    
    data <- data.frame(
      featureEngineeringSettingsJson = json
    )
    
    DatabaseConnector::insertTable(
      connection = conn, 
      databaseSchema = resultSchema, 
      tableName = paste0(tablePrefix, 'feature_engineering_settings'),
      data = data, 
      dropTableIfExists = F, 
      createTable = F, 
      tempTable = F, 
      progressBar = T,
      camelCaseToSnakeCase = T,
      tempEmulationSchema = tempEmulationSchema
    )
    
    #getId of new
    jsonId <- checkJson(
      conn = conn,
      resultSchema = resultSchema, 
      tablePrefix = tablePrefix,
      targetDialect = targetDialect, 
      tableName = 'feature_engineering_settings',
      jsonColumnName = 'featureEngineeringSettingsJson',
      id = 'featureEngineeringSettingId',
      json = json,
      tempEmulationSchema = tempEmulationSchema
    )
    
  } else{
    ParallelLogger::logInfo('feature engineering setting exists')
  }
  
  return(jsonId)
  
}

addSplitSettings <- function(
  conn, 
  resultSchema, 
  targetDialect,
  tablePrefix = '',
  json,
  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema")
){
  
  if(!inherits(x = json, what = 'character')){
    json <- orderJson(json) # to ensure attributes are alphabetic order
    json <- ParallelLogger::convertSettingsToJson(json)
    json <- as.character(json) # now convert to character
  }
  
  jsonId <- checkJson(
    conn = conn,
    resultSchema = resultSchema, 
    tablePrefix = tablePrefix,
    targetDialect = targetDialect, 
    tableName = 'split_settings',
    jsonColumnName = 'splitSettingsJson',
    id = 'splitSettingId',
    json = json,
    tempEmulationSchema = tempEmulationSchema
  )
  
  if(is.null(jsonId)){
    
    ParallelLogger::logInfo('Adding new split settings')
    
    data <- data.frame(
      splitSettingsJson = json
    )
    
    DatabaseConnector::insertTable(
      connection = conn, 
      databaseSchema = resultSchema, 
      tableName = paste0(tablePrefix, 'split_settings'),
      data = data, 
      dropTableIfExists = F, 
      createTable = F, 
      tempTable = F, 
      progressBar = T,
      camelCaseToSnakeCase = T,
      tempEmulationSchema = tempEmulationSchema
    )
    
    #getId of new
    jsonId <- checkJson(
      conn = conn,
      resultSchema = resultSchema, 
      tablePrefix = tablePrefix,
      targetDialect = targetDialect, 
      tableName = 'split_settings',
      jsonColumnName = 'splitSettingsJson',
      id = 'splitSettingId',
      json = json,
      tempEmulationSchema = tempEmulationSchema
    )
    
  } else{
    ParallelLogger::logInfo('Split setting exists')
  }
  
  return(jsonId)
  
}



# the ParallelLogger conversion orders attributes - use this for consistency
orderJson <- function(x){
newx <- ParallelLogger::convertJsonToSettings(
  ParallelLogger::convertSettingsToJson(
    x
    )
  )

return(newx)
}
