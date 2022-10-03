#' Function to insert results into a database from csvs
#' @description
#' This function converts a folder with csv results into plp objects and loads them into a plp result database
#'
#' @details
#' The user needs to have plp csv results in a single folder and an existing plp result database  
#' 
#' @param csvFolder        The location to the csv folder with the plp results
#' @param conn             A connection to the plp results database that the csv results will be inserted into
#' @param databaseSchemaSettings       A object created by \code{createDatabaseSchemaSettings} with all the settings specifying the result tables to insert the csv results into  
#' @param modelSaveLocation        The location to save any models from the csv folder - this should be the same location you picked when inserting other models into the database
#' @param csvTableAppend       A string that appends the csv file names                       
#' 
#' @return
#' Returns a data.frame indicating whether the results were inported into the database
#' 
#' @export
insertCsvToDatabase <- function(
  csvFolder,
  conn,
  databaseSchemaSettings,
  modelSaveLocation,
  csvTableAppend = ''
){
  
  ensure_installed('readr')
  
  ParallelLogger::logInfo('Starting input checks')
  
  csvFileNames <- tryCatch({
    dir(csvFolder, pattern = 'csv')
  }, 
  error = function(e){ParallelLogger::logInfo(e); return(NULL)}
  )
  if(is.null(csvFileNames)){
    return(invisible(NULL))
  }
  
  if(!missing(csvTableAppend)){ 
    csvFileNamesNoAppend <- sub(csvTableAppend, '', csvFileNames)
  } else{
    csvFileNamesNoAppend <- csvFileNames
    }
  
  # check all tables are in folder
  # settings/resultsDataModelSpecification.csv table_name
  resultNames <- paste0(unique(
    readr::read_csv(
      system.file(
        'settings',
        'resultsDataModelSpecification.csv',
        package = "PatientLevelPrediction"
      )
    )$table_name
  ), '.csv')
  if(sum(csvFileNamesNoAppend %in% resultNames) != length(resultNames)){
    missingTables <- paste(resultNames[!resultNames %in% csvFileNamesNoAppend], collapse = ',')
    ParallelLogger::logInfo(paste0('CSV folder missing these tables: ', missingTables))
    return(invisible(NULL))
  }
  
  # check some plp tables exists in databaseSchemaSettings 
  alltables <- DatabaseConnector::getTableNames(
    connection = conn, 
    databaseSchema = databaseSchemaSettings$resultSchema
  )
  
  if(!paste0(toupper(databaseSchemaSettings$tablePrefix),'PERFORMANCES') %in% alltables){
    ParallelLogger::logInfo(
      paste0(
        'performance table: ',paste0(toupper(databaseSchemaSettings$tablePrefix),'PERFORMANCES'),' not found, result database only contains ', 
        paste(alltables, collapse =  ',')
        )
      )
    return(invisible(NULL))
  }
  
  ParallelLogger::logInfo('Input checks passed')
  ParallelLogger::logInfo('Extracting cohort definitions')
  # create cohortDefinitions:
  cohortDefinitions <- extractCohortDefinitionsCSV(
    csvFolder = csvFolder
  )
    
  ParallelLogger::logInfo('Extracting database details')
  # create databaseList
  databaseList <- extractDatabaseListCSV(
    csvFolder = csvFolder
  )
  
  ParallelLogger::logInfo('Extracting performance ids')
  performanceIds <- readr::read_csv(file.path(csvFolder, csvFileNames[grep('performances', csvFileNames)]))$performance_id
    
  if(length(performanceIds) > 0 ){
    for(performanceId in performanceIds){  
      ParallelLogger::logInfo(
        paste0(
          'Converting and inserting performance id', 
          performanceId
        )
      )
      # convert to runPlp
      runPlp <- extractObjectFromCsv(
        performanceId = performanceId, 
        csvFolder = csvFolder
      )
      
      # load into database
      addRunPlpToDatabase(
        runPlp = runPlp,
        conn = conn,
        databaseSchemaSettings = databaseSchemaSettings,
        cohortDefinitions = cohortDefinitions,
        modelSaveLocation = modelSaveLocation,
        databaseList = databaseList
      )
    }
  }
  
  diagnosticIds <- readr::read_csv(file.path(csvFolder, csvFileNames[grep('diagnostics', csvFileNames)]))$diagnostic_id

  if(length(diagnosticIds) > 0){
    for(diagnosticId in diagnosticIds){  
      ParallelLogger::logInfo(
        paste0(
          'Converting and inserting diagnostic id', 
          diagnosticId
        )
      )
      diagnosePlp <- extractDiagnosticFromCsv(
        diagnosticId = diagnosticId, 
        csvFolder = csvFolder
      )
      if(!is.null(diagnosePlp)){
        tryCatch(
          {
            addDiagnosePlpToDatabase(
              diagnosePlp = diagnosePlp,
              conn = conn,
              databaseSchemaSettings = databaseSchemaSettings,
              cohortDefinitions = cohortDefinitions,
              databaseList = databaseList
            )
          }, error = function(e){ParallelLogger::logError(e)}
        )
      }
      
    }
  }
  
  
  return(TRUE)
  
}





extractCohortDefinitionsCSV <- function(
  csvFolder
){
  
  # cohorts: cohort_id, cohort_definition_id, cohort_name
  # cohort_definition: cohort_definition_id	cohort_name	description	json	sql_command 
  
  cohortDefinitionName <- dir(csvFolder, pattern = 'cohort_definition.csv')
  cohort_definition <- readr::read_csv(file.path(csvFolder, cohortDefinitionName))
  
  result <- data.frame(
    cohortId = cohort_definition$cohort_definition_id,
    cohortName = cohort_definition$cohort_name,
    json = cohort_definition$json,
    sql = cohort_definition$sql_command 
      )
  
  return(result)
}

extractDatabaseListCSV <- function(
  csvFolder
){
  # database_meta_data: database_id	cdm_source_name	cdm_source_abbreviation
  # database_details: database_id	database_meta_data_id
  databaseMetaDataName <- dir(csvFolder, pattern = 'database_meta_data.csv')
  databaseMetaData  <- readr::read_csv(file.path(csvFolder, databaseMetaDataName))

  databaseList <- createDatabaseList(
    cdmDatabaseSchemas = databaseMetaData$cdm_source_name,
    cdmDatabaseNames = databaseMetaData$cdm_source_abbreviation,
    databaseRefIds = databaseMetaData$database_id
  )
  
  return(databaseList)
}


getModelDesignSettingTable <- function(modeldesignsRow){
  result <- data.frame(
    tableName = c('cohorts', 'cohorts',
                  'population_settings', 'plp_data_settings',
                  'model_settings', 'covariate_settings', 'sample_settings', 
                  'split_settings', 'feature_engineering_settings', 
                  'tidy_covariates_settings'),
    idColumn = c('cohort_id', 'cohort_id',
                 'population_setting_id', 'plp_data_setting_id',
                 'model_setting_id', 'covariate_setting_id', 'sample_setting_id', 
                 'split_setting_id', 'feature_engineering_setting_id', 
                 'tidy_covariates_setting_id'),
    jsonColumn = c('cohort_definition_id', 'cohort_definition_id',
                   'population_settings_json', 'plp_data_settings_json',
                   'model_settings_json', 'covariate_settings_json', 'sample_settings_json', 
                   'split_settings_json', 'feature_engineering_settings_json', 
                   'tidy_covariates_settings_json'),
    convertJson = c(rep(F,2), rep(T, 8)),
    value = c(modeldesignsRow$target_id, modeldesignsRow$outcome_id,
              modeldesignsRow$population_setting_id, modeldesignsRow$plp_data_setting_id,
              modeldesignsRow$model_setting_id, modeldesignsRow$covariate_setting_id, modeldesignsRow$sample_setting_id,
              modeldesignsRow$split_setting_id, modeldesignsRow$feature_engineering_setting_id	, 
              modeldesignsRow$tidy_covariates_setting_id),
    modelDesignInput = c('targetId', 'outcomeId', 
                         'populationSettings', 'restrictPlpDataSettings', 
                         'modelSettings', 'covariateSettings', 'sampleSettings', 
                         'splitSettings', 'featureEngineeringSettings',
                         'preprocessSettings')
  )
  return(result)
}

getModelDesignCsv <- function(
  modelDesignSettingTable, 
  csvFolder = csvFolder
) {
  
  csvFileNames <- dir(csvFolder, pattern = '.csv')
  
  result <- list()
  for(i in 1:nrow(modelDesignSettingTable)){
    table <- readr::read_csv(file.path(csvFolder, csvFileNames[grep(modelDesignSettingTable$tableName[i], csvFileNames)]))
    ind <- table[modelDesignSettingTable$idColumn[i]] == modelDesignSettingTable$value[i]
    result[[i]] <- table[ind,][modelDesignSettingTable$jsonColumn[i]]
    if(modelDesignSettingTable$convertJson[i]){
      result[[i]] <- ParallelLogger::convertJsonToSettings(as.character(result[[i]]))
    } else{
      # ids need to be integer
      result[[i]] <- as.double(result[[i]])
    }
  }
  names(result) <- modelDesignSettingTable$modelDesignInput

  modelDesign <- do.call(what = PatientLevelPrediction::createModelDesign, args = result)
  
  return(modelDesign)
}

getPerformanceEvaluationCsv <- function(
  performanceId, 
  csvFolder
){
  
  csvFileNames <- dir(csvFolder, pattern = '.csv')
  
  result <- list(
    
    evaluationStatistics = tryCatch(
      {
      res <- readr::read_csv(file.path(csvFolder, csvFileNames[grep('evaluation_statistics', csvFileNames)])) %>%
        dplyr::filter(.data$performance_id == !!performanceId) %>%
        dplyr::select(-.data$performance_id);
      colnames(res) <- SqlRender::snakeCaseToCamelCase( colnames(res));
      res
    }, 
    error = function(e){ParallelLogger::logInfo(e); return(NULL)}
    ),
    
    thresholdSummary = tryCatch({
      res <- readr::read_csv(file.path(csvFolder, csvFileNames[grep('threshold_summary', csvFileNames)])) %>%
        dplyr::filter(.data$performance_id == !!performanceId) %>%
        dplyr::select(-.data$performance_id);
      colnames(res) <- SqlRender::snakeCaseToCamelCase( colnames(res));
      res
    }, 
    error = function(e){ParallelLogger::logInfo(e); return(NULL)}
    ),
    
    calibrationSummary = tryCatch({
      res <- readr::read_csv(file.path(csvFolder, csvFileNames[grep('calibration_summary', csvFileNames)])) %>%
        dplyr::filter(.data$performance_id == !!performanceId) %>%
        dplyr::select(-.data$performance_id);
      colnames(res) <- SqlRender::snakeCaseToCamelCase( colnames(res));
      res
    }, 
    error = function(e){ParallelLogger::logInfo(e); return(NULL)}
    ),
    
    demographicSummary = tryCatch({
      res <- readr::read_csv(file.path(csvFolder, csvFileNames[grep('demographic_summary', csvFileNames)])) %>%
        dplyr::filter(.data$performance_id == !!performanceId) %>%
        dplyr::select(-.data$performance_id);
      colnames(res) <- SqlRender::snakeCaseToCamelCase( colnames(res));
      res
    }, 
    error = function(e){ParallelLogger::logInfo(e); return(NULL)}
    ),
    
    predictionDistribution = tryCatch({
      res <- readr::read_csv(file.path(csvFolder, csvFileNames[grep('prediction_distribution', csvFileNames)])) %>%
        dplyr::filter(.data$performance_id == !!performanceId) %>%
        dplyr::select(-.data$performance_id);
      colnames(res) <- SqlRender::snakeCaseToCamelCase( colnames(res));
      res
    }, 
    error = function(e){ParallelLogger::logInfo(e); return(NULL)}
    )
  )
  
  return(result)
  
}

extractObjectFromCsv <- function(
  performanceId, 
  csvFolder
){
  
  csvFileNames <- dir(csvFolder, pattern = '.csv')
  
  # get the model design
  # performance_id	model_design_id	development_database_id	validation_database_id	target_id	outcome_id	tar_id	plp_data_setting_id	population_setting_id	model_development	execution_date_time	plp_version
  performances <- readr::read_csv(file.path(csvFolder, csvFileNames[grep('performances', csvFileNames)]))
  poi <- performances[performances$performance_id == performanceId,,]
  
  modelDesignId <- poi$model_design_id
  modeldesigns <- readr::read_csv(file.path(csvFolder, csvFileNames[grep('model_designs', csvFileNames)]))
  # model_design_id	target_id	outcome_id	tar_id	plp_data_setting_id	population_setting_id	model_setting_id	covariate_setting_id	sample_setting_id	split_setting_id	feature_engineering_setting_id	tidy_covariates_setting_id
  modeldesigns <- modeldesigns[modeldesigns$model_design_id == modelDesignId,,]
  
  modelDesignSettingTable <- getModelDesignSettingTable(
    modeldesignsRow = modeldesigns
  )
    
  modelDesign <- getModelDesignCsv(
    modelDesignSettingTable = modelDesignSettingTable, 
    csvFolder = csvFolder
    )
  
  covariateSummary <- readr::read_csv(file.path(csvFolder, csvFileNames[grep('covariate_summary', csvFileNames)])) %>%
    dplyr::filter(.data$performance_id == !!poi$performance_id) %>%
    dplyr::select(-.data$performance_id)
  colnames(covariateSummary) <- SqlRender::snakeCaseToCamelCase(colnames(covariateSummary))
  
  performanceEvaluation <- getPerformanceEvaluationCsv(
    performanceId = poi$performance_id, 
    csvFolder = csvFolder
  )
  
  modelMissing <- F
  if(poi$model_development == 1){

    modelsName <- dir(csvFolder, pattern = 'models.csv')
    models  <- readr::read_csv(file.path(csvFolder, modelsName))
    models <- models %>% 
      dplyr::filter(.data$model_design_id == !!poi$model_design_id ) %>%
      dplyr::filter(.data$database_id == !!poi$development_database_id)
    
    modelLoc <- strsplit(x = models$plp_model_file, split = '/')[[1]][length(strsplit(x = models$plp_model_file, split = '/')[[1]])]
    plpModel <- tryCatch({
      PatientLevelPrediction::loadPlpModel(file.path(csvFolder, 'models', modelLoc))
    }, 
      error = function(e){ParallelLogger::logInfo(e); return(NULL)}
    )
    
    resultClass <- 'runPlp'
  
    if(is.null(modelLoc)){
      ParallelLogger::logInfo('Models missing from csv folder - just adding performance')
      modelMissing <- T
    }
    
  }
  
  if(poi$model_development == 0 | modelMissing){
    
    # database_details: database_id	database_meta_data_id
    databaseMetaDataName <- dir(csvFolder, pattern = 'database_meta_data.csv')
    databaseMetaData  <- readr::read_csv(file.path(csvFolder, databaseMetaDataName))
    databaseDetailsName <- dir(csvFolder, pattern = 'database_details.csv')
    databaseDetails  <- readr::read_csv(file.path(csvFolder, databaseDetailsName))
    databases <- merge(databaseDetails, databaseMetaData, by.x = 'database_meta_data_id', by.y = 'database_id')
    
    dev <- databases[databases$database_id == poi$development_database_id,,]
    val <- databases[databases$database_id == poi$validation_database_id,,]
    
    developmentDatabase <- dev$cdm_source_name
    developmentDatabaseId <- dev$database_meta_data_id
    validationDatabase <- val$cdm_source_name
    validationDatabaseId <- val$database_meta_data_id
    
      attritionName <- dir(csvFolder, pattern = 'attrition.csv')
      attrition  <- readr::read_csv(file.path(csvFolder, attritionName)) %>%
        dplyr::filter(.data$performance_id == !!poi$performance_id) %>%
        dplyr::select(-.data$performance_id)
      colnames(attrition) <- SqlRender::snakeCaseToCamelCase(colnames(attrition))
      
      cohortsName <- dir(csvFolder, pattern = 'cohorts.csv')
      cohorts  <- readr::read_csv(file.path(csvFolder, cohortsName))
      plpDataSetName <- dir(csvFolder, pattern = 'plp_data_settings.csv')
      plpDataSet  <- readr::read_csv(file.path(csvFolder, plpDataSetName))
      popSetName <- dir(csvFolder, pattern = 'population_settings.csv')
      popSet  <- readr::read_csv(file.path(csvFolder, popSetName))
      
      # get the model
      plpModel <- list(
        model = 'external validation of model',
        modelDesign = modelDesign,
        validationDetails = list(
          analysisId = '', 
          analysisSource = '', 
          developmentDatabase = developmentDatabase,
          developmentDatabaseId = developmentDatabaseId,
          validationDatabase = validationDatabase,
          validationDatabaseId = validationDatabaseId,
          
          populationSettings = ParallelLogger::convertJsonToSettings(
            as.character(
              popSet %>% 
                dplyr::filter(.data$population_setting_id == !!poi$population_setting_id) %>%
                dplyr::select(.data$population_settings_json)
            )
          ),
          restrictPlpDataSettings = ParallelLogger::convertJsonToSettings(
            as.character(
              plpDataSet %>% 
              dplyr::filter(.data$plp_data_setting_id == !!poi$plp_data_setting_id) %>%
              dplyr::select(.data$plp_data_settings_json)
            )
          ),
          
          outcomeId = as.double(
            cohorts %>% 
              dplyr::filter(.data$cohort_id == !!poi$outcome_id) %>%
              dplyr::select(.data$cohort_definition_id)
          ),
          targetId = as.double(
            cohorts %>% 
              dplyr::filter(.data$cohort_id == !!poi$target_id) %>%
              dplyr::select(.data$cohort_definition_id)
          ),
          
          attrition = attrition
        )
      )
      attr(plpModel, "predictionFunction") <- 'none'
      attr(plpModel, "saveType") <- 'RtoJson'
      class(plpModel) <- 'plpModel'
    
    resultClass <- 'externalValidatePlp'
  }
  

  result <- list(
    executionSummary = list(
      PackageVersion = list(
        packageVersion = poi$plp_version
      ),
      #TotalExecutionElapsedTime = ,
      ExecutionDateTime = poi$execution_date_time	
    ),
    model = plpModel,
    performanceEvaluation = performanceEvaluation,
    covariateSummary = covariateSummary,
    analysisRef = list(
      analysisId = ''
      )
  )
  class(result) <- resultClass
  
  # return the object
  return(result)
  
}

extractDiagnosticFromCsv <- function(
  diagnosticId, 
  csvFolder
){
  
  # diagnostic_id	model_design_id	database_id	execution_date_time
  csvFileNames <- dir(csvFolder, pattern = '.csv')
  
  # get the model design
  # performance_id	model_design_id	development_database_id	validation_database_id	target_id	outcome_id	tar_id	plp_data_setting_id	population_setting_id	model_development	execution_date_time	plp_version
  diagnostics <- readr::read_csv(file.path(csvFolder, csvFileNames[grep('diagnostics', csvFileNames)]))
  if(length(diagnostics) == 0){
    ParallelLogger::logInfo('No diagnostics in csv results')
    return(NULL)
  }
  doi <- diagnostics[diagnostics$diagnostic_id == diagnosticId,,]
  if(nrow(doi) == 0){
    ParallelLogger::logInfo('No diagnostics in csv results with specified diagnosticId')
    return(NULL)
  }
  
  modelDesignId <- doi$model_design_id
  modeldesigns <- readr::read_csv(file.path(csvFolder, csvFileNames[grep('model_designs', csvFileNames)]))
  # model_design_id	target_id	outcome_id	tar_id	plp_data_setting_id	population_setting_id	model_setting_id	covariate_setting_id	sample_setting_id	split_setting_id	feature_engineering_setting_id	tidy_covariates_setting_id
  modeldesigns <- modeldesigns[modeldesigns$model_design_id == modelDesignId,,]
  
  modelDesignSettingTable <- getModelDesignSettingTable(
    modeldesignsRow = modeldesigns
  )
  
  modelDesign <- getModelDesignCsv(
    modelDesignSettingTable = modelDesignSettingTable, 
    csvFolder = csvFolder
  )
  
  databaseMetaDataName <- dir(csvFolder, pattern = 'database_meta_data.csv')
  databaseMetaData  <- readr::read_csv(file.path(csvFolder, databaseMetaDataName))
  databaseDetailsName <- dir(csvFolder, pattern = 'database_details.csv')
  databaseDetails  <- readr::read_csv(file.path(csvFolder, databaseDetailsName))
  databases <- merge(databaseDetails, databaseMetaData, by.x = 'database_meta_data_id', by.y = 'database_id')
  
  db <- databases[databases$database_id == doi$database_id]
  
  databaseSchema <- db$cdm_source_name
  databaseId <- db$database_meta_data_id
  
  outcomesName <- dir(csvFolder, pattern = 'diagnostic_outcomes.csv')
  outcomes  <- readr::read_csv(file.path(csvFolder, outcomesName)) %>%
    dplyr::filter(.data$diagnostic_id == !! diagnosticId) %>%
    dplyr::select(-.data$diagnostic_id)
  colnames(outcomes)  <- SqlRender::snakeCaseToCamelCase(colnames(outcomes))
  
  predictorsName <- dir(csvFolder, pattern = 'diagnostic_predictors.csv')
  predictors   <- readr::read_csv(file.path(csvFolder, predictorsName)) %>%
    dplyr::filter(.data$diagnostic_id == !! diagnosticId) %>%
    dplyr::select(-.data$diagnostic_id)
  colnames(predictors)  <- SqlRender::snakeCaseToCamelCase(colnames(predictors))
  
  participantsName <- dir(csvFolder, pattern = 'diagnostic_participants.csv')
  participants  <- readr::read_csv(file.path(csvFolder, participantsName)) %>%
    dplyr::filter(.data$diagnostic_id == !! diagnosticId) %>%
    dplyr::select(-.data$diagnostic_id)
  colnames(participants)  <- SqlRender::snakeCaseToCamelCase(colnames(participants))
  
  summaryName <- dir(csvFolder, pattern = 'diagnostic_summary.csv')
  summary  <- readr::read_csv(file.path(csvFolder, summaryName)) %>%
    dplyr::filter(.data$diagnostic_id == !! diagnosticId) %>%
    dplyr::select(-.data$diagnostic_id)
  colnames(summary)  <- SqlRender::snakeCaseToCamelCase(colnames(summary))
  
  result <- list(
    summary = summary,
    participants = participants,
    predictors = predictors,
    outcomes = outcomes,
    designs = NULL,
    modelDesign = modelDesign,
    databaseSchema = databaseSchema,
    databaseId = databaseId
  )
  
  class(result) <- 'diagnosePlp'
  
  return(result)
}

