# @file RunMultiplePlp.R
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


#' Run a list of predictions analyses
#'
#' @details
#' This function will run all specified predictions as defined using . 
#'
#' @param databaseDetails               The database settings created using \code{createDatabaseDetails()}
#' @param modelDesignList                A list of model designs created using \code{createModelDesign()}
#' @param onlyFetchData                  Only fetches and saves the data object to the output folder without running the analysis.
#' @param cohortDefinitions               A list of cohort definitions for the target and outcome cohorts
#' @param logSettings                    The setting spexcifying the logging for the analyses created using \code{createLogSettings()}
#' @param saveDirectory                   Name of the folder where all the outputs will written to.
#' 
#' @return
#' A data frame with the following columns: \tabular{ll}{ \verb{analysisId} \tab The unique identifier
#' for a set of analysis choices.\cr \verb{cohortId} \tab The ID of the target cohort populations.\cr
#' \verb{outcomeId} \tab The ID of the outcomeId.\cr \verb{dataLocation} \tab The location where the plpData was saved 
#'  \cr \verb{the settings ids} \tab The ids for all other settings used for model development.\cr }
#'
#' @export
runMultiplePlp <- function(
  databaseDetails = createDatabaseDetails(),
  modelDesignList = list(
    createModelDesign(cohortId = 1, outcomeId = 2, modelSettings = setLassoLogisticRegression()), 
    createModelDesign(cohortId = 1, outcomeId = 3, modelSettings = setLassoLogisticRegression())
  ),
  onlyFetchData = F,
  cohortDefinitions = NULL,
  logSettings = createLogSettings(
    verbosity = "DEBUG", 
    timeStamp = T, 
    logName = "runPlp Log"
  ),
  saveDirectory = getwd()
){
  
  #input checks
  checkIsClass(databaseDetails, c('databaseDetails'))
  checkIsClass(modelDesignList, c('list', 'modelDesign'))
  checkIsClass(onlyFetchData, 'logical')
  checkIsClass(logSettings, 'logSettings')
  checkIsClass(saveDirectory, 'character')
  if(!dir.exists(saveDirectory)){
    dir.create(saveDirectory, recursive = T)
  }
  
  settingstable <- convertToJson(modelDesignList,cohortDefinitions)
  
  if(nrow(settingstable) != length(modelDesignList)){
    stop('Error in settingstable')
  }
  
  # save the settings
  utils::write.csv(settingstable, file.path(saveDirectory,'settings.csv'), row.names = F)

  # group the outcomeIds per combination of data extraction settings
  dataSettings <- settingstable %>% 
    dplyr::group_by(
      .data$cohortId,
      .data$covariateSettings,
      .data$restrictPlpDataSettings,
      .data$dataLocation
    ) %>% 
    dplyr::summarise(
      outcomeIds = paste(unique(.data$outcomeId), collapse = ',')
      )
  
  # extract data
  for(i in 1:nrow(dataSettings)){
    dataExists <- length(dir(file.path(saveDirectory, dataSettings$dataLocation[i])))>0
    if(!dataExists){
      ParallelLogger::logInfo(paste('Extracting data for cohort', dataSettings$cohortId[i], 'to', file.path(saveDirectory, dataSettings$dataLocation[i])))
      
      databaseDetails$cohortId <- dataSettings$cohortId[i]
      databaseDetails$outcomeIds <- strsplit(dataSettings$outcomeIds[i], ',')[[1]]
      
      plpDataSettings <- list(
        databaseDetails = databaseDetails,
        covariateSettings = ParallelLogger::convertJsonToSettings(dataSettings$covariateSettings[i]),
        restrictPlpDataSettings = ParallelLogger::convertJsonToSettings(dataSettings$restrictPlpDataSettings[i])
      )
      
      plpData <- tryCatch(
        {do.call(getPlpData, plpDataSettings)},
        error = function(e){ParallelLogger::logInfo(e); return(NULL)}
      )
      if(!is.null(plpData)){
        savePlpData(plpData, file.path(saveDirectory, dataSettings$dataLocation[i]))
      }
    } else{
      ParallelLogger::logInfo(paste('Data for cohort', dataSettings$cohortId[i], 'exists at', file.path(saveDirectory, dataSettings$dataLocation[i])))
    }
  }
  
  # runDiagnosis - NEW
  if(!onlyFetchData){
    for(i in 1:nrow(settingstable)){
      modelDesign <- modelDesignList[[i]]
      settings <- settingstable[i,] # just the data locations?
      
      dataExists <- length(dir(file.path(saveDirectory, settings$dataLocation)))>0
      
      if(dataExists){
        plpData <- PatientLevelPrediction::loadPlpData(file.path(saveDirectory, settings$dataLocation))
        
        analysisExists <- file.exists(file.path(saveDirectory, settings$analysisId,'diagnosePlp.rds'))
        
        if(!analysisExists){
          diagnosePlpSettings <- list(
            plpData = plpData,
            outcomeId = modelDesign$outcomeId,
            analysisId = settings$analysisId,
            populationSettings = modelDesign$populationSettings,
            splitSettings = modelDesign$splitSettings,
            sampleSettings = modelDesign$sampleSettings,
            featureEngineeringSettings = modelDesign$featureEngineeringSettings,
            preprocessSettings = modelDesign$preprocessSettings,
            modelSettings = modelDesign$modelSettings,
            logSettings = logSettings,
            saveDirectory = saveDirectory
          )
          
          diagnose <- tryCatch(
            {do.call(diagnosePlp, diagnosePlpSettings)},
            error = function(e){ParallelLogger::logInfo(e); return(NULL)}
          )
        } else{
          ParallelLogger::logInfo(paste('Diagnosis ', settings$analysisId, 'exists at', file.path(saveDirectory, settings$analysisId)))
        }
      }
    }
  }
  
  # runPlp
  if(!onlyFetchData){
    for(i in 1:nrow(settingstable)){
      modelDesign <- modelDesignList[[i]]
      settings <- settingstable[i,] # just the data locations?
      
      dataExists <- length(dir(file.path(saveDirectory, settings$dataLocation)))>0
      
      if(dataExists){
        plpData <- PatientLevelPrediction::loadPlpData(file.path(saveDirectory, settings$dataLocation))
        
        analysisExists <- file.exists(file.path(saveDirectory, settings$analysisId,'plpResult', 'runPlp.rds'))
        if(!analysisExists){
          
          runPlpSettings <- list(
            plpData = plpData,
            outcomeId = modelDesign$outcomeId,
            analysisId = settings$analysisId,
            populationSettings = modelDesign$populationSettings,
            splitSettings = modelDesign$splitSettings,
            sampleSettings = modelDesign$sampleSettings,
            featureEngineeringSettings = modelDesign$featureEngineeringSettings,
            preprocessSettings = modelDesign$preprocessSettings,
            modelSettings = modelDesign$modelSettings,
            logSettings = logSettings,
            executeSettings = modelDesign$executeSettings,
            saveDirectory = saveDirectory
          )
          
          result <- tryCatch(
            {do.call(runPlp, runPlpSettings)},
            error = function(e){ParallelLogger::logInfo(e); return(NULL)}
          )
        } else{
          ParallelLogger::logInfo(paste('Analysis ', settings$analysisId, 'exists at', file.path(saveDirectory, settings$analysisId)))
        }
      }
    } # end run per setting
  }
  
  # [TODO] add code to create sqlite database and populate with results...
  if(!onlyFetchData){
    insertResultsToSqlite(
      resultLocation = saveDirectory, 
      cohortDefinitions = cohortDefinitions,
      databaseList = createDatabaseList(
        cdmDatabaseSchemas = databaseDetails$cohortDatabaseSchema
      ),
      sqliteLocation = file.path(saveDirectory, 'sqlite')
    )
  }
  
  
  return(invisible(settingstable))
}


#' Specify settings for deceloping a single model 
#'
#' @details
#' This specifies a single analysis for developing as single model
#'
#' @param cohortId              The id of the target cohort that will be used for data extraction (e.g., the ATLAS id)
#' @param outcomeId              The id of the outcome that will be used for data extraction (e.g., the ATLAS id)
#' @param restrictPlpDataSettings       The settings specifying the extra restriction settings when extracting the data created using \code{createRestrictPlpDataSettings()}.
#' @param populationSettings             The population settings specified by \code{createStudyPopulationSettings()}
#' @param covariateSettings              The covariate settings, this can be a list or a single \code{'covariateSetting'} object.
#' @param featureEngineeringSettings      Either NULL or an object of class \code{featureEngineeringSettings} specifying any feature engineering used during model development
#' @param sampleSettings                  Either NULL or an object of class \code{sampleSettings} with the over/under sampling settings used for model development
#' @param preprocessSettings              Either NULL or an object of class \code{preprocessSettings} created using \code{createPreprocessingSettings()}
#' @param modelSettings                   The model settings such as \code{setLassoLogisticRegression()}
#' @param splitSettings                  The train/validation/test splitting used by all analyses created using \code{createDefaultSplitSetting()}
#' @param runCovariateSummary             Whether to run the covariateSummary
#' 
#' @return
#' A list with analysis settings used to develop a single prediction model
#'
#' @export
createModelDesign <- function(
  cohortId,
  outcomeId,
  restrictPlpDataSettings = createRestrictPlpDataSettings(),
  populationSettings = createStudyPopulationSettings(),
  covariateSettings = FeatureExtraction::createDefaultCovariateSettings(),
  featureEngineeringSettings = NULL,
  sampleSettings = NULL,
  preprocessSettings = NULL,
  modelSettings = NULL,
  splitSettings = createDefaultSplitSetting(
    type = "stratified", 
    testFraction = 0.25,
    trainFraction = 0.75, 
    splitSeed = 123, 
    nfold = 3
  ),
  runCovariateSummary = T
){
  
  checkIsClass(cohortId, c('numeric','integer'))
  checkIsClass(outcomeId, c('numeric','integer'))
  
  checkIsClass(populationSettings, c('populationSettings'))
  checkIsClass(restrictPlpDataSettings, 'restrictPlpDataSettings')
  checkIsClass(covariateSettings, c('covariateSettings', 'list'))
  checkIsClass(splitSettings, 'splitSettings')
  
  useFE <- F
  if(!is.null(featureEngineeringSettings)){
    if(class(featureEngineeringSettings) == 'featureEngineeringSettings'){
      featureEngineeringSettings <- list(featureEngineeringSettings)
    }
    lapply(featureEngineeringSettings, function(x) checkIsClass(x, c('featureEngineeringSettings')))
    useFE <- T
  } else{
    featureEngineeringSettings <- createFeatureEngineeringSettings(type = "none")
  }
  
  useSample <- F
  if(!is.null(sampleSettings)){
    
    if(class(sampleSettings) == 'sampleSettings'){
      sampleSettings <- list(sampleSettings)
    }
    lapply(sampleSettings, function(x) checkIsClass(x, c('sampleSettings')))
    
    useSample <- T
  } else{
    sampleSettings <- createSampleSettings(type = "none")
  }
  
  usePreprocess <- F
  if(!is.null(preprocessSettings)){
    checkIsClass(preprocessSettings, c('preprocessSettings'))
    usePreprocess <- T
  } else{
    preprocessSettings <- createPreprocessSettings(
      minFraction = 0.001,
      normalize = T
    )
  }
  
  checkIsClass(modelSettings, c('modelSettings'))
  
  settings <- list(
    cohortId = cohortId,
    outcomeId = outcomeId,
    restrictPlpDataSettings = restrictPlpDataSettings,
    covariateSettings = covariateSettings,
    populationSettings = populationSettings,
    sampleSettings = sampleSettings,
    featureEngineeringSettings = featureEngineeringSettings,
    preprocessSettings = preprocessSettings,
    modelSettings = modelSettings,
    splitSettings = splitSettings,
    executeSettings = createExecuteSettings(
      runSplitData = T,
      runSampleData = useSample,
      runfeatureEngineering = useFE,
      runPreprocessData = usePreprocess,
      runModelDevelopment = !is.null(modelSettings),
      runCovariateSummary =  runCovariateSummary
    )
    
  )
  
  class(settings) <- 'modelDesign'
  return(settings)
  
}


#' Save the modelDesignList to a json file
#'
#' @details
#' This function creates a json file with the modelDesignList saved
#' 
#' @param modelDesignList          A list of modelDesigns created using \code{createModelDesign()}
#' @param cohortDefinitions        A list of the cohortDefinitions (generally extracted from ATLAS)
#' @param saveDirectory            The directory to save the modelDesignList settings
#'                                       
#' @examples
#' \dontrun{
#' savePlpAnalysesJson(
#' modelDesignList = list(
#' createModelDesign(cohortId = 1, outcomeId = 2, modelSettings = setLassoLogisticRegression()), 
#' createModelDesign(cohortId = 1, outcomeId = 3, modelSettings = setLassoLogisticRegression())
#' ),
#' saveDirectory = 'C:/bestModels'
#' )
#' }
#'
#' @export
savePlpAnalysesJson <- function(
  modelDesignList = list(
  createModelDesign(cohortId = 1, outcomeId = 2, modelSettings = setLassoLogisticRegression()), 
  createModelDesign(cohortId = 1, outcomeId = 3, modelSettings = setLassoLogisticRegression())
  ),
  cohortDefinitions = NULL,
  # add cohortDefinitions
  saveDirectory = NULL
  ){
  
  if(class(modelDesignList) == 'modelDesign'){
    modelDesignList <- list(modelDesignList)
  }
  
  lapply(modelDesignList, function(x){checkIsClass(x, 'modelDesign')})

  if(!is.null(saveDirectory)){
    checkIsClass(saveDirectory, 'character')
    
    if(!dir.exists(saveDirectory)){
      dir.create(saveDirectory, recursive = T)
    }
    
    ParallelLogger::saveSettingsToJson(
      object = list(
        plpVersion = as.character(utils::packageVersion("PatientLevelPrediction")),
        analyses = modelDesignList,
        cohortDefinitions = cohortDefinitions
      ), 
      fileName = file.path(saveDirectory,"predictionAnalysisList.json")
    )
    
    return(file.path(saveDirectory,"predictionAnalysisList.json")) 
  }
  
  return(
    ParallelLogger::convertSettingsToJson(
      object = list(
        plpVersion = as.character(utils::packageVersion("PatientLevelPrediction")),
        analyses = modelDesignList,
        cohortDefinitions = cohortDefinitions
      )
    )
  )
}


#' Load the multiple prediction json settings from a file
#'
#' @details
#' This function interprets a json with the multiple prediction settings and creates a list 
#' that can be combined with connection settings to run a multiple prediction study
#' 
#' @param jsonFileLocation    The location of the file 'predictionAnalysisList.json' with the modelDesignList  
#'                                      
#' @examples
#' \dontrun{
#' modelDesignList <- loadPlpAnalysesJson('location of json settings')$analysis
#' }
#'
#' @export
loadPlpAnalysesJson <- function(
  jsonFileLocation 
){
  
  checkIsClass(jsonFileLocation, 'character')
  if(!file.exists(jsonFileLocation)){
    ParallelLogger::logError('Invalid directory - does not exist')
  }
  
  rList <- ParallelLogger::loadSettingsFromJson(fileName = jsonFileLocation)
  
  return(rList)
}





#' externally validate the multiple plp models across new datasets
#' @description
#' This function loads all the models in a multiple plp analysis folder and
#' validates the models on new data
#' @details
#' Users need to input a location where the results of the multiple plp analyses
#' are found and the connection and database settings for the new data
#' 
#' @param analysesLocation                The location where the multiple plp analyses are
#' @param validationDatabaseDetails       The validation database settings created using \code{createDatabaseDetails()}
#' @param validationRestrictPlpDataSettings  The settings specifying the extra restriction settings when extracting the data created using \code{createRestrictPlpDataSettings()}.
#' @param recalibrate                      A vector of recalibration methods (currently supports 'RecalibrationintheLarge' and/or 'weakRecalibration')
#' @param cohortDefinitions           A list of cohortDefinitions
#' @param saveDirectory               The location to save to validation results
#' 
#' @export 
validateMultiplePlp <- function(
  analysesLocation,
  validationDatabaseDetails,
  validationRestrictPlpDataSettings = createRestrictPlpDataSettings(),
  recalibrate = NULL,
  cohortDefinitions = NULL,
  saveDirectory = NULL
  ){

  # add input checks 
  checkIsClass(analysesLocation, 'character')
  
  checkIsClass(validationDatabaseDetails, 'databaseDetails')
  checkIsClass(validationRestrictPlpDataSettings, 'restrictPlpDataSettings')
  
  checkIsClass(recalibrate, c('character', 'NULL'))
  checkIsClass(saveDirectory, c('character', 'NULL'))

  # for each model run externalValidateDbPlp()
  analyses <- dir(analysesLocation, recursive = F, full.names = F)
  
  # now fine all analysis folders..
  analyses <- analyses[grep('Analysis_',analyses)]
  
  for(i in 1:length(analyses)){
    
    if(is.null(saveDirectory)){
      saveLocation <- file.path(analysesLocation, 'Validation')
    } else{
      saveLocation <- saveDirectory
    }
    
    analysis <- analyses[i]
    modelSettings <- file.path(analysesLocation, analysis)
    
    ParallelLogger::logInfo(paste0('Evaluating model in ',modelSettings ))
    
    if(dir.exists(file.path(modelSettings[i],'plpResult'))){
      ParallelLogger::logInfo(paste0('plpResult found in ',modelSettings ))
      
      plpModel <- loadPlpModel(file.path(modelSettings,'plpResult','model'))
      
      validations <-   tryCatch(
        {
          externalValidateDbPlp(
            plpModel = plpModel,
            validationDatabaseDetails = validationDatabaseDetails,
            validationRestrictPlpDataSettings = validationRestrictPlpDataSettings,
            settings = createValidationSettings(
              recalibrate = recalibrate
            ),
            outputFolder = saveLocation
          )},
        error = function(cont){ParallelLogger::logInfo(paste0('Error: ',cont ))
          ;return(NULL)}
      )
      
    }
  }
  
  # add to sqlite database - needed for shiny app
  #=======================
    if(is.null(saveDirectory)){
      sqliteLocation <- file.path(analysesLocation, 'sqlite')
    } else{
      sqliteLocation <- file.path(saveDirectory,'sqlite')
    }
  
  for(validationDatabase in dir(saveLocation)){
    tryCatch({
      insertResultsToSqlite(
        resultLocation = file.path(saveLocation, validationDatabase), 
        cohortDefinitions = cohortDefinitions,
        databaseList = createDatabaseList(
          cdmDatabaseSchemas = 'none'
        ),
        sqliteLocation = sqliteLocation
      )
    })
  }
  #=======================
  
}

convertToJson <-function(
  modelDesignList,
  cohortDefinitions = NULL
){
  
  convertToJsonString <- function(x){as.character(ParallelLogger::convertSettingsToJson(x))}
  
  result <- data.frame(
    analysisId = paste0('Analysis_', 1:length(modelDesignList)),
    cohortId = unlist(lapply(modelDesignList, function(x) ifelse(is.null(x$cohortId), x$targetId, x$cohortId))),
    outcomeId = unlist(lapply(modelDesignList, function(x) x$outcomeId)),
    covariateSettings = unlist(lapply(modelDesignList, function(x) convertToJsonString(x$covariateSettings))),
    restrictPlpDataSettings = unlist(lapply(modelDesignList, function(x)  convertToJsonString(x$restrictPlpDataSettings))),
    populationSettings = unlist(lapply(modelDesignList, function(x)  convertToJsonString(x$populationSettings))),
    sampleSettings = unlist(lapply(modelDesignList, function(x)  convertToJsonString(x$sampleSettings))),
    splitSettings = unlist(lapply(modelDesignList, function(x)  convertToJsonString(x$splitSettings))),
    featureEngineeringSettings = unlist(lapply(modelDesignList, function(x)  convertToJsonString(x$featureEngineeringSettings))),
    preprocessSettings = unlist(lapply(modelDesignList, function(x)  convertToJsonString(x$preprocessSettings))),
    modelSettings = unlist(lapply(modelDesignList, function(x)  convertToJsonString(x$modelSettings))),
    executeSettings = unlist(lapply(modelDesignList, function(x)  convertToJsonString(x$executeSettings)))
  )
  
  if(!is.null(cohortDefinitions)){
    
    cohorts <- data.frame(
      cohortName = unlist(lapply(cohortDefinitions, function(x) x$name)),
      cohortId = unlist(lapply(cohortDefinitions, function(x) x$id))
    )
    
    result <- result %>% 
      dplyr::left_join(cohorts, by = c("outcomeId" = "cohortId")) %>%
      dplyr::rename(outcomeName = .data$cohortName) %>%
      dplyr::left_join(cohorts, by = c('cohortId' = 'cohortId'))
    
  }
  
  # get the names
  uniqueSettings <-  result %>% 
    dplyr::distinct(
      .data$cohortId, 
      .data$covariateSettings, 
      .data$restrictPlpDataSettings
    ) %>%
    dplyr::mutate(dataLocation = paste0('cohortId_',.data$cohortId, '_L', dplyr::row_number()))
  
  # add the data names
  result <- result %>% 
    dplyr::left_join(
      uniqueSettings, 
      by = c(
        "cohortId" = "cohortId",
        "covariateSettings" = "covariateSettings",
        "restrictPlpDataSettings" = "restrictPlpDataSettings"
      )
    )
  
  return(result)
}

