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
#' @param splitSettings                  The train/validation/test splitting used by all analyses created using \code{createDefaultSplitSetting()}
#' @param cohortDefinitions               A list of cohort definitions for the target and outcome cohorts
#' @param logSettings                    The setting spexcifying the logging for the analyses created using \code{createLogSettings()}
#' @param saveDirectory                   Name of the folder where all the outputs will written to.
#' 
#' @return
#' A data frame with the following columns: \tabular{ll}{ \verb{analysisId} \tab The unique identifier
#' for a set of analysis choices.\cr \verb{cohortId} \tab The ID of the target cohort populations.\cr
#' \verb{outcomeId} \tab The ID of the outcomeId.\cr \verb{dataLocation} \tab The location where the plpData was saved \cr \verb{evaluationFolder} \tab The name of file containing the evaluation saved as a csv
#'  \cr \verb{the settings ids} \tab The ids for all other settings used for model development.\cr }
#'
#' @export
runMultiplePlp <- function(
  databaseDetails = createDatabaseDetails(),
  modelDesignList = list(
    createModelDesign(targetId = 1, outcomeId = 2, modelSettings = setLassoLogisticRegression()), 
    createModelDesign(targetId = 1, outcomeId = 3, modelSettings = setLassoLogisticRegression())
  ),
  onlyFetchData = F,
  splitSettings = createDefaultSplitSetting(
    type = "stratified", 
    testFraction = 0.25,
    trainFraction = 0.75, 
    splitSeed = 123, 
    nfold = 3
  ),
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
  checkIsClass(splitSettings, 'splitSettings')
  checkIsClass(logSettings, 'logSettings')
  checkIsClass(saveDirectory, 'character')
  if(!dir.exists(saveDirectory)){
    dir.create(saveDirectory, recursive = T)
  }
  
  # get idList
  idList <- getidList(modelDesignList = modelDesignList)
  
  # get settings data.frame
  settingstable <- getSettingsTable(
    modelDesignList = modelDesignList, 
    idList = idList
  )
  
  if(!is.null(cohortDefinitions)){
    cohortNames <- data.frame(
      targetName = getNames(cohortDefinitions, settingstable$targetId),
      outcomeName = getNames(cohortDefinitions, settingstable$outcomeId)
      )
    settingstable <- cbind(cohortNames, settingstable)
  }
  
  utils::write.csv(settingstable, file.path(saveDirectory,'settings.csv'), row.names = F)
  saveJsonFile(idList, file.path(saveDirectory,'settings.json'))
  
  # list(targetId, covariateSetting, outcomeIds, saveLocation)
  dataSettings <- getDataSettings(settingstable)
  
  # extract data
  for(i in 1:length(dataSettings)){
    dataExists <- length(dir(file.path(saveDirectory, dataSettings[[i]]$dataLocation)))>0
    if(!dataExists){
      ParallelLogger::logInfo(paste('Extracting data for cohort', dataSettings[[i]]$targetId, 'to', file.path(saveDirectory, dataSettings[[i]]$dataLocation)))
      
      databaseDetails$cohortId <- dataSettings[[i]]$targetId
      databaseDetails$outcomeIds <- dataSettings[[i]]$outcomeIds
      
      plpDataSettings <- list(
        databaseDetails = databaseDetails,
        covariateSettings = getSettingFromId(idList, type = 'covariateSettings', dataSettings[[i]]$covariateSettings),
        restrictPlpDataSettings = getSettingFromId(idList, type = 'restrictPlpDataSettings', dataSettings[[i]]$restrictPlpDataSettings)
        )
      
      
      plpData <- tryCatch(
        {do.call(getPlpData, plpDataSettings)},
        error = function(e){ParallelLogger::logInfo(e); return(NULL)}
      )
      if(!is.null(plpData)){
        savePlpData(plpData, file.path(saveDirectory, dataSettings[[i]]$dataLocation))
      }
    } else{
      ParallelLogger::logInfo(paste('Data for cohort', dataSettings[[i]]$targetId, 'exists at', file.path(saveDirectory, dataSettings[[i]]$dataLocation)))
    }
  }
  
  # runPlp
  if(!onlyFetchData){
    for(i in 1:nrow(settingstable)){
      modelDesign <- modelDesignList[[i]]
      settings <- settingstable[i,]
      
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
            splitSettings = splitSettings,
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
    }
    
    
  }
  return(invisible(settingstable))
}


#' Specify settings for deceloping a single model 
#'
#' @details
#' This specifies a single analysis for developing as single model
#'
#' @param targetId              The id of the target cohort that will be used for data extraction (e.g., the ATLAS id)
#' @param outcomeId              The id of the outcome that will be used for data extraction (e.g., the ATLAS id)
#' @param restrictPlpDataSettings       The settings specifying the extra restriction settings when extracting the data created using \code{createRestrictPlpDataSettings()}.
#' @param populationSettings             The population settings specified by \code{createStudyPopulationSettings()}
#' @param covariateSettings              The covariate settings, this can be a list or a single \code{'covariateSetting'} object.
#' @param featureEngineeringSettings      Either NULL or an object of class \code{featureEngineeringSettings} specifying any feature engineering used during model development
#' @param sampleSettings                  Either NULL or an object of class \code{sampleSettings} with the over/under sampling settings used for model development
#' @param preprocessSettings              Either NULL or an object of class \code{preprocessSettings} created using \code{createPreprocessingSettings()}
#' @param modelSettings                   The model settings such as \code{setLassoLogisticRegression()}
#' @param runCovariateSummary             Whether to run the covariateSummary
#' 
#' @return
#' A list with analysis settings used to develop a single prediction model
#'
#' @export
createModelDesign <- function(
  targetId,
  outcomeId,
  restrictPlpDataSettings = createRestrictPlpDataSettings(),
  populationSettings = createStudyPopulationSettings(),
  covariateSettings = FeatureExtraction::createDefaultCovariateSettings(),
  featureEngineeringSettings = NULL,
  sampleSettings = NULL,
  preprocessSettings = NULL,
  modelSettings = NULL,
  runCovariateSummary = T
){
  
  checkIsClass(targetId, c('numeric','integer'))
  checkIsClass(outcomeId, c('numeric','integer'))
  
  checkIsClass(populationSettings, c('populationSettings'))
  checkIsClass(restrictPlpDataSettings, 'restrictPlpDataSettings')
  checkIsClass(covariateSettings, c('covariateSettings', 'list'))
  
  useFE <- F
  if(!is.null(featureEngineeringSettings)){
    checkIsClass(featureEngineeringSettings, c('featureEngineeringSettings'))
    useFE <- T
  } else{
    featureEngineeringSettings <- createFeatureEngineeringSettings(type = "none")
  }
  
  useSample <- F
  if(!is.null(sampleSettings)){
    checkIsClass(sampleSettings, c('sampleSettings'))
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
    targetId = targetId,
    outcomeId = outcomeId,
    restrictPlpDataSettings = restrictPlpDataSettings,
    covariateSettings = covariateSettings,
    populationSettings = populationSettings,
    sampleSettings = sampleSettings,
    featureEngineeringSettings = featureEngineeringSettings,
    preprocessSettings = preprocessSettings,
    modelSettings = modelSettings,
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
#' @param saveDirectory            The directory to save the modelDesignList settings
#'                                       
#' @examples
#' \dontrun{
#' savePlpAnalysesJson(
#' modelDesignList = list(
#' createModelDesign(targetId = 1, outcomeId = 2, modelSettings = setLassoLogisticRegression()), 
#' createModelDesign(targetId = 1, outcomeId = 3, modelSettings = setLassoLogisticRegression())
#' ),
#' saveDirectory = 'C:/bestModels'
#' )
#' }
#'
#' @export
savePlpAnalysesJson <- function(
  modelDesignList = list(
  createModelDesign(targetId = 1, outcomeId = 2, modelSettings = setLassoLogisticRegression()), 
  createModelDesign(targetId = 1, outcomeId = 3, modelSettings = setLassoLogisticRegression())
  ),
  saveDirectory = NULL
  ){
  
  if(class(modelDesignList) == 'modelDesign'){
    modelDesignList <- list(modelDesignList)
  }
  
  lapply(modelDesignList, function(x){checkIsClass(x, 'modelDesign')})

  
  # save this as a json
  modelDesignList <- lapply(modelDesignList, function(x) prepareToJson(x))
  jsonSettings <- list(analyses = modelDesignList) # TODO: rename this ModelDesignList?
  
  if(!is.null(saveDirectory)){
    checkIsClass(saveDirectory, 'character')
    
    if(!dir.exists(saveDirectory)){
      dir.create(saveDirectory, recursive = T)
    }
    
    modelDesignList <- jsonlite::toJSON(
      x = jsonSettings, 
      pretty = T, 
      digits = 23, 
      auto_unbox=TRUE, 
      null = "null"
      )
    write(modelDesignList, file.path(saveDirectory,"predictionAnalysisList.json"))
    
    # should we add splitSettings to this and the input?
    
    return(file.path(saveDirectory,"predictionAnalysisList.json")) 
  }
  
  return(jsonSettings)
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
  
  if(!file.exists(file.path(jsonFileLocation))){
    ParallelLogger::logError('predictionAnalysisList.json not found ')
  }
  
  
  json <- tryCatch(
    {readChar(jsonFileLocation, file.info(jsonFileLocation)$size)},
    error= function(cond) {
      ParallelLogger::logInfo('Issue with loading json file...');
      ParallelLogger::logError(cond)
    })
  json <- tryCatch(
    {jsonlite::fromJSON(json, simplifyVector = T, simplifyDataFrame = F, simplifyMatrix = T)},
    error = function(cond) {
      ParallelLogger::logInfo('Issue with parsing json object...');
      ParallelLogger::logError(cond)
    })
  json$analyses <- tryCatch(
    {lapply(json$analyses, function(x) prepareToRlist(x))},
    error = function(cond) {
      ParallelLogger::logInfo('Issue converting json to R list...');
      ParallelLogger::logError(cond)
    })
  
  # if splitSettings in json
  if('splitSettings' %in% names(json)){
    # update the splitsetting (move this into load/saveplpAnalysis)
    if('attributes' %in% names(json$splitSettings)){
      atts <- json$splitSettings$attributes
      json$splitSettings$attributes <- NULL
      attributes(json$splitSettings) <- atts
    }
  }
  
  return(json)
  
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
#' @param saveDirectory               The location to save to validation results
#' 
#' @export 
validateMultiplePlp <- function(
  analysesLocation,
  validationDatabaseDetails,
  validationRestrictPlpDataSettings = createRestrictPlpDataSettings(),
  recalibrate = NULL,
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
  
}















# HELPERS
#===============================
getidList <- function(modelDesignList){
  
  types <- c(
    'targetId', 
    'outcomeId', 
    'restrictPlpDataSettings',
    'covariateSettings', 
    'populationSettings', 
    'sampleSettings',
    'featureEngineeringSettings',
    'preprocessSettings',
    'modelSettings',
    'executeSettings'
  )
  
  idList <- list()
  length(idList) <- length(types)
  names(idList) <- types
  
  for(type in types){
    idList[[type]] <- getSettingValues(modelDesignList, type = type )
  }
  
  return(idList)
}


getSettingValues <- function(modelDesignList, type = 'cohortId' ){
  
  if(class(modelDesignList) == 'list'){
    values <- unique(unlist(lapply(modelDesignList, function(x)jsonlite::serializeJSON(x[[type]])))
      )
  } else{
    values <- jsonlite::serializeJSON(modelDesignList[[type]])
  }
  
  if(! type %in% c('targetId', 'outcomeId')  ){
    result <- data.frame(
      value = values,
      id = 1:length(values)
    )
  } else{
    result <- data.frame(
      value = sapply(values, function(x) jsonlite::unserializeJSON(x)),
      id = sapply(values, function(x) jsonlite::unserializeJSON(x))
    )
  }
  
  return(result)
}

# get the ids for the model design settings
getIdsForSetting <- function(modelDesign, idList){
  
  ids <- c()
  
  for(settingType in names(idList)){
    
    if(!settingType %in% c('targetId', 'outcomeId')){
    # get the index of the setting matching the design setting
      ind <- which(idList[[settingType]]$value == jsonlite::serializeJSON(modelDesign[[settingType]]))
    } else{
      ind <- which(idList[[settingType]]$value == modelDesign[[settingType]])
    }
    # get the id
    id <- idList[[settingType]]$id[ind]
    
    ids <- c(ids, id)
  }
  
  names(ids) <- names(idList)
  
  return(ids)
}


# this creates a data.frame with the analysisId and settingsId for each analysis
# need to add the data location to this
getSettingsTable <- function(modelDesignList, idList){
  
  result <- lapply(modelDesignList, function(x) getIdsForSetting(x, idList) )
  settingsTable <- do.call(rbind, result)
  settingsTable <- as.data.frame(settingsTable)
  
  settingsTable$analysisId <- paste0('Analysis_', 1:nrow(settingsTable))
  
  settingsTable$dataLocation <- paste0('T_',settingsTable$targetId, '_L_', settingsTable$covariateSettings*settingsTable$restrictPlpDataSettings)
  
  return(settingsTable)
}


getSettingFromId <- function(
  idList, 
  type, 
  id
){
  ind <- which(idList[[type]]$id == id)
  if(!type %in% c('targetId', 'outcomeId')){
    return(jsonlite::unserializeJSON(as.character(idList[[type]]$value[[ind]])))
  } else{
    return(idList[[type]]$value[[ind]])
  }
}


getDataSettings <- function(settingstable){
  
  combos <- settingstable %>% 
    dplyr::distinct(.data$targetId,.data$covariateSettings,.data$restrictPlpDataSettings,.data$dataLocation)
  
  result <- list()
  length(result) <- nrow(combos)
  for(i in 1:nrow(combos)){
    result[[i]] <- list(
      targetId = combos$targetId[i],
      covariateSettings = combos$covariateSettings[i],
      restrictPlpDataSettings = combos$restrictPlpDataSettings[i],
      dataLocation = combos$dataLocation[i],
      outcomeIds = settingstable %>%  
        dplyr::filter(.data$dataLocation == combos$dataLocation[i]) %>%  
        dplyr::select(.data$outcomeId) %>%  
        dplyr::pull()
    )
  }
  return(result)
}

getNames <- function(
  cohortDefinitions, 
  ids
){
  
  idNames <- lapply(cohortDefinitions, function(x) c(x$id, x$name))
  idNames <- do.call(rbind, idNames)
  colnames(idNames) <- c('id', 'name')
  idNames <- as.data.frame(idNames)
  
  nams <- c()
  for(id in ids){
    nams <- c(nams, idNames$name[idNames$id == id])
  }
  
  return(nams)
  
}
