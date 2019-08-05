# @file RunMultiplePlp.R
#
# Copyright 2019 Observational Health Data Sciences and Informatics
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

#' Run a list of predictions
#'
#' @details
#' Run a list of predictions for the target cohorts and outcomes of interest. This function will run all
#' specified predictions, meaning that the total number of outcome
#' models is `length(cohortIds) * length(outcomeIds) * length(modelAnalysisList)`. 
#'
#' @param connectionDetails              An R object of type \code{connectionDetails} created using the
#'                                       function \code{createConnectionDetails} in the
#'                                       \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema              The name of the database schema that contains the OMOP CDM
#'                                       instance. Requires read permissions to this database. On SQL
#'                                       Server, this should specifiy both the database and the schema,
#'                                       so for example 'cdm_instance.dbo'.
#' @param cdmDatabaseName                A string with a shareable name of the database (this will be shown to OHDSI researchers if the results get transported)
#' @param cdmVersion                     Define the OMOP CDM version used: currently support "4" and
#'                                       "5".
#' @param oracleTempSchema               For Oracle only: the name of the database schema where you
#'                                       want all temporary tables to be managed. Requires
#'                                       create/insert permissions to this database.
#' @param cohortDatabaseSchema           The name of the database schema that is the location where the
#'                                       target cohorts are available.  Requires read
#'                                       permissions to this database.
#' @param cohortTable                    The tablename that contains the target cohorts.  Expectation is cohortTable
#'                                       has format of COHORT table: COHORT_DEFINITION_ID, SUBJECT_ID,
#'                                       COHORT_START_DATE, COHORT_END_DATE.
#' @param outcomeDatabaseSchema          The name of the database schema that is the location where the
#'                                       data used to define the outcome cohorts is available. Requires read permissions to
#'                                       this database.
#' @param outcomeTable                   The tablename that contains the outcome cohorts.  Expectation is
#'                                       outcomeTable has format of COHORT table: COHORT_DEFINITION_ID,
#'                                       SUBJECT_ID, COHORT_START_DATE, COHORT_END_DATE.
#' @param outputFolder                   Name of the folder where all the outputs will written to.
#' @param modelAnalysisList              A list of objects of type \code{modelSettings} as created using
#'                                       the \code{\link{createPlpModelSettings}} function.
#' @param cohortIds                      A vector of cohortIds that specify all the target cohorts
#' @param cohortNames                    A vector of cohortNames corresponding to the cohortIds
#' @param outcomeIds                     A vector of outcomeIds that specify all the outcome cohorts                                        
#' @param outcomeNames                   A vector of outcomeNames corresponding to the outcomeIds
#' @param washoutPeriod                  Minimum number of prior observation days
#' @param maxSampleSize                  Max number of target people to sample from to develop models
#' @param minCovariateFraction           Any covariate with an incidence less than this value if ignored
#' @param normalizeData                  Whether to normalize the covariates
#' @param testSplit                      How to split into test/train (time or person)
#' @param testFraction                   Fraction of data to use as test set
#' @param splitSeed                      The seed used for the randomization into test/train
#' @param nfold                          Number of folds used to do cross validation
#' @param verbosity                      The logging level
#' 
#' @return
#' A data frame with the following columns: \tabular{ll}{ \verb{analysisId} \tab The unique identifier
#' for a set of analysis choices.\cr \verb{cohortId} \tab The ID of the target cohort populations.\cr
#' \verb{outcomeId} \tab The ID of the outcomeId.\cr \verb{plpDataFolder} \tab The location where the plpData was saved\cr \verb{studyPopFile} \tab The
#' name of the file containing the study population \cr \verb{evaluationFolder} \tab The name of file containing the evaluation saved as a csv
#'  \cr \verb{modelFolder} \tab The name of the file containing the developed model.\cr }
#'
#' @export
runPlpAnalyses <- function(connectionDetails,
                          cdmDatabaseSchema,
                          cdmDatabaseName,
                          oracleTempSchema = cdmDatabaseSchema,
                          cohortDatabaseSchema = cdmDatabaseSchema,
                          cohortTable = "cohort",
                          outcomeDatabaseSchema = cdmDatabaseSchema,
                          outcomeTable = "cohort",
                          cdmVersion = 5,
                          outputFolder = "./PlpOutput",
                          modelAnalysisList,
                          cohortIds,
                          cohortNames,
                          outcomeIds,
                          outcomeNames,
                          washoutPeriod = 0,
                          maxSampleSize = NULL,
                          minCovariateFraction = 0,
                          normalizeData = T,
                          testSplit = "person",
                          testFraction = 0.25,
                          splitSeed = NULL,
                          nfold = 3,
                          verbosity = "INFO") {
  
  # start log:
  clearLoggerType("Multple PLP Log")
  if(!dir.exists(outputFolder)){dir.create(outputFolder,recursive=T)}
  logFileName = paste0(outputFolder,'/plplog.txt')
  logger <- ParallelLogger::createLogger(name = "Multple PLP Log",
                                      threshold = verbosity,
                                      appenders = list(ParallelLogger::createFileAppender(layout = ParallelLogger::layoutParallel,
                                                                                       fileName = logFileName)))
  ParallelLogger::registerLogger(logger)
  
  if (missing(outcomeIds)){
    stop("Need to specify outcome ids")
  }
  if (missing(cohortIds)){
    stop("Need to specify cohort ids")
  }
  if (missing(connectionDetails)){
    stop("Need to specify connectionDetails")
  }
  if (missing(cdmDatabaseSchema)){
    stop("Need to specify cdmDatabaseSchema")
  }
  if (missing(cdmDatabaseName)){
    stop("Need to specify cdmDatabaseName - a shareable name for the database")
  }
  if (missing(modelAnalysisList)){
    stop("Need to specify modelAnalysisList")
  }
# check input types
  plpDataSettings <- list(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          oracleTempSchema = oracleTempSchema, 
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          cohortTable = cohortTable,
                          outcomeDatabaseSchema = outcomeDatabaseSchema,
                          outcomeTable = outcomeTable,
                          cdmVersion = cdmVersion,
                          firstExposureOnly = F,
                          washoutPeriod = washoutPeriod,
                          sampleSize = maxSampleSize
                          )
  
  runPlpSettings <- list(minCovariateFraction = minCovariateFraction,
                           normalizeData = normalizeData,
                           testSplit = testSplit,
                           testFraction = testFraction,
                           splitSeed = splitSeed,
                           nfold = nfold,
                           verbosity = verbosity )

  if (!dir.exists(outputFolder)){
    dir.create(outputFolder)
    }
  if (!dir.exists(file.path(outputFolder,'Validation'))){
    dir.create(file.path(outputFolder,'Validation'), recursive = T)
  }
  
  ParallelLogger::logTrace(paste0('Creating reference table'))
  referenceTable <- tryCatch({createPlpReferenceTable(modelAnalysisList,
                                         cohortIds,
                                         outcomeIds,
                                         outputFolder, cdmDatabaseName)},
                             error = function(cont){ParallelLogger::logTrace(paste0('Creating reference table error:', cont)); stop()})
  if(!missing(cohortNames)){
    if(!is.null(cohortNames))
      if(length(cohortNames)!=length(cohortIds)){
        stop('cohortNames entered but different length to cohortIds')
      }
      cnames <- data.frame(cohortId=cohortIds, cohortName=cohortNames)
      referenceTable <- merge(referenceTable, cnames, by='cohortId', all.x=T)
  }
  if(!missing(outcomeNames)){
    if(!is.null(outcomeNames))
      if(length(outcomeNames)!=length(outcomeIds)){
        stop('outcomeNames entered but different length to outcomeIds')
      }
    onames <- data.frame(outcomeId=outcomeIds, outcomeName=outcomeNames)
    referenceTable <- merge(referenceTable, onames, by='outcomeId', all.x=T)
  }
  
  if(!file.exists(file.path(outputFolder,'settings.csv'))){
    ParallelLogger::logTrace(paste0('Writing settings csv to ',file.path(outputFolder,'settings.csv') ))
    utils::write.csv(referenceTable,
              file.path(outputFolder,'settings.csv'), 
              row.names = F )
  }
  
  for(i in 1:nrow(referenceTable)){
    
    plpDataFolder <- referenceTable$plpDataFolder[i]
    if(!dir.exists(plpDataFolder)){
      ParallelLogger::logTrace(paste0('Running setting ', i ))
      
      oind <- referenceTable$cohortId==referenceTable$cohortId[i] & 
              referenceTable$covariateSettingId==referenceTable$covariateSettingId[i]
      outcomeIds <- unique(referenceTable$outcomeId[oind])
          
      plpDataSettings$cohortId <- referenceTable$cohortId[i]
      plpDataSettings$outcomeIds <- outcomeIds 
      plpDataSettings$covariateSettings <- modelAnalysisList$covariateSettings[[referenceTable$covariateSettingId[i]]]
        
      plpData <- tryCatch(do.call(getPlpData, plpDataSettings),
               finally= ParallelLogger::logTrace('Done plpData.'),
               error= function(cond){ParallelLogger::logTrace(paste0('Error with getPlpData:',cond));return(NULL)})
  
      if(!is.null(plpData)){
        ParallelLogger::logTrace(paste0('Saving data in setting ', i ))
        savePlpData(plpData, referenceTable$plpDataFolder[i])
      } else{
        ParallelLogger::logInfo('No plpData - probably empty cohort issue')
      }
    } else{
      ParallelLogger::logTrace(paste0('Loading data in setting ', i ))
      plpData <- loadPlpData(referenceTable$plpDataFolder[i])
    }
    
    if(!file.exists(referenceTable$studyPop[i])){
      ParallelLogger::logTrace(paste0('Setting population settings for setting ', i ))
      # get pop and save to referenceTable$popFile
      popSettings <- modelAnalysisList$populationSettings[[referenceTable$populationSettingId[i]]]
      popSettings$outcomeId <- referenceTable$outcomeId[i] 
      popSettings$plpData <- plpData
      population <- tryCatch(do.call(createStudyPopulation, popSettings),
               finally= ParallelLogger::logTrace('Done pop.'), 
               error= function(cond){ParallelLogger::logTrace(paste0('Error with pop:',cond));return(NULL)})
      if(!is.null(population)){
        ParallelLogger::logTrace(paste0('Saving population for setting ', i ))
        saveRDS(population, referenceTable$studyPop[i])
      }
    } else{
      ParallelLogger::logTrace(paste0('Loading population for setting', i ))
      population <- readRDS(referenceTable$studyPop[i])
    }
    
    plpResultFolder = file.path(referenceTable$plpResultFolder[i],'plpResult')
    if(!dir.exists(plpResultFolder)){
      ParallelLogger::logTrace(paste0('Running runPlp for setting ', i ))
      dir.create(referenceTable$plpResultFolder[i], recursive = T)
      # runPlp and save result to referenceTable$plpResultFolder
      runPlpSettings$modelSettings <- modelAnalysisList$models[[referenceTable$modelSettingId[i]]]
      runPlpSettings$plpData <- plpData
      runPlpSettings$population <- population
      runPlpSettings$saveDirectory <- gsub(paste0('/Analysis_',referenceTable$analysisId[i]),'',referenceTable$plpResultFolder[i])
      runPlpSettings$analysisId <- paste0('Analysis_',referenceTable$analysisId[i])
      runPlpSettings$savePlpData <- F
      runPlpSettings$savePlpResult <- T
      runPlpSettings$savePlpPlots <- F
      runPlpSettings$saveEvaluation <- F
      result <- tryCatch(do.call(runPlp, runPlpSettings),
                             finally= ParallelLogger::logTrace('Done runPlp.'), 
                             error= function(cond){ParallelLogger::logTrace(paste0('Error with runPlp:',cond));return(NULL)})
    }
    
  }
  return(referenceTable)
}

createPlpReferenceTable <- function(modelAnalysisList,
                        cohortIds,
                        outcomeIds,
                        outputFolder, cdmDatabaseName){
  
  #analysisId, cohortId, outcomeId, settingsFile, plpDataFolder, studyPopFile, plpResultFolder
  
  analyses <- expand.grid(cohortId = cohortIds,
                          outcomeId = outcomeIds,
                          modelSettingsId = modelAnalysisList$settingLookupTable$lookupId)
  # remove rows with same cohortId and outcomeId
  removeInd <- analyses$cohortId == analyses$outcomeId
  analyses <- analyses[!removeInd, ]
  
  analyses$analysisId <- 1:nrow(analyses)
  analyses$devDatabase <- cdmDatabaseName
  analyses <- merge(analyses, modelAnalysisList$settingLookupTable, 
                    by.x='modelSettingsId', by.y='lookupId', all.x=T)
  
  # TODO: replace outputFolder with '.' to make relative positions
  analyses$plpDataFolder <- file.path(outputFolder,
                                      paste0('PlpData_L',analyses$covariateSettingId,'_T',analyses$cohortId))
  analyses$studyPopFile <- file.path(outputFolder,
                                     paste0('StudyPop_L',analyses$populationSettingId,'_T',analyses$cohortId,'_O',analyses$outcomeId,'.rds'))
  analyses$plpResultFolder <- file.path(outputFolder,
                                        paste0('Analysis_',analyses$analysisId))
return(analyses)  
}

#' create a an object specifying the multiple Plp model settings
#'
#' @details
#' Takes a list of models, covariates, population and returns the cartesian product combining all
#' settings.
#' @param modelList                      A list of model settings 
#' @param covariateSettingList           A list of covariate settings
#' @param populationSettingList          A list of population settings
#' 
#' @return
#' A list containing a dataframe settingLookupTable containing all the model, covariate and popualtion combination details, 
#' a list models containing all the model settings, a list covariateSettings containing all the covariate settings and a list 
#' populationSettings containing all the population settings.
#' @export
createPlpModelSettings <- function(modelList, 
                                   covariateSettingList, 
                                   populationSettingList){
#check inputs
 if(!class(modelList)%in%c('modelSettings','list')){
   stop('Incorrect class for modelList')
 } else if(class(modelList)=='modelSettings'){
   modelList <- list(modelList)
 }
  if(!class(covariateSettingList)%in%c('covariateSettings','list')){
    stop('Incorrect class for covariateSettingList')
  } else if(class(covariateSettingList)=='covariateSettings'){
    covariateSettingList <- list(covariateSettingList)
  }
  if(!class(populationSettingList)%in%c('populationSettings','list')){
    stop('Incorrect class for populationSettingList')
  } else if(class(populationSettingList)=='populationSettings'){
    populationSettingList <- list(populationSettingList)
  }
 models = unique(modelList)
 names(models) <- 1:length(models)
 
 covariateSettings = unique(covariateSettingList)
 names(covariateSettings) <- 1:length(covariateSettings)
 
 populationSettings = unique(populationSettingList)
 names(populationSettings) <- 1:length(populationSettings)
 
 settingTable <- expand.grid(modelSettingId = names(models), 
                             covariateSettingId = names(covariateSettings),
                             populationSettingId = names(populationSettings)
                             )
 
 # extract the model names to save
 modelSettingNames <- unlist(lapply(models, function(x) x$name))
 settingTable <- merge(settingTable, 
                           data.frame(modelSettingId = 1:length(modelSettingNames),
                                      modelSettingName = modelSettingNames),
                           all.x = T, by= 'modelSettingId')
 
 # extract time-at-risk from population settings
 tarSetting <- do.call(rbind,(lapply(populationSettings, 
                                     function(x) c(x$addExposureDaysToStart, x$riskWindowStart,x$addExposureDaysToEnd,x$riskWindowEnd ))))
 tarSetting <- as.data.frame(tarSetting)
 colnames(tarSetting) <- c('addExposureDaysToStart','riskWindowStart','addExposureDaysToEnd','riskWindowEnd' )
 tarSetting$populationSettingId <- 1:nrow(tarSetting)
 settingTable <- merge(settingTable,tarSetting,
                       all.x = T, by= 'populationSettingId')

 settingTable$lookupId <- 1:nrow(settingTable)
 
 return(list(settingLookupTable=settingTable,
             models = models,
             covariateSettings = covariateSettings,
             populationSettings = populationSettings))
 
}

#' combine two objects specifying multiple Plp model settings
#'
#' @details
#' Takes two output of running createPlpModelSettings() and combined them
#' @param plpModelSetting1               A combination of model, covariate and population settings
#' @param plpModelSetting2               A combination of model, covariate and population settings
#' 
#' @return
#' A list containing a dataframe settingLookupTable containing all the model, covariate and popualtion combination details, 
#' a list models containing all the model settings, a list covariateSettings containing all the covariate settings and a list 
#' populationSettings containing all the population settings.
#' @export
combinePlpModelSettings <- function(plpModelSetting1, plpModelSetting2){
  
  settingLookupTable1 <- plpModelSetting1$settingLookupTable
  settingLookupTable2 <- plpModelSetting2$settingLookupTable
  
  settingLookupTable2$lookupId <- settingLookupTable2$lookupId + max(settingLookupTable1$lookupId)
  settingLookupTable2$modelSettingId <- settingLookupTable2$modelSettingId + max(settingLookupTable1$modelSettingId)
  settingLookupTable2$covariateSettingId <- settingLookupTable2$covariateSettingId + max(settingLookupTable1$covariateSettingId)
  settingLookupTable2$populationSettingId <- settingLookupTable2$populationSettingId + max(settingLookupTable1$populationSettingId)
  
  settingLookupTable <- rbind(settingLookupTable1,
                              settingLookupTable2)
  
  models1 <- plpModelSetting1$models
  models2 <- plpModelSetting2$models
  names(models2) <- names(models2) + max(settingLookupTable1$modelSettingId)
  models <- c(models1, models2)
  
  covariateSettings1 <- plpModelSetting1$covariateSettings
  covariateSettings2 <- plpModelSetting2$covariateSettings
  names(covariateSettings2) <- names(covariateSettings2) + max(settingLookupTable1$covariateSettingId)
  covariateSettings <- c(covariateSettings1,covariateSettings2)
  
  
  populationSettings1 <- plpModelSetting1$populationSettings
  populationSettings2 <- plpModelSetting2$populationSettings
  names(populationSettings2) <- names(populationSettings2) + max(settingLookupTable1$populationSettingId)
  populationSettings <- c(populationSettings1,populationSettings2)
  
  result <- list(settingLookupTable = settingLookupTable,
                 models = models,
                 covariateSettings = covariateSettings,
                 populationSettings = populationSettings)
  
  return(result)
}

#' create the study population settings
#'
#' @details
#' Takes as input the inputs to create study population
#' @param binary                Forces the outcomeCount to be 0 or 1 (use for binary prediction problems)                              
#' @param includeAllOutcomes    (binary) indicating whether to include people with outcomes who are not observed for the whole at risk period
#' @param firstExposureOnly            Should only the first exposure per subject be included? Note that
#'                                     this is typically done in the \code{createStudyPopulation} function,
#' @param washoutPeriod                The mininum required continuous observation time prior to index
#'                                     date for a person to be included in the cohort.
#' @param removeSubjectsWithPriorOutcome  Remove subjects that have the outcome prior to the risk window start?
#' @param priorOutcomeLookback            How many days should we look back when identifying prior outcomes?
#' @param requireTimeAtRisk      Should subject without time at risk be removed?
#' @param minTimeAtRisk          The minimum number of days at risk required to be included
#' @param riskWindowStart        The start of the risk window (in days) relative to the index date (+
#'                               days of exposure if the \code{addExposureDaysToStart} parameter is
#'                               specified).
#' @param addExposureDaysToStart   Add the length of exposure the start of the risk window?
#' @param riskWindowEnd          The end of the risk window (in days) relative to the index data (+
#'                               days of exposure if the \code{addExposureDaysToEnd} parameter is
#'                               specified).
#' @param addExposureDaysToEnd   Add the length of exposure the risk window?
#' @param verbosity              Sets the level of the verbosity. If the log level is at or higher in priority than the logger threshold, a message will print. The levels are:
#'                               \itemize{
#'                               \item{DEBUG}{Highest verbosity showing all debug statements}
#'                               \item{TRACE}{Showing information about start and end of steps}
#'                               \item{INFO}{Show informative information (Default)}
#'                               \item{WARN}{Show warning messages}
#'                               \item{ERROR}{Show error messages}
#'                               \item{FATAL}{Be silent except for fatal errors} 
#'                               }#' 
#' @return
#' A list containing all the settings required for creating the study population
#' @export
createStudyPopulationSettings <- function(binary = T,
                                          includeAllOutcomes = T,
                                          firstExposureOnly = FALSE,
                                          washoutPeriod = 0,
                                          removeSubjectsWithPriorOutcome = TRUE,
                                          priorOutcomeLookback = 99999,
                                          requireTimeAtRisk = T,
                                          minTimeAtRisk=364,
                                          riskWindowStart = 1,
                                          addExposureDaysToStart = FALSE,
                                          riskWindowEnd = 365,
                                          addExposureDaysToEnd = F,
                                          verbosity = "INFO"){
  
  result <- list(binary = binary,
              includeAllOutcomes = includeAllOutcomes,
              firstExposureOnly = firstExposureOnly,
              washoutPeriod = washoutPeriod,
              removeSubjectsWithPriorOutcome = removeSubjectsWithPriorOutcome,
              priorOutcomeLookback = priorOutcomeLookback,
              requireTimeAtRisk = requireTimeAtRisk, 
              minTimeAtRisk = minTimeAtRisk,
              riskWindowStart = riskWindowStart,
              addExposureDaysToStart = addExposureDaysToStart,
              riskWindowEnd = riskWindowEnd,
              addExposureDaysToEnd = addExposureDaysToEnd, 
              verbosity = verbosity)
  
  class(result) <- 'populationSettings'
  return(result)
  
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
#' @param outputLocation                The location to save to validation results
#' @param connectionDetails                The connection details for extracting the new data 
#' @param validationSchemaTarget         A string or list of strings specifying the database containing the target cohorts
#' @param validationSchemaOutcome       A string or list of strings specifying the database containing the outcome cohorts
#' @param validationSchemaCdm            A string or list of strings specifying the database containing the cdm
#' @param databaseNames                  A string of lift of strings specifying sharing friendly database names corresponding to validationSchemaCdm
#' @param validationTableTarget          A string or list of strings specifying the table containing the target cohorts
#' @param validationTableOutcome        A string or list of strings specifying the table containing the outcome cohorts
#' @param validationIdTarget             An iteger or list of integers specifying the cohort id for the target cohorts
#' @param validationIdOutcome           An iteger or list of integers specifying the cohort id for the outcome cohorts
#' @param oracleTempSchema                 The temp oracle schema requires read/write 
#' @param verbosity                        Sets the level of the verbosity. If the log level is at or higher in priority than the logger threshold, a message will print. The levels are:
#'                                         \itemize{
#'                                         \item{DEBUG}{Highest verbosity showing all debug statements}
#'                                         \item{TRACE}{Showing information about start and end of steps}
#'                                         \item{INFO}{Show informative information (Default)}
#'                                         \item{WARN}{Show warning messages}
#'                                         \item{ERROR}{Show error messages}
#'                                         \item{FATAL}{Be silent except for fatal errors}
#'                                         }
#' @param keepPrediction                   Whether to keep the predicitons for the new data                                         
#' @param sampleSize                       If not NULL, the number of people to sample from the target cohort
#' 
#' @export 
evaluateMultiplePlp <- function(analysesLocation,
                                outputLocation,
                                connectionDetails, 
                                validationSchemaTarget,
                                validationSchemaOutcome,
                                validationSchemaCdm, 
                                databaseNames,
                                validationTableTarget,
                                validationTableOutcome,
                                validationIdTarget = NULL,
                                validationIdOutcome = NULL,
                                oracleTempSchema = NULL,
                                verbosity = 'INFO',
                                keepPrediction = F,
                                sampleSize = NULL){
  
  clearLoggerType("Multple Evaluate PLP Log")
  if(!dir.exists(outputLocation)){dir.create(outputLocation,recursive=T)}
  logFileName = paste0(outputLocation,'/plplog.txt')
  logger <- ParallelLogger::createLogger(name = "Multple Evaluate PLP Log",
                                      threshold = verbosity,
                                      appenders = list(ParallelLogger::createFileAppender(layout = ParallelLogger::layoutParallel,
                                                                                       fileName = logFileName)))
  ParallelLogger::registerLogger(logger)
  
  if(missing(databaseNames)){
    stop('Need to put a shareable name/s for the database/s')
  }

  # for each model run externalValidatePlp()
  modelSettings <- dir(analysesLocation, recursive = F, full.names = T)
  
  # now fine all analysis folders..
  modelSettings <- modelSettings[grep('Analysis_',modelSettings)]
  
  for(i in 1:length(modelSettings)){
    
    ParallelLogger::logInfo(paste0('Evaluating model in ',modelSettings[i] ))
    
    if(dir.exists(file.path(modelSettings[i],'plpResult'))){
      ParallelLogger::logInfo(paste0('plpResult found in ',modelSettings[i] ))
      
      plpResult <- loadPlpResult(file.path(modelSettings[i],'plpResult'))
      
      validations <-   tryCatch(externalValidatePlp(plpResult = plpResult,
                                                    connectionDetails = connectionDetails, 
                                                    validationSchemaTarget = validationSchemaTarget,
                                                    validationSchemaOutcome = validationSchemaOutcome, 
                                                    validationSchemaCdm = validationSchemaCdm, 
                                                    databaseNames = databaseNames,
                                                    validationTableTarget = validationTableTarget, 
                                                    validationTableOutcome = validationTableOutcome,
                                                    validationIdTarget = validationIdTarget, 
                                                    validationIdOutcome = validationIdOutcome,
                                                    oracleTempSchema = oracleTempSchema,
                                                    verbosity = verbosity, 
                                                    keepPrediction = keepPrediction,
                                                    sampleSize=sampleSize),
                                error = function(cont){ParallelLogger::logInfo(paste0('Error: ',cont ))
                                  ;return(NULL)})
      
      if(!is.null(validations)){
        if(length(validations$validation)>1){
          for(j in 1:length(validations$validation)){
            saveName <- file.path(outputLocation, databaseNames[j], paste0(plpResult$analysisRef$analysisId))
            if(!dir.exists(saveName)){
              dir.create(saveName, recursive = T)
            }
            ParallelLogger::logInfo(paste0('Evaluation result save in ',file.path(saveName,'validationResult.rds') ))
            
            saveRDS(validations$validation[[j]], file.path(saveName,'validationResult.rds'))
            
          }
        } else {
          saveName <- file.path(outputLocation, databaseNames,paste0(plpResult$analysisRef$analysisId))
          if(!dir.exists(saveName)){
            dir.create(saveName, recursive = T)
          }
          ParallelLogger::logInfo(paste0('Evaluation result save in ',file.path(saveName,'validationResult.rds') ))
          saveRDS(validations$validation[[1]], file.path(saveName,'validationResult.rds'))
        }
      }
    }
  }
  
}




#' Load the multiple prediction json settings from a file
#'
#' @details
#' This function interprets a json with the multiple prediction settings and creates a list 
#' that can be combined with connection settings to run a multiple prediction study
#' 
#' @param predictionAnalysisListFile    The prediciton specification json extracted from atlas.  
#'                                      
#' @examples
#' \dontrun{
#' predictionAnalysisList <- loadPredictionAnalysisList('./predictionStudyAnalyses.json')
#' predictionAnalysisList$connectionDetails = connectionDetails
#' predictionAnalysisList$cdmDatabaseSchema = cdmDatabaseSchema
#' predictionAnalysisList$cdmDatabaseName = cdmDatabaseName
#' predictionAnalysisList$oracleTempSchema = oracleTempSchema
#' predictionAnalysisList$cohortDatabaseSchema = cohortDatabaseSchema
#' predictionAnalysisList$cohortTable = cohortTable
#' predictionAnalysisList$outcomeDatabaseSchema = outcomeDatabaseSchema
#' predictionAnalysisList$outcomeTable = outcomeTable
#' predictionAnalysisList$cdmVersion = cdmVersion
#' predictionAnalysisList$outputFolder = outputFolder
#' result <- do.call(runPlpAnalyses, predictionAnalysisList)
#' }
#'
#' @export
loadPredictionAnalysisList <- function(predictionAnalysisListFile){
  # load the json file and parse into prediction list
  json <- tryCatch({ParallelLogger::loadSettingsFromJson(file=predictionAnalysisListFile)},
                   error=function(cond) {
                     stop('Issue with json file...')
                   })

  modelList <- list()
  length(modelList) <-  length(json$modelSettings)
  for(i in 1:length(json$modelSettings)){
    name <- names(json$modelSettings[[i]])
    modelList[[i]] <- do.call(get(paste0('set',gsub('Settings','',name)), envir = environment(PatientLevelPrediction::accuracy)), 
                              json$modelSettings[[i]][[1]]
    )
  }
  
  # this can be multiple?
  ##covariateSettingList <- lapply(json$covariateSettings, function(x) do.call(FeatureExtraction::createCovariateSettings, x))
  covariateSettingList <- json$covariateSettings
  
  # extract the population settings:
  populationSettingList <- lapply(json$populationSettings, function(x) do.call(PatientLevelPrediction::createStudyPopulationSettings, x))
  
  #create model analysis list:
  modelAnalysisList <- PatientLevelPrediction::createPlpModelSettings(modelList = modelList,
                                                                      covariateSettingList = covariateSettingList, 
                                                                      populationSettingList = populationSettingList
  )
  
  runPlpAnalyses <- list(modelAnalysisList = modelAnalysisList,
                         cohortIds = json$targetIds,
                         cohortNames = getCohortNames(json, json$targetIds),
                         outcomeIds = json$outcomeIds,
                         outcomeNames =  getCohortNames(json, json$outcomeIds),
                         maxSampleSize = json$getPlpDataArgs$maxSampleSize,
                         washoutPeriod = json$getPlpDataArgs$washoutPeriod,
                         minCovariateFraction = json$runPlpArgs$minCovariateFraction,
                         normalizeData = json$runPlpArgs$normalizeData,
                         testSplit = json$runPlpArgs$testSplit,
                         testFraction = json$runPlpArgs$testFraction,
                         splitSeed = json$runPlpArgs$splitSeed,
                         nfold = json$runPlpArgs$nfold#,
                         #verbosity = json$runPlpArgs$verbosity - isnt in atlas
  )
  
  return(runPlpAnalyses)
}

getCohortNames <- function(json, targetIds){
  idNames <- do.call(rbind, lapply(json$cohortDefinitions, function(x) c(x$id, as.character(x$name))))
  colnames(idNames) <- c('id','name')
  idNames <- as.data.frame(idNames)
  name <- c()
  for(tid in targetIds){
    name <- c(name, as.character(idNames$name[idNames$id==tid][1]))
  }
  return(name)
}



#' Saves a json prediction settings given R settings
#'
#' @details
#' This function interprets a json with the multiple prediction settings and creates a list 
#' that can be combined with connection settings to run a multiple prediction study
#' 
#' @param  workFolder    Location to save json specification
#' @param  cohortIds     Vector of target population cohort ids
#' @param  outcomeIds     Vector of outcome cohort ids
#' @param  cohortSettingCsv   The location to the csv containing the cohort details
#' @param  covariateSettingList   A list of covariate settings
#' @param populationSettingList   A list of population settings
#' @param modelSettingList        A list of model settings
#' @param maxSampleSize           If not NULL then max number of target population to sample for model training
#' @param washoutPeriod           Minimum prior observation for each person in target pop to be included
#' @param minCovariateFraction    Minimum covariate fraction to include
#' @param normalizeData           Whether to normalise data
#' @param testSplit               Split by person or time
#' @param testFraction            Fractiuon of data to use for test set
#' @param splitSeed               Seed used in test split
#' @param nfold                   Number of folds used when training model
#'
#' @export
savePredictionAnalysisList <- function(workFolder="inst/settings",
                                       cohortIds,
                                       outcomeIds,
                                       cohortSettingCsv = file.path(workFolder, 'CohortsToCreate.csv'),
                                       covariateSettingList,
                                       populationSettingList,
                                       modelSettingList,
                                       
                                       maxSampleSize= NULL,
                                       washoutPeriod=0,
                                       minCovariateFraction=0,
                                       normalizeData=T,
                                       testSplit='person',
                                       testFraction=0.25,
                                       splitSeed=1,
                                       nfold=3
                                       ){
  
  json <- list()
  json$targetIds <- cohortIds
  json$outcomeIds <- outcomeIds
  
  cohortsToCreate <- utils::read.csv(cohortSettingCsv)
  json$cohortDefinitions <- apply(cohortsToCreate[,c('cohortId','name')], 1, function(x) list(id=x[1], name=x[2]))
  # could extract the cohort json and add expression?
  
  json$getPlpDataArgs <- list(maxSampleSize=maxSampleSize,
                              washoutPeriod=washoutPeriod)
  
  json$runPlpArgs <- list(minCovariateFraction=minCovariateFraction,
                          normalizeData=normalizeData,
                          testSplit=testSplit,
                          testFraction=testFraction,
                          splitSeed=splitSeed,
                          nfold=nfold)#,
                          #verbosity=verbosity) removed due to atlas
  
  json$covariateSettings <- covariateSettingList
  json$populationSettings <- populationSettingList
  
  #format modelSettings
  json$modelSettings <- list()
  if(class(modelSettingList)=='list'){
    length(json$modelSettings) <- length(modelSettingList)
    for( k in 1:length(modelSettingList)){
      modSet <- list()
      if(modelSettingList[[k]]$model%in%c('fitLassoLogisticRegression','fitKNN','fitNaiveBayes')){
        modSet[[1]] <- modelSettingList[[k]]$param
      } else {
        if(class(modelSettingList[[k]]$param)=='data.frame'){modelSettingList[[k]]$param <- split(modelSettingList[[k]]$param, factor(1:nrow(modelSettingList[[k]]$param)))}
        params <- lapply(1:ncol(modelSettingList[[k]]$param[[1]]), function(i) unique(unlist(lapply(modelSettingList[[k]]$param, function(x) x[[i]]))))
        names(params) <- colnames(modelSettingList[[k]]$param[[1]])
        if(params$seed=='NULL'){
          params$seed <- NULL
        }
        modSet[[1]] <- params
      }
      names(modSet) <- paste0(gsub('fit','',modelSettingList[[k]]$model),'Settings')
      json$modelSettings[[k]] <- modSet  
    }
  } else {
    length(json$modelSettings) <- 1
    modSet <- list()
    if(modelSettingList$model%in%c('fitLassoLogisticRegression','fitKNN','fitNaiveBayes')){
      modSet[[1]] <- modelSettingList$param
    } else {
      if(class(modelSettingList$param)=='data.frame'){modelSettingList$param <- split(modelSettingList$param, factor(1:nrow(modelSettingList$param)))}
      params <- lapply(1:ncol(modelSettingList$param[[1]]), function(i) unique(unlist(lapply(modelSettingList$param, function(x) x[[i]]))))
      names(params) <- colnames(modelSettingList$param[[1]])
      if(params$seed=='NULL'){
        params$seed <- NULL
      }
      modSet[[1]] <- params
    }
    names(modSet) <- paste0(gsub('fit','',modelSettingList$model),'Settings')
    json$modelSettings[[1]] <- modSet 
    }
  
  ParallelLogger::saveSettingsToJson(json, file=file.path(workFolder,"predictionAnalysisList.json"))

  return(file.path(workFolder,"predictionAnalysisList.json"))  
}
