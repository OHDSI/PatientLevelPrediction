# @file Diagnostics.R
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

#' Run a list of predictions diagnoses
#'
#' @details
#' This function will run all specified prediction design diagnoses as defined using . 
#'
#' @param databaseDetails               The database settings created using \code{createDatabaseDetails()}
#' @param modelDesignList                A list of model designs created using \code{createModelDesign()}
#' @param cohortDefinitions               A list of cohort definitions for the target and outcome cohorts
#' @param logSettings                    The setting spexcifying the logging for the analyses created using \code{createLogSettings()}
#' @param saveDirectory                   Name of the folder where all the outputs will written to.
#' 
#' @return
#' A data frame with the following columns: \tabular{ll}{ \verb{analysisId} \tab The unique identifier
#' for a set of analysis choices.\cr \verb{targetId} \tab The ID of the target cohort populations.\cr
#' \verb{outcomeId} \tab The ID of the outcomeId.\cr \verb{dataLocation} \tab The location where the plpData was saved 
#'  \cr \verb{the settings ids} \tab The ids for all other settings used for model development.\cr }
#'
#' @export
diagnoseMultiplePlp <- function(
  databaseDetails = createDatabaseDetails(),
  modelDesignList = list(
    createModelDesign(targetId = 1, outcomeId = 2, modelSettings = setLassoLogisticRegression()), 
    createModelDesign(targetId = 1, outcomeId = 3, modelSettings = setLassoLogisticRegression())
  ),
  cohortDefinitions = NULL,
  logSettings = createLogSettings(
    verbosity = "DEBUG", 
    timeStamp = T, 
    logName = "diagnosePlp Log"
  ),
  saveDirectory = getwd()
){
  
  #input checks
  checkIsClass(databaseDetails, c('databaseDetails'))
  checkIsClass(modelDesignList, c('list', 'modelDesign'))
  checkIsClass(logSettings, 'logSettings')
  checkIsClass(saveDirectory, 'character')
  if(!dir.exists(saveDirectory)){
    dir.create(saveDirectory, recursive = T)
  }
  
  if(is.null(cohortDefinitions)){
    
    cohortIds <- unlist(
      lapply(
        X = 1:length(modelDesignList), 
        FUN = function(i){
          c(
            modelDesignList[[i]]$targetId,
            modelDesignList[[i]]$outcomeId
          )
        }
      )
    )
    
    cohortDefinitions <- lapply(
      X = cohortIds, 
      FUN = function(x){
        list(
          id = x, 
          name = paste0('Cohort: ', x)
        )
      }
    )
    
  }
  
  settingstable <- convertToJson(modelDesignList,cohortDefinitions) # from runMultiplePlp.R
  
  if(nrow(settingstable) != length(modelDesignList)){
    stop('Error in settingstable')
  }
  
  # save the settings: TODO fix
  utils::write.csv(
    x = settingstable %>% dplyr::select(
      "analysisId",
      "targetId", 
      "targetName",
      "outcomeId", 
      "outcomeName",
      "dataLocation"
    ), 
    file.path(saveDirectory,'settings.csv'), 
    row.names = F
  )

  # group the outcomeIds per combination of data extraction settings
  dataSettings <- settingstable %>% 
    dplyr::group_by(
      .data$targetId,
      .data$covariateSettings,
      .data$restrictPlpDataSettings,
      .data$dataLocation
    ) %>% 
    dplyr::summarise(
      outcomeIds = paste(unique(.data$outcomeId), collapse = ',')
    )
  
  # extract data
  for(i in 1:nrow(as.data.frame(dataSettings))){
    dataExists <- length(dir(file.path(saveDirectory, dataSettings$dataLocation[i])))>0
    if(!dataExists){
      ParallelLogger::logInfo(paste('Extracting data for cohort', dataSettings$targetId[i], 'to', file.path(saveDirectory, dataSettings$dataLocation[i])))
      
      databaseDetails$targetId <- dataSettings$targetId[i]
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
      ParallelLogger::logInfo(paste('Data for cohort', dataSettings$targetId[i], 'exists at', file.path(saveDirectory, dataSettings$dataLocation[i])))
    }
  }
  
  # diagnosePlp
  for(i in 1:nrow(as.data.frame(settingstable))){
    modelDesign <- modelDesignList[[i]]
    settings <- settingstable[i,] # just the data locations?
    
    dataExists <- length(dir(file.path(saveDirectory, settings$dataLocation)))>0
    
    if(dataExists){
      plpData <- PatientLevelPrediction::loadPlpData(file.path(saveDirectory, settings$dataLocation))
      
      diagnoseExists <- file.exists(file.path(saveDirectory, settings$analysisId, 'diagnosePlp.rds'))
      if(!diagnoseExists){
        
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
        
        result <- tryCatch(
          {do.call(diagnosePlp, diagnosePlpSettings)},
          error = function(e){ParallelLogger::logInfo(e); return(NULL)}
        )
      } else{
        ParallelLogger::logInfo(paste('Diagnosis ', settings$analysisId, 'exists at', file.path(saveDirectory, settings$analysisId)))
      }
    } # end run per setting
    
  }
  return(invisible(settingstable))
}


#' diagnostic - Investigates the prediction problem settings - use before training a model
#'
#' @description
#' This function runs a set of prediction diagnoses to help pick a suitable T, O, TAR and determine 
#' whether the prediction problem is worth executing.
#' 
#' @details
#' Users can define set of Ts, Os, databases and population settings.  A list of data.frames containing details such as
#' follow-up time distribution, time-to-event information, characteriszation details, time from last prior event, 
#' observation time distribution. 
#'
#' @param plpData                    An object of type \code{plpData} - the patient level prediction
#'                                   data extracted from the CDM.  Can also include an initial population as 
#'                                   plpData$popualtion.
#' @param outcomeId                  (integer) The ID of the outcome.                                       
#' @param analysisId                 (integer) Identifier for the analysis. It is used to create, e.g., the result folder. Default is a timestamp.
#' @param populationSettings         An object of type \code{populationSettings} created using \code{createStudyPopulationSettings} that
#'                                   specifies how the data class labels are defined and addition any exclusions to apply to the 
#'                                   plpData cohort
#' @param splitSettings              An object of type \code{splitSettings} that specifies how to split the data into train/validation/test.  
#'                                   The default settings can be created using \code{createDefaultSplitSetting}.                               
#' @param sampleSettings             An object of type \code{sampleSettings} that specifies any under/over sampling to be done.
#'                                   The default is none.
#' @param featureEngineeringSettings An object of \code{featureEngineeringSettings} specifying any feature engineering to be learned (using the train data)                                                        
#' @param preprocessSettings         An object of \code{preprocessSettings}. This setting specifies the minimum fraction of 
#'                                   target population who must have a covariate for it to be included in the model training                            
#'                                   and whether to normalise the covariates before training  
#' @param modelSettings              An object of class \code{modelSettings} created using one of the function:
#'                                         \itemize{
#'                                         \item setLassoLogisticRegression() A lasso logistic regression model
#'                                         \item setGradientBoostingMachine() A gradient boosting machine
#'                                         \item setAdaBoost() An ada boost model
#'                                         \item setRandomForest() A random forest model
#'                                         \item setDecisionTree() A decision tree model
#'                                         \item setKNN() A KNN model
#'                                         
#'                                         } 
#' @param logSettings           An object of \code{logSettings} created using \code{createLogSettings} 
#'                              specifying how the logging is done                                                                            
#' @param saveDirectory         The path to the directory where the results will be saved (if NULL uses working directory)
#' 
#' @return
#' An object containing the model or location where the model is save, the data selection settings, the preprocessing
#' and training settings as well as various performance measures obtained by the model.
#'
#' \item{distribution}{list for each O of a data.frame containing: i) Time to observation end distribution, ii) Time from observation start distribution, iii) Time to event distribution and iv) Time from last prior event to index distribution (only for patients in T who have O before index) }
#' \item{incident}{list for each O of incidence of O in T during TAR}
#' \item{characterization}{list for each O of Characterization of T, TnO, Tn~O}
#'
#'
#' @export
#' @examples
#' \dontrun{
#' #******** EXAMPLE 1 ********* 
#' } 
diagnosePlp <- function(
  plpData = NULL,
  outcomeId,
  analysisId,
  populationSettings,
  splitSettings = createDefaultSplitSetting(),
  sampleSettings = createSampleSettings(), # default none
  saveDirectory = NULL,
  featureEngineeringSettings = createFeatureEngineeringSettings(), # default none
  modelSettings = setLassoLogisticRegression(), # default to logistic regression
  logSettings =  createLogSettings(
    verbosity = 'DEBUG',
    timeStamp = T,
    logName = 'diagnosePlp Log'
  ),
  preprocessSettings = createPreprocessSettings()
){
  
  # start log 
  analysisPath <- file.path(saveDirectory, analysisId)
  logSettings$saveDirectory <- analysisPath
  logSettings$logFileName <- 'plpLog'
  logger <- do.call(createLog,logSettings)
  ParallelLogger::registerLogger(logger)
  on.exit(closeLog(logger))
  
  participantsDiag <- probastParticipants(
    plpData, 
    outcomeId = outcomeId,
    populationSettings = populationSettings
  )
  
  predictorDiag <- probastPredictors(
    plpData, 
    outcomeId = outcomeId,
    populationSettings = populationSettings
  )
  
  outcomeDiag <- probastOutcome(
    plpData, 
    outcomeId = outcomeId,
    populationSettings = populationSettings
  )
  
  designDiag <- probastDesign(
    plpData, 
    outcomeId = outcomeId,
    populationSettings = populationSettings
  )
  
  # Question: what about 
  # splitSettings, sampleSettings, 
  # FeatureEngineeringSettings, modelSettings
  
  result <- list(
    summary = rbind(
      participantsDiag$diagnosticParticipantsAggregate,
      predictorDiag$diagnosticPredictorsAggregate,
      outcomeDiag$diagnosticOutcomeAggregate,
      designDiag$diagnosticDesignAggregate
    ),
    participants = participantsDiag$diagnosticParticipantsFull,
    predictors = predictorDiag$diagnosticPredictorsFull,
    outcomes = outcomeDiag$diagnosticOutcomeFull,
    designs = designDiag$diagnosticDesignFull,
    modelDesign = PatientLevelPrediction::createModelDesign(
      targetId = attr(plpData$cohorts, "metaData")$targetId,
      outcomeId = outcomeId,
      restrictPlpDataSettings = plpData$metaData$restrictPlpDataSettings,
      covariateSettings = plpData$metaData$covariateSettings,
      populationSettings = populationSettings,
      featureEngineeringSettings = featureEngineeringSettings,
      preprocessSettings = preprocessSettings,
      modelSettings = modelSettings,
      splitSettings = splitSettings,
      sampleSettings = sampleSettings
    ),
    databaseSchema = plpData$metaData$databaseDetails$cdmDatabaseSchema,
    databaseId = plpData$metaData$databaseDetails$cdmDatabaseId
  )
  
  class(result) <- 'diagnosePlp'
  
  if(!is.null(saveDirectory)){
    if(!dir.exists(file.path(saveDirectory, analysisId))){
      dir.create(file.path(saveDirectory, analysisId), recursive = T)
    }
    saveLocation <- file.path(saveDirectory, analysisId, 'diagnosePlp.rds')
    ParallelLogger::logInfo(paste0('Saving diagnosePlp to ', saveLocation))
    saveRDS(result, saveLocation)
  }
  
  return(result)
}

probastDesign <- function(
  plpData, 
  outcomeId,
  populationSettings
){
  
  diagnosticAggregate <- c()
  diagnosticFull <- c()
  
  population <- PatientLevelPrediction::createStudyPopulation(
    plpData = plpData, 
    outcomeId = outcomeId, 
    populationSettings = populationSettings
  )
  
  probastId <- '4.1'
  if(min(sum(population$outcomeCount > 0),nrow(population) - sum(population$outcomeCount > 0)) >= 1000){
    diagnosticAggregate <- rbind(
      diagnosticAggregate,
      c(probastId, 'Pass')
    )
  }else if((min(sum(population$outcomeCount > 0),nrow(population) - sum(population$outcomeCount > 0)) >= 100)){
    diagnosticAggregate <- rbind(
      diagnosticAggregate,
      c(probastId, 'Unknown')
    )
  } else{
    diagnosticAggregate <- rbind(
      diagnosticAggregate,
      c(probastId, 'Fail')
    )
  }
  
  if(!is.null(dim(diagnosticAggregate))){
    diagnosticAggregate <- as.data.frame(diagnosticAggregate) %>% 
      dplyr::mutate_if(is.factor, as.character)
    colnames(diagnosticAggregate) <- c('probastId','resultValue')
  }
  
  return(
    list
    (
      diagnosticDesignFull = diagnosticFull,
      diagnosticDesignAggregate = diagnosticAggregate
    )
  )
  
  
}

probastParticipants <- function(
  plpData, 
  outcomeId,
  populationSettings
){
  
  diagnosticAggregate <- c()
  diagnosticFull <- c()
  
  # true due to plp
  probastId <- '1.1'
  diagnosticAggregate <- rbind(
    diagnosticAggregate,
    c(probastId, 'Pass')
  )
  
  # appropriate inclusions
  ## 1.2.1 min prior observation
  if(populationSettings$washoutPeriod != 0 ){
    var <- 'washoutPeriod'
    probastId <- '1.2.1'
    
    result <- getDiagnostic(
      probastId,
      plpData, 
      outcomeId, 
      populationSettings, 
      var, 
      0  
    )
    
    diagnosticAggregate <- rbind(
      diagnosticAggregate,
      result$diagnosticAggregate
    )
    
    diagnosticFull <- rbind(
      diagnosticFull,
      result$diagnosticFull
    )
    
  }
  
  ## 1.2.2 min time-at-risk
  if(populationSettings$requireTimeAtRisk & populationSettings$minTimeAtRisk > 0){
    probastId <- '1.2.2'
    var <- 'minTimeAtRisk'
    
    result <- getDiagnostic(
      probastId,
      plpData, 
      outcomeId, 
      populationSettings, 
      var, 
      0  
    )
    
    diagnosticAggregate <- rbind(
      diagnosticAggregate,
      result$diagnosticAggregate
    )
    
    diagnosticFull <- rbind(
      diagnosticFull,
      result$diagnosticFull
    )
    
  }
  
  
  ## 1.2.3 first exposure only
  if(populationSettings$firstExposureOnly){
    probastId <- '1.2.3'
    var <- 'firstExposureOnly'
    default <- F
    
    result <- getDiagnostic(
      probastId,
      plpData, 
      outcomeId, 
      populationSettings, 
      var, 
      default  
    )
    
    diagnosticAggregate <- rbind(
      diagnosticAggregate,
      result$diagnosticAggregate
    )
    
    diagnosticFull <- rbind(
      diagnosticFull,
      result$diagnosticFull
    )
    
  }
  
  ## 1.2.4 prior observation 
  if(populationSettings$removeSubjectsWithPriorOutcome & populationSettings$priorOutcomeLookback > 0){
    probastId <- '1.2.4'
    var <- 'priorOutcomeLookback'
    default <- 0
    
    result <- getDiagnostic(
      probastId,
      plpData, 
      outcomeId, 
      populationSettings, 
      var, 
      default  
    )
    
    diagnosticAggregate <- rbind(
      diagnosticAggregate,
      result$diagnosticAggregate
    )
    
    diagnosticFull <- rbind(
      diagnosticFull,
      result$diagnosticFull
    )
    
  }
  
  if(!is.null(dim(diagnosticAggregate))){
    diagnosticAggregate <- as.data.frame(diagnosticAggregate) %>% 
      dplyr::mutate_if(is.factor, as.character)
    colnames(diagnosticAggregate) <- c('probastId','resultValue')
  }
  
  return(
    list
    (
      diagnosticParticipantsFull = diagnosticFull,
      diagnosticParticipantsAggregate = diagnosticAggregate
    )
  )
}


getMaxEndDaysFromCovariates <- function(covariateSettings){
  
  if(inherits(covariateSettings, 'covariateSettings')){
    covariateSettings <- list(covariateSettings)
  }
  
  vals <- unlist(lapply(covariateSettings, function(x){x$endDays}))
  
  if(length(vals) == 0){
    return(0)
  } else{
    return(max(vals))
  }
}



probastPredictors <- function(
  plpData, 
  outcomeId,
  populationSettings
){
  
  diagnosticAggregate <- c()
  diagnosticFull <- c()
  
  # true due to plp
  probastId <- '2.1'
  diagnosticAggregate <- rbind(
    diagnosticAggregate,
    c(probastId, 'Pass')
  )
  
  # 2.2.1
  # cov end date < tar_start
  # covariate + outcome correlation; km of outcome (close to index or not)?
  probastId <- '2.2'
  if(populationSettings$startAnchor == 'cohort start'){
    if(populationSettings$riskWindowStart > getMaxEndDaysFromCovariates(plpData$metaData$covariateSettings)){
      diagnosticAggregate <- rbind(
        diagnosticAggregate,
        c(probastId, 'Pass')
      )
    } else{
      diagnosticAggregate <- rbind(
        diagnosticAggregate,
        c(probastId, 'Fail')
      )
    }
  } else{
    diagnosticAggregate <- rbind(
      diagnosticAggregate,
      c(probastId, 'Unknown')
    )
  }
  
  # KM of outcome
  populationSettingsFull <- populationSettings
  populationSettingsFull$riskWindowEnd <- 10*365
  
  population <- PatientLevelPrediction::createStudyPopulation(
    plpData = plpData, 
    outcomeId = outcomeId, 
    populationSettings = populationSettings
  )
  
  populationFull <- PatientLevelPrediction::createStudyPopulation(
    plpData = plpData, 
    outcomeId = outcomeId, 
    populationSettings = populationSettingsFull
  )
  
  #dayOfEvent, outcomeAtTime, observedAtStartOfDay
  kmObservation <- population %>% 
    dplyr::group_by(.data$daysToEvent) %>% 
    dplyr::summarise(
      outcomeAtTime = sum(!is.na(.data$daysToEvent))
    )
  
  kmObservation$observedAtStartOfDay <-  unlist(
    lapply(
      kmObservation$daysToEvent,
      function(x){
        population %>% 
          dplyr::filter(.data$survivalTime >= x) %>%
          dplyr::tally() %>% 
          dplyr::select("n")
      }
    )
  )
  
  kmObservation$probastId <- probastId
  kmObservation$inputType <- 'populationSettings'
  
  kmObservationFull <- populationFull %>% 
    dplyr::group_by(.data$daysToEvent) %>% 
    dplyr::summarise(
      outcomeAtTime = sum(!is.na(.data$daysToEvent))
    )
  
  kmObservationFull$observedAtStartOfDay <-  unlist(
    lapply(
      kmObservationFull$daysToEvent,
      function(x){
        populationFull %>% 
          dplyr::filter(.data$survivalTime >= x) %>%
          dplyr::tally() %>% 
          dplyr::select("n")
      }
    )
  )
  
  kmObservationFull$probastId <- probastId
  kmObservationFull$inputType <- '10-year'
  
  diagnosticFull <- rbind(kmObservation, kmObservationFull)
  
  
  # 2.3.1
  # cov end_date <=0
  probastId <- '2.3'
  if(getMaxEndDaysFromCovariates(plpData$metaData$covariateSettings) <= 0){
    
    if(getMaxEndDaysFromCovariates(plpData$metaData$covariateSettings) < 0){
      diagnosticAggregate <- rbind(
        diagnosticAggregate,
        c(probastId, 'Pass')
      )
    }
    else{
      diagnosticAggregate <- rbind(
        diagnosticAggregate,
        c(probastId, 'Unknown')
      )
    }
  } else{
    diagnosticAggregate <- rbind(
      diagnosticAggregate,
      c(probastId, 'Fail')
    )
  }
  
  if(!is.null(dim(diagnosticAggregate))){
    diagnosticAggregate <- as.data.frame(diagnosticAggregate) %>% 
      dplyr::mutate_if(is.factor, as.character)
    colnames(diagnosticAggregate) <- c('probastId','resultValue')
  }
  
  return(
    list
    (
      diagnosticPredictorsFull = diagnosticFull,
      diagnosticPredictorsAggregate = diagnosticAggregate
    )
  )
  
}



probastOutcome <- function(
  plpData, 
  outcomeId,
  populationSettings
){
  
  diagnosticAggregate <- c()
  diagnosticFull <- c()
  
  # true due to plp
  probastId <- '3.4'
  diagnosticAggregate <- rbind(
    diagnosticAggregate,
    c(probastId, 'Pass')
  )
  
  # 3.5 - check the outcome definition doesn't use things before index?
  
  # 3.6 - check tar after covariate end_days
  probastId <- '3.6'
  if(populationSettings$startAnchor == 'cohort start'){
    if(populationSettings$riskWindowStart > getMaxEndDaysFromCovariates(plpData$metaData$covariateSettings)){
      diagnosticAggregate <- rbind(
        diagnosticAggregate,
        c(probastId, 'Pass')
      )
    } else{
      diagnosticAggregate <- rbind(
        diagnosticAggregate,
        c(probastId, 'Fail')
      )
    }
  } else{
    diagnosticAggregate <- rbind(
      diagnosticAggregate,
      c(probastId, 'Unknown')
    )
  }
  
  # 3.1.1 - check the outcome rate per gender/age/index year
  probastId <- '3.1.1'
  
  # all cohort vs pop
  pop <- PatientLevelPrediction::createStudyPopulation(
    plpData = plpData, 
    outcomeId = outcomeId, 
    populationSettings = populationSettings
  )
  
  popSum <- getOutcomeSummary(
    type = 'population',
    population = pop
  )
  
  cohort <- PatientLevelPrediction::createStudyPopulation(
    plpData = plpData, 
    outcomeId = outcomeId, 
    populationSettings = PatientLevelPrediction::createStudyPopulationSettings(
      includeAllOutcomes = F, 
      firstExposureOnly = F, 
      washoutPeriod = 0, 
      removeSubjectsWithPriorOutcome = F, 
      priorOutcomeLookback = 0, 
      requireTimeAtRisk = F, 
      minTimeAtRisk = 0, 
      riskWindowStart = populationSettings$riskWindowStart, 
      startAnchor = populationSettings$startAnchor, 
      riskWindowEnd = populationSettings$riskWindowEnd, 
      endAnchor = populationSettings$endAnchor
    )
  )
  
  cohortSum <- getOutcomeSummary(
    type = 'cohort',
    population = cohort
  )
  
  
  
  #dayOfEvent, outcomeAtTime, observedAtStartOfDay
  diagnosticFull <- rbind(
    do.call(rbind, popSum),
    do.call(rbind, cohortSum)
  )
  
  if(!is.null(dim(diagnosticAggregate))){
    diagnosticAggregate <- as.data.frame(diagnosticAggregate) %>% 
      dplyr::mutate_if(is.factor, as.character)
    colnames(diagnosticAggregate) <- c('probastId','resultValue')
  }
  
  return(
    list
    (
      diagnosticOutcomeFull = diagnosticFull,
      diagnosticOutcomeAggregate = diagnosticAggregate
    )
  )
  
}

getOutcomeSummary <- function(
  type = 'population',
  population
){
  
  res <- list()
  length(res) <- 4
  probastId <- '3'
  
  res[[1]] <- population %>% 
    dplyr::group_by(.data$ageYear) %>%
    dplyr::summarise(outcomePercent = sum(.data$outcomeCount>0)/length(.data$outcomeCount)) %>%
    dplyr::mutate(
      probastId = probastId,
      aggregation = 'age',
      inputType = type
    ) %>% 
    dplyr::rename(xvalue = "ageYear")
  
  res[[2]] <-  population %>% 
    dplyr::group_by(.data$gender) %>%
    dplyr::summarise(outcomePercent = sum(.data$outcomeCount>0)/length(.data$outcomeCount)) %>%
    dplyr::mutate(
      probastId = probastId,
      aggregation = 'gender',
      inputType = type
    )%>% 
    dplyr::rename(xvalue = "gender")
  
  res[[3]] <- population %>% 
    dplyr::mutate(
      year = substring(.data$cohortStartDate,1,4)
    ) %>%
    dplyr::group_by(.data$year) %>%
    dplyr::summarise(outcomePercent = sum(.data$outcomeCount>0)/length(.data$outcomeCount)) %>%
    dplyr::mutate(
      probastId = probastId,
      aggregation = 'year',
      inputType = type
    ) %>% 
    dplyr::rename(xvalue = "year")
  
  res[[4]] <- population %>% 
    dplyr::mutate(
      year = substring(.data$cohortStartDate,6,7)
    ) %>%
    dplyr::group_by(.data$year) %>%
    dplyr::summarise(outcomePercent = sum(.data$outcomeCount>0)/length(.data$outcomeCount)) %>%
    dplyr::mutate(
      probastId = probastId,
      aggregation = 'month',
      inputType = type
    ) %>% 
    dplyr::rename(xvalue = "year")
  
  return(res)
}

cos_sim <- function(a,b) 
{
  return( sum(a*b)/sqrt(sum(a^2)*sum(b^2)) )
} 

getDiagnostic <- function(
  probastId,
  plpData, 
  outcomeId,
  populationSettings, 
  var,
  defaultValue = 0
){
  ParallelLogger::logInfo(paste0('Diagnosing impact of ',var,' in populationSettings'))
  
  populationSettingsCheck <- populationSettings
  populationSettingsCheck[var] <- defaultValue
  
  pop <- PatientLevelPrediction::createStudyPopulation(
    plpData = plpData, 
    outcomeId = outcomeId, 
    populationSettings = populationSettings
  )
  
  popCheck <- PatientLevelPrediction::createStudyPopulation(
    plpData = plpData, 
    outcomeId = outcomeId, 
    populationSettings = populationSettingsCheck 
  )
  
  #compare the populations:
  diag <- rbind(
    data.frame(
      probastId = probastId,
      design = paste0(var,': ', defaultValue),
      metric = c(
        'N', 'outcomePercent', 'minAge',
        'meanAge', 'medianAge', 'maxAge',
        'malePercent'
      ),
      value = c(
        nrow(popCheck), sum(popCheck$outcomeCount>0)/nrow(popCheck)*100, min(popCheck$ageYear),
        mean(popCheck$ageYear), stats::median(popCheck$ageYear), max(popCheck$ageYear),
        sum(popCheck$gender == 8507)/nrow(popCheck)*100
      )
    ),
    data.frame(
      probastId = probastId,
      design = paste0(var,': ',populationSettings[var]),
      metric = c(
        'N', 'outcomePercent', 'minAge',
        'meanAge', 'medianAge', 'maxAge',
        'malePercent'
      ),
      value = c(
        nrow(pop), sum(pop$outcomeCount>0)/nrow(pop)*100, min(pop$ageYear),
        mean(pop$ageYear), stats::median(pop$ageYear), max(pop$ageYear),
        sum(pop$gender == 8507)/nrow(pop)*100
      )
    )
  )
  
  diagSim <- cos_sim(
    diag %>% 
      dplyr::filter(.data$design == unique(diag$design)[1]) %>%
      dplyr::filter(.data$metric != 'N') %>%
      dplyr::arrange(.data$metric) %>%
      dplyr::select("value")
    , 
    diag %>% 
      dplyr::filter(.data$design == unique(diag$design)[2]) %>%
      dplyr::filter(.data$metric != 'N') %>%
      dplyr::arrange(.data$metric) %>%
      dplyr::select("value")
  )
  
  
  return(
    list(
      diagnosticAggregate = c(
        probastId, 
        diagSim
      ),
      diagnosticFull = diag
    )
  )
}
  
