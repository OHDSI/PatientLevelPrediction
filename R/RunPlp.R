# @file RunPlp.R
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

#' runPlp - Develop and internally evaluate a model using specified settings
#'
#' @description
#' This provides a general framework for training patient level prediction models.  The user can select 
#' various default feature selection methods or incorporate their own,  The user can also select from
#' a range of default classifiers or incorporate their own.  There are three types of evaluations for the model
#' patient (randomly splits people into train/validation sets) or year (randomly splits data into train/validation sets
#' based on index year - older in training, newer in validation) or both (same as year spliting but checks there are
#' no overlaps in patients within training set and validaiton set - any overlaps are removed from validation set)
#' 
#' @details
#' This function takes as input the plpData extracted from an OMOP CDM database and follows the specified settings to
#' develop and internally validate a model for the specified outcomeId.
#'
#' @param plpData                    An object of type \code{plpData} - the patient level prediction
#'                                   data extracted from the CDM.
#' @param outcomeId                  (integer) The ID of the outcome.                                       
#' @param analysisId                 (integer) Identifier for the analysis. It is used to create, e.g., the result folder. Default is a timestamp.
#' @param analysisName               (character) Name for the analysis
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
#'                                         \item{setLassoLogisticRegression()}{ A lasso logistic regression model}
#'                                         \item{setGradientBoostingMachine()}{ A gradient boosting machine}
#'                                         \item{setAdaBoost()}{ An ada boost model}
#'                                         \item{setRandomForest()}{ A random forest model}
#'                                         \item{setDecisionTree()}{ A decision tree model}
#'                                         \item{setCovNN())}{ A convolutional neural network model}
#'                                         \item{setCIReNN()}{ A recurrent neural network model}
#'                                         \item{setMLP()}{ A neural network model}
#'                                         \item{setDeepNN()}{ A deep neural network model}
#'                                         \item{setKNN()}{ A KNN model}
#'                                         
#'                                         } 
#' @param logSettings                An object of \code{logSettings} created using \code{createLogSettings} 
#'                                   specifying how the logging is done                                                                            
#' @param executeSettings            An object of \code{executeSettings} specifying which parts of the analysis to run
#' 
#'                                                                                 
#' @param saveDirectory         The path to the directory where the results will be saved (if NULL uses working directory)
#' @return
#' An object containing the following:
#'
#'  \itemize{
#'           \item{inputSettings}{A list containing all the settings used to develop the model}
#'           \item{model}{ The developed model of class \code{plpModel}}
#'           \item{executionSummary}{ A list containing the hardward details, R package details and execution time}
#'           \item{performanceEvaluation}{ Various internal performance metrics in sparse format}
#'           \item{prediction}{ The plpData cohort table with the predicted risks added as a column (named value)}
#'           \item{covariateSummary)}{ A characterization of the features for patients with and without the outcome during the time at risk}
#'           \item{analysisRef}{ A list with details about the analysis}
#'           } 
#'
#'
#' @export
#' @examples
#' \dontrun{
#' #******** EXAMPLE 1 ********* 
#' #load plpData:
#' plpData <- loadPlpData(file.path('C:','User','home','data'))
#' 
#' # specify the outcome to predict (the plpData can have multiple outcomes)
#' outcomeId <- 2042
#' 
#' # specify a unique identifier for the analysis
#' analysisId <- 'lrModel'
#' 
#' # create population settings (this defines the labels in the data)
#' #create study population to develop model on
#' #require minimum of 365 days observation prior to at risk start
#' #no prior outcome and person must be observed for 365 after index (minTimeAtRisk)
#' #with risk window from 0 to 365 days after index
#' populationSettings <- createStudyPopulationSettings(plpData,
#'                                     firstExposureOnly = FALSE,
#'                                     washoutPeriod = 365,
#'                                     removeSubjectsWithPriorOutcome = TRUE,
#'                                     priorOutcomeLookback = 99999,
#'                                     requireTimeAtRisk = TRUE,
#'                                     minTimeAtRisk=365,
#'                                     riskWindowStart = 0,
#'                                     addExposureDaysToStart = FALSE,
#'                                     riskWindowEnd = 365,
#'                                     addExposureDaysToEnd = FALSE)
#'                                     
#' # create the split setting by specifying how you want to
#' # partition the data into development (train/validation) and evaluation (test or CV)
#' splitSettings <- createDefaultSplitSetting(testFraction = 0.25, 
#'                                            trainFraction = 0.75, 
#'                                            splitSeed = sample(100000,1), 
#'                                            nfold=3,
#'                                            type = 'stratified')                                   
#'                                     
#'                                     
#' # create the settings specifying any under/over sampling 
#' # in this example we do not do any
#' sampleSettings <- createSampleSettings(type = 'none')  
#' 
#' # specify any feature engineering that will be applied to the train data
#' # in this example we do not do any
#' featureEngineeringSettings <- createFeatureEngineeringSettings(type = 'none')   
#' 
#' # specify whether to use normalization and removal of rare features
#' # preprocessSettings <- ... 
#' 
#' 
#' #lasso logistic regression predicting outcome 200 in cohorts 10 
#' #using no feature selection with a time split evaluation with 30% in test set
#' #70% in train set where the model hyper-parameters are selected using 3-fold cross validation:
#' #and results are saved to file.path('C:','User','home')
#' modelSettingsLR <- setLassoLogisticRegression()
#' 
#' # specify how you want the logging for the analysis
#' # generally this is saved in a file with the results 
#' # but you can define the level of logging 
#' logSettings <- createLogSettings(verbosity = 'DEBUG',
#'                                  timeStamp = T,
#'                                  logName = 'runPlp LR Log')
#'                                  
#' # specify what parts of the analysis to run:
#' # in this example we run everything
#' executeSettings <- createExecuteSettings(runSplitData = T,
#'                                          runSampleData = T,
#'                                          runfeatureEngineering = T,
#'                                          runProcessData = T,
#'                                          runModelDevelopment = T,
#'                                          runCovariateSummary = T)                                        
#' 
#' lrModel <- runPlp(plpData = plpData,
#'                   outcomeId = outcomeId, 
#'                   analysisId = analysisId,
#'                   populationSettings = populationSettings,
#'                   splitSettings = splitSettings,
#'                   sampleSettings = sampleSettings,
#'                   featureEngineeringSettings = featureEngineeringSettings,
#'                   preprocessSettings = preprocessSettings,
#'                   modelSettings = modelSettingsLR,
#'                   logSettings = logSettings
#'                   executeSettings = executeSettings,
#'                   saveDirectory = saveDirectory
#'                   )
#'  
#' #******** EXAMPLE 2 *********                                               
#' # Gradient boosting machine with a grid search to select hyper parameters  
#' # using the test/train/folds created for the lasso logistic regression above                       
#' modelSettingsGBM <- gradientBoostingMachine.set(rsampRate=c(0.5,0.9,1),csampRate=1, 
#'                            ntrees=c(10,100), bal=c(F,T),
#'                            max_depth=c(4,5), learn_rate=c(0.1,0.01))
#'                            
#' analysisId <- 'gbmModel'
#' 
#' gbmModel <- runPlp(plpData = plpData,
#'                   outcomeId = outcomeId, 
#'                   analysisId = analysisId,
#'                   populationSettings = populationSettings,
#'                   splitSettings = splitSettings,
#'                   sampleSettings = sampleSettings,
#'                   featureEngineeringSettings = featureEngineeringSettings,
#'                   preprocessSettings = preprocessSettings,
#'                   modelSettings = modelSettingsGBM,
#'                   logSettings = logSettings
#'                   executeSettings = executeSettings,
#'                   saveDirectory = saveDirectory
#'                   )
#' } 
runPlp <- function(
  plpData,
  outcomeId = plpData$metaData$call$outcomeIds[1],
  analysisId = paste(Sys.Date(), plpData$metaData$call$outcomeIds[1], sep = '-'),
  analysisName = 'Study details',
  populationSettings = createStudyPopulationSettings(),
  splitSettings = createDefaultSplitSetting(
    type = 'stratified', 
    testFraction=0.25, 
    trainFraction = 0.75, 
    splitSeed=123, 
    nfold=3
    ),
  sampleSettings = createSampleSettings(type = 'none'),
  featureEngineeringSettings = createFeatureEngineeringSettings(type = 'none'),
  preprocessSettings = createPreprocessSettings(
    minFraction = 0.001,
    normalize = T
    ),
  modelSettings = setLassoLogisticRegression(),
  logSettings = createLogSettings(
    verbosity = 'DEBUG',
    timeStamp = T,
    logName = 'runPlp Log'
    ),
  executeSettings = createDefaultExecuteSettings(),
  saveDirectory = getwd()
){
  
  # start log 
  analysisPath <- file.path(saveDirectory, analysisId)
  logSettings$saveDirectory <- analysisPath
  logSettings$logFileName <- 'plpLog'
  logger <- do.call(createLog,logSettings)
  ParallelLogger::registerLogger(logger)
  on.exit(closeLog(logger))
  
  #check inputs + print 
  settingsValid <- tryCatch(
    {
      checkInputs(
        inputs = list(
          plpData = plpData, 
          outcomeId = outcomeId,
          populationSettings = populationSettings, 
          splitSettings = splitSettings,
          sampleSettings = sampleSettings,
          featureEngineeringSettings = featureEngineeringSettings, 
          preprocessSettings = preprocessSettings, 
          modelSettings = modelSettings,
          executeSettings = executeSettings
        )
      )
    },
    error = function(e){ParallelLogger::logError(e); return(NULL)}
    )
  
  if(is.null(settingsValid)){
    stop('Settings are invalid - check log for error message')
  }
  
  # log the start time:
  ExecutionDateTime <- Sys.time()
  
  # print the header in the log
  tryCatch({
    printHeader(
      plpData, 
      plpData$metaData$databaseDetails$cohortId, 
      outcomeId, 
      analysisId, 
      analysisName,
      ExecutionDateTime
    )
  })
  
  # create the population
  population <- tryCatch(
    {
      do.call(
        createStudyPopulation, 
        list(
          plpData = plpData,
          outcomeId = outcomeId,
          populationSettings = populationSettings
        )
      )
    },
    error = function(e){ParallelLogger::logError(e); return(NULL)}
  )
  
  if(is.null(population)){
    stop('population NULL')
  }
  
  if(executeSettings$runSplitData){
    # split the data (test/train/cv) + summarise at the end
    data <- tryCatch(
      {
        splitData(
          plpData = plpData,
          population = population,
          splitSettings = splitSettings
        )
      },
      error = function(e){ParallelLogger::logError(e); return(NULL)}
    )
    if(is.null(data)){
      stop('data NULL after splitting')
    }
    
    dataSummary(data)
  } 
  
  if(executeSettings$runSampleData){
    # sampling
    data$Train <- tryCatch(
      {
        sampleData(
          trainData = data$Train, 
          sampleSettings = sampleSettings
        )
      },
      error = function(e){ParallelLogger::logError(e); return(NULL)}
    )
    if(is.null(data$Train)){
      stop('train data NULL after sample')
    }
    dataSummary(data)
  }
  
  if(executeSettings$runfeatureEngineering){
    
    data$Train <- tryCatch(
      {
        featureEngineer(
          data = data$Train, 
          featureEngineeringSettings = featureEngineeringSettings
        )
      },
      error = function(e){ParallelLogger::logError(e); return(NULL)}
    )
    if(is.null(data$Train)){
      stop('train data NULL after feature engineering')
    }
    dataSummary(data)
  }
  
  if(executeSettings$runPreprocessData){
    
    data$Train$covariateData <- tryCatch(
      {
        preprocessData(
          covariateData = data$Train$covariateData, 
          preprocessSettings = preprocessSettings
        )
      },
      error = function(e){ParallelLogger::logError(e); return(NULL)}
    )
    if(is.null(data$Train$covariateData)){
      stop('train data NULL after preprocessing')
    }
    dataSummary(data)
  }
  
  
  model <- NULL
  prediction <- NULL
  performance <- NULL
  if(executeSettings$runModelDevelopment){
    # fit model
    settings <- list(
      trainData = data$Train, 
      modelSettings = modelSettings,
      analysisId = analysisId
    )
    
    ParallelLogger::logInfo(sprintf('Training %s model',settings$modelSettings$name))  
    model <- tryCatch(
      {
        do.call(fitPlp, settings)
      },
      error = function(e) { ParallelLogger::logError(e); return(NULL)}
    )
    
    if(!is.null(model)){
      prediction <- model$prediction
      # remove prediction from model
      model$prediction <- NULL
      
      #apply to test data if exists:
      if('Test' %in% names(data)){
        predictionTest <- tryCatch(
          {
            predictPlp(
              plpModel = model, 
              plpData = data$Test,
              population = data$Test$labels
            )
          },
          error = function(e) { ParallelLogger::logError(e); return(NULL)}
        )
        
        predictionTest$evaluationType <- 'Test'
        
        if(!is.null(predictionTest)){
          prediction <- rbind(predictionTest, prediction[, colnames(prediction)!='index'])
        } 
        
        
      }
      
      # evaluate model
      performance <- tryCatch(
        {
          evaluatePlp(prediction, typeColumn = 'evaluationType')
        },
        error = function(e) { ParallelLogger::logError(e); return(NULL)}
      )
    }
    
  }
  
  
  # covariateSummary
  covariateSummaryResult <- NULL
  if(executeSettings$runCovariateSummary){
    
    if(!is.null(data$Test)){
      strata <- data.frame(
        rowId = c(
          data$Train$labels$rowId, 
          data$Test$labels$rowId 
        ),
        strataName = c(
          rep('Train', nrow(data$Train$labels)), 
          rep('Test', nrow(data$Test$labels))
        )
      )
    } else{
      strata <- data.frame(
        rowId = c( data$Train$labels$rowId ),
        strataName = c( rep('Train', nrow(data$Train$labels)) )
      )
    }
    
    variableImportance <- plpData$covariateData$covariateRef %>% 
      dplyr::mutate(covariateValue = 0) %>% 
      dplyr::select(.data$covariateId, .data$covariateValue) %>% 
      dplyr::collect()
    if(!is.null(model)){
      if(!is.null(model$covariateImportance)){
        variableImportance <- model$covariateImportance %>% dplyr::select(.data$covariateId, .data$covariateValue)
      }
    }
    
    covariateSummaryResult <- do.call(covariateSummary,   
      list(
        covariateData = plpData$covariateData,
        cohort = population %>% dplyr::select(.data$rowId),
        labels = population %>% dplyr::select(.data$rowId, .data$outcomeCount), 
        strata = strata,
        variableImportance = variableImportance,
        featureEngineering = NULL
        )
    )
  
  }
  
  #  ExecutionSummary details:
  # log the end time:
  endTime <- Sys.time()
  TotalExecutionElapsedTime <- difftime(endTime, ExecutionDateTime, units='mins')
  
  executionSummary <- list(
    PackageVersion = list(
      rVersion= R.Version()$version.string,
      packageVersion = utils::packageVersion("PatientLevelPrediction")
    ),
    PlatformDetails= list(
      platform = R.Version()$platform,
      cores = Sys.getenv('NUMBER_OF_PROCESSORS'),
      RAM = memuse::Sys.meminfo()[1]
      ),
    TotalExecutionElapsedTime = TotalExecutionElapsedTime,
    ExecutionDateTime = ExecutionDateTime,
    Log = logSettings$logFileName # location for now
    #Not available at the moment: CDM_SOURCE -  meta-data containing CDM version, release date, vocabulary version
  )
  
  # if model is NULL convert it to list for saving 
  if(is.null(model)){
    model <- list(noModel = T)
    attr(model, "predictionFunction") <- 'noModel'
    attr(model, "saveType") <- 'RtoJson'
    class(model) <- 'plpModel'
  }
  
  results <- list(
    #inputSetting = inputSetting, 
    executionSummary = executionSummary, 
    model = model,
    prediction = prediction,
    performanceEvaluation = performance,
    covariateSummary = covariateSummaryResult,
    analysisRef = list(
      analysisId = analysisId,
      analysisName = analysisName
      )
    )
  class(results) <- c('runPlp')
  
  ParallelLogger::logInfo("Run finished successfully.")
  
  # save the results
  ParallelLogger::logInfo(paste0('Saving PlpResult'))
  tryCatch(savePlpResult(results, file.path(analysisPath,'plpResult')),
    finally= ParallelLogger::logTrace('Done.'))
  ParallelLogger::logInfo(paste0('plpResult saved to ..\\', analysisPath ,'\\plpResult'))
  
  return(results)
  
}


