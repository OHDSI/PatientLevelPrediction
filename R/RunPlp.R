# @file RunPlp.R
#
# Copyright 2025 Observational Health Data Sciences and Informatics
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
#'                                   data extracted from the CDM.  Can also include an initial population as 
#'                                   plpData$popualtion.
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
#'                                         \item setLassoLogisticRegression() A lasso logistic regression model
#'                                         \item setGradientBoostingMachine() A gradient boosting machine
#'                                         \item setAdaBoost() An ada boost model
#'                                         \item setRandomForest() A random forest model
#'                                         \item setDecisionTree() A decision tree model
#'                                         \item setKNN() A KNN model
#'                                         } 
#' @param logSettings                An object of \code{logSettings} created using \code{createLogSettings} 
#'                                   specifying how the logging is done                                                                            
#' @param executeSettings            An object of \code{executeSettings} specifying which parts of the analysis to run
#' 
#'                                                                                 
#' @param saveDirectory         The path to the directory where the results will be saved (if NULL uses working directory)
#' @return
#' An plpResults object containing the following:
#'
#'  \itemize{
#'           \item model The developed model of class \code{plpModel}
#'           \item executionSummary A list containing the hardward details, R package details and execution time
#'           \item performanceEvaluation Various internal performance metrics in sparse format
#'           \item prediction The plpData cohort table with the predicted risks added as a column (named value)
#'           \item covariateSummary A characterization of the features for patients with and without the outcome during the time at risk
#'           \item analysisRef A list with details about the analysis
#'           } 
#' @export
runPlp <- function(
  plpData,
  outcomeId = plpData$metaData$call$outcomeIds[1],
  analysisId = paste(Sys.Date(), plpData$metaData$call$outcomeIds[1], sep = "-"),
  analysisName = "Study details",
  populationSettings = createStudyPopulationSettings(),
  splitSettings = createDefaultSplitSetting(
    type = "stratified", 
    testFraction = 0.25, 
    trainFraction = 0.75, 
    splitSeed = 123, 
    nfold = 3
    ),
  sampleSettings = createSampleSettings(type = "none"),
  featureEngineeringSettings = createFeatureEngineeringSettings(type = "none"),
  preprocessSettings = createPreprocessSettings(
    minFraction = 0.001,
    normalize = TRUE
    ),
  modelSettings = setLassoLogisticRegression(),
  logSettings = createLogSettings(
    verbosity = "DEBUG",
    timeStamp = TRUE,
    logName = "runPlp Log"
    ),
  executeSettings = createDefaultExecuteSettings(),
  saveDirectory = getwd()
) {
  start <- Sys.time()
  
  # start log 
  analysisPath <- file.path(saveDirectory, analysisId)
  logSettings$saveDirectory <- analysisPath
  logSettings$logFileName <- "plpLog"
  logger <- do.call(createLog, logSettings)
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
    error = function(e) {
      ParallelLogger::logError(e)
      return(NULL)
    })
  
  if (is.null(settingsValid)) {
    stop("Settings are invalid - check log for error message")
  }
  
  # log the start time:
  executionDateTime <- Sys.time()
  
  # print the header in the log
  tryCatch({
    printHeader(
      plpData, 
      plpData$metaData$databaseDetails$targetId, 
      outcomeId, 
      analysisId, 
      analysisName,
      executionDateTime
    )
  })
  
  # create the population
  if (!is.null(plpData$population)) {
    ParallelLogger::logInfo("Using existing population")
    population <- plpData$population
  } else {
    ParallelLogger::logInfo("Creating population")
    population <- tryCatch({
      do.call(createStudyPopulation,
              list(plpData = plpData,
                   outcomeId = outcomeId,
                   populationSettings = populationSettings,
                   population = plpData$population
                   )
      )},
    error = function(e) {
        ParallelLogger::logError(e)
        return(NULL)
      }
    )
  }
    
  if (is.null(population)) {
    stop("population NULL")
  }
  
  if (executeSettings$runSplitData) {
    # split the data (test/train/cv) + summarise at the end
    data <- tryCatch(
      {
        splitData(
          plpData = plpData,
          population = population,
          splitSettings = splitSettings
        )
      },
      error = function(e) {
        ParallelLogger::logError(e)
        return(NULL)
      }
    )
    if (is.null(data)) {
      stop("data NULL after splitting")
    }
    
    dataSummary(data)
  } 
  
  if (executeSettings$runSampleData) {
    # sampling
    data$Train <- tryCatch(
      {
        sampleData(
          trainData = data$Train, 
          sampleSettings = sampleSettings
        )
      },
      error = function(e) {
        ParallelLogger::logError(e)
        return(NULL)
      }
    )
    if (is.null(data$Train)) {
      stop("train data NULL after sample")
    }
    dataSummary(data)
  }
  
  if (executeSettings$runfeatureEngineering) {
    
    data$Train <- tryCatch(
      {
        featureEngineer(
          data = data$Train, 
          featureEngineeringSettings = featureEngineeringSettings
        )
      },
      error = function(e) {
        ParallelLogger::logError(e)
        return(NULL)
      }
    )
    if (is.null(data$Train)) {
      stop("train data NULL after feature engineering")
    }
    dataSummary(data)
  }
  
  if (executeSettings$runPreprocessData) {
    
    data$Train$covariateData <- tryCatch(
      {
        preprocessData(
          covariateData = data$Train$covariateData, 
          preprocessSettings = preprocessSettings
        )
      },
      error = function(e) {
        ParallelLogger::logError(e)
        return(NULL)
      }
    )
    if (is.null(data$Train$covariateData)) {
      stop("train data NULL after preprocessing")
    }
    dataSummary(data)
  }
  
  
  model <- NULL
  prediction <- NULL
  performance <- NULL
  if (executeSettings$runModelDevelopment) {
    # fit model
    settings <- list(
      trainData = data$Train, 
      modelSettings = modelSettings,
      analysisId = analysisId,
      analysisPath = analysisPath
    )
    
    ParallelLogger::logInfo(sprintf("Training %s model",
      settings$modelSettings$name))  

    model <- tryCatch(
      {
        do.call(fitPlp, settings)
      },
      error = function(e) {
        ParallelLogger::logError(e)
        return(NULL)
      }
    )
    
    if (!is.null(model)) {
      prediction <- model$prediction
      # remove prediction from model
      model$prediction <- NULL
      
      #apply to test data if exists:
      if ("Test" %in% names(data)) {
        predictionTest <- tryCatch(
          {
            predictPlp(
              plpModel = model, 
              plpData = data$Test,
              population = data$Test$labels
            )
          },
          error = function(e) {
            ParallelLogger::logError(e)
            return(NULL)
          }
        )
        
        predictionTest$evaluationType <- "Test"
        
        if (!is.null(predictionTest)) {
          prediction <- rbind(predictionTest, prediction[, colnames(prediction) != "index"])
        } 
        
        
      }
      
      # evaluate model
      performance <- tryCatch(
        {
          evaluatePlp(prediction, typeColumn = "evaluationType")
        },
        error = function(e) {
          ParallelLogger::logError(e)
          return(NULL)
        }
      )
    }
    
  }
  
  
  # covariateSummary
  covariateSummaryResult <- NULL
  if (executeSettings$runCovariateSummary) {
    
    if (!is.null(data$Test)) {
      strata <- data.frame(
        rowId = c(
          data$Train$labels$rowId, 
          data$Test$labels$rowId 
        ),
        strataName = c(
          rep("Train", nrow(data$Train$labels)), 
          rep("Test", nrow(data$Test$labels))
        )
      )
    } else {
      strata <- data.frame(
        rowId = c(data$Train$labels$rowId),
        strataName = c(rep("Train", nrow(data$Train$labels)))
      )
    }
    
    variableImportance <- plpData$covariateData$covariateRef %>% 
      dplyr::mutate(covariateValue = 0) %>% 
      dplyr::select("covariateId", "covariateValue") %>% 
      dplyr::collect()
    if (!is.null(model)) {
      if (!is.null(model$covariateImportance)) {
        variableImportance <- model$covariateImportance %>% 
          dplyr::select("covariateId", "covariateValue")
      }
    }
    
    # apply FE if it is used
    featureEngineering <- NULL
    if (!is.null(model)) {
      featureEngineering <- model$preprocessing$featureEngineering
    }
    
    covariateSummaryResult <- do.call(covariateSummary,   
      list(
        covariateData = plpData$covariateData,
        cohort = population %>% dplyr::select("rowId"),
        labels = population %>% dplyr::select("rowId", "outcomeCount"), 
        strata = strata,
        variableImportance = variableImportance,
        featureEngineering = featureEngineering
        )
    )
  
  }
  
  #  ExecutionSummary details:
  # log the end time:
  endTime <- Sys.time()
  TotalExecutionElapsedTime <- difftime(endTime, executionDateTime, units = "mins")
  
  executionSummary <- list(
    PackageVersion = list(
      rVersion = R.Version()$version.string,
      packageVersion = utils::packageVersion("PatientLevelPrediction")
    ),
    PlatformDetails = list(
      platform = R.Version()$platform,
      cores = Sys.getenv("NUMBER_OF_PROCESSORS"),
      RAM = memuse::Sys.meminfo()[1]
      ),
    TotalExecutionElapsedTime = TotalExecutionElapsedTime,
    ExecutionDateTime = executionDateTime,
    Log = logSettings$logFileName # location for now
    #Not available at the moment: CDM_SOURCE -  meta-data containing CDM version, release date, vocabulary version
  )
  
  # if model is NULL convert it to list for saving 
  if (is.null(model)) {
    model <- list(noModel = TRUE)
    attr(model, "predictionFunction") <- "noModel"
    attr(model, "saveType") <- "RtoJson"
    class(model) <- "plpModel"
  }
  
  results <- list(
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
  class(results) <- c("runPlp")
  
  ParallelLogger::logInfo("Run finished successfully.")
  # save the results
  ParallelLogger::logInfo(paste0("Saving PlpResult"))
  tryCatch(savePlpResult(results, file.path(analysisPath, "plpResult")),
    finally = ParallelLogger::logTrace("Done."))
  ParallelLogger::logInfo(paste0("plpResult saved to ..\\", analysisPath, "\\plpResult"))
  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste0("runPlp time taken: ", signif(delta, 3), " ", attr(delta, "units")))
  return(results)
  
}
