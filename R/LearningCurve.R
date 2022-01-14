# @file LearningCurve.R
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

#' @title createLearningCurve
#'
#' @description Creates a learning curve object, which can be plotted using the
#'  \code{plotLearningCurve()} function.
#' 
#' @param plpData                    An object of type \code{plpData} - the patient level prediction
#'                                   data extracted from the CDM.
#' @param outcomeId                  (integer) The ID of the outcome.                                       
#' @param analysisId                 (integer) Identifier for the analysis. It is used to create, e.g., the result folder. Default is a timestamp.
#' @param populationSettings         An object of type \code{populationSettings} created using \code{createStudyPopulationSettings} that
#'                                   specifies how the data class labels are defined and addition any exclusions to apply to the 
#'                                   plpData cohort
#' @param splitSettings              An object of type \code{splitSettings} that specifies how to split the data into train/validation/test.  
#'                                   The default settings can be created using \code{createDefaultSplitSetting}.                               
#' @param sampleSettings             An object of type \code{sampleSettings} that specifies any under/over sampling to be done.
#'                                   The default is none.
#' @param trainFractions A list of training fractions to create models for.
#'   Note, providing \code{trainEvents} will override your input to
#'   \code{trainFractions}.
#' @param trainEvents Events have shown to be determinant of model performance.
#'   Therefore, it is recommended to provide \code{trainEvents} rather than
#'   \code{trainFractions}. Note, providing \code{trainEvents} will override
#'   your input to \code{trainFractions}. The format should be as follows:
#'   \itemize{
#'     \item{ \code{c(500, 1000, 1500) } - a list of training events}
#'   }
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
#' @param cores                 The number of computer cores to use if running in parallel
#' @param parallel              Whether to run the code in parallel
#'
#' @return A learning curve object containing the various performance measures
#'  obtained by the model for each training set fraction. It can be plotted
#'  using \code{plotLearningCurve}.
#' 
#' @examples
#' \dontrun{
#' # define model
#' modelSettings = PatientLevelPrediction::setLassoLogisticRegression()
#' 
#' # create learning curve
#' learningCurve <- PatientLevelPrediction::createLearningCurve(population,
#'                                                              plpData,
#'                                                              modelSettings)
#' # plot learning curve
#' PatientLevelPrediction::plotLearningCurve(learningCurve)
#' }
#' 
#' @export
createLearningCurve <- function(
  plpData,
  outcomeId,
  parallel = T,
  cores = 4,
  modelSettings,
  saveDirectory = getwd(),
  analysisId = 'learningCurve',
  populationSettings = createStudyPopulationSettings(),
  splitSettings = createDefaultSplitSetting(),
  trainFractions = c(0.25, 0.50, 0.75),
  trainEvents = c(500, 1000, 1500),
  sampleSettings = createSampleSettings(),
  featureEngineeringSettings = createFeatureEngineeringSettings(),
  preprocessSettings = createPreprocessSettings(
    minFraction = 0.001,
    normalize = T
  ),
  logSettings = createLogSettings(),
  executeSettings = createExecuteSettings(
    runSplitData = T, 
    runSampleData = F,
    runfeatureEngineering = F,
    runPreprocessData = T,
    runModelDevelopment = T,
    runCovariateSummary = F
    )
){
  
  if (is.null(analysisId)) {
    analysisId <- gsub(':', '', gsub('-', '', gsub(' ', '', Sys.time())))
  }
  
  
  # if trainEvents is provided override trainFractions input
  if (!is.null(trainEvents)) {
    
    trainFractions <- getTrainFractions(
      trainEvents,
      plpData, 
      outcomeId, 
      populationSettings,
      splitSettings
    )
    
  }
  
  # record global start time
  ExecutionDateTime <- Sys.time()
  
  if(parallel){
    ensure_installed('parallel')
    if(is.null(cores)){
      ParallelLogger::logInfo(paste0('Number of cores not specified'))
      cores <- parallel::detectCores()
      ParallelLogger::logInfo(paste0('Using all ', cores))
      ParallelLogger::logInfo(paste0('Set cores input to use fewer...'))
    }
    
    # save data
    savePlpData(plpData, file.path(saveDirectory,'data'))
    
    # code to run in parallel
    getLcSettings <- function(i){
      result <- list(
        plpData = file.path(saveDirectory,'data'),
        outcomeId = outcomeId,
        analysisId = paste0(analysisId,i),
        populationSettings = populationSettings,
        splitSettings = splitSettings,
        sampleSettings = sampleSettings,
        featureEngineeringSettings = featureEngineeringSettings,
        preprocessSettings = preprocessSettings,
        modelSettings = modelSettings,
        logSettings = logSettings,
        executeSettings = executeSettings,
        saveDirectory = saveDirectory
      )
      result$splitSettings$train <- trainFractions[i]
      
      return(result)
    }
    lcSettings <- lapply(1:length(trainFractions), getLcSettings)
    
    cluster <- ParallelLogger::makeCluster(numberOfThreads = cores)
    ParallelLogger::clusterRequire(cluster, c("PatientLevelPrediction", "Andromeda", "FeatureExtraction"))
    
    learningCurve <- ParallelLogger::clusterApply(cluster = cluster, 
      x = lcSettings, 
      fun = lcWrapper, 
      stopOnError = FALSE,
      progressBar = TRUE)
    ParallelLogger::stopCluster(cluster)
    
  } else{
  
    # code to run not in parallel
    # number of training set fractions
    nRuns <- length(trainFractions)
    
    settings = list(
      plpData = plpData,
      outcomeId = outcomeId,
      analysisId = analysisId,
      populationSettings = populationSettings,
      splitSettings = splitSettings,
      sampleSettings = sampleSettings,
      featureEngineeringSettings = featureEngineeringSettings,
      preprocessSettings = preprocessSettings,
      modelSettings = modelSettings,
      logSettings = logSettings,
      executeSettings = executeSettings,
      saveDirectory = saveDirectory
    )
    
  learningCurve <- lapply(1:nRuns, function(i){
    
    settings$splitSettings$train = trainFractions[i]
    settings$analysisId = paste0(settings$analysisId, '_', i)                                  
    result <- do.call(runPlp, settings)  
    
    result <- learningCurveHelper(
      result = result,
      trainFractions = trainFractions[i]
      )
    return(result)
    
  })
  
  }
  
  learningCurve <- do.call(rbind,learningCurve)
  
  learningCurve <- tidyr::pivot_wider(
    data = learningCurve, 
    names_from = 'name', 
    values_from = 'value'
    )
  #learningCurve <- reshape2::dcast(data = learningCurve,  trainFraction ~ name)

  endTime <- Sys.time()
  TotalExecutionElapsedTime <-
    as.numeric(difftime(endTime, ExecutionDateTime,
                        units = "secs"))
  ParallelLogger::logInfo('Finished in ', round(TotalExecutionElapsedTime), ' secs.')
  
  return(learningCurve)
}

lcWrapper <- function(settings){
  plpData <- PatientLevelPrediction::loadPlpData(settings$plpData)
  settings$plpData <- plpData
  result <- tryCatch({do.call(runPlp, settings)},
                     warning = function(war) {
                       ParallelLogger::logInfo(paste0('a warning: ', war))
                     }, 
                     error = function(err) {
                       ParallelLogger::logError(paste0('an error: ', err))
                       return(NULL)
                     }       
  )
  if(!is.null(result)){
    ParallelLogger::logInfo('Extracting performance for learning curve...')
    final <- learningCurveHelper(result, settings$splitSettings$train)
    return(final)
    
  } else{
    return(c())
  }
}



getTrainFractions <- function(
  trainEvents,
  plpData, 
  outcomeId, 
  populationSettings,
  splitSettings
){
  
  population <- do.call(
    createStudyPopulation, 
    list(
      plpData = plpData,
      outcomeId = outcomeId,
      populationSettings = populationSettings
    )
  )
  
  # compute training set fractions from training events
  samplesRequired <- trainEvents/(sum(population$outcomeCount/nrow(population)))
  trainFractionsTemp <- samplesRequired/nrow(population)
  
  # filter out no. of events that would exceed the available training set size
  binaryMask <- trainFractionsTemp <= (1.0 - splitSettings$testFraction)
  
  # override any input to trainFractions with event-based training fractions
  trainFractions <- trainFractionsTemp[binaryMask]
  
  # Check if any train fractions could be associated with the provided events
  if(!length(trainFractions)) {
    # If not, fall back on default train fractions
    trainFractions <- c(0.25, 0.50, 0.75)
  }
  
  return(trainFractions)
}


learningCurveHelper <- function(result, trainFractions){

  executeTime <- result$executionSummary$TotalExecutionElapsedTime
  nPredictors <- result$model$covariateImportance %>% dplyr::filter(.data$covariateValue != 0) %>% dplyr::tally() %>% dplyr::pull()
  
  # evaluationStatistics is a data.frame with columns 'evaluation','metric','value'
  result <- result$performanceEvaluation$evaluationStatistics
  
  result$name <- paste(result$evaluation, result$metric, sep='_')
  
  result <- result %>% dplyr::select(.data$name, .data$value)
  
  result <- rbind(
    c('executionTime', executeTime), 
    result, 
    c('nPredictors', nPredictors)
  )
  
  result$trainFraction <-   trainFractions * 100
  
  return(result)
}




#' @title plotLearningCurve
#'
#' @description Create a plot of the learning curve using the object returned
#' from \code{createLearningCurve}.
#'
#' @param learningCurve An object returned by \code{\link{createLearningCurve}}
#'   function.
#' @param metric Specifies the metric to be plotted:
#'   \itemize{
#'     \item{\code{'AUROC'} - use the area under the Receiver Operating
#'       Characteristic curve}
#'     \item{\code{'AUPRC'} - use the area under the Precision-Recall curve}
#'     \item{\code{'sBrier'} - use the scaled Brier score}
#'   }
#' @param abscissa Specify the abscissa metric to be plotted:
#'   \itemize{
#'     \item{\code{'events'} - use number of events}
#'     \item{\code{'observations'} - use number of observations}
#'   }
#' @param plotTitle Title of the learning curve plot.
#' @param plotSubtitle Subtitle of the learning curve plot.
#' @param fileName Filename of plot to be saved, for example \code{'plot.png'}.
#'   See the function \code{ggsave} in the ggplot2 package for supported file 
#'   formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to 
#' file in a different format.
#' 
#' @examples
#' \dontrun{
#' # create learning curve object
#' learningCurve <- createLearningCurve(population,
#'                                      plpData,
#'                                      modelSettings)
#' # plot the learning curve
#' plotLearningCurve(learningCurve)
#' }
#' 
#' @export
plotLearningCurve <- function(learningCurve,
  metric = "AUROC",
  abscissa = "events",
  plotTitle = "Learning Curve", 
  plotSubtitle = NULL,
  fileName = NULL){
  
  tidyLearningCurve <- NULL
  yAxisRange <- NULL
  y <- NULL
  
  learningCurve <- as.data.frame(learningCurve)
  
  # check for performance metric to plot
  if(metric == "AUROC") {
    # create a data.frame with evalautionType, AUROC
    tidyLearningCurve <- learningCurve %>% 
      dplyr::rename(
        Occurrences = .data$Train_outcomeCount, 
        Observations = .data$Train_populationSize ) %>%
      dplyr::select(.data$trainFraction, .data$Occurrences, .data$Observations, .data$Test_AUROC, .data$Train_AUROC)
    
    for(i in 1:ncol(tidyLearningCurve)){
      tidyLearningCurve[,i] <- as.double(as.character(tidyLearningCurve[,i]))
    }
    
    tidyLearningCurve <- tidyr::pivot_longer(
      data = as.data.frame(tidyLearningCurve),
      cols = colnames(as.data.frame(tidyLearningCurve))[!colnames(as.data.frame(tidyLearningCurve)) %in% c('trainFraction', 'Occurrences', 'Observations')], 
      values_to = "value", 
      names_to = 'variable'
    )
    
    #tidyLearningCurve <- reshape2::melt(as.data.frame(tidyLearningCurve), id.vars = c('trainFraction', 'Occurrences', 'Observations'))
    
    tidyLearningCurve$Dataset <- sapply(tidyLearningCurve$variable, function(x)strsplit(as.character(x), '_')[[1]][1])
    
    # define plot properties
    yAxisRange <- c(0.5, 1.0)
    
  } else if (metric == "AUPRC") {
    # tidy up dataframe
    tidyLearningCurve <- learningCurve %>% 
      dplyr::rename(
        Occurrences = .data$Train_outcomeCount, 
        Observations = .data$Train_populationSize ) %>%
      dplyr::select(.data$trainFraction, .data$Occurrences, .data$Observations, .data$Test_AUPRC, .data$Train_AUPRC)
    
    for(i in 1:ncol(tidyLearningCurve)){
      tidyLearningCurve[,i] <- as.double(as.character(tidyLearningCurve[,i]))
    }

    tidyLearningCurve <- tidyr::pivot_longer(
      data = as.data.frame(tidyLearningCurve),
      cols = colnames(as.data.frame(tidyLearningCurve))[!colnames(as.data.frame(tidyLearningCurve)) %in% c('trainFraction', 'Occurrences', 'Observations')], 
      values_to = "value", 
      names_to = 'variable'
    )
    #tidyLearningCurve <- reshape2::melt(as.data.frame(tidyLearningCurve), id.vars = c('trainFraction', 'Occurrences', 'Observations'))
    
    tidyLearningCurve$Dataset <- sapply(tidyLearningCurve$variable, function(x)strsplit(as.character(x), '_')[[1]][1])
    
    # define plot properties
    yAxisRange <- c(0.0, 1.0)
    
  } else if (metric == "sBrier") {
    # tidy up dataframe
    tidyLearningCurve <- learningCurve %>% 
      dplyr::rename(
        Occurrences = .data$Train_outcomeCount, 
        Observations = .data$Train_populationSize ) %>%
      dplyr::select(.data$trainFraction, .data$Occurrences, .data$Observations, .data$`Test_brier score scaled`, .data$`Train_brier score scaled`)
    
    for(i in 1:ncol(tidyLearningCurve)){
      tidyLearningCurve[,i] <- as.double(as.character(tidyLearningCurve[,i]))
    }
    
    tidyLearningCurve <- tidyr::pivot_longer(
      data = as.data.frame(tidyLearningCurve),
      cols = colnames(as.data.frame(tidyLearningCurve))[!colnames(as.data.frame(tidyLearningCurve)) %in% c('trainFraction', 'Occurrences', 'Observations')], 
      values_to = "value", 
      names_to = 'variable'
    )
    #tidyLearningCurve <- reshape2::melt(as.data.frame(tidyLearningCurve), id.vars = c('trainFraction', 'Occurrences', 'Observations'))
    
    tidyLearningCurve$Dataset <- sapply(tidyLearningCurve$variable, function(x)strsplit(as.character(x), '_')[[1]][1])
    
    
    # define plot properties
    yAxisRange <- c(0.0, 1.0)
    
  } else {
    stop("An incorrect metric has been specified.")
  }
  
  if (abscissa == "observations") {
    abscissa <- "Observations"
    abscissaLabel <- "No. of observations"
  } else if (abscissa == "events") {
    abscissa <- "Occurrences"
    abscissaLabel <- "No. of events"
  } else {
    stop("An incorrect abscissa has been specified.")
  }
  
  # create plot object
  plot <- tidyLearningCurve %>%
    ggplot2::ggplot(ggplot2::aes_string(x = abscissa, y= 'value',
      col = "Dataset")) +
    ggplot2::geom_line() +
    ggplot2::coord_cartesian(ylim = yAxisRange, expand = FALSE) +
    ggplot2::labs(title = plotTitle, subtitle = plotSubtitle, 
      x = abscissaLabel, y = metric) +
    ggplot2::theme_light()
  
  # save plot, if fucntion call provides a file name
  if ((!is.null(fileName)) & (is.character(fileName))) {
    ggplot2::ggsave(fileName, plot, width = 5, height = 4.5, dpi = 400)
  }
  
  return(plot)
}
