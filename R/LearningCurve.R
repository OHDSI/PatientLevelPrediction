# @file LearningCurve.R
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

#' @title createLearningCurve
#'
#' @description Creates a learning curve object, which can be plotted using the
#'  \code{plotLearningCurve()} function.
#' 
#' @param population The population created using \code{createStudyPopulation()}
#'   that will be used to develop the model.
#' @param plpData An object of type \code{plpData} - the patient level
#'   prediction data extracted from the CDM.
#' @param modelSettings An object of class \code{modelSettings} created using
#'   one of the function:
#'   \itemize{
#'     \item{{setLassoLogisticRegression} - a lasso logistic regression
#'       model}
#'     \item{\code{setGradientBoostingMachine} - a gradient boosting machine}
#'     \item{\code{setRandomForest} - a random forest model}
#'     \item{\code{setKNN} - a k-nearest neighbour model}
#'   }
#' @param testSplit Specifies the type of evaluation used. Can be either 
#'   \code{'person'} or \code{'time'}. The value \code{'time'} finds the date
#'   that splots the population into the testing and training fractions
#'   provided. Patients with an index after this date are assigned to the test
#'   set and patients with an index prior to this date are assigned to the
#'   training set. The value \code{'person'} splits the data randomly into
#'   testing and training sets according to fractions provided. The split is
#'   stratified by the class label.
#' @param testFraction The fraction of the data, which will be used as the 
#'   testing set in the patient split evaluation.
#' @param trainFractions A list of training fractions to create models for.
#' @param splitSeed The seed used to split the testing and training set when
#'   using a 'person' type split                  
#' @param nfold The number of folds used in the cross validation (default = 
#'   \code{3}).
#' @param indexes A dataframe containing a rowId and index column where the 
#'   index value of -1 means in the test set, and positive integer represents
#'   the cross validation fold (default is \code{NULL}).
#' @param saveDir The path to the directory where the models will be saved
#'   (if \code{NULL}, uses working directory).
#' @param saveModel Logical indicating whether to save the model once it has
#'   been trained (default is \code{TRUE}).
#' @param verbosity Sets the level of the verbosity. If the log level is at or
#'   higher in priority than the logger threshold, a message will print. The 
#'   levels are:
#'   \itemize{
#'     \item{\code{DEBUG} - highest verbosity showing all debug statements}
#'     \item{\code{TRACE} - showing information about start and end of steps}
#'     \item{\code{INFO} - show informative messages (default)}
#'     \item{\code{WARN} - show warning messages}
#'     \item{\code{ERROR} - show error messages}
#'     \item{\code{FATAL} - be silent except for fatal errors}
#'   }
#' @param clearffTemp Clears the temporary ff-directory after each iteration. 
#'   This can be useful, if the fitted models are large.
#' @param minCovariateFraction Minimum covariate prevalence in population to
#'   avoid removal during preprocssing.
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
createLearningCurve <- function(population,
                                plpData,
                                modelSettings,
                                testSplit = 'person',
                                testFraction = 0.25,
                                trainFractions = c(0.25, 0.50, 0.75),
                                splitSeed = NULL,
                                nfold = 3,
                                indexes = NULL,
                                saveDir = NULL,
                                saveModel = TRUE,
                                verbosity = 'TRACE',
                                clearffTemp = FALSE,
                                minCovariateFraction = 0.001) {
  
  # the analysis id will always be generated automatically
  analysisId <- NULL
  
  # if no path is provided, save to working directory
  if (is.null(saveDir)) {
    saveDir <- file.path(getwd(), 'plpmodels')
  }
  
  logPath = file.path(saveDir, "plplog.txt")
  
  
  # remove all registered loggers
  ParallelLogger::clearLoggers()
  
  # check logger
  if (length(ParallelLogger::getLoggers()) == 0) {
    logger <- ParallelLogger::createLogger(
      name = "SIMPLE",
      threshold = verbosity,
      appenders = list(
        ParallelLogger::createConsoleAppender(layout = ParallelLogger::layoutSimple),
        ParallelLogger::createFileAppender(layout = ParallelLogger::layoutTimestamp,
                                        fileName = logPath)
      )
    )
    ParallelLogger::registerLogger(logger)
  }
  
  # number of training set fractions
  nRuns <- length(trainFractions)
  
  # record global start time
  ExecutionDateTime <- Sys.time()
  
  # store a copy of the original population
  originalPopulation <- population
  
  learningCurve <- foreach::foreach(i = 1:nRuns,
                                    .combine = rbind,
                                    .errorhandling = "remove") %do% {
                                      
    # record start time
    startTime <- Sys.time()
    
    # restore the original population
    population <- originalPopulation
    
    # create an analysis id and folder to save the results of each run
    start.all <- Sys.time()
    if (is.null(analysisId)) {
      analysisId <- gsub(':', '', gsub('-', '', gsub(' ', '', start.all)))
    }
    
    analysisPath = file.path(saveDir, analysisId)
    if (!dir.exists(analysisPath)) {
      dir.create(analysisPath, recursive = T)
    }

    # write log to both, console and file
    # other appenders can be created, e.g. to a web service or database

    ParallelLogger::logInfo(paste0(
      'Patient-Level Prediction Package version ',
      utils::packageVersion("PatientLevelPrediction")
    ))
    
    # get target and outcome cohort ids
    cohortId <- attr(population, "metaData")$cohortId
    outcomeId <- attr(population, "metaData")$outcomeId
    
    # add header to analysis log
    ParallelLogger::logInfo(sprintf('%-20s%s', 'Training Size: ', trainFractions[i]))
    ParallelLogger::logInfo(sprintf('%-20s%s', 'AnalysisID: ', analysisId))
    ParallelLogger::logInfo(sprintf('%-20s%s', 'CohortID: ', cohortId))
    ParallelLogger::logInfo(sprintf('%-20s%s', 'OutcomeID: ', outcomeId))
    ParallelLogger::logInfo(sprintf('%-20s%s', 'Cohort size: ', nrow(plpData$cohorts)))
    ParallelLogger::logInfo(sprintf('%-20s%s', 'Covariates: ', nrow(plpData$covariateRef)))
    ParallelLogger::logInfo(sprintf('%-20s%s', 'Population size: ', nrow(population)))
    ParallelLogger::logInfo(sprintf('%-20s%s', 'Cases: ', sum(population$outcomeCount > 0)))

    # check parameters
    ParallelLogger::logTrace('Parameter Check Started')
    checkInStringVector(testSplit, c('person', 'time'))
    checkHigherEqual(sum(population[, 'outcomeCount'] > 0), 25)
    checkIsClass(plpData, c('plpData.coo', 'plpData'))
    checkIsClass(testFraction, 'numeric')
    checkHigher(testFraction, 0)
    checkIsClass(nfold, 'numeric')
    checkHigher(nfold, 0)
    
    # construct the training and testing set according to split type
    # indices will be non-zero for rows in training and testing set
    if (testSplit == 'time') {
      ParallelLogger::logTrace('Dataset time split started')
      indexes <- tryCatch({
        timeSplitter(
          population,
          test = testFraction,
          train = trainFractions[i],
          nfold = nfold,
          seed = splitSeed
        )
      },
      finally = ParallelLogger::logTrace('Done.'))
    }
    if (testSplit == 'person') {
      ParallelLogger::logTrace('Dataset person split started')
      indexes <- tryCatch({
        personSplitter(
          population,
          test = testFraction,
          train = trainFractions[i],
          nfold = nfold,
          seed = splitSeed
        )
      },
      finally = ParallelLogger::logTrace('Done.'))
    }
    
    # check if population count is equal to the number of indices returned
    if (nrow(population) != nrow(indexes)) {
      ParallelLogger::logError(sprintf(
        'Population dimension not compatible with indexes: %d <-> %d',
        nrow(population),
        nrow(indexes)
      ))
      stop('Population dimension not compatible with indexes')
    }
    
    # concatenate indices to population dataframe
    tempmeta <- attr(population, "metaData")
    if (is.null(population$indexes)) {
      population <- merge(population, indexes)
      colnames(population)[colnames(population) == 'index'] <-
        'indexes'
    } else{
      attr(population, 'indexes') <- indexes
    }
    attr(population, "metaData") <- tempmeta
    
    # create settings object
    settings <- list(
      data = plpData,
      minCovariateFraction = minCovariateFraction,
      modelSettings = modelSettings,
      population = population,
      cohortId = cohortId,
      outcomeId = outcomeId
    )
    
    ParallelLogger::logInfo(sprintf('Training %s model', settings$modelSettings$name))
    # the call is sinked because of the external calls (Python etc)
    if (sink.number() > 0) {
      ParallelLogger::logWarn(paste0('sink had ', sink.number(), ' connections open!'))
    }

    # fit the model
    model <- tryCatch({
      do.call(fitPlp, settings)
    },
    error = function(e) {
      ParallelLogger::logError(e)
      stop(paste0(e))
    },
    finally = {
      ParallelLogger::logTrace('Done.')
    })
    
    # save the model
    if (saveModel == TRUE) {
      modelLoc <- file.path(saveDir, analysisId, 'savedModel')
      tryCatch({
        savePlpModel(model, modelLoc)},
        finally = ParallelLogger::logInfo(paste0(
          'Model saved to ..\\', analysisId, '\\savedModel'
        ))
      )
    }
    
    # calculate metrics
    ParallelLogger::logTrace('Prediction')
    ParallelLogger::logTrace(paste0('Calculating prediction for ', sum(indexes$index != 0)))
    ind <- population$rowId %in% indexes$rowId[indexes$index != 0]
    prediction <-
      model$predict(plpData = plpData, population = population[ind,])
    
    metaData <- list(
      predictionType = "binary",
      cohortId = attr(population, 'metaData')$cohortId,
      outcomeId = attr(population, 'metaData')$outcomeId
    )
    
    attr(prediction, "metaData") <- metaData
    
    finally = ParallelLogger::logTrace('Done.')
    
    # measure model performance
    if (ifelse(is.null(prediction), FALSE, length(unique(prediction$value)) >
               1)) {
      # add analysisID
      attr(prediction, "metaData")$analysisId <- analysisId
      
      ParallelLogger::logInfo('Train set evaluation')
      performance.train <-
        evaluatePlp(prediction[prediction$indexes > 0,], plpData)
      ParallelLogger::logTrace('Done.')
      ParallelLogger::logInfo('Test set evaluation')
      performance.test <-
        evaluatePlp(prediction[prediction$indexes < 0,], plpData)
      ParallelLogger::logTrace('Done.')
      
      # combine the test and train data and add analysisId
      performance <-
        reformatPerformance(train = performance.train, test = performance.test,
                            analysisId)
      
      if (!is.null(saveDir)) {
        ParallelLogger::logTrace('Saving evaluation')
        if (!dir.exists(file.path(analysisPath, 'evaluation')))
          dir.create(file.path(analysisPath, 'evaluation'))
        tryCatch(
          utils::write.csv(
            performance$evaluationStatistics,
            file.path(analysisPath, 'evaluation', 'evaluationStatistics.csv'),
            row.names = F
          ),
          finally = ParallelLogger::logTrace('Saved EvaluationStatistics.')
        )
        tryCatch(
          utils::write.csv(
            performance$thresholdSummary,
            file.path(analysisPath, 'evaluation', 'thresholdSummary.csv'),
            row.names = F
          ),
          finally = ParallelLogger::logTrace('Saved ThresholdSummary.')
        )
        tryCatch(
          utils::write.csv(
            performance$demographicSummary,
            file.path(analysisPath, 'evaluation', 'demographicSummary.csv'),
            row.names = F
          ),
          finally = ParallelLogger::logTrace('Saved DemographicSummary.')
        )
        tryCatch(
          utils::write.csv(
            performance$calibrationSummary,
            file.path(analysisPath, 'evaluation', 'calibrationSummary.csv'),
            row.names = F
          ),
          finally = ParallelLogger::logTrace('Saved CalibrationSummary.')
        )
        tryCatch(
          utils::write.csv(
            performance$predictionDistribution,
            file.path(analysisPath,
                      'evaluation',
                      'predictionDistribution.csv'),
            row.names = F
          ),
          finally = ParallelLogger::logTrace('Saved PredictionDistribution.')
        )
      }

    } else{
      ParallelLogger::logWarn(paste0(
        'Evaluation not possible as prediciton NULL or all the same values'
      ))
      performance.test <- NULL
      performance.train <- NULL
    }
    
    # record end time
    endTime <- Sys.time()
    TotalExecutionElapsedTime <-
      as.numeric(difftime(endTime, ExecutionDateTime,
                          units = "secs"))
    
    # compute execution time for each run
    timeDiff <- as.numeric(difftime(endTime, startTime, units = "secs"))
    
    # input settings
    inputSetting <-
      list(
        dataExtrractionSettings = plpData$metaData$call,
        populationSettings = attr(population, "metaData"),
        modelSettings = modelSettings,
        testSplit = testSplit,
        testFraction = testFraction
      )
    
    # execution summary
    executionSummary <-
      list(
        PackageVersion = list(
          rVersion = R.Version()$version.string,
          packageVersion = utils::packageVersion("PatientLevelPrediction")
        ),
        PlatformDetails = list(
          platform = R.Version()$platform,
          cores = Sys.getenv('NUMBER_OF_PROCESSORS'),
          RAM = utils::memory.size()
        ),
        #  test for non-windows needed
        TotalExecutionElapsedTime = TotalExecutionElapsedTime,
        ExecutionDateTime = ExecutionDateTime,
        Log = logPath # location for now
        # Not available at the moment: CDM_SOURCE - meta-data containing CDM
        # version, release date, vocabulary version
      )
    
    ParallelLogger::logInfo(paste0('Calculating covariate summary @ ', Sys.time()))
    ParallelLogger::logInfo('This can take a while...')
    covSummary <- covariateSummary(plpData, population)
    covSummary <-
      merge(model$varImp, covSummary, by = 'covariateId', all = T)
    trainCovariateSummary <-
      covariateSummary(plpData, population[population$index > 0,])
    colnames(trainCovariateSummary)[colnames(trainCovariateSummary) != 'covariateId'] <-
      paste0('Train', colnames(trainCovariateSummary)[colnames(trainCovariateSummary) !=
                                                        'covariateId'])
    testCovariateSummary <-
      covariateSummary(plpData, population[population$index < 0,])
    colnames(testCovariateSummary)[colnames(testCovariateSummary) != 'covariateId'] <-
      paste0('Test', colnames(testCovariateSummary)[colnames(testCovariateSummary) !=
                                                      'covariateId'])
    covSummary <-
      merge(covSummary,
            trainCovariateSummary,
            by = 'covariateId',
            all = T)
    covSummary <-
      merge(covSummary,
            testCovariateSummary,
            by = 'covariateId',
            all = T)
    if (!is.null(saveDir)) {
      ParallelLogger::logTrace('Saving covariate summary')
      if (!dir.exists(file.path(analysisPath, 'evaluation')))
        dir.create(file.path(analysisPath, 'evaluation'))
      tryCatch({
        utils::write.csv(
          covSummary,
          file.path(analysisPath, 'evaluation', 'covariateSummary.csv'),
          row.names = F
        )},
        finally = ParallelLogger::logTrace('Saved covariate summary.')
      )
    }
    ParallelLogger::logInfo(paste0('Finished covariate summary @ ', Sys.time()))
    
    # create result object
    result <- list(
      inputSetting = inputSetting,
      executionSummary = executionSummary,
      model = model,
      prediction = prediction,
      performanceEvaluation = performance,
      covariateSummary = covSummary,
      analysisRef = list(
        analysisId = analysisId,
        analysisName = NULL,
        analysisSettings = NULL
      )
    )
    class(result) <- c('list', 'plpModel')
    
    ParallelLogger::logInfo("Run finished successfully.")
    ParallelLogger::logInfo()
    
    # combine performance metrics
    df <- data.frame(
      x = trainFractions[i] * 100,
      popSizeTrain = nrow(population[population$index > 0,]),
      outcomeCountTrain = sum(population[population$index > 0,]$outcomeCount),
      executionTime = TotalExecutionElapsedTime,
      trainAUCROC = performance.train$evaluationStatistics$AUC[[1]],
      testAUCROC = performance.test$evaluationStatistics$AUC[[1]],
      trainAUCPR = performance.train$evaluationStatistics$AUPRC[[1]],
      testAUCPR = performance.test$evaluationStatistics$AUPRC[[1]],
      trainBrierScore = performance.train$evaluationStatistics$BrierScore,
      testBrierScore = performance.test$evaluationStatistics$BrierScore,
      trainBrierScaled = performance.train$evaluationStatistics$BrierScaled,
      testBrierScaled = performance.test$evaluationStatistics$BrierScaled,
      trainCalibrationIntercept = performance.train$evaluationStatistics$CalibrationIntercept,
      testCalibrationIntercept = performance.test$evaluationStatistics$CalibrationIntercept,
      trainCalibrationSlope = performance.train$evaluationStatistics$CalibrationSlope,
      testCalibrationSlope = performance.test$evaluationStatistics$CalibrationSlope
    )

    # remove temporary files after each run
    if (clearffTemp) {
      clearffTempDir()
    }
    
    # reset analysis id
    analysisId <- NULL
    
    # return data frame row for each run
    return(df)
  }

  ParallelLogger::clearLoggers()
  
  names(learningCurve) <- c(
    "x",
    "popSizeTrain",
    "outcomeCountTrain",
    "executionTime",
    "trainAUCROC",
    "testAUCROC",
    "trainAUCPR",
    "testAUCPR",
    "trainBrierScore",
    "testBrierScore",
    "trainBrierScaled",
    "testBrierScaled",
    "trainCalibrationIntercept",
    "testCalibrationIntercept",
    "trainCalibrationSlope",
    "testCalibrationSlope"
  )
  
  return(learningCurve)
}

#' @title createLearningCurvePar
#' 
#' @description Creates a learning curve in parallel, which can be plotted using
#'  the \code{plotLearningCurve()} function. Currently this functionality is
#'  only supported by Lasso Logistic Regression.
#' 
#' @param population The population created using \code{createStudyPopulation()}
#'   that will be used to develop the model.
#' @param plpData An object of type \code{plpData} - the patient level
#'   prediction data extracted from the CDM.
#' @param modelSettings An object of class \code{modelSettings} created using
#'   one of the function. Currently only one model is supported:
#'   \itemize{
#'     \item{\code{setLassoLogisticRegression} - a lasso logistic regression
#'       model}
#'   }
#' @param testSplit Specifies the type of evaluation used. Can be either 
#'   \code{'person'} or \code{'time'}. The value \code{'time'} finds the date
#'   that splots the population into the testing and training fractions
#'   provided. Patients with an index after this date are assigned to the test
#'   set and patients with an index prior to this date are assigned to the
#'   training set. The value \code{'person'} splits the data randomly into
#'   testing and training sets according to fractions provided. The split is
#'   stratified by the class label.
#' @param testFraction The fraction of the data, which will be used as the 
#'   testing set in the patient split evaluation.
#' @param trainFractions A list of training fractions to create models for.
#' @param splitSeed The seed used to split the testing and training set when
#'   using a 'person' type split                  
#' @param nfold The number of folds used in the cross validation (default = 
#'   \code{3}).
#' @param indexes A dataframe containing a rowId and index column where the 
#'   index value of -1 means in the test set, and positive integer represents
#'   the cross validation fold (default is \code{NULL}).
#' @param minCovariateFraction Minimum covariate prevalence in population to
#'   avoid removal during preprocssing.
#'
#' @return A learning curve object containing the various performance measures
#'  obtained by the model for each training set fraction. It can be plotted
#'  using \code{plotLearningCurve}.
#' 
#' @examples
#' \dontrun{
#' # define model
#' modelSettings = setLassoLogisticRegression()
#' 
#' # register parallel backend
#' registerParallelBackend()
#' 
#' # create learning curve
#' learningCurve <- createLearningCurvePar(population,
#'                                         plpData,
#'                                         modelSettings)
#' # plot learning curve
#' plotLearningCurve(learningCurve)
#' }
#' 
#' @export
createLearningCurvePar <- function(population,
                                   plpData,
                                   modelSettings,
                                   testSplit = 'person',
                                   testFraction = 0.25,
                                   trainFractions = c(0.25, 0.50, 0.75),
                                   splitSeed = NULL,
                                   nfold = 3,
                                   indexes = NULL,
                                   minCovariateFraction = 0.001) {
  
  # register a parallel backend
  registerParallelBackend()
  
  # verify that a parallel backend has been registered
  setup_parallel()
  
  ParallelLogger::logInfo('Started to run in parallel, this can take a while...')
  
  # record global start time
  ExecutionDateTime <- Sys.time()
  
  # store a copy of the original population
  originalPopulation <- population
  
  learningCurve <- foreach::foreach(
    i = 1:length(trainFractions),
    .combine = rbind,
    .errorhandling = "remove",
    .packages = c("doParallel",
                  "PatientLevelPrediction")
  ) %dopar% {
    
    # restore original population
    population <- originalPopulation
    
    cohortId <- attr(population, "metaData")$cohortId
    outcomeId <- attr(population, "metaData")$outcomeId
    
    # construct the training and testing set according to split type
    # indices will be non-zero for rows in training and testing set
    if (testSplit == 'time') {
      indexes <- PatientLevelPrediction::timeSplitter(
        population,
        test = testFraction,
        train = trainFractions[i],
        nfold = nfold,
        seed = splitSeed
      )
    }
    
    if (testSplit == 'person') {
      indexes <- PatientLevelPrediction::personSplitter(
        population,
        test = testFraction,
        train = trainFractions[i],
        nfold = nfold,
        seed = splitSeed
      )
    }
    
    # concatenate indices to population dataframe
    tempmeta <- attr(population, "metaData")
    if (is.null(population$indexes)) {
      population <- merge(population, indexes)
      colnames(population)[colnames(population) == 'index'] <-
        'indexes'
    } else{
      attr(population, 'indexes') <- indexes
    }
    attr(population, "metaData") <- tempmeta
    
    # create settings object
    settings <- list(
      data = plpData,
      minCovariateFraction = minCovariateFraction,
      modelSettings = modelSettings,
      population = population,
      cohortId = cohortId,
      outcomeId = outcomeId
    )
    
    # fit the model
    model <- do.call(PatientLevelPrediction::fitPlp, settings)
    
    ind <- population$rowId %in% indexes$rowId[indexes$index != 0]
    prediction <-
      model$predict(plpData = plpData, population = population[ind,])
    
    metaData <- list(
      predictionType = "binary",
      cohortId = attr(population, 'metaData')$cohortId,
      outcomeId = attr(population, 'metaData')$outcomeId
    )
    
    attr(prediction, "metaData") <- metaData
    
    # measure model performance
    if (ifelse(is.null(prediction), FALSE, length(unique(prediction$value)) >
               1)) {
      # add analysisID
      attr(prediction, "metaData")$analysisId <- NULL
      
      performance.train <-
        PatientLevelPrediction::evaluatePlp(prediction[prediction$indexes > 0,],
                                            plpData)
      
      performance.test <-
        PatientLevelPrediction::evaluatePlp(prediction[prediction$indexes < 0,],
                                            plpData)
    }
    
    # record end time
    endTime <- Sys.time()
    TotalExecutionElapsedTime <-
      as.numeric(difftime(endTime, ExecutionDateTime,
                          units = "secs"))    
    
    # return data frame row for each run
    return(
      data.frame(
        x = trainFractions[i] * 100,
        popSizeTrain = nrow(population[population$index > 0,]),
        outcomeCountTrain = sum(population[population$index > 0,]$outcomeCount),
        executionTime = TotalExecutionElapsedTime,
        trainAUCROC = performance.train$evaluationStatistics$AUC[[1]],
        testAUCROC = performance.test$evaluationStatistics$AUC[[1]],
        trainAUCPR = performance.train$evaluationStatistics$AUPRC[[1]],
        testAUCPR = performance.test$evaluationStatistics$AUPRC[[1]],
        trainBrierScore = performance.train$evaluationStatistics$BrierScore,
        testBrierScore = performance.test$evaluationStatistics$BrierScore,
        trainBrierScaled = performance.train$evaluationStatistics$BrierScaled,
        testBrierScaled = performance.test$evaluationStatistics$BrierScaled,
        trainCalibrationIntercept = performance.train$evaluationStatistics$CalibrationIntercept,
        testCalibrationIntercept = performance.test$evaluationStatistics$CalibrationIntercept,
        trainCalibrationSlope = performance.train$evaluationStatistics$CalibrationSlope,
        testCalibrationSlope = performance.test$evaluationStatistics$CalibrationSlope
      )
    )

  }
  names(learningCurve) <- c(
    "x",
    "popSizeTrain",
    "outcomeCountTrain",
    "executionTime",
    "trainAUCROC",
    "testAUCROC",
    "trainAUCPR",
    "testAUCPR",
    "trainBrierScore",
    "testBrierScore",
    "trainBrierScaled",
    "testBrierScaled",
    "trainCalibrationIntercept",
    "testCalibrationIntercept",
    "trainCalibrationSlope",
    "testCalibrationSlope"
  )
  
  endTime <- Sys.time()
  TotalExecutionElapsedTime <-
    as.numeric(difftime(endTime, ExecutionDateTime,
                        units = "secs"))
  ParallelLogger::logInfo('Finished in ', round(TotalExecutionElapsedTime), ' secs.')
  
  # de-register the parallel backend by registering a sequential backend
  registerSequentialBackend()
  
  return(learningCurve)
}
