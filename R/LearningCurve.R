#
# Copyright 2018 Observational Health Data Sciences and Informatics
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

#' createLearningCurve - Creates a learning curve object
#'
#' @description
#' Creates a learning curve object, which can be plotted using the 
#' \code{plotLearningCurve()} function.
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
#' @param save The path to the directory where the models will be saved
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
#' @param timeStamp If \code{TRUE} a timestamp will be added to each logging 
#'   statement. Automatically switched on for \code{TRACE} level.
#' @param analysisId Identifier for the analysis. It is used for example used in 
#'   naming the result folder. The default is a timestamp.
#' @param clearffTemp Clears the temporary ff-directory after each iteration. 
#'   This can be useful, if the fitted models are large.
#' @param minCovariateFraction Minimum covariate prevalence in population to
#'   avoid removal during preprocssing.
#'
#' @return
#' An object containing the model or location where the model is save, the data 
#' selection settings, the preprocessing and training settings as well as
#' various performance measures obtained by the model.
#' \itemize{
#'   \item{\code{predict} - a function that can be applied to new data to apply
#'     the trained model and make predictions}
#'   \item{\code{model} - a list of class \code{plpModel} containing the model,
#'     training metrics and model metadata}
#'   \item{\code{prediction} - a dataframe containing the prediction for each 
#'     person in the test set }
#'   \item{\code{evalType} - the type of evaluation that was performed ('person'
#'     or time')}
#'   \item{\code{performanceTest} - a list detailing the size of the test sets}
#'   \item{\code{performanceTrain} - a list detailing the size of the train 
#'     sets}
#'   \item{\code{time} - the complete time taken to fit the model framework}
#' }
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
                                save = NULL,
                                saveModel = TRUE,
                                verbosity = futile.logger::INFO,
                                timeStamp = TRUE,
                                analysisId = NULL,
                                clearffTemp = FALSE,
                                minCovariateFraction = 0.001) {
  
  nrRuns <- length(trainFractions)
  
  # log the start time:
  ExecutionDateTime <- Sys.time()
  
  if (timeStamp | verbosity == TRACE) {
    flog.layout(layout.format('[~l]\t[~t]\t~m'))
  } else {
    flog.layout(layout.format('~m'))
  }
  flog.threshold(verbosity)
  originalPopulation <- population
  
  learningCurve <- foreach::foreach(i = 1:nrRuns,
                                    .combine = rbind,
                                    .errorhandling = "remove") %do% {
                                      
    # record start time
    startTime <- Sys.time()
    
    population <- originalPopulation
    # create an analysisid and folder to save the results of this run
    start.all <- Sys.time()
    if (is.null(analysisId))
      analysisId <- gsub(':', '', gsub('-', '', gsub(' ', '', start.all)))
    
    if (is.null(save))
      save <- file.path(getwd(), 'plpmodels') #if NULL save to wd
    
    analysisPath = file.path(save, analysisId)
    if (!dir.exists(analysisPath)) {
      dir.create(analysisPath, recursive = T)
    }
    logFileName = paste0(analysisPath, '/plplog.txt')
    
    # write log to both console and file (tee).
    # note other appenders can be created, e.g., to webservice or database!
    
    flog.appender(appender.tee(logFileName))
    
    flog.seperator()
    flog.info(paste0(
      'Patient-Level Prediction Package version ',
      utils::packageVersion("PatientLevelPrediction")
    ))
    
    # get ids
    cohortId <- attr(population, "metaData")$cohortId
    outcomeId <- attr(population, "metaData")$outcomeId
    
    # add header to analysis log
    flog.seperator()
    flog.info(sprintf('%-20s%s', 'Training Size: ', trainFractions[i]))
    flog.info(sprintf('%-20s%s', 'AnalysisID: ', analysisId))
    flog.info(sprintf('%-20s%s', 'CohortID: ', cohortId))
    flog.info(sprintf('%-20s%s', 'OutcomeID: ', outcomeId))
    flog.info(sprintf('%-20s%s', 'Cohort size: ', nrow(plpData$cohorts)))
    flog.info(sprintf('%-20s%s', 'Covariates: ', nrow(plpData$covariateRef)))
    flog.info(sprintf('%-20s%s', 'Population size: ', nrow(population)))
    flog.info(sprintf('%-20s%s', 'Cases: ', sum(population$outcomeCount >
                                                  0)))
    flog.seperator()
    
    # check parameters
    flog.trace('Parameter Check Started')
    checkInStringVector(testSplit, c('person', 'time'))
    checkHigherEqual(sum(population[, 'outcomeCount'] > 0), 25)
    checkIsClass(plpData, c('plpData.coo', 'plpData'))
    checkIsClass(testFraction, 'numeric')
    checkHigher(testFraction, 0)
    checkIsClass(nfold, 'numeric')
    checkHigher(nfold, 0)
    
    # construct the train and test set.
    # note that index will be zero for rows not in train or test
    if (testSplit == 'time') {
      flog.trace('Dataset time split starter')
      indexes <-
        ftry(
          timeSplitter(
            population,
            test = testFraction,
            train = trainFractions[i],
            nfold = nfold,
            seed = splitSeed
          ),
          finally = flog.trace('Done.')
        )
    }
    if (testSplit == 'person') {
      flog.trace('Dataset person split starter')
      indexes <-
        ftry(
          personSplitter(
            population,
            test = testFraction,
            train = trainFractions[i],
            nfold = nfold,
            seed = splitSeed
          ),
          finally = flog.trace('Done.')
        )
    }
    
    # TODO better to move this to the splitter if this is important?
    if (nrow(population) != nrow(indexes)) {
      flog.error(sprintf(
        'Population dimension not compatible with indexes: %d <-> %d',
        nrow(population),
        nrow(indexes)
      ))
      stop('Population dimension not compatible with indexes')
    }
    
    # train the model
    flog.seperator()
    tempmeta <- attr(population, "metaData")
    if (is.null(population$indexes)) {
      population <- merge(population, indexes)
      colnames(population)[colnames(population) == 'index'] <-
        'indexes'
    } else{
      attr(population, 'indexes') <- indexes
    }
    attr(population, "metaData") <- tempmeta
    
    settings <- list(
      data = plpData,
      minCovariateFraction = minCovariateFraction,
      modelSettings = modelSettings,
      population = population,
      cohortId = cohortId,
      outcomeId = outcomeId
    )
    
    flog.info(sprintf('Training %s model', settings$modelSettings$name))
    # the call is sinked because of the external calls (Python etc)
    if (sink.number() > 0) {
      flog.warn(paste0('sink had ', sink.number(), ' connections open!'))
    }
    sink(logFileName, append = TRUE, split = TRUE)
    
    model <- ftry(
      do.call(fitPlp, settings),
      error = function(e) {
        sink()
        flog.error(e)
        stop(e)
      }
    )
    sink()
    flog.trace('Done.')
    
    # save the model
    if (saveModel == T) {
      modelLoc <- file.path(save, analysisId, 'savedModel')
      ftry(savePlpModel(model, modelLoc))
      flog.info(paste0('Model saved to ..\\', analysisId, '\\savedModel'))
    }
    
    # calculate metrics
    flog.seperator()
    flog.trace('Prediction')
    flog.trace(paste0('Calculating prediction for ', sum(indexes$index !=
                                                           0)))
    ind <- population$rowId %in% indexes$rowId[indexes$index != 0]
    prediction <-
      model$predict(plpData = plpData, population = population[ind,])
    
    metaData <- list(
      predictionType = "binary",
      cohortId = attr(population, 'metaData')$cohortId,
      outcomeId = attr(population, 'metaData')$outcomeId
    )
    
    attr(prediction, "metaData") <- metaData
    
    finally = flog.trace('Done.')
    
    if (ifelse(is.null(prediction), FALSE, length(unique(prediction$value)) >
               1)) {
      # add analysisID
      attr(prediction, "metaData")$analysisId <- analysisId
      
      flog.info('Train set evaluation')
      performance.train <-
        evaluatePlp(prediction[prediction$indexes > 0,], plpData)
      flog.trace('Done.')
      flog.info('Test set evaluation')
      performance.test <-
        evaluatePlp(prediction[prediction$indexes < 0,], plpData)
      flog.trace('Done.')
      
      # now combine the test and train data and add analysisId
      performance <-
        reformatPerformance(train = performance.train, test = performance.test,
                            analysisId)
      
      if (!is.null(save)) {
        flog.trace('Saving evaluation')
        if (!dir.exists(file.path(analysisPath, 'evaluation')))
          dir.create(file.path(analysisPath, 'evaluation'))
        ftry(
          utils::write.csv(
            performance$evaluationStatistics,
            file.path(analysisPath, 'evaluation', 'evaluationStatistics.csv'),
            row.names = F
          ),
          finally = flog.trace('Saved EvaluationStatistics.')
        )
        ftry(
          utils::write.csv(
            performance$thresholdSummary,
            file.path(analysisPath, 'evaluation', 'thresholdSummary.csv'),
            row.names = F
          ),
          finally = flog.trace('Saved ThresholdSummary.')
        )
        ftry(
          utils::write.csv(
            performance$demographicSummary,
            file.path(analysisPath, 'evaluation', 'demographicSummary.csv'),
            row.names = F
          ),
          finally = flog.trace('Saved DemographicSummary.')
        )
        ftry(
          utils::write.csv(
            performance$calibrationSummary,
            file.path(analysisPath, 'evaluation', 'calibrationSummary.csv'),
            row.names = F
          ),
          finally = flog.trace('Saved CalibrationSummary.')
        )
        ftry(
          utils::write.csv(
            performance$predictionDistribution,
            file.path(analysisPath,
                      'evaluation',
                      'predictionDistribution.csv'),
            row.names = F
          ),
          finally = flog.trace('Saved PredictionDistribution.')
        )
      }
      flog.seperator()
      
    } else{
      flog.warn(paste0(
        'Evaluation not possible as prediciton NULL or all the same values'
      ))
      performance.test <- NULL
      performance.train <- NULL
    }
    
    # log the end time:
    endTime <- Sys.time()
    TotalExecutionElapsedTime <-
      as.numeric(difftime(endTime, ExecutionDateTime,
                          units = "secs"))
    
    timeDiff <-
      as.numeric(difftime(endTime, startTime, units = "secs"))
    
    # 1) input settings:
    inputSetting <-
      list(
        dataExtrractionSettings = plpData$metaData$call,
        populationSettings = attr(population, "metaData"),
        modelSettings = modelSettings,
        testSplit = testSplit,
        testFraction = testFraction
      )
    
    # 2) Executionsummary details:
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
        # Sys.info()
        TotalExecutionElapsedTime = TotalExecutionElapsedTime,
        ExecutionDateTime = ExecutionDateTime,
        Log = logFileName # location for now
        # Not available at the moment: CDM_SOURCE - meta-data containing CDM
        # version, release date, vocabulary version
      )
    
    flog.seperator()
    flog.info(paste0('Calculating covariate summary @ ', Sys.time()))
    flog.info('This can take a while...')
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
    if (!is.null(save)) {
      flog.trace('Saving covariate summary')
      if (!dir.exists(file.path(analysisPath, 'evaluation')))
        dir.create(file.path(analysisPath, 'evaluation'))
      ftry(
        utils::write.csv(
          covSummary,
          file.path(analysisPath, 'evaluation', 'covariateSummary.csv'),
          row.names = F
        ),
        finally = flog.trace('Saved covariate summary.')
      )
    }
    flog.info(paste0('Finished covariate summary @ ', Sys.time()))
    
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
        #analysisName,
        analysisSettings = NULL
      )
    )
    class(result) <- c('list', 'plpModel')
    
    flog.info(paste0('Log saved to ', logFileName))
    flog.info("Run finished successfully.")
    
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
    
    if (clearffTemp) {
      # Remove temporary files
      file.remove(dir(getOption("fftempdir"), full.names = TRUE))
    }
    
    # return data frame row for each process
    return(df)
  }

  names(learningCurve) <- c("x", "popSizeTrain", "outcomeCountTrain",
                          "executionTime", "trainAUCROC", "testAUCROC",
                          "trainAUCPR", "testAUCPR", "trainBrierScore",
                          "testBrierScore", "trainBrierScaled",
                          "testBrierScaled", "trainCalibrationIntercept",
                          "testCalibrationIntercept", "trainCalibrationSlope",
                          "testCalibrationSlope")
  
  return(learningCurve)
}

#' createLearningCurvePar - Creates a learning curve object in parallel
#' 
#' @description 
#' Creates a learning curve in parallel, which can be plotted using the 
#' \code{plotLearningCurve()} function. Currently this functionality is only 
#' supported by Lasso Logistic Regression.
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
#' @param analysisId Identifier for the analysis. It is used for example used in 
#'   naming the result folder. The default is a timestamp.
#' @param minCovariateFraction Minimum covariate prevalence in population to
#'   avoid removal during preprocssing.
#'
#' @return
#' An object containing the various performance measures obtained by the model.
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
                                   analysisId = NULL,
                                   minCovariateFraction = 0.001) {
  
  # register a parallel backend
  registerParallelBackend()
  
  # verify that a parallel backend has been registered
  setup_parallel()
  
  ExecutionDateTime <- Sys.time()
  
  originalPopulation <- population
  
  learningCurve <- foreach::foreach(
    i = 1:length(trainFractions),
    .combine = rbind,
    .errorhandling = "remove",
    .packages = c("doParallel",
                  "PatientLevelPrediction")
  ) %dopar% {
    
    # reset population
    population <- originalPopulation
    
    # create an analysisid of this run
    start.all <- Sys.time()
    if (is.null(analysisId))
      analysisId <-
      gsub(':', '', gsub('-', '', gsub(' ', '', start.all)))
    
    cohortId <- attr(population, "metaData")$cohortId
    outcomeId <- attr(population, "metaData")$outcomeId
    
    # construct the train and test set.
    # note that index will be zero for rows not in train or test
    # errors are handled by the foreach construct
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
    
    # train the model
    tempmeta <- attr(population, "metaData")
    if (is.null(population$indexes)) {
      population <- merge(population, indexes)
      colnames(population)[colnames(population) == 'index'] <-
        'indexes'
    } else{
      attr(population, 'indexes') <- indexes
    }
    attr(population, "metaData") <- tempmeta
    
    settings <- list(
      data = plpData,
      minCovariateFraction = minCovariateFraction,
      modelSettings = modelSettings,
      population = population,
      cohortId = cohortId,
      outcomeId = outcomeId
    )
    
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
    
    if (ifelse(is.null(prediction), FALSE, length(unique(prediction$value)) >
               1)) {
      # add analysisID
      attr(prediction, "metaData")$analysisId <- analysisId
      
      performance.train <-
        PatientLevelPrediction::evaluatePlp(prediction[prediction$indexes > 0,],
                                            plpData)
      
      performance.test <-
        PatientLevelPrediction::evaluatePlp(prediction[prediction$indexes < 0,],
                                            plpData)
    }
    
    # log the end time:
    endTime <- Sys.time()
    TotalExecutionElapsedTime <- endTime - ExecutionDateTime
    
    # return data frame row for each process
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
  
  # de-register parallel backend by registering sequential backend
  registerSequentialBackend()
  
  return(learningCurve)
}