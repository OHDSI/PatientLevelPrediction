#
# Copyright 2017 Observational Health Data Sciences and Informatics
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

#' learningCurve - Create a learning curve
#'
#' @description
#' #' 
#' @details
#' 
#' 
#' @param population                       The population created using createStudyPopulation() who will be used to develop the model
#' @param plpData                          An object of type \code{plpData} - the patient level prediction
#'                                         data extracted from the CDM.
#' @param modelSettings                    An object of class \code{modelSettings} created using one of the function:
#'                                         \itemize{
#'                                         \item{logisticRegressionModel()}{ A lasso logistic regression model}
#'                                         \item{GBMclassifier()}{ A gradient boosting machine}
#'                                         \item{RFclassifier()}{ A random forest model}
#'                                         \item{GLMclassifier ()}{ A generalised linear model}
#'                                         \item{KNNclassifier()}{ A KNN model}
#'                                         }
#' @param testSplit                        Either 'person' or 'time' specifying the type of evaluation used.
#'                                         'time' find the date where testFraction of patients had an index after the date and assigns patients with an index prior to this date into the training set and post the date into the test set
#'                                         'person' splits the data into test (1-testFraction of the data) and
#'                                         train (validationFraction of the data) sets.  The split is stratified by the class label.
#' @param testFraction                     The fraction of the data to be used as the test set in the patient
#'                                         split evaluation.
#' @param trainFractions                   A list of trainFractions to try 
#' @param splitSeed                        The seed used to split the test/train set when using a person type testSplit                  
#' @param nfold                            The number of folds used in the cross validation (default 3)
#' @param indexes                          A dataframe containing a rowId and index column where the index value of -1 means in the test set, and positive integer represents the cross validation fold (default is NULL)
#' @param save                             The path to the directory where the models will be saved (if NULL uses working directory)
#' @param saveModel                        Binary indicating whether to save the model once it is trained (default is T)
#' @param verbosity                        Sets the level of the verbosity. If the log level is at or higher in priority than the logger threshold, a message will print. The levels are:
#'                                         \itemize{
#'                                         \item{DEBUG}{Highest verbosity showing all debug statements}
#'                                         \item{TRACE}{Showing information about start and end of steps}
#'                                         \item{INFO}{Show informative information (Default)}
#'                                         \item{WARN}{Show warning messages}
#'                                         \item{ERROR}{Show error messages}
#'                                         \item{FATAL}{Be silent except for fatal errors}
#'                                         }
#' @param timeStamp                        If TRUE a timestamp will be added to each logging statement. Automatically switched on for TRACE level.
#' @param analysisId                       Identifier for the analysis. It is used to create, e.g., the result folder. Default is a timestamp.
#'
#' @return
#' An object containing the model or location where the model is save, the data selection settings, the preprocessing
#' and training settings as well as various performance measures obtained by the model.
#'
#' \item{predict}{A function that can be applied to new data to apply the trained model and make predictions}
#' \item{model}{A list of class \code{plpModel} containing the model, training metrics and model metadata}
#' \item{prediction}{A dataframe containing the prediction for each person in the test set }
#' \item{evalType}{The type of evaluation that was performed ('person' or 'time')}
#' \item{performanceTest}{A list detailing the size of the test sets}
#' \item{performanceTrain}{A list detailing the size of the train sets}
#' \item{time}{The complete time taken to do the model framework}
#'
#'
#' @export

createLearningCurve <- function(population, plpData,
                   modelSettings,
                   testSplit = 'time', testFraction=0.25, trainFractions = c(0.25,0.50,0.75), splitSeed=NULL, nfold=3, indexes=NULL,
                   save=NULL, saveModel=T,verbosity=futile.logger::INFO, timeStamp=FALSE, analysisId=NULL){
  
  nrRuns <- length(trainFractions);
  learningCurve <- data.frame(x = numeric(nrRuns),
                           trainAUC = integer(nrRuns),
                           testAUC = integer(nrRuns))
  
  # log the start time:
  ExecutionDateTime <- Sys.time()
  
  if (timeStamp | verbosity == TRACE){
    flog.layout(layout.format('[~l]\t[~t]\t~m'))
  } else {
    flog.layout(layout.format('~m'))
  }
  flog.threshold(verbosity)
  originalPopulation <- population
  run <- 1;
  for (trainFraction in trainFractions) {
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
    flog.info(sprintf('%-20s%s', 'Training Size: ', trainFraction))
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
          ftry(timeSplitter(population, test = testFraction,train = trainFraction, nfold = nfold),
               finally = flog.trace('Done.'))
      }
      if (testSplit == 'person') {
        flog.trace('Dataset person split starter')
        indexes <-
          ftry(
            personSplitter(population, test = testFraction, train = trainFraction, nfold = nfold, 
                           seed = splitSeed),
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
    if (is.null(population$indexes)){
    population <- merge(population, indexes)
    colnames(population)[colnames(population) == 'index'] <- 'indexes'
    } else{
      attr(population, 'indexes') <- indexes
    }
    attr(population, "metaData") <- tempmeta
    
    settings <- list(
      data = plpData,
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
      },
      finally = {
        flog.trace('Done.')
      }
    )
    sink()
    
    # save the model
    if (saveModel == T) {
      modelLoc <- file.path(save, analysisId, 'savedModel')
      ftry(savePlpModel(model, modelLoc), finally = flog.trace('Done.'))
      flog.info(paste0('Model saved to ..\\', analysisId, '\\savedModel'))
    }
    
    # calculate metrics
    flog.seperator()
    flog.trace('Prediction')
    prediction <-
      ftry(
        predictPlp(
          plpModel = model,
          population = population,
          plpData = plpData,
          index = NULL
        ),
        finally = flog.trace('Done.')
      )
    if (ifelse(is.null(prediction), FALSE, length(unique(prediction$value)) >
               1)) {
      # add analysisID
      attr(prediction, "metaData")$analysisId <- analysisId
      
      flog.info('Train set evaluation')
      performance.train <-
        evaluatePlp(prediction[prediction$indexes > 0, ], plpData)
      flog.trace('Done.')
      flog.info('Test set evaluation')
      performance.test <-
        evaluatePlp(prediction[prediction$indexes < 0, ], plpData)
      flog.trace('Done.')
      
      # now combine the test and train data and add analysisId
      performance <-
        reformatPerformance(train = performance.train, test = performance.test, analysisId)
      
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
            file.path(
              analysisPath,
              'evaluation',
              'predictionDistribution.csv'
            ),
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
    TotalExecutionElapsedTime <- endTime - ExecutionDateTime
    
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
        #Not available at the moment: CDM_SOURCE -  meta-data containing CDM version, release date, vocabulary version
      )
    
    flog.seperator()
    flog.info(paste0('Calculating covariate summary @ ', Sys.time()))
    flog.info('This can take a while...')
    covSummary <- covariateSummary(plpData, population)
    covSummary <-
      merge(model$varImp, covSummary, by = 'covariateId', all = T)
    trainCovariateSummary <-
      covariateSummary(plpData, population[population$index > 0, ])
    colnames(trainCovariateSummary)[colnames(trainCovariateSummary) != 'covariateId'] <-
      paste0('Train', colnames(trainCovariateSummary)[colnames(trainCovariateSummary) !=
                                                        'covariateId'])
    testCovariateSummary <-
      covariateSummary(plpData, population[population$index < 0, ])
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
    
    # save the current trainFraction
    learningCurve$x[run]<-trainFraction*100
    learningCurve$trainAUC[run] <- performance.train$evaluationStatistics$AUC$auc
    learningCurve$testAUC[run] <- performance.test$evaluationStatistics$AUC$auc
    run <- run + 1
  }
  return(learningCurve)

}