# @file RunPlp.R
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

#' runPlp - Train and evaluate the model
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
#' Users can define a risk period of interest for the prediction of the outcome relative to index or use
#' the cohprt dates.  The user can then specify whether they wish to exclude patients who are not observed
#' during the whole risk period, cohort period or experienced the outcome prior to the risk period.
#'
#' @param population                       The population created using createStudyPopulation() who will be used to develop the model
#' @param plpData                          An object of type \code{plpData} - the patient level prediction
#'                                         data extracted from the CDM.
#' @param minCovariateFraction             The minimum fraction of target population who must have a covariate for it to be included in the model training                            
#' @param normalizeData                    Whether to normalise the covariates before training (Default: TRUE)      
#' @param modelSettings                    An object of class \code{modelSettings} created using one of the function:
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
#' @param testSplit                        Either 'stratified', 'subject' or 'time' specifying the type of evaluation used.
#'                                         'time' find the date where testFraction of patients had an index after the date and assigns patients with an index prior to this date into the training set and post the date into the test set
#'                                         'stratified' splits the data into test (1-testFraction of the data) and
#'                                         train (validationFraction of the data) sets.  The split is stratified by the class label. 'subject' split is useful
#'                                         when a subject is in the data multiple times and you want all rows for the same subject in either the test or the train set but not in both.
#' @param testFraction                     The fraction of the data to be used as the test set in the patient
#'                                         split evaluation.
#' @param trainFraction                    A real number between 0 and 1 indicating the train set fraction of the data.
#'                                         If not set trainFraction is equal to 1 - test
#' @param splitSeed                        The seed used to split the test/train set when using a person type testSplit                  
#' @param nfold                            The number of folds used in the cross validation (default 3)
#' @param indexes                          A dataframe containing a rowId and index column where the index value of -1 means in the test set, and positive integer represents the cross validation fold (default is NULL)
#' @param saveDirectory                    The path to the directory where the results will be saved (if NULL uses working directory)
#' @param savePlpData                      Binary indicating whether to save the plpData object (default is T)
#' @param savePlpResult                    Binary indicating whether to save the object returned by runPlp (default is T)
#' @param savePlpPlots                     Binary indicating whether to save the performance plots as pdf files (default is T)
#' @param saveEvaluation                   Binary indicating whether to save the oerformance as csv files (default is T)
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
#' @param save                             Old input - please now use saveDirectory
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
#' @examples
#' \dontrun{
#' #******** EXAMPLE 1 ********* 
#' #load plpData:
#' plpData <- loadPlpData(file.path('C:','User','home','data'))
#' 
#' #create study population to develop model on
#' #require minimum of 365 days observation prior to at risk start
#' #no prior outcome and person must be observed for 365 after index (minTimeAtRisk)
#' #with risk window from 0 to 365 days after index
#' population <- createStudyPopulation(plpData,outcomeId=2042,
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
#' #lasso logistic regression predicting outcome 200 in cohorts 10 
#' #using no feature selection with a time split evaluation with 30% in test set
#' #70% in train set where the model hyper-parameters are selected using 3-fold cross validation:
#' #and results are saved to file.path('C:','User','home')
#' model.lr <- lassoLogisticRegression.set()
#' mod.lr <- runPlp(population=population,
#'                         plpData= plpData, minCovariateFraction = 0.001,
#'                         modelSettings = model.lr ,
#'                         testSplit = 'time', testFraction=0.3, 
#'                         nfold=3, indexes=NULL,
#'                         saveDirectory =file.path('C:','User','myPredictionName'),
#'                         verbosity='INFO')
#'  
#' #******** EXAMPLE 2 *********                                               
#' # Gradient boosting machine with a grid search to select hyper parameters  
#' # using the test/train/folds created for the lasso logistic regression above                       
#' model.gbm <- gradientBoostingMachine.set(rsampRate=c(0.5,0.9,1),csampRate=1, 
#'                            ntrees=c(10,100), bal=c(F,T),
#'                            max_depth=c(4,5), learn_rate=c(0.1,0.01))
#' mod.gbm <- runPlp(population=population,
#'                         plpData= plpData,
#'                         modelSettings = model.gbm,
#'                         testSplit = 'time', testFraction=0.3, 
#'                         nfold=3, indexes=mod.lr$indexes,
#'                         saveDirectory =file.path('C:','User','myPredictionName2'))
#' } 
runPlp <- function(population, plpData,  minCovariateFraction = 0.001, normalizeData=T,
                   modelSettings,
                   testSplit = 'stratified', testFraction=0.25, trainFraction = NULL, splitSeed=NULL, nfold=3, indexes=NULL,
                   saveDirectory=NULL, savePlpData=T,
                   savePlpResult=T, savePlpPlots = T, saveEvaluation = T,
                   verbosity="INFO", timeStamp=FALSE, analysisId=NULL, 
                   save=NULL
){
  
  if(!missing(save)){
    warning('save has been replaced with saveDirectory - please use this input from now on')
    if(is.null(saveDirectory)){saveDirectory <- save}
  }
  
  if(missing(verbosity)){
    verbosity <- "INFO"
  } else{
    if(!verbosity%in%c("DEBUG","TRACE","INFO","WARN","FATAL","ERROR", "NONE")){
      stop('Incorrect verbosity string')
    }
  }
  
  # log the start time:
  ExecutionDateTime <- Sys.time()
  
  # create an analysisid and folder to save the results
  start.all <- Sys.time()
  if(is.null(analysisId))
    analysisId <- gsub(':','',gsub('-','',gsub(' ','',start.all)))
  
  if(is.null(saveDirectory)){
    analysisPath <- file.path(getwd(),analysisId)
  } else {
    analysisPath <- file.path(saveDirectory,analysisId) 
  }
  
  if(verbosity!="NONE"){
    if(!dir.exists(analysisPath)){dir.create(analysisPath,recursive=T)}
  }
  logFileName = paste0(analysisPath,'/plplog.txt')
  
  # write log to both console and file (tee). 
  # note other appenders can be created, e.g., to webservice or database!
  clearLoggerType("PLP Log")
  if(verbosity!="NONE"){
    logger <- ParallelLogger::createLogger(name = "PLP Log",
                                           threshold = verbosity,
                                           appenders = list(ParallelLogger::createFileAppender(layout = ParallelLogger::layoutParallel,
                                                                                               fileName = logFileName)))
    ParallelLogger::registerLogger(logger)
  }
  
  ParallelLogger::logInfo(paste0('Patient-Level Prediction Package version ', utils::packageVersion("PatientLevelPrediction")))
  
  # get ids
  cohortId <- attr(population, "metaData")$cohortId
  outcomeId <- attr(population, "metaData")$outcomeId
  
  # add header to analysis log
  ParallelLogger::logInfo(sprintf('%-20s%s', 'AnalysisID: ',analysisId))
  ParallelLogger::logInfo(sprintf('%-20s%s', 'CohortID: ', cohortId))
  ParallelLogger::logInfo(sprintf('%-20s%s', 'OutcomeID: ', outcomeId))
  ParallelLogger::logInfo(sprintf('%-20s%s', 'Cohort size: ', nrow(plpData$cohorts)))
  ParallelLogger::logInfo(sprintf('%-20s%s', 'Covariates: ', nrow(plpData$covariateRef)))
  ParallelLogger::logInfo(sprintf('%-20s%s', 'Population size: ', nrow(population)))
  ParallelLogger::logInfo(sprintf('%-20s%s', 'Cases: ', sum(population$outcomeCount>0)))
  
  # check parameters
  ParallelLogger::logTrace('Parameter Check Started')
  ParallelLogger::logDebug(paste0('testSplit: ', testSplit))
  checkInStringVector(testSplit, c('person','time', 'stratified','subject'))
  ParallelLogger::logDebug(paste0('outcomeCount: ', sum(population[,'outcomeCount']>0)))
  checkHigherEqual(sum(population[,'outcomeCount']>0), 25)
  ParallelLogger::logDebug(paste0('plpData class: ', class(plpData)))
  checkIsClass(plpData, c('plpData'))
  ParallelLogger::logDebug(paste0('testfraction: ', testFraction))
  checkIsClass(testFraction, c('numeric','integer'))
  checkHigher(testFraction,0)
  checkHigher(-1*testFraction,-1)
  ParallelLogger::logDebug(paste0('nfold class: ', class(nfold)))
  ParallelLogger::logDebug(paste0('nfold: ', nfold))
  checkIsClass(nfold, c('numeric','integer'))
  checkHigher(nfold, 0)
  
  # if savePlpData
  if(savePlpData){
    ParallelLogger::logInfo(sprintf('%-20s%s', 'Saving plpData to ', file.path(analysisPath,'plpData')))
    savePlpData(plpData, file.path(analysisPath,'plpData'))
  }
  
  # construct the settings for the model pipeline
  if(is.null(indexes)){
    if(testSplit=='stratified'){
      ParallelLogger::logTrace('Dataset stratified split started')
      if(is.null(splitSeed)){ #keep record of splitSeed
        splitSeed <- sample(20000000,1)-10000000
        ParallelLogger::logInfo(paste0('splitSeed: ', splitSeed))
      } 
      indexes <-tryCatch(randomSplitter(population, test=testFraction, train = trainFraction, nfold=nfold, seed=splitSeed),
                         finally=ParallelLogger::logTrace('Done.'))
    }
    if(testSplit=='subject'){
      ParallelLogger::logTrace('Dataset subject split started')
      if(is.null(splitSeed)){ #keep record of splitSeed
        splitSeed <- sample(20000000,1)-10000000
        ParallelLogger::logInfo(paste0('splitSeed: ', splitSeed))
      } 
      indexes <-tryCatch(subjectSplitter(population, test=testFraction, train = trainFraction, nfold=nfold, seed=splitSeed),
                         finally=ParallelLogger::logTrace('Done.'))
    }
    if(testSplit=='time'){
      ParallelLogger::logTrace('Dataset time split started')
      indexes <-tryCatch(timeSplitter(population, test=testFraction, train = trainFraction, nfold=nfold),
                         finally=ParallelLogger::logTrace('Done.'))
    }
    if(testSplit=='person'){
      ParallelLogger::logTrace('Dataset person split started')
      if(is.null(splitSeed)){ #keep record of splitSeed
        splitSeed <- sample(20000000,1)-10000000
        ParallelLogger::logInfo(paste0('splitSeed: ', splitSeed))
      } 
      indexes <- tryCatch(personSplitter(population, test=testFraction, train = trainFraction, nfold=nfold, seed=splitSeed),
                          finally= ParallelLogger::logTrace('Done.')
      )
    }
  }
  
  if(nrow(population)!=nrow(indexes)){
    ParallelLogger::logError(sprintf('Population dimension not compatible with indexes: %d <-> %d', nrow(population), nrow(indexes)))
    stop('Population dimension not compatible with indexes')
  }
  
  # train the model
  tempmeta <- attr(population, "metaData")
  population <- merge(population, indexes, by = 'rowId')
  colnames(population)[colnames(population)=='index'] <- 'indexes'
  attr(population, "metaData") <- tempmeta
  
  settings <- list(data=plpData, minCovariateFraction=minCovariateFraction,
                   normalizeData = normalizeData,
                   modelSettings = modelSettings,
                   population=population,
                   cohortId=cohortId,
                   outcomeId=outcomeId)
  
  ParallelLogger::logInfo(sprintf('Training %s model',settings$modelSettings$name))  
  # the call is sinked because of the external calls (Python etc)
  if (sink.number()>0){
    ParallelLogger::logWarn(paste0('sink had ',sink.number(),' connections open!'))
  }
  #sink(logFileName, append = TRUE, split = TRUE)
  
  model <- tryCatch(do.call(fitPlp, settings),
                    error = function(e) {
                      stop(paste0(e))},
                    finally = {
                      ParallelLogger::logTrace('Done.')})
  model$analysisId <- analysisId # adding this so we can link validation to models
  
  # get train prediction and remove it from model
  predictionTrain <- model$predictionTrain
  model$predictionTrain <- NULL
  
  # create test subset of population
  populationTest <- population[population$indexes<0,]
  attr(populationTest, 'metaData') <- attr(population, 'metaData')
  # calculate metrics
  ParallelLogger::logTrace('Prediction')
  predictionTest <- tryCatch(predictPlp(plpModel = model, 
                                        population = populationTest, #population, 
                                        plpData = plpData, 
                                        index = NULL), 
                             finally = ParallelLogger::logTrace('Done.'))
  
  prediction <- rbind(predictionTest, predictionTrain[,colnames(predictionTest)])
  
  ParallelLogger::logDebug(paste0('prediction null: ', is.null(prediction)))
  ParallelLogger::logDebug(paste0('prediction unique values: ', length(unique(prediction$value))))
  if(ifelse(is.null(prediction), FALSE, length(unique(prediction$value))>1)){
    
    # add analysisID
    attr(prediction, "metaData")$analysisId <- analysisId
    
    ParallelLogger::logInfo('Train set evaluation')
    performance.train <- evaluatePlp(prediction[prediction$indexes>0,], plpData)
    ParallelLogger::logTrace('Done.')
    ParallelLogger::logInfo('Test set evaluation')
    performance.test <- evaluatePlp(prediction[prediction$indexes<0,], plpData)
    ParallelLogger::logTrace('Done.')
    
    # now combine the test and train data and add analysisId
    performance <- reformatPerformance(train=performance.train, test=performance.test, analysisId)
    
    if(saveEvaluation){
      ParallelLogger::logTrace('Saving evaluation csv files')
      if(!dir.exists( file.path(analysisPath, 'evaluation') ))
        dir.create(file.path(analysisPath, 'evaluation'))
      tryCatch(utils::write.csv(performance$evaluationStatistics, file.path(analysisPath, 'evaluation', 'evaluationStatistics.csv'), row.names=F ),
               finally= ParallelLogger::logTrace('Saved EvaluationStatistics.')
      )
      tryCatch(utils::write.csv(performance$thresholdSummary, file.path(analysisPath, 'evaluation', 'thresholdSummary.csv'), row.names=F ),
               finally= ParallelLogger::logTrace('Saved ThresholdSummary.')
      )
      tryCatch(utils::write.csv(performance$demographicSummary, file.path(analysisPath, 'evaluation', 'demographicSummary.csv'), row.names=F),
               finally= ParallelLogger::logTrace('Saved DemographicSummary.')
      )
      tryCatch(utils::write.csv(performance$calibrationSummary, file.path(analysisPath, 'evaluation', 'calibrationSummary.csv'), row.names=F),
               finally= ParallelLogger::logTrace('Saved CalibrationSummary.')
      )
      tryCatch(utils::write.csv(performance$predictionDistribution, file.path(analysisPath, 'evaluation', 'predictionDistribution.csv'), row.names=F),
               finally= ParallelLogger::logTrace('Saved PredictionDistribution.')
      )
    }
    
  }else{
    ParallelLogger::logWarn(paste0('Evaluation not possible as prediciton NULL or all the same values'))
    performance.test <- NULL
    performance.train <- NULL
    performance <- NULL
  }
  
  # log the end time:
  endTime <- Sys.time()
  TotalExecutionElapsedTime <- endTime-ExecutionDateTime
  
  # 1) input settings:
  inputSetting <- list(dataExtrractionSettings=plpData$metaData$call,
                       populationSettings=attr(population, "metaData"),
                       modelSettings = modelSettings,
                       testSplit = testSplit, 
                       testFraction= testFraction,
                       nfold=nfold,
                       splitSeed = splitSeed)
  
  # 2) Executionsummary details:
  executionSummary <- list(PackageVersion = list(rVersion= R.Version()$version.string,
                                                 packageVersion = utils::packageVersion("PatientLevelPrediction")),
                           PlatformDetails= list(platform= R.Version()$platform,
                                                 cores= Sys.getenv('NUMBER_OF_PROCESSORS'),
                                                 RAM=utils::memory.size()), #  test for non-windows needed
                           # Sys.info()
                           TotalExecutionElapsedTime = TotalExecutionElapsedTime,
                           ExecutionDateTime = ExecutionDateTime,
                           Log = logFileName # location for now
                           #Not available at the moment: CDM_SOURCE -  meta-data containing CDM version, release date, vocabulary version
  )
  
  ParallelLogger::logInfo(paste0('Calculating covariate summary @ ', Sys.time()))
  ParallelLogger::logInfo('This can take a while...')
  covSummary <- covariateSummary(plpData, population)
  if(exists("model")){
    if(!is.null(model$varImp)){
      covSummary <- merge(model$varImp[,colnames(model$varImp)!='covariateName'], covSummary, by='covariateId', all=T)
    }
  }
  trainCovariateSummary <- covariateSummary(plpData, population[population$index>0,])
  trainCovariateSummary <- trainCovariateSummary[,colnames(trainCovariateSummary)!='covariateName']
  colnames(trainCovariateSummary)[colnames(trainCovariateSummary)!='covariateId'] <- paste0('Train',colnames(trainCovariateSummary)[colnames(trainCovariateSummary)!='covariateId'])
  testCovariateSummary <- covariateSummary(plpData, population[population$index<0,])
  testCovariateSummary <- testCovariateSummary[,colnames(testCovariateSummary)!='covariateName']
  colnames(testCovariateSummary)[colnames(testCovariateSummary)!='covariateId'] <- paste0('Test',colnames(testCovariateSummary)[colnames(testCovariateSummary)!='covariateId'])
  covSummary <- merge(covSummary,trainCovariateSummary, by='covariateId', all=T)
  covSummary <- merge(covSummary,testCovariateSummary, by='covariateId', all=T)
  
  # make covariateValue 0 if NA
  if('covariateValue'%in%colnames(covSummary)){
    covSummary$covariateValue[is.na(covSummary$covariateValue)] <- 0
  }
  
  
  if(saveEvaluation){
    ParallelLogger::logTrace('Saving covariate summary as csv')
    if(!dir.exists( file.path(analysisPath, 'evaluation') ))
      dir.create(file.path(analysisPath, 'evaluation'))
    tryCatch(utils::write.csv(covSummary, file.path(analysisPath, 'evaluation', 'covariateSummary.csv'), row.names=F ),
             finally= ParallelLogger::logTrace('Saved covariate summary.')
    )
  }
  ParallelLogger::logInfo(paste0('Finished covariate summary @ ', Sys.time()))
  
  results <- list(inputSetting=inputSetting,
                  executionSummary=executionSummary,
                  model=model,
                  prediction=prediction,
                  performanceEvaluation=performance,
                  covariateSummary=covSummary,
                  analysisRef=list(analysisId=analysisId,
                                   analysisName=NULL,#analysisName,
                                   analysisSettings= NULL))
  class(results) <- c('runPlp')
  
  # save the plots?
  if(savePlpPlots & !is.null(performance)){
    plotPlp(result = results, filename = file.path(analysisPath))
  }
  
  # save the results
  if(savePlpResult){
    ParallelLogger::logInfo(paste0('Saving PlpResult'))
    tryCatch(savePlpResult(results, file.path(analysisPath,'plpResult')),
             finally= ParallelLogger::logTrace('Done.'))
    ParallelLogger::logInfo(paste0('plpResult saved to ..\\', analysisPath ,'\\plpResult'))
    
    # update from temp location to saved location
    results$model <- updateModelLocation(results$model, file.path(analysisPath,'plpResult'))
  }
  
  
  if(verbosity!="NONE"){
    ParallelLogger::logInfo(paste0('Log saved to ',logFileName))  
  }
  ParallelLogger::logInfo("Run finished successfully.")
  
  # stop logger
  ParallelLogger::clearLoggers()
  logger <- ParallelLogger::createLogger(name = "SIMPLE",
                                         threshold = "INFO",
                                         appenders = list(ParallelLogger::createConsoleAppender(layout = ParallelLogger::layoutTimestamp)))
  ParallelLogger::registerLogger(logger)
  
  return(results)
  
}


#' @export
summary.plpModel <- function(object, ...) {
  
  if(object$model$modelSettings$model=="lr_lasso")
    hyper <-  paste0("The final model hyper-parameters were - variance: ",format(as.double(object$model$hyperParamSearch['priorVariance']), digits = 5))
  if(is.null(object$model$hyperParamSearch)){
    hyper <- 'No hyper-parameters...'
  } else {
    finalmod <- object$model$hyperParamSearch[which.max(object$model$hyperParamSearch$cv_auc),]
    finalmod <- finalmod[,!colnames(finalmod)%in%c('seed','cv_auc')]
    hyper <- paste0("The final model hyper-parameters were -", 
                    paste(colnames(finalmod), finalmod, collapse='; ', sep=': ')
    )
  }
  
  writeLines(paste0("The study was started at: ", object$executionSummary$ExecutionDateTime, 
                    " and took at total of ", as.double(object$executionSummary$TotalExecutionElapsedTime, unit='mins'),
                    " minutes.  ", hyper))
  
  aucInd <- object$performanceEvaluation$evaluationStatistics[,'Eval']=='test' & 
    object$performanceEvaluation$evaluationStatistics[,'Metric']%in%c('auc','AUC.auc')
  
  brierScoreInd <- object$performanceEvaluation$evaluationStatistics[,'Eval']=='test' & 
    object$performanceEvaluation$evaluationStatistics[,'Metric']%in%c('BrierScore')
  
  brierScaledInd <- object$performanceEvaluation$evaluationStatistics[,'Eval']=='test' & 
    object$performanceEvaluation$evaluationStatistics[,'Metric']%in%c('BrierScaled')
  
  calibrationSlopeInd <- object$performanceEvaluation$evaluationStatistics[,'Eval']=='test' & 
    object$performanceEvaluation$evaluationStatistics[,'Metric']%in%c('CalibrationSlope.Gradient')
  
  calibrationInterceptInd <- object$performanceEvaluation$evaluationStatistics[,'Eval']=='test' & 
    object$performanceEvaluation$evaluationStatistics[,'Metric']%in%c('CalibrationIntercept.Intercept')
  
  result <- list(cohortId=attr(object$prediction, "metaData")$cohortId,
                 outcomeId=attr(object$prediction, "metaData")$outcomeId,
                 model= object$model$modelSettings$model,
                 parameters = object$model$modelSettings$param,
                 hyperParamsearch = object$model$hyperParamSearch,
                 elaspsedTime = object$executionSummary$TotalExecutionElapsedTime,
                 AUC = object$performanceEvaluation$evaluationStatistics[aucInd,'Value'],
                 BrierScore = object$performanceEvaluation$evaluationStatistics[brierScoreInd,'Value'],
                 BrierScaled = object$performanceEvaluation$evaluationStatistics[brierScaledInd,'Value'],
                 CalibrationIntercept = object$performanceEvaluation$evaluationStatistics[calibrationInterceptInd,'Value'],
                 CalibrationSlope = object$performanceEvaluation$evaluationStatistics[calibrationSlope,'Value']
                 
  )
  class(result) <- "summary.plpModel"
  return(result)
}



# this function calcualtes:
# CovariateCount	CovariateCountWithOutcome	
# CovariateCountWithNoOutcome	CovariateMeanWithOutcome	
# CovariateMeanWithNoOutcome	CovariateStDevWithOutcome	
# CovariateStDevWithNoOutcome	CovariateStandardizedMeanDifference
covariateSummary <- function(plpData, population){
  #===========================
  # all 
  #===========================
  ppl <- ff::as.ff(population$rowId)
  idx <- ffbase::ffmatch(x = plpData$covariates$rowId, table = ppl)
  idx <- ffbase::ffwhich(idx, !is.na(idx))
  covariates <- plpData$covariates[idx, ]
  
  covariates$ones <- ff::as.ff(rep(1, length(covariates$covariateValue)))
  grp_qty <- bySumFf(covariates$ones, covariates$covariateId)
  
  allPeople <- data.frame(covariateId=ff::as.ram(grp_qty$bins), 
                          CovariateCount=ff::as.ram(grp_qty$sums))
  
  #===========================
  # outcome prevs
  #===========================
  ppl <- ff::as.ff(population$rowId[population$outcomeCount==1])
  idx <- ffbase::ffmatch(x = plpData$covariates$rowId, table = ppl)
  idx <- ffbase::ffwhich(idx, !is.na(idx))
  covariates <- plpData$covariates[idx, ]
  
  covariates$ones <- ff::as.ff(rep(1, length(covariates$covariateValue)))
  covariates$squared <- covariates$covariateValue^2
  
  lengths <- bySumFf(covariates$ones, covariates$covariateId)
  sumval <- bySumFf(covariates$covariateValue, covariates$covariateId)
  sumvalsquared <- bySumFf(covariates$squared, covariates$covariateId)
  outPeople <- data.frame(covariateId=lengths$bins, 
                          CovariateCountWithOutcome=lengths$sums,
                          CovariateMeanWithOutcome=sumval$sums/length(ppl),
                          CovariateStDevWithOutcome=  sqrt( (sumvalsquared$sums-(sumval$sums^2)/length(ppl) )/(length(ppl)-1)  ))
  
  #===========================
  # non-outcome prevs
  #===========================
  ppl <- ff::as.ff(population$rowId[population$outcomeCount==0])
  idx <- ffbase::ffmatch(x = plpData$covariates$rowId, table = ppl)
  idx <- ffbase::ffwhich(idx, !is.na(idx))
  covariates <- plpData$covariates[idx, ]
  
  
  covariates$ones <- ff::as.ff(rep(1, length(covariates$covariateValue)))
  covariates$squared <- covariates$covariateValue^2
  
  lengths <- bySumFf(covariates$ones, covariates$covariateId)
  sumval <- bySumFf(covariates$covariateValue, covariates$covariateId)
  sumvalsquared <- bySumFf(covariates$squared, covariates$covariateId)
  noOutPeople <- data.frame(covariateId=lengths$bins, 
                            CovariateCountWithNoOutcome=lengths$sums,
                            CovariateMeanWithNoOutcome=sumval$sums/length(ppl),
                            CovariateStDevWithNoOutcome=  sqrt( (sumvalsquared$sums-(sumval$sums^2)/length(ppl) )/(length(ppl)-1)  ))
  
  # now merge the predictors with prev.out and prev.noout
  prevs <- merge(merge(allPeople,outPeople, all=T), noOutPeople, all=T)
  prevs[is.na(prevs)] <- 0
  
  prevs <- merge(ff::as.ram(plpData$covariateRef[,c('covariateName','covariateId')]), prevs, by='covariateId')
  
  return(prevs)
  
}

characterize <- function(plpData, population, N=1){
  #===========================
  # all 
  #===========================
  popCount <- nrow(plpData$cohorts)
  if(!missing(population)){
    ppl <- ff::as.ff(population$rowId)
    idx <- ffbase::ffmatch(x = plpData$covariates$rowId, table = ppl)
    idx <- ffbase::ffwhich(idx, !is.na(idx))
    covariates <- plpData$covariates[idx, ]
    popCount <- nrow(population)
  }
  
  covariates$ones <- ff::as.ff(rep(1, length(covariates$covariateValue)))
  grp_qty <- bySumFf(covariates$ones, covariates$covariateId)
  
  ind <- ff::as.ram(grp_qty)>=N
  
  allPeople <- data.frame(covariateId=ff::as.ram(grp_qty$bins)[ind], 
                          CovariateCount=ff::as.ram(grp_qty$sums)[ind],
                          CovariateFraction = ff::as.ram(grp_qty$sums)[ind]/popCount)
  
  return(allPeople)
}