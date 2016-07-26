# @file RunPlp.R
#
# Copyright 2015 Observational Health Data Sciences and Informatics
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

#' RunPlp - Train and evaluate the model
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
#' @param nfold                            The number of folds used in the cross validation (default 3)
#' @param indexes                          A dataframe containing a rowId and index column where the index value of -1 means in the test set, and positive integer represents the cross validation fold (default is NULL)
#' @param save                             The path to the directory where the models will be saved
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
#' \item{model}{A list of class \code{plpModel} containing the model, training metrics and model metadata}
#' \item{dataSummary}{A list detailing the size of the train/test sets and outcome prevalence}
#' \item{indexes}{The dataframe with the rowIds and indexes used to split the data into test/train and cross-validation folds}
#' \item{type}{The type of evaluation that was performed ('person' or 'time')}
#' \item{prediction}{A dataframe containing the prediction for each person in the test set }
#' \item{performance}{A list detailing the performance of the model}
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
#' mod.lr <- RunPlp(population=population,
#'                         plpData= plpData,
#'                         modelSettings = model.lr ,
#'                         testSplit = 'time', testFraction=0.3, 
#'                         nfold=3, indexes=NULL,
#'                         save=file.path('C:','User','home'))
#'  
#' #******** EXAMPLE 2 *********                                               
#' # Gradient boosting machine with a grid search to select hyper parameters  
#' # using the test/train/folds created for the lasso logistic regression above                       
#' model.gbm <- gradientBoostingMachine.set(rsampRate=c(0.5,0.9,1),csampRate=1, 
#'                            ntrees=c(10,100), bal=c(F,T),
#'                            max_depth=c(4,5), learn_rate=c(0.1,0.01))
#' mod.gbm <- RunPlp(population=population,
#'                         plpData= plpData,
#'                         modelSettings = model.gbm,
#'                         testSplit = 'time', testFraction=0.3, 
#'                         nfold=3, indexes=mod.lr$indexes,
#'                         save=file.path('C:','User','home'))
#' } 
RunPlp <- function(population, plpData,
                         modelSettings,
                         testSplit = 'time', testFraction=0.3, nfold=3, indexes=NULL,
                         save=NULL, verbosity=INFO, timeStamp=FALSE, analysisId=NULL
                         ){
  
  if (timeStamp | verbosity == TRACE){
    flog.layout(layout.format('[~l]\t[~t]\t~m'))
  } else {
    flog.layout(layout.format('~m'))
  }
  flog.threshold(verbosity)

  # create an analysisid and folder to save the results
  start.all <- Sys.time()
  if(is.null(analysisId))
    analysisId <- gsub(':','',gsub('-','',gsub(' ','',start.all)))
  #if(is.null(dirPath)) dirPath <- file.path(getwd(),'plpmodels') if NULL dont save
  
  # TODO: This will not work for example if libsvm conversion is needed and no Save is filled in.
  
  if(!is.null(save)) {
     analysisPath = file.path(save,analysisId)
     if(!dir.exists(analysisPath)){dir.create(analysisPath,recursive=T)}
     logFileName = paste0(analysisPath,'/plplog.txt')
  } else {
     analysisPath = getwd()
     logFileName = paste0(analysisPath,'/plplog.txt') 
  }
  	
  # write log to both console and file (tee). 
  # note other appenders can be created, e.g., to webservice or database!

  flog.appender(appender.tee(logFileName))
  
  flog.seperator()
  flog.info(paste0('Patient-Level Prediction Package version ', utils::packageVersion("PatientLevelPrediction")))

  # get ids
  cohortId <- attr(population, "metaData")$cohortId
  outcomeId <- attr(population, "metaData")$outcomeId
  
  # add header to analysis log
  flog.seperator()
  flog.info(sprintf('%-20s%s', 'AnalysisID: ',analysisId))
  flog.info(sprintf('%-20s%s', 'CohortID: ', cohortId))
  flog.info(sprintf('%-20s%s', 'OutcomeID: ', outcomeId))
  flog.info(sprintf('%-20s%s', 'Cohort size: ', nrow(plpData$cohorts)))
  flog.info(sprintf('%-20s%s', 'Covariates: ', nrow(plpData$covariateRef)))
  flog.info(sprintf('%-20s%s', 'Population size: ', nrow(population)))
  flog.info(sprintf('%-20s%s', 'Cases: ', sum(population$outcomeCount>0)))
  flog.seperator()
  
  # check parameters
  flog.trace('Parameter Check Started')
  checkInStringVector(testSplit, c('person','time'))
  checkHigherEqual(sum(population[,'outcomeCount']>0), 25)
  checkIsClass(plpData, c('plpData.libsvm','plpData.coo','plpData'))
  checkIsClass(testFraction, 'numeric')
  checkHigher(testFraction,0)
  checkIsClass(nfold, 'numeric')
  checkHigher(nfold, 0)
  
  # check format and convert if needed
  if(attr(modelSettings, 'libSVM')){
    plpData <- checkLibsvm(plpData, filePath=analysisPath)
    flog.seperator()
  }
  
  # construct the settings for the model pipeline
  if(is.null(indexes)){
    if(testSplit=='time'){
      flog.trace('Dataset time split starter')
      indexes <-ftry(timeSplitter(population, test=testFraction, nfold=nfold),
                     finally=flog.trace('Done.'))
    }
    if(testSplit=='person'){
      flog.trace('Dataset time split starter')
      indexes <- ftry(personSplitter(population, test=testFraction, nfold=nfold),
                      finally= flog.trace(log,'Done.')
      )
    }
  }
  
  # TODO better to move this to the splitter if this is important?
  if(nrow(population)!=nrow(indexes)){
    flog.error(sprintf('Population dimension not compatible with indexes: %d <-> %d', nrow(population), nrow(indexes)))
    stop('Population dimension not compatible with indexes')
  }
  
  # train the model
  flog.seperator()
  tempmeta <- attr(population, "metaData")
  population <- merge(population, indexes)
  colnames(population)[colnames(population)=='index'] <- 'indexes'
  attr(population, "metaData") <- tempmeta
  
  settings <- list(data=plpData,  index=indexes,
                   #featureSettings = featureSettings,
                   modelSettings = modelSettings,
                   population=population,
                   cohortId=cohortId,
                   outcomeId=outcomeId)
  
  flog.info(sprintf('Training %s model',settings$modelSettings$name))  
  # the call is sinked because of the external calls (Python etc)
  if (sink.number()>0){
    flog.warn(paste0('sink had ',sink.number,' connections open!'))    
  }
  sink(logFileName, append = TRUE, split = TRUE)

  model <- ftry(do.call(fitPlp, settings),
                error = {sink()
                  flog.error(e)
                  stop(e)},
                finally = {
                  flog.trace('Done.')})
  sink()
  
  # save the model
  if(!is.null(save)){
    modelLoc <- file.path(save,analysisId, 'savedModel' )
    ftry(savePlpModel(model, modelLoc),finally= flog.trace('Done.'))
    flog.info(paste0('Model saved to ..\\',analysisId,'\\savedModel'))
  }

  # calculate metrics
  flog.seperator()
  flog.trace('Prediction')
  prediction <- ftry(predictPlp(plpModel = model, population = population, plpData = plpData, index = NULL), 
                     finally = flog.trace('Done.'))
  if(ifelse(is.null(prediction), FALSE, length(unique(prediction$value))>1)){

    flog.info('Train set evaluation')
    performance.train <- evaluatePlp(prediction[prediction$indexes>0,])
    flog.trace('Done.')
    flog.info('Test set evaluation')
    performance.test <- evaluatePlp(prediction[prediction$indexes<0,])
    flog.trace('Done.')

    flog.trace('Saving evaluation')
    ftry(writeOutput(prediction=prediction, 
                performance.test=performance.test, 
                performance.train=performance.train, 
                plpModel=model,
                population = population,
                plpData = plpData,
                dirPath=save,
                analysisId=analysisId,
                start.all=start.all,
                testSplit=testSplit,
                modelLoc=modelLoc),
        finally= flog.trace('Done.')
    )
    flog.seperator()
    
  }else{
    flog.warn(paste0('Evaluation not possible as prediciton NULL or all the same values'))
    performance.test <- NULL
    performance.train <- NULL
  }
  
  results <- list(predict=model$predict,
                  model=model,
                  prediction=prediction,
                  #index=indexes, - moved this to model
                  evalType=testSplit,
                  performanceTest=performance.test,
                  performanceTrain=performance.train,
                  time=format(difftime(Sys.time(), start.all, units='hours'), nsmall=1))
  class(results) <- c('list','plpModel')

  flog.info(paste0('Log saved to ',logFileName))  
  flog.info("Run finished successfully.")
  return(results)
  
}


#' @export
summary.plpModel <- function(object, ...) {
  
  result <- list(cohortId=attr(object$prediction, "metaData")$cohortId,
                 outcomeId=attr(object$prediction, "metaData")$outcomeId,
                 model= object$model$modelSettings$model,
                 parameters = object$model$modelSettings$param,
                 modelTime = object$time,
                 AUC = object$performance$auc,
                 Brier = object$performance$brier,
                 BrierScaled = object$performance$brierScaled,
                 hosmerlemeshow = object$performance$hosmerlemeshow,
                 calibrationIntercept = object$performance$calibrationIntercept,
                 calibrationGradient = object$performance$calibrationGradient
                 
  )
  class(result) <- "summary.plpModel"
  return(result)
}





