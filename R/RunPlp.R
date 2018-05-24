# @file RunPlp.R
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
#'                         save=file.path('C:','User','home'),
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
#'                         save=file.path('C:','User','home'))
#' } 
runPlp <- function(population, plpData,  minCovariateFraction = 0.001,
                   modelSettings,
                   testSplit = 'time', testFraction=0.25, splitSeed=NULL, nfold=3, indexes=NULL,
                   save=NULL, saveModel=T,
                   verbosity=futile.logger::INFO, timeStamp=FALSE, analysisId=NULL
){
  
  # log the start time:
  ExecutionDateTime <- Sys.time()
  
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
  
  if(is.null(save)) save <- file.path(getwd(),'plpmodels') #if NULL save to wd
  
  # TODO: This will not work for example if libsvm conversion is needed and no Save is filled in.
  
  analysisPath = file.path(save,analysisId)
  if(!dir.exists(analysisPath)){dir.create(analysisPath,recursive=T)}
  logFileName = paste0(analysisPath,'/plplog.txt')
  
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
  checkIsClass(plpData, c('plpData'))
  checkIsClass(testFraction, 'numeric')
  checkHigher(testFraction,0)
  checkHigher(-1*testFraction,-1)
  checkIsClass(nfold, 'numeric')
  checkHigher(nfold, 0)
  

  # construct the settings for the model pipeline
  if(is.null(indexes)){
    if(testSplit=='time'){
      flog.trace('Dataset time split starter')
      indexes <-ftry(timeSplitter(population, test=testFraction, nfold=nfold),
                     finally=flog.trace('Done.'))
    }
    if(testSplit=='person'){
      flog.trace('Dataset person split starter')
      if(is.null(splitSeed)){ splitSeed <- sample(20000000,1)-10000000} #keep record of splitSeed
      indexes <- ftry(personSplitter(population, test=testFraction, nfold=nfold, seed=splitSeed),
                      finally= flog.trace('Done.')
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
  
  settings <- list(data=plpData, minCovariateFraction=minCovariateFraction,
                   modelSettings = modelSettings,
                   population=population,
                   cohortId=cohortId,
                   outcomeId=outcomeId)
  
  flog.info(sprintf('Training %s model',settings$modelSettings$name))  
  # the call is sinked because of the external calls (Python etc)
  if (sink.number()>0){
    flog.warn(paste0('sink had ',sink.number(),' connections open!'))
  }
  sink(logFileName, append = TRUE, split = TRUE)
  
  model <- ftry(do.call(fitPlp, settings),
                error = function(e) {sink()
                  flog.error(e)
                  stop(e)},
                finally = {
                  flog.trace('Done.')})
  sink()
  
  # save the model
  if(saveModel==T){
    modelLoc <- file.path(save,analysisId, 'savedModel' )
    ftry(savePlpModel(model, modelLoc),finally= flog.trace('Done.'))
    flog.info(paste0('Model saved to ..\\',analysisId,'\\savedModel'))
    
    #update the python saved location
    if(attr(model, 'type')=='python'){
      model$model <- file.path(modelLoc,'python_model')
      model$predict <- createTransform(model)
    }
  }
  
  # calculate metrics
  flog.seperator()
  flog.trace('Prediction')
  prediction <- ftry(predictPlp(plpModel = model, population = population, plpData = plpData, index = NULL), 
                     finally = flog.trace('Done.'))
  if(ifelse(is.null(prediction), FALSE, length(unique(prediction$value))>1)){
    
    # add analysisID
    attr(prediction, "metaData")$analysisId <- analysisId
    
    flog.info('Train set evaluation')
    performance.train <- evaluatePlp(prediction[prediction$indexes>0,], plpData)
    flog.trace('Done.')
    flog.info('Test set evaluation')
    performance.test <- evaluatePlp(prediction[prediction$indexes<0,], plpData)
    flog.trace('Done.')
    
    # now combine the test and train data and add analysisId
    performance <- reformatPerformance(train=performance.train, test=performance.test, analysisId)
    
    if(!is.null(save)){
      flog.trace('Saving evaluation')
      if(!dir.exists( file.path(analysisPath, 'evaluation') ))
        dir.create(file.path(analysisPath, 'evaluation'))
      ftry(utils::write.csv(performance$evaluationStatistics, file.path(analysisPath, 'evaluation', 'evaluationStatistics.csv'), row.names=F ),
           finally= flog.trace('Saved EvaluationStatistics.')
      )
      ftry(utils::write.csv(performance$thresholdSummary, file.path(analysisPath, 'evaluation', 'thresholdSummary.csv'), row.names=F ),
           finally= flog.trace('Saved ThresholdSummary.')
      )
      ftry(utils::write.csv(performance$demographicSummary, file.path(analysisPath, 'evaluation', 'demographicSummary.csv'), row.names=F),
           finally= flog.trace('Saved DemographicSummary.')
      )
      ftry(utils::write.csv(performance$calibrationSummary, file.path(analysisPath, 'evaluation', 'calibrationSummary.csv'), row.names=F),
           finally= flog.trace('Saved CalibrationSummary.')
      )
      ftry(utils::write.csv(performance$predictionDistribution, file.path(analysisPath, 'evaluation', 'predictionDistribution.csv'), row.names=F),
           finally= flog.trace('Saved PredictionDistribution.')
      )
    }
    flog.seperator()
    
  }else{
    flog.warn(paste0('Evaluation not possible as prediciton NULL or all the same values'))
    performance.test <- NULL
    performance.train <- NULL
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
  
  flog.seperator()
  flog.info(paste0('Calculating covariate summary @ ', Sys.time()))
  flog.info('This can take a while...')
  covSummary <- covariateSummary(plpData, population)
  covSummary <- merge(model$varImp[,colnames(model$varImp)!='covariateName'], covSummary, by='covariateId', all=T)
  trainCovariateSummary <- covariateSummary(plpData, population[population$index>0,])
  trainCovariateSummary <- trainCovariateSummary[,colnames(trainCovariateSummary)!='covariateName']
  colnames(trainCovariateSummary)[colnames(trainCovariateSummary)!='covariateId'] <- paste0('Train',colnames(trainCovariateSummary)[colnames(trainCovariateSummary)!='covariateId'])
  testCovariateSummary <- covariateSummary(plpData, population[population$index<0,])
  testCovariateSummary <- testCovariateSummary[,colnames(testCovariateSummary)!='covariateName']
  colnames(testCovariateSummary)[colnames(testCovariateSummary)!='covariateId'] <- paste0('Test',colnames(testCovariateSummary)[colnames(testCovariateSummary)!='covariateId'])
  covSummary <- merge(covSummary,trainCovariateSummary, by='covariateId', all=T)
  covSummary <- merge(covSummary,testCovariateSummary, by='covariateId', all=T)
  if(!is.null(save)){
    flog.trace('Saving covariate summary')
    if(!dir.exists( file.path(analysisPath, 'evaluation') ))
      dir.create(file.path(analysisPath, 'evaluation'))
    ftry(utils::write.csv(covSummary, file.path(analysisPath, 'evaluation', 'covariateSummary.csv'), row.names=F ),
         finally= flog.trace('Saved covariate summary.')
    )}
  flog.info(paste0('Finished covariate summary @ ', Sys.time()))
  
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
  
  flog.info(paste0('Log saved to ',logFileName))  
  flog.info("Run finished successfully.")
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