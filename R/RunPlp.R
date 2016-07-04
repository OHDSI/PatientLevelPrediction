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
#' @param featureSettings                  An object of class \code{featureSettings} created using one of the function:
#'                                         \itemize{
#'                                         \item{filterCovariates()}{ Filter covariate/concept/analysis ids }
#'                                         \item{GLRMfeature()}{ Non-negative matrix factorisation}
#'                                         
#'                                         }
#' @param testSplit                         Either 'person' or 'time' specifying the type of evaluation used.
#'                                         'time' find the date where testFraction of patients had an index after the date and assigns patients with an index prior to this date into the training set and post the date into the test set
#'                                         'person' splits the data into test (1-testFraction of the data) and
#'                                         train (validationFraction of the data) sets.  The split is stratified by the class label.
#' @param testFraction               The fraction of the data to be used as the test set in the patient
#'                                         split evaluation.
#' @param nfold                            The number of folds used in the cross validation (default 3)
#' @param indexes                          A dataframe containing a rowId and index column where the index value of -1 means in the test set, and positive integer represents the cross validation fold (default is NULL)
#' @param dirFold                          The path to the directory where the models will be saved
#' @param silent                           Whether to suppress output progress to the consol (default F)
#' @param log                              The location of the log file (if null uses dirFold, if dirFold also null then it is in the folder plpmodels within the current working directory)

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
#'                                     requireTimeAtRisk = T,
#'                                     minTimeAtRisk=365,
#'                                     riskWindowStart = 0,
#'                                     addExposureDaysToStart = FALSE,
#'                                     riskWindowEnd = 365,
#'                                     addExposureDaysToEnd = F)
#' 
#' #lasso logistic regression predicting outcome 200 in cohorts 10 
#' #using no feature selection with a time split evaluation with 30% in test set
#' #70% in train set where the model hyper-parameters are selected using 3-fold cross validation:
#' #and results are saved to file.path('C:','User','home')
#' model.lr <- logisticRegressionModel()
#' mod.lr <- RunPlp(population=population,
#'                         plpData= plpData,
#'                         featureSettings = NULL,
#'                         modelSettings = model.lr ,
#'                         testSplit = 'time', testFraction=0.3, 
#'                         nfold=3, indexes=NULL,
#'                         dirPath=file.path('C:','User','home'))
#'  
#' #******** EXAMPLE 2 *********                                               
#' # Gradient boosting machine with a grid search to select hyper parameters  
#' # using the test/train/folds created for the lasso logistic regression above                       
#' model.gbm <- GBMclassifier(rsampRate=c(0.5,0.9,1),csampRate=1, ntrees=c(10,100), bal=c(F,T),
#'                            max_depth=c(4,5), learn_rate=c(0.1,0.01))
#' mod.gbm <- RunPlp(population=population,
#'                         plpData= plpData,
#'                         featureSettings = NULL,
#'                         modelSettings = model.gbm,
#'                         testSplit = 'time', testFraction=0.3, 
#'                         nfold=3, indexes=mod.lr$indexes,
#'                         dirPath=file.path('C:','User','home'))
#' 
RunPlp <- function(population, plpData,
                         featureSettings=NULL,
                         modelSettings,
                         testSplit = 'time', testFraction=0.3, nfold=3, indexes=NULL,
                         dirPath=NULL, silent=F, log=NULL, analysisId=NULL
                         ){
  start.all <- Sys.time()
  if(is.null(analysisId))
    analysisId <- gsub(':','',gsub('-','',gsub(' ','',start.all)))
  
  if(is.null(dirPath)) dirPath <- file.path(getwd(),'plpmodels')
  if(!dir.exists(file.path(dirPath,analysisId))){dir.create(file.path(dirPath,analysisId),recursive=T)}
  
  # start logging connection
  if(missing(log) || is.null(log)){log <- dirPath}
  log <- file.path(log, 'plplog.txt')
  write('******************************************', file=log, append=F)
  write('Develop plp model log', file=log, append=T)
  write(paste0('Start:', Sys.time()), file=log, append=T)
  write('******************************************', file=log, append=T)
  write(paste0('Checking input started at: ', Sys.time()), file=log, append=T)
  #===================== START INPUT CHECKS ===============================#
  if(!testSplit%in%c('person','time')){
    write(paste0('#ERROR: Invalid testSplit'), file=log, append=T)
    stop('Invalid testSplit')
  }
  if(sum(population[,'outcomeCount']>0) < 25){
    write(paste0('#ERROR: Outcome too low'), file=log, append=T)
    stop('Outcome occurrence too low')
  }
  
  if(!class(plpData)%in%c('plpData.libsvm','plpData.coo','plpData')){
    write(paste0('#ERROR: Wrong plpData input'), file=log, append=T)
    stop('Wrong plpData input')
  }
  
  if(class(testFraction)!="numeric"){
    write(paste0('#ERROR: Invalid testFraction'), file=log, append=T)
    stop('testFraction must be a numeric between 0 and 1')
  }
  if(testFraction>=1){
    write(paste0('@WARNING: testFraction too high, using 0.25 instead'), file=log, append=T)
    warning('testFraction greater or equal to 1 - using 0.25 instead')
    testFraction <- 0.25
  }
  if(testFraction<=0){
    write(paste0('@WARNING: testFraction too low using 0.25 instead'), file=log, append=T)
    warning('testFraction less or equal to 0 - using 0.25 instead')
    testFraction <- 0.25
  }
  if(class(nfold)!="numeric"){
    write(paste0('#ERROR: nfold invalid'), file=log, append=T)
    stop('nfold must be an integer 1 or greater')
  }
  if(nfold<=0){
    write(paste0('@WARNING: nfold invalid - using nfold=1 instead'), file=log, append=T)
    warning('nfold less or equal to 0 - using 1 instead')
    nfold <- 1
  }
  
  # check correct plpData
  if(attr(modelSettings, 'libSVM')){
    if(class(plpData)!='plpData.libsvm'){
      write(paste0('#ERROR: Invalid plpData format'), file=log, append=T)
      stop('Model requires libsvm plpData - apply convertToLibsvm to plpData')
    }
  }
  
  # if libsvm connect to h2o:
  if(attr(modelSettings, 'libSVM')){
    maxRam <- '4g'
    systemInfo <- Sys.info()
    if(systemInfo['sysname']=="Windows"){
      maxRam <- paste0(round(memory.limit(size=NA)/1000*0.8),'g')
      #writeLines(paste0('Initiating H2o with max Ram of: ',maxRam))
      #write(paste0('-- Initiating h2o with maxRam of: ',maxRam ), file=log, append=T)
      #tryCatch(h2o::h2o.init(nthreads=-1, max_mem_size = maxRam),
      #         error =function(e) write(paste0('#ERROR: ', e), file=log, append=T),
      #         warning = function(w) write(paste0('@WARNING: ', w), file=log, append=T),
      #         finally=write(paste0('-- end of h2o initiating'), file=log, append=T)
      #)
    } else { # need to test this on mac
      #maxRam <- paste0(mem.limits(nsize=NA, vsize=NA)*0.8,'g')
      #writeLines(paste0('Initiating H2o with default Ram - initiate h2o yourself to edit this '))
      #tryCatch(h2o::h2o.init(nthreads=-1), 
      #         error =function(e) write(paste0('#ERROR: ', e), file=log, append=T),
      #         warning = function(w) write(paste0('@WARNING: ', w), file=log, append=T),
      #         finally=write(paste0('-- end of h2o initiating'), file=log, append=T)
      #)
    }
  }
  
  write(paste0('Checking input ended at: ', Sys.time()), file=log, append=T)
  #===================== END INPUT CHECKS ===============================#
    
  #================ GET COHORT/OUTCOME ==============================
  cohortId <- attr(population, "metaData")$cohortId
  outcomeId <- attr(population, "metaData")$outcomeId
  #=========================================================================
  write(paste0(' '), file=log, append=T)
  write(paste0('***************************************************'), file=log, append=T)
  write(paste0('AnalysisID: ', analysisId), file=log, append=T)
  write(paste0('CohortID: ', cohortId), file=log, append=T)
  write(paste0('OutcomeID: ', outcomeId), file=log, append=T)
  write(paste0('***************************************************'), file=log, append=T)
  write(paste0(' '), file=log, append=T)
  
  
  
  # construct the settings for the model pipeline
  if(is.null(indexes)){
    if(testSplit=='time'){
      write(paste0('Starting time test/train/validation splits at time: ', Sys.time()), file=log, append=T)
      indexes <- tryCatch(timeSplitter(population, test=testFraction, nfold=nfold, silent=silent),
                          error = function(e) stop(e),
                          warning = function(w) write(paste0('@WARNING: ', w)))
      write(paste0('Completed test/train/validation splits by time: ', Sys.time()), file=log, append=T)
    }
    if(testSplit=='person'){
      write(paste0('Starting person test/train/validation splits at time: ', Sys.time()), file=log, append=T)
      indexes <- tryCatch(personSplitter(population, test=testFraction, nfold=nfold, silent=silent),
                          error = function(e) stop(e),
                          warning = function(w) write(paste0('@WARNING: ', w)))
      write(paste0('Completed test/train/validation splits by time: ', Sys.time()), file=log, append=T)
    }
  }
  
  if(nrow(population)!=nrow(indexes)){
    write(paste0('#ERROR: Population dimension not compatible with indexes: ', nrow(population) ,'--', nrow(indexes)), file=log, append=T)
    stop('Population dimension not compatible with indexes')
  }
  tempmeta <- attr(population, "metaData")
  population <- merge(population, indexes)
  colnames(population)[colnames(population)=='index'] <- 'indexes'
  attr(population, "metaData") <- tempmeta
 
  #================= LOG POPULATION, PLPDATA, COVARIATE SETTINGS =================#
  write(paste0(' '), file=log, append=T)
  write(paste0('****************************************************'), file=log, append=T)
  
  write(paste0('-- PlpData has: ', nrow(plpData$cohorts) ,' people'), file=log, append=T)
  
  write(paste0('-- Population has: ', nrow(population) ,' rows and ', ncol(population),' columns'), file=log, append=T)
  write(paste0('-- Population has: ', sum(population$outcomeCount>0) ,' outcomes'), file=log, append=T)

  write(paste0('-- PlpData has: ', nrow(plpData$covariateRef) ,' covariates'), file=log, append=T)
  
  write(paste0('****************************************************'), file=log, append=T)
  write(paste0(' '), file=log, append=T)
  #===============================================================================#
  
  
  #================== SUMMARISE TRAIN/TEST/FOLD DATA FOR LOG =============================#
  
  # TODO add covariate summary - test/train/val frequencies
  
  #=======================================================================================#

  
  settings <- list(data=plpData,  index=indexes,
                   featureSettings = featureSettings,
                   modelSettings = modelSettings,
                   population=population, quiet=silent,
                   cohortId=cohortId,
                   outcomeId=outcomeId)
  
  #============================  TRAIN MODEL =====================================#
  write(paste0('-- Starting model training at ', Sys.time()), file=log, append=T)
  model <- tryCatch(do.call(fitPlp, settings),
                    warning = function(w)  write(paste0('@WARNING: ', w ), file=log, append=T),
                    error = function(e) stop(e),
                    finally= write(paste0('-- Finished model training at ', Sys.time()), file=log, append=T)
                    )
  
  if(!silent)
    writeLines('1: Model Trained')
  modelLoc <- file.path(dirPath,analysisId, 'savedModel' )
  tryCatch(savePlpModel(model, modelLoc),
           warning = function(w)  write(paste0('@WARNING: ', w ), file=log, append=T),
           error = function(e) stop(e),
           finally= write(paste0('-- Model (class:',class(model),') saved to ', modelLoc), file=log, append=T)
  )
  
  # TODO: LOG MODELING TRAINING START TIME, PROGRESS, END TIME + SUMMARY
  #       LOG CONVERGENCE + OPTIMAL HYPER-PARAMERTERS
  
  #==============================================================================#
  
  #==============================================================================#
  # do prediction (on all data as we want test + train model performances)
  # TODO:put all in trycatch()
  write(paste0('-- Starting prediction calculation at ', Sys.time()), file=log, append=T)

  prediction <- tryCatch(predictPlp(plpModel=model, population=population, plpData=plpData, 
                           index=NULL,silent=silent),
                         warning = function(w)  write(paste0('@WARNING: ', w ), file=log, append=T),
                         error = function(e) stop(e),
                         finally= write(paste0('-- Finished prediction at ', Sys.time()), file=log, append=T)
  )
  writeLines(paste('silent 5 ',silent))
  writeLines(paste('silent 2: ',!silent))
  if(!silent)
    writeLines('2) Prediction Calculated')
 
  # LOGGING COMPLETE UP TO HERE
  # calculate metrics
  if(ifelse(is.null(prediction), FALSE, length(unique(prediction$value))>1)){
    
    write(paste0(' '), file=log, append=T)
    write(paste0('-- Starting test set evaluation at ', Sys.time()), file=log, append=T)
    performance.test <- tryCatch(evaluatePlp(prediction[prediction$indexes<0,]),
                                 warning = function(w)  write(paste0('@WARNING: ', w ), file=log, append=T),
                                 error = function(e) stop(e),
                                 finally= write(paste0('-- Finished test set evaluation at ', Sys.time()), file=log, append=T)
    )
    writeLines(' Test set predictions evaluated')
    
    write(paste0(' '), file=log, append=T)
    write(paste0('-- Starting train set evaluation at ', Sys.time()), file=log, append=T)
    performance.train <- tryCatch(evaluatePlp(prediction[prediction$indexes>0,]),
                                  warning = function(w)  write(paste0('@WARNING: ', w ), file=log, append=T),
                                  error = function(e) stop(e),
                                  finally= write(paste0('-- Finished train set evaluation at ', Sys.time()), file=log, append=T)
    )
    writeLines(' Train set predictions evaluated')
    if(!silent)
      writeLines('3) Performance calculated')
    
    write(paste0(' '), file=log, append=T)
    write(paste0('-- Started saving evaluation at ', Sys.time()), file=log, append=T)
    tryCatch(writeOutput(prediction=prediction, 
                performance.test=performance.test, 
                performance.train=performance.train, 
                plpModel=model,
                population = population,
                plpData = plpData,
                dirPath=dirPath,
                analysisId=analysisId,
                start.all=start.all,
                testSplit=testSplit,
                modelLoc=modelLoc),
             warning = function(w)  write(paste0('@WARNING: ', w ), file=log, append=T),
             error = function(e) write(paste0('#ERROR: ', e ), file=log, append=T),
             finally= write(paste0('-- Finished saving evaluation at ', Sys.time()), file=log, append=T)
    )
    
  }else{
    write(paste0('@WARNING: evaluation not possible as prediciton NULL or all the same values'), file=log, append=T)
    performance.test <- NULL
    performance.train <- NULL
  }
  
  
  #===============================================================================#
  
  results <- list(transform=model$transform,
                  model=model,
                  prediction=prediction,
                  index=indexes,
                  evalType=testSplit,
                  performanceTest=performance.test,
                  performanceTrain=performance.train,
                  # ADD OUTPUT LOCATION?
                  time=format(difftime(Sys.time(), start.all, units='hours'), nsmall=1))
  class(results) <- c('list','plpModel')
  
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





