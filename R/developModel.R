# @file developModelFramework.R
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

#' developModel - Train and evaluate the model
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
#' mod.lr <- developModel(population=population,
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
#' mod.gbm <- developModel(population=population,
#'                         plpData= plpData,
#'                         featureSettings = NULL,
#'                         modelSettings = model.gbm,
#'                         testSplit = 'time', testFraction=0.3, 
#'                         nfold=3, indexes=mod.lr$indexes,
#'                         dirPath=file.path('C:','User','home'))
#' 
developModel <- function(population, plpData,
                         featureSettings=NULL,
                         modelSettings,
                         testSplit = 'time', testFraction=0.3, nfold=3, indexes=NULL,
                         dirPath=NULL, silent=F
                         ){
  if(!testSplit%in%c('person','time'))
    stop('Invalid testSplit')
  if(sum(population[,'outcomeCount']>0) < 25)
    stop('Outcome occurrence too low')
  
  # TODO - add input checks
    
  start.all <- Sys.time()
  analysisId <- gsub(':','',gsub('-','',gsub(' ','',start.all)))
  
  if(is.null(dirPath)) dirPath <- file.path(getwd(),'plpmodels')
  if(!dir.exists(file.path(dirPath,analysisId))){dir.create(file.path(dirPath,analysisId),recursive=T)}
  
  #================ GET COHORT/OUTCOME ==============================
  cohortId <- attr(population, "metaData")$cohortId
  outcomeId <- attr(population, "metaData")$outcomeId
  #=========================================================================
  
  # construct the settings for the model pipeline
  if(is.null(indexes)){
    if(testSplit=='time')
      indexes <- timeSplitter(population, test=testFraction, nfold=nfold, silent=silent)
    if(testSplit=='person')
      indexes <- personSplitter(population, test=testFraction, nfold=nfold, silent=silent)
  }
  
  if(nrow(population)!=nrow(indexes))
    stop('Population dimension not compatible with indexes')
  tempmeta <- attr(population, "metaData")
  population <- merge(population, indexes)
  colnames(population)[colnames(population)=='index'] <- 'indexes'
  attr(population, "metaData") <- tempmeta
  #population$indexes <- indexes$index
  
  settings <- list(data=plpData, dirPath=dirPath, index=indexes,
                   featureSettings = featureSettings,
                   modelSettings = modelSettings,
                   population=population, quiet=silent,
                   cohortId=cohortId,
                   outcomeId=outcomeId)
  
  populationLoc <- file.path(dirPath,analysisId, 'population.txt')
  write.table(population, populationLoc, row.names=F)
  
  # train model
  model <- do.call(fitPlp, settings)
  if(!silent)
    writeLines('1: Model Trained')
  modelLoc <- file.path(dirPath,analysisId, 'savedModel' )
  savePlpModel(model, modelLoc)
  
  # do prediction (on all data as we want test + train model performances)
  prediction <- predictPlp(plpModel=model, population=population, plpData=plpData, 
                           index=NULL, dirPath=dirPath, silent=silent)
  if(!silent)
    writeLines('2) Prediction Calculated')
 
  # calculate metrics
  if(ifelse(is.null(prediction), FALSE, length(unique(prediction$value))>1)){
    
    performance.test <- evaluatePlp(prediction[prediction$indexes<0,])
    writeLines(' Test set predictions evaluated')
    performance.train <- evaluatePlp(prediction[prediction$indexes>0,])
    writeLines(' Train set predictions evaluated')
    if(!silent)
      writeLines('3) Performance calculated')
    
    writeOutput(prediction=prediction, 
                performance.test=performance.test, 
                performance.train=performance.train, 
                plpModel=model,
                population = population,
                plpData = plpData,
                dirPath=dirPath,
                analysisId=analysisId,
                start.all=start.all,
                testSplit=testSplit,
                modelLoc=modelLoc,
                populationLoc=populationLoc)
    
  }else{
    performance.test <- NULL
    performance.train <- NULL
  }
  
  results <- list(transform=model$transform,
                  model=model,
                  prediction=prediction,
                  index=indexes,
                  evalType=testSplit,
                  performanceTest=performance.test,
                  performanceTrain=performance.train,
                  time=format(difftime(Sys.time(), start.all, units='hours'), nsmall=1))
  class(results) <- c('list','plpModel')
  
  return(results)
  
}






#' fitModel
#'
#' @description
#' Train various models using a default parameter gird search or user specified parameters
#'
#' @details
#' The user can define the machine learning model to train (regularised logistic regression, random forest,
#' gradient boosting machine, neural network and )
#' 
#' @param population                       The population created using createStudyPopulation() who will have their risks predicted
#' @param data                             An object of type \code{plpData} - the patient level prediction
#'                                         data extracted from the CDM.
#' @param index                            A data frame containing rowId: a vector of rowids and index: a vector of doubles the same length as the 
#'                                         rowIds. If used, only the rowIds with a negative index value are used to calculate the prediction.  
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
#' @param dirPath                              The path to the directory where the model will be saved
#' @return
#' An object of class \code{plpModel} containing:
#' 
#' \item{model}{The trained prediction model}
#' \item{modelLoc}{The path to where the model is saved (if saved)}
#' \item{trainAuc}{The AUC obtained on the training set}
#' \item{trainCalibration}{The calibration obtained on the training set}
#' \item{modelSettings}{A list specifiying the model, preprocessing, outcomeId and cohortId}
#' \item{metaData}{The model meta data}
#' \item{trainingTime}{The time taken to train the classifier}
#'
#'

#' @export
fitPlp <- function(population, data, index,  modelSettings,featureSettings, dirPath, quiet,
                   cohortId, outcomeId){
  plpData <- list(outcomes =data$outcomes,
                  cohorts = data$cohorts,
                  covariates =ff::clone(data$covariates),
                  covariateRef=ff::clone(data$covariateRef),
                  metaData=data$metaData
  )

  
  #=========================================================
  # run through pipeline list and apply:
  #=========================================================
  plpTransform <- NULL
  
  # apply each featureSetting option in order of the list entry to do feature engineering/selection
  if (class(featureSettings) == "featureSettings") {
    fun <- featureSettings$method
    args <- list(plpData =plpData, dirPath=dirPath, index=index, param=featureSettings$param)
    plpTransform <- do.call(fun, args)
    
    # transform all the data:
    plpData <- plpTransform$transform(plpData)
    
    if (nrow(plpData$covariates) == 0) {
      warning("No features remaining")
    } 
  } else if (is.list(featureSettings)) {
    for (i in 1:length(featureSettings)) {
      #fun <- attr(featureSettings[[i]], "fun")
      fun <- featureSettings[[i]]$method
      args <- list(plpData=plpData,dirPath=dirPath,  index=index, param=featureSettings[[i]]$param)
      plpTransform.temp <- do.call(fun, args)
      
      if(i==1){
        plpTransform$transform <- plpTransform.temp$transform
        plpTransform$transformDetails <- plpTransform.temp$transformDetails
      }
      if(i > 1){
        plpTransform$transform <- c(plpTransform$transform,  plpTransform.temp$transform)
        plpTransform$transformDetails <- c(plpTransform$transformDetails,  plpTransform.temp$transformDetails)
      }
      
      # transform all the data:
      plpData <- plpTransform$transform[[i]](plpData)
      
      if (nrow(plpData$covariates) == 0) {
        warning("No features remaining")
      } 
    }
  }
  

  # if model is h2o save to libSVM:
  if(attr(modelSettings, 'libSVM')){
    mappingVal <- saveLibSVM(population, plpData, filePath=dirPath)
  }

  # Now apply the classifier:
  fun <- modelSettings$model
  args <- list(plpData =plpData,param =modelSettings$param, index=index, dirPath=dirPath,
               population=population, quiet=quiet, cohortId=cohortId, outcomeId=outcomeId)
  plpModel <- do.call(fun, args)
  
  # add the transform functions and details to the model:
  if(!is.null(plpTransform))
    plpModel$metaData$featureSettings <-  plpTransform 
  
  # create transformation function
  createTransform <- function(plpModel,mappingVal){
    transform <- function(plpData=NULL, population=NULL, file=dirPath, silent=F){
      #check model fitting makes sense:
      if(ifelse(!is.null(attr(population, "metaData")$cohortId),attr(population, "metaData")$cohortId,-1)!=plpModel$cohortId)
        warning('cohortId of new data does not match training data')
      if(ifelse(!is.null(attr(population, "metaData")$outcomeId),attr(population, "metaData")$outcomeId,-1)!=plpModel$outcomeId)
        warning('outcomeId of new data does not match training data or does not exist')
      
      #TODO: recalibrate
      
    if(!is.null(plpModel$metaData$featureSettings)){
      if(!silent) writeLines("Applying model's feature selection...")
      plpData <- lapply(plpModel$metaData$featureSettings$transform, function(x) do.call(x, list(newData=plpData)))
     }
      if(attr(modelSettings, 'libSVM'))
        saveLibSVM(population, plpData, filePath=file, mapping=mappingVal)
      if(!silent) writeLines('Applying model to calculate predictions...')
      pred <- do.call(paste0('predict.',attr(plpModel, 'type')), list(plpModel=plpModel,
                                                              plpData=plpData, 
                                                              population=population, 
                                                              dirPath=file,
                                                              silent=silent))
      metaData <- list(trainDatabase = strsplit(do.call(paste, list(plpModel$metaData$call$cdmDatabaseSchema)),'\\.')[[1]][1],
                       testDatabase = strsplit(do.call(paste, list(plpData$metaData$call$cdmDatabaseSchema)),'\\.')[[1]][1],
                       studyStartDate = do.call(paste,list(plpModel$metaData$call$studyStartDate)), 
                       studyEndDate = do.call(paste,list(plpModel$metaData$call$studyEndDate)),
                       cohortId = plpModel$cohortId,
                       outcomeId = plpModel$outcomeId,
                       predictionType ='binary'
      )
      attr(pred, 'metaData') <- metaData
      return(pred)
    }
    return(transform)
  }
  plpModel$transform <- createTransform(plpModel,mappingVal)
  
  return(plpModel)
  
}


#' predictPlp
#'
#' @description
#' Predict the risk of the outcome using the input plpModel for the input plpData
#' @details
#' The function applied the trained model on the plpData to make predictions
#' @param plpModel                         An object of type \code{plpModel} - a patient level prediction model
#' @param population                       The population created using createStudyPopulation() who will have their risks predicted
#' @param plpData                          An object of type \code{plpData} - the patient level prediction
#'                                         data extracted from the CDM.
#' @param dirPath                          The location of the output directory.  If using a h2o model, the libSvm will be saved here                                         
#' @param index                            A data frame containing rowId: a vector of rowids and index: a vector of doubles the same length as the rowIds. If used, only the rowIds with a negative index value are used to calculate the prediction.  
#' 
#' @return
#' A dataframe containing the prediction for each person in the population with an attribute metaData containing prediction details.
#'

#' @export
predictPlp <- function(plpModel, population, plpData, dirPath, index=NULL, silent=F){
  # in the model part add an attribute type - plp, h2o, caret, ... then apply prediciton for that type or allow custom
  # apply the feature transformations
  if(!is.null(index)){
    if(!silent) writeLines(paste0('Calculating prediction for ',sum(index$index<0),' in test set'))
    ind <- population$rowId%in%index$rowId[index$index<0]
  } else{
    if(!silent) writeLines(paste0('Calculating prediction for ',nrow(population),' in dataset'))
    ind <- rep(T, nrow(population))
  }
  # do the predction on the new data
  if(class(plpModel)=='plpModel'){
    # extract the classifier type
    prediction <- plpModel$transform(plpData=plpData,population=population[ind,],file=dirPath, silent=silent)
    if(nrow(prediction)!=nrow(population[ind,]))
      warning(paste0('Dimension mismatch between prediction and population test cases.  Population test: ',nrow(population[ind, ]), '-- Prediction:', nrow(prediction) ))
  } else{
    stop('Non plpModel input')
  }
  
  metaData <- list(predictionType="binary",
                   cohortId = attr(population,'metaData')$cohortId,
                   outcomeId = attr(population,'metaData')$outcomeId)
  
  attr(prediction, "metaData") <- metaData
  return(prediction)
}


#' makeRandomString
#'
#' @description
#' A function for making a random string
#' @details
#' The function creates n random strings of size length
#' @param n                                An integer - the number of random string to generate
#' @param length                           An integer - the number of characters for each string
#'
#' @return
#' A list containing n random strings with the number of characters specified by the use input length
#'
makeRandomString <- function(n=1, lenght=12)
{
  randomString <- c(1:n)                  # initialize vector
  for (i in 1:n)
  {
    randomString[i] <- paste(sample(c(0:9, letters, LETTERS),
                                    lenght, replace=TRUE),
                             collapse="")
  }
  return(randomString)
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



#' @export
savePlpModel <- function(plpModel, dirPath){
  if (missing(plpModel))
    stop("Must specify plpModel")
  if (missing(dirPath))
    stop("Must specify directory path")
  if (class(plpModel) != "plpModel")
    stop("Not a plpModel")
  
  if(!dir.exists(dirPath)) dir.create(dirPath)
  
  saveRDS(plpModel$model, file = file.path(dirPath, "model.rds"))
  saveRDS(plpModel$transform, file = file.path(dirPath, "transform.rds"))
  saveRDS(plpModel$trainCVAuc, file = file.path(dirPath, "trainCVAuc.rds"))
  saveRDS(plpModel$modelSettings, file = file.path(dirPath,  "modelSettings.rds"))
  saveRDS(plpModel$metaData, file = file.path(dirPath, "metaData.rds"))
  saveRDS(plpModel$populationSettings, file = file.path(dirPath, "populationSettings.rds"))
  saveRDS(plpModel$trainingTime, file = file.path(dirPath,  "trainingTime.rds"))
  saveRDS(plpModel$varImp, file = file.path(dirPath,  "varImp.rds"))
  
  
  attributes <- list(type=attr(plpModel, 'type'), predictionType=attr(plpModel, 'predictionType') )
  saveRDS(attributes, file = file.path(dirPath,  "attributes.rds"))
  
  
}

#' @export
loadPlpModel <- function(dirPath, readOnly = TRUE) {
  if (!file.exists(dirPath))
    stop(paste("Cannot find folder", dirPath))
  if (!file.info(dirPath)$isdir)
    stop(paste("Not a folder", dirPath))
  

  result <- list(model = readRDS(file.path(dirPath, "model.rds")),
                 transform = readRDS(file.path(dirPath, "transform.rds")),
                 trainCVAuc = readRDS(file.path(dirPath, "trainCVAuc.rds")),
                 modelSettings = readRDS(file.path(dirPath, "modelSettings.rds")),
                 metaData = readRDS(file.path(dirPath, "metaData.rds")),
                 populationSettings= readRDS(file.path(dirPath, "populationSettings.rds")),
                 trainingTime = readRDS(file.path(dirPath, "trainingTime.rds")),
                 varImp = readRDS(file.path(dirPath, "varImp.rds"))
                 
                 )
  attributes <- readRDS(file.path(dirPath, "attributes.rds"))
  attr(result, 'type') <- attributes$type
  attr(result, 'predictionType') <- attributes$predictionType
  class(result) <- "plpModel"
  
  return(result)
}



writeOutput <- function(prediction, 
                        performance.test, 
                        performance.train, 
                        plpModel,
                        population,
                        plpData,
                        dirPath,
                        analysisId,
                        start.all,
                        testSplit,
                        modelLoc,
                        populationLoc){
  if(!dir.exists(file.path(dirPath,analysisId , 'test'))){dir.create(file.path(dirPath,analysisId , 'test'))}
  write.table(performance.test$raw, file.path(dirPath,analysisId , 'test','rocRawSparse.txt'), row.names=F)
  write.table(performance.test$preferenceScores, file.path(dirPath,analysisId , 'test','preferenceScoresSparse.txt'), row.names=F)
  write.table(performance.test$calSparse, file.path(dirPath,analysisId , 'test','calSparse.txt'), row.names=F)
  write.table(performance.test$calSparse2_10, file.path(dirPath,analysisId , 'test','calSparse2_10.txt'), row.names=F)
  write.table(performance.test$calSparse2_100, file.path(dirPath,analysisId , 'test','calSparse2_100.txt'), row.names=F)
  write.table(performance.test$quantiles, file.path(dirPath,analysisId , 'test','quantiles.txt'), row.names=F)
  
  if(!dir.exists(file.path(dirPath,analysisId , 'train'))){dir.create(file.path(dirPath,analysisId , 'train'))}
  write.table(performance.train$raw, file.path(dirPath,analysisId , 'train','rocRawSparse.txt'), row.names=F)
  write.table(performance.train$preferenceScores, file.path(dirPath,analysisId , 'train','preferenceScoresSparse.txt'), row.names=F)
  write.table(performance.train$calSparse, file.path(dirPath,analysisId , 'train','calSparse.txt'), row.names=F)
  write.table(performance.train$calSparse2_10, file.path(dirPath,analysisId , 'train','calSparse2_10.txt'), row.names=F)
  write.table(performance.train$calSparse2_100, file.path(dirPath,analysisId , 'train','calSparse2_100.txt'), row.names=F)
  write.table(performance.train$quantiles, file.path(dirPath,analysisId , 'train','quantiles.txt'), row.names=F)
  
  
  #save plots:
  pdf(file.path(dirPath,analysisId,'plots.pdf'))
  gridExtra::grid.arrange(performance.test$calPlot, 
                          gridExtra::arrangeGrob(performance.test$prefScorePlot, performance.test$boxPlot), 
                          nrow=2,
                          top='Performance Plots')
  print(PatientLevelPrediction::plotRoc(prediction[prediction$indexes<0,]))
  
  dev.off()
  
  comp <- format(difftime(Sys.time(), start.all, units='hours'), nsmall=1)
  
  # make nice formated model info table and performance table
  tryCatch({
    modelInfo <- data.frame(modelId = analysisId,
                            database = strsplit(do.call(paste, list(plpModel$metaData$call$cdmDatabaseSchema)), '\\.')[[1]][1],
                            cohortId=attr(prediction, "metaData")$cohortId,
                            outcomeId=attr(prediction, "metaData")$outcomeId,
                            # add fold information and test/train size/ num events?
                            model= plpModel$modelSettings$model,
                            splitOn = testSplit,
                            modelLoc =modelLoc ,
                            populationLoc=populationLoc ,
                            parameters = paste(names(plpModel$modelSettings$modelParameters), unlist(plpModel$modelSettings$modelParameters), sep=':', collapse=','),
                            modelTime = comp)
  }, error= function(err){print(paste("MY_ERROR:  ",err))
    writeLines(paste(plpData$metaData$call$cdmDatabaseSchema,attr(prediction, "metaData")$cohortId, plpModel$modelSettings$model, sep='-'))
    
  })
  performanceInfoTest <- data.frame(modelId =analysisId,
                                    AUC = performance.test$auc[1],
                                    AUC_lb = performance.test$auc[2],
                                    AUC_ub = performance.test$auc[3],
                                    Brier = performance.test$brier,
                                    BrierScaled = performance.test$brierScaled,
                                    hosmerlemeshow_chi2 = performance.test$hosmerlemeshow[1],
                                    hosmerlemeshow_df = performance.test$hosmerlemeshow[2],
                                    hosmerlemeshow_pvalue = performance.test$hosmerlemeshow[3],
                                    calibrationIntercept = performance.test$calibrationIntercept10,
                                    calibrationGradient = performance.test$calibrationGradient10,
                                    preference4070_0 = performance.test$preference4070_0,
                                    preference4070_1 = performance.test$preference4070_1
  )
  
  performanceInfoTrain <- data.frame(modelId =analysisId,
                                     AUC = performance.train$auc[1],
                                     AUC_lb = performance.train$auc[2],
                                     AUC_ub = performance.train$auc[3],
                                     Brier = performance.train$brier,
                                     BrierScaled = performance.train$brierScaled,
                                     hosmerlemeshow_chi2 = performance.train$hosmerlemeshow[1],
                                     hosmerlemeshow_df = performance.train$hosmerlemeshow[2],
                                     hosmerlemeshow_pvalue = performance.train$hosmerlemeshow[3],
                                     calibrationIntercept = performance.train$calibrationIntercept10,
                                     calibrationGradient = performance.train$calibrationGradient10,
                                     preference4070_0 = performance.train$preference4070_0,
                                     preference4070_1 = performance.train$preference4070_1
  )
  
  # search for modelInfo in directory - if does not exist create and save model info table
  # otherwise append model info to existing file
  if(file.exists(file.path(dirPath, 'modelInfo.txt')))
    write.table(modelInfo, file.path(dirPath, 'modelInfo.txt'), append=T, row.names = F, col.names = F)
  if(!file.exists(file.path(dirPath, 'modelInfo.txt')))
    write.table(modelInfo, file.path(dirPath, 'modelInfo.txt'), row.names = F)
  
  # repeat for performance info
  if(file.exists(file.path(dirPath, 'performanceInfoTest.txt')))
    write.table(performanceInfoTest, file.path(dirPath, 'performanceInfoTest.txt'), append=T, row.names = F, col.names = F)
  if(!file.exists(file.path(dirPath, 'performanceInfoTest.txt')))
    write.table(performanceInfoTest, file.path(dirPath, 'performanceInfoTest.txt'), row.names = F)
  if(file.exists(file.path(dirPath, 'performanceInfoTrain.txt')))
    write.table(performanceInfoTrain, file.path(dirPath, 'performanceInfoTrain.txt'), append=T, row.names = F, col.names = F)
  if(!file.exists(file.path(dirPath, 'performanceInfoTrain.txt')))
    write.table(performanceInfoTrain, file.path(dirPath, 'performanceInfoTrain.txt'), row.names = F)
  
  
  
}