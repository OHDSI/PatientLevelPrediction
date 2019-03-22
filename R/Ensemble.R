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

#' ensemble - Create an ensembling model using different models
#'
#' @description
#' #'
#' @details
#'
#'
#' @param population         The population created using createStudyPopulation() who will be used to
#'                           develop the model
#' @param dataList           An list of object of type \code{plpData} - the patient level prediction
#'                           data extracted from the CDM.
#' @param modelList          An list of type of base model created using one of the function in final
#'                           ensembling model, the base model can be any model implemented in this
#'                           package.
#' @param testSplit          Either 'person' or 'time' specifying the type of evaluation used. 'time'
#'                           find the date where testFraction of patients had an index after the date
#'                           and assigns patients with an index prior to this date into the training
#'                           set and post the date into the test set 'person' splits the data into test
#'                           (1-testFraction of the data) and train (validationFraction of the data)
#'                           sets.  The split is stratified by the class label.
#' @param testFraction       The fraction of the data to be used as the test set in the patient split
#'                           evaluation.
#' @param splitSeed          The seed used to split the test/train set when using a person type
#'                           testSplit
#' @param nfold              The number of folds used in the cross validation (default 3)
#' @param saveDirectory      The path to the directory where the results will be saved (if NULL uses working directory)
#' @param saveEnsemble       Binary indicating whether to save the ensemble 
#' @param savePlpData        Binary indicating whether to save the plpData object (default is F)
#' @param savePlpResult      Binary indicating whether to save the object returned by runPlp (default is F)
#' @param savePlpPlots       Binary indicating whether to save the performance plots as pdf files (default is F)
#' @param saveEvaluation     Binary indicating whether to save the oerformance as csv files (default is T)
#' @param analysisId         The analysis ID
#' @param verbosity          Sets the level of the verbosity. If the log level is at or higher in
#'                           priority than the logger threshold, a message will print. The levels are:
#'                           \itemize{
#'                             \item {DEBUG}{Highest verbosity showing all debug statements}
#'                             \item {TRACE}{Showing information about start and end of steps}
#'                             \item {INFO}{Show informative information (Default)}
#'                             \item {WARN}{Show warning messages}
#'                             \item {ERROR}{Show error messages}
#'                             \item {FATAL}{Be silent except for fatal errors}
#'                           }
#'
#' @param ensembleStrategy   The strategy used for ensembling the outputs from different models, it can
#'                           be 'mean', 'product', 'weighted' and 'stacked' 'mean' the average
#'                           probability from differnt models 'product' the product rule 'weighted' the
#'                           weighted average probability from different models using train AUC as
#'                           weights. 'stacked' the stakced ensemble trains a logistics regression on
#'                           different models.
#'
#' @export
runEnsembleModel <- function(population,
                             dataList,
                             modelList,
                             testSplit = "time",
                             testFraction = 0.2,
                             splitSeed = NULL,
                             nfold = 3,
                             saveDirectory=NULL,
                             saveEnsemble = F,
                             savePlpData=F, 
                             savePlpResult=F, 
                             savePlpPlots = F, 
                             saveEvaluation = F,
                             analysisId = NULL,
                             verbosity = "INFO",
                             ensembleStrategy = "mean") {
  ExecutionDateTime <- Sys.time()
  if (is.null(analysisId))
    analysisId <- gsub(":", "", gsub("-", "", gsub(" ", "", ExecutionDateTime)))

  # check logger
  if (length(ParallelLogger::getLoggers()) == 0) {
    logger <- ParallelLogger::createLogger(name = "SIMPLE",
                                        threshold = verbosity,
                                        appenders = list(ParallelLogger::createFileAppender(layout = ParallelLogger::layoutTimestamp)))
    ParallelLogger::registerLogger(logger)
  }
  
  if(is.null(splitSeed)){
    splitSeed <- sample(1000000, 1)
  }
  prediction <- population
  
  stackerFraction <- 0
  if (ensembleStrategy == "stacked") {
    stackerFraction <- 0.2 * (1 - testFraction)
    ParallelLogger::logInfo(paste0(stackerFraction*100,"% of the data in validation set for training logistics regression as the combinator for stacked ensemble!"))
  }
  
  if(is.null(saveDirectory)){
    saveDirectory <- file.path(getwd(), 'ensemble_models')
  }
  
  trainAUCs <- c()

  # run the models in list one by one.#
  level1 <- list()
  length(level1) <- length(modelList)
  for (Index in seq_along(modelList)) {
    results <- PatientLevelPrediction::runPlp(population,
                                              dataList[[Index]],
                                              modelSettings = modelList[[Index]],
                                              testSplit = testSplit,
                                              testFraction = testFraction+stackerFraction,
                                              #trainFraction = trainFraction,
                                              nfold = nfold,
                                              saveDirectory=file.path(saveDirectory,Index), 
                                              savePlpData=savePlpData, 
                                              savePlpResult=savePlpResult, 
                                              savePlpPlots = savePlpPlots, 
                                              saveEvaluation = saveEvaluation,
                                              splitSeed = splitSeed,
                                              analysisId = analysisId)
    
    metaData <- attr(results$prediction, 'metaData')
    level1[[Index]] <- results$model
    # train auc or test auc?  replace 3,4 with logic 
    ##trainAUCs <- c(trainAUCs, as.numeric(results$performanceEvaluation$evaluationStatistics[3, 4]))
    tempres <- as.data.frame(results$performanceEvaluation$evaluationStatistics)
    trainAUCs <- c(trainAUCs, as.numeric(as.character(tempres$Value[tempres$Metric=='AUC.auc' & tempres$Eval=='train']))) # overfitting?!

    cnames <- c('rowId','value')
    if(!'indexes'%in%colnames(prediction)){
      cnames <- c('rowId','indexes','value')
    }
    prediction <- merge(prediction, results$prediction[,cnames], by='rowId', all.x=T)
    colnames(prediction)[ncol(prediction)] <- paste0('value_',Index)
  }
  names(level1) <- paste0('model_',1:length(level1))
  pred_probas <- prediction[,grep('value_',colnames(prediction))] #

  if (ensembleStrategy == "mean") {
    ensem_proba <- rowMeans(pred_probas)
    level2 <- list(ensembleStrategy = "mean",
                   pfunction = rowMeans)
  } else if (ensembleStrategy == "product") {
    ensem_proba <- apply(pred_probas, 1, prod)
    ensem_proba <- ensem_proba^(1/length(modelList))
    pfunction <- function(x){
      x <- apply(x,1,prod)^(1/length(modelList))
      return(x)
    }
    level2 <- list(ensembleStrategy = "product",
                   pfunction = pfunction)
  } else if (ensembleStrategy == "weighted") {
    trainAUCs <- trainAUCs/sum(trainAUCs)
    ensem_proba <- rowSums(t(t(as.matrix(pred_probas)) * trainAUCs))
    pfunction <- function(x){
      x <- rowSums(t(t(as.matrix(x)) * trainAUCs))
      return(x)
    }
    level2 <- list(ensembleStrategy = "weighted",
    pfunction = pfunction)
    
  } else if (ensembleStrategy == "stacked") {
    nontrain_index <- which(prediction$indexes < 0)
    test_index <- sample(nontrain_index, round(testFraction*nrow(prediction)))
    stacker_index <- setdiff(nontrain_index, test_index)
    prediction$indexes[stacker_index] <- 0
    stacker_prob <- pred_probas[stacker_index, ]
    stacker_y <- as.matrix(prediction$outcomeCount)[stacker_index]
    dataStack <- as.data.frame(stacker_prob)
    dataStack$y <- stacker_y
    ParallelLogger::logInfo("Training Stacker logistic model")
    lr_model <- glm(formula = y ~ ., data = dataStack, family = binomial(link = "logit"))
    ensem_proba <- predict(lr_model, newdata = data.frame(pred_probas), type = "response")
    pfunction <- function(x){
      x <- predict(lr_model, newdata = data.frame(x), type = "response")
      return(x)
    }
    level2 <- list(ensembleStrategy = "stacked",
                   pfunction = pfunction)
  } else {
    stop("ensembleStrategy must be mean, product, weighted and stacked")
  }

  prediction$value <- ensem_proba
  attr(prediction, 'metaData') <- metaData

  ParallelLogger::logInfo("Train set evaluation")
  performance.train <- evaluatePlp(prediction[prediction$indexes >= 0, ], dataList[[1]])
  ParallelLogger::logTrace("Done.")
  ParallelLogger::logInfo("Test set evaluation")
  performance.test <- evaluatePlp(prediction[prediction$indexes < 0, ], dataList[[1]])
  ParallelLogger::logTrace("Done.")
  performance <- reformatPerformance(train = performance.train, test = performance.test, analysisId)

  
  endTime <- Sys.time()
  TotalExecutionElapsedTime <- endTime-ExecutionDateTime
  
  executionSummary <- list(PackageVersion = list(rVersion= R.Version()$version.string,
                                                 packageVersion = utils::packageVersion("PatientLevelPrediction")),
                           PlatformDetails= list(platform= R.Version()$platform,
                                                 cores= Sys.getenv('NUMBER_OF_PROCESSORS'),
                                                 RAM=utils::memory.size()), #  test for non-windows needed
                           # Sys.info()
                           TotalExecutionElapsedTime = TotalExecutionElapsedTime,
                           ExecutionDateTime = ExecutionDateTime,
                           Log = NULL #logFileName # location for now
                           #Not available at the moment: CDM_SOURCE -  meta-data containing CDM version, release date, vocabulary version
  )
  results <- list(inputSetting=list(modelList=modelList,
                                    testSplit = testSplit,
                                    testFraction = testFraction,
                                    splitSeed = splitSeed,
                                    nfold =  nfold, 
                                    ensembleStrategy=ensembleStrategy),
                  executionSummary=executionSummary,
                  model=list(level1=level1,
                             level2=level2),
                  prediction=prediction,
                  performanceEvaluation=performance,
                  covariateSummary=results$covariateSummary,
                  analysisRef=list(analysisId=analysisId,
                                   analysisName=NULL,#analysisName,
                                   analysisSettings= NULL))
  class(results) <- c('ensemblePlp')
  
  if(saveEnsemble==T){
    saveEnsemblePlpResult(results, saveDirectory)
  } else{
    return(results)
  }
}

#' Apply trained ensemble model on new data Apply a Patient Level Prediction model on Patient Level
#' Prediction Data and get the predicted risk in [0,1] for each person in the population. If the user
#' inputs a population with an outcomeCount column then the function also returns the evaluation of
#' the prediction (AUC, brier score, calibration)
#'
#' @param population             The population of people who you want to predict the risk for
#' @param dataList               The plpData list for the population
#' @param ensembleModel          The trained ensemble model returned by running runEnsembleModel
#' @param calculatePerformance   Whether to also calculate the performance metrics [default TRUE]
#' @param analysisId             The analysis ID, which is the ID of running ensemble model training.
#' @examples
#' \dontrun{
#' # load the model and data
#' plpData <- loadPlpData("plpdata/")
#' results <- PatientLevelPrediction::runEnsembleModel(population,
#'                                                     dataList = list(plpData, plpData),
#'                                                     modelList = list(model, model),
#'                                                     testSplit = "person",
#'                                                     testFraction = 0.2,
#'                                                     nfold = 3,
#'                                                     splitSeed = 1000,
#'                                                     ensembleStrategy = "stacked")
#' # use the same population settings as the model:
#' populationSettings <- plpModel$populationSettings
#' populationSettings$plpData <- plpData
#' population <- do.call(createStudyPopulation, populationSettings)
#'
#' # get the prediction, please make sure the ensemble strategy for training and apply is the same:
#' prediction <- applyEnsembleModel(population,
#'                                  dataList = list(plpData, plpData),
#'                                  ensembleModel = results,
#'                                  analysisId = NULL)$prediction
#' }
#' @export
applyEnsembleModel <- function(population,
                               dataList,
                               ensembleModel, # contains modelList
                               analysisId = NULL,
                               calculatePerformance = T) {
  # check input:
  if (is.null(population))
    stop("NULL population")
  if (class(dataList[[1]]) != "plpData")
    stop("Incorrect plpData class")
  if (class(ensembleModel) != "ensemblePlp")
    stop("Incorrect ensembleModel")
  if (length(dataList) != length(ensembleModel$model$level1)) {
    stop("Data list wrong size")
  }
  
  combinator <- ensembleModel$model$level2
  modelList <- ensembleModel$model$level1

  # get prediction counts:
  peopleCount <- nrow(population)

  pred_probas <- matrix(nrow = length(population$subjectId), ncol = 0)
  for (Index in seq_along(modelList)) {
    prob <- modelList[[Index]]$predict(plpData = dataList[[Index]], population = population)
    pred_probas <- cbind(pred_probas, prob$value)
  }
  value <- combinator$pfunction(pred_probas)
  ensem_proba <- data.frame(rowId=prob$rowId,
                            value= value)
  
  prediction <- merge(population, ensem_proba, by='rowId')
  attr(prediction, "metaData") <- list(predictionType="binary")
  if (!"outcomeCount" %in% colnames(prediction))
    return(list(prediction = prediction))

  if (!calculatePerformance || nrow(prediction) == 1)
    return(prediction)

  performance <- evaluatePlp(prediction, dataList[[1]])


  result <- list(prediction = prediction, performanceEvaluation = performance)
  return(result)
}


#' saves the Ensmeble plp model 
#'
#' @details
#' Saves a plp ensemble model 
#'
#' @param ensembleModel            The ensemble model to save
#' @param dirPath                  The location to save the model
#'
#' @export
saveEnsemblePlpModel <- function(ensembleModel, dirPath) {
  if (!file.exists(file.path(dirPath,'level1')))
    dir.create(file.path(dirPath,'level1'), recursive = T)
  if (!file.exists(file.path(dirPath,'level2')))
    dir.create(file.path(dirPath,'level2'), recursive = T)
  
  for (i in 1:length(ensembleModel$level1)){
    modelPath <- file.path(dirPath,'level1', paste0('model_',i))
    savePlpModel(ensembleModel$level1[[i]], modelPath)
  }
  saveRDS(ensembleModel$level2, file.path(dirPath, "level2/combinator.rds"))

}

#' loads the Ensmeble plp model and return a model list
#'
#' @details
#' Loads a plp model list that was saved using \code{savePlpModel()}
#'
#' @param dirPath                  The location of the model
#'
#' @export
loadEnsemblePlpModel <- function(dirPath) {
  if (!file.exists(dirPath))
    stop(paste("Cannot find folder", dirPath))
  if (!file.info(dirPath)$isdir)
    stop(paste("Not a folder", dirPath))
  level1 <- list()
  dirList <- list.dirs(file.path(dirPath, 'level1'), recursive = FALSE)
  index <- 1
  for (subdir in dirList){
    model <- loadPlpModel(subdir)
    level1[[index]] <- model
    index <- index + 1
  }
  names(level1) <- paste0('model_',1:length(level1))
  level2 <- readRDS(file.path(dirPath, 'level2/combinator.rds'))
  
  model <- list(level1= level1,
                level2 = level2)
  
  return(model)
}

#' saves the Ensemble plp results 
#'
#' @details
#' Saves a plp ensemble results
#'
#' @param ensembleResult           The ensemble result
#' @param dirPath                  The location to save the ensemble results
#'
#' @export
saveEnsemblePlpResult <- function(ensembleResult, dirPath) {
  if (!file.exists(file.path(dirPath)))
    dir.create(file.path(dirPath))

  saveRDS(ensembleResult$inputSetting, file.path(dirPath,'inputSetting.rds'))
  saveRDS(ensembleResult$executionSummary, file.path(dirPath,'executionSummary.rds'))
  saveRDS(ensembleResult$prediction, file.path(dirPath,'prediction.rds'))
  saveRDS(ensembleResult$performanceEvaluation, file.path(dirPath,'performanceEvaluation.rds'))
  saveRDS(ensembleResult$covariateSummary, file.path(dirPath,'covariateSummary.rds'))
  saveRDS(ensembleResult$analysisRef, file.path(dirPath,'analysisRef.rds'))
  
  saveEnsemblePlpModel(ensembleResult$model, file.path(dirPath,'ensemble_model'))

}

#' loads the Ensemble plp results 
#'
#' @details
#' Loads a plp model list that was saved using \code{saveEnsemblePlpResults()}
#'
#' @param dirPath                  The location of the model
#'
#' @export
loadEnsemblePlpResult <- function(dirPath) {
  if (!file.exists(dirPath))
    stop(paste("Cannot find folder", dirPath))
  if (!file.info(dirPath)$isdir)
    stop(paste("Not a folder", dirPath))
  
  inputSetting <- readRDS(file.path(dirPath,'inputSetting.rds'))
  executionSummary <- readRDS(file.path(dirPath,'executionSummary.rds'))
  model <- loadEnsemblePlpModel(file.path(dirPath,'ensemble_model'))
  prediction <- readRDS(file.path(dirPath,'prediction.rds'))
  performanceEvaluation <- readRDS(file.path(dirPath,'performanceEvaluation.rds'))
  covariateSummary <- readRDS(file.path(dirPath,'covariateSummary.rds'))
  analysisRef <- readRDS(file.path(dirPath,'analysisRef.rds'))
  
  results <- list(inputSetting = inputSetting,
                  executionSummary = executionSummary,
                  model = model,
                  prediction = prediction,
                  performanceEvaluation = performanceEvaluation,
                  covariateSummary = covariateSummary,
                  analysisRef = analysisRef)
  class(results) <- c('ensemblePlp')
  return(results)
}
