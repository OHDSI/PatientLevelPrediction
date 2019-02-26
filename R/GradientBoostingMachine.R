# @file gradientBoostingMachine.R
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

#' Create setting for gradient boosting machine model using gbm_xgboost implementation
#'
#' @param ntrees     The number of trees to build 
#' @param nthread   The number of computer threads to (how many cores do you have?)
#' @param earlyStopRound   If the performance does not increase over earlyStopRound number of interactions then training stops (this prevents overfitting)
#' @param maxDepth  Maximum number of interactions - a large value will lead to slow model training
#' @param minRows   The minimum number of rows required at each end node of the tree
#' @param learnRate The boosting learn rate
#' @param seed       An option to add a seed when training the final model
#'
#' @examples
#' model.gbm <- setGradientBoostingMachine(ntrees=c(10,100), nthread=20,
#'                            maxDepth=c(4,6), learnRate=c(0.1,0.3))
#'
#' @export
setGradientBoostingMachine <- function(ntrees=c(100, 1000), nthread=20, earlyStopRound = 25,
                                  maxDepth=c(4,6,17), minRows=2, learnRate=c(0.005, 0.01,0.1),
                                  seed= NULL){
  
  if(length(nthread)>1)
    stop(paste('nthreads must be length 1'))
  if(!class(seed)%in%c('numeric','NULL', 'integer'))
    stop('Invalid seed')
  if(!class(ntrees) %in% c("numeric", "integer"))
    stop('ntrees must be a numeric value >0 ')
  if(sum(ntrees < 1)>0)
    stop('ntrees must be greater that 0 or -1')
  if(!class(maxDepth) %in% c("numeric", "integer"))
    stop('maxDepth must be a numeric value >0')
  if(sum(maxDepth < 1)>0)
    stop('maxDepth must be greater that 0')
  if(!class(minRows) %in% c("numeric", "integer"))
    stop('minRows must be a numeric value >1')
  if(sum(minRows < 2)>0)
    stop('minRows must be greater that 1')
  if(class(learnRate)!='numeric')
    stop('learnRate must be a numeric value >0 and <= 1')
  if(sum(learnRate <= 0)>0)
    stop('learnRate must be greater that 0')
  if(sum(learnRate > 1)>0)
    stop('learnRate must be less that or equal to 1')
  if(!class(earlyStopRound) %in% c("numeric", "integer", "NULL"))
    stop('incorrect class for earlyStopRound')
  
  # set seed
  if(is.null(seed[1])){
    seed <- as.integer(sample(100000000,1))
  }
  
  result <- list(model='fitGradientBoostingMachine', 
                 param= split(expand.grid(ntrees=ntrees, earlyStopRound=earlyStopRound,
                                          maxDepth=maxDepth, minRows=minRows, 
                                          learnRate=learnRate, nthread=nthread,
                                          seed= seed[1] ),
                              1:(length(ntrees)*length(maxDepth)*length(minRows)*length(learnRate)*length(earlyStopRound)  )),
                 name='Gradient boosting machine'
  )
  class(result) <- 'modelSettings' 
  
  return(result)
}


#xgboost
fitGradientBoostingMachine <- function(population, plpData, param, quiet=F,
                        outcomeId, cohortId, ...){
  # check logger
  if(length(ParallelLogger::getLoggers())==0){
    logger <- ParallelLogger::createLogger(name = "SIMPLE",
                                        threshold = "INFO",
                                        appenders = list(ParallelLogger::createConsoleAppender(layout = ParallelLogger::layoutTimestamp)))
    ParallelLogger::registerLogger(logger)
  }
  
  if(!quiet)
    ParallelLogger::logTrace('Training GBM model')
  
  if(param[[1]]$seed!='NULL')
    set.seed(param[[1]]$seed)
  
  # check plpData is coo format:
  if(!'ffdf'%in%class(plpData$covariates) )
    stop('This algorithm requires plpData in coo format')
  
  metaData <- attr(population, 'metaData')
  if(!is.null(population$indexes))
    population <- population[population$indexes>0,]
  attr(population, 'metaData') <- metaData
  #TODO - how to incorporate indexes?
  
  # convert data into sparse Matrix:
  result <- toSparseM(plpData,population,map=NULL, temporal = F)
  data <- result$data
  
  # now get population of interest
  data <- data[population$rowId,]
  
  # set test/train sets (for printing performance as it trains)
  ParallelLogger::logInfo(paste0('Training gradient boosting machine model on train set containing ', nrow(population), ' people with ',sum(population$outcomeCount>0), ' outcomes'))
  start <- Sys.time()
  
  # pick the best hyper-params and then do final training on all data...
  datas <- list(data=data, population=population)
  param.sel <- lapply(param, function(x) do.call("gbm_model2", c(datas,x)  ))
  hyperSummary <- do.call("rbind", lapply(param.sel, function(x) x$hyperSum))
  hyperSummary <- as.data.frame(hyperSummary)
  hyperSummary$auc <- unlist(lapply(param.sel, function(x) x$auc)) # new edit
    
  param.sel <- unlist(lapply(param.sel, function(x) x$auc))
  param <- param[[which.max(param.sel)]]
  param$final=T
  
  #ParallelLogger::logTrace("Final train")
  trainedModel <- do.call("gbm_model2", c(param,datas)  )$model
  
  comp <- Sys.time() - start
  if(!quiet)
    ParallelLogger::logInfo(paste0('Model GBM trained - took:',  format(comp, digits=3)))
  
  varImp <- xgboost::xgb.importance(model =trainedModel)
  
  # get the original feature names:
  varImp$Feature <- as.numeric(varImp$Feature)+1 # adding +1 as xgboost index starts at 0
  varImp <- merge(result$map, varImp, by.x='newIds', by.y='Feature')
  
  varImp<- merge(ff::as.ram(plpData$covariateRef),varImp,  by.y='oldIds', by.x='covariateId', all=T)
  varImp$Gain[is.na(varImp$Gain)] <- 0
  varImp <- varImp[order(-varImp$Gain),]
  colnames(varImp)[colnames(varImp)=='Gain'] <- 'covariateValue'
  
  # apply the model to the train set:
  prediction <- data.frame(rowId=population$rowId,
                           value=stats::predict(trainedModel, data)
  )
  prediction <- merge(population, prediction, by='rowId')
  prediction <- prediction[,colnames(prediction)%in%c('rowId','subjectId','cohortStartDate','outcomeCount','indexes', 'value')] # need to fix no index issue
  attr(prediction, "metaData") <- list(predictionType = "binary") 

  result <- list(model = trainedModel,
                 modelSettings = list(model='gbm_xgboost', modelParameters=param), #todo get lambda as param
                 trainCVAuc = NULL,
                 hyperParamSearch = hyperSummary,
                 metaData = plpData$metaData,
                 populationSettings = attr(population, 'metaData'),
                 outcomeId=outcomeId,# can use populationSettings$outcomeId?
                 cohortId=cohortId,
                 varImp = varImp,
                 trainingTime=comp,
                 covariateMap=result$map,
                 predictionTrain = prediction
  )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'xgboost'
  attr(result, 'predictionType') <- 'binary'
  return(result)
}

gbm_model2 <- function(data, population,
                       maxDepth=6, minRows=20, nthread=20,
                       ntrees=100, learnRate=0.1, final=F, earlyStopRound=NULL, ...){
  
  if(missing(final)){
    final <- F
  }
  if(missing(population)){
    stop('No population')
  }
  if(!is.null(population$indexes) && final==F){
    ParallelLogger::logInfo(paste0("Training GBM with ",length(unique(population$indexes))," fold CV"))
    index_vect <- unique(population$indexes)
    ParallelLogger::logDebug(paste0('index vect: ', paste0(index_vect, collapse='-')))
    perform <- c()
    
    # create prediction matrix to store all predictions
    predictionMat <- population
    ParallelLogger::logDebug(paste0('population nrow: ', nrow(population)))
    
    predictionMat$value <- 0
    attr(predictionMat, "metaData") <- list(predictionType = "binary")
    
    for(index in 1:length(index_vect )){
      ParallelLogger::logInfo(paste('Fold ',index, ' -- with ', sum(population$indexes!=index),'train rows'))
      train <- xgboost::xgb.DMatrix(data = data[population$indexes!=index,], label=population$outcomeCount[population$indexes!=index])
      test <- xgboost::xgb.DMatrix(data = data[population$indexes==index,], label=population$outcomeCount[population$indexes==index])
      watchlist <- list(train=train, test=test)
      
      model <- xgboost::xgb.train(data = train, 
                                  max.depth = maxDepth, 
                                  eta = learnRate, 
                                  nthread = nthread, 
                                  min_child_weight = minRows,
                                  nround = ntrees,
                                  watchlist = watchlist,
                                  objective = "binary:logistic",
                                  eval.metric = "logloss", eval.metric = "auc",
                                  print_every_n=10,
                                  early_stopping_rounds = earlyStopRound)
      
      pred <- stats::predict(model, data[population$indexes==index,])
      prediction <- population[population$indexes==index,]
      prediction$value <- pred
      attr(prediction, "metaData") <- list(predictionType = "binary")
      aucVal <- computeAuc(prediction)
      perform <- c(perform,aucVal)
      
      # add the fold predictions and compute AUC after loop
      predictionMat$value[population$indexes==index] <- pred
      
     }
    ##auc <- mean(perform) # want overal rather than mean
    auc <- computeAuc(predictionMat)
    
    foldPerm <- perform
  } else {
    train <- xgboost::xgb.DMatrix(data = data, label=population$outcomeCount)
    watchlist <- NULL
    
    if(!is.null(earlyStopRound)){
      ind <- (1:nrow(population))%in%sample(nrow(population), floor(nrow(population)*0.9))
      train <- xgboost::xgb.DMatrix(data = data[ind,], label=population$outcomeCount[ind])
      test <- xgboost::xgb.DMatrix(data = data[!ind,], label=population$outcomeCount[!ind])
      watchlist <- list(train=train, test=test)
    }
      
    model <- xgboost::xgb.train(data = train, 
                                max.depth = maxDepth, 
                                eta = learnRate, 
                                nthread = nthread, 
                                min_child_weight = minRows,
                                nround = ntrees,
                                watchlist = watchlist,
                                objective = "binary:logistic",
                                eval.metric = "logloss", eval.metric = "auc",
                                print_every_n=10,
                                early_stopping_rounds = earlyStopRound)
    
    pred <- stats::predict(model, data)
    prediction <- population
    prediction$value <- pred
    attr(prediction, "metaData") <- list(predictionType = "binary") 
    auc <- computeAuc(prediction)
    foldPerm <- auc
  }
  param.val <- paste0('maxDepth: ',maxDepth,'-- minRows: ', minRows, 
                      '-- nthread: ', nthread, ' ntrees: ',ntrees, '-- learnRate: ', learnRate)
  ParallelLogger::logInfo("==========================================")
  ParallelLogger::logInfo(paste0("GMB with parameters: ", param.val," obtained an AUC of ",auc))
  ParallelLogger::logInfo("==========================================")
  
  result <- list(model=model,
                 auc=auc,
                 hyperSum = unlist(list(maxDepth = maxDepth, learnRate = learnRate, nthread = nthread, 
                                        minRows = minRows, ntrees = ntrees, fold_auc=foldPerm))
  )
  return(result)
}
