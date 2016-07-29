# @file gradientBoostingMachine.R
#
# Copyright 2016 Observational Health Data Sciences and Informatics
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
#' @param max_depth  Maximum number of interactions - a large value will lead to slow model training
#' @param min_rows   The minimum number of rows required at each end node of the tree
#' @param learn_rate The boosting learn rate
#'
#' @examples
#' model.gbm <- gradientBoostingMachine.set(ntrees=c(10,100), nthread=20,
#'                            max_depth=c(4,6), learn_rate=c(0.1,0.3))
#'
#' @export
gradientBoostingMachine.set <- function(ntrees=c(10,100), nthread=20,
                                  max_depth=6, min_rows=20, learn_rate=0.1){
  
  if(length(nthread)>1)
    stop(paste('nthreads must be length 1'))
  
  result <- list(model='fitGradientBoostingMachine', 
                 param= split(expand.grid(nround=ntrees, 
                                          max.depth=max_depth, min_child_weight=min_rows, 
                                          eta=learn_rate, nthread=nthread),
                              1:(length(ntrees)*length(max_depth)*length(min_rows)*length(learn_rate)  )),
                 name='Gradient boosting machine'
  )
  class(result) <- 'modelSettings' 
  attr(result, 'libSVM') <- F
  
  return(result)
}


#xgboost
                        outcomeId, cohortId, ...){
  
  if(!quiet)
    writeLines('Training GBM model')
  
  # check plpData is coo format:
  if(!'ffdf'%in%class(plpData$covariates) || class(plpData)=='plpData.libsvm')
    stop('This algorithm requires plpData in coo format')
  
  metaData <- attr(population, 'metaData')
  if(!is.null(population$indexes))
    population <- population[population$indexes>0,]
  attr(population, 'metaData') <- metaData
  #TODO - how to incorporate indexes?
  
  # convert data into sparse Matrix:
  result <- toSparseM(plpData,population,map=NULL)
  data <- result$data
  
  # now get population of interest
  data <- data[population$rowId,]
  
  # set test/train sets (for printing performance as it trains)
  if(!quiet)
    writeLines(paste0('Training gradient boosting machine model on train set containing ', nrow(population), ' people with ',sum(population$outcomeCount>0), ' outcomes'))
  start <- Sys.time()
  
  # pick the best hyper-params and then do final training on all data...
  writeLines('train')
  datas <- list(population=population, data=data)
  param.sel <- lapply(param, function(x) do.call(gbm_model2, c(x,datas)  ))
  #writeLines('hyper')
  param.sel <- unlist(lapply(param.sel, function(x) x$auc))
  
  param <- param[[which.max(param.sel)]]
  param$final=T
  
  writeLines('final train')
  trainedModel <- do.call(gbm_model2, c(param,datas)  )$model
  
  comp <- Sys.time() - start
  if(!quiet)
    writeLines(paste0('Model GBM trained - took:',  format(comp, digits=3)))
  
  varImp <- xgboost::xgb.importance(model =trainedModel)
  varImp$Feature <- as.double(varImp$Feature)
  varImp<- merge(varImp, ff::as.ram(result$covariateRef), by.x='Feature', by.y='covariateId', all=T)
  varImp$Gain[is.na(varImp$Gain)] <- 0
  varImp <- varImp[order(-varImp$Gain),]
  colnames(varImp)[colnames(varImp)=='Gain'] <- 'value'
  
  result <- list(model = trainedModel,
                 modelSettings = list(model='gbm_xgboost', modelParameters=param), #todo get lambda as param
                 trainCVAuc = NULL,
                 metaData = plpData$metaData,
                 populationSettings = attr(population, 'metaData'),
                 outcomeId=outcomeId,# can use populationSettings$outcomeId?
                 cohortId=cohortId,
                 varImp = varImp,
                 trainingTime=comp,
                 covariateMap=result$map
  )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'xgboost'
  attr(result, 'predictionType') <- 'binary'
  return(result)
}

gbm_model2 <- function(data, population,
                       max.depth=6, min_child_weight=20, nthread=20,
                       nround=100, eta=0.1, final=F, ...){
  
  writeLines(paste('Training GBM with ',length(unique(population$indexes)),' fold CV'))
  if(!is.null(population$indexes) && final==F){
    index_vect <- unique(population$indexes)
    perform <- c()
    for(index in 1:length(index_vect )){
      writeLines(paste('Fold ',index, ' -- with ', sum(population$index!=index),'train rows'))
      train <- xgboost::xgb.DMatrix(data = data[population$index!=index,], label=population$outcomeCount[population$index!=index])
      test <- xgboost::xgb.DMatrix(data = data[population$index==index,], label=population$outcomeCount[population$index==index])
      watchlist <- list(train=train, test=test)
      
      model <- xgboost::xgb.train(data = train, 
                                  max.depth = max.depth, eta = eta, nthread = nthread, 
                                  min_child_weight = min_child_weight,
                                  nround = nround,
                                  watchlist = watchlist,
                                  objective = "binary:logistic",
                                  eval.metric = "logloss", eval.metric = "auc",
                                  print.every.n=10)
      
      pred <- xgboost::predict(model, data[population$index==index,])
      prediction <- population[population$index==index,]
      prediction$value <- pred
      attr(prediction, "metaData") <- list(predictionType = "binary") 
      perform <- c(perform,computeAuc(prediction))
    }
    auc <- mean(perform)
    
  } else {
    train <- xgboost::xgb.DMatrix(data = data, label=population$outcomeCount)
    model <- xgboost::xgb.train(data = train, 
                                max.depth = max.depth, eta = eta, nthread = nthread, 
                                min_child_weight = min_child_weight,
                                nround = nround,
                                objective = "binary:logistic",
                                eval.metric = "logloss", eval.metric = "auc",
                                print.every.n=10)
    
    pred <- xgboost::predict(model, data)
    prediction <- population
    prediction$value <- pred
    attr(prediction, "metaData") <- list(predictionType = "binary") 
    auc <-computeAuc(prediction)
  }
  param.val <- paste0('max depth: ',max.depth,'-- min_child_weight: ', min_child_weight, 
                      '-- nthread: ', nthread, ' nround: ',nround, '-- eta: ', eta)
  writeLines('==========================================')
  writeLines(paste0('GMB with parameters:', param.val,' obtained an AUC of ',auc))
  writeLines('==========================================')
  
  result <- list(model=model,
                 auc=auc)
  return(result)
}