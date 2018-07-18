# @file DeepNN.R
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

#' Create setting for DeepNN model
#'
#' @param units         The number of units of the deep network - as a list of vectors
#' @param layer_dropout      The layer dropout rate (regularisation)
#' @param lr                 Learning rate
#' @param decay              Learning rate decay over each update.
#' @param outcome_weight      The weight of the outcome class in the loss function
#' @param batch_size          The number of data points to use per training batch
#' @param epochs          Number of times to iterate over dataset
#' @param seed            Random seed used by deep learning model
#'
#' @examples
#' \dontrun{
#' model <- setDeepNN()
#' }
#' @export
setDeepNN <- function(units=list(c(128, 64), 128), layer_dropout=c(0.2),
                      lr =c(1e-4), decay=c(1e-5), outcome_weight = c(1.0), batch_size = c(100), 
                      epochs= c(100),  seed=NULL  ){
  
  # if(class(indexFolder)!='character')
  #     stop('IndexFolder must be a character')
  # if(length(indexFolder)>1)
  #     stop('IndexFolder must be one')
  # 
  # if(class(units)!='numeric')
  #     stop('units must be a numeric value >0 ')
  # if(units<1)
  #     stop('units must be a numeric value >0 ')
  # 
  # #if(length(units)>1)
  # #    stop('units can only be a single value')
  # 
  # if(class(recurrent_dropout)!='numeric')
  #     stop('dropout must be a numeric value >=0 and <1')
  # if( (recurrent_dropout<0) | (recurrent_dropout>=1))
  #     stop('dropout must be a numeric value >=0 and <1')
  # if(class(layer_dropout)!='numeric')
  #     stop('layer_dropout must be a numeric value >=0 and <1')
  # if( (layer_dropout<0) | (layer_dropout>=1))
  #     stop('layer_dropout must be a numeric value >=0 and <1')
  # if(class(lr)!='numeric')
  #     stop('lr must be a numeric value >0')
  # if(lr<=0)
  #     stop('lr must be a numeric value >0')
  # if(class(decay)!='numeric')
  #     stop('decay must be a numeric value >=0')
  # if(decay<=0)
  #     stop('decay must be a numeric value >=0')
  # if(class(outcome_weight)!='numeric')
  #     stop('outcome_weight must be a numeric value >=0')
  # if(outcome_weight<=0)
  #     stop('outcome_weight must be a numeric value >=0')
  # if(class(batch_size)!='numeric')
  #     stop('batch_size must be an integer')
  # if(batch_size%%1!=0)
  #     stop('batch_size must be an integer')
  # if(class(epochs)!='numeric')
  #     stop('epochs must be an integer')
  # if(epochs%%1!=0)
  #     stop('epochs must be an integer')
  # if(!class(seed)%in%c('numeric','NULL'))
  #     stop('Invalid seed')
  #if(class(UsetidyCovariateData)!='logical')
  #    stop('UsetidyCovariateData must be an TRUE or FALSE')
  
  param <- expand.grid(units=units,
    layer_dropout=layer_dropout,
    lr =lr, decay=decay, outcome_weight=outcome_weight,epochs= epochs,
    seed=ifelse(is.null(seed),'NULL', seed))
  param$units1=unlist(lapply(param$units, function(x) x[1])) 
  param$units2=unlist(lapply(param$units, function(x) x[2])) 
  param$units3=unlist(lapply(param$units, function(x) x[3]))
  
  result <- list(model='fitDeepNN', param=split(param,
    1:(length(units)*length(layer_dropout)*length(lr)*length(decay)*length(outcome_weight)*length(epochs)*max(1,length(seed)))),
    name='DeepNN'
  )
  
  class(result) <- 'modelSettings' 
  return(result)
}


fitDeepNN <- function(plpData,population, param, search='grid', quiet=F,
                      outcomeId, cohortId, ...){
  # check plpData is coo format:
  if(!'ffdf'%in%class(plpData$covariates) )
    stop('DeepNN requires plpData in coo format')
  if(!is.null(plpData$timeRef)){
    warning('Data temporal but deepNN uses non-temporal data...')
  }
  
  metaData <- attr(population, 'metaData')
  if(!is.null(population$indexes))
    population <- population[population$indexes>0,]
  attr(population, 'metaData') <- metaData
  
  start<-Sys.time()
  
  result<- toSparseM(plpData,population,map=NULL, temporal=F)
  data <- result$data
  
  #one-hot encoding
  population$y <- population$outcomeCount#keras::to_categorical(population$outcomeCount, length(unique(population$outcomeCount)))
  
  # do cross validation to find hyperParameter
  datas <- list(population=population, plpData=data)
  hyperParamSel <- lapply(param, function(x) do.call(trainDeepNN, c(x,datas,train=TRUE)  ))
  
  hyperSummary <- cbind(do.call(rbind, lapply(hyperParamSel, function(x) x$hyperSum)))
  hyperSummary <- as.data.frame(hyperSummary)
  hyperSummary$auc <- unlist(lapply(hyperParamSel, function (x) x$auc))
  hyperParamSel<-unlist(lapply(hyperParamSel, function(x) x$auc))
  
  #now train the final model and return coef
  bestInd <- which.max(abs(unlist(hyperParamSel)-0.5))[1]
  finalModel<-do.call(trainDeepNN, c(param[[bestInd]],datas, train=FALSE))$model
  
  covariateRef <- ff::as.ram(plpData$covariateRef)
  incs <- rep(1, nrow(covariateRef)) 
  covariateRef$included <- incs
  
  #modelTrained <- file.path(outLoc) 
  param.best <- param[[bestInd]]
  
  comp <- start-Sys.time()
  
  # return model location 
  result <- list(model = finalModel,
                 trainCVAuc = -1, # ToDo decide on how to deal with this
                 hyperParamSearch = hyperSummary,
                 modelSettings = list(model='fitDeepNN',modelParameters=param.best),
                 metaData = plpData$metaData,
                 populationSettings = attr(population, 'metaData'),
                 outcomeId=outcomeId,
                 cohortId=cohortId,
                 varImp = covariateRef, 
                 trainingTime =comp,
                 covariateMap=result$map
  )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'deep'
  attr(result, 'predictionType') <- 'binary'
  
  return(result)
}

trainDeepNN<-function(plpData, population,
                      units1=128, units2= NULL, units3=NULL, 
                      layer_dropout=0.2,
                      lr =1e-4, decay=1e-5, outcome_weight = 1.0, batch_size = 100, 
                      epochs= 100, seed=NULL, train=TRUE, ...){
  
  OhdsiRTools::logInfo(paste('Training deep neural network with ',length(unique(population$indexes)),' fold CV'))
  if(!is.null(population$indexes) && train==T){
    index_vect <- unique(population$indexes)
    perform <- c()
    
    # create prediction matrix to store all predictions
    predictionMat <- population
    predictionMat$value <- 0
    attr(predictionMat, "metaData") <- list(predictionType = "binary")
    
    for(index in 1:length(index_vect )){
      OhdsiRTools::logInfo(paste('Fold ',index, ' -- with ', sum(population$indexes!=index),'train rows'))
      
      model <- keras::keras_model_sequential()
      
      if(is.na(units2)){
      model %>%
        keras::layer_dense(units=units1, #activation='identify', 
                           input_shape=ncol(plpData)) %>%
        keras::layer_dropout(layer_dropout) %>%
        keras::layer_dense(units=1, activation='sigmoid', use_bias = T)
      } else if(is.na(units3)){
        model %>%
          keras::layer_dense(units=units1, #activation='identify', 
                             input_shape=ncol(plpData)) %>%
          keras::layer_dropout(layer_dropout) %>%
          keras::layer_dense(units=units2 #,activation='identify'
                             ) %>%
          keras::layer_dropout(layer_dropout) %>%
          keras::layer_dense(units=1, activation='sigmoid', use_bias = T)
      } else{
        model %>%
          keras::layer_dense(units=units1, #activation='identify', 
                             input_shape=ncol(plpData)) %>%
          keras::layer_dropout(layer_dropout) %>%
          keras::layer_dense(units=units2 #,activation='identify'
                             ) %>%
          keras::layer_dropout(layer_dropout) %>%
          keras::layer_dense(units=units3 #,activation='identify'
                             ) %>%
          keras::layer_dropout(layer_dropout) %>%
          keras::layer_dense(units=1, activation='sigmoid', use_bias = T)
      }
      
      model %>% keras::compile(
        loss = 'binary_crossentropy'
        ,metrics = c('accuracy'),
        #optimizer = "adadelta"
        optimizer = keras::optimizer_rmsprop(lr = lr,decay = decay)
      )
      
      class_weight=list("0"=1,"1"=outcome_weight)
      maxVal <- sum(population$indexes!=index)
      batches <- lapply(1:ceiling(maxVal/batch_size), function(x) ((x-1)*batch_size+1):min((x*batch_size),maxVal))
      
      for(e in 1:epochs){
        for(batch in batches){
          model %>%keras::train_on_batch(x=as.array(plpData[population$rowId[population$indexes!=index],][batch,]),
                                         y=population$y[population$indexes!=index][batch]
                                         #,callbacks=list(earlyStopping,reduceLr)
                                         ,class_weight=class_weight
          )
        }
      }
      
      
      # batch prediciton 
      maxVal <- sum(population$indexes==index)
      batches <- lapply(1:ceiling(maxVal/batch_size), function(x) ((x-1)*batch_size+1):min((x*batch_size),maxVal))
      prediction <- population[population$indexes==index,]
      prediction$value <- 0
      for(batch in batches){
        pred <- keras::predict_proba(model, as.array(plpData[population$rowId[population$indexes==index],][batch,]))
        prediction$value[batch] <- pred
      }
      
      attr(prediction, "metaData") <- list(predictionType = "binary")
      aucVal <- computeAuc(prediction)
      perform <- c(perform,aucVal)
      
      # add the fold predictions and compute AUC after loop
      predictionMat$value[population$indexes==index] <- prediction$value
      
    }
    
    auc <- computeAuc(predictionMat)
    foldPerm <- perform
    
    # Output  ----------------------------------------------------------------
    param.val <- paste0('units1: ',units1,'units2: ',units2,'units3: ',units3,
                        'layer_dropout: ',layer_dropout,'-- lr: ', lr,
                        '-- decay: ', decay, '-- batch_size: ',batch_size, '-- epochs: ', epochs)
    OhdsiRTools::logInfo('==========================================')
    OhdsiRTools::logInfo(paste0('CIReNN with parameters:', param.val,' obtained an AUC of ',auc))
    OhdsiRTools::logInfo('==========================================')
    
  } else {
    
    model <- keras::keras_model_sequential()
    if(is.na(units2)){
      model %>%
        keras::layer_dense(units=units1, #activation='identify', 
                           input_shape=ncol(plpData)) %>%
        keras::layer_dropout(layer_dropout) %>%
        keras::layer_dense(units=1, activation='sigmoid', use_bias = T)
    } else if(is.na(units3)){
      model %>%
        keras::layer_dense(units=units1, #activation='identify', 
                           input_shape=ncol(plpData)) %>%
        keras::layer_dropout(layer_dropout) %>%
        keras::layer_dense(units=units2 #,activation='identify'
                           ) %>%
        keras::layer_dropout(layer_dropout) %>%
        keras::layer_dense(units=1, activation='sigmoid', use_bias = T)
    } else{
      model %>%
        keras::layer_dense(units=units1, #activation='identify', 
                           input_shape=ncol(plpData)) %>%
        keras::layer_dropout(layer_dropout) %>%
        keras::layer_dense(units=units2 #,activation='identify'
                           ) %>%
        keras::layer_dropout(layer_dropout) %>%
        keras::layer_dense(units=units3 #,activation='identify'
                           ) %>%
        keras::layer_dropout(layer_dropout) %>%
        keras::layer_dense(units=1, activation='sigmoid', use_bias = T)
    }
    
    model %>% keras::compile(
      loss = 'binary_crossentropy'
      ,metrics = c('accuracy'),
      #optimizer = "adadelta"
      optimizer = keras::optimizer_rmsprop(lr = lr,decay = decay)
    )
    
    class_weight=list("0"=1,"1"=outcome_weight)
    
    maxVal <- length(population$indexes)
    batches <- lapply(1:ceiling(maxVal/batch_size), function(x) ((x-1)*batch_size+1):min((x*batch_size),maxVal))
    
    for(e in 1:epochs){
      for(batch in batches){
        model %>% keras::train_on_batch(x=as.array(plpData[batch,,]),
                                        y=population$y[batch]
                                        ,class_weight=class_weight
        )
      }
    }
    
    # batched prediciton 
    prediction <- population
    prediction$value <- 0
    for(batch in batches){
      pred <- keras::predict_proba(model, as.array(plpData[batch,]))
      prediction$value[batch] <- pred
    }
    
    attr(prediction, "metaData") <- list(predictionType = "binary")
    auc <- computeAuc(prediction)
    foldPerm <- auc
  }
  
  result <- list(model=model,
                 auc=auc,
                 hyperSum = unlist(list(units1=units1,units2=units2,units3=units3, 
                                        layer_dropout=layer_dropout,lr =lr, decay=decay,
                                        batch_size = batch_size, epochs= epochs))
  )
  return(result)
  
}
