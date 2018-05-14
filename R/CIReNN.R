# @file CIReNN.R
# Code edited from OHDSI contributor @chandryou CIReNN branch
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

# BuildCIReNN<-function(outcomes=ff::as.ffdf(population[,c('rowId','y')]),
#     covariates = result$data,
#     indexFolder=indexFolder){
# 
# }

#' Create setting for CIReNN model
#'
#' @param units         The number of units of RNN layer - as a list of vectors
#' @param recurrent_dropout  The reccurrent dropout rate (regularisation)
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
#' model.CIReNN <- setCIReNN()
#' }
#' @export
setCIReNN <- function(units=c(128, 64), recurrent_dropout=c(0.2), layer_dropout=c(0.2),
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
  
  result <- list(model='fitCIReNN', param=split(expand.grid(
    units=units, recurrent_dropout=recurrent_dropout, 
    layer_dropout=layer_dropout,
    lr =lr, decay=decay, outcome_weight=outcome_weight,epochs= epochs,
    seed=ifelse(is.null(seed),'NULL', seed)),
    1:(length(units)*length(recurrent_dropout)*length(layer_dropout)*length(lr)*length(decay)*length(outcome_weight)*length(epochs)*max(1,length(seed)))),
    name='CIReNN'
  )

  class(result) <- 'modelSettings' 
  return(result)
}


fitCIReNN <- function(plpData,population, param, search='grid', quiet=F,
                      outcomeId, cohortId, ...){
  # check plpData is coo format:
  if(!'ffdf'%in%class(plpData$covariates) )
    stop('CIReNN requires plpData in coo format')
  
  metaData <- attr(population, 'metaData')
  if(!is.null(population$indexes))
    population <- population[population$indexes>0,]
  attr(population, 'metaData') <- metaData
  
  start<-Sys.time()
  
  result<- toSparseM(plpData,population,map=NULL, temporal=T)
  data <- result$data
  
  #one-hot encoding
  population$y <- population$outcomeCount#keras::to_categorical(population$outcomeCount, length(unique(population$outcomeCount)))

  # do cross validation to find hyperParameter
  datas <- list(population=population, plpData=data)
  hyperParamSel <- lapply(param, function(x) do.call(trainCIReNN, c(x,datas,train=TRUE)  ))
  
  hyperSummary <- cbind(do.call(rbind, lapply(hyperParamSel, function(x) x$hyperSum)))
  hyperSummary <- as.data.frame(hyperSummary)
  hyperSummary$auc <- unlist(lapply(hyperParamSel, function (x) x$auc))
  hyperParamSel<-unlist(lapply(hyperParamSel, function(x) x$auc))
  
  #now train the final model and return coef
  bestInd <- which.max(abs(unlist(hyperParamSel)-0.5))[1]
  finalModel<-do.call(trainCIReNN, c(param[[bestInd]],datas, train=FALSE))$model
  
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
                 modelSettings = list(model='fitCIReNN',modelParameters=param.best),
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

trainCIReNN<-function(plpData, population,
                      units=128, recurrent_dropout=0.2, layer_dropout=0.2,
                      lr =1e-4, decay=1e-5, outcome_weight = 1.0, batch_size = 100, 
                      epochs= 100, seed=NULL, train=TRUE){
  
 writeLines(paste('Training recurrent neural network with ',length(unique(population$indexes)),' fold CV'))
  if(!is.null(population$indexes) && train==T){
    index_vect <- unique(population$indexes)
    perform <- c()
    
    # create prediction matrix to store all predictions
    predictionMat <- population
    predictionMat$value <- 0
    attr(predictionMat, "metaData") <- list(predictionType = "binary")
    
    for(index in 1:length(index_vect )){
      writeLines(paste('Fold ',index, ' -- with ', sum(population$indexes!=index),'train rows'))
      
      ##single-layer gru
      model <- keras::keras_model_sequential()
      model %>%
        keras::layer_gru(units=units, recurrent_dropout = recurrent_dropout,
                         input_shape = c(dim(plpData)[2],dim(plpData)[3]), #time step x number of features
                         return_sequences=FALSE#,stateful=TRUE
        ) %>%
        keras::layer_dropout(layer_dropout) %>%
        keras::layer_dense(units=1, activation='softmax')
      
      model %>% keras::compile(
        loss = 'binary_crossentropy',
        metrics = c('accuracy'),
        optimizer = keras::optimizer_rmsprop(lr = lr,decay = decay)
      )
      earlyStopping=keras::callback_early_stopping(monitor = "val_loss", patience=10,mode="auto",min_delta = 1e-4)
      reduceLr=keras::callback_reduce_lr_on_plateau(monitor="val_loss", factor =0.1, 
                                                    patience = 5,mode = "auto", epsilon = 1e-5, cooldown = 0, min_lr = 0)
      
      class_weight=list("0"=1,"1"=outcome_weight)
      maxVal <- sum(population$indexes!=index)
      batches <- lapply(1:ceiling(maxVal/batch_size), function(x) ((x-1)*batch_size+1):min((x*batch_size),maxVal))
      
      for(e in 1:epochs){
        for(batch in batches){
          model %>%keras::train_on_batch(x=as.array(plpData[population$rowId[population$indexes!=index],,][batch,,]),
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
        pred <- keras::predict_on_batch(model, as.array(plpData[population$rowId[population$indexes==index],,][batch,,]))
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
    param.val <- paste0('units: ',units,'-- recurrent_dropout: ', recurrent_dropout,
                        'layer_dropout: ',layer_dropout,'-- lr: ', lr,
                        '-- decay: ', decay, '-- batch_size: ',batch_size, '-- epochs: ', epochs)
    writeLines('==========================================')
    writeLines(paste0('CIReNN with parameters:', param.val,' obtained an AUC of ',auc))
    writeLines('==========================================')
    
      } else {
        ##single-layer gru
        model <- keras::keras_model_sequential()
        model %>%
          keras::layer_gru(units=units, recurrent_dropout = recurrent_dropout,
                           input_shape = c(dim(plpData)[2],dim(plpData)[3]), #time step x number of features
                           return_sequences=FALSE#,stateful=TRUE
          ) %>%
          keras::layer_dropout(layer_dropout) %>%
          keras::layer_dense(units=1, activation='softmax')
        
        model %>% keras::compile(
          loss = 'binary_crossentropy',
          metrics = c('accuracy'),
          optimizer = keras::optimizer_rmsprop(lr = lr,decay = decay)
        )
        earlyStopping=keras::callback_early_stopping(monitor = "val_loss", patience=10,mode="auto",min_delta = 1e-4)
        reduceLr=keras::callback_reduce_lr_on_plateau(monitor="val_loss", factor =0.1, 
                                                      patience = 5,mode = "auto", epsilon = 1e-5, cooldown = 0, min_lr = 0)
        
        
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
      pred <- keras::predict_on_batch(model, as.array(plpData[batch,,]))
      prediction$value[batch] <- pred
    }
    
    attr(prediction, "metaData") <- list(predictionType = "binary")
    auc <- computeAuc(prediction)
    foldPerm <- auc
  }
  
  result <- list(model=model,
                 auc=auc,
                 hyperSum = unlist(list(units=units, recurrent_dropout=recurrent_dropout, 
                                        layer_dropout=layer_dropout,lr =lr, decay=decay,
                                        batch_size = batch_size, epochs= epochs))
  )
  return(result)
  
}