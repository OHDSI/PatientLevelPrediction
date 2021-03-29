# @file DeepNN.R
#
# Copyright 2020 Observational Health Data Sciences and Informatics
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
  
  ensure_installed("keras")
  
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
  if (!FeatureExtraction::isCovariateData(plpData$covariateData)){
    stop('DeepNN requires correct covariateData')
  }
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
  population$y <- keras::to_categorical(population$outcomeCount, length(unique(population$outcomeCount)))
  
  # do cross validation to find hyperParameter
  datas <- list(population=population, plpData=data)
  hyperParamSel <- lapply(param, function(x) do.call(trainDeepNN, c(x,datas,train=TRUE)  ))
  
  hyperSummary <- cbind(do.call(rbind, lapply(hyperParamSel, function(x) x$hyperSum)))
  hyperSummary <- as.data.frame(hyperSummary)
  hyperSummary$auc <- unlist(lapply(hyperParamSel, function (x) x$auc))
  hyperParamSel<-unlist(lapply(hyperParamSel, function(x) x$auc))
  
  #now train the final model and return coef
  bestInd <- which.max(abs(unlist(hyperParamSel)-0.5))[1]
  finalModel<-do.call(trainDeepNN, c(param[[bestInd]],datas, train=FALSE))
  
  covariateRef <- as.data.frame(plpData$covariateData$covariateRef)
  incs <- rep(1, nrow(covariateRef)) 
  covariateRef$included <- incs
  covariateRef$covariateValue <- rep(0, nrow(covariateRef))
  
  #modelTrained <- file.path(outLoc) 
  param.best <- param[[bestInd]]
  
  comp <- start-Sys.time()
  
  # train prediction
  prediction <- finalModel$prediction
  finalModel$prediction <- NULL
  
  # return model location 
  result <- list(model = finalModel$model,
                 trainCVAuc = -1, # ToDo decide on how to deal with this
                 hyperParamSearch = hyperSummary,
                 modelSettings = list(model='fitDeepNN',modelParameters=param.best),
                 metaData = plpData$metaData,
                 populationSettings = attr(population, 'metaData'),
                 outcomeId=outcomeId,
                 cohortId=cohortId,
                 varImp = covariateRef, 
                 trainingTime =comp,
                 covariateMap=result$map,
                 predictionTrain = prediction
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
  
  ParallelLogger::logInfo(paste('Training deep neural network with ',length(unique(population$indexes)),' fold CV'))
  if(!is.null(population$indexes) && train==T){
    index_vect <- unique(population$indexes)
    perform <- c()
    
    # create prediction matrix to store all predictions
    predictionMat <- population
    predictionMat$value <- 0
    attr(predictionMat, "metaData") <- list(predictionType = "binary")
    
    for(index in 1:length(index_vect )){
      ParallelLogger::logInfo(paste('Fold ',index, ' -- with ', sum(population$indexes!=index),'train rows'))
      
      model <- keras::keras_model_sequential()
      
      if(is.na(units2)){
      model %>%
        keras::layer_dense(units=units1, #activation='identify', 
                           input_shape=ncol(plpData)) %>%
        keras::layer_dropout(layer_dropout) %>%
        keras::layer_dense(units=2, activation='sigmoid', use_bias = T)
      } else if(is.na(units3)){
        model %>%
          keras::layer_dense(units=units1, #activation='identify', 
                             input_shape=ncol(plpData)) %>%
          keras::layer_dropout(layer_dropout) %>%
          keras::layer_dense(units=units2 #,activation='identify'
                             ) %>%
          keras::layer_dropout(layer_dropout) %>%
          keras::layer_dense(units=2, activation='sigmoid', use_bias = T)
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
          keras::layer_dense(units=2, activation='sigmoid', use_bias = T)
      }
      

      # Prepare model for training
      model %>% keras::compile(
        loss = "binary_crossentropy",
        metrics = c('accuracy'),
        optimizer = keras::optimizer_rmsprop(lr = lr,decay = decay)
      )
      earlyStopping=keras::callback_early_stopping(monitor = "val_loss", patience=10,mode="auto",min_delta = 1e-4)
      reduceLr=keras::callback_reduce_lr_on_plateau(monitor="val_loss", factor =0.1, 
                                                    patience = 5,mode = "auto", min_delta = 1e-5, cooldown = 0, min_lr = 0)
      
      class_weight=list("0"=1,"1"=outcome_weight)
      
      data <- plpData[population$rowId[population$indexes!=index],]
      
      #Extract validation set first - 10k people or 5%
      valN <- min(10000,sum(population$indexes!=index)*0.05)
      val_rows<-sample(1:sum(population$indexes!=index), valN, replace=FALSE)
      train_rows <- c(1:sum(population$indexes!=index))[-val_rows]
      
      sampling_generator<-function(data, population, batch_size, train_rows, index){
        function(){
          gc()
          rows<-sample(train_rows, batch_size, replace=FALSE)
          
          list(as.array(data[rows,]),
               population$y[population$indexes!=index,1:2][rows,])
        }
      }
      
      
      #print(table(population$y))
      
      history <- model %>% keras::fit_generator(sampling_generator(data,population,batch_size,train_rows, index),
                                                steps_per_epoch = floor(sum(population$indexes!=index)/batch_size),
                                                epochs=epochs,
                                                validation_data=list(as.array(data[val_rows,]),
                                                                     population$y[population$indexes!=index,1:2][val_rows,]),
                                                callbacks=list(earlyStopping,reduceLr),
                                                class_weight=class_weight)
      
      
      # batch prediciton 
      maxVal <- sum(population$indexes==index)
      batches <- lapply(1:ceiling(maxVal/batch_size), function(x) ((x-1)*batch_size+1):min((x*batch_size),maxVal))
      prediction <- population[population$indexes==index,]
      prediction$value <- 0
      for(batch in batches){
        pred <- keras::predict_proba(model, as.array(plpData[population$rowId[population$indexes==index],][batch,,drop=FALSE]))
        prediction$value[batch] <- pred[,2]
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
    ParallelLogger::logInfo('==========================================')
    ParallelLogger::logInfo(paste0('DeepNN with parameters:', param.val,' obtained an AUC of ',auc))
    ParallelLogger::logInfo('==========================================')
    
  } else {
    
    model <- keras::keras_model_sequential()
    if(is.na(units2)){
      model %>%
        keras::layer_dense(units=units1, #activation='identify', 
                           input_shape=ncol(plpData)) %>%
        keras::layer_dropout(layer_dropout) %>%
        keras::layer_dense(units=2, activation='sigmoid', use_bias = T)
    } else if(is.na(units3)){
      model %>%
        keras::layer_dense(units=units1, #activation='identify', 
                           input_shape=ncol(plpData)) %>%
        keras::layer_dropout(layer_dropout) %>%
        keras::layer_dense(units=units2 #,activation='identify'
                           ) %>%
        keras::layer_dropout(layer_dropout) %>%
        keras::layer_dense(units=2, activation='sigmoid', use_bias = T)
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
        keras::layer_dense(units=2, activation='sigmoid', use_bias = T)
    }
    
    # Prepare model for training
    model %>% keras::compile(
      loss = "binary_crossentropy",
      metrics = c('accuracy'),
      optimizer = keras::optimizer_rmsprop(lr = lr,decay = decay)
    )
    earlyStopping=keras::callback_early_stopping(monitor = "val_loss", patience=10,mode="auto",min_delta = 1e-4)
    reduceLr=keras::callback_reduce_lr_on_plateau(monitor="val_loss", factor =0.1, 
                                                  patience = 5,mode = "auto", min_delta = 1e-5, cooldown = 0, min_lr = 0)
    
    class_weight=list("0"=1,"1"=outcome_weight)
    
    #Extract validation set first - 10k people or 5%
    valN <- min(10000,nrow(population)*0.05)
    val_rows<-sample(1:nrow(population), valN, replace=FALSE)
    train_rows <- c(1:nrow(population))[-val_rows]
    
    sampling_generator2<-function(data, population, batch_size, train_rows){
      function(){
        gc()
        rows<-sample(train_rows, batch_size, replace=FALSE)
        
        list(as.array(data[rows,,]),
             population$y[,1:2][rows,])
      }
    }

    history <- model %>% keras::fit_generator(sampling_generator2(plpData,population,batch_size,train_rows),
                                              steps_per_epoch = nrow(population)/batch_size,
                                              epochs=epochs,
                                              validation_data=list(as.array(plpData[val_rows,,]),
                                                                   population$y[val_rows,1:2]),
                                              callbacks=list(earlyStopping,reduceLr),
                                              class_weight=class_weight)
    
    
    # batch prediciton 
    maxVal <- nrow(population)
    batches <- lapply(1:ceiling(maxVal/batch_size), function(x) ((x-1)*batch_size+1):min((x*batch_size),maxVal))
    prediction <- population
    prediction$value <- 0
    for(batch in batches){
      pred <- keras::predict_proba(model, as.array(plpData[batch,,drop=FALSE]))
      prediction$value[batch] <- pred[,2]
    }
    
    attr(prediction, "metaData") <- list(predictionType = "binary")
    auc <- computeAuc(prediction)
    foldPerm <- auc
    predictionMat <- prediction
  }
  
  result <- list(model=model,
                 auc=auc,
                 prediction = predictionMat,
                 hyperSum = unlist(list(units1=units1,units2=units2,units3=units3, 
                                        layer_dropout=layer_dropout,lr =lr, decay=decay,
                                        batch_size = batch_size, epochs= epochs))
  )
  return(result)
  
}



#' [Under development] Transfer learning
#'
#' @param plpResult   The plp result when training a kersa deep learning model on big data
#' @param plpData     The new data to fine tune the model on
#' @param population  The population for the new data
#' @param fixLayers   boolean specificying whether to fix weights in model being transferred
#' @param includeTop  If TRUE the final layer of the model being transferred is removed 
#' @param addLayers   vector specifying nodes in each layer to add e.g. c(100,10) will add another layer with 100 nodels and then a final layer with 10
#' @param layerDropout  Add dropout to each new layer (binary vector length of addLayers)
#' @param layerActivation Activation function for each new layer (string vector length of addLayers)
#' @param outcomeWeight  The weight to assign the class 1 when training the model
#' @param batchSize   Size of each batch for updating layers
#' @param epochs      Number of epoches to run 
#' @examples
#' \dontrun{
#' modelSet <- setDeepNN()
#' plpResult <- runPlp(plpData, population, modelSettings = modelSet, ...)
#' 
#' transferLearning(...)
#' }
#' @export
transferLearning <- function(plpResult, 
                 plpData, 
                 population, 
                 fixLayers = T, 
                 includeTop= F,
                 addLayers = c(100,10), 
                 layerDropout = c(T,T),
                 layerActivation = c('relu','softmax'),
                 outcomeWeight = 1,
                 batchSize = 10000, 
                 epochs=20){
  
  # checks
  if(!is.null(addLayers)){
    if(length(addLayers)!=length(layerDropout)){
      stop('Layer vector not same length as layer dropout vector')
    }
    if(length(addLayers)!=length(layerActivation)){
      stop('Layer vector not same length as layer activation vector')
    }
  }
  if(batchSize > nrow(population)){
    warning('batchSize is too big for your data...')
    batchSize = nrow(population)/10
  }
  
  if(!includeTop){
  # remove last layer if not dropout
    if(length(grep('Dropout',as.character(plpResult$model$model$layers[[length(plpResult$model$model$layers)]])))==0){
      keras::pop_layer(plpResult$model$model)
    }
  }
 
  # create the base pre-trained
  base_model <-  plpResult$model$model
  
  # add our custom layers
  predictions <- base_model$output 
  
  
  # fix the older layers
  if(fixLayers){
    for (i in 1:length(base_model$layers)){
      try({base_model$layers[[i]]$trainable <- F}, silent = TRUE)
    }
  }
  
  ## add loop over settings here - move code to new function and call it
  ##====
  # !!check this looping logic works
  if(!is.null(addLayers)){
    for(i in 1:length(addLayers)){
      predictions <- keras::layer_dense(predictions,units = addLayers[i], activation = layerActivation[i])
      if(layerDropout[i]){
        predictions <- keras::layer_dropout(predictions, rate = 0.5)
      }
    }
  }
  
  # add find layer for binary outcome
  predictions <- keras::layer_dense(predictions,units = 2, activation = 'sigmoid')
  
  
  # this is the model we will train
  model <- keras::keras_model(inputs = base_model$input, outputs = predictions)
  
  # compile the model (should be done *after* setting layers to non-trainable)
  model %>% keras::compile(optimizer = 'rmsprop', loss = 'binary_crossentropy',
                           metrics = c('accuracy'))
  
  
  # make this input...
  earlyStopping=keras::callback_early_stopping(monitor = "val_loss", patience=10,mode="auto",min_delta = 1e-4)
  reduceLr=keras::callback_reduce_lr_on_plateau(monitor="val_loss", factor =0.1, 
                                                patience = 5,mode = "auto", min_delta = 1e-5, cooldown = 0, 
                                                min_lr = 0)
  
  class_weight=list("0"=1,"1"=outcomeWeight)
  

  sampling_generator<-function(data, population, batchSize, train_rows){
    function(){
      gc()
      rows<-sample(train_rows, batchSize, replace=FALSE)
      
      list(as.array(data[rows,]),
           population$y[rows,1:2])
    }
  }
  
  # convert plpdata to matrix:
  metaData <- attr(population, 'metaData')
  if(!is.null(population$indexes))
    population <- population[population$indexes>0,]
  attr(population, 'metaData') <- metaData
  
  result<- toSparseM(plpData,population,map=plpResult$model$covariateMap, temporal=F)
  data <- result$data
  population$y <- keras::to_categorical(population$outcomeCount, length(unique(population$outcomeCount)))
  
  #Extract validation set first - 10k people or 5%
  valN <- min(10000,nrow(population)*0.05)
  val_rows<-sample(1:nrow(population), valN, replace=FALSE)
  train_rows <- c(1:nrow(population))[-val_rows]
  
  history <- model %>% keras::fit_generator(sampling_generator(data,population,batchSize,train_rows),
                                            steps_per_epoch = nrow(population)/batchSize,
                                            epochs=epochs,
                                            validation_data=list(as.array(data[val_rows,,]),
                                                                 population$y[val_rows,1:2]),
                                            callbacks=list(earlyStopping,reduceLr),
                                            class_weight=class_weight)
  
  
  # batch prediciton 
  maxVal <- nrow(population)
  batches <- lapply(1:ceiling(maxVal/batchSize), function(x) ((x-1)*batchSize+1):min((x*batchSize),maxVal))
  prediction <- population
  prediction$value <- 0
  for(batch in batches){
    pred <- model$predict(as.array(data[batch,,drop=FALSE])) # added drop=FALSE
    prediction$value[batch] <- pred[,2]
  }
  
  ##===
  
  attr(prediction, "metaData") <- list(predictionType = "binary")
  auc <- computeAuc(prediction)
  foldPerm <- auc
  predictionMat <- prediction

result <- list(model=model,
               auc=auc,
               prediction = predictionMat,
               hyperSum = list(fixLayers = fixLayers, 
                               addLayers = addLayers, 
                               layerDropout = layerDropout,
                               layerActivation = layerActivation))
  
  return(result)
  
}
