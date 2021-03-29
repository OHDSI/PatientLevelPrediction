# @file CovNN2.R
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

#' Create setting for CovNN2 model - convolution across input and time - https://arxiv.org/pdf/1608.00647.pdf
#'
#' @param batchSize         The number of samples to used in each batch during model training
#' @param outcomeWeight     The weight assined to the outcome (make greater than 1 to reduce unballanced label issue)
#' @param lr                The learning rate
#' @param decay             The decay of the learning rate
#' @param dropout            [currently not used] the dropout rate for regularisation
#' @param epochs            The number of times data is used to train the model (e.g., epoches=1 means data only used once to train)
#' @param filters            The number of columns output by each convolution
#' @param kernelSize        The number of time dimensions used for each convolution
#' @param loss               The loss function implemented 
#' @param seed               The random seed
#'
#' @examples
#' \dontrun{
#' model.CovNN <- setCovNN()
#' }
#' @export
setCovNN2 <- function(batchSize = 1000,
                      outcomeWeight=1,
                      lr=0.00001,
                      decay=0.000001,
                      dropout=0,
                      epochs = 10,
                      filters = 3, kernelSize = 10,
                      loss = "binary_crossentropy", 
                      seed=NULL  ){
  #[TODO: add input checks...]
  
  ensure_installed("keras")
  
  if(!is.null(seed)){
    warning('seed currently not implemented in CovNN')
  }
  
  result <- list(model='fitCovNN2', param=split(expand.grid(
    batchSize=batchSize, 
    outcomeWeight=outcomeWeight,
    lr=lr,
    decay=decay,
    dropout=dropout,
    epochs= epochs,filters=filters,
    kernelSize=kernelSize,loss =loss,
    seed=ifelse(is.null(seed),'NULL', seed)),
    1:(length(batchSize)*length(outcomeWeight)*length(epochs)*
         length(filters)*length(lr)*length(decay)*
         length(kernelSize)*length(loss)*max(1,length(seed)))),
    name='CovNN2'
  )
  
  class(result) <- 'modelSettings' 
  return(result)
}


fitCovNN2 <- function(plpData,population, param, search='grid', quiet=F,
                      outcomeId, cohortId, ...){
  # check plpData is coo format:
  if (!FeatureExtraction::isCovariateData(plpData$covariateData))
    stop("Needs correct covariateData")
  if(is.null(plpData$timeRef)){
    stop('Data not temporal...')
  }
  
  metaData <- attr(population, 'metaData')
  if(!is.null(population$indexes))
    population <- population[population$indexes>0,]
  attr(population, 'metaData') <- metaData
  
  start<-Sys.time()
  
  result<- toSparseM(plpData,population,map=NULL, temporal=T)
  
  data <- result$data#[population$rowId,,]
  #data<-as.array(data) -- cant make dense on big data!
  
  #one-hot encoding
  population$y <- keras::to_categorical(population$outcomeCount, 2)
  #colnames(population$y) <- c('0','1')
  
  # do cross validation to find hyperParameter
  datas <- list(population=population, plpData=data)
  hyperParamSel <- lapply(param, function(x) do.call(trainCovNN2, c(x,datas,train=TRUE)  ))
  
  hyperSummary <- cbind(do.call(rbind, lapply(hyperParamSel, function(x) x$hyperSum)))
  hyperSummary <- as.data.frame(hyperSummary)
  hyperSummary$auc <- unlist(lapply(hyperParamSel, function (x) x$auc))
  hyperParamSel<-unlist(lapply(hyperParamSel, function(x) x$auc))
  
  #now train the final model and return coef
  bestInd <- which.max(abs(unlist(hyperParamSel)-0.5))[1]
  finalModel<-do.call(trainCovNN2, c(param[[bestInd]],datas, train=FALSE))
  
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
                 modelSettings = list(model='fitCovNN2',modelParameters=param.best),
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

trainCovNN2<-function(plpData, population,
                      outcomeWeight=1, lr=0.0001, decay=0.9,
                      dropout=0.5, filters=3,
                      kernelSize = dim(plpData)[3],
                      batchSize, epochs, loss= "binary_crossentropy",
                      seed=NULL, train=TRUE){
  
  if(!is.null(population$indexes) && train==T){
    writeLines(paste('Training covolutional neural network (input and time) with ',length(unique(population$indexes)),' fold CV'))
    
    index_vect <- unique(population$indexes)
    perform <- c()
    
    # create prediction matrix to store all predictions
    predictionMat <- population
    predictionMat$value <- 0
    attr(predictionMat, "metaData") <- list(predictionType = "binary")
    
    for(index in 1:length(index_vect )){
      writeLines(paste('Fold ',index, ' -- with ', sum(population$indexes!=index),'train rows'))
      
      model <- keras::keras_model_sequential()  %>%
        
        ##model_input <- keras::layer_input(shape=c(dim(plpData)[2], dim(plpData)[3]), 
        ##                                      name='initial_input') %>%
        keras::layer_permute(dims = c(2,1), 
                             input_shape = c(dim(plpData)[2], dim(plpData)[3])) %>%
        
        # Begin with 1D convolutional layer across input - hidden layer 1
        ##model_output <-  keras::layer_input(shape=c(dim(plpData)[3], dim(plpData)[2]), 
        ##                                    name='second_input') %>%
        keras::layer_conv_1d(
          filters = filters, 
          kernel_size = dim(plpData)[3]-2,
          padding = "valid"
        ) %>%
        keras::layer_batch_normalization() %>%
        keras::layer_activation(activation = 'relu') %>% 
        # second layer across input - hidden layer 2   time x filters
        keras::layer_conv_1d(
          filters = filters, 
          kernel_size = 3,#filters,
          padding = "valid"
        ) %>%
        keras::layer_batch_normalization() %>%
        keras::layer_activation(activation = 'relu') %>% 
        
        # permute bact to time x var
        keras::layer_permute(dims = c(2,1)) %>% 
        #max pool over time and conv
        keras::layer_max_pooling_1d(pool_size = 2) %>%
        keras::layer_conv_1d(
          filters = filters, 
          kernel_size = 2,
          padding = "valid"
        ) %>%
        keras::layer_batch_normalization() %>%
        keras::layer_activation(activation = 'relu') %>% 
        keras::layer_flatten() %>%
        # final 2 deep layers with dropout and batchnorm/ relu activation
        keras::layer_dropout(rate=dropout) %>%
        # add fully connected layer 2
        keras::layer_dense( 
          units = 100, 
          activation = "linear", use_bias=T
        ) %>%
        keras::layer_batch_normalization() %>%
        keras::layer_activation(activation = 'relu') %>%
        
        # =========== FULLY CONNECTED LAYER 2
        # add drop out of 0.5
        keras::layer_dropout(rate=dropout) %>%
        # add fully connected layer 2
        keras::layer_dense(
          units = 100, 
          activation = "linear", use_bias=T
        ) %>%
        keras::layer_batch_normalization() %>%
        keras::layer_activation(activation = 'relu') %>%
        
        # =========== FINAL LAYER
        keras::layer_dropout(rate=dropout) %>%
        keras::layer_dense(name = 'final',
                           units = 2, 
                           activation = "linear", use_bias=T
        ) %>%
        keras::layer_batch_normalization() %>%
        keras::layer_activation(activation = 'sigmoid', name='main_output')
      
      ##model <- keras::keras_model(
      ##  inputs = c(model_input), 
      ##  outputs = c(model_output)
      ##)
      
      # Prepare model for training
      model %>% keras::compile(
        loss = "binary_crossentropy",
        metrics = c('accuracy'),
        optimizer = keras::optimizer_rmsprop(lr = lr,decay = decay)
      )
      earlyStopping=keras::callback_early_stopping(monitor = "val_loss", patience=10,mode="auto",min_delta = 1e-4)
      reduceLr=keras::callback_reduce_lr_on_plateau(monitor="val_loss", factor =0.1, 
                                                    patience = 5,mode = "auto", min_delta = 1e-5, cooldown = 0, min_lr = 0)
      
      class_weight=list("0"=1,"1"=outcomeWeight)
      
      data <- plpData[population$rowId[population$indexes!=index],,]
      
      #Extract validation set first - 10k people or 5%
      valN <- min(10000,sum(population$indexes!=index)*0.05)
      val_rows<-sample(1:sum(population$indexes!=index), valN, replace=FALSE)
      train_rows <- c(1:sum(population$indexes!=index))[-val_rows]
      
      sampling_generator<-function(data, population, batchSize, train_rows, index){
        function(){
          gc()
          rows<-sample(train_rows, batchSize, replace=FALSE)
          
          list(as.array(data[rows,,]),
               population$y[population$indexes!=index,1:2][rows,])
        }
      }
      
      
      #print(table(population$y))
      if(length(train_rows) < batchSize){
        # checking if this fixes issue with batchsize too big
        batchSize <- length(train_rows)
        ParallelLogger::logInfo('Reducing batchSize to training size')
      }
      
      history <- model %>% keras::fit_generator(sampling_generator(data,population,batchSize,train_rows, index),
                                                steps_per_epoch = length(train_rows)/batchSize,
                                                epochs=epochs,
                                                validation_data=list(as.array(data[val_rows,,]),
                                                                     population$y[population$indexes!=index,1:2][val_rows,]),
                                                callbacks=list(earlyStopping,reduceLr),
                                                class_weight=class_weight)
      
      
      
      
      # batch prediciton 
      maxVal <- sum(population$indexes==index)
      batches <- lapply(1:ceiling(maxVal/batchSize), function(x) ((x-1)*batchSize+1):min((x*batchSize),maxVal))
      prediction <- population[population$indexes==index,]
      prediction$value <- 0
      for(batch in batches){
        pred <- keras::predict_on_batch(model, as.array(plpData[population$rowId[population$indexes==index],,][batch,,]))
        prediction$value[batch] <- as.double(pred[,2])
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
    param.val <- paste0('outcomeWeight: ', outcomeWeight,
                        '-- kernelSize: ',paste0(kernelSize,collapse ='-'),
                        '-- filters: ', filters, '--loss: ', loss, '-- lr: ', lr, '-- decay: ', decay,
                        '-- dropout: ', dropout, '-- batchSize: ',batchSize, '-- epochs: ', epochs)
    writeLines('==========================================')
    writeLines(paste0('CovNN with parameters:', param.val,' obtained an AUC of ',auc))
    writeLines('==========================================')
  } else {
    
    model <- keras::keras_model_sequential()  %>%
      
      keras::layer_permute(dims = c(2,1), 
                           input_shape = c(dim(plpData)[2], dim(plpData)[3])) %>%
      
      # Begin with 1D convolutional layer across input - hidden layer 1
      keras::layer_conv_1d(
        filters = filters, 
        kernel_size = dim(plpData)[3]-2,
        padding = "valid"
      ) %>%
      keras::layer_batch_normalization() %>%
      keras::layer_activation(activation = 'relu') %>% 
      # second layer across input - hidden layer 2   time x filters
      keras::layer_conv_1d(
        filters = filters, 
        kernel_size = 3,
        padding = "valid"
      ) %>%
      keras::layer_batch_normalization() %>%
      keras::layer_activation(activation = 'relu') %>% 
      
      # permute bact to time x var
      keras::layer_permute(dims = c(2,1)) %>% 
      #max pool over time and conv
      keras::layer_max_pooling_1d(pool_size = 2) %>%
      keras::layer_conv_1d(
        filters = filters, 
        kernel_size = 2,
        padding = "valid"
      ) %>%
      keras::layer_batch_normalization() %>%
      keras::layer_activation(activation = 'relu') %>% 
      keras::layer_flatten() %>%
      # final 2 deep layers with dropout and batchnorm/ relu activation
      keras::layer_dropout(rate=dropout) %>%
      # add fully connected layer 2
      keras::layer_dense( 
        units = 100, 
        activation = "linear", use_bias=T
      ) %>%
      keras::layer_batch_normalization() %>%
      keras::layer_activation(activation = 'relu') %>%
      
      # =========== FULLY CONNECTED LAYER 2
      # add drop out of 0.5
      keras::layer_dropout(rate=dropout) %>%
      # add fully connected layer 2
      keras::layer_dense(
        units = 100, 
        activation = "linear", use_bias=T
      ) %>%
      keras::layer_batch_normalization() %>%
      keras::layer_activation(activation = 'relu') %>%
      
      # =========== FINAL LAYER
      keras::layer_dropout(rate=dropout) %>%
      keras::layer_dense(name = 'final',
                         units = 2, 
                         activation = "linear", use_bias=T
      ) %>%
      keras::layer_batch_normalization() %>%
      keras::layer_activation(activation = 'sigmoid', name='main_output')
    
    
    
    # Prepare model for training
    model %>% keras::compile(
      loss = "binary_crossentropy",#loss, 
      metrics = c('accuracy'),
      optimizer = keras::optimizer_rmsprop(lr = lr,decay = decay)
    )
    earlyStopping=keras::callback_early_stopping(monitor = "val_loss", patience=10,mode="auto",min_delta = 1e-4)
    reduceLr=keras::callback_reduce_lr_on_plateau(monitor="val_loss", factor =0.1, 
                                                  patience = 5,mode = "auto", min_delta = 1e-5, cooldown = 0, min_lr = 0)
    
    
    class_weight=list("0"=1,"1"=outcomeWeight)
    
    data <- plpData[population$rowId,,]
    
    #Extract validation set first - 10k people or 5%
    valN <- min(10000,length(population$indexes)*0.05)
    val_rows<-sample(1:length(population$indexes), valN, replace=FALSE)
    train_rows <- c(1:length(population$indexes))[-val_rows]
    
    
    sampling_generator2<-function(data, population, batchSize, train_rows){
      function(){
        gc()
        rows<-sample(train_rows, batchSize, replace=FALSE)
        list(as.array(data[rows,,]), population$y[rows,])
      }
    }
    
    if(length(train_rows) < batchSize){
      # checking if this fixes issue with batchsize too big
      batchSize <- length(train_rows)
      ParallelLogger::logInfo('Reducing batchSize to training size')
    }
    
    
    history <- model %>% keras::fit_generator(sampling_generator2(data,population,batchSize,train_rows),
                                              steps_per_epoch = length(train_rows)/batchSize,
                                              epochs=epochs,
                                              validation_data=list(as.array(data[val_rows,,]),
                                                                   population$y[val_rows,]),
                                              callbacks=list(earlyStopping,reduceLr),
                                              class_weight=class_weight,
                                              view_metrics=F)
    
    # batched prediciton 
    maxVal <- nrow(population)
    batches <- lapply(1:ceiling(maxVal/batchSize), function(x) ((x-1)*batchSize+1):min((x*batchSize),maxVal))
    prediction <- population
    prediction$value <- 0
    for(batch in batches){
      pred <- keras::predict_on_batch(model, as.array(plpData[batch,,]))
      prediction$value[batch] <- as.double(pred[,2])
    }
    
    attr(prediction, "metaData") <- list(predictionType = "binary")
    auc <- computeAuc(prediction)
    foldPerm <- auc
    predictionMat <- prediction
  }
  
  result <- list(model=model,
                 auc=auc,
                 prediction = predictionMat,
                 hyperSum = unlist(list(batchSize = batchSize, lr=lr, decay=decay,
                                        outcomeWeight=outcomeWeight,
                                        dropout=dropout,filters=filters,
                                        kernelSize=paste0(kernelSize,collapse ='-'),
                                        epochs=epochs, loss=loss,
                                        fold_auc=foldPerm))
  )
  
  return(result)
  
}
