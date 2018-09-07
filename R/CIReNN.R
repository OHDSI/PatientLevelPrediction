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
#' @param recurrentDropout  The reccurrent dropout rate (regularisation)
#' @param layerDropout      The layer dropout rate (regularisation)
#' @param lr                 Learning rate
#' @param decay              Learning rate decay over each update.
#' @param outcomeWeight      The weight of the outcome class in the loss function
#' @param batchSize          The number of data points to use per training batch
#' @param epochs          Number of times to iterate over dataset
#' @param earlyStoppingMinDelta         minimum change in the monitored quantity to qualify as an improvement for early stopping, i.e. an absolute change of less than min_delta in loss of validation data, will count as no improvement.
#' @param earlyStoppingPatience         Number of epochs with no improvement after which training will be stopped.
#' @param useVae                        logical (either TRUE or FALSE) value for using Variational AutoEncoder before RNN
#' @param vaeDataSamplingProportion     Data sampling proportion for VAE
#' @param vaeValidationSplit            Validation split proportion for VAE
#' @param vaeBatchSize                  batch size for VAE
#' @param vaeLatentDim                  Number of latent dimesion for VAE
#' @param vaeIntermediateDim            Number of intermediate dimesion for VAE
#' @param vaeEpoch                      Number of times to interate over dataset for VAE
#' @param vaeEpislonStd                 Epsilon
#' @param seed            Random seed used by deep learning model
#'
#' @examples
#' \dontrun{
#' model.CIReNN <- setCIReNN()
#' }
#' @export
setCIReNN <- function(units=c(128, 64), recurrentDropout=c(0.2), layerDropout=c(0.2),
                      lr =c(1e-4), decay=c(1e-5), outcomeWeight = c(1.0), batchSize = c(100), 
                      epochs= c(100), earlyStoppingMinDelta = c(1e-4), earlyStoppingPatience = c(10), 
                      useVae = T, vaeDataSamplingProportion = 0.1,vaeValidationSplit= 0.2, 
                      vaeBatchSize = 100L, vaeLatentDim = 10L, vaeIntermediateDim = 256L, 
                      vaeEpoch = 100L, vaeEpislonStd = 1.0,
                      seed=NULL  ){
  
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
    units=units, recurrentDropout=recurrentDropout, 
    layerDropout=layerDropout,
    lr =lr, decay=decay, outcomeWeight=outcomeWeight,epochs= epochs,
    earlyStoppingMinDelta = earlyStoppingMinDelta, earlyStoppingPatience = earlyStoppingPatience,
    useVae= useVae,vaeDataSamplingProportion = vaeDataSamplingProportion, vaeValidationSplit= vaeValidationSplit, 
    vaeBatchSize = vaeBatchSize, vaeLatentDim = vaeLatentDim, vaeIntermediateDim = vaeIntermediateDim, 
    vaeEpoch = vaeEpoch, vaeEpislonStd = vaeEpislonStd,
    seed=ifelse(is.null(seed),'NULL', seed)),
    
    1:(length(units)*length(recurrentDropout)*length(layerDropout)*length(lr)*length(decay)*length(outcomeWeight)*length(earlyStoppingMinDelta)*length(earlyStoppingPatience)*length(epochs)*max(1,length(seed)))),
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
  covariateMap <- result$map

  #remove result to save memory
  rm(result)
  
  #function for building vae
  buildVae<-function(data, vaeValidationSplit= 0.2, vaeBatchSize = 100L, vaeLatentDim = 10L, vaeIntermediateDim = 256L,
                     vaeEpoch = 100L, vaeEpislonStd = 1.0, temporal = TRUE){
    if (temporal) dataSample <- data %>% apply(3, as.numeric) else dataSample <- data
    originalDim<-dim(dataSample)[2]
    K <- keras::backend()
    x <- keras::layer_input (shape =originalDim)
    h <- keras::layer_dense (x, vaeIntermediateDim, activation = 'relu')
    z_mean <- keras::layer_dense(h, vaeLatentDim)
    z_log_var <- keras::layer_dense(h, vaeLatentDim)
    
    sampling<- function(arg){
      z_mean <- arg[,1:vaeLatentDim]
      z_log_var <- arg[, (vaeLatentDim+1):(2*vaeLatentDim)]
      
      epsilon <- keras::k_random_normal(
        shape = c(keras::k_shape(z_mean)[[1]]),
        mean = 0.,
        stddev = vaeEpislonStd
      )
      
      z_mean + keras::k_exp(z_log_var/2)*epsilon
    }
    
    z <- keras::layer_concatenate(list(z_mean, z_log_var)) %>% 
      keras::layer_lambda(sampling)
    
    #we instantiate these layers separately so as to reuse them later
    decoder_h <- keras::layer_dense(units = vaeIntermediateDim, activation = 'relu')
    decoder_mean <- keras::layer_dense (units = originalDim, activation = 'sigmoid')
    h_decoded <- decoder_h (z)
    x_decoded_mean <- decoder_mean(h_decoded)
    
    #end-to-end autoencoder
    vae <- keras::keras_model (x,x_decoded_mean)
    #encoder, from inputs to latent space
    encoder <- keras::keras_model(x, z_mean)
    
    #generator, from latent space to reconstruted inputs
    decoder_input <- keras::layer_input (shape = vaeLatentDim)
    h_decoded_2 <- decoder_h(decoder_input)
    x_decoded_mean_2 <- decoder_mean(h_decoded_2)
    generator <- keras::keras_model (decoder_input, x_decoded_mean_2)
    
    vae_loss <- function(x, x_decoded_mean){
      xent_loss <- (originalDim/1.0)* keras::loss_binary_crossentropy(x, x_decoded_mean)
      k1_loss <- -0.5 * keras::k_mean(1 + z_log_var - keras::k_square(z_mean) - keras::k_exp(z_log_var), axis = -1L)
      xent_loss + k1_loss
    }
    
    vae %>% keras::compile (optimizer = "rmsprop", loss = vae_loss)
    #if (!is.null(dataValidation)) dataValidation<-list(dataValidation,dataValidation)
    vaeEarlyStopping=keras::callback_early_stopping(monitor = "val_loss", patience=5,mode="auto",min_delta = 1e-3)
    
    vae %>% keras::fit (
      dataSample,dataSample
      ,shuffle = TRUE
      ,epochs = vaeEpoch
      ,batch_size = vaeBatchSize
      #,validation_data = dataValidation
      ,validation_split = vaeValidationSplit
      ,callbacks = list(vaeEarlyStopping)
    )
    return (list (vae,encoder))
  }
  
  if(param[[1]]$useVae){
    #Sampling the data for bulding VAE
    vaeSampleData<-data[sample(seq(dim(data)[1]), floor(dim(data)[1]*param[[1]]$vaeDataSamplingProportion),replace=FALSE),,]
    
    #Build VAE
    vae<-buildVae(vaeSampleData, vaeValidationSplit= param[[1]]$vaeValidationSplit, 
                  vaeBatchSize = param[[1]]$vaeBatchSize, vaeLatentDim = param[[1]]$vaeLatentDim, vaeIntermediateDim = param[[1]]$vaeIntermediateDim,
                  vaeEpoch = param[[1]]$vaeEpoch, vaeEpislonStd = param[[1]]$vaeEpislonStd, temporal = TRUE)
    #remove sample data for VAE to save memory
    rm(vaeSampleData)
    
    vaeEnDecoder<- vae[[1]]
    vaeEncoder  <- vae[[2]]
    
    #Embedding by using VAE encoder
    data<- plyr::aaply(as.array(data), 2, function(x) predict(vaeEncoder, x, batch_size = param$vaeBatchSize))
    data<-aperm(data, perm = c(2,1,3))#rearrange of dimension
    
    ##Check the performance of vae
    # decodedVaeData<-plyr::aaply(as.array(data), 2, function(x) predict(vaeEnDecoder, x, batch_size = param$vaeBatchSzie))
    # decodedVaeData<-aperm(decodedVaeData, c(2,1,3))
    # a1=Epi::ROC(form=as.factor(as.vector(data))~as.vector(decodedVaeData),plot="ROC")
    
  }else {
    vaeEnDecoder <- NULL
    vaeEncoder <- NULL
  }
  
  #one-hot encoding
  population$y <- keras::to_categorical(population$outcomeCount, 2)#[,2] #population$outcomeCount

  # do cross validation to find hyperParameter
  datas <- list(population=population, plpData=data)
  
  #remove data to save memory
  rm(data)  
  
  #Selection of hyperparameters
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
                 covariateMap=covariateMap,
                 useVae = param[[1]]$useVae,
                 vaeBatchSize = param[[1]]$vaeBatchSize,
                 vaeEnDecoder = vaeEnDecoder,
                 vaeEncoder = vaeEncoder
  )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'deep'
  attr(result, 'predictionType') <- 'binary'
  
  return(result)
}

trainCIReNN<-function(plpData, population,
                      units=128, recurrentDropout=0.2, layerDropout=0.2,
                      lr =1e-4, decay=1e-5, outcomeWeight = 1.0, batchSize = 100, 
                      epochs= 100, earlyStoppingMinDelta = c(1e-4), earlyStoppingPatience = c(10), 
                      useVae = T, vaeDataSamplingProportion = 0.1,vaeValidationSplit= 0.2, 
                      vaeBatchSize = 100L, vaeLatentDim = 10L, vaeIntermediateDim = 256L, 
                      vaeEpoch = 100L, vaeEpislonStd = 1.0,
                      seed=NULL, train=TRUE){
  
  if(!is.null(population$indexes) && train==T){
    writeLines(paste('Training recurrent neural network with ',length(unique(population$indexes)),' fold CV'))
    
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
        keras::layer_gru(units=units, recurrent_dropout = recurrentDropout,
                         input_shape = c(dim(plpData)[2],dim(plpData)[3]), #time step x number of features
                         return_sequences=FALSE#,stateful=TRUE
        ) %>%
        keras::layer_dropout(layerDropout) %>%
        keras::layer_dense(units=2, activation='softmax')
      
      model %>% keras::compile(
        loss = 'binary_crossentropy',
        metrics = c('accuracy'),
        optimizer = keras::optimizer_rmsprop(lr = lr,decay = decay)
      )
      
      earlyStopping=keras::callback_early_stopping(monitor = "val_loss", patience=earlyStoppingPatience,
                                                   mode="auto",min_delta = earlyStoppingMinDelta)
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
          
          list(as.array(data[rows,,]), population$y[population$indexes!=index,][rows,])
        }
      }
      
 
      #print(table(population$y))
      
        history <- model %>% keras::fit_generator(sampling_generator(data,population,batchSize,train_rows, index),
                                       steps_per_epoch = sum(population$indexes!=index)/batchSize,
                                       epochs=epochs,
                                       validation_data=list(as.array(data[val_rows,,]), 
                                                            population$y[population$indexes!=index,][val_rows,]),
                                       callbacks=list(earlyStopping,reduceLr),
                                       class_weight=class_weight)
      
      # batch prediciton 
      maxVal <- sum(population$indexes==index)
      batches <- lapply(1:ceiling(maxVal/batchSize), function(x) ((x-1)*batchSize+1):min((x*batchSize),maxVal))
      prediction <- population[population$indexes==index,]
      prediction$value <- 0
      for(batch in batches){
        pred <- keras::predict_proba(model, as.array(plpData[population$rowId[population$indexes==index],,][batch,,]))
        prediction$value[batch] <- pred[,2]
        #writeLines(paste0(dim(pred[,2]), collapse='-'))
        #writeLines(paste0(pred[1,2], collapse='-'))
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
    param.val <- paste0('units: ',units,'-- recurrentDropout: ', recurrentDropout,
                        'layerDropout: ',layerDropout,'-- lr: ', lr,
                        '-- decay: ', decay, '-- batchSize: ',batchSize, '-- epochs: ', epochs)
    writeLines('==========================================')
    writeLines(paste0('CIReNN with parameters:', param.val,' obtained an AUC of ',auc))
    writeLines('==========================================')
    
      } else {
        ##single-layer gru
        model <- keras::keras_model_sequential()
        model %>%
          keras::layer_gru(units=units, recurrent_dropout = recurrentDropout,
                           input_shape = c(dim(plpData)[2],dim(plpData)[3]), #time step x number of features
                           return_sequences=FALSE#,stateful=TRUE
          ) %>%
          keras::layer_dropout(layerDropout) %>%
          keras::layer_dense(units=2, activation='softmax')
        
        model %>% keras::compile(
          loss = 'binary_crossentropy',
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

    
    sampling_generator<-function(data, population, batchSize, train_rows){
      function(){
        gc()
        rows<-sample(train_rows, batchSize, replace=FALSE)
        list(as.array(data[rows,,]), population$y[rows,])
      }
    }
      
      
      history <- model %>% keras::fit_generator(sampling_generator(data,population,batchSize,train_rows),
                                     steps_per_epoch = nrow(population[-val_rows,])/batchSize,
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
      prediction$value[batch] <- pred[,2]
    }
    
    attr(prediction, "metaData") <- list(predictionType = "binary")
    auc <- computeAuc(prediction)
    foldPerm <- auc
  }
  
  
  result <- list(model=model,
                 auc=auc,
                 hyperSum = unlist(list(units=units, recurrentDropout=recurrentDropout, 
                                        layerDropout=layerDropout,lr =lr, decay=decay,
                                        batchSize = batchSize, epochs= epochs, earlyStoppingMinDelta = earlyStoppingMinDelta, 
                                        earlyStoppingPatience=earlyStoppingPatience))
  )
  return(result)
  
}