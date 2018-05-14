# @file CovNN.R
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

#' Create setting for CovNN model
#'
#' @param batch_size         The number of samples to used in each batch during model training
#' @param outcome_weight     The weight assined to the outcome (make greater than 1 to reduce unballanced label issue)
#' @param dropout            [currently not used] the dropout rate for regularisation
#' @param epochs            The number of times data is used to train the model (e.g., epoches=1 means data only used once to train)
#' @param filters            The number of columns output by each convolution
#' @param kernel_size        The number of time dimensions used for each convolution
#' @param loss               The loss function implemented 
#' @param seed               The random seed
#'
#' @examples
#' \dontrun{
#' model.CovNN <- setCovNN()
#' }
#' @export
setCovNN <- function(batch_size = 1000,
                     outcome_weight=2,
                     dropout=0,
                     epochs = 10,
                     filters = 3, kernel_size = 10,
                     loss = "binary_crossentropy", 
                     seed=NULL  ){
  #[TODO: add input checks...]
  
  result <- list(model='fitCovNN', param=split(expand.grid(
    batch_size=batch_size, 
    outcome_weight=outcome_weight,
    dropout=dropout,
    epochs= epochs,filters=filters,
    kernel_size=kernel_size,loss =loss,
    seed=ifelse(is.null(seed),'NULL', seed)),
    1:(length(batch_size)*length(outcome_weight)*length(epochs)*
         length(filters)*
         length(kernel_size)*length(loss)*max(1,length(seed)))),
    name='CovNN'
  )
  
  class(result) <- 'modelSettings' 
  return(result)
}


fitCovNN <- function(plpData,population, param, search='grid', quiet=F,
                      outcomeId, cohortId, ...){
  # check plpData is coo format:
  if(!'ffdf'%in%class(plpData$covariates) )
    stop('CovNN requires plpData in coo format')
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
  population$y <- population$outcomeCount#keras::to_categorical(population$outcomeCount, length(unique(population$outcomeCount)))
  #colnames(population$y) <- c('0','1')
  
  # do cross validation to find hyperParameter
  datas <- list(population=population, plpData=data)
  hyperParamSel <- lapply(param, function(x) do.call(trainCovNN, c(x,datas,train=TRUE)  ))
  
  hyperSummary <- cbind(do.call(rbind, lapply(hyperParamSel, function(x) x$hyperSum)))
  hyperSummary <- as.data.frame(hyperSummary)
  hyperSummary$auc <- unlist(lapply(hyperParamSel, function (x) x$auc))
  hyperParamSel<-unlist(lapply(hyperParamSel, function(x) x$auc))
  
  #now train the final model and return coef
  bestInd <- which.max(abs(unlist(hyperParamSel)-0.5))[1]
  finalModel<-do.call(trainCovNN, c(param[[bestInd]],datas, train=FALSE))$model
  
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
                 modelSettings = list(model='fitCovNN',modelParameters=param.best),
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

trainCovNN<-function(plpData, population,
                     outcome_weight=1,
                     dropout=0, filters=3,
                     kernel_size = dim(plpData)[3],
                     batch_size, epochs, loss= "binary_crossentropy",
                     seed=NULL, train=TRUE){
  
  writeLines(paste('Training covolutional neural network with ',length(unique(population$indexes)),' fold CV'))
  if(!is.null(population$indexes) && train==T){
    index_vect <- unique(population$indexes)
    perform <- c()
    
    # create prediction matrix to store all predictions
    predictionMat <- population
    predictionMat$value <- 0
    attr(predictionMat, "metaData") <- list(predictionType = "binary")
    
    for(index in 1:length(index_vect )){
      writeLines(paste('Fold ',index, ' -- with ', sum(population$indexes!=index),'train rows'))
      
      #Initialize model
      model <- keras::keras_model_sequential()
      
      model %>%
        # Begin with 1D convolutional layer
        keras::layer_conv_1d(
          input_shape = c(dim(plpData)[2], dim(plpData)[3]), 
          filters = filters, 
          kernel_size = kernel_size,
          padding = "valid"
        ) %>%
        # Normalize the activations of the previous layer
        keras::layer_batch_normalization() %>%
        
        keras::layer_flatten() %>%
        
        # Add final dense output layer 
        keras::layer_dense(
          units = 1, 
          activation = "sigmoid", use_bias=T
        )
      
      # Prepare model for training
      model %>% keras::compile(
        loss = "binary_crossentropy",#loss, 
        optimizer = "adadelta"
      )
      class_weight=list("0"=1,"1"=outcome_weight)
      #model %>% keras::fit(plpData[population$indexes!=index,,],
      #                     population$y[population$indexes!=index], 
      #                     epochs=epochs,
      #                     batch_size =batch_size,
      #                     class_weight=class_weight)
      
      ##batches <- 
      
      maxVal <- sum(population$indexes!=index)
      batches <- lapply(1:ceiling(maxVal/batch_size), function(x) ((x-1)*batch_size+1):min((x*batch_size),maxVal))
        
      for(e in 1:epochs){
        for(batch in batches){
          model %>%keras::train_on_batch(x=as.array(plpData[population$rowId[population$indexes!=index],,][batch,,]),
                                         y=population$y[population$indexes!=index][batch]
                                         ,class_weight=class_weight
                                         )
          #pred <- keras::predict_on_batch(model, as.array(plpData[1,,]))
          #writeLines(paste0('Test pred for person 1: ',pred))
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
    param.val <- paste0('outcome_weight: ', outcome_weight,
                        'kernel_size: ',paste0(kernel_size,collapse ='-'),
                        '-- filters: ', filters, '--loss: ', loss,
                        '-- dropout: ', dropout, '-- batch_size: ',batch_size, '-- epochs: ', epochs)
    writeLines('==========================================')
    writeLines(paste0('CovNN with parameters:', param.val,' obtained an AUC of ',auc))
    writeLines('==========================================')
  } else {
    #Initialize model
    model <- keras::keras_model_sequential()
    
    model %>%
      # Begin with 1D convolutional layer
      keras::layer_conv_1d(
        input_shape = c(dim(plpData)[2], dim(plpData)[3]), 
        filters = filters, 
        kernel_size = kernel_size,
        padding = "valid"
      ) %>%
      # Normalize the activations of the previous layer
      keras::layer_batch_normalization() %>%
      
      keras::layer_flatten() %>%
      
      # Add final dense output layer 
      keras::layer_dense(
        units = 1, 
        activation = "sigmoid", use_bias=T
      )
    
    # Prepare model for training
    model %>% keras::compile(
      loss = "binary_crossentropy",#loss, 
      optimizer = "adadelta"
    )
    
    class_weight=list("0"=1,"1"=outcome_weight)
    #model %>% keras::fit(plpData,
    #                     population$y, 
    #                     epochs=epochs,
    #                     batch_size =batch_size,
    #                     class_weight=class_weight)
    
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
                 hyperSum = unlist(list(batch_size = batch_size,
                                        outcome_weight=outcome_weight,
                                        dropout=dropout,filters=filters,
                                        kernel_size=paste0(kernel_size,collapse ='-'),
                                        epochs=epochs, loss=loss,
                                        fold_auc=foldPerm))
                 )
  
  return(result)
  
}