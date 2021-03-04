# @file CIReNN.R
# Code edited from OHDSI contributor @chandryou CIReNN branch
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

# BuildCIReNN<-function(outcomes=ff::as.ffdf(population[,c('rowId','y')]),
#     covariates = result$data,
#     indexFolder=indexFolder){
# 
# }

#' Create setting for CIReNN model
#'
#' @param numberOfRNNLayer The number of RNN layer, only 1, 2, or 3 layers available now. eg. 1, c(1,2), c(1,2,3) 
#' @param units         The number of units of RNN layer - as a list of vectors
#' @param recurrentDropout  The reccurrent dropout rate (regularisation)
#' @param layerDropout      The layer dropout rate (regularisation)
#' @param lr                 Learning rate
#' @param decay              Learning rate decay over each update.
#' @param outcomeWeight      The weight of the outcome class in the loss function. Default is 0, which will be replaced by balanced weight. 
#' @param batchSize          The number of data points to use per training batch
#' @param epochs          Number of times to iterate over dataset
#' @param earlyStoppingMinDelta         minimum change in the monitored quantity to qualify as an improvement for early stopping, i.e. an absolute change of less than min_delta in loss of validation data, will count as no improvement.
#' @param earlyStoppingPatience         Number of epochs with no improvement after which training will be stopped.
#' @param useDeepEnsemble               logical (either TRUE or FALSE) value for using Deep Ensemble
#' @param numberOfEnsembleNetwork       Integer, Number of Ensemble. If you want to use Deep Ensemble, this number should be greater than 1. 
#' @param bayes                         logical (either TRUE or FALSE) value for using Bayesian Drop Out Layer to measure uncertainty. If it is TRUE, both Epistemic and Aleatoric uncertainty will be measured through Bayesian Drop Out layer
#' @param useDeepEnsemble               logical (either TRUE or FALSE) value for using Deep Ensemble (Lakshminarayanan et al., 2017) to measure uncertainty. It cannot be used together with Bayesian deep learing. 
#' @param numberOfEnsembleNetwork       Integer. Number of network used for Deep Ensemble (Lakshminarayanan et al recommended 5).
#' @param useVae                        logical (either TRUE or FALSE) value for using Variational AutoEncoder before RNN
#' @param vaeDataSamplingProportion     Data sampling proportion for VAE
#' @param vaeValidationSplit            Validation split proportion for VAE
#' @param vaeBatchSize                  batch size for VAE
#' @param vaeLatentDim                  Number of latent dimesion for VAE
#' @param vaeIntermediateDim            Number of intermediate dimesion for VAE
#' @param vaeEpoch                      Number of times to interate over dataset for VAE
#' @param vaeEpislonStd                 Epsilon
#' @param useGPU                        logical (either TRUE or FALSE) value. If you have GPUs in your machine, and want to use multiple GPU for deep learning, set this value as TRUE
#' @param maxGPUs                       Integer, If you will use GPU, how many GPUs will be used for deep learning in VAE? GPU parallelisation for deep learning will be activated only when parallel vae is true. Integer >= 2 or list of integers, number of GPUs or list of GPU IDs on which to create model replicas.
#' @param seed            Random seed used by deep learning model
#' @importFrom zeallot %<-%
#' @examples
#' \dontrun{
#' model.CIReNN <- setCIReNN()
#' }
#' @export
setCIReNN <- function(numberOfRNNLayer=c(1),units=c(128, 64), recurrentDropout=c(0.2), layerDropout=c(0.2),
                      lr =c(1e-4), decay=c(1e-5), outcomeWeight = c(0), batchSize = c(100), 
                      epochs= c(100), earlyStoppingMinDelta = c(1e-4), earlyStoppingPatience = c(10), 
                      bayes = T, useDeepEnsemble = F, numberOfEnsembleNetwork = 5, 
                      useVae = T, vaeDataSamplingProportion = 0.1,vaeValidationSplit= 0.2, 
                      vaeBatchSize = 100L, vaeLatentDim = 10L, vaeIntermediateDim = 256L, 
                      vaeEpoch = 100L, vaeEpislonStd = 1.0, useGPU = FALSE, maxGPUs = 2,
                      seed=1234  ){
  
  ensure_installed("keras")
  ensure_installed("tensorflow")
  ensure_installed("plyr")
  
  if( sum(!( numberOfRNNLayer %in% c(1,2,3)))!=0 ) stop ('Only 1,2 or 3 is available now. ')
  if(!((all.equal(numberOfEnsembleNetwork, as.integer(numberOfEnsembleNetwork))) & (numberOfEnsembleNetwork>=1) )) {
    stop ('Number of ensemble network should be a natural number')  }
  
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
    numberOfRNNLayer=numberOfRNNLayer,units=units, recurrentDropout=recurrentDropout, 
    layerDropout=layerDropout,
    lr =lr, decay=decay, outcomeWeight=outcomeWeight, epochs= epochs,
    earlyStoppingMinDelta = earlyStoppingMinDelta, earlyStoppingPatience = earlyStoppingPatience,
    bayes= bayes, useDeepEnsemble = useDeepEnsemble,numberOfEnsembleNetwork = numberOfEnsembleNetwork,
    useVae= useVae,vaeDataSamplingProportion = vaeDataSamplingProportion, vaeValidationSplit= vaeValidationSplit, 
    vaeBatchSize = vaeBatchSize, vaeLatentDim = vaeLatentDim, vaeIntermediateDim = vaeIntermediateDim, 
    vaeEpoch = vaeEpoch, vaeEpislonStd = vaeEpislonStd, useGPU = useGPU, maxGPUs = maxGPUs,
    seed=ifelse(is.null(seed),'NULL', seed)),
    
    1:(length(numberOfRNNLayer)*length(units)*length(recurrentDropout)*length(layerDropout)*length(lr)*length(decay)*length(outcomeWeight)*length(earlyStoppingMinDelta)*length(earlyStoppingPatience)*length(epochs)*max(1,length(seed)))),
    name='CIReNN'
  )
  
  class(result) <- 'modelSettings' 
  return(result)
}


fitCIReNN <- function(plpData,population, param, search='grid', quiet=F,
                      outcomeId, cohortId, ...){
  # check plpData is coo format:
  if (!FeatureExtraction::isCovariateData(plpData$covariateData))
    stop("Needs correct covariateData")
  
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
  
  if(param[[1]]$useVae){
    #Sampling the data for bulding VAE
    vaeSampleData <- data[sample(seq(dim(data)[1]), floor(dim(data)[1]*param[[1]]$vaeDataSamplingProportion),replace=FALSE),,]
    
    #Build VAE
    vae <- buildVae(vaeSampleData, vaeValidationSplit= param[[1]]$vaeValidationSplit, 
                  vaeBatchSize = param[[1]]$vaeBatchSize, vaeLatentDim = param[[1]]$vaeLatentDim, vaeIntermediateDim = param[[1]]$vaeIntermediateDim,
                  vaeEpoch = param[[1]]$vaeEpoch, vaeEpislonStd = param[[1]]$vaeEpislonStd, useGPU= param[[1]]$useGPU, maxGPUs= param[[1]]$maxGPUs, temporal = TRUE)
    #remove sample data for VAE to save memory
    vaeSampleData <- NULL
    
    vaeEnDecoder<- vae[[1]]
    vaeEncoder  <- vae[[2]]
    
    #Embedding by using VAE encoder
    data <- plyr::aaply(as.array(data), 2, function(x) stats::predict(vaeEncoder, x, batch_size = param$vaeBatchSize))
    data <- aperm(data, perm = c(2,1,3))#rearrange of dimension
    
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
  data <- NULL
  
  #Selection of hyperparameters
  hyperParamSel <- lapply(param, function(x) do.call(trainCIReNN, c(x,datas,train=TRUE)  ))
  hyperSummary <- cbind(do.call(rbind, lapply(hyperParamSel, function(x) x$hyperSum)))
  hyperSummary <- as.data.frame(hyperSummary)
  hyperSummary$auc <- unlist(lapply(hyperParamSel, function (x) x$auc))
  hyperParamSel <- unlist(lapply(hyperParamSel, function(x) x$auc))
  
  #now train the final model and return coef
  bestInd <- which.max(abs(unlist(hyperParamSel)-0.5))[1]
  finalModel<-do.call(trainCIReNN, c(param[[bestInd]],datas, train=FALSE))
  
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
                 modelSettings = list(model='fitCIReNN',modelParameters=param.best),
                 metaData = plpData$metaData,
                 populationSettings = attr(population, 'metaData'),
                 outcomeId = outcomeId,
                 cohortId = cohortId,
                 varImp = covariateRef, 
                 trainingTime = comp,
                 covariateMap = covariateMap,
                 useDeepEnsemble = param.best$useDeepEnsemble,
                 numberOfEnsembleNetwork = param.best$numberOfEnsembleNetwork,
                 useVae = param.best$useVae,
                 vaeBatchSize = param.best$vaeBatchSize,
                 vaeEnDecoder = vaeEnDecoder,
                 vaeEncoder = vaeEncoder,
                 predictionTrain  = prediction
  )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'deep'
  if(param.best$useDeepEnsemble){
    attr(result, 'type') <- 'deepEnsemble'
    }
  if(param.best$bayes){
    attr(result, 'type') <- 'BayesianDeep'
    }
  attr(result, 'predictionType') <- 'binary'
  
  return(result)
}

trainCIReNN<-function(plpData, population,
                      numberOfRNNLayer=1,units=128, recurrentDropout=0.2, layerDropout=0.2,
                      lr =1e-4, decay=1e-5, outcomeWeight = 0, batchSize = 100, 
                      epochs= 100, earlyStoppingMinDelta = c(1e-4), earlyStoppingPatience = c(10), 
                      bayes = T, useDeepEnsemble = F,numberOfEnsembleNetwork =3,
                      useVae = T, vaeDataSamplingProportion = 0.1,vaeValidationSplit= 0.2, 
                      vaeBatchSize = 100L, vaeLatentDim = 10L, vaeIntermediateDim = 256L, 
                      vaeEpoch = 100L, vaeEpislonStd = 1.0, useGPU = FALSE, maxGPUs = 2,
                      seed=NULL, train=TRUE){
  
  mu <- function(){return(NULL)}
  sigma <- function(){return(NULL)}
  
  output_dim = 2 #output dimension for outcomes
  num_MC_samples = 100 #sample number for MC sampling in Bayesian Deep Learning Prediction
  if(outcomeWeight == 0){
    outcomeWeight = round(sum(population$outcomeCount==0)/sum(population$outcomeCount>=1),1) #if outcome weight = 0, then it means balanced weight
  }
  #heteroscedatic loss function
  heteroscedastic_loss = function(y_true, y_pred) {
    mean = y_pred[, 1:output_dim]
    log_var = y_pred[, (output_dim + 1):(output_dim * 2)]
    precision = keras::k_exp(-log_var)
    keras::k_sum(precision * (y_true - mean) ^ 2 + log_var, axis = 2)
  }
  
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
      
      if(useDeepEnsemble){
        predList<-list()
        for (i in seq(numberOfEnsembleNetwork)){
          #print(i)
          ParallelLogger::logInfo(paste(i,'th process is started'))
          pred <- createEnsembleNetwork(train = train, plpData=plpData,population=population,batchSize=batchSize,epochs = epochs,
                                      earlyStoppingMinDelta=earlyStoppingMinDelta, earlyStoppingPatience=earlyStoppingPatience,
                                      train_rows=train_rows,index=index,lr=lr,decay=decay,
                                      units=units,recurrentDropout=recurrentDropout,numberOfRNNLayer=numberOfRNNLayer,
                                      layerDropout=layerDropout, useGPU = useGPU, maxGPUs = maxGPUs)
          ParallelLogger::logInfo(paste(i,'th process is ended started'))
          predList <- append(predList,pred)
        }
        model <- predList
        
        # batch prediciton 
        maxVal <- sum(population$indexes==index)
        batches <- lapply(1:ceiling(maxVal/batchSize), function(x) ((x-1)*batchSize+1):min((x*batchSize),maxVal))
        prediction <- population[population$indexes==index,]
        prediction$value <- 0
        prediction$sigmas <- 0
        
        for(batch in batches){
          
          for (i in seq(numberOfEnsembleNetwork)){
            if(i==1){
              muMatrix <- data.frame()
              sigmaMatrix <-data.frame()
            }
            c(mu,sigma) %<-% predList[[i]](inputs=list(as.array(plpData[population$rowId[population$indexes==index],,][batch,,])))
            muMatrix <- rbind(muMatrix,t(as.data.frame(mu[,2])))
            sigmaMatrix <- rbind(sigmaMatrix,t(as.data.frame(sigma[,2])))
          }
          
          muMean <- apply(muMatrix,2,mean)
          muSq <- muMatrix^2
          sigmaSq <- sigmaMatrix^2
          sigmaMean <- apply(sigmaMatrix,2,mean)
          sigmaResult = apply(muSq+sigmaSq,2, mean)- muMean^2
          
          prediction$value[batch] <- c(muMean)
          #if prediction$value is negative, make this positive
          prediction$sigmas[batch] <- c(sigmaResult)
          
        }
        prediction$value[prediction$value>1] <- 1
        prediction$value[prediction$value<0] <- 0
        #prediction$value[batch] <- mu[,2]
        #prediction$sigmas[batch] <- sigma[,2]
        
        #writeLines(paste0(dim(pred[,2]), collapse='-'))
        #writeLines(paste0(pred[1,2], collapse='-'))
        
        attr(prediction, "metaData") <- list(predictionType = "binary")
        aucVal <- computeAuc(prediction)
        perform <- c(perform,aucVal)
        
        # add the fold predictions and compute AUC after loop
        predictionMat$value[population$indexes==index] <- prediction$value
        
      }else{
        layerInput <- keras::layer_input(shape = c(dim(plpData)[2],dim(plpData)[3]))
        if(useGPU){
          ##GRU layer
          if(numberOfRNNLayer==1){
            layerOutput <- layerInput %>% 
              keras::layer_cudnn_gru(units=units, #time step x number of features
                                     return_sequences=FALSE) %>% 
              keras::layer_dropout(layerDropout)
          } 
          if(numberOfRNNLayer==2){
            layerOutput <- layerInput %>% 
              keras::layer_cudnn_gru(units=units, #time step x number of features
                                     return_sequences=TRUE) %>% 
              keras::layer_dropout(layerDropout) %>%
              keras::layer_cudnn_gru(units=units, return_sequences=FALSE) %>% 
              keras::layer_dropout(layerDropout)
          }
          if(numberOfRNNLayer==3){
            layerOutput <- layerInput %>% 
              keras::layer_cudnn_gru(units=units, #time step x number of features
                                     return_sequences=TRUE) %>% 
              keras::layer_dropout(layerDropout) %>%
              keras::layer_cudnn_gru(units=units, return_sequences=TRUE) %>%
              keras::layer_dropout(layerDropout) %>%
              keras::layer_cudnn_gru(units=units, return_sequences=FALSE) %>% 
              keras::layer_dropout(layerDropout)
          }
        }else{
          ##GRU layer
          if(numberOfRNNLayer == 1){
            layerOutput <- layerInput %>% 
              keras::layer_gru(units=units, recurrent_dropout = recurrentDropout,
                               return_sequences=FALSE) 
          }
          if(numberOfRNNLayer > 1 ){
            layerInput %>% # !ISSUE : "missing layerOutput <- "?
              keras::layer_gru(units=units, recurrent_dropout = recurrentDropout,
                               return_sequences=TRUE) 
          }
          if(numberOfRNNLayer == 2){
            layerOutput <- layerInput %>% 
              keras::layer_gru(units=units, recurrent_dropout = recurrentDropout,
                               return_sequences=FALSE)
          }
          if(numberOfRNNLayer==3){
            layerOutput <- layerInput %>% 
              keras::layer_gru(units=units, recurrent_dropout = recurrentDropout,
                               return_sequences=TRUE) %>%
              # ISSUE- I removed layerOutput <- layerInput %>%  as this was after a pipe
              keras::layer_gru(units=units, recurrent_dropout = recurrentDropout,
                               return_sequences=FALSE) 
          }
        }
        
        earlyStopping = keras::callback_early_stopping(monitor = "val_loss", patience=earlyStoppingPatience,
                                                     mode="auto",min_delta = earlyStoppingMinDelta)
        reduceLr = keras::callback_reduce_lr_on_plateau(monitor="val_loss", factor =0.1, 
                                                      patience = 5,mode = "auto", 
                                                      min_delta = 1e-5, cooldown = 0, min_lr = 0)
        
        class_weight=list("0"=1,"1"=outcomeWeight)
        
        if(bayes){
          mean = layerOutput %>% 
            layer_concrete_dropout(layer = keras::layer_dense(units = output_dim))
          
          log_var = layerOutput %>% 
            layer_concrete_dropout(layer = keras::layer_dense(units = output_dim))
          
          output = keras::layer_concatenate(list(mean, log_var))
          model = keras::keras_model(layerInput, output)
          #model = keras::keras_model(keras::layer_input(shape = c(dim(plpData)[2],dim(plpData)[3])), output)
          model %>% keras::compile(
            optimizer = "adam",
            loss = heteroscedastic_loss,
            metrics = c(keras::custom_metric("heteroscedastic_loss", heteroscedastic_loss))
          )
          
        }else{
          model<- layerInput %>% 
            keras::layer_dense(units=2, activation='softmax')
          model %>% keras::compile(
            loss = 'binary_crossentropy',
            metrics = c('accuracy'),
            optimizer = keras::optimizer_rmsprop(lr = lr,decay = decay)
          )
        }
        
        data <- plpData[population$rowId[population$indexes!=index],,]
        
        #Extract validation set first - 10k people or 5%
        valN <- min(10000,sum(population$indexes!=index)*0.05)
        val_rows <- sample(1:sum(population$indexes!=index), valN, replace=FALSE)
        train_rows <- c(1:sum(population$indexes!=index))[-val_rows]
        
        sampling_generator <- function(data, population, batchSize, train_rows, index){
          function(){
            gc()
            rows<-sample(train_rows, batchSize, replace=FALSE)
            
            list(as.array(data[rows,,]), population$y[population$indexes!=index,][rows,])
          }
        }
        

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
        
        if(bayes){
          prediction$epistemicUncertainty <- 0
          prediction$aleatoricUncertainty <- 0
          for(batch in batches){
            MC_samples <- array(0, dim = c(num_MC_samples, length(batch), 2 * output_dim))
            for (k in 1:num_MC_samples){
              MC_samples[k,, ] = stats::predict(model, as.array(plpData[population$rowId[population$indexes==index],,][batch,,]))
                #keras::predict_proba(model, as.array(plpData[population$rowId[population$indexes==index],,][batch,,]))
            }
            pred <- apply(MC_samples[,,output_dim], 2, mean)
            epistemicUncertainty <- apply(MC_samples[,,output_dim], 2, stats::var)
            logVar = MC_samples[, , output_dim * 2]
            
            if(length(dim(logVar))<=1){
              aleatoricUncertainty = exp(mean(logVar))
            }else{
              aleatoricUncertainty = exp(colMeans(logVar))
            }
            
            prediction$value[batch] <- pred
            prediction$epistemicUncertainty[batch] = epistemicUncertainty
            prediction$aleatoricUncertainty[batch] = aleatoricUncertainty
            
          }
          
        }else{
          for(batch in batches){
            pred <- keras::predict_proba(model, as.array(plpData[population$rowId[population$indexes==index],,][batch,,]))
            prediction$value[batch] <- pred[,2]
            #writeLines(paste0(dim(pred[,2]), collapse='-'))
            #writeLines(paste0(pred[1,2], collapse='-'))
          }
        }
        prediction$value[prediction$value>1] <- 1
        prediction$value[prediction$value<0] <- 0
        attr(prediction, "metaData") <- list(predictionType = "binary")
        aucVal <- computeAuc(prediction)
        perform <- c(perform,aucVal)
        
        # add the fold predictions and compute AUC after loop
        predictionMat$value[population$indexes==index] <- prediction$value
        # add uncertainty
        predictionMat$aleatoricUncertainty[population$indexes==index] <- prediction$aleatoricUncertainty
        predictionMat$epistemicUncertainty[population$indexes==index] <- prediction$epistemicUncertainty
      }
      
    }
    
    auc <- computeAuc(predictionMat)
    foldPerm <- perform
    
    # Output  ----------------------------------------------------------------
    param.val <- paste0('RNNlayer Number: ', numberOfRNNLayer, '-- units: ',units,'-- recurrentDropout: ', recurrentDropout,
                        'layerDropout: ',layerDropout,'-- lr: ', lr,
                        '-- decay: ', decay,'-- outcomeWeight',outcomeWeight, '-- batchSize: ',batchSize, '-- epochs: ', epochs)
    writeLines('==========================================')
    writeLines(paste0('CIReNN with parameters:', param.val,' obtained an AUC of ',auc))
    writeLines('==========================================')
    
  } else {
    if(useDeepEnsemble){
      predList<-list()
      for (i in seq(numberOfEnsembleNetwork)){
       #print(i)
       pred <- createEnsembleNetwork(train = train, plpData=plpData,population=population,batchSize=batchSize,epochs = epochs,
                                   earlyStoppingMinDelta=earlyStoppingMinDelta, earlyStoppingPatience=earlyStoppingPatience,
                                   train_rows=train_rows,index=index,lr=lr,decay=decay,
                                   units=units,recurrentDropout=recurrentDropout,numberOfRNNLayer=numberOfRNNLayer,
                                   layerDropout=layerDropout, useGPU = useGPU, maxGPUs = maxGPUs)
       
       predList <- append(predList,pred)
      }
      model <- predList
      
      # batch prediciton 
      maxVal <- nrow(population)
      batches <- lapply(1:ceiling(maxVal/batchSize), function(x) ((x-1)*batchSize+1):min((x*batchSize),maxVal))
      prediction <- population
      prediction$value <- 0
      prediction$sigmas <- 0
      
      for(batch in batches){
        
        for (i in seq(numberOfEnsembleNetwork)){
          if(i == 1){
            muMatrix <- data.frame()
            sigmaMatrix <-data.frame()
          }
          c(mu,sigma) %<-% predList[[i]](inputs=list(as.array(plpData[batch,,])))
          muMatrix <- rbind(muMatrix,t(as.data.frame(mu[,2])))
          sigmaMatrix <- rbind(sigmaMatrix,t(as.data.frame(sigma[,2])))
        }
        
        muMean <- apply(muMatrix,2,mean)
        muSq <- muMatrix^2
        sigmaSq <- sigmaMatrix^2
        sigmaMean <- apply(sigmaMatrix,2,mean)
        sigmaResult = apply(muSq+sigmaSq,2, mean)- muMean^2
        
        prediction$value[batch] <- c(muMean)
        prediction$sigmas[batch] <- c(sigmaResult)
      }
      prediction$value[prediction$value>1] <- 1
      prediction$value[prediction$value<0] <- 0

      
    }else{
      layerInput <- keras::layer_input(shape = c(dim(plpData)[2],dim(plpData)[3]))
      if(useGPU){
        ##GRU layer
        if(numberOfRNNLayer==1){
          layerOutput <- layerInput %>% 
            keras::layer_cudnn_gru(units=units, #time step x number of features
                                   return_sequences=FALSE) %>% 
            keras::layer_dropout(layerDropout)
        } 
        if(numberOfRNNLayer==2){
          layerOutput <- layerInput %>% 
            keras::layer_cudnn_gru(units=units, #time step x number of features
                                   return_sequences=TRUE) %>% 
            keras::layer_dropout(layerDropout) %>%
            keras::layer_cudnn_gru(units=units, return_sequences=FALSE) %>% 
            keras::layer_dropout(layerDropout)
        }
        if(numberOfRNNLayer==3){
          layerOutput <- layerInput %>% 
            keras::layer_cudnn_gru(units=units, #time step x number of features
                                   return_sequences=TRUE) %>% 
            keras::layer_dropout(layerDropout) %>%
            keras::layer_cudnn_gru(units=units, return_sequences=TRUE) %>%
            keras::layer_dropout(layerDropout) %>%
            keras::layer_cudnn_gru(units=units, return_sequences=FALSE) %>% 
            keras::layer_dropout(layerDropout)
        }
      }else{
        ##GRU layer
        if(numberOfRNNLayer==1){
          layerOutput <- layerInput %>% 
            keras::layer_gru(units=units, recurrent_dropout = recurrentDropout,
                             return_sequences=FALSE) 
        }
        if(numberOfRNNLayer>1 ){
          layerInput %>%   # ISSUE - "layerInput <- " missing?
            keras::layer_gru(units=units, recurrent_dropout = recurrentDropout,
                             return_sequences=TRUE) 
        }
        if(numberOfRNNLayer==2){
          layerOutput <- layerInput %>% 
            keras::layer_gru(units=units, recurrent_dropout = recurrentDropout,
                             return_sequences=FALSE) 
        }
        if(numberOfRNNLayer==3){
          layerOutput <- layerInput %>% 
            keras::layer_gru(units=units, recurrent_dropout = recurrentDropout,
                             return_sequences=TRUE) %>%
            #layerOutput <- layerInput %>%  ISSUE - pipe above?
            keras::layer_gru(units=units, recurrent_dropout = recurrentDropout,
                             return_sequences=FALSE) 
        }
      }
      
      earlyStopping = keras::callback_early_stopping(monitor = "val_loss", patience=earlyStoppingPatience,
                                                   mode="auto",min_delta = earlyStoppingMinDelta)
      reduceLr = keras::callback_reduce_lr_on_plateau(monitor="val_loss", factor =0.1, 
                                                    patience = 5,mode = "auto", 
                                                    min_delta = 1e-5, cooldown = 0, min_lr = 0)
      
      class_weight = list("0" = 1, 
                          "1" = outcomeWeight)
      
      if(bayes){
        mean = layerOutput %>% 
          layer_concrete_dropout(layer = keras::layer_dense(units = output_dim))
        
        log_var = layerOutput %>% 
          layer_concrete_dropout(layer = keras::layer_dense(units = output_dim))
        
        output = keras::layer_concatenate(list(mean, log_var))
        model = keras::keras_model(layerInput, output)
        #model = keras::keras_model(keras::layer_input(shape = c(dim(plpData)[2],dim(plpData)[3])), output)
        model %>% keras::compile(
          optimizer = "adam",
          loss = heteroscedastic_loss,
          metrics = c(keras::custom_metric("heteroscedastic_loss", heteroscedastic_loss))
        )
        
      }else{
        model <- layerInput %>% 
          keras::layer_dense(units=2, activation='softmax')
        model %>% keras::compile(
          loss = 'binary_crossentropy',
          metrics = c('accuracy'),
          optimizer = keras::optimizer_rmsprop(lr = lr,decay = decay)
        )
      }
      
      data <- plpData[population$rowId,,]
      
      #Extract validation set first - 10k people or 5%
      valN <- min(10000,length(population$indexes)*0.05)
      val_rows <- sample(1:length(population$indexes), valN, replace=FALSE)
      train_rows <- c(1:length(population$indexes))[-val_rows]
      
      sampling_generator2 <- function(data, population, batchSize, train_rows){
        function(){
          gc()
          rows<-sample(train_rows, batchSize, replace=FALSE)
          list(as.array(data[rows,,]), population$y[rows,])
        }
      }
      
      
      history <- model %>% keras::fit_generator(sampling_generator2(data,population,batchSize,train_rows),
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
      
      if(bayes){
        prediction$epistemicUncertainty <- 0
        prediction$aleatoricUncertainty <- 0
        for(batch in batches){
          MC_samples <- array(0, dim = c(num_MC_samples, length(batch), 2 * output_dim))
          for (k in 1:num_MC_samples){
            MC_samples[k,, ] = stats::predict(model, as.array(plpData[batch,,]))
            #keras::predict_proba(model, as.array(plpData[population$rowId[population$indexes==index],,][batch,,]))
          }
          pred <- apply(MC_samples[,,output_dim], 2, mean)
          epistemicUncertainty <- apply(MC_samples[,,output_dim], 2, stats::var)
          logVar = MC_samples[, , output_dim * 2]
          if(length(dim(logVar)) <= 1){
            aleatoricUncertainty = exp(mean(logVar))
          }else{
            aleatoricUncertainty = exp(colMeans(logVar))
            
          }
          prediction$value[batch] <- pred
          prediction$epistemicUncertainty[batch] = epistemicUncertainty
          prediction$aleatoricUncertainty[batch] = aleatoricUncertainty
        }
        
      }else{
        for(batch in batches){
          pred <- keras::predict_on_batch(model, as.array(plpData[batch,,]))
          prediction$value[batch] <- pred[,2]
        }
        
      }
      prediction$value[prediction$value>1] <- 1
      prediction$value[prediction$value<0] <- 0
      
      attr(prediction, "metaData") <- list(predictionType = "binary")
      auc <- computeAuc(prediction)
      foldPerm <- auc
      predictionMat <- prediction
      
    }
  
  }
  result <- list(model=model,
                 auc=auc,
                 prediction = predictionMat,
                 hyperSum = unlist(list(numberOfRNNLayer=numberOfRNNLayer, 
                                        units=units, recurrentDropout=recurrentDropout, 
                                        layerDropout=layerDropout,lr =lr, decay=decay,outcomeWeight=outcomeWeight,
                                        batchSize = batchSize, epochs= epochs, earlyStoppingMinDelta = earlyStoppingMinDelta, 
                                        earlyStoppingPatience=earlyStoppingPatience,
                                        useDeepEnsemble = useDeepEnsemble,
                                        numberOfEnsembleNetwork =numberOfEnsembleNetwork,
                                        useVae = useVae, vaeDataSamplingProportion =vaeDataSamplingProportion ,vaeValidationSplit = vaeValidationSplit,
                                        vaeBatchSize =vaeBatchSize,
                                        vaeLatentDim = vaeLatentDim,
                                        vaeIntermediateDim = vaeIntermediateDim,
                                        vaeEpoch = vaeEpoch, vaeEpislonStd = vaeEpislonStd))
  )
  return(result)
  
}

#function for building vae
buildVae<-function(data, vaeValidationSplit= 0.2, vaeBatchSize = 100L, vaeLatentDim = 10L, vaeIntermediateDim = 256L,
                   vaeEpoch = 100L, vaeEpislonStd = 1.0, useGPU= FALSE, maxGPUs = NULL, temporal = TRUE){
  if (temporal){
    dataSample <- data %>% 
      apply(3, as.numeric)
    } else{
      dataSample <- data
    }
  originalDim <- dim(dataSample)[2]
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
  #Activating parallelisation of GPU in encoder
  if(useGPU & (maxGPUs>1) ){
    vae <- keras::multi_gpu_model(vae,gpus = maxGPUs)
  }
  
  vae %>% keras::compile (optimizer = "rmsprop", loss = vae_loss)
  #if (!is.null(dataValidation)) dataValidation<-list(dataValidation,dataValidation)
  vaeEarlyStopping=keras::callback_early_stopping(monitor = "val_loss", patience=5,mode="auto",min_delta = 1e-3)
  naanStopping = keras::callback_terminate_on_naan()
  csvLogging = keras::callback_csv_logger (filename="./vae.csv",separator = ",", append =TRUE )
  
  vae %>% keras::fit (
    dataSample,dataSample
    ,shuffle = TRUE
    ,epochs = vaeEpoch
    ,batch_size = vaeBatchSize
    #,validation_data = dataValidation
    ,validation_split = vaeValidationSplit
    ,callbacks = list(vaeEarlyStopping, 
                      csvLogging,
                      naanStopping)
  )
  return (list (vae,encoder))
}

#Defining Gaussian Layer for Deep Ensemble
GaussianLayer <- R6::R6Class("GaussianLayer",
                             inherit = keras::KerasLayer,
                             
                             public = list(
                               output_dim = NULL,
                               kernel_1 = NULL,
                               kernel_2 = NULL,
                               bias_1 = NULL,
                               bias_2 = NULL,
                               
                               initialize = function(output_dim){
                                 self$output_dim <- output_dim
                               },
                               build = function(input_shape){
                                 super$build(input_shape)
                                 
                                 self$kernel_1 = self$add_weight(name = 'kernel_1',
                                                                 shape = list(as.integer(input_shape[[2]]), self$output_dim), #list(30, self$output_dim),#shape = keras::shape(30, self$output_dim),
                                                                 initializer = keras::initializer_glorot_normal(),
                                                                 trainable = TRUE)
                                 self$kernel_2 = self$add_weight(name = 'kernel_2',
                                                                 shape = list(as.integer(input_shape[[2]]), self$output_dim),#list(30, self$output_dim),  #shape = keras::shape(30, self$output_dim),
                                                                 initializer = keras::initializer_glorot_normal(),
                                                                 trainable = TRUE)
                                 self$bias_1 = self$add_weight(name = 'bias_1',
                                                               shape = list(self$output_dim),  #shape = keras::shape(self$output_dim),
                                                               initializer = keras::initializer_glorot_normal(),
                                                               trainable = TRUE)
                                 self$bias_2 = self$add_weight(name = 'bias_2',
                                                               shape = list(self$output_dim), #shape = keras::shape(self$output_dim),
                                                               initializer = keras::initializer_glorot_normal(),
                                                               trainable = TRUE)
                               },
                               
                               call = function(x, mask = NULL){
                                 output_mu = keras::k_dot(x, self$kernel_1) + self$bias_1
                                 output_sig = keras::k_dot(x, self$kernel_2) + self$bias_2
                                 output_sig_pos = keras::k_log(1 + keras::k_exp(output_sig)) + 1e-06
                                 return (list(output_mu, output_sig_pos))
                               },
                               
                               
                               compute_output_shape = function(input_shape){
                                 return (list (
                                   list(input_shape[[1]], self$output_dim), 
                                   list(input_shape[[1]], self$output_dim) )
                                 )
                               } 
                             )
)

#define layer wrapper function for Deep Ensemble
layer_custom <- function(object, output_dim, name = NULL, trainable = TRUE) {
  keras::create_layer(GaussianLayer, object, list(
    output_dim = as.integer(output_dim),
    name = name,
    trainable = trainable
  ))
}

#Custom loss function for Deep Ensemble
custom_loss <- function(sigma){
  gaussian_loss <- function(y_true,y_pred){
    tensorflow::tf$reduce_mean(0.5*tensorflow::tf$log(sigma) + 0.5*tensorflow::tf$div(tensorflow::tf$square(y_true - y_pred), sigma)) + 1e-6
  }
  return(gaussian_loss)
}

#Create Deep Ensemble Network function
createEnsembleNetwork<-function(train, plpData,population,batchSize,epochs, earlyStoppingPatience, earlyStoppingMinDelta,
                                train_rows=NULL,index=NULL,lr,decay,
                                units,recurrentDropout,numberOfRNNLayer,layerDropout, useGPU = useGPU, maxGPUs = maxGPUs){
  
  mu <- function(){return(NULL)}
  sigma <- function(){return(NULL)}
  
  if(useGPU){
    ##GRU layer
    layerInput <- keras::layer_input(shape = c(dim(plpData)[2],dim(plpData)[3]))
    
    if(numberOfRNNLayer==1){
      layers <- layerInput %>% keras::layer_cudnn_gru(units=units, #time step x number of features
                                                return_sequences=FALSE) %>% 
        keras::layer_dropout(layerDropout) %>%
        keras::layer_dense(units=2, activation='softmax')
    } 
    if(numberOfRNNLayer==2){
      layers <- layerInput %>% keras::layer_cudnn_gru(units=units, #time step x number of features
                                                return_sequences=TRUE) %>% 
        keras::layer_dropout(layerDropout) %>%
        keras::layer_gru(units=units, return_sequences=FALSE) %>% 
        keras::layer_dropout(layerDropout) %>%
        keras::layer_dense(units=2, activation='softmax')
    }
    if(numberOfRNNLayer==3){
      layers <- layerInput %>% keras::layer_cudnn_gru(units=units, #time step x number of features
                                                return_sequences=TRUE) %>% 
        keras::layer_dropout(layerDropout) %>%
        keras::layer_cudnn_gru(units=units, return_sequences=TRUE) %>%
        keras::layer_dropout(layerDropout) %>%
        keras::layer_cudnn_gru(units=units, return_sequences=FALSE) %>% 
        keras::layer_dropout(layerDropout) %>%
        keras::layer_dense(units=2, activation='softmax')
    }
  }else{
    ##GRU layer
    layerInput <- keras::layer_input(shape = c(dim(plpData)[2],dim(plpData)[3]))
    
    if(numberOfRNNLayer==1){
      layers <- layerInput %>% keras::layer_gru(units=units, recurrent_dropout = recurrentDropout, #time step x number of features
                                                return_sequences=FALSE) %>% keras::layer_dropout(layerDropout) %>%
        keras::layer_dense(units=2, activation='softmax')
    } 
    if(numberOfRNNLayer==2){
      layers <- layerInput %>% keras::layer_gru(units=units, recurrent_dropout = recurrentDropout, #time step x number of features
                                                return_sequences=TRUE) %>% 
        keras::layer_gru(units=units, recurrent_dropout = recurrentDropout,return_sequences=FALSE)%>% keras::layer_dropout(layerDropout) %>%
        keras::layer_dense(units=2, activation='softmax')
    }
    if(numberOfRNNLayer==3){
      layers <- layerInput %>% keras::layer_gru(units=units, recurrent_dropout = recurrentDropout, #time step x number of features
                                                return_sequences=TRUE) %>% 
        keras::layer_gru(units=units, recurrent_dropout = recurrentDropout,return_sequences=TRUE) %>%
        keras::layer_gru(units=units, recurrent_dropout = recurrentDropout,return_sequences=FALSE) %>% keras::layer_dropout(layerDropout) %>%
        keras::layer_dense(units=2, activation='softmax')
    }
  }
  
  c(mu,sigma) %<-% layer_custom(layers, 2, name = 'main_output')
  
  model <- keras::keras_model(inputs = layerInput,outputs=mu)
  
  earlyStopping=keras::callback_early_stopping(monitor = "val_loss", patience=earlyStoppingPatience,
                                               mode="auto",min_delta = earlyStoppingMinDelta)
  #Currently using Parallel GPU makes errors in Deep Ensemble
  #if(useGPU & (maxGPUs>1) ) model <- keras::multi_gpu_model(model,gpus = maxGPUs) 
  
  model %>% keras::compile(
    loss = custom_loss(!!sigma),
    optimizer = keras::optimizer_rmsprop(lr = lr,decay = decay)
  )
  
  
  if(!is.null(population$indexes) && train==T){
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
    
    history <- model %>% keras::fit_generator(sampling_generator(data,population,batchSize,train_rows, index),
                                              steps_per_epoch = sum(population$indexes!=index)/batchSize,
                                              epochs=epochs,
                                              validation_data=list(as.array(data[val_rows,,]), 
                                                                   population$y[population$indexes!=index,][val_rows,])#,
                                              ,callbacks=list(earlyStopping)
                                              #callbacks=list(earlyStopping,reduceLr),
    )
    
  }else{
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
    
    
    history <- model %>% keras::fit_generator(sampling_generator2(data,population,batchSize,train_rows),
                                              steps_per_epoch = nrow(population[-val_rows,])/batchSize,
                                              epochs=epochs,
                                              validation_data=list(as.array(data[val_rows,,]),
                                                                   population$y[val_rows,]),
                                              callbacks=list(earlyStopping),
                                              view_metrics=F)

  }
  #ParallelLogger::logInfo('right before get_intermediate')
  layer_name = 'main_output'
  get_intermediate = keras::k_function(inputs=list(model$input),
                                       outputs=model$get_layer(layer_name)$output)
  #ParallelLogger::logInfo('right after get_intermediate')
  return(get_intermediate)
}

#Custom layer for Bayesian Drop Out Layer
ConcreteDropout <- R6::R6Class("ConcreteDropout",
                               
                               inherit = keras::KerasWrapper,
                               
                               public = list(
                                 weight_regularizer = NULL,
                                 dropout_regularizer = NULL,
                                 init_min = NULL,
                                 init_max = NULL,
                                 is_mc_dropout = NULL,
                                 supports_masking = TRUE,
                                 p_logit = NULL,
                                 p = NULL,
                                 
                                 initialize = function(weight_regularizer,
                                                       dropout_regularizer,
                                                       init_min,
                                                       init_max,
                                                       is_mc_dropout) {
                                   self$weight_regularizer <- weight_regularizer
                                   self$dropout_regularizer <- dropout_regularizer
                                   self$is_mc_dropout <- is_mc_dropout
                                   self$init_min <- keras::k_log(init_min) - keras::k_log(1 - init_min)
                                   self$init_max <- keras::k_log(init_max) - keras::k_log(1 - init_max)
                                 },
                                 
                                 build = function(input_shape) {
                                   super$build(input_shape)
                                   
                                   self$p_logit <- super$add_weight(
                                     name = "p_logit",
                                     shape = keras::shape(1),
                                     initializer = keras::initializer_random_uniform(self$init_min, self$init_max),
                                     trainable = TRUE
                                   )
                                   
                                   self$p <- keras::k_sigmoid(self$p_logit)
                                   
                                   input_dim <- input_shape[[2]]
                                   
                                   weight <- private$py_wrapper$layer$kernel
                                   
                                   kernel_regularizer <- self$weight_regularizer * 
                                     keras::k_sum(keras::k_square(weight)) / 
                                     (1 - self$p)
                                   
                                   dropout_regularizer <- self$p * keras::k_log(self$p)
                                   dropout_regularizer <- dropout_regularizer +  
                                     (1 - self$p) * keras::k_log(1 - self$p)
                                   dropout_regularizer <- dropout_regularizer * 
                                     self$dropout_regularizer * 
                                     keras::k_cast(input_dim, keras::k_floatx())
                                   
                                   regularizer <- keras::k_sum(kernel_regularizer + dropout_regularizer)
                                   super$add_loss(regularizer)
                                 },
                                 
                                 concrete_dropout = function(x) {
                                   eps <- keras::k_cast_to_floatx(keras::k_epsilon())
                                   temp <- 0.1
                                   
                                   unif_noise <- keras::k_random_uniform(shape = keras::k_shape(x))
                                   
                                   drop_prob <- keras::k_log(self$p + eps) - 
                                     keras::k_log(1 - self$p + eps) + 
                                     keras::k_log(unif_noise + eps) - 
                                     keras::k_log(1 - unif_noise + eps)
                                   drop_prob <- keras::k_sigmoid(drop_prob / temp)
                                   
                                   random_tensor <- 1 - drop_prob
                                   
                                   retain_prob <- 1 - self$p
                                   x <- x * random_tensor
                                   x <- x / retain_prob
                                   x
                                 },
                                 
                                 call = function(x, mask = NULL, training = NULL) {
                                   if (self$is_mc_dropout) {
                                     super$call(self$concrete_dropout(x))
                                   } else {
                                     k_in_train_phase(
                                       function()
                                         super$call(self$concrete_dropout(x)),
                                       super$call(x),
                                       training = training
                                     )
                                   }
                                 }
                               )
)
#define layer wrapper for Bayesian Drop-out layer
layer_concrete_dropout <- function(object, 
                                   layer,
                                   weight_regularizer = 1e-6,
                                   dropout_regularizer = 1e-5,
                                   init_min = 0.1,
                                   init_max = 0.1,
                                   is_mc_dropout = TRUE,
                                   name = NULL,
                                   trainable = TRUE) {
  keras::create_wrapper(ConcreteDropout, object, list(
    layer = layer,
    weight_regularizer = weight_regularizer,
    dropout_regularizer = dropout_regularizer,
    init_min = init_min,
    init_max = init_max,
    is_mc_dropout = is_mc_dropout,
    name = name,
    trainable = trainable
  ))
}
