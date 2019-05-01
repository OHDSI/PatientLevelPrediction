# @file SageMakerBinary.R
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

#' Create setting for sagemaker model
#'
#' @param classifier     The name of the sagemaker binary classifier to use (pick from: knn, xgboost or linear-learner)
#' @param bucket         The s3 bucker string to save data for model training
#' @param prefix         The s3 subdirectory for the data
#' @param roleArn        The amazon roleArn
#' @param otherparams    Other parameters for training (currently not working)
#' @param seed           The seed for the training
#'
#' @examples
#' \dontrun{
#' model.sm <- setSagemakerBinary(classifier='gxboost',  bucket='ohdsi3')
#' }                           
#' @export
setSagemakerBinary<- function(classifier='xgboost', 
                              bucket,
                              prefix='data',
                              roleArn,
                              otherparams=NULL, 
                              seed=NULL){
  ensure_installed("aws.s3")
  # check inputs here
  if(!classifier%in%c('xgboost', 'linear-learner','knn')){
    stop('Unsupported classifier...')
  }
  if(missing(bucket)){
    stop('Must enter S3 bucket')
  }
  if(missing(roleArn)){
    sm <- reticulate::import('sagemaker')
    roleArn <- sm$get_execution_role()
  }
  
  # boost: num_class, num_round, alpha, base_score (like linear b), booster (gbtree, dart, gblinear), colsample_bylevel
  #        colsample_bytree, csv_weights = 0 (need second column as weights), early_stopping_rounds, eta = 0.3
  #        gamma (overfitting) =0, lambda  (ridge) = 1, max_depth = 6,  seed
  # linear: wd, l1, learning_rate, use_bias, positive_example_weight_mult
  # knn: k, index_metric, dimension_reduction_type, dimension_reduction_target 
  
  Sys.setenv(s3_bucket = bucket)
  Sys.setenv(aws_role_arn = roleArn)
  
  result <- list(model=paste0('fitSagemaker'), param= list(bucket=bucket, prefix=prefix, 
                                                           roleArn = roleArn,
                                                           classifier=classifier,
                                                           otherparams=otherparams,
                                                           seed=seed),
                 name=paste0('Amazon-Sagemaker-',classifier))
  class(result) <- 'modelSettings' 
  
  return(result)
}



fitSagemaker <- function(population, plpData, param, quiet=F,
                         outcomeId, cohortId, ...){
  
  # check logger
  if(length(ParallelLogger::getLoggers())==0){
    logger <- ParallelLogger::createLogger(name = "SIMPLE",
                                        threshold = "INFO",
                                        appenders = list(ParallelLogger::createConsoleAppender(layout = ParallelLogger::layoutTimestamp)))
    ParallelLogger::registerLogger(logger)
  }
  
  
  if(colnames(population)[ncol(population)]!='indexes'){
    ParallelLogger::logWarn(paste0('population columns: ', paste0(colnames(population), collapse='-')))
    warning('indexes column not present as last column - setting all index to 1')
    population$indexes <- rep(1, nrow(population))
  }
  
  metaData = plpData$metaData
  populationSettings = attr(population, 'metaData')
  varImp <- ff::as.ram(plpData$covariateRef)
  
  # create the python object to set variables
  pyt <- reticulate::py
  
  if(quiet==F){
    ParallelLogger::logTrace(paste0('Training sagemaker model...' ))
  }
  start <- Sys.time()
  
  # create the sparse matrix and save as a csv
  population$rowIdPython <- population$rowId - 1  # -1 to account for python/r index difference
  pPopulation <- as.matrix(population[, c("rowIdPython", "outcomeCount", "indexes")])
  
  x <- PatientLevelPrediction::toSparseM(plpData,population,map=NULL, temporal = F)
  plpData <- reticulate::r_to_py(x$data)
  
  ParallelLogger::logInfo(paste0('Setting parameters...' ))
  sm <- reticulate::import('sagemaker')
  session <- sm$Session()
  container <- sm$amazon$amazon_estimator$get_image_uri(session$boto_region_name, param$classifier)
  
  # modify this to use the temp location like efficient branch
  outLoc <- file.path(getwd(), "python_models")
  
  # clear the existing model pickles
  for (file in dir(outLoc)) file.remove(file.path(outLoc, file))
  
  ParallelLogger::logInfo(paste0('Running python code...' ))
  e <- environment()
  reticulate::source_python(system.file(package='PatientLevelPrediction','python','sageMakerFunctions.py'), envir = e)
  #reticulate::source_python('python_sagemaker_train.py', envir = e)
  
  result <- train_sagemaker(population=pPopulation, 
                            plpData=plpData, 
                            classifier = param$classifier,
                            hyperParameters = param$otherparams, 
                            container = container,
                            bucket = param$bucket,
                            s3_output = paste0('s3://', param$bucket, '/output'),
                            role_arn = param$roleArn,
                            prefix = param$prefix,
                            job_name =  paste('sagemaker-train',param$classifier, format(Sys.time(), '%H-%M-%S'), sep = '-'),
                            modelOutput = outLoc
                            #seed = seed, 
                            #quiet = quiet
  )
  
  buc <- aws.s3::get_bucket(param$bucket) 
  pred <- aws.s3::s3read_using(function(x) utils::read.csv(x, header=F), object = "prediction/test.csv.out", bucket = buc)
  prediction <- pPopulation[pPopulation[,3]>0,]
  if(length(grep('predicted', pred$V1[1]))>0){
    if(is.null(pred$V2)){
      pred$V2 <- as.double(gsub('\\}','',gsub('\\{predicted_label:','',as.character(pred$V1))))
    }
    prediction <- cbind(prediction,as.double(gsub('\\}','',gsub('score:','',as.character(pred$V2)))))
  }else{
    prediction <- cbind(prediction, pred$V1)
  }
  prediction <- as.data.frame(prediction)
  colnames(prediction) <- c("rowId", "outcomeCount", "indexes", "value")
  prediction$value[prediction$value < 0] <- 0
  prediction$value[prediction$value >1] <- 1
  # convert to R indexes
  prediction$rowId <- prediction$rowId + 1
  attr(prediction, "metaData") <- list(predictionType = "binary")
  
  auc <- PatientLevelPrediction::computeAuc(prediction)
  writeLines(paste0("Model obtained CV AUC of ", auc))
  
  prediction <- merge(population, prediction[,c('rowId', 'value')], by='rowId')
  
  comp <- start-Sys.time()
  
  # return model location
  result <- list(model = list(loc= file.path(outLoc), 
                              job_name=paste('sagemaker-train',param$classifier, format(Sys.time(), '%H-%M-%S'), sep = '-'), 
                              container=container),
                 trainCVAuc = NULL,
                 modelSettings = list(model=paste0('sagemaker-',param$classifier)),
                 hyperParamSearch = NULL,
                 metaData = metaData, 
                 populationSettings = populationSettings,
                 outcomeId=outcomeId,
                 cohortId=cohortId,
                 varImp = varImp,
                 trainingTime =comp,
                 dense=0,
                 covariateMap=x$map,
                 predictionTrain = prediction
  )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'sagemaker'
  attr(result, 'predictionType') <- 'binary'
  
  return(result)
}


predict.sagemaker <- function(plpModel, population, plpData){
  
  bucket <- Sys.getenv('s3_bucket') 
  #roleArn <- Sys.getenv('aws_role_arn')
  sm <- reticulate::import('sagemaker')
  roleArn <- paste(strsplit(sm$get_execution_role(),'\\/')[[1]][1],Sys.getenv('aws_role_arn'), sep='/') 
  
  prefix <- 'PLP_prediction'
  
  #delete any existing output...
  aws.s3::delete_object('pred.csv.out',bucket=paste0(bucket, '/',prefix) )
  #upload model
  aws.s3::put_object(file=file.path(plpModel$model$loc,'model.tar.gz'), bucket=paste0(bucket,'/plpModel')) #use model$job_name?
  
  ParallelLogger::logInfo('Mapping covariates...')
  #load python model mapping.txt
  # create missing/mapping using plpData$covariateRef
  newData <- PatientLevelPrediction::toSparseM(plpData, population, map=plpModel$covariateMap)
  plpData <- reticulate::r_to_py(newData$data)
  
  # save population
  if('indexes'%in%colnames(population)){
    population$rowIdPython <- population$rowId-1 # -1 to account for python/r index difference
    pPopulation <- as.matrix(population[,c('rowIdPython','outcomeCount','indexes')])
    popNames <- c('rowIdPython','outcomeCount','indexes')
    
  } else {
    population$rowIdPython <- population$rowId-1 # -1 to account for python/r index difference
    pPopulation <- as.matrix(population[,c('rowIdPython','outcomeCount')])
    popNames <- c('rowIdPython','outcomeCount')
  }
  
  # run the python predict code:
  ParallelLogger::logInfo('Executing prediction...')
  e <- environment()
  reticulate::source_python(system.file(package='PatientLevelPrediction','python','sageMakerPredict.py'), envir = e)
  #reticulate::source_python('sagemaker_predict.py', envir = e)
  
  pred <- sagemaker_predict(population = pPopulation, 
                            plpData = plpData, 
                            bucket = bucket , 
                            prefix =  prefix, 
                            container = plpModel$model$container, 
                            role_arn = roleArn, 
                            model_name =  paste('sagemaker-pred', format(Sys.time(), '%H-%M-%S'), sep = '-')
  )
  
  #get the prediction from python and reformat:
  ParallelLogger::logInfo('Returning results...')
  
  #pred <- PythonInR::pyGet("prediction", simplify = FALSE)
  buc <- aws.s3::get_bucket(bucket) 
  pred <- aws.s3::s3read_using(function(x) utils::read.csv(x, header=F), object = paste0(prefix,"/pred.csv.out"), bucket = buc)
  if(length(grep('predicted', pred$V1[1]))>0){
    if(is.null(pred$V2)){
      pred$V2 <- as.double(gsub('\\}','',gsub('\\{predicted_label:','',as.character(pred$V1))))
    }
    prediction <- cbind(pPopulation,as.double(gsub('\\}','',gsub('score:','',as.character(pred$V2)))))
  }else{
    prediction <- cbind(pPopulation, pred$V1)
  }
  prediction <- as.data.frame(prediction)
  colnames(prediction) <- c(popNames,'value')
  prediction$rowId <- prediction$rowIdPython + 1
  
  # fix negative or greater 1 values
  prediction$value[prediction$value < 0] <- 0
  prediction$value[prediction$value >1] <- 1
  #prediction <- prediction[,c('rowId','outcomeCount','value')]
  
  attr(prediction, "metaData") <- list(predictionType = "binary")
  prediction <- merge(population, prediction[,c('rowId', 'value')], by='rowId')
  
  
  return(prediction)
}
