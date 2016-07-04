# @file randomForest.R
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

#' Create setting for random forest model with python (very fast)
#'
#' @param mtries     The number of features to include in each tree (-1 defaults to square root of total features)
#' @param ntrees     The number of trees to build 
#' @param max_depth  Maximum number of interactions - a large value will lead to slow model training
#'
#' @examples
#' model.rf <- randomForest.set(mtries=c(-1,5,20),  ntrees=c(10,100), 
#'                            max_depth=c(5,20))
#' @export
randomForest.set<- function(mtries=-1,ntrees=c(10,500),max_depth=17, varImp=T){
  
  result <- list(model='randomForest.fit', param= expand.grid(ntrees=ntrees, mtries=mtries,
                                                       max_depth=max_depth, varImp=varImp))
  class(result) <- 'modelSettings' 
  attr(result, 'libSVM') <- T
  
  return(result)
}




randomForest.fit <- function(population, plpData, param, index, search='grid', quiet=F,
                      outcomeId, cohortId, ...){
  
  # check plpData is libsvm format:
  if('ffdf'%in%class(plpData$covariates) || class(plpData)!='plpData.libsvm')
    stop('Random forest requires plpData in libsvm format')
  if(!file.exists(file.path(plpData$covariates,'covariate.txt')))
    stop('Cannot find libsvm file')
  
  if(colnames(population)[ncol(population)]!='indexes'){
    warning('indexes column not present as last column - setting all index to 1')
    population$indexes <- rep(1, nrow(popualtion))
  }
  
  
  if(!quiet)
    writeLines(paste0('Training random forest model...' ))
  start <- Sys.time()
  
  # create vector of 1s and 0s indicating whether the plpData row is in the populaiton
  rowIds <- read.table(file.path(plpData$covariates,'rowId.txt'))[,1]
  rowData <- rep(0, length(rowIds))
  rowData[rowIds%in%population$rowId] <- 1
  write.table(rowData, file.path(plpData$covariates,'dataRows.txt'), col.names=F, row.names = F)
  
  # make sure populating is ordered?
  write.table(population[,c('rowId','outcomeCount','indexes')], file.path(plpData$covariates,'population.txt'), col.names=F, row.names = F)
  
  #do var imp
  if(param$varImp[1]==T){
    
    
    # if windows: 
    if(systemInfo['sysname']=="Windows"){
      system(paste(system.file(package='PatientLevelPrediction', 'executionables',
                               'win64','python','rf_var_imp.exe'), gsub('/','\\\\',plpData$covariates), 
                   gsub('/','\\\\',plpData$covariates )  ))
      
      # end if windows and add else:  
    } else {
      system(paste('python',  system.file(package='PatientLevelPrediction','python','rf_var_imp.py '),gsub('/','\\\\',plpData$covariates),gsub('/','\\\\',plpData$covariates)))
    }
    
    #load var imp and create mapping/missing
    varImp <- read.table(file.path(plpData$covariates, 'varImp.txt'))[,1]
    
    if(mean(varImp)==0)
      stop('No important variables - seems to be an issue with the data')
    
    inc <- which(varImp>mean(varImp), arr.ind=T)
    covariateRef <- ff::as.ram(plpData$covariateRef)[inc,]
    
    # save mapping, missing, indexes
  } else{
    covariateRef <- ff::as.ram(plpData$covariateRef)
    inc <- 1:ncol(covariateRef)
  }
  write.table(inc-1, file.path(plpData$covariates, 'included.txt'), row.names=F, col.names = F)
  
  # run rf_plp for each grid search:
  outLoc <- file.path(getwd(),'temp_models')
  all_auc <- c()
  for(i in 1:nrow(param)){
    
    if(systemInfo['sysname']=="Windows"){
      system(paste(system.file(package='PatientLevelPrediction', 'executionables',
                               'win64','python','rf_train.exe'), gsub('/','\\\\',plpData$covariates),
                   param$ntrees[i],param$max_depth[i], 
                   param$mtries[i], 0,0, gsub('/','\\\\',outLoc),i )  )
      
      # end if windows and add else:  
    } else {
      system(paste('python',  system.file(package='PatientLevelPrediction','python','rf_train.py '),
                   gsub('/','\\\\',plpData$covariates),
                   param$ntrees[i],param$max_depth[i], 
                   param$mtries[i], 0,0, gsub('/','\\\\',outLoc),i
      ))
    }
    
    
    pred <- read.csv(file.path(outLoc,i,'prediction.txt'), header=F)
    colnames(pred) <- c('rowId','outcomeCount','indexes', 'value')
    attr(pred, "metaData") <- list(predictionType="binary")
    auc <- PatientLevelPrediction::computeAuc(pred)
    all_auc <- c(all_auc, auc)
    writeLines(paste0('Model with settings: ntrees:',param$ntrees[i],' max_depth: ',param$max_depth[i], 
                      'mtry: ', param$mtry[i] , ' obtained AUC of ', auc))
  }
  
  # select best model and remove the others
  modelTrained <- file.path(outLoc, which.max(all_auc)) # location - delete others
  param.best <- param[which.max(all_auc),]
  varImp <- read.csv(file.path(outLoc, which.max(all_auc),
                               'varImp.txt'), header=F) 
  covariateRef <- cbind(covariateRef, varImp)
  write.table(covariateRef, file.path(outLoc, which.max(all_auc),'covs.txt'), row.names=F, col.names=T)
  
  # delete all other models and move model to models folder:
  #...
  
  comp <- start-Sys.time()
  
  # return model location
  result <- list(model = modelTrained,
                 trainCVAuc = all_auc[which.max(all_auc)],
                 modelSettings = list(model='randomForest_python',modelParameters=param.best),
                 metaData = plpData$metaData,
                 populationSettings = attr(population, 'metaData'),
                 outcomeId=outcomeId,
                 cohortId=cohortId,
                 varImp = covariateRef,
                 trainingTime =comp,
                 dense=0
  )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'python'
  attr(result, 'predictionType') <- 'binary'
  
  
  return(result)
}