# @file naiveBayes.R
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

#' Create setting for naive bayes model with python 
#'
#' @examples
#' model.nb <- NBclassifie_pythonr()
#' @export
naiveBayes.set <- function(){
  
  result <- list(model='naiveBayes.fit', param= '')
  class(result) <- 'modelSettings' 
  attr(result, 'libSVM') <- T
  
  return(result)
}

naiveBayes.fit <- function(population, plpData, param, index, search='grid', quiet=F,
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
    writeLines(paste0('Training naive bayes model...' ))
  start <- Sys.time()
  
  # create vector of 1s and 0s indicating whether the plpData row is in the populaiton
  rowIds <- read.table(file.path(plpData$covariates,'rowId.txt'))[,1]
  rowData <- rep(0, length(rowIds))
  rowData[rowIds%in%population$rowId] <- 1
  write.table(rowData, file.path(plpData$covariates,'dataRows.txt'), col.names=F, row.names = F)
  
  # make sure populating is ordered?
  write.table(population[,c('rowId','outcomeCount','indexes')], file.path(plpData$covariates,'population.txt'), col.names=F, row.names = F)
  
  # run model:
  outLoc <- file.path(getwd(),'temp_models')
  
  if(systemInfo['sysname']=="Windows"){
    system(paste(system.file(package='PatientLevelPrediction', 'executionables',
                             'win64','python','naive_bayes.exe'), gsub('/','\\\\',plpData$covariates),
                 gsub('/','\\\\',outLoc),1 )  )
    
    # end if windows and add else:  
  } else {
    system(paste('python', system.file(package='PatientLevelPrediction','python','naive_bayes.py '),
                 gsub('/','\\\\',plpData$covariates),
                 gsub('/','\\\\',outLoc),1 )  )
  }
  
  
  pred <- read.csv(file.path(outLoc,1,'prediction.txt'), header=F)
  colnames(pred) <- c('rowId','outcomeCount','indexes', 'value')
  attr(pred, "metaData") <- list(predictionType="binary")
  pred$value <- 1-pred$value
  auc <- PatientLevelPrediction::computeAuc(pred)
  writeLines(paste0('Model obtained CV AUC of ', auc))
  
  # get the univeriate selected features (nb requires dense so need feat sel)
  varImp <- read.csv(file.path(outLoc,1, 'varImp.txt'), header=F)[,1]
  varImp[is.na(varImp)] <- 0
  if(mean(varImp)==0)
    stop('No important variables - seems to be an issue with the data')
  
  top2000 <- varImp[order(-varImp)][2000]
  inc <- which(varImp>=top2000, arr.ind=T)
  covariateRef <- ff::as.ram(plpData$covariateRef)
  covariateRef$varImp <- varImp
  
  covariateRef <- covariateRef[inc,] # this messes up order
  write.table(covariateRef, file.path(outLoc, 1,'covs.txt'), row.names=F, col.names=T)
  
  
  # select best model and remove the others
  modelTrained <- file.path(outLoc, 1) 
  param.best <- ''
  
  comp <- start-Sys.time()
  
  # return model location
  result <- list(model = modelTrained,
                 trainCVAuc = auc,
                 modelSettings = list(model='naiveBayes_python',modelParameters=param.best),
                 metaData = plpData$metaData,
                 populationSettings = attr(population, 'metaData'),
                 outcomeId=outcomeId,
                 cohortId=cohortId,
                 varImp = covariateRef,
                 trainingTime =comp,
                 dense=1
  )
  class(result) <- 'plpModel'
  attr(result, 'type') <- 'python'
  attr(result, 'predictionType') <- 'binary'
  
  
  return(result)
}