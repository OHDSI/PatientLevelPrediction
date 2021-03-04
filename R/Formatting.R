# @file formatting.R
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

#' Convert the plpData in COO format into a sparse R matrix
#'
#' @description
#' Converts the standard plpData to a sparse matrix
#'
#' @details
#' This function converts the covariate file from ffdf in COO format into a sparse matrix from
#' the package Matrix
#' @param plpData                       An object of type \code{plpData} with covariate in coo format - the patient level prediction
#'                                      data extracted from the CDM.
#' @param population                    The population to include in the matrix
#' @param map                           A covariate map (telling us the column number for covariates)
#' @param temporal                      Whether you want to convert temporal data
#' @examples
#' #TODO
#'
#' @return
#' Returns a list, containing the data as a sparse matrix, the plpData covariateRef
#' and a data.frame named map that tells us what covariate corresponds to each column
#' This object is a list with the following components: \describe{
#' \item{data}{A sparse matrix with the rows corresponding to each person in the plpData and the columns corresponding to the covariates.}
#' \item{covariateRef}{The plpData covariateRef.}
#' \item{map}{A data.frame containing the data column ids and the corresponding covariateId from covariateRef.}
#' }
#'
#' @export
toSparseM <- function(plpData,population, map=NULL, temporal=F){
  # check logger
  if(length(ParallelLogger::getLoggers())==0){
    logger <- ParallelLogger::createLogger(name = "SIMPLE",
                                        threshold = "INFO",
                                        appenders = list(ParallelLogger::createConsoleAppender(layout = ParallelLogger::layoutTimestamp)))
    ParallelLogger::registerLogger(logger)
  }
  
  ParallelLogger::logInfo(paste0('starting toSparseM'))
  
  
  ParallelLogger::logDebug(paste0('covariates nrow: ', nrow(plpData$covariateData$covariates)))
  ParallelLogger::logDebug(paste0('covariateRef nrow: ', nrow(plpData$covariateData$covariateRef)))
  
   
  #assign newIds to covariateRef
  newcovariateData <- MapCovariates(plpData$covariateData,
                                 population, 
                                 mapping=map)
  
  ParallelLogger::logDebug(paste0('Max covariateId in covariates: ',as.data.frame(newcovariateData$covariates %>% dplyr::summarise(max = max(.data$covariateId, na.rm=T)))))
  ParallelLogger::logDebug(paste0('# covariates in covariateRef: ', nrow(newcovariateData$covariateRef)))
  ParallelLogger::logDebug(paste0('Max rowId in covariates: ', as.data.frame(newcovariateData$covariates %>% dplyr::summarise(max = max(.data$rowId, na.rm=T)))))

  maxY <- as.data.frame(newcovariateData$mapping %>% dplyr::summarise(max=max(.data$newCovariateId, na.rm = TRUE)))$max
  ParallelLogger::logDebug(paste0('Max newCovariateId in mapping: ',maxY))
  maxX <- max(population$rowId)
  ParallelLogger::logDebug(paste0('Max rowId in population: ',maxX))
  
  # chunk then add
  if(!temporal){
    ParallelLogger::logInfo(paste0('toSparseM non temporal used'))
  data <- Matrix::sparseMatrix(i=1,
                               j=1,
                               x=0,
                               dims=c(maxX,maxY))
  
  dataEnv <- environment()
  convertData1 <- function(batch,dataEnv) {
    data <- get("data", envir = dataEnv)
    data <- data + Matrix::sparseMatrix(i=as.data.frame(batch %>% dplyr::select(.data$rowId))$rowId,
                                         j=as.data.frame(batch %>% dplyr::select(.data$covariateId))$covariateId,
                                         x=as.data.frame(batch %>% dplyr::select(.data$covariateValue))$covariateValue,
                                         dims=c(maxX,maxY))
    assign("data", data, envir = dataEnv)
    return(NULL)
  }
  Andromeda::batchApply(newcovariateData$covariates, convertData1, batchSize = 100000, dataEnv = dataEnv)
  
  } else {
    ParallelLogger::logInfo(paste0('toSparseM temporal used'))
    
    ParallelLogger::logTrace(paste0('Min time:', min(plpData$timeRef$timeId)))
    ParallelLogger::logTrace(paste0('Max time:', max(plpData$timeRef$timeId)))
    
    # do we want to use for(i in sort(plpData$timeRef$timeId)){ ?
    for(i in min(plpData$timeRef$timeId):max(plpData$timeRef$timeId)){
      
      if(nrow(newcovariateData$covariates %>% dplyr::filter(.data$timeId==i))>0){
        ParallelLogger::logTrace(paste0('Found covariates for timeId ', i))
        
 
        # initiate the sparse matrix
        dataPlp <- Matrix::sparseMatrix(i=1,
                                     j=1,
                                     x=0,
                                     dims=c(maxX, maxY)) 
        dataEnv <- environment()
        ParallelLogger::logTrace(paste0('Initiated Mapping covariates for timeId ', i))
        
        
        # add function to batch creating matrix from Andromeda data
        convertData <- function(batch, dataEnv) {
          dataPlp <- get("dataPlp", envir = dataEnv)
          dataPlp <- dataPlp + Matrix::sparseMatrix(i=as.double(as.character(batch$rowId)),
                                               j=as.double(as.character(batch$covariateId)),
                                               x=batch$covariateValue,
                                               dims=c(maxX,maxY))
          
          assign("dataPlp", dataPlp, envir = dataEnv)
          return(NULL)
        }
        
        # add age for each time
        tempCovs <- addAgeTemp(timeId = i,newcovariateData, plpData$timeRef) # EDITED adding newCov
        if(!is.null(tempCovs)){
          Andromeda::batchApply(tempCovs, convertData, batchSize = 100000, dataEnv=dataEnv)
          ParallelLogger::logTrace(paste0('Added any age covariates for timeId ', i))
        }
        
        # add non age temporal covs for each time
        tempCovs <- addNonAgeTemp(timeId = i,newcovariateData)
        if(!is.null(tempCovs)){
          Andromeda::batchApply(tempCovs, convertData, batchSize = 100000, dataEnv=dataEnv)
          ParallelLogger::logTrace(paste0('Added non-age non-temporal covariates for timeId ', i))
        }
        
        # add non temporal covs
        tempCovs <- newcovariateData$covariates %>% 
          dplyr::filter(!is.na(.data$timeId)) %>% 
          dplyr::filter(.data$timeId == i) 
        Andromeda::batchApply(tempCovs, convertData, batchSize = 100000, dataEnv=dataEnv)
        
        data_array <- slam::as.simple_sparse_array(dataPlp)
        # remove dataPlp
        #dataPlp <<- NULL
        ParallelLogger::logTrace(paste0('Dim of data_array: ', paste0(dim(data_array), collapse='-')))
        
        #extending one more dimesion to the array
        data_array<-slam::extend_simple_sparse_array(data_array, MARGIN =c(1L))
        ParallelLogger::logTrace(paste0('Finished Mapping covariates for timeId ', i))
      } else {
        data_array <- tryCatch(slam::simple_sparse_array(i=matrix(c(1,1,1), ncol = 3), 
                                                   v=0,
                                                   dim=c(maxX,1, maxY))
        )
        
      }
      #binding arrays along the dimesion
      if(i==min(plpData$timeRef$timeId)) {
        result_array <- data_array
      }else{
        result_array <- slam::abind_simple_sparse_array(result_array,data_array,MARGIN=2L)
      }
    }
    data <- result_array
  }
  
  ParallelLogger::logDebug(paste0('Sparse matrix with dimensionality: ', paste(dim(data), collapse=',')  ))

  ParallelLogger::logInfo(paste0('finishing toSparseM'))
  
  result <- list(data=data,
                 covariateRef=as.data.frame(newcovariateData$covariateRef),
                 map=as.data.frame(newcovariateData$mapping))
  return(result)

}

# restricts to pop and saves/creates mapping
MapCovariates <- function(covariateData,population, mapping){
  
  # to remove check notes
  #covariateId <- oldCovariateId <- newCovariateId <- NULL
  ParallelLogger::logInfo(paste0('starting MapCovariates'))
  
  newCovariateData <- Andromeda::andromeda(covariateRef = covariateData$covariateRef,
                                           analysisRef = covariateData$analysisRef)
  
  # restrict to population for speed
  ParallelLogger::logTrace('restricting to population for speed and mapping')
  if(is.null(mapping)){
    mapping <- data.frame(oldCovariateId = as.data.frame(covariateData$covariateRef %>% dplyr::select(.data$covariateId)),
                          newCovariateId = 1:nrow(covariateData$covariateRef))
  }
  if(sum(colnames(mapping)%in%c('oldCovariateId','newCovariateId'))!=2){
    colnames(mapping) <- c('oldCovariateId','newCovariateId')
  }
  covariateData$mapping <- mapping
  covariateData$population <- data.frame(rowId = population[,'rowId'])
  # assign new ids :
  newCovariateData$covariates <- covariateData$covariates %>%
    dplyr::inner_join(covariateData$population) %>% 
    dplyr::rename(oldCovariateId = .data$covariateId) %>% 
    dplyr::inner_join(covariateData$mapping) %>% 
    dplyr::select(- .data$oldCovariateId)  %>%
    dplyr::rename(covariateId = .data$newCovariateId)
  covariateData$population <- NULL
  covariateData$mapping <- NULL
  
  newCovariateData$mapping <- mapping
  
  ParallelLogger::logInfo(paste0('finished MapCovariates'))
  
  return(newCovariateData)
}



#' Convert the plpData in COO format into a sparse python matrix using torch.sparse
#'
#' @description
#' Converts the standard plpData to a sparse matrix firectly into python
#'
#' @details
#' This function converts the covariate file from ffdf in COO format into a sparse matrix from
#' the package Matrix
#' @param plpData                       An object of type \code{plpData} with covariate in coo format - the patient level prediction
#'                                      data extracted from the CDM.
#' @param population                    The population to include in the matrix
#' @param map                           A covariate map (telling us the column number for covariates)
#' @param temporal                      Whether to include timeId into tensor
#' @param pythonExePath                 Location of python exe you want to use
#' @examples
#' #TODO
#'
#' @return
#' Returns a list, containing the python object name of the sparse matrix, the plpData covariateRef
#' and a data.frame named map that tells us what covariate corresponds to each column
#' This object is a list with the following components: \describe{
#' \item{data}{The python object name containing a sparse matrix with the rows corresponding to each person in the plpData and the columns corresponding to the covariates.}
#' \item{covariateRef}{The plpData covariateRef.}
#' \item{map}{A data.frame containing the data column ids and the corresponding covariateId from covariateRef.}
#' }
#'
#' @export
toSparseTorchPython <- function(plpData,population, map=NULL, temporal=F, pythonExePath=NULL){
  
  map_python_initiate <- map_python <- function(){return(NULL)}
  
  # check logger
  if(length(ParallelLogger::getLoggers())==0){
    logger <- ParallelLogger::createLogger(name = "SIMPLE",
                                           threshold = "INFO",
                                           appenders = list(ParallelLogger::createConsoleAppender(layout = 'layoutTimestamp')))
    ParallelLogger::registerLogger(logger)
  }
  
  newcovariateData <- MapCovariates(plpData$covariateData,
                                         population, 
                                         mapping=map)
  
  ParallelLogger::logDebug(paste0('Max ',as.data.frame(newcovariateData$covariates %>% dplyr::summarise(max = max(.data$covariateId, na.rm=T)))))
  ParallelLogger::logDebug(paste0('# cols: ', nrow(newcovariateData$covariateRef)))
  ParallelLogger::logDebug(paste0('Max rowId: ', as.data.frame(newcovariateData$covariates %>% dplyr::summarise(max = max(.data$rowId, na.rm=T)))))
  
  ParallelLogger::logTrace(paste0('Converting data into python sparse matrix...'))
  
  maxT <- NULL
  if(temporal){
    maxT <- as.data.frame(newcovariateData$covariates$timeId %>% dplyr::summarise(max = max(.data$id, na.rm=T)))
    ParallelLogger::logDebug(paste0('Max timeId: ', maxT))
  }
  
  maxCol <- as.data.frame(newcovariateData$mapping %>% dplyr::summarise(max=max(.data$newCovariateId,na.rm = TRUE)))$max
  maxRow <- max(population$rowId)
  
  # source the python fucntion
  e <- environment()
  reticulate::source_python(system.file(package='PatientLevelPrediction','python','TorchMap.py'), envir = e)
  
  dataEnv <- e # adding to remove <<- 
  #dataPlp <<- map_python_initiate(maxCol = as.integer(maxCol), 
  dataPlp <- map_python_initiate(maxCol = as.integer(maxCol), 
                                         maxRow = as.integer(maxRow), 
                                         maxT= as.integer(maxT))
  
  convertData <- function(batch, temporal=T, dataEnv) {
    if(temporal){
      #dataPlp <<- map_python(matrix = dataPlp ,
      dataEnv$dataPlp <- map_python(matrix = dataEnv$dataPlp,
                                    datas = as.matrix(as.data.frame(batch %>% dplyr::select(.data$rowId,.data$covariateId,.data$timeId,.data$covariateValue))),
                                    maxCol = as.integer(maxCol),
                                    maxRow = as.integer(maxRow),
                                    maxT = as.integer(maxT))
    }else{
     # dataPlp <<- map_python(matrix = dataPlp ,
      dataEnv$dataPlp <- map_python(matrix = dataEnv$dataPlp,
                             datas = as.matrix(as.data.frame(batch %>% dplyr::select(.data$rowId,.data$covariateId,.data$covariateValue))),
                             maxCol = as.integer(maxCol),
                             maxRow = as.integer(maxRow),
                             maxT = NULL) 
    }
    return(NULL)
  }
  
  if(temporal==T){
    # add the age and non-temporal data
    timeIds <- unique(plpData$timeRef$timeId)
    for(timeId in timeIds){
      tempData <- addAgeTemp(timeId, newcovariateData)
      if(!is.null(tempData)){
        Andromeda::batchApply(tempData, convertData,temporal =T, batchSize = 100000, dataEnv=dataEnv)
      }
      #tempData <- addNonAgeTemp(timeId,plpData.mapped) - what is plpData.mapped?
      tempData <- addNonAgeTemp(timeId, newcovariateData)
      if(!is.null(tempData)){
        Andromeda::batchApply(tempData, convertData,temporal =T, batchSize = 100000, dataEnv=dataEnv)
      }
      tempData <- NULL
    }
    
    # add the rest
    tempData <- newcovariateData$covariates %>%
      dplyr::filter(.data$timeId!=0) %>%
      dplyr::filter(!is.na(.data$timeId))
    Andromeda::batchApply(tempData, convertData,temporal =T, batchSize = 100000, dataEnv=dataEnv)
    tempData <- NULL
  } else {
    Andromeda::batchApply(newcovariateData$covariates, convertData,
                          temporal =F, batchSize = 100000, dataEnv=dataEnv)
  }
  ##result <- dataEnv$dataPlp
  ##dataPlp <<- NULL
  ##dataEnv$dataPlp <- NULL
  ParallelLogger::logTrace(paste0('Sparse python tensor converted'))                            
  
  result <- list(data=dataPlp,
                 covariateRef=as.data.frame(newcovariateData$covariateRef),
                 map=as.data.frame(newcovariateData$mapping))
  return(result)
}


# reformat the evaluation
reformatPerformance <- function(train, test, analysisId){
  
  ParallelLogger::logInfo(paste0('starting reformatPerformance'))

  nr1 <- length(unlist(train$evaluationStatistics[-1]))
  nr2 <- length(unlist(test$evaluationStatistics[-1]))
  evaluationStatistics <- cbind(analysisId=rep(analysisId,nr1+nr2),
                                Eval=c(rep('train', nr1),rep('test', nr2)),
                                Metric = names(c(unlist(train$evaluationStatistics[-1]),
                                                 unlist(test$evaluationStatistics[-1]))),
                                Value = c(unlist(train$evaluationStatistics[-1]),
                                      unlist(test$evaluationStatistics[-1]))
                                )


  if(!is.null(test$thresholdSummary) & !is.null(train$thresholdSummary)){
    nr1 <- nrow(train$thresholdSummary)
    nr2 <- nrow(test$thresholdSummary)
    thresholdSummary <- rbind(cbind(analysisId=rep(analysisId,nr1),Eval=rep('train', nr1),
                                    train$thresholdSummary),
                              cbind(analysisId=rep(analysisId,nr2),Eval=rep('test', nr2),
                                    test$thresholdSummary))
  } else{
    thresholdSummary <- NULL
  }
  

  if(!is.null(train$demographicSummary) & !is.null(test$demographicSummary)){
    nr1 <- nrow(train$demographicSummary)
    nr2 <- nrow(test$demographicSummary)
    demographicSummary <- rbind(cbind(analysisId=rep(analysisId,nr1),Eval=rep('train', nr1),
                                      train$demographicSummary),
                                cbind(analysisId=rep(analysisId,nr2),Eval=rep('test', nr2),
                                      test$demographicSummary))
  } else{
    demographicSummary <- NULL
  }

  nr1 <- nrow(train$calibrationSummary)
  nr2 <- nrow(test$calibrationSummary)
  calibrationSummary <- rbind(cbind(analysisId=rep(analysisId,nr1),Eval=rep('train', nr1),
                                    train$calibrationSummary),
                              cbind(analysisId=rep(analysisId,nr2),Eval=rep('test', nr2),
                                    test$calibrationSummary))

  if(!is.null(train$predictionDistribution) & !is.null(test$predictionDistribution)){
    nr1 <- nrow(train$predictionDistribution)
    nr2 <- nrow(test$predictionDistribution)
    predictionDistribution <- rbind(cbind(analysisId=rep(analysisId,nr1),Eval=rep('train', nr1),
                                    train$predictionDistribution),
                              cbind(analysisId=rep(analysisId,nr2),Eval=rep('test', nr2),
                                    test$predictionDistribution))
  } else {
    predictionDistribution <- NULL
  }
    
  result <- list(evaluationStatistics=evaluationStatistics,
                 thresholdSummary=thresholdSummary,
                 demographicSummary =demographicSummary,
                 calibrationSummary=calibrationSummary,
                 predictionDistribution=predictionDistribution)

  return(result)
}


# helpers for converting temporal PLP data to matrix/tensor
addAgeTemp <- function(timeId, newcovariateData, timeRef){
  
  startDay <- as.data.frame(timeRef[timeRef$timeId==timeId,])$startDay
  
  ageId <- as.data.frame(newcovariateData$mapping %>% 
    dplyr::filter(.data$oldCovariateId == 1002) %>%
    dplyr::select(.data$newCovariateId))$newCovariateId
  
  ageData <- newcovariateData$covariates%>% # changed from plpData$covariateData
    dplyr::filter(.data$covariateId == ageId) %>%
    dplyr::mutate(covariateValueNew = .data$covariateValue*365 + startDay,
                  timeId = timeId) %>%
    dplyr::select(- .data$covariateValue) %>%
    dplyr::rename(covariateValue = .data$covariateValueNew) %>%
    dplyr::select(.data$rowId,.data$covariateId,.data$covariateValue, .data$timeId)
  
  if(nrow(ageData)==0){
    return(NULL)
  }
  return(ageData)
}


addNonAgeTemp <- function(timeId, newcovariateData){

  ageId <- as.data.frame(newcovariateData$mapping %>% 
                           dplyr::filter(.data$oldCovariateId == 1002) %>%
                           dplyr::select(.data$newCovariateId))$newCovariateId
  
  otherTempCovs <- newcovariateData$covariates%>% 
    dplyr::filter(is.na(.data$timeId)) %>%
    dplyr::filter(.data$covariateId != ageId) %>%
    dplyr::mutate(timeId = timeId) %>%
    dplyr::select(.data$rowId,.data$covariateId,.data$covariateValue,.data$timeId)
  
  if(nrow(otherTempCovs)==0){
    return(NULL)
  }
  return(otherTempCovs)
}
