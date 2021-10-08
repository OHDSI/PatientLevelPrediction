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
toSparseM <- function(plpData,population, map=NULL){
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
                                 population = population, 
                                 mapping=map)
  
  ParallelLogger::logDebug(paste0('Max covariateId in covariates: ',as.data.frame(newcovariateData$covariates %>% dplyr::summarise(max = max(.data$covariateId, na.rm=T)))))
  ParallelLogger::logDebug(paste0('# covariates in covariateRef: ', nrow(newcovariateData$covariateRef)))
  ParallelLogger::logDebug(paste0('Max rowId in covariates: ', as.data.frame(newcovariateData$covariates %>% dplyr::summarise(max = max(.data$rowId, na.rm=T)))))

  maxY <- as.data.frame(newcovariateData$mapping %>% dplyr::summarise(max=max(.data$newCovariateId, na.rm = TRUE)))$max
  ParallelLogger::logDebug(paste0('Max newCovariateId in mapping: ',maxY))
  maxX <- max(population$rowId)
  ParallelLogger::logDebug(paste0('Max rowId in population: ',maxX))
  

  ParallelLogger::logInfo(paste0('toSparseM non temporal used'))
    
  checkRam(newcovariateData, 0.9)  # estimates size of RAM required and makes sure it is less that 90%
    
    data <- Matrix::sparseMatrix(i=as.data.frame(newcovariateData$covariates %>% dplyr::select(.data$rowId))$rowId,
                                 j=as.data.frame(newcovariateData$covariates %>% dplyr::select(.data$covariateId))$covariateId,
                                 x=as.data.frame(newcovariateData$covariates %>% dplyr::select(.data$covariateValue))$covariateValue,
                                 dims=c(maxX,maxY))
    
  ParallelLogger::logDebug(paste0('Sparse matrix with dimensionality: ', paste(dim(data), collapse=',')  ))

  ParallelLogger::logInfo(paste0('finishing toSparseM'))
  
  result <- list(data=data,
                 covariateRef=as.data.frame(newcovariateData$covariateRef),
                 map=as.data.frame(newcovariateData$mapping))
  return(result)

}

# restricts to pop and saves/creates mapping
MapCovariates <- function(covariateData,population = NULL, mapping){
  
  # to remove check notes
  #covariateId <- oldCovariateId <- newCovariateId <- NULL
  ParallelLogger::logInfo(paste0('starting MapCovariates'))
  
  newCovariateData <- Andromeda::andromeda(covariateRef = covariateData$covariateRef,
                                           analysisRef = covariateData$analysisRef)
  

  if(is.null(mapping)){
    mapping <- data.frame(oldCovariateId = as.data.frame(covariateData$covariateRef %>% dplyr::select(.data$covariateId)),
                          newCovariateId = 1:nrow(covariateData$covariateRef))
  }
  if(sum(colnames(mapping)%in%c('oldCovariateId','newCovariateId'))!=2){
    colnames(mapping) <- c('oldCovariateId','newCovariateId')
  }
  covariateData$mapping <- mapping
  
  if(!is.null(population)){
  # restrict to population for speed
  ParallelLogger::logTrace('restricting to population for speed and mapping')
  covariateData$population <- data.frame(rowId = population[,'rowId'])
  # assign new ids :
  newCovariateData$covariates <- covariateData$covariates %>%
    dplyr::inner_join(covariateData$population) %>% 
    dplyr::rename(oldCovariateId = .data$covariateId) %>% 
    dplyr::inner_join(covariateData$mapping) %>% 
    dplyr::select(- .data$oldCovariateId)  %>%
    dplyr::rename(covariateId = .data$newCovariateId)
  covariateData$population <- NULL
  } else{
    newCovariateData$covariates <- covariateData$covariates %>%
      dplyr::rename(oldCovariateId = .data$covariateId) %>% 
      dplyr::inner_join(covariateData$mapping) %>% 
      dplyr::select(- .data$oldCovariateId)  %>%
      dplyr::rename(covariateId = .data$newCovariateId)
  }
  covariateData$mapping <- NULL
  
  newCovariateData$mapping <- mapping
  
  ParallelLogger::logInfo(paste0('finished MapCovariates'))
  
  return(newCovariateData)
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

checkRam <- function(covariateData, maxPercent){
  
  ensure_installed('memuse')
  
  nrowV <- covariateData$covariates %>% dplyr::summarise(size = n()) %>% dplyr::collect()
  estRamB <- (nrowV$size/1000000*24000984)
  
  ramFree <- memuse::Sys.meminfo()
  ramFree <- as.double(ramFree$freeram)
  
  if(0.9*ramFree<estRamB){
    ParallelLogger::logWarn('plpData size bigger than current available RAM')
    stop('Insufficient RAM')
  } else{
    ParallelLogger::logInfo(paste0('plpData size estimated to use ',round(estRamB/ramFree*100,2),'% of available RAM (', round(estRamB/1000000000,1), 'GBs)'))
  }
  
  return(invisible(TRUE))
}
