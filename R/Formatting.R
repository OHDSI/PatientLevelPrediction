# @file formatting.R
#
# Copyright 2021 Observational Health Data Sciences and Informatics
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
#' @param cohort                        If specified the plpData is restricted to the rowIds in the cohort (otherwise plpData$labels is used)                                     
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
toSparseM <- function(plpData, cohort = NULL, map=NULL){
  
  ParallelLogger::logInfo(paste0('starting toSparseM'))
  
  ParallelLogger::logDebug(
    paste0(
      'Max covariateId in original covariates: ', 
      plpData$covariateData$covariates %>% 
        dplyr::summarise(max = max(.data$covariateId, na.rm=T)) %>% 
        dplyr::pull() 
    )
  )
  
  # assign covariateId to colummId specified in map (if null create columnIds)
  # assign rowId to xId 
  # return the new covariateData (rowId changes in covariates plus covariates/covariateRef with modified columnIds)
  # return labels with modified rowId if plpData$labels exists

  if(!is.null(plpData$labels)){
    newcovariateData <- MapIds(
      plpData$covariateData,
      cohort = plpData$labels,
      mapping=map
      )
  } else{
    newcovariateData <- MapIds(
      plpData$covariateData,
      cohort = cohort,
      mapping = map
    )
  }
  
  
  ParallelLogger::logDebug(paste0('# covariates in mapped covariateRef: ', nrow(newcovariateData$covariateRef)))

  maxY <- newcovariateData$mapping %>% 
    dplyr::summarise(max=max(.data$columnId, na.rm = TRUE)) %>% 
    dplyr::pull()
  ParallelLogger::logDebug(paste0('Max newCovariateId in mapping: ',maxY))
  maxX <- newcovariateData$cohort %>% dplyr::summarise(max = max(.data$rowId, na.rm=T)) %>% dplyr::pull()
  ParallelLogger::logDebug(paste0('Max rowId in new : ',maxX))
  

  ParallelLogger::logInfo(paste0('toSparseM non temporal used'))
    
  checkRam(newcovariateData, 0.9)  # estimates size of RAM required and makes sure it is less that 90%
    
  data <- Matrix::sparseMatrix(
    i = newcovariateData$covariates %>% dplyr::select(.data$rowId) %>% dplyr::pull(),
    j = newcovariateData$covariates %>% dplyr::select(.data$columnId) %>% dplyr::pull(),
    x = newcovariateData$covariates %>% dplyr::select(.data$covariateValue) %>% dplyr::pull(),
    dims=c(maxX,maxY)
  )
    
  ParallelLogger::logDebug(paste0('Sparse matrix with dimensionality: ', paste(dim(data), collapse=',')  ))

  ParallelLogger::logInfo(paste0('finishing toSparseM'))
  
  result <- list(
    dataMatrix = data,
    labels = newcovariateData$cohort %>% dplyr::collect(),
    covariateRef = as.data.frame(newcovariateData$covariateRef),
    covariateMap = as.data.frame(newcovariateData$mapping)
  )
  return(result)
}

# this functions takes covariate data and a cohort/population and remaps 
# the covaiate and row ids 
# restricts to pop and saves/creates mapping
MapIds <- function(
  covariateData,
  cohort = NULL,
  mapping = NULL
  ){
  
  ParallelLogger::logInfo(paste0('starting to map the columns and rows'))
  
  
  # change the rowIds in cohort (if exists)
  if(!is.null(cohort)){
    rowMap <- data.frame(
      rowId = cohort %>% dplyr::select(.data$rowId)
    )
    rowMap$xId <- 1:nrow(rowMap)
  } else{
    rowMap <- data.frame(
      rowId = covariateData$covariates %>% 
        dplyr::distinct(.data$rowId) %>% 
        dplyr::pull()
    )
    rowMap$xId <- 1:nrow(rowMap)
  }
  
  covariateData$rowMap <- rowMap
  on.exit(covariateData$rowMap <- NULL)
  
  # change the rowIds in covariateData$covariates
  if(is.null(mapping)){
  
    mapping <- data.frame(
      covariateId = covariateData$covariates %>% 
        dplyr::inner_join(covariateData$rowMap, by = 'rowId') %>%  # first restrict the covariates to the rowMap$rowId
        dplyr::distinct(.data$covariateId) %>% 
        dplyr::pull()
    )
    mapping$columnId <- 1:nrow(mapping)
  }
  covariateData$mapping <- mapping
  on.exit(covariateData$mapping <- NULL, add = T)
  
  newCovariateData <- Andromeda::andromeda()
  # change the covariateIds in covariates
    newCovariateData$covariates <- covariateData$covariates %>%
      dplyr::inner_join(covariateData$mapping, by = 'covariateId')
  
    
    # change the covariateIds in covariateRef
    newCovariateData$covariateRef <- covariateData$mapping %>%
      dplyr::inner_join(covariateData$covariateRef, by = 'covariateId')
    
    # change the rowId in covariates
    newCovariateData$rowMap <- rowMap
    newCovariateData$covariates <- newCovariateData$covariates %>%
      dplyr::inner_join(newCovariateData$rowMap, by = 'rowId') %>% 
      dplyr::select(- .data$rowId) %>%
      dplyr::rename(rowId = .data$xId)
    
    if(!is.null(cohort)){
      # change the rowId in labels
      newCovariateData$cohort <- cohort %>%
        dplyr::inner_join(rowMap, by = 'rowId') %>% 
        #dplyr::select(- .data$rowId) %>%
        dplyr::rename(
          originalRowId = .data$rowId,
          rowId = .data$xId
          ) %>%
        dplyr::arrange(.data$rowId)  # make sure it is ordered lowest to highest
    }
  
  newCovariateData$mapping <- mapping
  
  ParallelLogger::logInfo(paste0('finished MapCovariates'))
  
  return(newCovariateData)
}

checkRam <- function(covariateData, maxPercent){
  
  ensure_installed('memuse')
  
  nrowV <- covariateData$covariates %>% dplyr::summarise(size = dplyr::n()) %>% dplyr::collect()
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
