# @file ffHelperFunctions.R
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

limitCovariatesToPopulation <- function(covariateData, rowIds) {
  ParallelLogger::logTrace(paste0('Starting to limit covariate data to population...'))
  
  metaData <- attr(covariateData, 'metaData')
  
  newCovariateData <- Andromeda::andromeda(covariateRef = covariateData$covariateRef,
                                           analysisRef = covariateData$analysisRef)
  
  covariateData$pop <- data.frame(rowId = rowIds)
  Andromeda::createIndex(tbl = covariateData$pop, columnNames = 'rowId', 
                         indexName = 'pop_rowIds')
  
  on.exit(covariateData$pop <- NULL, add = T)
  
  newCovariateData$covariates <- covariateData$covariates %>% 
    dplyr::inner_join(covariateData$pop, by = 'rowId')
  Andromeda::createIndex(tbl = newCovariateData$covariates, columnNames = 'covariateId', 
                         indexName = 'covariates_ncovariateIds')
  
  metaData$populationSize <- length(rowIds)
  attr(newCovariateData, 'metaData') <- metaData
  class(newCovariateData) <- "CovariateData"
  ParallelLogger::logTrace(paste0('Finished limiting covariate data to population...'))
  return(newCovariateData)
}



batchRestrict <- function(covariateData, population, sizeN = 10000000){
  
  ParallelLogger::logInfo('Due to data size using batchRestrict to limit covariate data to population')
  
  start <- Sys.time()
  
  metaData <- attr(covariateData, 'metaData')
  
  newCovariateData <- Andromeda::andromeda(covariateRef = covariateData$covariateRef,
                                           analysisRef = covariateData$analysisRef)
  
  Andromeda::batchApply(covariateData$covariates, function(tempData) {
  
    filtered <- dplyr::inner_join(tempData, population, by = 'rowId')
    
    if ("covariates" %in% names(newCovariateData)) {
      Andromeda::appendToTable(newCovariateData$covariates, data = filtered)
    } else {
      newCovariateData$covariates <- filtered
    }
  },
  progressBar = TRUE,
  batchSize = sizeN)
  
  Andromeda::createIndex(tbl = newCovariateData$covariates,
                         columnNames = 'covariateId',
                         indexName = 'covariates_ncovariateIds')
  Andromeda::createIndex(tbl = newCovariateData$covariates,
                         columnNames = 'rowId',
                         indexName = 'covariates_rowId')
  Andromeda::createIndex(tbl = newCovariateData$covariates,
                         columnNames = c('covariateId', 'covariateValue'),
                         indexName = 'covariates_covariateId_value')
  
  metaData$populationSize <- nrow(population)
  attr(newCovariateData, 'metaData') <- metaData
  class(newCovariateData) <- "CovariateData"
  
  timeTaken <- as.numeric(Sys.time() - start, units = "mins")
  ParallelLogger::logInfo(paste0('Limiting covariate data took: ', timeTaken, ' mins'))
  
  return(newCovariateData)
}


# is this used?
# return prev of ffdf 
calculatePrevs <- function(plpData, population){
  #===========================
  # outcome prevs
  #===========================
  
  # add population to sqllite
  population <- tibble::as_tibble(population)
  plpData$covariateData$population <- population %>% dplyr::select(.data$rowId, .data$outcomeCount)
  
  outCount <- nrow(plpData$covariateData$population %>% dplyr::filter(.data$outcomeCount == 1))
  nonOutCount <- nrow(plpData$covariateData$population %>% dplyr::filter(.data$outcomeCount == 0))
  
  # join covariate with label
  prevs <- plpData$covariateData$covariates %>% dplyr::inner_join(plpData$covariateData$population) %>%
    dplyr::group_by(.data$covariateId) %>% 
    dplyr::summarise(prev.out = 1.0*sum(.data$outcomeCount==1, na.rm = TRUE)/outCount,
              prev.noout = 1.0*sum(.data$outcomeCount==0, na.rm = TRUE)/nonOutCount) %>%
    dplyr::select(.data$covariateId, .data$prev.out, .data$prev.noout)
  
  #clear up data
  ##plpData$covariateData$population <- NULL
  
  return(as.data.frame(prevs))
}

