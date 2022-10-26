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
  
  on.exit(covariateData$pop <- NULL, add = T)
  
  newCovariateData$covariates <- covariateData$covariates %>% 
    dplyr::inner_join(covariateData$pop, by = 'rowId')
  
  metaData$populationSize <- length(rowIds)
  attr(newCovariateData, 'metaData') <- metaData
  class(newCovariateData) <- c("CovariateData")
  ParallelLogger::logTrace(paste0('Finished limiting covariate data to population...'))
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

