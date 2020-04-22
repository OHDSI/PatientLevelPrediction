# @file ffHelperFunctions.R
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

limitCovariatesToPopulation <- function(covariateData, rowIds) {
  covariateData$covariates <- covariateData$covariates %>% dplyr::filter(rowId %in% rowIds)
  return(covariateData)
}

# return prev of ffdf 
calculatePrevs <- function(plpData, population){
  #===========================
  # outcome prevs
  #===========================
  
  # add population to sqllite
  plpData$population <- population 
  plpData$population <- plpData$population %>% dplyr::select(rowId, outcomeCount)
  
  outCount <- nrow(plpData$population %>% dplyr::filter(outcomeCount == 1))
  nonOutCount <- nrow(plpData$population %>% dplyr::filter(outcomeCount == 0))
  
  # join covariate with label
  prevs <- plpData$covariateData$covariates %>% dplyr::inner_join(population) %>%
    dplyr::group_by(covariateId) %>% 
    dplyr::summarise(prev.out = sum(outcomeCount==1)/outCount,
              prev.noout = sum(outcomeCount==0)/nonOutCount) %>%
    dplyr::select(covariateId, prev.out, prev.noout)
  
  
  return(as.data.frame(prevs))
}

