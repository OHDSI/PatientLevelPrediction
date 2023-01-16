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

library("testthat")
context("AndromedaHelperFunctions")

test_that("limitCovariatesToPopulation works", {
  rowIds <- population$rowId
  
  newCovariateData <- limitCovariatesToPopulation(plpData$covariateData, rowIds)
  
  expect_equal(length(unique(rowIds)), newCovariateData$covariates %>% 
                 dplyr::summarise(unique=dplyr::n_distinct(rowId)) %>% 
                 dplyr::collect() %>% 
                 dplyr::pull())  
  
})