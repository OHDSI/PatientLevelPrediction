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

context("FeatureImportance")

# Test unit for the creation of the study externalValidatePlp


test_that("pfi feature importance returns data.frame", {
  
  pfiTest <- pfi(plpResult, population, plpData, repeats = 1,
                  covariates = NULL, cores = NULL, log = NULL,
                  logthreshold = "INFO")
  
  testthat::expect_equal(class(pfiTest), 'data.frame')
  testthat::expect_equal(sum(names(pfiTest)%in%c("covariateId", "pfi")), 2)
  
})

