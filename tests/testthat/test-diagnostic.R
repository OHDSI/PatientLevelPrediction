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

context("Diagnostic")

test_that("tar id works", {
  riskWindowStart = sample(1000,1)
  startAnchor = ifelse(runif(1)<0.5, 'cohort start','cohort end')
  riskWindowEnd = sample(9000,1)
  endAnchor = ifelse(runif(1)<0.5, 'cohort start','cohort end')
  
  id <- getTarId(riskWindowStart = riskWindowStart,
                       startAnchor = startAnchor,
                       riskWindowEnd = riskWindowEnd ,
                       endAnchor = endAnchor)
  
  reverse <- convertTarId(id)
  
  testthat::expect_equal(riskWindowStart, reverse$riskWindowStart)
  testthat::expect_equal(startAnchor, as.character(reverse$startAnchor))
  testthat::expect_equal(riskWindowEnd, reverse$riskWindowEnd)
  testthat::expect_equal(endAnchor, as.character(reverse$endAnchor))
  

})


test_that("test code works when using plpData", {
  test <- diagnostic(plpData = plpData, cdmDatabaseName = 'madeup', 
                     riskWindowStart = 10, 
                     startAnchor = 'cohort start', 
                     riskWindowEnd = 1*365,
                     endAnchor = 'cohort start',
                     outputFolder = file.path(saveLoc, 'diagnostics'))
  #check results are a list
  testthat::expect_equal(class(test), 'list')
                         
  # check list names
  testthat::expect_equal(sum(names(test)%in%c('distribution','incidence','characterization')), 3)
  
  # check the results are saved into the databaseName directory
  testthat::expect_equal(T, dir.exists(file.path(saveLoc, 'diagnostics','madeup')))
  
})