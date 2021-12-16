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

context("Diagnostic")

test_that("test code works when using plpData", {
  test <- diagnostic(
    plpData = plpData, 
    cdmDatabaseName = 'madeup', 
    cohortName = 'made up target',
    outcomeNames = paste0('made up outcome', 1:2),
    databaseDetails,
    restrictPlpDataSettings,
    populationSettings = createStudyPopulationSettings(
      riskWindowStart = 1, 
      startAnchor = 'cohort start', 
      riskWindowEnd = 365,
      endAnchor = 'cohort start'
      ),
    minCellCount = 5,
    outputFolder = file.path(saveLoc, 'diagnostics')
  )
  #check results are a list
  testthat::expect_equal(class(test), 'list')
                         
  # check list names
  testthat::expect_equal(sum(names(test)%in%c('distribution','proportion','characterization')), 3)
  
  # check the results are saved into the databaseName directory
  testthat::expect_equal(T, dir.exists(file.path(saveLoc, 'diagnostics')))
  
  #check tar

  testthat::expect_equal(unique(test$proportion$TAR)[1], paste0('cohort start', ' + ', 1, ' days - ',
                                                        'cohort start', ' + ', 365, ' days'))
})
