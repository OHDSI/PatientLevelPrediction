# Copyright 2018 Observational Health Data Sciences and Informatics
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
testthat::context('Package Functions')
library("testthat")

# Test unit for the packagePlp functions

test_that("exportPlpResult input checks work", {
  
  # error if plpResult missing
  expect_error(exportPlpResult(plpResult, modelName,
                              packageName, 
                              gitHubLocation,
                              n=NULL, 
                              includeEvaluationStatistics = T, 
                              includeThresholdSummary = T, 
                              includeDemographicSummary = T, 
                              includeCalibrationSummary = T, 
                              includePredictionDistribution = T, 
                              includeCovariateSummary = F))
  
  # error if packageName missing
  expect_error(exportPlpResult(plpResult=list(), 
                               modelName,
                               packageName, 
                               gitHubLocation,
                               n=NULL, 
                               includeEvaluationStatistics = T, 
                               includeThresholdSummary = T, 
                               includeDemographicSummary = T, 
                               includeCalibrationSummary = T, 
                               includePredictionDistribution = T, 
                               includeCovariateSummary = F))
  
  # error if gitHubLocation missing
  expect_error(exportPlpResult(plpResult=list(), 
                               modelName,
                               packageName='test', 
                               gitHubLocation,
                               n=NULL, 
                               includeEvaluationStatistics = T, 
                               includeThresholdSummary = T, 
                               includeDemographicSummary = T, 
                               includeCalibrationSummary = T, 
                               includePredictionDistribution = T, 
                               includeCovariateSummary = F))
  
  # error if non existant file.path(gitHubLocation, packageName)
  expect_error(exportPlpResult(plpResult=list(), 
                               modelName,
                               packageName='test', 
                               gitHubLocation='madeuploc435958',
                               n=NULL, 
                               includeEvaluationStatistics = T, 
                               includeThresholdSummary = T, 
                               includeDemographicSummary = T, 
                               includeCalibrationSummary = T, 
                               includePredictionDistribution = T, 
                               includeCovariateSummary = F))
})


test_that("createCohort input checks work", {
  
  # error if package missing
  expect_error(createCohort(cohortDetails=data.frame(cohortId=1), 
                                        cohortLocation='cohort',
                                        connectionDetails=list(),
                                        cdmDatabaseSchema='cdmDatabaseSchema',
                                        cohortDatabaseSchema='cohortDatabaseSchema',
                                        cohortTable='cohort',
                                        package)
               )
  
  # cohortDetails no data.frame
  expect_error(createCohort(cohortDetails=list(cohortId=1), 
                            cohortLocation='cohort',
                            connectionDetails=list(),
                            cdmDatabaseSchema='cdmDatabaseSchema',
                            cohortDatabaseSchema='cohortDatabaseSchema',
                            cohortTable='cohort',
                            package='test')
  )
  
  # cohortLocation not character
  expect_error(createCohort(cohortDetails=data.frame(cohortId=1), 
                            cohortLocation=list('cohort'),
                            connectionDetails=list(),
                            cdmDatabaseSchema='cdmDatabaseSchema',
                            cohortDatabaseSchema='cohortDatabaseSchema',
                            cohortTable='cohort',
                            package='test')
  )
  
  
  
})


test_that("standardOutput input checks work", {
  
  # error if result missing
  expect_error(standardOutput(result, table1,
                                          outputLocation='here',
                                          studyName='studyName',
                                          databaseName='databaseName', 
                                          cohortName='cohortName', 
                                          outcomeName='outcomeName')
  )
  
  # error if sum(names(result)%in%c('performanceEvaluation', 'performance'))
  expect_error(standardOutput(result=list(performanceEvaluation2='test'), 
                              table1,
                              outputLocation='here',
                              studyName='studyName',
                              databaseName='databaseName', 
                              cohortName='cohortName', 
                              outcomeName='outcomeName')
  )
  
  # missing outputLocation
  expect_error(standardOutput(result=list(performanceEvaluation='test'), 
                              table1,
                              outputLocation,
                              studyName='studyName',
                              databaseName='databaseName', 
                              cohortName='cohortName', 
                              outcomeName='outcomeName')
  )
  
  # missing studyName
  expect_error(standardOutput(result=list(performanceEvaluation='test'), 
                              table1,
                              outputLocation='here',
                              studyName,
                              databaseName='databaseName', 
                              cohortName='cohortName', 
                              outcomeName='outcomeName')
  )
  
  
  # missing databaseName
  expect_error(standardOutput(result=list(performanceEvaluation='test'), 
                              table1,
                              outputLocation='here',
                              studyName='studyName',
                              databaseName, 
                              cohortName='cohortName', 
                              outcomeName='outcomeName')
  )
  
  # missing cohortName
  expect_error(standardOutput(result=list(performanceEvaluation='test'), 
                              table1,
                              outputLocation='here',
                              studyName='studyName',
                              databaseName='databaseName', 
                              cohortName, 
                              outcomeName='outcomeName')
  )
  
  # missing outcomeName
  expect_error(standardOutput(result=list(performanceEvaluation='test'), 
                              table1,
                              outputLocation='here',
                              studyName='studyName',
                              databaseName='databaseName', 
                              cohortName='cohortName', 
                              outcomeName)
  )
  
  
  
})





test_that("packageResults input checks work", {
  
  # error if mainFolder missing
  expect_error(packageResults(mainFolder, 
                                          includeROCplot= T,
                                          includeCalibrationPlot = T,
                                          includePRPlot = T,
                                          includeTable1 = T,
                                          includeThresholdSummary =T,
                                          includeDemographicSummary = T,
                                          includeCalibrationSummary = T,
                                          includePredictionDistribution =T,
                                          includeCovariateSummary = F,
                                          removeLessThanN = F,
                                          N = 10)
  )
  
  
})



