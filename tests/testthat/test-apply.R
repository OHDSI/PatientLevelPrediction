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

library("testthat")

context("ApplyPlp")

# this no longer checks predictions as models don't exist and take too long to train during test
# generate simulated data:
set.seed(1234)
data(plpDataSimulationProfile)
sampleSize <- 2000
plpData <- simulatePlpData(plpDataSimulationProfile, n = sampleSize)

# create popualtion for outcome 2
population <- createStudyPopulation(plpData,
                                    outcomeId = 2,
                                    firstExposureOnly = FALSE,
                                    washoutPeriod = 0,
                                    removeSubjectsWithPriorOutcome = FALSE,
                                    priorOutcomeLookback = 99999,
                                    requireTimeAtRisk = FALSE,
                                    minTimeAtRisk=0,
                                    riskWindowStart = 0,
                                    addExposureDaysToStart = FALSE,
                                    riskWindowEnd = 365,
                                    addExposureDaysToEnd = FALSE)

index <- PatientLevelPrediction::randomSplitter(population, test=0.2, seed=1)
population <- merge(population, index)
colnames(population)[colnames(population)=='index'] <- 'indexes'

test_that("applyModel inputs", {

  testthat::expect_error(applyModel(population=NULL,
                                    plpData,
                                    plpModel,
                                    calculatePerformance=T,
                                    databaseOutput = NULL,
                                    silent = F))
  
  testthat::expect_error(applyModel(population=population,
                                    plpData=NULL,
                                    plpModel,
                                    calculatePerformance=T,
                                    databaseOutput = NULL,
                                    silent = F))
  
  testthat::expect_error(applyModel(population=population,
                                    plpData=plpData,
                                    plpModel = NULL,
                                    calculatePerformance=T,
                                    databaseOutput = NULL,
                                    silent = F))

  })



test_that("similarPlpData inputs", {
  
  testthat::expect_null(similarPlpData(plpModel=NULL,
                                        createCohorts = T,
                                        newConnectionDetails,
                                        newCdmDatabaseSchema = NULL,
                                        newCohortDatabaseSchema = NULL,
                                        newCohortTable = NULL,
                                        newCohortId = NULL,
                                        newOutcomeDatabaseSchema = NULL,
                                        newOutcomeTable = NULL,
                                        newOutcomeId = NULL,
                                        newOracleTempSchema = newCdmDatabaseSchema,
                                        sample=NULL, 
                                        createPopulation= T))
  
})


