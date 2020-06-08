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

library("testthat")

context("ApplyPlp")

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

appliedModel <- applyModel(population=population2,
                 plpData = plpData2,
                 plpModel = plpResult$model,
                 calculatePerformance=T,
                 databaseOutput = NULL,
                 silent = F)

test_that("applyModel works", {

  # all outputs:
  testthat::expect_equal(sum(names(appliedModel)=="prediction"), 1)
  testthat::expect_equal(sum(names(appliedModel)=="performanceEvaluation"), 1)
  testthat::expect_equal(sum(names(appliedModel)=="executionSummary"), 1)
  testthat::expect_equal(sum(names(appliedModel)=="inputSetting"), 1)
  testthat::expect_equal(sum(names(appliedModel)=="model"), 1)
  testthat::expect_equal(sum(names(appliedModel)=="analysisRef"), 1)
  testthat::expect_equal(sum(names(appliedModel)=="covariateSummary"), 1)
  
  # correct size
  testthat::expect_equal(nrow(appliedModel$prediction), nrow(population2))
  
  
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
                                        newOracleTempSchema = NULL,
                                        sample=NULL, 
                                        createPopulation= T))
  
})


