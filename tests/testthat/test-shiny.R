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

context("Shiny")


test_that("checkPlpInput runPlp", {
  res <- checkPlpInput(plpResult)
  testthat::expect_equal(res, 'plpResult')
})


test_that("getSummary runPlp without external validation", {
  
  res <- getSummary(result = plpResult, inputType = 'plpResult', validation = NULL)
  testthat::expect_equal(class(res), 'data.frame')
  testthat::expect_equal(nrow(res), 1)
  })


test_that("getFilter works", {
  summaryTable <- data.frame(Dev = c('Dev1','Dev1'),
                             Val = c('Dev1', 'Val1'),
                             T= c('T','T'),
                             O = c('O','O'),
                             Model = c('lr','gbm'),
                             "TAR start" = c(1,0),
                             "TAR end" = c(365,90)
                             )
  colnames( summaryTable) <- c('Dev','Val','T','O','Model','TAR start','TAR end')
  input <- data.frame(devDatabase = 'All',
                      valDatabase = 'All',
                      T = 'All',
                      O = 'All',
                      modelSettingName = 'All',
                      riskWindowStart = 'All',
                      riskWindowEnd = 'All')
  res <- getFilter(summaryTable,input)
  testthat::expect_equal(length(res), 2)
  
  input <- data.frame(devDatabase = 'All',
                      valDatabase = 'Val1',
                      T = 'All',
                      O = 'All',
                      modelSettingName = 'All',
                      riskWindowStart = 'All',
                      riskWindowEnd = 'All')
  res <- getFilter(summaryTable,input)
  testthat::expect_equal(length(res), 1)
})

test_that("getPlpResult runPlp", {
  res <- getPlpResult(result = plpResult,
               validation = NULL,
               summaryTable = NULL, 
               inputType = 'plpResult',
               filterIndex = 1, 
               selectedRow= 1)
  testthat::expect_equal(class(res), 'runPlp')
})


test_that("formatModSettings works", {
  res <- formatModSettings(plpResult$model$modelSettings)
  testthat::expect_equal(class(res), 'data.frame')
})
test_that("formatCovSettings works", {
  res <- formatCovSettings(plpResult$model$metaData$call$covariateSettings)
  testthat::expect_equal(class(res), 'data.frame')
})
test_that("formatPopSettings works", {
  res <- formatPopSettings(plpResult$model$populationSettings)
  testthat::expect_equal(class(res), 'data.frame')
})


test_that("formatCovariateTable works", {
  res <- formatCovariateTable(plpResult$covariateSummary)
  testthat::expect_equal(class(res), 'data.frame')
})

