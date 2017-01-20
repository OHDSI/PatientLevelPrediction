# @file test_DataSplitting.R
# Copyright 2017 Observational Health Data Sciences and Informatics
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

test_that("Data splitting by person", {

  flog.threshold(FATAL)
  # error message checks
  population1 <- data.frame(rowId=1:20, outcomeCount=c(1,1,1,1,rep(0,16))) 
  expect_error(personSplitter(population1, test=0.3, nfold=3))
  
  population2 <- data.frame(rowId=1:200, outcomeCount=c(rep(1,42),rep(0,158)))
  expect_error(personSplitter(population2, test=0.3, nfold=-1))
  expect_error(personSplitter(population2, test=1.5, nfold=5))
  expect_error(personSplitter(population2, test=-1, nfold=5))
  
  # fold creation check 1 (fixed)
  test <- personSplitter(population2, test=0.2, nfold=4)
  test <- merge(population2, test)
  test <- table(test$outcomeCount, test$index)
  test.returned <- paste(test, collapse='-')
  test.expected <- paste(matrix(c(32,32,32,31,31,8,9,9,8,8), ncol=5, byrow=T),collapse='-')
  expect_identical(test.returned, test.expected)
  
  # fold creation check 2 (sum)
  size <- 500
  population3 <- data.frame(rowId=1:size, outcomeCount=c(rep(1,floor(size/3)),rep(0,size-floor(size/3)))) 
  test <- personSplitter(population3, test=0.2, nfold=4) 
  test <- merge(population3, test)
  test <- table(test$outcomeCount, test$index)
  expect_that(sum(test), equals(size))
  
})

test_that("Data splitting by time", {
  
  # error message checks
  population1 <- data.frame(rowId=1:200, outcomeCount=c(rep(1,42),rep(0,158)), 
                            cohortStartDate = as.Date("2016-01-01") + c(1:200))
  expect_error(timeSplitter(population1, test=0.3, nfold=-1))
  expect_error(timeSplitter(population1, test=1.5, nfold=5))
  expect_error(timeSplitter(population1, test=-1, nfold=5))
  
  # fold creation check (sum)
  size <- 500
  set.seed(1)
  population2 <- data.frame(rowId=1:size, outcomeCount=sample(0:1,size,replace=TRUE),cohortStartDate = as.Date("2010-01-01") + c(1:size))
  test <- timeSplitter(population2, test=0.2, nfold=4) 
  test <- merge(population2, test)
  test <- table(test$outcomeCount, test$index)
  expect_that(sum(test), equals(size))
  
})
