# @file test_DataSplitting.R
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
context("Data splitting")


# make sure pop is all plpData people

populationT <- plpData$cohorts
populationT$outcomeCount <- sample(c(0,1), nrow(populationT), replace = T)
attr(populationT, "metaData")$outcomeId <- 2
attr(populationT, "metaData")$populationSettings <- list(madeup = T)
attr(populationT, "metaData")$plpDataSettings <- list(madeup = T)
attr(populationT, "metaData")$attrition <- c(1,2,3)

# check correct inputs
testFraction1 <- sample(9,1)/10
trainFraction1 <- 1-testFraction1
splitSeed1 <- sample(100000,1)
nfold1 <- 1+sample(10,1)
type1 <- sample(c('stratified', 'time', 'subject'), 1)

defaultSetting <- function(
  testFraction = testFraction1, 
  trainFraction = trainFraction1, 
  splitSeed = splitSeed1, 
  nfold = nfold1,
  type = type1
){
  result <- createDefaultSplitSetting(
    testFraction = testFraction, 
    trainFraction = trainFraction, 
    splitSeed = splitSeed, 
    nfold = nfold,
    type = type
  )
  return(result)
}


test_that("createDefaultSplitSetting", {
  
  splitSettings <- defaultSetting()
  
  expect_is(splitSettings, 'splitSettings')
  
  expectFun <- 'randomSplitter'
  if(type1 == 'time'){
    expectFun <- 'timeSplitter'
  }
  if(type1 == 'subject'){
    expectFun <- 'subjectSplitter'
  }
  
  expect_equal(attr(splitSettings, "fun"), expectFun)
  
  expect_equal(splitSettings$test, testFraction1)
  expect_equal(splitSettings$train, trainFraction1)
  expect_equal(splitSettings$seed, splitSeed1)
  expect_equal(splitSettings$nfold, nfold1)
  
  #check input errors for testFraction
  expect_error(
    defaultSetting(testFraction = 'character')
  )
  
  expect_error(
    defaultSetting(testFraction = -0.1)
  )
  
  expect_error(
    defaultSetting(testFraction = 1.001)
  )
  
  # check input error for trainFraction
  expect_error(
    defaultSetting(trainFraction = 'trainFraction')
  )
  
  expect_error(
    defaultSetting(trainFraction = 1.2)
  )
  
  expect_error(
    defaultSetting(trainFraction = -0.2)
  )
  
  #check error for splitSeed
  
  expect_error(
    defaultSetting(splitSeed = NULL)  
  )
  
  expect_error(
    defaultSetting(splitSeed = 'NULL')  
  )
  
  # check error for nfold
  expect_error(
    defaultSetting(nfold = NULL)
  )
  expect_error(
    defaultSetting(nfold = 'NULL')
  )
  
  # incorrect type
  expect_error(
    defaultSetting(type = 'madeup')
  )
  expect_error(
    defaultSetting(type =  NULL)
  )
  expect_error(
    defaultSetting(type =  1)
  )
  
})



test_that("Main split function: splitData", {
  
  # check default settings with test/train
  splitSettings <- defaultSetting()
  
  splitData <- splitData(
    plpData = plpData,
    population = populationT,
    splitSettings = splitSettings
  )
  
  # check class
  expect_is(splitData, 'splitData')
  
  # should have test/train
  expect_equal(names(splitData), c('Train', 'Test'))
  
  # train and test are CovariateData
  expect_is(splitData$Train$covariateData, 'CovariateData')
  expect_is(splitData$Test$covariateData, 'CovariateData')
  
  # Train has labels/folds/covariateData
  expect_equal(names(splitData$Train), c('labels', 'folds', 'covariateData'))
  
  # Test has labels/covariateData
  expect_equal(names(splitData$Test), c('labels', 'covariateData'))
  
  # check attributes for Train
  expect_equal(attr(splitData$Train, "metaData")$outcomeId, attr(populationT, "metaData")$outcomeId)
  expect_equal(attr(splitData$Train, "metaData")$cohortId, plpData$metaData$databaseDetails$cohortId)
  expect_equal(
    attr(splitData$Train, "metaData")$cdmDatabaseSchema, 
    plpData$metaData$databaseDetails$cdmDatabaseSchema
    )
  
  expect_is(attr(splitData$Train, "metaData")$plpDataSettings, 'list')
  expect_equal(
    attr(splitData$Train, "metaData")$covariateSettings, 
    plpData$metaData$covariateSettings
  )
  expect_equal(
    attr(splitData$Train, "metaData")$populationSettings, 
    attr(populationT, "metaData")$populationSettings
  )
  expect_equal(
    attr(splitData$Train, "metaData")$attrition, 
    attr(populationT, "metaData")$attrition
  )
  
  expect_equal(
    attr(splitData$Train, "metaData")$splitSettings, 
    splitSettings
  )
  
  # train+test should be full data as train+test = 1
  expect_equal(
    nrow(splitData$Train$labels) + nrow(splitData$Test$labels), 
    nrow(populationT)
  )
  expect_equal(
    splitData$Train$covariateData$covariates %>% dplyr::tally() %>% dplyr::pull() + 
      splitData$Test$covariateData$covariates %>% dplyr::tally() %>% dplyr::pull(), 
    plpData$covariateData$covariates %>% dplyr::tally() %>% dplyr::pull()
  )
  
  # make sure nfolds is correct
  expect_equal(
    min(splitData$Train$folds$index),
    1
  )
  
  expect_equal(
    max(splitData$Train$folds$index),
    splitSettings$nfold
  )
  
  
  # check when test is 0
  splitSettings <- defaultSetting(
    testFraction = 0, 
    trainFraction = 1
    )
  
  splitData <- splitData(
    plpData = plpData,
    population = populationT,
    splitSettings = splitSettings
  )
  
  # should just have train
  expect_equal(names(splitData), c('Train'))
  
  # train labels should be the same size at the population
  expect_equal(
    nrow(splitData$Train$labels), 
    nrow(populationT)
  )
  expect_equal(
    splitData$Train$covariateData$covariates %>% dplyr::tally() %>% dplyr::pull(), 
    plpData$covariateData$covariates %>% dplyr::tally() %>% dplyr::pull()
  )
  
})

test_that("dataSummary works", {
  
  splitSettings <- defaultSetting(
    testFraction = 0, 
    trainFraction = 1
  )
  
  splitData <- splitData(
    plpData = plpData,
    population = populationT,
    splitSettings = splitSettings
  )

summaryPrint <- dataSummary(splitData)
expect_equal(summaryPrint, TRUE)
  
})


test_that("Data stratified splitting", {
  
  splitSettings <- defaultSetting(
    test=0.3, 
    nfold=3
    )

  # error due to insufficient outcomes
  DSpopulation1 <- data.frame(rowId=1:20, outcomeCount=c(1,1,1,1,rep(0,16))) 
  expect_error(randomSplitter(population = DSpopulation1, splitSettings = splitSettings))
  
  DSpopulation2 <- data.frame(rowId=1:200, outcomeCount=c(rep(1,42),rep(0,158)))
  splitSettings <- defaultSetting(
    train = 0.7,
    test = 0.3, 
    nfold = 4
  )
  # fold creation check 1 (fixed)
  test <- randomSplitter(population = DSpopulation2, splitSettings = splitSettings)
  test <- merge(DSpopulation2, test)
  test <- table(test$outcomeCount, test$index)
  test.returned <- paste(test, collapse='-')
  test.expected <- paste(matrix(c(47,28,28,28,27,12,8,8,7,7), ncol=5, byrow=T),collapse='-')
  expect_identical(test.returned, test.expected)
  
  # fold creation check 2 (sum)
  size <- 500
  DSpopulation3 <- data.frame(rowId=1:size, outcomeCount=c(rep(1,floor(size/3)),rep(0,size-floor(size/3)))) 
  splitSettings <- defaultSetting(
    train = 0.8,
    test = 0.2, 
    nfold = 4
  )
  test <- randomSplitter(population = DSpopulation3, splitSettings = splitSettings) 
  test <- merge(DSpopulation3, test)
  test <- table(test$outcomeCount, test$index)
  expect_that(sum(test), equals(size))
  
  # test the training fraction parameter for learning curves
  size = 500
  DSpopulation4 <- data.frame(rowId=1:size,
                            outcomeCount=c(rep(1,floor(size/3)),
                                           rep(0,size-floor(size/3))))
  splitSettings <- defaultSetting(
    train = 0.4,
    test = 0.2, 
    nfold = 4
  )
  test <- randomSplitter(population = DSpopulation4, splitSettings = splitSettings)

  tolerance = 5
  excludedPatients = 200
  # test, if the number of patients in each fold are roughly the same
  expect_equal(length(test$index[test$index == 1]),
               length(test$index[test$index == 3]),
               tolerance = tolerance)
  expect_equal(length(test$index[test$index == 2]),
               length(test$index[test$index == 4]),
               tolerance = tolerance)
  expect_equal(length(test$index[test$index == 1]),
               length(test$index[test$index == 4]),
               tolerance = tolerance)
  # test, if patients were excluded according to the training fraction
  expect_equal(length(test$index[test$index == 0]),
               excludedPatients)
})

test_that("Data splitting by time", {
  
  # fold creation check (sum)
  size <- 500
  set.seed(1)
  DSpopulation2 <- data.frame(
    rowId=1:size, 
    outcomeCount=sample(0:1,size,replace=TRUE),
    cohortStartDate = as.Date("2010-01-01") + c(1:size)
    )
  splitSettings <- defaultSetting(
    train = 0.8,
    test = 0.2, 
    nfold = 4
  )
  
  test <- timeSplitter(population = DSpopulation2, splitSettings = splitSettings) 
  test <- merge(DSpopulation2, test)
  test <- table(test$outcomeCount, test$index)
  expect_that(sum(test), equals(size))
  
  # test the training fraction parameter for learning curves
  size <- 500
  set.seed(1)
  DSpopulation3 <- data.frame(rowId=1:size,
                            outcomeCount=sample(0:1,size,replace=TRUE),
                            cohortStartDate = as.Date("2010-01-01") + c(1:size))
  splitSettings <- defaultSetting(
    train = 0.4,
    test = 0.2, 
    nfold = 4
  )
  test <- timeSplitter(population = DSpopulation3, splitSettings = splitSettings)
  
  tolerance = 5
  excludedPatients = 196
  # test, if the number of patients in each fold are roughly the same
  expect_equal(length(test$index[test$index == 1]),
               length(test$index[test$index == 3]),
               tolerance = tolerance)
  expect_equal(length(test$index[test$index == 2]),
               length(test$index[test$index == 4]),
               tolerance = tolerance)
  expect_equal(length(test$index[test$index == 1]),
               length(test$index[test$index == 4]),
               tolerance = tolerance)
  # test, if patients were excluded according to the training fraction
  expect_equal(length(test$index[test$index == 0]),
               excludedPatients)
  
})



test_that("Data splitting by subject", {
# error message checks
  DSpopulation1 <- data.frame(rowId=1:20, subjectId = 1:20, outcomeCount=c(1,1,1,1,rep(0,16))) 
  splitSettings <- defaultSetting(
    train = 0.7,
    test = 0.3, 
    nfold = 3
  )
  expect_error(subjectSplitter(population = DSpopulation1, splitSettings = splitSettings ))

  DSpopulation2 <- data.frame(rowId=1:200,subjectId = 1:200, outcomeCount=c(rep(1,42),rep(0,158)))
  splitSettings <- defaultSetting(
    train = 0.8,
    test = 0.2, 
    nfold = 4
  )
  test <- subjectSplitter(population = DSpopulation2, splitSettings = splitSettings)
  test <- merge(DSpopulation2, test)
  test <- table(test$outcomeCount, test$index)
  test.returned <- paste(test, collapse='-')
  test.expected <- paste(matrix(c(32,32,32,31,31,8,9,9,8,8), ncol=5, byrow=T),collapse='-')
  expect_identical(test.returned, test.expected)

# test that people are not in multiple folds
  DSpopulation3 <- data.frame(rowId=1:200,subjectId = rep(1:50,4), outcomeCount=c(rep(1,42),rep(0,158)))
  splitSettings <- defaultSetting(
    train = 0.75,
    test = 0.25, 
    nfold = 3
  )
  test <- subjectSplitter(population = DSpopulation3, splitSettings  = splitSettings )
  test <- merge(DSpopulation3, test)

  expect_equal(unique(table(test$subjectId[test$index==-1])), 4)
  expect_equal(unique(table(test$subjectId[test$index==2])), 4)
  expect_equal(unique(table(test$subjectId[test$index==3])), 4)
  expect_equal(unique(table(test$subjectId[test$index==1])), 4)
  
# test that no subject is not assigned a fold
  expect_equal(sum(test$index==0), 0)


})
