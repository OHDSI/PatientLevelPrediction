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
context("Formatting")

createCovariateData <- function(){
  # testing manually constructed data...
  covs <- data.frame(rowId=c(1,1,1,2,4,4,4,6,6), # 3 and 5 have nothing
    covariateId=c(123,2002,10,123,2002,3,4,9,8),
    covariateValue=rep(1,9))
  covref <- data.frame(covariateId=c(123,2002,3,4,5,6,7,8,9,10),
    covariateName=1:10,
    analysisId=rep(1,10),
    conceptId=1:10) 
  
  covariateData <- Andromeda::andromeda()
  covariateData$covariates <- covs
  covariateData$covariateRef <- covref
  
  return(covariateData)
}


test_that("MapIds with no cohort", {
  
  covariateDataTest <- createCovariateData()
  
  mappings <- MapIds(
    covariateData = covariateDataTest,
    cohort = NULL,
    mapping = NULL
  )

  #rowMap, covariates, mapping, covariateRef  (no cohort as NULL)
  expect_true('rowMap' %in% names(mappings))
  expect_true('covariates' %in% names(mappings))
  expect_true('mapping' %in% names(mappings))
  expect_true('covariateRef' %in% names(mappings))
  expect_false('cohort' %in% names(mappings))
  
  # 4 rowIds in the data
  expect_equal(mappings$rowMap %>% dplyr::tally() %>% dplyr::pull(), 4)
  
  # some covariates not in data 5,6,7 so should be removed from covRef
  expect_equal(mappings$covariateRef %>% dplyr::tally() %>% dplyr::pull(), 7)
  
  correctCov <- mappings$covariateRef %>% dplyr::select(.data$covariateId) %>% dplyr::pull() %in% c(123,2002,10,3,4,9,8)
  expect_equal(sum(correctCov), length(correctCov))
  
})


test_that("MapIds with a cohort", {
  
  covariateDataTest <- createCovariateData()
  
  cohort <- data.frame(rowId=c(2,6), outcomeCount = c(1,0))
  
  mappings <- MapIds(
    covariateData = covariateDataTest,
    cohort = cohort,
    mapping = NULL
  )
  
  #rowMap, covariates, mapping, covariateRef  (no cohort as NULL)
  expect_true('rowMap' %in% names(mappings))
  expect_true('covariates' %in% names(mappings))
  expect_true('mapping' %in% names(mappings))
  expect_true('covariateRef' %in% names(mappings))
  expect_true('cohort' %in% names(mappings))
  
  # 4 rowIds in the data
  expect_equal(mappings$rowMap %>% dplyr::tally() %>% dplyr::pull(), 2)
  
  # no covariates should be lost
  expect_equal(mappings$covariates %>% dplyr::tally() %>% dplyr::pull(), 3)
  
  # some covariates not in data 5,6,7 so should be removed from covRef
  expect_equal(mappings$covariateRef %>% dplyr::tally() %>% dplyr::pull(), 3)
  
  correctCov <- mappings$covariateRef %>% dplyr::select(.data$covariateId) %>% dplyr::pull() %in% c(123,9,8)
  expect_equal(sum(correctCov), length(correctCov))
  
})

# switch of all messages
test_that("toSparseM", {
  
  cohorts <- data.frame(rowId=1:6,   
                        subjectId=1:6, 
                        cohortId=rep(1,6),
                        cohortStartDate= rep('2007-12-28 00:00:00.0',6),
                        daysFromObsStart= c(500,50,500,500,500,500),
                        daysToCohortEnd= rep(200,6),
                        daysToObsEnd=rep(200,6),
                        ageYear = rep(25, 6),
                        gender = rep(8507, 6))
  
  attr(cohorts, "metaData") <- list(attrition=data.frame(outcomeId=2,description='test',
                                                         targetCount=6,uniquePeople=6,
                                                         outcomes=2))
  
  outcomes <- data.frame(rowId=c(1,2), 
                         outcomeId=rep(2,2), 
                         daysToEvent=c(150,40))
  
  FplpData <- list(cohorts=cohorts,
                  outcomes=outcomes,
                  covariateData = createCovariateData())
  class(FplpData) <- 'plpData'
  Fpopulation <- data.frame(
    rowId = c(1,3:6),
    outcomeCount = c(1,0,0,0,0)
    )
  
  # test gbm coo to sparse matrix 
  sparseMat.test <- toSparseM(FplpData,Fpopulation, map=NULL)
  matrix.real <- matrix(rep(0, 5*7), ncol=7)
  x <- c(1,1,1,3,3,3,5,5)
  y <- c(1,2,3,2,4,5,6,7)
  for(a in 1:8) matrix.real[x[a],y[a]] <- 1
  expect_that(as.matrix(sparseMat.test$dataMatrix), is_equivalent_to(matrix.real))
  
  
  # check map works by permuting it and checking the result
  sparseMat.test$covariateMap <- sparseMat.test$covariateMap %>% dplyr::arrange(.data$covariateId)
  sparseMat.test$covariateMap$columnId <- 1:7
  withMapTest <- toSparseM(FplpData,Fpopulation, map=sparseMat.test$covariateMap)
  
  
  matrix.real <- matrix(rep(0, 5*7), ncol=7)
  x <- c(1,1,1,3,3,3,5,5)
  y <- c(6,7,5,7,1,2,3,4)
  for(a in 1:8) matrix.real[x[a],y[a]] <- 1
  expect_that(as.matrix(withMapTest$dataMatrix), is_equivalent_to(matrix.real))
  
  
  #==================================
  # check sizes using simulated data
  #==================================
# objects from helper-object.R
  test <- toSparseM(plpData,population, map=NULL)
  compTest <- as.matrix(test$dataMatrix)
  testthat::expect_equal(test$labels %>% dplyr::tally() %>% dplyr::pull(), length(population$rowId))
  testthat::expect_equal(nrow(compTest), length(population$rowId))
  testthat::expect_true(ncol(compTest) <= nrow(plpData$covariateData$covariateRef))
  testthat::expect_equal(ncol(compTest), test$covariateRef %>% dplyr::tally() %>% dplyr::pull())
  testthat::expect_equal(ncol(compTest), test$covariateMap %>% dplyr::tally() %>% dplyr::pull())
  
  
})

test_that("checkRam", {
  
  ramCheck <- checkRam(createCovariateData(), 0.8)
  expect_true(ramCheck)
  
})

formattingCovs <- Andromeda::andromeda()
formattingCovs$covariates <- data.frame(
  rowId = c(
    rep(1, 5), 
    rep(2, 3), 
    rep(4, 2),
    rep(50, 6)
  ),
  covariateId = c(
    c(1001, 1213104, 1233105, 1, 99),
    c(1001, 2, 99),
    c(1001, 4),
    c(1,99, 98, 2, 4,3)
  ),
  covariateValue = rep(1, 16)
)

formattingCovs$covariateRef <- data.frame(
  covariateId = c(1001, 1213104, 25,26, 1233105, 1, 99, 2,4,98,3),
  covariateName = c(paste0('Covariate_', 1:11))
  )

formattingcohort <- data.frame(
  rowId = c(1:4,50), 
  outcomeCount = c(1,1,0,0,0)
  )

formattingData <- list(
  labels = formattingcohort,
  covariateData = formattingCovs
)

test_that("testCorrectLables", {
  
  data <- toSparseM(plpData = formattingData)
  
  expect_equal(
    data.frame(
      outcomeCount = data$labels$outcomeCount,
      rowId = data$labels$rowId
      ), 
    data.frame(
      outcomeCount = c(1,1,0,0,0),
      rowId = 1:5
      )
    )
  
})
