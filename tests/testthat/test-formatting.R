# Copyright 2016 Observational Health Data Sciences and Informatics
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

test_that("Formatting", {
  
  # testing manually constructed data...
  covs <- ff::ffdf(rowId=ff::as.ff(c(1,1,1,2,4,4,4,6,6)), # 3 and 5 have nothing
                       covariateId=ff::ff(c(123,2002,10,123,2002,3,4,9,8)),
                       covariateValue=ff::ff(rep(1,9)))
  covref <- ff::ffdf(covariateId=ff::as.ff(c(c(123,2002,3,4,5,6,7,8,9,10))),
                     covariateName=ff::as.ff(1:10),
                     analysisId=ff::as.ff(rep(1,10)),
                     conceptId=ff::as.ff(1:10)) 
  cohorts <- data.frame(rowId=1:6,   
                        subjectId=1:6, 
                        cohortId=rep(1,6),
                        cohortStartDate= rep('2007-12-28 00:00:00.0',6),
                        daysFromObsStart= c(500,50,500,500,500,500),
                        daysToCohortEnd= rep(200,6),
                        daysToObsEnd=rep(200,6))
  outcomes <- data.frame(rowId=c(1,2), 
                         outcomeId=rep(2,2), 
                         daysToEvent=c(150,40))
  
  plpData <- list(cohorts=cohorts,
                  outcomes=outcomes,
                  covariates=covs,
                  covariateRef=covref)
  class(plpData) <- 'plpData'
  population <- createStudyPopulation(plpData=plpData,requireTimeAtRisk = F,
                                      outcomeId=2,riskWindowStart = 1,
                                      riskWindowEnd = 365)
  # test gbm coo to sparse matrix 
  sparseMat.test <- toSparseM(plpData,population, map=NULL, silent=T)
  matrix.real <- matrix(rep(0, 6*10), ncol=10)
  x <- c(1,1,1,2,4,4,4,6,6)
  y <- c(1,2,10,1,2,3,4,9,8)
  for(a in 1:9) matrix.real[x[a],y[a]] <- 1
  expect_that(as.matrix(sparseMat.test$data), is_equivalent_to(matrix.real))
  
  # test on population with missing people due to low prior obs - keeps them :)
  population2 <- createStudyPopulation(plpData=plpData,requireTimeAtRisk = F,
                                      outcomeId=2,riskWindowStart = 1,
                                      riskWindowEnd = 365,
                                      washoutPeriod = 100)
  sparseMat.test2 <- toSparseM(plpData,population2, map=NULL, silent=T)
  matrix.real2 <- matrix(rep(0, 6*10), ncol=10)
  x <- c(1,1,1,4,4,4,6,6)
  y <- c(1,2,10,2,3,4,9,8)
  for(a in 1:8) matrix.real2[x[a],y[a]] <- 1
  expect_that(as.matrix(sparseMat.test2$data), is_equivalent_to(matrix.real2))
  
  # now test the mapping on new people... (testing the prediciton mapping)
  covs2 <- ff::ffdf(rowId=ff::as.ff(c(1,6,3,3,4,5,5,6,6)), # 3 and 5 have nothing
                   covariateId=ff::ff(c(10,10,10,123,2002,123,4,123,8)),
                   covariateValue=ff::ff(rep(1,9)))
  plpData2 <- list(cohorts=cohorts,
                  outcomes=outcomes,
                  covariates=covs2,
                  covariateRef=covref)
  class(plpData2) <- 'plpData'
  population3 <- createStudyPopulation(plpData=plpData2,requireTimeAtRisk = F,
                                      outcomeId=2,riskWindowStart = 1,
                                      riskWindowEnd = 365)
  sparseMat.test3 <- toSparseM(plpData2,population3, map=sparseMat.test$map, silent=T)
  matrix.real3 <- matrix(rep(0, 6*10), ncol=10)
  x <- c(1,6,3,3,4,5,5,6,6)
  y <- c(10,10,10,1,2,1,4,1,8)
  for(a in 1:9) matrix.real3[x[a],y[a]] <- 1
  expect_that(as.matrix(sparseMat.test3$data), is_equivalent_to(matrix.real3))
  
  #==============================
  # test libsvm format
  plpData.lsvm <- convertToLibsvm(plpData,filePath=NULL,silent=F)

  # load the libsvm
  data.test <- e1071::read.matrix.csr(file.path(plpData.lsvm$covariates,'covariate.txt'))
  matrix.lsvm <- Matrix::as.matrix(data.test$x)

  #test the covaraite matrix:
  expect_that(as.matrix(matrix.lsvm), is_equivalent_to(matrix.real))
  # test the rows:
  data.rowid<- read.table(file.path(plpData.lsvm$covariates,'rowId.txt'))
  expect_that(nrow(data.rowid), is_equivalent_to(max(plpData$cohorts$rowId)))
  
  # test the class  'plpData.libsvm'
  expect_that(class(plpData.lsvm), is_equivalent_to('plpData.libsvm'))
  
  # test python new data for given covariateRef
  # predict lines: 124-140 (make into a function?)
  
  
})