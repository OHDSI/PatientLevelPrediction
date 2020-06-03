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
context("Formatting")

# switch of all messages
test_that("toSparseM", {
  
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
  
  cohorts <- data.frame(rowId=1:6,   
                        subjectId=1:6, 
                        cohortId=rep(1,6),
                        cohortStartDate= rep('2007-12-28 00:00:00.0',6),
                        daysFromObsStart= c(500,50,500,500,500,500),
                        daysToCohortEnd= rep(200,6),
                        daysToObsEnd=rep(200,6))
  
  attr(cohorts, "metaData") <- list(attrition=data.frame(outcomeId=2,description='test',
                                                         targetCount=6,uniquePeople=6,
                                                         outcomes=2))
  
  outcomes <- data.frame(rowId=c(1,2), 
                         outcomeId=rep(2,2), 
                         daysToEvent=c(150,40))
  
  FplpData <- list(cohorts=cohorts,
                  outcomes=outcomes,
                  covariateData=covariateData)
  class(FplpData) <- 'plpData'
  Fpopulation <- createStudyPopulation(plpData=FplpData,requireTimeAtRisk = F,
                                      outcomeId=2,riskWindowStart = 1,
                                      riskWindowEnd = 365)
  # test gbm coo to sparse matrix 
  sparseMat.test <- toSparseM(FplpData,Fpopulation, map=NULL)
  matrix.real <- matrix(rep(0, 6*10), ncol=10)
  x <- c(1,1,1,2,4,4,4,6,6)
  y <- c(1,2,10,1,2,3,4,9,8)
  for(a in 1:9) matrix.real[x[a],y[a]] <- 1
  expect_that(as.matrix(sparseMat.test$data), is_equivalent_to(matrix.real))
  
  # test on population with missing people due to low prior obs - keeps them :)
  Fpopulation2 <- createStudyPopulation(plpData=FplpData,requireTimeAtRisk = F,
                                       outcomeId=2,riskWindowStart = 1,
                                       riskWindowEnd = 365,
                                       washoutPeriod = 100
  )
  sparseMat.test2 <- toSparseM(FplpData,Fpopulation2, map=NULL)
  matrix.real2 <- matrix(rep(0, 6*10), ncol=10)
  x <- c(1,1,1,4,4,4,6,6)
  y <- c(1,2,10,2,3,4,9,8)
  for(a in 1:8) matrix.real2[x[a],y[a]] <- 1
  expect_that(as.matrix(sparseMat.test2$data), is_equivalent_to(matrix.real2))
  
  # now test the mapping on new people... (testing the prediciton mapping)
  covs2 <- data.frame(rowId=c(1,6,3,3,4,5,5,6,6), # 3 and 5 have nothing
                    covariateId=c(10,10,10,123,2002,123,4,123,8),
                    covariateValue=rep(1,9))
  covariateData <- Andromeda::andromeda()
  covariateData$covariates <- covs2
  covariateData$covariateRef <- covref
  
  FplpData2 <- list(cohorts=cohorts,
                   outcomes=outcomes,
                   covariateData=covariateData)
  attr(FplpData2$cohorts, "metaData") <- list(attrition=data.frame(outcomeId=2,description='test',
                                                                  targetCount=6,uniquePeople=6,
                                                                  outcomes=2))
  
  
  class(FplpData2) <- 'plpData'
  Fpopulation3 <- createStudyPopulation(plpData=FplpData2,requireTimeAtRisk = F,
                                       outcomeId=2,riskWindowStart = 1,
                                       riskWindowEnd = 365
  )
  sparseMat.test3 <- toSparseM(FplpData2,Fpopulation3, map=sparseMat.test$map)
  matrix.real3 <- matrix(rep(0, 6*10), ncol=10)
  x <- c(1,6,3,3,4,5,5,6,6)
  y <- c(10,10,10,1,2,1,4,1,8)
  for(a in 1:9) matrix.real3[x[a],y[a]] <- 1
  expect_that(as.matrix(sparseMat.test3$data), is_equivalent_to(matrix.real3))
  
  
  #=====================================
  # checking mapping
  #=====================================
  # test mapping with no existing map
  # make small dataset to test exact 
  covariateData2 <- Andromeda::andromeda()
  covariateData2$covariates <- data.frame(rowId=c(40,40,2),
                                    covariateId=c(34,21,21),
                                    covariateValue=rep(1,3))
  covariateData2$covariateRef <- data.frame(covariateId=c(21,34),
                                      covariateName=c('test1','test2'),
                                      analysisId=rep(1,2),
                                      conceptId=rep(1,2)
  )
  
  FplpDataExact <- list(cohorts=data.frame(rowId=c(100,2,40), cohortId=rep(1,3), 
                                          time=rep(700,3), daysFromObsStart=rep(700,3),
                                          daysToCohortEnd=rep(700,3), daysToObsEnd=rep(700,3)),
                       outcomes =data.frame(rowId=c(100), outcomeId=c(2), outcomeCount=c(1),
                                            daysToEvent=c(50)),
                      covariateData = covariateData2
  )
  attr(FplpDataExact$cohorts, "metaData") <- list(attrition=data.frame(outcomeId=2,description='test',
                                                                      targetCount=6,uniquePeople=6,
                                                                      outcomes=2))
  class(FplpDataExact) <- "plpData"
  FpopulationExact <- createStudyPopulation(FplpDataExact,
                                           outcomeId = 2,
                                           firstExposureOnly = FALSE,
                                           washoutPeriod = 0,
                                           removeSubjectsWithPriorOutcome = FALSE,
                                           priorOutcomeLookback = 99999,
                                           requireTimeAtRisk = FALSE,
                                           minTimeAtRisk=0,
                                           riskWindowStart = 0,
                                           startAnchor = 'cohort start',
                                           riskWindowEnd = 365,
                                           endAnchor = 'cohort start'
                                           #,verbosity=INFO
  )
  test <- toSparseM(FplpDataExact,FpopulationExact, map=NULL)
  compTest <- as.matrix(test$data)
  compReal <- matrix(rep(0, 100*2), ncol=2)
  compReal[40,1:2] <- 1
  compReal[2,1] <- 1
  
  testthat::expect_equivalent(compTest, compReal)
  
  # test on new data with old map:
  covariateData2 <- Andromeda::andromeda()
  covariateData2$covariates <- data.frame(rowId=c(47,26,26),
                                    covariateId=c(21,21,36),
                                    covariateValue=rep(1,3))
  covariateData2$covariateRef <- data.frame(covariateId=c(21,36),
                                      covariateName=c('test1','test3'),
                                      analysisId=rep(1,2),
                                      conceptId=rep(1,2))
  
  FplpDataExact2 <- list(cohorts=data.frame(rowId=c(1,26,47), cohortId=rep(1,3), 
                                           time=rep(700,3), daysFromObsStart=rep(700,3),
                                           daysToCohortEnd=rep(700,3), daysToObsEnd=rep(700,3)),
                        outcomes =data.frame(rowId=c(1), outcomeId=c(2), outcomeCount=c(1),
                                             daysToEvent=c(50)),
                        covariateData = covariateData2
  )
  attr(FplpDataExact2$cohorts, "metaData") <- list(attrition=data.frame(outcomeId=1,description='test',
                                                                       targetCount=20,uniquePeople=20,
                                                                       outcomes=3))
  class(FplpDataExact2) <- "plpData"
  FpopulationExact2 <- createStudyPopulation(FplpDataExact2,
                                            outcomeId = 2,
                                            firstExposureOnly = FALSE,
                                            washoutPeriod = 0,
                                            removeSubjectsWithPriorOutcome = FALSE,
                                            priorOutcomeLookback = 99999,
                                            requireTimeAtRisk = FALSE,
                                            minTimeAtRisk=0,
                                            riskWindowStart = 0,
                                            startAnchor = 'cohort start',
                                            riskWindowEnd = 365,
                                            endAnchor = 'cohort start'
                                            #,verbosity=INFO
  )
  
  test2 <- toSparseM(FplpDataExact2,FpopulationExact2, map=test$map)
  compTest2 <- compTest <- as.matrix(test2$data)
  compReal2 <- matrix(rep(0, 47*2), ncol=2)
  compReal2[c(26,47),1] <- 1
  testthat::expect_equivalent(compTest2, compReal2)
  
  #==================================
  # check sizes using simulated data
  #==================================
# objects from helper-object.R
  test <- toSparseM(plpData,population, map=NULL)
  compTest <- as.matrix(test$data)
  testthat::expect_equal(nrow(compTest), max(population$rowId))
  testthat::expect_equal(ncol(compTest), 
                         nrow(plpData$covariateData$covariateRef))
  testthat::expect_equal(ncol(compTest), nrow(test$map))
  
})




##[TODO] - ADD TESTS FOR SQL CREATION EXISTING AND PLP LOG REG MODELS...
