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
context("Population")

# Test unit for the creation of the study population. The firstExposureOnly, 
# washout, requireTimeAtRisk are checked. Additionally, error messages are checked.

set.seed(1234)
data(plpDataSimulationProfile)
sampleSize <- 2000
plpData <- PatientLevelPrediction::simulatePlpData(plpDataSimulationProfile, n = sampleSize)

test_that("population creation parameters", {
  
  studyPopulation <- createStudyPopulation(plpData,
                        outcomeId = 3,
                        binary = TRUE,
                        includeAllOutcomes = F,
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

  #plpData = plpData
  expect_is(studyPopulation, "data.frame")
  
  nrOutcomes1 <- sum(studyPopulation$outcomeCount) 
  expect_gt(nrOutcomes1,0)
  
  #firstExposureOnly test (should have no effect on simulated data)
  studyPopulation <- createStudyPopulation(plpData,
                                           outcomeId = 3,
                                           binary = TRUE,
                                           includeAllOutcomes = F,
                                           firstExposureOnly = TRUE,
                                           washoutPeriod = 0,
                                           removeSubjectsWithPriorOutcome = FALSE,
                                           priorOutcomeLookback = 99999,
                                           requireTimeAtRisk = FALSE,
                                           minTimeAtRisk=0,
                                           riskWindowStart = 0,
                                           addExposureDaysToStart = FALSE,
                                           riskWindowEnd = 365,
                                           addExposureDaysToEnd = FALSE)
  
  nrOutcomes2 <- sum(studyPopulation$outcomeCount)
  expect_gt(nrOutcomes2,0)
  expect_equal(nrOutcomes1,nrOutcomes2)
  
  #requireTimeAtRisk
  studyPopulation <- createStudyPopulation(plpData,
                                           outcomeId = 3,
                                           binary = TRUE,
                                           includeAllOutcomes = F,
                                           firstExposureOnly = TRUE,
                                           washoutPeriod = 0,
                                           removeSubjectsWithPriorOutcome = FALSE,
                                           priorOutcomeLookback = 99999,
                                           requireTimeAtRisk = TRUE,
                                           minTimeAtRisk=365,
                                           riskWindowStart = 0,
                                           addExposureDaysToStart = FALSE,
                                           riskWindowEnd = 365,
                                           addExposureDaysToEnd = FALSE)
  nrOutcomes3 <- sum(studyPopulation$outcomeCount)
  expect_gt(nrOutcomes3,0)
  expect_false(nrOutcomes3 == nrOutcomes1) 
  
  #washoutPeriod
  studyPopulation <- createStudyPopulation(plpData,
                                           outcomeId = 3,
                                           binary = TRUE,
                                           includeAllOutcomes = F,
                                           firstExposureOnly = TRUE,
                                           washoutPeriod = 365,
                                           removeSubjectsWithPriorOutcome = FALSE,
                                           priorOutcomeLookback = 99999,
                                           requireTimeAtRisk = FALSE,
                                           minTimeAtRisk=365,
                                           riskWindowStart = 0,
                                           addExposureDaysToStart = FALSE,
                                           riskWindowEnd = 365,
                                           addExposureDaysToEnd = FALSE)
  nrOutcomes4 <- sum(studyPopulation$outcomeCount)
  expect_gt(nrOutcomes4,0)
  expect_false(nrOutcomes4 == nrOutcomes1) 
  
  #washoutPeriod >=0
  expect_error(
                createStudyPopulation(plpData,
                                population = NULL,
                                outcomeId = 3,
                                binary = TRUE,
                                includeAllOutcomes = F,
                                firstExposureOnly = FALSE,
                                washoutPeriod = -1,
                                removeSubjectsWithPriorOutcome = TRUE,
                                priorOutcomeLookback = 99999,
                                requireTimeAtRisk = TRUE,
                                minTimeAtRisk=365,
                                riskWindowStart = 0,
                                addExposureDaysToStart = FALSE,
                                riskWindowEnd = 365,
                                addExposureDaysToEnd = FALSE)
  )
  
  #priorOutcomeLookback >=0
  expect_error(
    createStudyPopulation(plpData,
                          population = NULL,
                          outcomeId = 3,
                          binary = TRUE,
                          includeAllOutcomes = F,
                          firstExposureOnly = FALSE,
                          washoutPeriod = 0,
                          removeSubjectsWithPriorOutcome = TRUE,
                          priorOutcomeLookback = -1,
                          requireTimeAtRisk = TRUE,
                          minTimeAtRisk=365,
                          riskWindowStart = 0,
                          addExposureDaysToStart = FALSE,
                          riskWindowEnd = 365,
                          addExposureDaysToEnd = F)
  )
  
  #minTimeAtRisk >=0
  expect_error(
    createStudyPopulation(plpData,
                          population = NULL,
                          outcomeId = 3,
                          binary = T,
                          includeAllOutcomes = F,
                          firstExposureOnly = FALSE,
                          washoutPeriod = 0,
                          removeSubjectsWithPriorOutcome = TRUE,
                          priorOutcomeLookback = 99999,
                          requireTimeAtRisk = T,
                          minTimeAtRisk=-1,
                          riskWindowStart = 0,
                          addExposureDaysToStart = FALSE,
                          riskWindowEnd = 365,
                          addExposureDaysToEnd = F)
  )
  
  
  # check outcomes that only have partial timeatrisk are included:
  
  outcomes <- data.frame(rowId= c(1,1,1,4,5), 
                         outcomeId=c(1,1,1,1,2), 
                         outcomeCount=rep(1,5),
                         daysToEvent=c(-30,30,180,60,4)
  )
  cohorts <- data.frame(rowId=1:20, 
                        subjectId=1:20, 
                        cohortId=rep(2,20),
                        time=rep(365,20),
                        cohortStartDates=rep('2012-04-12',20),
                        daysFromObsStart=rep(740,20),
                        daysToCohortEnd=rep(1,20),
                        daysToObsEnd=c(40, rep(900,19))
                        )
  plpData2 <- plpData
  plpData2$outcomes <- outcomes
  plpData2$cohorts <- cohorts
  
  attr(plpData2$cohorts, "metaData") <- list(attrition=data.frame(outcomeId=1,description='test',
                                                                  targetCount=20,uniquePeople=20,
                                                                  outcomes=3))
  
  pop <- createStudyPopulation(plpData2,
                        population = NULL,
                        outcomeId = 1,
                        binary = T,
                        includeAllOutcomes = T,
                        firstExposureOnly = FALSE,
                        washoutPeriod = 0,
                        removeSubjectsWithPriorOutcome = F,
                        priorOutcomeLookback = 99999,
                        requireTimeAtRisk = T,
                        minTimeAtRisk=365,
                        riskWindowStart = 0,
                        addExposureDaysToStart = F,
                        riskWindowEnd = 365,
                        addExposureDaysToEnd = F)
  
  # person 1 and 4 should be retruned
  expect_equal(pop$rowId[pop$outcomeCount>0], c(1,4))
  
  pop2 <- createStudyPopulation(plpData2,
                               population = NULL,
                               outcomeId = 1,
                               binary = T,
                               includeAllOutcomes = T,
                               firstExposureOnly = F,
                               washoutPeriod = 0,
                               removeSubjectsWithPriorOutcome = T,
                               priorOutcomeLookback = 99999,
                               requireTimeAtRisk = T,
                               minTimeAtRisk=365,
                               riskWindowStart = 0,
                               addExposureDaysToStart = F,
                               riskWindowEnd = 365,
                               addExposureDaysToEnd = F)
  
  # person 4 only as person 1 has it before
  expect_equal(pop2$rowId[pop2$outcomeCount>0], c(4))
  
  pop3 <- createStudyPopulation(plpData2,
                               population = NULL,
                               outcomeId = 1,
                               binary = T,
                               includeAllOutcomes = F,
                               firstExposureOnly = FALSE,
                               washoutPeriod = 0,
                               removeSubjectsWithPriorOutcome = F,
                               priorOutcomeLookback = 99999,
                               requireTimeAtRisk = T,
                               minTimeAtRisk=365,
                               riskWindowStart = 0,
                               addExposureDaysToStart = F,
                               riskWindowEnd = 365,
                               addExposureDaysToEnd = F)
  
  # 4 only should be retruned
  expect_equal(pop3$rowId[pop3$outcomeCount>0], c(4))
  
  
})


