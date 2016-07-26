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
                        firstExposureOnly = FALSE,
                        washoutPeriod = 0,
                        removeSubjectsWithPriorOutcome = FALSE,
                        priorOutcomeLookback = 99999,
                        requireTimeAtRisk = FALSE,
                        minTimeAtRisk=0,
                        riskWindowStart = 0,
                        addExposureDaysToStart = FALSE,
                        riskWindowEnd = 365,
                        addExposureDaysToEnd = FALSE,
                        verbosity = FATAL)

  #plpData = plpData
  expect_is(studyPopulation, "data.frame")
  
  nrOutcomes1 <- sum(studyPopulation$outcomeCount) 
  expect_gt(nrOutcomes1,0)
  
  #firstExposureOnly test (should have no effect on simulated data)
  studyPopulation <- createStudyPopulation(plpData,
                                           outcomeId = 3,
                                           binary = TRUE,
                                           firstExposureOnly = TRUE,
                                           washoutPeriod = 0,
                                           removeSubjectsWithPriorOutcome = FALSE,
                                           priorOutcomeLookback = 99999,
                                           requireTimeAtRisk = FALSE,
                                           minTimeAtRisk=0,
                                           riskWindowStart = 0,
                                           addExposureDaysToStart = FALSE,
                                           riskWindowEnd = 365,
                                           addExposureDaysToEnd = FALSE,
                                           verbosity = FATAL)
  
  nrOutcomes2 <- sum(studyPopulation$outcomeCount)
  expect_gt(nrOutcomes2,0)
  expect_equal(nrOutcomes1,nrOutcomes2)
  
  #requireTimeAtRisk
  studyPopulation <- createStudyPopulation(plpData,
                                           outcomeId = 3,
                                           binary = TRUE,
                                           firstExposureOnly = TRUE,
                                           washoutPeriod = 0,
                                           removeSubjectsWithPriorOutcome = FALSE,
                                           priorOutcomeLookback = 99999,
                                           requireTimeAtRisk = TRUE,
                                           minTimeAtRisk=365,
                                           riskWindowStart = 0,
                                           addExposureDaysToStart = FALSE,
                                           riskWindowEnd = 365,
                                           addExposureDaysToEnd = FALSE,
                                           verbosity = FATAL)
  nrOutcomes3 <- sum(studyPopulation$outcomeCount)
  expect_gt(nrOutcomes3,0)
  expect_false(nrOutcomes3 == nrOutcomes1) 
  
  #washoutPeriod
  studyPopulation <- createStudyPopulation(plpData,
                                           outcomeId = 3,
                                           binary = TRUE,
                                           firstExposureOnly = TRUE,
                                           washoutPeriod = 365,
                                           removeSubjectsWithPriorOutcome = FALSE,
                                           priorOutcomeLookback = 99999,
                                           requireTimeAtRisk = FALSE,
                                           minTimeAtRisk=365,
                                           riskWindowStart = 0,
                                           addExposureDaysToStart = FALSE,
                                           riskWindowEnd = 365,
                                           addExposureDaysToEnd = FALSE,
                                           verbosity = FATAL)
  nrOutcomes4 <- sum(studyPopulation$outcomeCount)
  expect_gt(nrOutcomes4,0)
  expect_false(nrOutcomes4 == nrOutcomes1) 
  
  #washoutPeriod >=0
  expect_error(
                createStudyPopulation(plpData,
                                population = NULL,
                                outcomeId = 3,
                                binary = TRUE,
                                firstExposureOnly = FALSE,
                                washoutPeriod = -1,
                                removeSubjectsWithPriorOutcome = TRUE,
                                priorOutcomeLookback = 99999,
                                requireTimeAtRisk = TRUE,
                                minTimeAtRisk=365,
                                riskWindowStart = 0,
                                addExposureDaysToStart = FALSE,
                                riskWindowEnd = 365,
                                addExposureDaysToEnd = FALSE,
                                verbosity = FATAL)
  )
  
  #priorOutcomeLookback >=0
  expect_error(
    createStudyPopulation(plpData,
                          population = NULL,
                          outcomeId = 3,
                          binary = TRUE,
                          firstExposureOnly = FALSE,
                          washoutPeriod = 0,
                          removeSubjectsWithPriorOutcome = TRUE,
                          priorOutcomeLookback = -1,
                          requireTimeAtRisk = TRUE,
                          minTimeAtRisk=365,
                          riskWindowStart = 0,
                          addExposureDaysToStart = FALSE,
                          riskWindowEnd = 365,
                          addExposureDaysToEnd = F,
                          verbosity = FATAL)
  )
  
  #minTimeAtRisk >=0
  expect_error(
    createStudyPopulation(plpData,
                          population = NULL,
                          outcomeId = 3,
                          binary = T,
                          firstExposureOnly = FALSE,
                          washoutPeriod = 0,
                          removeSubjectsWithPriorOutcome = TRUE,
                          priorOutcomeLookback = 99999,
                          requireTimeAtRisk = T,
                          minTimeAtRisk=-1,
                          riskWindowStart = 0,
                          addExposureDaysToStart = FALSE,
                          riskWindowEnd = 365,
                          addExposureDaysToEnd = F,
                          verbosity = FATAL)
  )
  
})


