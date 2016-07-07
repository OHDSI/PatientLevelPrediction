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

# This is a test of the creation of the study population. It checks expected errors
set.seed(1234)
data(plpDataSimulationProfile)
sampleSize <- 2000
plpData <- PatientLevelPrediction::simulatePlpData(plpDataSimulationProfile, n = sampleSize)

test_that("population creation parameters", {
  
  studyPopulation <- createStudyPopulation(plpData,
                        population = NULL,
                        outcomeId,
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
                        silent=F,...)

  #plpData = plpData
  expect_is(studyPopulation, "plpData")
  
    
  
  #washoutPeriod >=0
  expect_error(
                createStudyPopulation(plpData,
                                population = NULL,
                                outcomeId,
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
                                silent=F,...)
  )
  
  #priorOutcomeLookback >=0
  expect_error(
    createStudyPopulation(plpData,
                          population = NULL,
                          outcomeId,
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
                          silent=F,...)
  )
  
  #minTimeAtRisk >=0
  expect_error(
    createStudyPopulation(plpData,
                          population = NULL,
                          outcomeId,
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
                          silent=F,...)
  )
  
  #minTimeAtRisk >=0
  expect_error(
    createStudyPopulation(plpData,
                          population = NULL,
                          outcomeId,
                          binary = T,
                          firstExposureOnly = FALSE,
                          washoutPeriod = 0,
                          removeSubjectsWithPriorOutcome = TRUE,
                          priorOutcomeLookback = 99999,
                          requireTimeAtRisk = TRUE,
                          minTimeAtRisk=-1,
                          riskWindowStart = 0,
                          addExposureDaysToStart = FALSE,
                          riskWindowEnd = 365,
                          addExposureDaysToEnd = FALSE,
                          silent=F,...)
  )
  
  
})


