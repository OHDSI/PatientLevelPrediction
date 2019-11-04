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

context("Prediction")

# this no longer checks predictions as models don't exist and take too long to train during test
# generate simulated data:
set.seed(1234)
data(plpDataSimulationProfile)
sampleSize <- 2000
plpData <- simulatePlpData(plpDataSimulationProfile, n = sampleSize)

# create popualtion for outcome 2
population <- createStudyPopulation(plpData,
                                    outcomeId = 2,
                                    firstExposureOnly = FALSE,
                                    washoutPeriod = 0,
                                    removeSubjectsWithPriorOutcome = FALSE,
                                    priorOutcomeLookback = 99999,
                                    requireTimeAtRisk = FALSE,
                                    minTimeAtRisk=0,
                                    riskWindowStart = 0,
                                    addExposureDaysToStart = FALSE,
                                    riskWindowEnd = 365,
                                    addExposureDaysToEnd = FALSE
                                    #,verbosity=INFO
)

index <- PatientLevelPrediction::randomSplitter(population, test=0.2, seed=1)
population <- merge(population, index)
colnames(population)[colnames(population)=='index'] <- 'indexes'

test_that("prediction inputs", {
  #=====================================
  # check prediction
  #=====================================
  testthat::expect_error(PatientLevelPrediction::predictPlp(model=NULL, population=population, 
                                                            plpData=plpData, index=NULL))
  testthat::expect_error(PatientLevelPrediction::predictPlp(model=list(), population=NULL, 
                                                            plpData=plpData, index=NULL))
  testthat::expect_error(PatientLevelPrediction::predictPlp(model=list(), population=population, 
                                                            plpData=NULL, index=NULL))
  

  })

# predict.*
test_that("predictProbabilities inputs", {
 # predictProbabilities
  
  testthat::expect_error(predictProbabilities(predictiveModel=list(modelType='notreal'), 
                       population=population, 
                       covariates=NULL) )
  
})


test_that("predictFfdf inputs", {
  # predictFfdf
  testthat::expect_error(predictFfdf(coefficients=NULL, 
                                     population=population, 
                                     covariates=NULL, 
                                     modelType = "madeup"))
  
})

N <- sample(10,1)
values <- ff::as.ff(runif(N))
bins <- ff::as.ff(sample(100,N))

test_that("bySumFf inputs", {
  testthat::expect_equal(nrow(bySumFf(values, bins)),N)
  
  testthat::expect_equal(bySumFf(values, bins)[,2],
                         ff::as.ram(values)[order(ff::as.ram(bins))])
  
})

