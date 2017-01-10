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

context("Multi-Analyses")

test_that("prediction", {
  
  # covaraiteSetting1
  covaraiteSetting1 <- FeatureExtraction::createCovariateSettings(useCovariateDemographics = T,
                                                                  useCovariateDemographicsGender = F,
                                                                  useCovariateDemographicsAge = F,
                                                                  useCovariateDrugExposure = T,
                                                                  useCovariateDrugExposure30d = T, 
                                                                  excludedCovariateConceptIds = c(13,20), 
                                                                  includedCovariateConceptIds = c(),
                                                                  deleteCovariatesSmallCount = 10)
  
  # covaraiteSetting2
  covaraiteSetting2 <- FeatureExtraction::createCovariateSettings(useCovariateDemographics = T,
                                                                  useCovariateDemographicsGender = F,
                                                                  useCovariateDemographicsAge = T,
                                                                  useCovariateDrugExposure = F,
                                                                  useCovariateDrugExposure30d = F, 
                                                                  useCovariateConditionOccurrence = T,
                                                                  useCovariateConditionOccurrence365d = T,
                                                                  excludedCovariateConceptIds = c(13,16), 
                                                                  includedCovariateConceptIds = c(19),
                                                                  deleteCovariatesSmallCount = 50)
  
  # covaraiteSetting3
  covaraiteSetting3 <- FeatureExtraction::createCovariateSettings(useCovariateDemographics = T,
                                                                  useCovariateDemographicsGender = F,
                                                                  useCovariateDemographicsAge = F,
                                                                  useCovariateDrugExposure = T,
                                                                  useCovariateDrugExposure30d = T, 
                                                                  useCovariateConditionOccurrence = T,
                                                                  useCovariateConditionOccurrence365d = T,
                                                                  excludedCovariateConceptIds = c(), 
                                                                  includedCovariateConceptIds = c(20,18),
                                                                  deleteCovariatesSmallCount = 5)
  
  #=================================
  # checking the superset covaraite function - takes list of covariateSettings
  supCovSet <- supersetCovariates(list(covaraiteSetting1,covaraiteSetting2,covaraiteSetting3))
  
  # 1) check the minimum count is min of all - 5
  testthat::expect_that(supCovSet$deleteCovariatesSmallCount, is_equivalent_to(5))
  
  # 2) check the excludedCovariateConceptIds is empty
  testthat::expect_that(supCovSet$excludedCovariateConceptIds, is_equivalent_to(c()))
  
  # 3) check the includedCovariateConceptIds is empty
  testthat::expect_that(sum(supCovSet$includedCovariateConceptIds%in%c(19,20,18)), is_equivalent_to(3))
  
  # 4) check the useCovariateDrugExposure30d is T
  testthat::expect_that(supCovSet$useCovariateDrugExposure30d, is_equivalent_to(T))
  
  # 5) check the useCovariateConditionOccurrence365d is T
  testthat::expect_that(supCovSet$useCovariateConditionOccurrence365d, is_equivalent_to(T))
  
  # 5) check the useCovariateConditionOccurrence30d is F
  testthat::expect_that(supCovSet$useCovariateConditionOccurrence30d, is_equivalent_to(F))
  
  #=================================
  
  # checking the function to restrict from lots of covariates to just the selected ones
  # generate simulated data:
  set.seed(1234)
  data(plpDataSimulationProfile)
  sampleSize <- 2000
  plpData <- simulatePlpData(plpDataSimulationProfile, n = sampleSize)
  
  #debug(PatientLevelPrediction:::restrictCovariates)
  firstCov <- PatientLevelPrediction:::restrictCovariates(plpData, covaraiteSetting1)
  
  # check max size:
  oneVals <- ff::as.ff(rep(1, length(firstCov$covariates$covariateValue)))
  grp_qty <- bySumFf(oneVals, firstCov$covariates$covariateId)
  testthat::expect_that( min(ff::as.ram(grp_qty$sums)) >= 10, equals(T))
  
  secCov <- PatientLevelPrediction:::restrictCovariates(plpData, covaraiteSetting2)
  # check inclusion - 17
  idx <- ffbase::ffmatch(x = secCov$covariates$covariateId, table = ff::as.ff(19))
  ind <- ff::as.ram(ffbase::ffwhich(idx, !is.na(idx)))
  testthat::expect_that( length(ind)>0 , equals(T))
  
  # check exclusion - 13,16
  idx <- ffbase::ffmatch(x = secCov$covariates$covariateId, table = ff::as.ff(c(13,16)))
  ind <- ff::as.ram(ffbase::ffwhich(idx, !is.na(idx)))
  testthat::expect_that( length(ind) , equals(0))
  
  # check covariates: (ConditionOccurrence365d 101, Age 4)
  idx <- ffbase::ffmatch(x = secCov$covariateRef$analysisId, table = ff::as.ff(c(101)))
  ind <- ff::as.ram(ffbase::ffwhich(idx, !is.na(idx)))
  testthat::expect_that( length(ind)>0 , equals(T))
  idx <- ffbase::ffmatch(x = secCov$covariateRef$analysisId, table = ff::as.ff(c(4)))
  ind <- ff::as.ram(ffbase::ffwhich(idx, !is.na(idx)))
  testthat::expect_that( length(ind)>0 , equals(T))
  
  idx <- ffbase::ffmatch(x = secCov$covariateRef$analysisId, table = ff::as.ff(c(6)))
  ind <- ff::as.ram(ffbase::ffwhich(idx, !is.na(idx)))
  testthat::expect_that( length(ind)==0 , equals(T))
  
  #==================================
  
 # checking the set tar parameters
  tar <- setTimeAtRisk(includeAllOutcomes = T,
                       firstExposureOnly = F,
                       washoutPeriod = 365,
                       removeSubjectsWithPriorOutcome = T,
                       priorOutcomeLookback = 99999,
                       riskWindowStart = 1,
                       addExposureDaysToStart = F,
                       riskWindowEnd = 366,
                       addExposureDaysToEnd = F,
                       requireTimeAtRisk = T 
  )
  
  testthat::expect_that( tar$washoutPeriod , equals(365))
  testthat::expect_that( tar$riskWindowStart , equals(1))
  testthat::expect_that( tar$riskWindowEnd , equals(366))

  })


