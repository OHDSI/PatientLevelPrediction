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

context("Plotting")
#TODO: add input checks and test these...
#options(fftempdir = getwd())
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
lr_model <- PatientLevelPrediction::setLassoLogisticRegression()
lr_results <- tryCatch(runPlp(population = population, plpData = plpData, 
                              verbosity = 'NONE',
                              modelSettings = lr_model, savePlpData = F,
                              savePlpResult = F, savePlpPlots =  F, 
                              saveEvaluation = F,
                              testSplit='stratified', # this splits by person
                              testFraction=0.25,
                              nfold=2))

test_that("plots", {

  # test all the outputs are ggplots
  test <- plotRoc(lr_results$prediction)
  testthat::expect_s3_class(test, 'ggplot')

  test <- plotSparseRoc(lr_results$performanceEvaluation)
  testthat::expect_s3_class(test, 'ggplot')
  
  test <- plotPredictedPDF(lr_results$performanceEvaluation)
  testthat::expect_s3_class(test, 'ggplot')
  
  test <- plotPreferencePDF(lr_results$performanceEvaluation)
  testthat::expect_s3_class(test, 'ggplot')
  
  test <- plotPrecisionRecall(lr_results$performanceEvaluation)
  testthat::expect_s3_class(test, 'ggplot')
  
  test <- plotF1Measure(lr_results$performanceEvaluation)
  testthat::expect_s3_class(test, 'ggplot')
  
  if(!is.null(lr_results$performanceEvaluation$demographicSummary)){
    test <- plotDemographicSummary(lr_results$performanceEvaluation)
    testthat::expect_s3_class(test, 'ggplot')
  }
  
  test <- plotSparseCalibration(lr_results$performanceEvaluation)
  testthat::expect_s3_class(test, 'ggplot')
  
  test <- plotPredictionDistribution(lr_results$performanceEvaluation)
  testthat::expect_s3_class(test, 'ggplot')
  
  test <- plotVariableScatterplot(lr_results$covariateSummary)
  testthat::expect_s3_class(test, 'ggplot')
  
  test <- plotGeneralizability(lr_results$covariateSummary)
  testthat::expect_s3_class(test, 'grob')

})


test_that("outcomeSurvivalPlot", {
  
  # test the plot works
  test <- outcomeSurvivalPlot(plpData = plpData, outcomeId = 2)
  testthat::expect_s3_class(test, 'ggsurvplot')
  
  testthat::expect_error(outcomeSurvivalPlot())
  testthat::expect_error(outcomeSurvivalPlot(plpData = NULL))
  testthat::expect_error(outcomeSurvivalPlot(outcomeId = 094954))
})

  
