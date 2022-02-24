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
context("Evaluation")

library("testthat")
library("pROC")
library("AUC")
library("scoring")
library("Metrics")
library("PRROC")


test_that("evaluatePlp", {
  eval <- evaluatePlp(
    prediction = plpResult$prediction,
    typeColumn = 'evaluationType'
    )
  testthat::expect_equal(class(eval), 'plpEvaluation')
  testthat::expect_equal(names(eval), c('evaluationStatistics', 'thresholdSummary', 'demographicSummary', 'calibrationSummary', 'predictionDistribution') )
})

test_that("modelBasedConcordance", {
  concordance <- PatientLevelPrediction::modelBasedConcordance(prediction = plpResult$prediction)
  testthat::expect_is(concordance, "numeric")
})

test_that("evaluatePlp_survival", {
  
  plpResultSurvivalPred <- data.frame(
    rowId = 1:300, 
    ageYear = sample(100, 300, replace = T),
    gender = sample(c('8507','8532'), 300, replace = T),
    outcomeCount = c(rep(1,40), rep(0,260)),
    value = runif(300),
    evaluationType = rep('Train', 300),
    survivalTime = sample(2000, 300, replace = T)
  )
  attr(plpResultSurvivalPred, "metaData")$modelType <- 'survival'
  attr(plpResultSurvivalPred, 'metaData')$timepoint <- 365
  
  eval <- evaluatePlp(
    prediction = plpResultSurvivalPred,
    typeColumn = 'evaluationType'
    )
  testthat::expect_equal(class(eval), 'plpEvaluation')
  testthat::expect_true(5==sum(names(eval) %in% c('evaluationStatistics', 'demographicSummary', 'calibrationSummary', 'thresholdSummary', 'predictionDistribution') ))
})

test_that("AUROC", {
  Eprediction <- data.frame(value= runif(100), outcomeCount = round(runif(100)))
  attr(Eprediction, "metaData") <- list(modelType = "binary")
  proc.auc <- pROC::roc(Eprediction$outcomeCount, Eprediction$value, algorithm = 3,
                        direction="<")
  tolerance <- 0.001

  plpAUC <- computeAuc(Eprediction, confidenceInterval = FALSE)
  expect_equal(as.numeric(proc.auc$auc), plpAUC, tolerance = tolerance)
  
  
})

test_that("AUPRC", {
  Eprediction <- data.frame(value= runif(100), outcomeCount = round(runif(100)))
  
  positive <- Eprediction$value[Eprediction$outcomeCount == 1]
  negative <- Eprediction$value[Eprediction$outcomeCount == 0]
  pr <- PRROC::pr.curve(scores.class0 = positive, scores.class1 = negative)
  auprc <- pr$auc.integral
  
  # area under precision-recall curve must be between 0 and 1
  expect_gte(auprc, 0)
  expect_lte(auprc, 1)
})

test_that("Brierscore", {
  Eprediction <- data.frame(value= runif(100), outcomeCount = round(runif(100)))

  Eprediction$dummy <- 1
  brier.scoring <- scoring::brierscore(outcomeCount ~ value, data=Eprediction, group='dummy')$brieravg
  brier.plp <- brierScore(Eprediction)$brier
  expect_that(as.double(brier.scoring), equals(brier.plp))
})

test_that("Average precision", {
  Eprediction <- data.frame(value= runif(100), outcomeCount = round(runif(100)))
  
  aveP.metrics <- Metrics::apk(nrow(Eprediction), 
                               which(Eprediction$outcomeCount==1), (1:nrow(Eprediction))[order(-Eprediction$value)])
  aveP.plp <- averagePrecision(Eprediction)
  expect_that(as.double(aveP.metrics), equals(aveP.plp))
})






test_that("Calibration metrics", {
  Eprediction <- data.frame(rowId=1:100,
                           value= c(rep(0,50),rep(1,50)), 
                           outcomeCount =c(rep(0,50),rep(1,50)))
  # test the output 
  calibrationTest1 <- calibrationLine(Eprediction,numberOfStrata=2)
  expect_that(calibrationTest1$lm['Intercept'],  is_equivalent_to(0))
  expect_that(calibrationTest1$lm['Gradient'],  is_equivalent_to(1))
  expect_that(nrow(calibrationTest1$aggregateLmData)==2, equals(T))
  
  # should return - need to test all three
  ##lm # has the 'Intercept' and 'Gradient'
  ##aggregateLmData # obs vs pred for groups
  ##hosmerlemeshow # hosmerlemeshow value
  Eprediction2 <- data.frame(rowId=1:100,
                           value= c(0.1+runif(50)*0.9,runif(50)*0.6), 
                           outcomeCount =c(rep(1,50),rep(0,50)))
  
  hs.exist2 <- ResourceSelection::hoslem.test(Eprediction2$outcomeCount, 
                                              Eprediction2$value, g=10)
  calibrationTest2 <- calibrationLine(Eprediction2,numberOfStrata=10)
  #  test plp values vs ResourceSelection::hoslem.test 
  expect_that(calibrationTest2$hosmerlemeshow['Xsquared'],  
              is_equivalent_to(hs.exist2$statistic))  
  expect_that(calibrationTest2$hosmerlemeshow['df'],  
              is_equivalent_to(hs.exist2$parameter))  
  expect_that(calibrationTest2$hosmerlemeshow['pvalue'],  
              is_equivalent_to(hs.exist2$p.value)) 
  
  })

  # TODO: test pref scores 
  # test computePreferenceScore(prediction)
 
  #############################################################################
  
  
  


