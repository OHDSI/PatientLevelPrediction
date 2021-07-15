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
context("Evaluation")

library("testthat")
library("pROC")
library("AUC")
library("scoring")
library("Metrics")
library("PRROC")


test_that("evaluatePlp", {
  eval <- evaluatePlp(prediction = plpResult$prediction, plpData = plpData)
  testthat::expect_equal(class(eval), 'plpEvaluation')
  testthat::expect_equal(names(eval), c('evaluationStatistics', 'thresholdSummary', 'demographicSummary', 'calibrationSummary', 'predictionDistribution') )
})

test_that("modelBasedConcordance", {
  concordance <- PatientLevelPrediction::modelBasedConcordance(prediction = plpResult$prediction)
  testthat::expect_is(concordance, "numeric")
})

test_that("evaluatePlp_survival", {
  eval <- evaluatePlp(prediction = plpResult2$prediction, plpData = plpData)
  testthat::expect_equal(class(eval), 'plpEvaluation')
  testthat::expect_equal(names(eval), c('evaluationStatistics', 'thresholdSummary', 'demographicSummary', 'calibrationSummary') )
})

test_that("AUROC", {
  Eprediction <- data.frame(value= runif(100), outcomeCount = round(runif(100)))
  attr(Eprediction, "metaData") <- list(predictionType = "binary")
  proc.auc <- pROC::roc(Eprediction$outcomeCount, Eprediction$value, algorithm = 3,
                        direction="<")
  auc.auc <- AUC::auc(AUC::roc(Eprediction$value, factor(Eprediction$outcomeCount)))
  tolerance <- 0.001
  expect_equal(as.numeric(proc.auc$auc), auc.auc, tolerance = tolerance)
  
  plpAUC <- computeAuc(Eprediction, confidenceInterval = FALSE)
  expect_equal(as.numeric(proc.auc$auc), plpAUC, tolerance = tolerance)
  
  plpAUCdf <- computeAucFromDataFrames(prediction = Eprediction$value,
                           status = Eprediction$outcomeCount,
                           modelType = "logistic")
  expect_equal(as.numeric(proc.auc$auc), plpAUCdf, tolerance = tolerance)
  
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


test_that("f1Score", {
  expect_that(f1Score(TP=0,TN=0,FN=0,FP=0), equals(NaN))
  expect_that(f1Score(TP=-1,TN=0,FN=0,FP=0),  throws_error())
  expect_that(f1Score(TP=1,TN=-1,FN=0,FP=0),  throws_error())
  expect_that(f1Score(TP=1,TN=3,FN=-1,FP=0),  throws_error())
  expect_that(f1Score(TP=1,TN=1,FN=5,FP=-1),  throws_error())
  expect_that(f1Score(TP=NULL,TN=0,FN=0,FP=0),  throws_error())
  expect_that(f1Score(TP=1,TN=NULL,FN=0,FP=0),  throws_error())
  expect_that(f1Score(TP=1,TN=3,FN=NULL,FP=0),  throws_error())
  expect_that(f1Score(TP=1,TN=1,FN=5,FP=NULL),  throws_error())
  expect_that(f1Score(TP=10,TN=3,FN=5,FP=5),  equals(0.6666667,tolerance  = 0.0001) )
})

test_that("accuracy", {
  expect_that(accuracy(TP=0,TN=0,FN=0,FP=0), equals(NaN))
  expect_that(accuracy(TP=-1,TN=0,FN=0,FP=0),  throws_error())
  expect_that(accuracy(TP=1,TN=-1,FN=0,FP=0),  throws_error())
  expect_that(accuracy(TP=1,TN=3,FN=-1,FP=0),  throws_error())
  expect_that(accuracy(TP=1,TN=1,FN=5,FP=-1),  throws_error())
  expect_that(accuracy(TP=NULL,TN=0,FN=0,FP=0),  throws_error())
  expect_that(accuracy(TP=1,TN=NULL,FN=0,FP=0),  throws_error())
  expect_that(accuracy(TP=1,TN=3,FN=NULL,FP=0),  throws_error())
  expect_that(accuracy(TP=1,TN=1,FN=5,FP=NULL),  throws_error())
  expect_that(accuracy(TP=10,TN=3,FN=5,FP=5), equals(13/23, tolerance  = 0.0001))
})

test_that("sensitivity", {
  expect_that(sensitivity(TP=0,TN=0,FN=0,FP=0), equals(NaN))
  expect_that(sensitivity(TP=-1,TN=0,FN=0,FP=0),  throws_error())
  expect_that(sensitivity(TP=1,TN=-1,FN=0,FP=0),  throws_error())
  expect_that(sensitivity(TP=1,TN=3,FN=-1,FP=0),  throws_error())
  expect_that(sensitivity(TP=1,TN=1,FN=5,FP=-1),  throws_error())
  expect_that(sensitivity(TP=NULL,TN=0,FN=0,FP=0),  throws_error())
  expect_that(sensitivity(TP=1,TN=NULL,FN=0,FP=0),  throws_error())
  expect_that(sensitivity(TP=1,TN=3,FN=NULL,FP=0),  throws_error())
  expect_that(sensitivity(TP=1,TN=1,FN=5,FP=NULL),  throws_error())
  expect_that(sensitivity(TP=10,TN=3,FN=5,FP=5), equals(10/(10+5),tolerance  = 0.0001))
})

test_that("falseNegativeRate", {
  expect_that(falseNegativeRate(TP=0,TN=0,FN=0,FP=0), equals(NaN))
  expect_that(falseNegativeRate(TP=-1,TN=0,FN=0,FP=0),  throws_error())
  expect_that(falseNegativeRate(TP=1,TN=-1,FN=0,FP=0),  throws_error())
  expect_that(falseNegativeRate(TP=1,TN=3,FN=-1,FP=0),  throws_error())
  expect_that(falseNegativeRate(TP=1,TN=1,FN=5,FP=-1),  throws_error())
  expect_that(falseNegativeRate(TP=NULL,TN=0,FN=0,FP=0),  throws_error())
  expect_that(falseNegativeRate(TP=1,TN=NULL,FN=0,FP=0),  throws_error())
  expect_that(falseNegativeRate(TP=1,TN=3,FN=NULL,FP=0),  throws_error())
  expect_that(falseNegativeRate(TP=1,TN=1,FN=5,FP=NULL),  throws_error())
  expect_that(falseNegativeRate(TP=10,TN=3,FN=5,FP=5), equals(5/(10+5), tolerance  = 0.0001))
})

test_that("falsePositiveRate", {
  expect_that(falsePositiveRate(TP=0,TN=0,FN=0,FP=0), equals(NaN))
  expect_that(falsePositiveRate(TP=-1,TN=0,FN=0,FP=0),  throws_error())
  expect_that(falsePositiveRate(TP=1,TN=-1,FN=0,FP=0),  throws_error())
  expect_that(falsePositiveRate(TP=1,TN=3,FN=-1,FP=0),  throws_error())
  expect_that(falsePositiveRate(TP=1,TN=1,FN=5,FP=-1),  throws_error())
  expect_that(falsePositiveRate(TP=NULL,TN=0,FN=0,FP=0),  throws_error())
  expect_that(falsePositiveRate(TP=1,TN=NULL,FN=0,FP=0),  throws_error())
  expect_that(falsePositiveRate(TP=1,TN=3,FN=NULL,FP=0),  throws_error())
  expect_that(falsePositiveRate(TP=1,TN=1,FN=5,FP=NULL),  throws_error())
  expect_that(falsePositiveRate(TP=10,TN=3,FN=5,FP=5), equals(5/(5+3), tolerance  = 0.0001))
})

test_that("specificity", {
  expect_that(specificity(TP=0,TN=0,FN=0,FP=0), equals(NaN))
  expect_that(specificity(TP=-1,TN=0,FN=0,FP=0),  throws_error())
  expect_that(specificity(TP=1,TN=-1,FN=0,FP=0),  throws_error())
  expect_that(specificity(TP=1,TN=3,FN=-1,FP=0),  throws_error())
  expect_that(specificity(TP=1,TN=1,FN=5,FP=-1),  throws_error())
  expect_that(specificity(TP=NULL,TN=0,FN=0,FP=0),  throws_error())
  expect_that(specificity(TP=1,TN=NULL,FN=0,FP=0),  throws_error())
  expect_that(specificity(TP=1,TN=3,FN=NULL,FP=0),  throws_error())
  expect_that(specificity(TP=1,TN=1,FN=5,FP=NULL),  throws_error())
  expect_that(specificity(TP=10,TN=3,FN=5,FP=5), equals(3/(5+3), tolerance  = 0.0001))
})

test_that("positivePredictiveValue", {
  expect_that(positivePredictiveValue(TP=0,TN=0,FN=0,FP=0), equals(NaN))
  expect_that(positivePredictiveValue(TP=-1,TN=0,FN=0,FP=0),  throws_error())
  expect_that(positivePredictiveValue(TP=1,TN=-1,FN=0,FP=0),  throws_error())
  expect_that(positivePredictiveValue(TP=1,TN=3,FN=-1,FP=0),  throws_error())
  expect_that(positivePredictiveValue(TP=1,TN=1,FN=5,FP=-1),  throws_error())
  expect_that(positivePredictiveValue(TP=NULL,TN=0,FN=0,FP=0),  throws_error())
  expect_that(positivePredictiveValue(TP=1,TN=NULL,FN=0,FP=0),  throws_error())
  expect_that(positivePredictiveValue(TP=1,TN=3,FN=NULL,FP=0),  throws_error())
  expect_that(positivePredictiveValue(TP=1,TN=1,FN=5,FP=NULL),  throws_error())
  expect_that(positivePredictiveValue(TP=10,TN=3,FN=5,FP=5), equals(10/(10+5), tolerance  = 0.0001))
})


test_that("falseDiscoveryRate", {
  expect_that(falseDiscoveryRate(TP=0,TN=0,FN=0,FP=0), equals(NaN))
  expect_that(falseDiscoveryRate(TP=-1,TN=0,FN=0,FP=0),  throws_error())
  expect_that(falseDiscoveryRate(TP=1,TN=-1,FN=0,FP=0),  throws_error())
  expect_that(falseDiscoveryRate(TP=1,TN=3,FN=-1,FP=0),  throws_error())
  expect_that(falseDiscoveryRate(TP=1,TN=1,FN=5,FP=-1),  throws_error())
  expect_that(falseDiscoveryRate(TP=NULL,TN=0,FN=0,FP=0),  throws_error())
  expect_that(falseDiscoveryRate(TP=1,TN=NULL,FN=0,FP=0),  throws_error())
  expect_that(falseDiscoveryRate(TP=1,TN=3,FN=NULL,FP=0),  throws_error())
  expect_that(falseDiscoveryRate(TP=1,TN=1,FN=5,FP=NULL),  throws_error())
  expect_that(falseDiscoveryRate(TP=10,TN=3,FN=5,FP=5), equals(5/(10+5), tolerance  = 0.0001))
})

test_that("negativePredictiveValue", {
  expect_that(negativePredictiveValue(TP=0,TN=0,FN=0,FP=0), equals(NaN))
  expect_that(negativePredictiveValue(TP=-1,TN=0,FN=0,FP=0),  throws_error())
  expect_that(negativePredictiveValue(TP=1,TN=-1,FN=0,FP=0),  throws_error())
  expect_that(negativePredictiveValue(TP=1,TN=3,FN=-1,FP=0),  throws_error())
  expect_that(negativePredictiveValue(TP=1,TN=1,FN=5,FP=-1),  throws_error())
  expect_that(negativePredictiveValue(TP=NULL,TN=0,FN=0,FP=0),  throws_error())
  expect_that(negativePredictiveValue(TP=1,TN=NULL,FN=0,FP=0),  throws_error())
  expect_that(negativePredictiveValue(TP=1,TN=3,FN=NULL,FP=0),  throws_error())
  expect_that(negativePredictiveValue(TP=1,TN=1,FN=5,FP=NULL),  throws_error())
  expect_that(negativePredictiveValue(TP=10,TN=3,FN=5,FP=5), equals(3/(5+3), tolerance  = 0.0001))
})

test_that("falseOmissionRate", {
  expect_that(falseOmissionRate(TP=0,TN=0,FN=0,FP=0), equals(NaN))
  expect_that(falseOmissionRate(TP=-1,TN=0,FN=0,FP=0),  throws_error())
  expect_that(falseOmissionRate(TP=1,TN=-1,FN=0,FP=0),  throws_error())
  expect_that(falseOmissionRate(TP=1,TN=3,FN=-1,FP=0),  throws_error())
  expect_that(falseOmissionRate(TP=1,TN=1,FN=5,FP=-1),  throws_error())
  expect_that(falseOmissionRate(TP=NULL,TN=0,FN=0,FP=0),  throws_error())
  expect_that(falseOmissionRate(TP=1,TN=NULL,FN=0,FP=0),  throws_error())
  expect_that(falseOmissionRate(TP=1,TN=3,FN=NULL,FP=0),  throws_error())
  expect_that(falseOmissionRate(TP=1,TN=1,FN=5,FP=NULL),  throws_error())
  expect_that(falseOmissionRate(TP=10,TN=3,FN=5,FP=5), equals(5/(5+3), tolerance  = 0.0001))
})

test_that("negativeLikelihoodRatio", {
  expect_that(negativeLikelihoodRatio(TP=0,TN=0,FN=0,FP=0), equals(NaN))
  expect_that(negativeLikelihoodRatio(TP=-1,TN=0,FN=0,FP=0),  throws_error())
  expect_that(negativeLikelihoodRatio(TP=1,TN=-1,FN=0,FP=0),  throws_error())
  expect_that(negativeLikelihoodRatio(TP=1,TN=3,FN=-1,FP=0),  throws_error())
  expect_that(negativeLikelihoodRatio(TP=1,TN=1,FN=5,FP=-1),  throws_error())
  expect_that(negativeLikelihoodRatio(TP=NULL,TN=0,FN=0,FP=0),  throws_error())
  expect_that(negativeLikelihoodRatio(TP=1,TN=NULL,FN=0,FP=0),  throws_error())
  expect_that(negativeLikelihoodRatio(TP=1,TN=3,FN=NULL,FP=0),  throws_error())
  expect_that(negativeLikelihoodRatio(TP=1,TN=1,FN=5,FP=NULL),  throws_error())
  expect_that(negativeLikelihoodRatio(TP=10,TN=3,FN=5,FP=5), equals((5/(10+5))/(3/(5+3)), tolerance  = 0.0001))
})

test_that("positiveLikelihoodRatio", {
  expect_that(positiveLikelihoodRatio(TP=0,TN=0,FN=0,FP=0), equals(NaN))
  expect_that(positiveLikelihoodRatio(TP=-1,TN=0,FN=0,FP=0),  throws_error())
  expect_that(positiveLikelihoodRatio(TP=1,TN=-1,FN=0,FP=0),  throws_error())
  expect_that(positiveLikelihoodRatio(TP=1,TN=3,FN=-1,FP=0),  throws_error())
  expect_that(positiveLikelihoodRatio(TP=1,TN=1,FN=5,FP=-1),  throws_error())
  expect_that(positiveLikelihoodRatio(TP=NULL,TN=0,FN=0,FP=0),  throws_error())
  expect_that(positiveLikelihoodRatio(TP=1,TN=NULL,FN=0,FP=0),  throws_error())
  expect_that(positiveLikelihoodRatio(TP=1,TN=3,FN=NULL,FP=0),  throws_error())
  expect_that(positiveLikelihoodRatio(TP=1,TN=1,FN=5,FP=NULL),  throws_error())
  expect_that(positiveLikelihoodRatio(TP=10,TN=3,FN=5,FP=5), equals((10/(10+5))/(5/(5+3)), tolerance  = 0.0001))
})


test_that("diagnosticOddsRatio", {
  expect_that(diagnosticOddsRatio(TP=0,TN=0,FN=0,FP=0), equals(NaN))
  expect_that(diagnosticOddsRatio(TP=-1,TN=0,FN=0,FP=0),  throws_error())
  expect_that(diagnosticOddsRatio(TP=1,TN=-1,FN=0,FP=0),  throws_error())
  expect_that(diagnosticOddsRatio(TP=1,TN=3,FN=-1,FP=0),  throws_error())
  expect_that(diagnosticOddsRatio(TP=1,TN=1,FN=5,FP=-1),  throws_error())
  expect_that(diagnosticOddsRatio(TP=NULL,TN=0,FN=0,FP=0),  throws_error())
  expect_that(diagnosticOddsRatio(TP=1,TN=NULL,FN=0,FP=0),  throws_error())
  expect_that(diagnosticOddsRatio(TP=1,TN=3,FN=NULL,FP=0),  throws_error())
  expect_that(diagnosticOddsRatio(TP=1,TN=1,FN=5,FP=NULL),  throws_error())
  expect_that(diagnosticOddsRatio(TP=10,TN=3,FN=5,FP=5), equals(((10/(10+5))/(5/(5+3)))/((5/(10+5))/(3/(5+3))), tolerance  = 0.0001))
})

#test_that("getDemographicSummary", {
#  prediction <- data.frame(rowId = 1:100, value= runif(100), outcomeCount =round(runif(100)))
#  data(plpDataSimulationProfile)
#  sampleSize <- 2000
  #plpData <- simulatePlpData(plpDataSimulationProfile, n = sampleSize)
  #demoSum <- getDemographicSummary(prediction, plpData)

  #expect_that(nrow(demoSum), equals(40))
  #expect_that(ncol(demoSum), equals(14))
#})

test_that("getPredictionDistribution", {
  Eprediction <- data.frame(value= runif(100), outcomeCount =round(runif(100)))
  predSum <- getPredictionDistribution(Eprediction)
  
  expect_that(nrow(predSum ), equals(2))
  expect_that(ncol(predSum ), equals(11))
})

test_that("getCalibration", {
  Eprediction <- data.frame(rowId=1:100, value= runif(100), outcomeCount =round(runif(100)))
  attr(Eprediction, "metaData")$predictionType <-  "binary"
  calib <- getCalibration(Eprediction)
  
  expect_that(nrow(calib ), equals(10))
  expect_that(ncol(calib ), equals(11))
})

test_that("getThresholdSummary", {
  Eprediction <- data.frame(value= runif(100), outcomeCount =round(runif(100)))
  thresSum <- getThresholdSummary(Eprediction)
  
  expect_that(nrow(thresSum), equals(length(unique(Eprediction$value)))) 
  expect_that(ncol(thresSum), equals(23))
  
  expect_that(thresSum$truePositiveCount+thresSum$falseNegativeCount, 
              equals(rep(sum(Eprediction$outcomeCount),length(thresSum$truePositiveCount))))
  
  expect_that(thresSum$truePositiveCount+thresSum$falsePositiveCount+
              thresSum$trueNegativeCount+thresSum$falseNegativeCount, 
              equals(rep(nrow(Eprediction),length(thresSum$truePositiveCount))))
})


test_that("Calibration", {
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
  
  
  


