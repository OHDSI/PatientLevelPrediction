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
context("Evaluation")

library("testthat")
library("pROC")
library("AUC")
library("scoring")
library("Metrics")
library("PRROC")

test_that("AUROC", {
  prediction <- data.frame(value= runif(100), outcomeCount = round(runif(100)))
  
  proc.auc <- pROC::roc(prediction$outcomeCount, prediction$value, algorithm = 3,
                        direction="<")
  auc.auc <- AUC::auc(AUC::roc(prediction$value, factor(prediction$outcomeCount)))
  tolerance <- 0.001
  expect_equal(as.numeric(proc.auc$auc), auc.auc, tolerance = tolerance)
})

test_that("AUPRC", {
  prediction <- data.frame(value= runif(100), outcomeCount = round(runif(100)))
  
  positive <- prediction$value[prediction$outcomeCount == 1]
  negative <- prediction$value[prediction$outcomeCount == 0]
  pr <- PRROC::pr.curve(scores.class0 = positive, scores.class1 = negative)
  auprc <- pr$auc.integral
  
  # area under precision-recall curve must be between 0 and 1
  expect_gte(auprc, 0)
  expect_lte(auprc, 1)
})

test_that("Brierscore", {
  prediction <- data.frame(value= runif(100), outcomeCount = round(runif(100)))

  prediction$dummy <- 1
  brier.scoring <- scoring::brierscore(outcomeCount ~ value, data=prediction, group='dummy')$brieravg
  brier.plp <- brierScore(prediction)$brier
  expect_that(as.double(brier.scoring), equals(brier.plp))
})

test_that("Average precision", {
  prediction <- data.frame(value= runif(100), outcomeCount = round(runif(100)))
  
  aveP.metrics <- Metrics::apk(nrow(prediction), 
                               which(prediction$outcomeCount==1), (1:nrow(prediction))[order(-prediction$value)])
  aveP.plp <- averagePrecision(prediction)
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
  expect_that(PatientLevelPrediction::accuracy(TP=0,TN=0,FN=0,FP=0), equals(NaN))
  expect_that(PatientLevelPrediction::accuracy(TP=-1,TN=0,FN=0,FP=0),  throws_error())
  expect_that(PatientLevelPrediction::accuracy(TP=1,TN=-1,FN=0,FP=0),  throws_error())
  expect_that(PatientLevelPrediction::accuracy(TP=1,TN=3,FN=-1,FP=0),  throws_error())
  expect_that(PatientLevelPrediction::accuracy(TP=1,TN=1,FN=5,FP=-1),  throws_error())
  expect_that(PatientLevelPrediction::accuracy(TP=NULL,TN=0,FN=0,FP=0),  throws_error())
  expect_that(PatientLevelPrediction::accuracy(TP=1,TN=NULL,FN=0,FP=0),  throws_error())
  expect_that(PatientLevelPrediction::accuracy(TP=1,TN=3,FN=NULL,FP=0),  throws_error())
  expect_that(PatientLevelPrediction::accuracy(TP=1,TN=1,FN=5,FP=NULL),  throws_error())
  expect_that(PatientLevelPrediction::accuracy(TP=10,TN=3,FN=5,FP=5), equals(13/23, tolerance  = 0.0001))
})

test_that("sensitivity", {
  expect_that(PatientLevelPrediction::sensitivity(TP=0,TN=0,FN=0,FP=0), equals(NaN))
  expect_that(PatientLevelPrediction::sensitivity(TP=-1,TN=0,FN=0,FP=0),  throws_error())
  expect_that(PatientLevelPrediction::sensitivity(TP=1,TN=-1,FN=0,FP=0),  throws_error())
  expect_that(PatientLevelPrediction::sensitivity(TP=1,TN=3,FN=-1,FP=0),  throws_error())
  expect_that(PatientLevelPrediction::sensitivity(TP=1,TN=1,FN=5,FP=-1),  throws_error())
  expect_that(PatientLevelPrediction::sensitivity(TP=NULL,TN=0,FN=0,FP=0),  throws_error())
  expect_that(PatientLevelPrediction::sensitivity(TP=1,TN=NULL,FN=0,FP=0),  throws_error())
  expect_that(PatientLevelPrediction::sensitivity(TP=1,TN=3,FN=NULL,FP=0),  throws_error())
  expect_that(PatientLevelPrediction::sensitivity(TP=1,TN=1,FN=5,FP=NULL),  throws_error())
  expect_that(PatientLevelPrediction::sensitivity(TP=10,TN=3,FN=5,FP=5), equals(10/(10+5),tolerance  = 0.0001))
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
  expect_that(PatientLevelPrediction::specificity(TP=0,TN=0,FN=0,FP=0), equals(NaN))
  expect_that(PatientLevelPrediction::specificity(TP=-1,TN=0,FN=0,FP=0),  throws_error())
  expect_that(PatientLevelPrediction::specificity(TP=1,TN=-1,FN=0,FP=0),  throws_error())
  expect_that(PatientLevelPrediction::specificity(TP=1,TN=3,FN=-1,FP=0),  throws_error())
  expect_that(PatientLevelPrediction::specificity(TP=1,TN=1,FN=5,FP=-1),  throws_error())
  expect_that(PatientLevelPrediction::specificity(TP=NULL,TN=0,FN=0,FP=0),  throws_error())
  expect_that(PatientLevelPrediction::specificity(TP=1,TN=NULL,FN=0,FP=0),  throws_error())
  expect_that(PatientLevelPrediction::specificity(TP=1,TN=3,FN=NULL,FP=0),  throws_error())
  expect_that(PatientLevelPrediction::specificity(TP=1,TN=1,FN=5,FP=NULL),  throws_error())
  expect_that(PatientLevelPrediction::specificity(TP=10,TN=3,FN=5,FP=5), equals(3/(5+3), tolerance  = 0.0001))
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
  prediction <- data.frame(value= runif(100), outcomeCount =round(runif(100)))
  predSum <- getPredictionDistribution(prediction)
  
  expect_that(nrow(predSum ), equals(2))
  expect_that(ncol(predSum ), equals(11))
})

test_that("getCalibration", {
  prediction <- data.frame(rowId=1:100, value= runif(100), outcomeCount =round(runif(100)))
  attr(prediction, "metaData")$predictionType <-  "binary"
  calib <- getCalibration(prediction)
  
  expect_that(nrow(calib ), equals(10))
  expect_that(ncol(calib ), equals(11))
})

test_that("getThresholdSummary", {
  prediction <- data.frame(value= runif(100), outcomeCount =round(runif(100)))
  thresSum <- getThresholdSummary(prediction)
  
  expect_that(nrow(thresSum), equals(100))
  expect_that(ncol(thresSum), equals(23))
  
  expect_that(thresSum$truePositiveCount+thresSum$falseNegativeCount, 
              equals(rep(sum(prediction$outcomeCount),100)))
  
  expect_that(thresSum$truePositiveCount+thresSum$falsePositiveCount+
              thresSum$trueNegativeCount+thresSum$falseNegativeCount, 
              equals(rep(nrow(prediction),100)))
})


test_that("Calibration", {
  prediction <- data.frame(rowId=1:100,
                           value= c(rep(0,50),rep(1,50)), 
                           outcomeCount =c(rep(0,50),rep(1,50)))
  # test the output 
  calibrationTest1 <- calibrationLine(prediction,numberOfStrata=2)
  expect_that(calibrationTest1$lm['Intercept'],  is_equivalent_to(0))
  expect_that(calibrationTest1$lm['Gradient'],  is_equivalent_to(1))
  expect_that(nrow(calibrationTest1$aggregateLmData)==2, equals(T))
  
  # should return - need to test all three
  ##lm # has the 'Intercept' and 'Gradient'
  ##aggregateLmData # obs vs pred for groups
  ##hosmerlemeshow # hosmerlemeshow value
  prediction2 <- data.frame(rowId=1:100,
                           value= c(0.1+runif(50)*0.9,runif(50)*0.6), 
                           outcomeCount =c(rep(1,50),rep(0,50)))
  
  hs.exist2 <- ResourceSelection::hoslem.test(prediction2$outcomeCount, 
                                              prediction2$value, g=10)
  calibrationTest2 <- calibrationLine(prediction2,numberOfStrata=10)
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
  
  
  


