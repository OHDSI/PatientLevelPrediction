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

library("testthat")
context("ThresholdSummary")


test_that("getThresholdSummary binary", {
  Eprediction <- data.frame(
    value = runif(100), 
    outcomeCount = round(runif(100)),
    evaluation = rep('Test', 100)
    )
  
  thresSum <- getThresholdSummary(
    prediction = Eprediction,
    predictionType = 'binary',
    typeColumn = 'evaluation'
  )
  
  expect_true('evaluation' %in% colnames(thresSum))
  expect_that(nrow(thresSum), equals(length(unique(Eprediction$value)))) 
  expect_that(ncol(thresSum), equals(24))
  
  expect_that(thresSum$truePositiveCount+thresSum$falseNegativeCount, 
    equals(rep(sum(Eprediction$outcomeCount),length(thresSum$truePositiveCount))))
  
  expect_that(thresSum$truePositiveCount+thresSum$falsePositiveCount+
      thresSum$trueNegativeCount+thresSum$falseNegativeCount, 
    equals(rep(nrow(Eprediction),length(thresSum$truePositiveCount))))
  
  # now do bianry directly
  
  thresSumBin <- getThresholdSummary_binary(
    prediction = Eprediction, 
    evalColumn= 'evaluation'
    )
  
  expect_equal(thresSumBin, thresSum)
  
})

test_that("getThresholdSummary survival", {
  Eprediction <- data.frame(
    value = c(
      (100+sample(10,50, replace = T)),
      
      (105+sample(10,150,replace = T))
      ), 
    outcomeCount = c(rep(1,50), rep(0,150)),
    evaluation = rep('Test', 200),
    survivalTime = 50 + sample(365*2,200, replace = T)
  )
  
  thresSum <- getThresholdSummary_survival(
    prediction = Eprediction, 
    evalColumn= 'evaluation',
    timepoint = 365
  )
  
  expect_true('evaluation' %in% colnames(thresSum))
  expect_true(nrow(thresSum)>0 ) 
  expect_that(ncol(thresSum), equals(5))
  
})


if(F){
test_that("f1Score", {
  netben <- stdca(
    data, 
    outcome, 
    ttoutcome, 
    timepoint, 
    predictors, 
    xstart = 0.01, 
    xstop = 0.99,
    xby = 0.01, 
    ymin = -0.05, 
    probability = NULL, 
    harm = NULL, 
    graph = TRUE, 
    intervention = FALSE, 
    interventionper = 100, 
    smooth = FALSE, 
    loess.span = 0.1, 
    cmprsk = FALSE
    ) 
  
})
}



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
