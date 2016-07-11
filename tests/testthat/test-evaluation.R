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
library("pROC")
library("AUC")
library("scoring")
library("Metrics")

context("Performance Measures")

test_that("AUC", {
  prediction <- data.frame(value= runif(100), outcomeCount =round(runif(100)))
  
  proc.auc <- pROC::roc(prediction$outcomeCount, prediction$value, algorithm = 3,
                        direction="<")
  auc.auc <- AUC::auc(AUC:::roc(prediction$value, factor(prediction$outcomeCount)))
  tolerance <- 0.001
  expect_equal(as.numeric(proc.auc$auc), auc.auc, tolerance = tolerance)
})

test_that("Brierscore", {
  prediction <- data.frame(value= runif(100), outcomeCount =round(runif(100)))

  prediction$dummy <- 1
  brier.scoring <- scoring::brierscore(outcomeCount ~ value, data=prediction, group='dummy')$mnbrier
  brier.plp <- brierScore(prediction)$brier
  expect_that(as.double(brier.scoring), equals(brier.plp))
})

test_that("Average precision", {
  prediction <- data.frame(value= runif(100), outcomeCount =round(runif(100)))
  
  aveP.metrics <- Metrics::apk(nrow(prediction), 
                               which(prediction$outcomeCount==1), (1:nrow(prediction))[order(-prediction$value)])
  aveP.plp <- averagePrecision(prediction)
  expect_that(as.double(aveP.metrics), equals(aveP.plp))
})

test_that("Quantiles", {
  prediction <- data.frame(value= runif(100), outcomeCount =round(runif(100)))

  prediction.quant <- data.frame(value=c(rep(0,10), 0.001,0.03,0.1,0.12,0.2,
                                         0.2,0.33,0.4,0.5,0.9),
                                 outcomeCount=c(rep(0,10), rep(1,10)))
  quant.test <- quantiles(prediction.quant)$quantiles
  
  quant.test.0 <- quant.test[quant.test[,1]==0,-1]
  quant.test.1max <- max(quant.test[quant.test[,1]==1,-1])
  quant.test.1min <- min(quant.test[quant.test[,1]==1,-1])
  quant.test.1median <- median(quant.test[quant.test[,1]==1,-1])
  
  expect_that(sum(quant.test[quant.test[,1]==0,-1]==c(0,0,0,0,0,0,0)), equals(7))
  expect_that(quant.test.1max, equals(0.9))
  expect_that(quant.test.1min, equals(0.001))
  expect_that(quant.test.1median, equals(0.2))
})

test_that("Sparse ROC", {
  prediction <- data.frame(value= runif(100), outcomeCount =round(runif(100)))
  
  prediction.quant <- data.frame(value=c(rep(0,10), 0.001,0.03,0.1,0.12,0.2,
                                         0.2,0.33,0.4,0.5,0.9),
                                 outcomeCount=c(rep(0,10), rep(1,10)))
  expect_warning(rocSparse(prediction.quant))
  
  prediction.quant2 <- data.frame(value=c(rep(0,50), rep(0.1,50)),
                                 outcomeCount=c(rep(0,50), rep(1,50))
  )
  rocSparse.plp <- rocSparse(prediction.quant2)
  rocSparse.manual <- c(50,0,50,0,1,0,1,0,1,1)
  expect_that(sum(rocSparse.plp==rocSparse.manual), equals(10))
  
  # check output of other predicts:
  prediction.roc1 <- data.frame(value=runif(10000),
                                 outcomeCount=c(rep(0,10000-49), rep(1,49))
                                )
  rocSparse.plp2 <- rocSparse(prediction.roc1)
  expect_that(nrow(rocSparse.plp2)<100, equals(T))
  
  prediction.roc2 <- data.frame(value=runif(10000),
                                outcomeCount=c(rep(0,50000), rep(1,50000))
  )
  rocSparse.plp3 <- rocSparse(prediction.roc2)
  expect_that(nrow(rocSparse.plp3)==100, equals(T))
  
  # total is correct:
  expect_that(sum(rocSparse.plp3$TP+rocSparse.plp3$FP+rocSparse.plp3$TN+rocSparse.plp3$FN == 1e+05),
              equals(length(rocSparse.plp3$TP)))
  
  # correct num of positives
  expect_that(sum(rocSparse.plp3$TP+rocSparse.plp3$FN == 5e+04),
              equals(length(rocSparse.plp3$TP)))
  
})

  # TODO: test pref scores and calibration
  # test computePreferenceScore(prediction)
  ##writeLines('Testing preference scores...')
  
  #############################################################################
  # prediction.perfect <- data.frame(value=c(rep(0, 10), rep(0.1,10), rep(0.2,10),
  #                                          rep(0.3,10), rep(0.4,10), rep(0.5,10),
  #                                          rep(0.6,10), rep(0.7,10), rep(0.8,10),
  #                                          rep(0.9,10)),
  #                                  outcomeCount =c(rep(0,10), 1, rep(0,9), 
  #                                                  rep(1,2), rep(0,8),
  #                                                  rep(1,3), rep(0,7),
  #                                                  rep(1,4), rep(0,6),
  #                                                  rep(1,5), rep(0,5),
  #                                                  rep(1,6), rep(0,4),
  #                                                  rep(1,7), rep(0,3),
  #                                                  rep(1,8), rep(0,2),
  #                                                  rep(1,9), rep(0,1))
  #                                 )
  # test calibrationLine(prediction, numberOfStrata = 10)
  
  #hl score: PredictABEL::plotCalibration(data, cOutcome, predRisk, groups, rangeaxis,plottitle, xlabel, ylabel, filename, fileplot, plottype)
  
  
  ## writeLines('Testing calibration...')
  
  


