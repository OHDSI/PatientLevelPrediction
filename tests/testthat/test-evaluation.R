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
  auc.auc <- AUC::auc(AUC::roc(prediction$value, factor(prediction$outcomeCount)))
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
  #prediction with no negatives - returns error
  sampSize <- sample(100,1)
  prediction.noneg <- data.frame(value=runif(sampSize), 
                                  outcomeCount=rep(1,sampSize))
  expect_warning(rocSparse(prediction.noneg))
  
  #prediction with no positives - returns error
  prediction.nopos <- data.frame(value=runif(sampSize), 
                                 outcomeCount=rep(0,sampSize))
  expect_warning(rocSparse(prediction.nopos))
  
  # test non-sparse return size when less than 100 negatives
  numNeg <- sample(100,1)
  prediction.sizeSmall <- data.frame(value= runif(200), 
                                     outcomeCount =c(rep(0,numNeg), rep(1,200-numNeg)))
  rocSparse.plp <- rocSparse(prediction.sizeSmall)
  expect_that(nrow(rocSparse.plp), equals(numNeg))
  
  # test non-sparse return size when more than 100 negatives
  numNeg <- 100+sample(100,1)
  prediction.sizeBig <- data.frame(value= runif(250), 
                                   outcomeCount =c(rep(0,numNeg), rep(1,250-numNeg)))
  rocSparse.plp <- rocSparse(prediction.sizeBig)
  expect_that(nrow(rocSparse.plp), equals(100))
  
  # check output of simple input:
  numPos <- sample(200,1)
  prediction.manualTest1 <- data.frame(value=runif(10000),
                                 outcomeCount=c(rep(0,10000-numPos), rep(1,numPos))
                                )
  rocSparse.manualTest1 <- rocSparse(prediction.manualTest1)

  # total is correct:
  expect_that(sum(rocSparse.manualTest1$TP+rocSparse.manualTest1$FP+
                  rocSparse.manualTest1$TN+rocSparse.manualTest1$FN == 1e+04),
              equals(length(rocSparse.manualTest1$TP)))
  
  # correct num of positives
  expect_that(sum(rocSparse.manualTest1$TP+rocSparse.manualTest1$FN == numPos),
              equals(length(rocSparse.manualTest1$TP)))
  
  ## check output for perfect prediction:
  numPos <- sample(200,1)
  prediction.manualTest2 <- data.frame(value=seq(0,1, length.out=1000),
                                       outcomeCount=c(rep(0,1000-numPos), rep(1,numPos))
  )
  rocSparse.manualTest2 <- rocSparse(prediction.manualTest2)
  
  expect_that(sum(rocSparse.manualTest2$TP == rep(numPos,100)),
              equals(100))
  
  ## check output for work prediction:
  numPos <- sample(200,1)
  prediction.manualTest3 <- data.frame(value=seq(0,1, length.out=1000),
                                       outcomeCount=c(rep(1,numPos),rep(0,1000-numPos))
  )
  rocSparse.manualTest3 <- rocSparse(prediction.manualTest3)
  
  expect_that(sum(rocSparse.manualTest3$TP == c(rep(0,99),numPos)),
              equals(100))
  
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
  
  
  


