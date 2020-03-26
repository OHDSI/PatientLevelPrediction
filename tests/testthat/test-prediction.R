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


test_that("prediction inputs", {
  #=====================================
  # check prediction
  #=====================================
  testthat::expect_error(predictPlp(model=NULL, population=population, 
                                                            plpData=plpData, index=NULL))
  testthat::expect_error(predictPlp(model=list(), population=NULL, 
                                                            plpData=plpData, index=NULL))
  testthat::expect_error(predictPlp(model=list(), population=population, 
                                                            plpData=NULL, index=NULL))
  

  })


test_that("prediction works", {
  #=====================================
  # check prediction
  #=====================================
  pred <- predictPlp(plpModel=plpResult$model, 
                                             population=population, 
                                             plpData=plpData )
  pred <- pred[order(pred$rowId),]
  plpResult$prediction <- plpResult$prediction[order(plpResult$prediction$rowId),]
  testthat::expect_equal(nrow(pred), 
                         nrow(plpResult$prediction))
  testthat::expect_equal(pred$value, 
                         plpResult$prediction$value)
  
  # add single person pred and compare with manual cal
  
  # add prediction of other models
  
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

