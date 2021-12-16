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

context("Prediction")


test_that("prediction inputs", {
  #=====================================
  # check prediction
  #=====================================
  testthat::expect_error(predictPlp(model=NULL, population=population, 
                                                            plpData=plpData))
  testthat::expect_error(predictPlp(model=list(), population=NULL, 
                                                            plpData=plpData))
  testthat::expect_error(predictPlp(model=list(), population=population, 
                                                            plpData=NULL))
  

  })


test_that("prediction works", {
  #=====================================
  # check prediction
  #=====================================
  pred <- predictPlp(plpModel=plpResult$model, 
    population=population, 
    plpData=plpData 
  )
  pred <- pred[order(pred$rowId),]
  plpResult$prediction <- plpResult$prediction[order(plpResult$prediction$rowId),]
  testthat::expect_equal(nrow(pred), 
                         nrow(population))
  
  rowId <- plpResult$prediction$rowId[plpResult$prediction$evaluationType == 'Test'][1]
  
  testthat::expect_equal(pred$value[pred$rowId == rowId], 
                         plpResult$prediction$value[
                           plpResult$prediction$evaluationType == 'Test' & 
                             plpResult$prediction$rowId == rowId
                           ]
    )
  
  # check metaData
  expect_equal(length(names(attr(pred, "metaData"))), 4)  # 6 if survivial
  
  # add single person pred and compare with manual cal
  
  # add prediction of other models
  
})

# predict.*


test_that("applyTidyCovariateData", {
  
  covariateIds <- plpData$covariateData$covariateRef %>% dplyr::select(.data$covariateId) %>% dplyr::pull()
  remove <- sample(covariateIds, 10)
  deletedRedundantCovariateIds = remove[1:5]
  deletedInfrequentCovariateIds = remove[6:10]
  
  prepocessSettings = list(
    normFactors = data.frame(
      covariateId = covariateIds,
      maxValue = rep(0.1,length(covariateIds))
      ),
    deletedRedundantCovariateIds = deletedRedundantCovariateIds,
    deletedInfrequentCovariateIds = deletedInfrequentCovariateIds
  )
  
  # get covariateSize before 
  covariateCount <- plpData$covariateData$covariates %>% dplyr::tally() %>% dplyr::pull()
  
  newCovariateData <- applyTidyCovariateData(
    covariateData = plpData$covariateData,
    preprocessSettings = prepocessSettings
  )
  
  # some covariates removed
  expect_true(newCovariateData$covariates %>% dplyr::tally() %>% dplyr::pull() < covariateCount)
  
  newCovs <- newCovariateData$covariateRef %>% dplyr::select(.data$covariateId) %>% dplyr::pull()
  
  expect_equal(sum(covariateIds[!covariateIds %in% newCovs] %in% remove),10)
  
})




