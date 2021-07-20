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

context("ImportExport")
library(testthat)

# how to test exportPlpDataToCsv?

outputFolder <- tempfile("importExport")
dir.create(outputFolder)

# Test unit for the creation of the study externalValidatePlp
model <- list(model='none - validation',
     modelSettings= NULL,
     hyperParamSearch=NULL,
     trainCVAuc=NULL,
     metaData=list(cohortIds=1, call=list(connectionDetails='sensitive',
                                          cdmDatabaseSchema='senitive',
                                          cohortDatabaseSchema='senitive',
                                          outcomeDatabaseSchema='senitive')),
     populationSettings=list(cohortId=1,outcomeId=2,riskWindowStart=1),
     outcomeId=2,cohortId=1, varImp=NULL, trainingTime=NULL, 
     predict=NULL, index='sensitive')
attr(model, "type") <- 'validation'
class(model) <- 'plpModel'

Ns <- sample(100,100)
NsO <- round(Ns*runif(100))
NsNO <- Ns-NsO
result <- list(inputSetting=list(dataExtrractionSettings=list(connectionDetails='connection',
                                                              cdmDatabaseSchema='sensitive',
                                                              cohortDatabaseSchema='sensitive',
                                                              cohortTable='cohort',
                                                              cohortIds=1,
                                                              outcomeDatabaseSchema='sensitive',
                                                              outcomeTable='cohort',
                                                              outcomeIds=2),
                                 populationSettings=list(cohortId=1,outcomeId=2,riskWindowStart=1),
                                 modelSettings=list(model='fitLassoLogisticRegression', param='0.1', 
                                                    name='Lasso Logistic Regression'),
                                 testSplit='person',
                                 testFraction=0.2,
                                 nfold=3, 
                                 splitSeed=1),
               executionSummary=list(log='sensitive',PackageVersion='R version'),
               model=model, 
               prediction=data.frame(rowId=1:5, value=runif(5)),           
               performanceEvaluation=list(evaluationStatistics=data.frame(a=1,b=2),
                                          thresholdSummary=data.frame(a=1,b=2),
                                          demographicSummary=NULL,
                                          calibrationSummary=data.frame(a=1,b=2),
                                          predictionDistribution=data.frame(a=1,b=2)),
               covariateSummary=data.frame(covaraiteId=1:100,
                                           covariateValue=runif(100),
                                           CovariateCount=Ns,
                                           CovariateCountWithOutcome=NsO,
                                           CovariateCountWithNoOutcome=NsNO
                                           ),
               analysisRef= NULL)
class(result) <- 'runPlp'

transport <- transportPlp(plpResult=result,modelName='model', dataName='data',
                                                  outputFolder, n=NULL,includeEvaluationStatistics=T,
                                                  includeThresholdSummary=T, includeDemographicSummary=T,
                                                  includeCalibrationSummary =T, includePredictionDistribution=T,
                                                  includeCovariateSummary=T, save=F)

test_that("check transportPlp", {
  
  expect_equal(transport$model$metaData$modelName, 'model')
  expect_equal(transport$model$metaData$call$cdmDatabaseSchema, 'data')
  expect_equal(transport$model$metaData$call$connectionDetails, NULL)
  
  # check everything is there:
  expect_equal(class(transport$covariateSummary),class(result$covariateSummary))
  expect_equal(class(transport$performanceEvaluation$evaluationStatistics),
               class(result$performanceEvaluation$evaluationStatistics))
  expect_equal(class(transport$performanceEvaluation$thresholdSummary),
               class(result$performanceEvaluation$thresholdSummary))
  expect_equal(class(transport$performanceEvaluation$demographicSummary),
               class(result$performanceEvaluation$demographicSummary))
  expect_equal(class(transport$performanceEvaluation$calibrationSummary),
               class(result$performanceEvaluation$calibrationSummary))
  expect_equal(class(transport$performanceEvaluation$predictionDistribution),
               class(result$performanceEvaluation$predictionDistribution))
  
  
})

transport <- transportPlp(plpResult=result,modelName=NULL, dataName='data',
                                                  outputFolder, n=NULL,includeEvaluationStatistics=T,
                                                  includeThresholdSummary=T, includeDemographicSummary=T,
                                                  includeCalibrationSummary =T, includePredictionDistribution=T,
                                                  includeCovariateSummary=T, save=F)

test_that("check transportPlp modelName", {
  
  expect_equal(transport$model$metaData$modelName, NULL)
  expect_equal(transport$model$metaData$call$cdmDatabaseSchema, 'data')
  expect_equal(transport$model$metaData$call$connectionDetails, NULL)
  
  # check everything is there:
  expect_equal(class(transport$covariateSummary),class(result$covariateSummary))
  expect_equal(class(transport$performanceEvaluation$evaluationStatistics),
               class(result$performanceEvaluation$evaluationStatistics))
  expect_equal(class(transport$performanceEvaluation$thresholdSummary),
               class(result$performanceEvaluation$thresholdSummary))
  expect_equal(class(transport$performanceEvaluation$demographicSummary),
               class(result$performanceEvaluation$demographicSummary))
  expect_equal(class(transport$performanceEvaluation$calibrationSummary),
               class(result$performanceEvaluation$calibrationSummary))
  expect_equal(class(transport$performanceEvaluation$predictionDistribution),
               class(result$performanceEvaluation$predictionDistribution))
  
  
})

transport <- transportPlp(plpResult=result,modelName='model', dataName= NULL,
                                                  outputFolder, n=NULL,includeEvaluationStatistics=T,
                                                  includeThresholdSummary=T, includeDemographicSummary=T,
                                                  includeCalibrationSummary =T, includePredictionDistribution=T,
                                                  includeCovariateSummary=T, save=F)

test_that("check transportPlp dataName", {
  
  expect_equal(transport$model$metaData$modelName, 'model')
  expect_equal(transport$model$metaData$call$cdmDatabaseSchema, NULL)
  expect_equal(transport$model$metaData$call$connectionDetails, NULL)
  
  # check everything is there:
  expect_equal(class(transport$covariateSummary),class(result$covariateSummary))
  expect_equal(class(transport$performanceEvaluation$evaluationStatistics),
               class(result$performanceEvaluation$evaluationStatistics))
  expect_equal(class(transport$performanceEvaluation$thresholdSummary),
               class(result$performanceEvaluation$thresholdSummary))
  expect_equal(class(transport$performanceEvaluation$demographicSummary),
               class(result$performanceEvaluation$demographicSummary))
  expect_equal(class(transport$performanceEvaluation$calibrationSummary),
               class(result$performanceEvaluation$calibrationSummary))
  expect_equal(class(transport$performanceEvaluation$predictionDistribution),
               class(result$performanceEvaluation$predictionDistribution))
  
  
})


transport <- transportPlp(plpResult=result,
                                                  outputFolder=outputFolder, n=NULL,includeEvaluationStatistics=F,
                                                  includeThresholdSummary=T, includeDemographicSummary=T,
                                                  includeCalibrationSummary =T, includePredictionDistribution=T,
                                                  includeCovariateSummary=T, save=F)

test_that("check transportPlp eval stats", {
  
  # check everything is there:
  expect_equal(class(transport$covariateSummary),class(result$covariateSummary))
  expect_null(transport$performanceEvaluation$evaluationStatistics)
  expect_equal(class(transport$performanceEvaluation$thresholdSummary),
               class(result$performanceEvaluation$thresholdSummary))
  expect_equal(class(transport$performanceEvaluation$demographicSummary),
               class(result$performanceEvaluation$demographicSummary))
  expect_equal(class(transport$performanceEvaluation$calibrationSummary),
               class(result$performanceEvaluation$calibrationSummary))
  expect_equal(class(transport$performanceEvaluation$predictionDistribution),
               class(result$performanceEvaluation$predictionDistribution))
  
  
})

transport <- transportPlp(plpResult=result,
                                                  outputFolder=outputFolder, n=NULL,includeEvaluationStatistics=T,
                                                  includeThresholdSummary=F, includeDemographicSummary=T,
                                                  includeCalibrationSummary =T, includePredictionDistribution=T,
                                                  includeCovariateSummary=T, save=F)

test_that("check transportPlp threshold", {
  
  # check everything is there:
  expect_equal(class(transport$covariateSummary),class(result$covariateSummary))
  expect_equal(class(transport$performanceEvaluation$evaluationStatistics),
               class(result$performanceEvaluation$evaluationStatistics))
  expect_null(transport$performanceEvaluation$thresholdSummary)
  expect_equal(class(transport$performanceEvaluation$demographicSummary),
               class(result$performanceEvaluation$demographicSummary))
  expect_equal(class(transport$performanceEvaluation$calibrationSummary),
               class(result$performanceEvaluation$calibrationSummary))
  expect_equal(class(transport$performanceEvaluation$predictionDistribution),
               class(result$performanceEvaluation$predictionDistribution))
  
  
})


transport <- transportPlp(plpResult=result,
                                                  outputFolder=outputFolder, n=NULL,includeEvaluationStatistics=T,
                                                  includeThresholdSummary=T, includeDemographicSummary=F,
                                                  includeCalibrationSummary =T, includePredictionDistribution=T,
                                                  includeCovariateSummary=T, save=F)

test_that("check transportPlp demo", {

  # check everything is there:
  expect_equal(class(transport$covariateSummary),class(result$covariateSummary))
  expect_equal(class(transport$performanceEvaluation$evaluationStatistics),
               class(result$performanceEvaluation$evaluationStatistics))
  expect_equal(class(transport$performanceEvaluation$thresholdSummary),
               class(result$performanceEvaluation$thresholdSummary))
  expect_null(transport$performanceEvaluation$demographicSummary)
  expect_equal(class(transport$performanceEvaluation$calibrationSummary),
               class(result$performanceEvaluation$calibrationSummary))
  expect_equal(class(transport$performanceEvaluation$predictionDistribution),
               class(result$performanceEvaluation$predictionDistribution))
  
  
})

transport <- transportPlp(plpResult=result,
                                                  outputFolder=outputFolder, n=NULL,includeEvaluationStatistics=T,
                                                  includeThresholdSummary=T, includeDemographicSummary=T,
                                                  includeCalibrationSummary =F, includePredictionDistribution=T,
                                                  includeCovariateSummary=T, save=F)

test_that("check transportPlp cal", {

  # check everything is there:
  expect_equal(class(transport$covariateSummary),class(result$covariateSummary))
  expect_equal(class(transport$performanceEvaluation$evaluationStatistics),
               class(result$performanceEvaluation$evaluationStatistics))
  expect_equal(class(transport$performanceEvaluation$thresholdSummary),
               class(result$performanceEvaluation$thresholdSummary))
  expect_equal(class(transport$performanceEvaluation$demographicSummary),
               class(result$performanceEvaluation$demographicSummary))
  expect_null(transport$performanceEvaluation$calibrationSummary)
  expect_equal(class(transport$performanceEvaluation$predictionDistribution),
               class(result$performanceEvaluation$predictionDistribution))
  
  
})

transport <- transportPlp(plpResult=result,
                                                  outputFolder=outputFolder, n=NULL,includeEvaluationStatistics=T,
                                                  includeThresholdSummary=T, includeDemographicSummary=T,
                                                  includeCalibrationSummary =T, includePredictionDistribution=F,
                                                  includeCovariateSummary=T, save=F)

test_that("check transportPlp pred dist", {
  
  # check everything is there:
  expect_equal(class(transport$covariateSummary),class(result$covariateSummary))
  expect_equal(class(transport$performanceEvaluation$evaluationStatistics),
               class(result$performanceEvaluation$evaluationStatistics))
  expect_equal(class(transport$performanceEvaluation$thresholdSummary),
               class(result$performanceEvaluation$thresholdSummary))
  expect_equal(class(transport$performanceEvaluation$demographicSummary),
               class(result$performanceEvaluation$demographicSummary))
  expect_equal(class(transport$performanceEvaluation$calibrationSummary),
               class(result$performanceEvaluation$calibrationSummary))
  expect_null(transport$performanceEvaluation$predictionDistribution)
  
  
})


transport <- transportPlp(plpResult=result,
                                                  outputFolder=outputFolder, n=NULL,includeEvaluationStatistics=T,
                                                  includeThresholdSummary=T, includeDemographicSummary=T,
                                                  includeCalibrationSummary =T, includePredictionDistribution=T,
                                                  includeCovariateSummary=F, save=F)

test_that("check transportPlp cov sum", {
  
  # check everything is there:
  expect_null(transport$covariateSummary)
  expect_equal(class(transport$performanceEvaluation$evaluationStatistics),
               class(result$performanceEvaluation$evaluationStatistics))
  expect_equal(class(transport$performanceEvaluation$thresholdSummary),
               class(result$performanceEvaluation$thresholdSummary))
  expect_equal(class(transport$performanceEvaluation$demographicSummary),
               class(result$performanceEvaluation$demographicSummary))
  expect_equal(class(transport$performanceEvaluation$calibrationSummary),
               class(result$performanceEvaluation$calibrationSummary))
  expect_equal(class(transport$performanceEvaluation$predictionDistribution),
               class(result$performanceEvaluation$predictionDistribution))
  
  
})


transport <- transportPlp(plpResult=result,
                                                  outputFolder=outputFolder, n=5,includeEvaluationStatistics=T,
                                                  includeThresholdSummary=T, includeDemographicSummary=T,
                                                  includeCalibrationSummary =T, includePredictionDistribution=T,
                                                  includeCovariateSummary=T, save=F)

test_that("check transportPlp N is 5", {
  check1 <- sum(transport$covariateSummary$CovariateCount < 5 & transport$covariateSummary$CovariateCount > 0)==0
  expect_equal(check1, TRUE)
  
  check2 <- sum(transport$covariateSummary$CovariateCountWithOutcome < 5 & transport$covariateSummary$CovariateCountWithOutcome > 0)==0
  expect_equal(check2, TRUE)
  
  check3 <- sum(transport$covariateSummary$CovariateCountWithNoOutcome < 5 & transport$covariateSummary$CovariateCountWithNoOutcome > 0)==0
  expect_equal(check3, TRUE)
})

test_that("transportModel", {
  transportModel(plpModel = result$model,outputFolder = file.path(outputFolder,'transportModel'))
  testthat::expect_equal(dir.exists(file.path(outputFolder,'transportModel')), T)
  
  tmod <- loadPlpModel(file.path(outputFolder,'transportModel'))
  testthat::expect_equal(tmod$metaData$call$connectionDetails, NULL)
  })
