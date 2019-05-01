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
context("ImportExport.R")

# how to test exportPlpDataToCsv?

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

transport <- PatientLevelPrediction::transportPlp(plpResult=result,modelName='model', dataName='data',
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

transport <- PatientLevelPrediction::transportPlp(plpResult=result,modelName=NULL, dataName='data',
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

transport <- PatientLevelPrediction::transportPlp(plpResult=result,modelName='model', dataName= NULL,
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


transport <- PatientLevelPrediction::transportPlp(plpResult=result,
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

transport <- PatientLevelPrediction::transportPlp(plpResult=result,
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


transport <- PatientLevelPrediction::transportPlp(plpResult=result,
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

transport <- PatientLevelPrediction::transportPlp(plpResult=result,
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

transport <- PatientLevelPrediction::transportPlp(plpResult=result,
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


transport <- PatientLevelPrediction::transportPlp(plpResult=result,
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


transport <- PatientLevelPrediction::transportPlp(plpResult=result,
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

#test_that("transportModel", {
#  })

test_that("createLrSql", {
  testthat::expect_error(createLrSql(modelNames=NULL, covariateConstructionName='prediction', 
                                     modelTable='#model_table',
                                     analysisId=111, e=environment(),
                                     databaseOutput=NULL))
  
  testthat::expect_error(createLrSql(models=NULL, covariateConstructionName='prediction', 
                                     modelTable='#model_table',
                                     analysisId=111, e=environment(),
                                     databaseOutput=NULL))
  })


test_that("getPredictionCovariateData", {
  covariateSettings <- FeatureExtraction::createDefaultCovariateSettings()
  testthat::expect_error(PatientLevelPrediction:::getPredictionCovariateData(connection =NULL,
                                                                oracleTempSchema = NULL,
                                                                cdmDatabaseSchema = NULL,
                                                                cohortTable = "#cohort_person",
                                                                cohortId = -1,
                                                                cdmVersion = "5",
                                                                rowIdField = "subject_id",
                                                                covariateSettings= NULL,
                                                                aggregated = FALSE,
                                                                analysisId=111,
                                                                databaseOutput=NULL))
  
  testthat::expect_error(PatientLevelPrediction:::getPredictionCovariateData(connection =NULL,
                                                    oracleTempSchema = NULL,
                                                    cdmDatabaseSchema = NULL,
                                                    cohortTable = "#cohort_person",
                                                    cohortId = -1,
                                                    cdmVersion = "4",
                                                    rowIdField = "subject_id",
                                                    covariateSettings=  covariateSettings,
                                                    aggregated = FALSE,
                                                    analysisId=111,
                                                    databaseOutput=NULL))
  
})


test_that("createExistingModelSq", {
  covariateSettings <- FeatureExtraction::createDefaultCovariateSettings()
  testthat::expect_error(createExistingModelSql(modelNames = NULL, interceptTable = NULL,
                                                covariateTable, type='logistic',
                                                analysisId=112, covariateSettings,
                                                asFunctions=F, customCovariates=NULL,
                                                e=environment(),
                                                covariateValues = F
  ))
  
  testthat::expect_error(createExistingModelSql(modelTable = NULL, interceptTable = NULL,
                                                covariateTable, type='logistic',
                                                analysisId=112, covariateSettings,
                                                asFunctions=F, customCovariates=NULL,
                                                e=environment(),
                                                covariateValues = F
  ))
  
})

test_that("getExistingmodelsCovariateData", {
# getExistingmodelsCovariateData
  covariateSettings <- FeatureExtraction::createDefaultCovariateSettings()
  testthat::expect_error(PatientLevelPrediction:::getExistingmodelsCovariateData(covariateSettings= NULL))
  testthat::expect_error(PatientLevelPrediction:::getExistingmodelsCovariateData(covariateSettings= covariateSettings,
                                                                                 cdmVersion = "4"))
})

test_that("toPlpData", {
  
  # CHeCKING THE CONVERSION FROM MATRIX TO PLPDATA
  
  nppl <- 10
  ncov <- 10
  data <- matrix(runif(nppl*ncov), ncol=ncov)
  
  columnInfo <- data.frame(columnId=1:ncov, 
                           columnName = paste0('column',1:ncov), 
                           columnTime = c(rep(-1, ncov-1),0)
  )
  outcomeId <- ncov
  
  # check input fails
  options(fftempdir = getwd())
  testData <- PatientLevelPrediction::toPlpData(data, columnInfo, outcomeId, outcomeThreshold=0.5,
                                                indexTime =0, includeIndexDay=T )
  
  # should convert all the entries 10 variables per 10 people = 100 rows
  testthat::expect_equal(nrow(ff::as.ram(testData$covariates)), nppl*(ncov-1))
  testthat::expect_equal(nrow(ff::as.ram(testData$covariateRef)), nrow(columnInfo))
  testthat::expect_equal(nrow(testData$cohorts), nppl)
  testthat::expect_equal(nrow(testData$outcomes), sum(data[,ncov]>=0.5))
  
  ## Now test the failed inputs...
  # [TODO]
  
})
