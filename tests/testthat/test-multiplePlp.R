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
context("MultiplePlp")

test_that("evaluateMultiplePlp errors", {
  
  testthat::expect_error(evaluateMultiplePlp(analysesLocation = file.path(saveLoc,'madeup') ,
                                  outputLocation= file.path(saveLoc,'madeup'),
                                  connectionDetails='madeup', 
                                  validationSchemaTarget='madeup',
                                  validationSchemaOutcome='madeup',
                                  validationSchemaCdm='madeup', 
                                  databaseNames,
                                  validationTableTarget='madeup',
                                  validationTableOutcome='madeup',
                                  validationIdTarget = NULL,
                                  validationIdOutcome = NULL,
                                  oracleTempSchema = NULL,
                                  verbosity = 'INFO',
                                  keepPrediction = F))
})

test_that("createStudyPopulationSettings", {
  
  binary <- sample(c(T,F),1)
  includeAllOutcomes <-  sample(c(T,F),1)
  firstExposureOnly <-  sample(c(T,F),1)
  washoutPeriod <- sample(1000,1)
  result <- createStudyPopulationSettings(binary = binary,
                                            includeAllOutcomes = includeAllOutcomes,
                                            firstExposureOnly = firstExposureOnly,
                                            washoutPeriod = washoutPeriod,
                                            removeSubjectsWithPriorOutcome = firstExposureOnly,
                                            priorOutcomeLookback = washoutPeriod,
                                            requireTimeAtRisk = includeAllOutcomes,
                                            minTimeAtRisk=washoutPeriod,
                                            riskWindowStart = washoutPeriod,
                                            addExposureDaysToStart = binary,
                                            riskWindowEnd = washoutPeriod,
                                            addExposureDaysToEnd = binary,
                                            verbosity = "INFO")
  
  testthat::expect_true(class(result)=='populationSettings')
  testthat::expect_true(result$includeAllOutcomes==includeAllOutcomes)
  testthat::expect_true(result$firstExposureOnly==firstExposureOnly)
  testthat::expect_true(result$washoutPeriod==washoutPeriod)
  testthat::expect_true(result$priorOutcomeLookback == washoutPeriod)
  testthat::expect_true(result$removeSubjectsWithPriorOutcome == firstExposureOnly)
  testthat::expect_true(result$requireTimeAtRisk == includeAllOutcomes)
  testthat::expect_true(result$minTimeAtRisk == washoutPeriod)
  testthat::expect_true(result$riskWindowStart == washoutPeriod)
  testthat::expect_true(result$riskWindowEnd == washoutPeriod)
  testthat::expect_true(result$addExposureDaysToStart == binary)
  testthat::expect_true(result$addExposureDaysToEnd == binary)
  
})

# TODO
#test_that("combinePlpModelSettings", {
  #result <- combinePlpModelSettings(plpModelSetting1, plpModelSetting2)
#})

test_that("createPlpModelSettings", {

  modN <- sample(10,1)
  modList <- as.list(sample(10,modN))
  modList <- lapply(modList, function(x) list(x, name='test'))
  covN <- sample(10,1)
  covList <- as.list(sample(10,covN))
  popN <- sample(10,1)
  popList <- as.list(sample(10,popN))
  popList <- lapply(popList, function(x) list(x, addExposureDaysToStart=T, riskWindowStart=1, addExposureDaysToEnd=T, riskWindowEnd=365 ))
  modelAnalysisList <- createPlpModelSettings(modelList = modList, 
                                              covariateSettingList = covList, 
                                              populationSettingList = popList)

  testthat::expect_true(length(modelAnalysisList$models) == modN)
  testthat::expect_true(length(modelAnalysisList$covariateSettings) == covN)
  testthat::expect_true(length(modelAnalysisList$populationSettings) == popN)
  testthat::expect_true(nrow(modelAnalysisList$settingLookupTable) == modN*covN*popN)
  testthat::expect_true(max(as.double(as.character(modelAnalysisList$settingLookupTable$populationSettingId)))==popN)
  testthat::expect_true(max(as.double(as.character(modelAnalysisList$settingLookupTable$covariateSettingId)))==covN)
  testthat::expect_true(max(as.double(as.character(modelAnalysisList$settingLookupTable$modelSettingId)))==modN)
  testthat::expect_true(length(unique(modelAnalysisList$settingLookupTable$populationSettingId))==popN)
  testthat::expect_true(length(unique(modelAnalysisList$settingLookupTable$covariateSettingId))==covN)
  testthat::expect_true(length(unique(modelAnalysisList$settingLookupTable$modelSettingId))==modN)

})

studyPop1 <- createStudyPopulationSettings(binary = T,
                                           includeAllOutcomes = F,
                                           removeSubjectsWithPriorOutcome = F,
                                           priorOutcomeLookback = 99999,
                                           requireTimeAtRisk = T,
                                           minTimeAtRisk=1,
                                           riskWindowStart = 0,
                                           riskWindowEnd = 1000,
                                           verbosity = "INFO")
studyPop2 <- createStudyPopulationSettings(binary = T,
                                           includeAllOutcomes = F,
                                           removeSubjectsWithPriorOutcome = F,
                                           priorOutcomeLookback = 99999,
                                           requireTimeAtRisk = T,
                                           minTimeAtRisk=1,
                                           riskWindowStart = 0,
                                           riskWindowEnd = 2000,
                                           verbosity = "INFO")

covSet1 <- createCovariateSettings(useDemographicsGender = T, 
                                   useDemographicsAgeGroup = T)
modelAnalysisList <- createPlpModelSettings(modelList = list(setRandomForest(mtries = -1,ntrees = 10, maxDepth = 2, varImp = F, seed=1)), 
                                            covariateSettingList = list(covSet1), 
                                            populationSettingList = list(studyPop1,
                                                                         studyPop2))

test_that("createPlpReferenceTable", {
  
  cohorts <- sample(200,sample(10,1))
  outs <- 200+sample(200,sample(10,1))
  result <- PatientLevelPrediction:::createPlpReferenceTable(modelAnalysisList,
                                    cohortIds = cohorts,
                                    outcomeIds = outs,
                                    outputFolder = 'test', 
                                    cdmDatabaseName = 'data')
  
  testthat::expect_true(nrow(result) == length(outs)*length(cohorts)*nrow(modelAnalysisList$settingLookupTable))
  testthat::expect_true(sum(result$cohortId==cohorts[1])==nrow(modelAnalysisList$settingLookupTable)*length(outs))
  testthat::expect_true(unique(result$devDatabase)== 'data')
  
})

# returns location
test_that("save and loadPredictionAnalysisList works", {
  if(!dir.exists(file.path(saveLoc,'multList'))){dir.create(file.path(saveLoc,'multList'))}
  write.csv(data.frame(cohortId = 1:2,name = paste('blabla',1:2)), file.path(saveLoc,'multList/CohortsToCreate.csv'))
  predictionAnalysisListFile <- savePredictionAnalysisList(workFolder= file.path(saveLoc,"multList"),
                                                           cohortIds = c(1,2),
                                                           outcomeIds = c(3,4),
                                                           modelSettingList = list(setLassoLogisticRegression()),
                                                           covariateSettingList = list(covSet1), 
                                                           populationSettingList = list(studyPop1,
                                                                                        studyPop2),
                                                           maxSampleSize= NULL,
                                                           washoutPeriod=0,
                                                           minCovariateFraction=0,
                                                           normalizeData=T,
                                                           testSplit='person',
                                                           testFraction=0.25,
                                                           splitSeed=1,
                                                           nfold=3)
  
  testthat::expect_equal(file.exists(predictionAnalysisListFile), T)
  
  # returns a list
  res <- loadPredictionAnalysisList(predictionAnalysisListFile)
  testthat::expect_equal(class(res), 'list')
})

runPlpAnalysesResults <- runPlpAnalyses(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cdmDatabaseName = 'test',
                          oracleTempSchema = cdmDatabaseSchema,
                          cohortDatabaseSchema = ohdsiDatabaseSchema,
                          cohortTable = "cohorts",
                          outcomeDatabaseSchema = ohdsiDatabaseSchema,
                          outcomeTable = 'outs_test',
                          cdmVersion = 5,
                          outputFolder = file.path(saveLoc,"mult"),
                          modelAnalysisList = modelAnalysisList,
                          cohortIds = 1,
                          cohortNames = 'test cohort',
                          outcomeIds = 2,
                          outcomeNames = 'test outcome',
                          washoutPeriod = 0,
                          maxSampleSize = NULL,
                          minCovariateFraction = 0,
                          normalizeData = T,
                          testSplit = "subject",
                          testFraction = 0.25,
                          splitSeed = 1,
                          nfold = 3,
                          verbosity = "INFO")
test_that("runPlpAnalyses works", {
testthat::expect_equal(class(runPlpAnalysesResults), 'data.frame')

})

test_that("getSummary file without external validation", {
  res <- getSummary(result = file.path(saveLoc,"mult"), inputType = 'file', validation = NULL)
  testthat::expect_equal(class(res), 'data.frame')
  testthat::expect_equal(nrow(res), 2)
})
test_that("checkPlpInput file", {
  res <- checkPlpInput(file.path(saveLoc,"mult"))
  testthat::expect_equal(res, 'file')
})

valMult <- evaluateMultiplePlp(analysesLocation = file.path(saveLoc,"mult"),
                               outputLocation = file.path(saveLoc,'mult/Validation'),
                               connectionDetails = connectionDetails, 
                               validationSchemaTarget = ohdsiDatabaseSchema,
                               validationSchemaOutcome = ohdsiDatabaseSchema,
                               validationSchemaCdm = cdmDatabaseSchema, 
                               databaseNames = 'test',
                               validationTableTarget = 'cohorts',
                               validationTableOutcome = 'outs_test',
                               validationIdTarget = 1,
                               validationIdOutcome = 2,
                               oracleTempSchema = NULL,
                               verbosity = 'INFO',
                               keepPrediction = F,
                               sampleSize = NULL)
test_that("evaluateMultiplePlp", {
testthat::expect_equal(dir.exists(file.path(saveLoc,'mult/Validation/test')), T)

})

