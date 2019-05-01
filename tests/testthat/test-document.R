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
context("Document.R")
# Test unit for the document creation 

test_that("document creation parameters", {
  
  #test createPlpDocument inputs
  expect_error(createPlpJournalDocument(plpResult=NULL))
  
  expect_error(createPlpJournalDocument(plpResult=1:5))
  
  plpResult <- list(1)
  class(plpResult) <- 'plpModel'
  # target name not character
  expect_error(createPlpJournalDocument(plpResult=plpResult, targetName=1))
  
  # outcomeName not character
  expect_error(createPlpJournalDocument(plpResult=plpResult, targetName='target test', outcomeName=1))
  
  # characterisationSettings not list
  expect_error(createPlpJournalDocument(plpResult=plpResult, targetName='target test', 
                                        outcomeName='outcome test',characterisationSettings=1 ))
  
  # includeTrain not logical
  expect_error(createPlpJournalDocument(plpResult=plpResult, targetName='target test', 
                                        outcomeName='outcome test',characterisationSettings=list(),
                                        includeTrain='Y'))
  
  # includeTest not logical
  expect_error(createPlpJournalDocument(plpResult=plpResult, targetName='target test', 
                                        outcomeName='outcome test',characterisationSettings=list(),
                                        includeTrain=T, includeTest='Y'))
  
  # includePredictionPicture not logical
  expect_error(createPlpJournalDocument(plpResult=plpResult, targetName='target test', 
                                        outcomeName='outcome test',characterisationSettings=list(),
                                        includeTrain=T, includeTest=T, includePredictionPicture='Y'))
  # includeAttritionPlot not logical
  expect_error(createPlpJournalDocument(plpResult=plpResult, targetName='target test', 
                                        outcomeName='outcome test',characterisationSettings=list(),
                                        includeTrain=T, includeTest=T, includePredictionPicture=T, 
                                        includeAttritionPlot='Y'))
  
  
  #set.seed(1234)
  #data(plpDataSimulationProfile)
  #sampleSize <- 2000
  #plpData <- PatientLevelPrediction::simulatePlpData(plpDataSimulationProfile, n = sampleSize)
  #population <- PatientLevelPrediction::createStudyPopulation(plpData, outcomeId=2,
  #                                                            riskWindowEnd = 365)
  #modelset <- PatientLevelPrediction::setLassoLogisticRegression()
  #plpResult <- PatientLevelPrediction::runPlp(population, plpData, modelset, saveModel = F)
  #doc <- PatientLevelPrediction::createPlpJournalDocument(plpResult=plpResult, plpData = plpData, 
  #                                                        targetName='target test', 
  #                                                 outcomeName='outcome test',
  #                                                 includeTrain=T, includeTest=T, 
  #                                                 includePredictionPicture=T, 
  #                                                 includeAttritionPlot=T)
  #expect_equal(doc, TRUE)
  ## clean up
  #file.remove(file.path(getwd(), 'plp_journal_document.docx'))
  
})

data(plpDataSimulationProfile)
sampleSize <- 2000
plpData <- PatientLevelPrediction::simulatePlpData(plpDataSimulationProfile, n = sampleSize)
population <- PatientLevelPrediction::createStudyPopulation(plpData, outcomeId=2,
                                                            riskWindowEnd = 365)
modelset <- PatientLevelPrediction::setCoxModel()
plpResult <- PatientLevelPrediction::runPlp(population=population, 
                                            plpData=plpData, modelSettings = modelset, 
                                            savePlpData = F, saveEvaluation = F, 
                                            savePlpResult = F, 
                                            savePlpPlots = F, verbosity = 'NONE')

test_that("createPlpJournalDocument document works", {
  
  doc <- PatientLevelPrediction::createPlpJournalDocument(plpResult=plpResult, plpData = plpData, 
                                                          targetName='target test', 
                                                          outcomeName='outcome test',
                                                          includeTrain=T, includeTest=T, 
                                                          includePredictionPicture=T, 
                                                          includeAttritionPlot=T, save=F)
  expect_equal(class(doc), "rdocx")
  
})

test_that("createPlpReport document works", {
  doc <- createPlpReport(plpResult=plpResult, plpValidation=NULL,
                         plpData = plpData,
                         targetName = '<target population>',
                         outcomeName = '<outcome>',
                         targetDefinition = NULL,
                         outcomeDefinition = NULL,
                         outputLocation=file.path(getwd(), 'plp_report.docx'),
                         save = F)
  
  expect_equal(class(doc), "rdocx")
  
})