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

test_that("createPlpJournalDocument creation parameters errors", {
  
  #test createPlpDocument inputs
  expect_error(createPlpJournalDocument(plpResult=NULL))
  
  expect_error(createPlpJournalDocument(plpResult=1:5))
  
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
  
})


test_that("createPlpJournalDocument document works", {
  
  doc <- createPlpJournalDocument(plpResult=plpResult, plpData = plpData, 
                                                          targetName='target test', 
                                                          outcomeName='outcome test',
                                                          includeTrain=T, includeTest=T, 
                                                          includePredictionPicture=T, 
                                                          includeAttritionPlot=T, save=F)
  expect_equal(class(doc), "rdocx")
  
})


test_that("createPlpReport creation parameters errors", {
  
  #test createPlpDocument inputs
  expect_error(createPlpReport(plpResult=NULL))
  
  expect_error(createPlpReport(plpResult=1:5))
  
  # target name not character
  expect_error(createPlpReport(plpResult=plpResult, targetName=1))
  
  # outcomeName not character
  expect_error(createPlpReport(plpResult=plpResult, targetName='target test', outcomeName=1))
  
  
})

test_that("createPlpReport document works without validation", {
  doc <- createPlpReport(plpResult=plpResult, plpValidation=NULL,
                         plpData = plpData,
                         targetName = '<target population>',
                         outcomeName = '<outcome>',
                         targetDefinition = NULL,
                         outcomeDefinition = NULL,
                         outputLocation=file.path(saveLoc, 'plp_report.docx'),
                         save = F)
  
  expect_equal(class(doc), "rdocx")
  
})

# add tests with validation (validation object in helper-objects.R)
# TODO...




# test get table 1 - need to make temp cohort table...
# getPlpTable <- function(cdmDatabaseSchema,oracleTempSchema, covariateSettings, longTermStartDays=-365,population, connectionDetails,cohortTable='#temp_person')
