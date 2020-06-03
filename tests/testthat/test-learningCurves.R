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
context("LearningCurves")

test_that("learningCurve", {
  testthat::expect_equal(class(learningCurve), "data.frame")
  testthat::expect_equal(sum(colnames(learningCurve)%in%c("Fraction",
                                                    "Time",
                                                    "Occurrences",
                                                    "Observations") ),4)
})

test_that("learningCurvePar", {
  learningCurvePar <- createLearningCurvePar(population = population, 
                                       plpData = plpData, 
                                       modelSettings = lrSet, 
                                       testSplit = 'stratified', 
                                       testFraction = 0.25, 
                                       trainFractions = c(0.5,0.6), 
                                       nfold = 3, 
                                       analysisId = 'learningCurvePar',
                                       saveDirectory =  saveLoc
  )
  testthat::expect_equal(class(learningCurvePar), "data.frame")
  testthat::expect_equal(colnames(learningCurve), colnames(learningCurvePar))
})




