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
context("HelperFunctions.R")

test_that("is_installed", {
  testthat::expect_equal(PatientLevelPrediction:::is_installed('FeatureExtraction'), T)
  testthat::expect_equal(PatientLevelPrediction:::is_installed('MadeUp4u834t3f'), F)
})

test_that("ensure_installed", {
  testthat::expect_equal(PatientLevelPrediction:::ensure_installed('FeatureExtraction'), NULL)
})

# how to test checkPlpInstallation?

test_that("interpretInstallCode", {
  testthat::expect_equal(PatientLevelPrediction:::interpretInstallCode(1), NULL)
})

test_that("clearLoggerType", {
  testthat::expect_equal(PatientLevelPrediction:::clearLoggerType('missing12134'), NULL)
})

test_that("createTempModelLoc", {
  testthat::expect_equal(class(PatientLevelPrediction:::createTempModelLoc()), "character")
})

list1 <- list(a=1:2, b=5:6)
list2 <- list(c=1:5)
test_that("listAppend", {
  testthat::expect_equal(length(PatientLevelPrediction::listAppend(list1, list2)), 3)
})

# how to test configurePython?

test_that("setPythonEnvironment", {
  testthat::expect_error(PatientLevelPrediction::setPythonEnvironment(envname='madeup34343'))
  testthat::expect_equal(class(PatientLevelPrediction::setPythonEnvironment(envname='madeup34343', envtype = 'conda')), "character")
})


# getOs test?
