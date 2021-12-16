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

context("Ensemble")

if(F){
ensemble <- runEnsembleModel(population = population,
                             dataList = list(plpData, plpData),
                             modelList = list(lrSet, gbmSet), # change to get rid of warning?
                             testSplit = "subject",
                             testFraction = 0.2, 
                             stackerUseCV = T,
                             splitSeed = 1,
                             nfold = 3,
                             saveDirectory= saveLoc,
                             saveEnsemble = F,
                             savePlpData=F, 
                             savePlpResult=F, 
                             savePlpPlots = F, 
                             saveEvaluation = F,
                             analysisId = 'ensemble',
                             verbosity = "INFO",
                             ensembleStrategy = "stacked")

test_that("run ensemble model", {
  
  testthat::expect_s3_class(ensemble, 'ensemblePlp')
  })

test_that("combine mean ensemble model works", {
  comEn <- createEnsemble(runPlpList = list(plpResult,plpDataReal))
  testthat::expect_s3_class(comEn, 'ensemblePlp')
})

test_that("combine AUC ensemble model works", {
  comEn <- createEnsemble(runPlpList = list(plpResult,plpDataReal), weighted = T)
  testthat::expect_s3_class(comEn, 'ensemblePlp')
})

test_that("combine manual weights ensemble model works", {
  comEn <- createEnsemble(runPlpList = list(plpResult,plpDataReal), weighted = T, weights = runif(2))
  testthat::expect_s3_class(comEn, 'ensemblePlp')
})

test_that("combine ensemble model fails when weights too long", {
  testthat::expect_error(createEnsemble(runPlpList = list(plpResult,plpDataReal), weighted = T, weights = runif(3)))
})


test_that("apply ensemble model", {
  ensemblePerf <- applyEnsembleModel(population = population,
                                     dataList = list(plpData,plpData),
                                     ensembleModel = ensemble, 
                                     calculatePerformance = T)
  
  testthat::expect_equal(class(ensemblePerf), 'list')
  testthat::expect_equal(sum(names(ensemblePerf)%in%c('prediction','performanceEvaluation')), 2)
  testthat::expect_s3_class(ensemblePerf$prediction, 'data.frame')
  testthat::expect_equal(class(ensemblePerf$performanceEvaluation), 'plpEvaluation')

})


test_that("save/load ensemble model", {
  saveEnsemblePlpModel(ensembleModel = ensemble$model, dirPath = file.path(saveLoc, 'ensembleSave'))
  
  testthat::expect_equal(dir.exists(file.path(saveLoc,'ensembleSave/level1')), T)
  testthat::expect_equal(dir.exists(file.path(saveLoc,'ensembleSave/level2')), T)
  
  ensembleModelLoad <- loadEnsemblePlpModel(file.path(saveLoc,'ensembleSave'))
  testthat::expect_equal(names(ensemble$model), names(ensembleModelLoad))
  
})


test_that("save/load ensemble result", {
  saveEnsemblePlpResult(ensembleResult = ensemble, dirPath = file.path(saveLoc,'ensembleResult'))
  
  testthat::expect_equal(file.exists(file.path(saveLoc,'ensembleResult/performanceEvaluation.rds')), T)
  testthat::expect_equal(file.exists(file.path(saveLoc,'ensembleResult/covariateSummary.rds')), T)
  
  ensembleLoad <- loadEnsemblePlpResult(file.path(saveLoc,'ensembleResult'))
  testthat::expect_equal(names(ensemble), names(ensembleLoad))
  
})

}
