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
context("FfHelperFunctions.R")

test_that("in.ff", {
  vect1 <- sample(100, 40)
  vect2 <- sample(100, 20)
  testthat::expect_equal(sum(PatientLevelPrediction:::in.ff(ff::as.ff(vect1), ff::as.ff(vect2))),
                         sum(vect1%in%vect2))
})



test_that("in.ff", {
  vect1 <- rep(F, 10)
  vect2 <- vect1
  vect2[1] <- T
  testthat::expect_equal(PatientLevelPrediction:::any.ff(ff::as.ff(vect1)), F)
  testthat::expect_equal(PatientLevelPrediction:::any.ff(ff::as.ff(vect2)), T)
})




test_that("calculatePrevs", {
  pop <- data.frame(rowId = 1:10,
                    outcomeCount = c(rep(0,5), rep(1,5)))
  N <- sample(9,1)
  covariates <- data.frame(rowId = sample(10, N),
                           covariateId = as.factor(c(rep(101, N))))
  covariates <- ff::as.ffdf(covariates)
  data <- list(covariates = covariates)
  res <- PatientLevelPrediction:::calculatePrevs(plpData = data, population= pop)
  
  testthat::expect_equal(res$covariateId, '101')
  testthat::expect_equal(res$prev.out, sum(merge(pop, ff::as.ram(covariates), by='rowId')[,2]==1)/5)
  testthat::expect_equal(res$prev.noout, sum(merge(pop, ff::as.ram(covariates), by='rowId')[,2]==0)/5)
  
})


# can't test clearffTempDir or checkffFolder?
