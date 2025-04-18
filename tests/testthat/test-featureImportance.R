# Copyright 2025 Observational Health Data Sciences and Informatics
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


test_that("pfi feature importance returns data.frame", {
  skip_if_offline()
  # limit to a sample of 2 covariates for faster test
  covariates <- plpResult$model$covariateImportance %>%
    dplyr::filter("covariateValue" != 0) %>%
    dplyr::select("covariateId") %>%
    dplyr::arrange(desc("covariateValue")) %>%
    dplyr::pull()

  # if the model had non-zero covariates
  if (length(covariates) > 0) {
    if (length(covariates) > 2) {
      covariates <- covariates[1:2]
    }
    pfiTest <- pfi(plpResult, population, plpData,
      repeats = 1,
      covariates = covariates, cores = 1, log = NULL,
      logthreshold = "INFO"
    )

    expect_equal(class(pfiTest), "data.frame")
    expect_equal(sum(names(pfiTest) %in% c("covariateId", "pfi")), 2)
    expect_true(all(!is.nan(pfiTest$pfi)))
  }
})

test_that("pfi feature importance works with logger or without covariates", {
  skip_if_offline()
  pfiTest <- pfi(tinyResults, population, nanoData,
    cores = 1,
    covariates = NULL, log = file.path(tempdir(), "pfiLog")
  )

  expect_equal(class(pfiTest), "data.frame")
  expect_equal(sum(names(pfiTest) %in% c("covariateId", "pfi")), 2)
  expect_true(all(!is.nan(pfiTest$pfi)))
})
