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

# add limitCovariatesToPopulation(covariateData, rowIds) test
# batcheRestrict test
test_that("batchRestrict", {
  skip_if_offline()
  metaData <- attr(plpData$covariateData, "metaData")
  covariateData <- PatientLevelPrediction:::batchRestrict(plpData$covariateData, population, sizeN = 1000000)
  expect_s4_class(covariateData, "CovariateData")

  expect_equal(names(metaData), names(attr(covariateData, "metaData")))
})


# can't test clearffTempDir or checkffFolder?
