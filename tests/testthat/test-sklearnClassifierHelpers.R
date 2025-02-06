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

test_that("listCartesian works", {
  allList <- list(a = list(1, 2), b = list(NULL, "auto"), c = list(-1))

  paramLists <- listCartesian(allList)

  expect_equal(length(paramLists), 2 * 2 * 1)
  expect_equal(names(paramLists[[1]]), c("a", "b", "c"))
  expect_equal(length(paramLists[[1]]), 3)
})
