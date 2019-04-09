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

context("Attrition")

attrition <- data.frame(description = c('Original cohorts', 'First exposure only','Have time at risk'), 
                        targetCount = c(1000,900,900),
                        uniquePeople = c(100,900,900) , 
                        outcomes = c(10,9,9))
res <- drawAttritionDiagramPlp(attrition,
                               targetLabel = "Target Population",
                               outcomeLabel = "Outcome Count",
                               fileName = NULL)
test_that("attrition type", {

  # make sure class of this is a ggplot object
  testthat::expect_s3_class(res, "ggplot") 

  })


