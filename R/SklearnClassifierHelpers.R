# @file SklearnClassifierHelpers.R
#
# Copyright 2022 Observational Health Data Sciences and Informatics
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

#' Cartesian product
#' 
#' Computes the Cartesian product of all the combinations of elements in a list
#' 
#' @param allList a list of lists
#' @return A list with all possible combinations from the input list of lists
#' @examples
#' listCartesian(list(list(1, 2), list(3, 4)))
#' @export
listCartesian <- function(allList) {
  combinations <- expand.grid(allList)
  results <- lapply(seq_len(nrow(combinations)),
                    function(i) lapply(combinations, function(x) x[i][[1]]))
  return(results)
}
