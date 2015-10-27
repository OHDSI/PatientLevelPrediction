# @file PatientLevelPrediction.R
#
# Copyright 2015 Observational Health Data Sciences and Informatics
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

#' PatientLevelPrediction
#'
#' @docType package
#' @name PatientLevelPrediction
#' @importFrom Rcpp evalCpp
#' @importFrom SqlRender loadRenderTranslateSql translateSql
#' @importFrom RJDBC dbDisconnect
#' @importFrom plyr ddply
#' @import bit
#' @import Cyclops
#' @import DatabaseConnector
#' @useDynLib PatientLevelPrediction
NULL

#' A simulation profile
#' @docType data
#' @keywords datasets
#' @name plpDataSimulationProfile
#' @usage
#' data(plpDataSimulationProfile)
NULL


.onLoad <- function(libname, pkgname) {
  # Copied this from the ff package:
  if (is.null(getOption("ffmaxbytes"))) {
    # memory.limit is windows specific
    if (.Platform$OS.type == "windows") {
      if (getRversion() >= "2.6.0")
        options(ffmaxbytes = 0.5 * utils::memory.limit() * (1024^2)) else options(ffmaxbytes = 0.5 * utils::memory.limit())
    } else {
      # some magic constant
      options(ffmaxbytes = 0.5 * 1024^3)
    }
  }

  # Workaround for problem with ff on machines with lots of memory (see
  # https://github.com/edwindj/ffbase/issues/37)
  options(ffmaxbytes = min(getOption("ffmaxbytes"), .Machine$integer.max * 12))
}
