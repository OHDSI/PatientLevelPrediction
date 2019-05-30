# @file PatientLevelPrediction.R
#
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

#' PatientLevelPrediction
#'
#' @docType package
#' @name PatientLevelPrediction
#' @importFrom Rcpp evalCpp
#' @importFrom SqlRender loadRenderTranslateSql translateSql
#' @importFrom plyr ddply
#' @importFrom methods is
#' @importFrom stats binom.test lm printCoefmat rpois runif sd aggregate
#' @importFrom utils write.csv write.table data ?
#' @importFrom grDevices dev.control dev.off pdf recordPlot rgb
#' @importFrom magrittr %>%
#' @importFrom foreach %dopar% %do%
#' @import bit
#' @import ffbase
#' @import Cyclops
#' @import DatabaseConnector
#' @import FeatureExtraction
#' @import plotly
#' @useDynLib PatientLevelPrediction
NULL

#' A simulation profile
#' @docType data
#' @keywords datasets
#' @name plpDataSimulationProfile
#' @format A data frame containing the following elements:
#' \describe{
#'   \item{covariatePrevalence}{prevalence of all covariates}
#'   \item{outcomeModels}{regression model parameters to simulate outcomes}
#'   \item{metaData}{settings used to simulate the profile}
#'   \item{covariateRef}{covariateIds and covariateNames}
#'   \item{timePrevalence}{time window}
#'   \item{exclusionPrevalence}{prevalence of exclusion of covariates}
#' }
#' @usage
#' data(plpDataSimulationProfile)
NULL


.onLoad <- function(libname, pkgname) {
  # Copied this from the ff package:
  if (is.null(getOption("ffbatchbytes"))) {
    # memory.limit is windows specific
    if (.Platform$OS.type == "windows")
    {
      if (getRversion() >= "2.6.0")  # memory.limit was silently changed from 2.6.0 to return in MB instead of bytes
        options(ffbatchbytes =  utils::memory.limit()*(1024^2 / 100))
      else
        options(ffbatchbytes =  utils::memory.limit() / 100)
    } else {
      # some magic constant
      options(ffbatchbytes = 16*1024^2)
    }
  }
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
  options(ffbatchbytes = min(getOption("ffbatchbytes"), .Machine$integer.max / 10))
  options(ffmaxbytes = min(getOption("ffmaxbytes"), .Machine$integer.max * 12))
}
