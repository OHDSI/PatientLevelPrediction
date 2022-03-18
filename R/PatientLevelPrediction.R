# @file PatientLevelPrediction.R
#
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

#' PatientLevelPrediction
#' 
#' @description A package for running predictions using data in the OMOP CDM
#'
#' @docType package
#' @name PatientLevelPrediction
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @import FeatureExtraction
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

