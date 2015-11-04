# @file GetTextCovariates.R
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

#' Get text covariate information from the database
#'
#' @description
#' Uses a bag-of-words approach to construct covariates based on free-text.
#'
#' @param covariateSettings   An object of type \code{covariateSettings} as created using the
#'                            \code{\link{createTextCovariateSettings}} function.
#'
#' @template GetCovarParams
#'
#' @export
getDbTextCovariateData <- function(connection,
                                   oracleTempSchema = NULL,
                                   cdmDatabaseSchema,
                                   cdmVersion = "4",
                                   cohortTempTable = "cohort_person",
                                   rowIdField = "subject_id",
                                   covariateSettings) {
  # Todo: implement this function
}

#' Create text covariate settings
#'
#' @details
#' creates an object specifying how covariates should be constructed from text in notes table in the
#' CDM model.
#'
#' @param language                     Specify the language of the free-text.
#' @param removeNegations              Remove negated text prior to constructing features.
#' @param deleteCovariatesSmallCount   A numeric value used to remove covariates that occur in both
#'                                     cohorts fewer than deleteCovariateSmallCounts time.
#'
#' @return
#' An object of type \code{covariateSettings}, to be used in other functions.
#'
#' @export
createTextCovariateSettings <- function(language = "eng",
                                        removeNegations = TRUE,
                                        deleteCovariatesSmallCount = 100) {
  # First: get the default values:
  covariateSettings <- list()
  for (name in names(formals(createTextCovariateSettings))) {
    covariateSettings[[name]] <- get(name)
  }
  # Next: overwrite defaults with actual values if specified:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(covariateSettings))
      covariateSettings[[name]] <- values[[name]]
  }

  attr(covariateSettings, "fun") <- "getDbTextCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}
