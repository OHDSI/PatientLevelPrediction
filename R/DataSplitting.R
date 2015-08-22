# @file DataSplitting.R

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

#' Split data into random subsets
#'
#' @details
#' Splits cohort, covariate, and outcome data into random subsets, to be used for validation.
#'
#' @param cohortData      An object of type \code{cohortData}.
#' @param covariateData   An object of type \code{covariateData}.
#' @param outcomeData     An object of type \code{outcomeData}.
#' @param splits          This can be either a single integer, in which case the data will be split up
#'                        into equally sized parts. If a vector is provided instead, these are
#'                        interpreted as the relative sizes of each part.
#'
#' @return
#' A list with entries for each part. An entry itself is a list containing a cohortData,
#' covariateData, and outcomeData object.
#'
#' @export
splitData <- function(cohortData, covariateData, outcomeData, splits = 2) {
  if (length(splits) == 1)
    splits <- rep(1/splits, splits)
  splits <- cumsum(splits)
  rows <- data.frame(rowId = 1:nrow(cohortData$cohorts), rnd = runif(nrow(cohortData$cohorts)))
  q <- quantile(rows$rnd, probs = splits)
  groups <- ff::as.ff(cut(rows$rnd, breaks = c(0, q), labels = FALSE))
  result <- list()
  for (i in 1:length(splits)) {
    writeLines(paste("Creating data objects for group", i))
    sampledRowIds <- ffbase::ffwhich(groups, groups == i)
    sampledCohorts <- cohortData$cohorts[sampledRowIds, ]
    sampledcohortData <- list(cohorts = sampledCohorts, metaData = cohortData$metaData)
    class(sampledcohortData) <- "cohortData"
    sampledOutcomes <- merge(outcomeData$outcomes, sampledCohorts)
    sampledOutcomes$time <- NULL
    sampledOutcomeData <- list(outcomes = sampledOutcomes, metaData = outcomeData$metaData)
    if (!is.null(outcomeData$exclude)) {
      sampledExclude <- merge(outcomeData$exclude, sampledCohorts)
      sampledExclude$time <- NULL
      sampledOutcomeData$exclude <- sampledExclude
    }
    class(sampledOutcomeData) <- "outcomeData"
    sampledCovariates <- merge(covariateData$covariates, sampledCohorts)
    sampledCovariateData <- list(covariates = sampledCovariates,
                                 covariateRef = ff::clone.ffdf(covariateData$covariateRef),
                                 metaData = covariateData$metaData)
    class(sampledCovariateData) <- "covariateData"
    result[[i]] <- list(cohortData = sampledcohortData,
                        covariateData = sampledCovariateData,
                        outcomeData = sampledOutcomeData)
  }
  return(result)
}
