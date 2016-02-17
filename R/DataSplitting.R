# @file DataSplitting.R
#
# Copyright 2016 Observational Health Data Sciences and Informatics
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
#' @param plpData   An object of type \code{plpData}.
#' @param splits    This can be either a single integer, in which case the data will be split up into
#'                  equally sized parts. If a vector is provided instead, these are interpreted as the
#'                  relative sizes of each part.
#'
#' @return
#' A list with entries for each part. An entry itself is a plpData object.
#'
#' @export
splitData <- function(plpData, splits = 2) {
  start <- Sys.time()

  if (length(splits) == 1)
    splits <- rep(1/splits, splits)
  splits <- cumsum(splits)
  rows <- data.frame(rowId = 1:nrow(plpData$cohorts), rnd = runif(nrow(plpData$cohorts)))
  q <- quantile(rows$rnd, probs = splits)
  groups <- ff::as.ff(cut(rows$rnd, breaks = c(0, q), labels = FALSE))
  result <- list()
  for (i in 1:length(splits)) {
    writeLines(paste("Creating data objects for group", i))
    sampledIndices <- ffbase::ffwhich(groups, groups == i)

    sampledCohorts <- plpData$cohorts[sampledIndices, ]
    sampledRowIds <- sampledCohorts$rowId

    idx <- ffbase::ffmatch(x = plpData$outcomes$rowId, table = sampledRowIds)
    idx <- ffbase::ffwhich(idx, !is.na(idx))
    sampledOutcomes <- plpData$outcomes[idx, ]

    idx <- ffbase::ffmatch(x = plpData$covariates$rowId, table = sampledRowIds)
    idx <- ffbase::ffwhich(idx, !is.na(idx))
    sampledCovariates <- plpData$covariates[idx, ]

    if (!is.null(plpData$exclude)) {
      idx <- ffbase::ffmatch(x = plpData$exclude$rowId, table = sampledRowIds)
      idx <- ffbase::ffwhich(idx, !is.na(idx))
      sampledExclude <- plpData$exclude[idx, ]
    } else {
      sampledExclude <- NULL
    }
    result[[i]] <- list(cohorts = sampledCohorts,
                        outcomes = sampledOutcomes,
                        exclude = sampledExclude,
                        covariates = sampledCovariates,
                        covariateRef = ff::clone.ffdf(plpData$covariateRef),
                        metaData = plpData$metaData)

    class(result[[i]]) <- "plpData"
  }
  delta <- Sys.time() - start
  writeLines(paste("Splitting data took", signif(delta, 3), attr(delta, "units")))
  return(result)
}
