# @file CovariatePrevalence.R
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

quickSum <- function(data, squared = FALSE) {
  if (squared) {
    x <- PatientLevelPrediction::bySumFf(data$covariateValue^2, data$covariateId)
    colnames(x) <- c("covariateId", "sumSqr")
  } else {
    x <- PatientLevelPrediction::bySumFf(data$covariateValue, data$covariateId)
    colnames(x) <- c("covariateId", "sum")
  }
  x$covariateId <- as.numeric(x$covariateId)
  return(x)
}

computeStats <- function(n, covariates, label = NULL) {
  stats <- quickSum(covariates)
  statsSqr <- quickSum(covariates, squared = TRUE)
  stats <- merge(stats, statsSqr)
  stats$sd <- sqrt((stats$sumSqr - (stats$sum^2/n))/n)
  stats$mean <- stats$sum/n
  stats$n <- n
  stats$sumSqr <- NULL
  if (!is.null(label)) {
    names(stats)[names(stats) != "covariateId"] <- paste(names(stats)[names(stats) != "covariateId"], label, sep = "_")
  }
  return(stats)
}


#' Compute covariate means
#'
#' @param plpData     An object of type \code{plpData}.
#' @param cohortId    The ID of the specific cohort for which to compute the means.
#' @param outcomeId   The ID of the specific outcome for which to compute the subgroup means.
#'
#' @export
computeCovariateMeans <- function(plpData, cohortId = NULL, outcomeId = NULL) {
  if (is.null(cohortId) && length(plpData$metaData$cohortIds) != 1)
    stop("No cohort ID specified, but multiple cohorts found")
  if (is.null(outcomeId) && length(plpData$metaData$outcomeIds) != 1)
    stop("No outcome ID specified, but multiple outcomes found")

  start <- Sys.time()

  covariates <- plpData$covariates
  cohorts <- plpData$cohorts
  outcomes <- plpData$outcomes

  if (!is.null(cohortId) && length(plpData$metaData$cohortIds) > 1) {
    # Filter by cohort ID:
    t <- cohorts$cohortId == cohortId
    if (!ffbase::any.ff(t)) {
      stop(paste("No cohorts with cohort ID", cohortId))
    }
    cohorts <- cohorts[ffbase::ffwhich(t, t == TRUE), ]

    idx <- ffbase::ffmatch(x = covariates$rowId, table = cohorts$rowId)
    idx <- ffbase::ffwhich(idx, !is.na(idx))
    covariates <- covariates[idx, ]

    # No need to filter outcomes since we'll merge outcomes with cohorts later
  }

  if (!is.null(outcomeId) && length(plpData$metaData$outcomeIds) > 1) {
    # Filter by outcome ID:
    t <- outcomes$outcomeId == outcomeId
    if (!ffbase::any.ff(t)) {
      stop(paste("No outcomes with outcome ID", outcomeId))
    }
    outcomes <- outcomes[ffbase::ffwhich(t, t == TRUE), ]
  }

  writeLines("Computing overall stats")
  stats <- computeStats(nrow(cohorts), covariates, "overall")
  if (!is.null(outcomes)) {
    writeLines("Computing subgroup stats")
    matched <- !ffbase::is.na.ff(ffbase::ffmatch(x = covariates$rowId, table = outcomes$rowId))
    nOutcomes <- nrow(outcomes)
    covariatesSubset <- covariates[ffbase::ffwhich(matched, matched == TRUE), ]
    statsWithOutcome <- computeStats(nOutcomes, covariatesSubset, "with_outcome")

    nNotOutcomes <- nrow(cohorts) - nOutcomes
    covariatesSubset <- covariates[ffbase::ffwhich(matched, matched == FALSE), ]
    statsWithoutOutcome <- computeStats(nNotOutcomes, covariatesSubset, "without_outcome")

    stats <- merge(stats, statsWithOutcome, all.x = TRUE)
    stats <- merge(stats, statsWithoutOutcome, all.x = TRUE)
    stats$std_difference <- (stats$mean_with_outcome - stats$mean_without_outcome)/sqrt((stats$sd_with_outcome^2 + stats$sd_without_outcome^2)/2)
  }
  stats <- merge(stats, ff::as.ram(plpData$covariateRef))
  delta <- Sys.time() - start
  writeLines(paste("Computing covariate means took", signif(delta, 3), attr(delta, "units")))
  return(stats)
}

.truncRight <- function(x, n) {
  nc <- nchar(x)
  x[nc > (n - 3)] <- paste("...",
                           substr(x[nc > (n - 3)], nc[nc > (n - 3)] - n + 1, nc[nc > (n - 3)]),
                           sep = "")
  x
}

#' Plot variables with largest standardized difference
#'
#' @description
#' Create a plot showing those variables having the largest standardized difference between the group
#' having the outcome and the group that doesn't have the outcome. Requires running
#' \code{computeCovariateMeans} first.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @param means          A data frame created by the \code{computeCovariateMeans} funcion.
#' @param n              Count of variates to plot.
#' @param maxNameWidth   Covariate names longer than this number of characters are truncated to create
#'                       a nicer plot.
#' @param fileName       Name of the file where the plot should be saved, for example 'plot.png'. See
#'                       the function \code{ggsave} in the ggplot2 package for supported file formats.
#'
#' @export
plotCovariateDifferenceOfTopVariables <- function(means,
                                                  n = 20,
                                                  maxNameWidth = 100,
                                                  fileName = NULL) {
  top <- means[order(-abs(means$std_difference)), ]
  top <- top[1:n, ]

  data <- data.frame(covariateId = top$covariateId,
                     covariate = top$covariateName,
                     difference = top$std_difference,
                     rowId = rep(nrow(top):1, 2))
  top$covariateName <- .truncRight(as.character(top$covariateName), maxNameWidth)
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = difference, y = rowId)) +
          ggplot2::geom_point(color = rgb(0, 0, 0.8, alpha = 0.5),
                              fill = rgb(0, 0, 0.8, alpha = 0.5)) +
          ggplot2::geom_vline(xintercept = 0) +
          ggplot2::scale_x_continuous("Standardized difference of mean") +
          ggplot2::scale_y_continuous(breaks = nrow(top):1, labels = top$covariateName) +
          ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7),
                         axis.title.y = ggplot2::element_blank(),
                         legend.position = "top",
                         legend.direction = "vertical",
                         legend.title = ggplot2::element_blank())
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 10, height = max(2 + n * 0.1, 5), dpi = 400)
  return(plot)
}

