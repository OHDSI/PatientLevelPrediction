# @file Evaluate.R
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

prepareDataForEval <- function(prediction, plpData, removeDropouts) {
  outcomeId <- attr(prediction, "outcomeId")

  outcomes <- plpData$outcomes
  prediction <- ff::as.ffdf(prediction)

  if (length(plpData$metaData$outcomeIds) > 1) {
    # Filter by outcome ID:
    t <- outcomes$outcomeId == outcomeId
    if (!ffbase::any.ff(t)) {
      stop(paste("No outcomes with outcome ID", outcomeId))
    }
    outcomes <- outcomes[ffbase::ffwhich(t, t == TRUE), ]
  }

  if (!is.null(plpData$exclude) && nrow(plpData$exclude) != 0) {
    # Filter subjects with previous outcomes:
    exclude <- plpData$exclude
    if (!is.null(outcomeId)) {
      t <- exclude$outcomeId == outcomeId
      if (ffbase::any.ff(t)) {
        exclude <- exclude[ffbase::ffwhich(t, t == TRUE), ]

        t <- ffbase::ffmatch(x = prediction$rowId, table = exclude$rowId, nomatch = 0L) > 0L
        if (ffbase::any.ff(t)) {
          prediction <- prediction[ffbase::ffwhich(t, t == FALSE), ]
        }

        t <- ffbase::ffmatch(x = outcomes$rowId, table = exclude$rowId, nomatch = 0L) > 0L
        if (ffbase::any.ff(t)) {
          outcomes <- outcomes[ffbase::ffwhich(t, t == FALSE), ]
        }
      }
    }
  }
  prediction <- merge(prediction, outcomes, all.x = TRUE)
  prediction <- ff::as.ram(prediction)
  prediction$outcomeCount[!is.na(prediction$outcomeCount)] <- 1
  prediction$outcomeCount[is.na(prediction$outcomeCount)] <- 0
  if (removeDropouts) {
    fullWindowLength <- ffbase::max.ff(plpData$cohorts$time)
    prediction <- merge(prediction, ff::as.ram(plpData$cohorts)[, c("rowId", "time")])
    prediction <- prediction[prediction$outcomeCount != 0 | prediction$time == fullWindowLength, ]
  }
  return(prediction)
}

#' Compute the area under the ROC curve
#'
#' @details
#' Computes the area under the ROC curve for the predicted probabilities, given the true observed
#' outcomes.
#'
#' @param prediction            A prediction object as generated using the
#'                              \code{\link{predictProbabilities}} function.
#' @param plpData               An object of type \code{plpData}.
#' @param removeDropoutsForLr   If TRUE and modelType is "logistic", subjects that do not have the full
#'                              observation window (i.e. are censored earlier) and do not have the
#'                              outcome are removed prior to evaluating the model.
#' @param confidenceInterval    Should 95 percebt confidence intervals be computed?
#'
#' @export
computeAuc <- function(prediction,
                       plpData,
                       removeDropoutsForLr = TRUE,
                       confidenceInterval = FALSE) {
  if (attr(prediction, "modelType") != "logistic")
    stop("Computing AUC is only implemented for logistic models")

  prediction <- prepareDataForEval(prediction, plpData, removeDropoutsForLr)

  if (confidenceInterval) {
    auc <- .Call("PatientLevelPrediction_aucWithCi",
                 PACKAGE = "PatientLevelPrediction",
                 prediction$value,
                 prediction$outcomeCount)
    return(data.frame(auc = auc[1], auc_lb95ci = auc[2], auc_lb95ci = auc[3]))
  } else {
    auc <- .Call("PatientLevelPrediction_auc",
                 PACKAGE = "PatientLevelPrediction",
                 prediction$value,
                 prediction$outcomeCount)
    return(auc)
  }
}

#' Compute the area under the ROC curve
#'
#' @details
#' Computes the area under the ROC curve for the predicted probabilities, given the true observed
#' outcomes.
#'
#' @param prediction           A vector with the predicted hazard rate.
#' @param status               A vector with the status of 1 (event) or 0 (no event).
#' @param time                 Only for survival models: a vector with the time to event or censor
#'                             (which ever comes first).
#' @param confidenceInterval   Should 95 percebt confidence intervals be computed?
#' @param timePoint            Only for survival models: time point when the AUC should be evaluated
#' @param modelType            Type of model. Currently supported are "logistic" and "survival".
#'
#' @export
computeAucFromDataFrames <- function(prediction,
                                     status,
                                     time = NULL,
                                     confidenceInterval = FALSE,
                                     timePoint,
                                     modelType = "logistic") {
  if (modelType == "survival" & confidenceInterval)
    stop("Currently not supporting confidence intervals for survival models")

  if (modelType == "survival") {
    Surv.rsp <- survival::Surv(time, status)
    Surv.rsp.new <- Surv.rsp
    if (missing(timePoint))
      timePoint <- max(time[status == 1])
    auc <- survAUC::AUC.uno(Surv.rsp, Surv.rsp.new, prediction, timePoint)$auc
    return(auc * auc)
  } else {
    if (confidenceInterval) {
      auc <- .Call("PatientLevelPrediction_aucWithCi",
                   PACKAGE = "PatientLevelPrediction",
                   prediction,
                   status)
      return(data.frame(auc = auc[1], auc_lb95ci = auc[2], auc_lb95ci = auc[3]))
    } else {
      auc <- .Call("PatientLevelPrediction_auc",
                   PACKAGE = "PatientLevelPrediction",
                   prediction,
                   status)
      return(auc)
    }
  }
}

#' Plot the calibration
#'
#' @details
#' Create a plot showing the predicted probabilities and the observed fractions. Predictions are
#' stratefied into equally sized bins of predicted probabilities.
#'
#' @param prediction            A prediction object as generated using the
#'                              \code{\link{predictProbabilities}} function.
#' @param plpData               An object of type \code{plpData}.
#' @param removeDropoutsForLr   If TRUE and modelType is "logistic", subjects that do not have the full
#'                              observation window (i.e. are censored earlier) and do not have the
#'                              outcome are removed prior to evaluating the model.
#' @param numberOfStrata        The number of strata in the plot.
#' @param truncateFraction      This fraction of probability values will be ignored when plotting, to
#'                              avoid the x-axis scale being dominated by a few outliers.
#' @param fileName              Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotCalibration <- function(prediction,
                            plpData,
                            removeDropoutsForLr = TRUE,
                            numberOfStrata = 5,
                            truncateFraction = 0.01,
                            fileName = NULL) {
  if (attr(prediction, "modelType") != "logistic")
    stop("Plotting the calibration is only implemented for logistic models")

  prediction <- prepareDataForEval(prediction, plpData, removeDropoutsForLr)

  q <- unique(quantile(prediction$value, (1:(numberOfStrata - 1))/numberOfStrata))
  prediction$strata <- cut(prediction$value,
                           breaks = c(0, q, max(prediction$value)),
                           labels = FALSE)
  computeStratumStats <- function(data) {
    return(data.frame(minx = min(data$value),
                      maxx = max(data$value),
                      fraction = sum(data$outcomeCount)/nrow(data)))
  }
  # strataData <- plyr::ddply(prediction, prediction$strata, computeStratumStats)
  counts <- aggregate(outcomeCount ~ strata, data = prediction, sum)
  names(counts)[2] <- "counts"
  backgroundCounts <- aggregate(personId ~ strata, data = prediction, length)
  names(backgroundCounts)[2] <- "backgroundCounts"
  minx <- aggregate(value ~ strata, data = prediction, min)
  names(minx)[2] <- "minx"
  maxx <- aggregate(value ~ strata, data = prediction, max)
  names(maxx)[2] <- "maxx"
  strataData <- merge(counts, backgroundCounts)
  strataData <- merge(strataData, minx)
  strataData <- merge(strataData, maxx)
  strataData$fraction <- strataData$counts/strataData$backgroundCounts
  lims <- quantile(prediction$value, c(truncateFraction, 1 - truncateFraction))
  plot <- ggplot2::ggplot(strataData,
                          ggplot2::aes(xmin = minx, xmax = maxx, ymin = 0, ymax = fraction)) +
          ggplot2::geom_abline() +
          ggplot2::geom_rect(color = rgb(0, 0, 0.8, alpha = 0.8),
                             fill = rgb(0, 0, 0.8, alpha = 0.5)) +
          ggplot2::scale_x_continuous("Predicted probability") +
          ggplot2::coord_cartesian(xlim = lims) +
          ggplot2::scale_y_continuous("Observed fraction")
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 5, height = 3.5, dpi = 400)
  return(plot)
}


#' Plot the ROC curve
#'
#' @details
#' Create a plot showing the Receiver Operator Characteristics (ROC) curve.
#'
#' @param prediction            A prediction object as generated using the
#'                              \code{\link{predictProbabilities}} function.
#' @param plpData               An object of type \code{plpData}.
#' @param removeDropoutsForLr   If TRUE and modelType is "logistic", subjects that do not have the full
#'                              observation window (i.e. are censored earlier) and do not have the
#'                              outcome are removed prior to evaluating the model.
#' @param fileName              Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotRoc <- function(prediction, plpData, removeDropoutsForLr = TRUE, fileName = NULL) {
  if (attr(prediction, "modelType") != "logistic")
    stop("Plotting the ROC curve is only implemented for logistic models")

  prediction <- prepareDataForEval(prediction, plpData, removeDropoutsForLr)

  prediction <- prediction[order(-prediction$value), c("value", "outcomeCount")]
  prediction$sens <- cumsum(prediction$outcomeCount)/sum(prediction$outcomeCount)
  prediction$fpRate <- cumsum(prediction$outcomeCount == 0)/sum(prediction$outcomeCount == 0)
  data <- aggregate(fpRate ~ sens, data = prediction, min)
  data <- aggregate(sens ~ fpRate, data = data, min)
  data <- rbind(data, data.frame(fpRate = 1, sens = 1))
  if (nrow(data) < 10000) {
    # Turn it into a step function:
    steps <- data.frame(sens = data$sens[1:(nrow(data) - 1)],
                        fpRate = data$fpRate[2:nrow(data)] - 1e-09)
    data <- rbind(data, steps)
    data <- data[order(data$sens, data$fpRate), ]
  }
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = fpRate, y = sens)) +
          ggplot2::geom_abline(intercept = 0, slope = 1) +
          ggplot2::geom_area(color = rgb(0, 0, 0.8, alpha = 0.8),
                             fill = rgb(0, 0, 0.8, alpha = 0.4)) +
          ggplot2::scale_x_continuous("1 - specificity") +
          ggplot2::scale_y_continuous("Sensitivity")
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 5, height = 4.5, dpi = 400)
  return(plot)
}
