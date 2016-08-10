# @file Plotting.R
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

#' Plot the ROC curve
#'
#' @details
#' Create a plot showing the Receiver Operator Characteristics (ROC) curve.
#'
#' @param prediction            A prediction object as generated using the
#'                              \code{\link{predictProbabilities}} function.
#' @param fileName              Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotRoc <- function(prediction, fileName = NULL) {
  if (attr(prediction, "metaData")$predictionType != "binary")
    stop("Plotting the ROC curve is only implemented for binary classification models")
  
  prediction <- prediction[order(-prediction$value), c("value", "outcomeCount")]
  prediction$sens <- cumsum(prediction$outcomeCount)/sum(prediction$outcomeCount)
  prediction$fpRate <- cumsum(prediction$outcomeCount == 0)/sum(prediction$outcomeCount == 0)
  data <- stats::aggregate(fpRate ~ sens, data = prediction, min)
  data <- stats::aggregate(sens ~ fpRate, data = data, min)
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
    ggplot2::geom_area(color = grDevices::rgb(0, 0, 0.8, alpha = 0.8),
                       fill = grDevices::rgb(0, 0, 0.8, alpha = 0.4)) +
    ggplot2::scale_x_continuous("1 - specificity") +
    ggplot2::scale_y_continuous("Sensitivity")
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 5, height = 4.5, dpi = 400)
  return(plot)
}



#' Plot the calibration
#'
#' @details
#' Create a plot showing the predicted probabilities and the observed fractions. Predictions are
#' stratefied into equally sized bins of predicted probabilities.
#'
#' @param strataData            A dataframe containing the calibration summary, 
#'                              returned by running the \code{getCalibration} function
#' @param fileName              Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotCalibration <- function(strataData, fileName){
  # TODO: CHECK INPUT
  plot <- ggplot2::ggplot(strataData,
                          ggplot2::aes(xmin = minx, xmax = maxx, ymin = 0, ymax = fraction)) +
    ggplot2::geom_abline() +
    ggplot2::geom_rect(color = grDevices::rgb(0, 0, 0.8, alpha = 0.8),
                       fill = grDevices::rgb(0, 0, 0.8, alpha = 0.5)) +
    ggplot2::scale_x_continuous("Predicted probability") +
    ggplot2::coord_cartesian(xlim = attr(staraData,'lims')) +
    ggplot2::scale_y_continuous("Observed fraction")
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 5, height = 3.5, dpi = 400)
  return(plot)
}



# TODO
#=============
# do the box plot as sep function with predictionDistribution as input
## boxPlot <- ggplot2::ggplot(prediction, ggplot2::aes(x=as.factor(outcomeCount), y=value, 
##fill=as.factor(outcomeCount))) + ggplot2::geom_boxplot() +
##  ggplot2::guides(fill=FALSE) + ggplot2::xlab("Class") + ggplot2::ylab("Prediction") 
#=============
