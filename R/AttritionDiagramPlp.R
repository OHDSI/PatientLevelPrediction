# @file AttritionDiagramPlp.R
#
# Copyright 2019 Observational Health Data Sciences and Informatics
#
# This file is part of CohortMethod
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

#' Draw the attrition diagram
#'
#' @description
#' \code{drawAttritionDiagramPlp} draws the attition diagram, showing how many people were excluded from
#' the study population, and for what reasons.
#'
#' @param attrition         The table of attrition details return from the population attr(popualtion, 'metaData')$attrition
#' @param targetLabel       A label to us for the treated cohort.
#' @param outcomeLabel      A label to us for the comparator cohort.
#' @param fileName          Name of the file where the plot should be saved, for example 'plot.png'.
#'                          See the function \code{ggsave} in the ggplot2 package for supported file
#'                          formats.
#'
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
drawAttritionDiagramPlp <- function(attrition,
                                    targetLabel = "Target Population",
                                    outcomeLabel = "Outcome Count",
                                    fileName = NULL) {
  
  addStep <- function(data, attrition, row) {
    label <- paste(strwrap(as.character(attrition$description[row]), width = 30), collapse = "\n")
    data$leftBoxText[length(data$leftBoxText) + 1] <- label
    data$rightBoxText[length(data$rightBoxText) + 1] <- paste(targetLabel,
                                                              ": n = ",
                                                              data$targetCount - attrition$targetCount[row],
                                                              "\n",
                                                              'Unique people in target:',
                                                              ": n = ",
                                                              data$uniquePeople - attrition$uniquePeople[row],
                                                              "\n",
                                                              outcomeLabel,
                                                              ": n = ",
                                                              data$outcomes - attrition$outcomes[row],
                                                              sep = "")
    data$targetCount <- attrition$targetCount[row]
    data$uniquePeople <- attrition$uniquePeople[row]
    data$outcomes <- attrition$outcomes[row]
    return(data)
  }
  data <- list(leftBoxText = c(paste("Original cohorts:\n",
                                     targetLabel,
                                     ": n = ",
                                     attrition$targetCount[1],
                                     "\n",
                                     'Unique people in target:',
                                     ": n = ",
                                     attrition$uniquePeople[1],
                                     "\n",
                                     outcomeLabel,
                                     ": n = ",
                                     attrition$outcomes[1],
                                     sep = "")), rightBoxText = c(""), 
               targetCount = attrition$targetCount[1],
               uniquePeople = attrition$uniquePeople[1],
               outcomes = attrition$outcomes[1])
  for (i in 2:nrow(attrition)) {
    data <- addStep(data, attrition, i)
  }
  
  
  data$leftBoxText[length(data$leftBoxText) + 1] <- paste("Study population:\n",
                                                          targetLabel,
                                                          ": n = ",
                                                          data$targetCount,
                                                          "\n",
                                                          'Unique people in target:',
                                                          ": n = ",
                                                          data$uniquePeople,
                                                          "\n",
                                                          outcomeLabel,
                                                          ": n = ",
                                                          data$outcomes,
                                                          sep = "")
  leftBoxText <- data$leftBoxText
  rightBoxText <- data$rightBoxText
  nSteps <- length(leftBoxText)
  
  boxHeight <- (1/nSteps) - 0.03
  boxWidth <- 0.45
  shadowOffset <- 0.01
  arrowLength <- 0.01
  x <- function(x) {
    return(0.25 + ((x - 1)/2))
  }
  y <- function(y) {
    return(1 - (y - 0.5) * (1/nSteps))
  }
  
  downArrow <- function(p, x1, y1, x2, y2) {
    p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x1, y = y1, xend = x2, yend = y2))
    p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x2,
                                                       y = y2,
                                                       xend = x2 + arrowLength,
                                                       yend = y2 + arrowLength))
    p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x2,
                                                       y = y2,
                                                       xend = x2 - arrowLength,
                                                       yend = y2 + arrowLength))
    return(p)
  }
  rightArrow <- function(p, x1, y1, x2, y2) {
    p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x1, y = y1, xend = x2, yend = y2))
    p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x2,
                                                       y = y2,
                                                       xend = x2 - arrowLength,
                                                       yend = y2 + arrowLength))
    p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x2,
                                                       y = y2,
                                                       xend = x2 - arrowLength,
                                                       yend = y2 - arrowLength))
    return(p)
  }
  box <- function(p, x, y) {
    p <- p + ggplot2::geom_rect(ggplot2::aes_string(xmin = x - (boxWidth/2) + shadowOffset,
                                                    ymin = y - (boxHeight/2) - shadowOffset,
                                                    xmax = x + (boxWidth/2) + shadowOffset,
                                                    ymax = y + (boxHeight/2) - shadowOffset), fill = rgb(0,
                                                                                                         0,
                                                                                                         0,
                                                                                                         alpha = 0.2))
    p <- p + ggplot2::geom_rect(ggplot2::aes_string(xmin = x - (boxWidth/2),
                                                    ymin = y - (boxHeight/2),
                                                    xmax = x + (boxWidth/2),
                                                    ymax = y + (boxHeight/2)), fill = rgb(0.94,
                                                                                          0.94,
                                                                                          0.94), color = "black")
    return(p)
  }
  label <- function(p, x, y, text, hjust = 0) {
    p <- p + ggplot2::geom_text(ggplot2::aes_string(x = x, y = y, label = paste("\"", text, "\"",
                                                                                sep = "")),
                                hjust = hjust,
                                size = 3.7)
    return(p)
  }
  
  p <- ggplot2::ggplot()
  for (i in 2:nSteps - 1) {
    p <- downArrow(p, x(1), y(i) - (boxHeight/2), x(1), y(i + 1) + (boxHeight/2))
    p <- label(p, x(1) + 0.02, y(i + 0.5), "Y")
  }
  for (i in 2:(nSteps - 1)) {
    p <- rightArrow(p, x(1) + boxWidth/2, y(i), x(2) - boxWidth/2, y(i))
    p <- label(p, x(1.5), y(i) - 0.02, "N", 0.5)
  }
  for (i in 1:nSteps) {
    p <- box(p, x(1), y(i))
  }
  for (i in 2:(nSteps - 1)) {
    p <- box(p, x(2), y(i))
  }
  for (i in 1:nSteps) {
    p <- label(p, x(1) - boxWidth/2 + 0.02, y(i), text = leftBoxText[i])
  }
  for (i in 2:(nSteps - 1)) {
    p <- label(p, x(2) - boxWidth/2 + 0.02, y(i), text = rightBoxText[i])
  }
  p <- p + ggplot2::theme(legend.position = "none",
                          plot.background = ggplot2::element_blank(),
                          panel.grid.major = ggplot2::element_blank(),
                          panel.grid.minor = ggplot2::element_blank(),
                          panel.border = ggplot2::element_blank(),
                          panel.background = ggplot2::element_blank(),
                          axis.text = ggplot2::element_blank(),
                          axis.title = ggplot2::element_blank(),
                          axis.ticks = ggplot2::element_blank())
  
  if (!is.null(fileName))
    ggplot2::ggsave(p, filename = fileName, width = 6, height = 7, dpi = 400)
  return(p)
}
