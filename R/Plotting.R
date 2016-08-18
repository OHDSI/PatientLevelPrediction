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


#' Plot all the PatientLevelPrediction plots
#'
#' @details
#' Create a directory with all the plots
#'
#' @param result                Object returned by the RunPlp() function
#' @param filename              Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' TRUE if it ran 
#'
#' @export
plotPlp <- function(result, filename){
  
  # check inputs
  
  if(!dir.exists(file.path(filename, 'plots'))){dir.create(file.path(filename, 'plots'), recursive =T)}
  
  # run each of the plots:
  plotSparseRoc(result$performanceEvaluationTest, 
                fileName=file.path(filename, 'plots','sparseROC.pdf'))
  plotPredictedPDF(result$performanceEvaluationTest, 
                   fileName=file.path(filename, 'plots','predictedPDF.pdf'))
  plotPreferencePDF(result$performanceEvaluationTest, 
                    fileName=file.path(filename, 'plots','preferencePDF.pdf'))
  plotPrecisionRecall(result$performanceEvaluationTest, 
                      fileName=file.path(filename, 'plots','precisionRecall.pdf'))
  plotF1Measure(result$performanceEvaluationTest, 
                fileName=file.path(filename, 'plots','f1Measure.pdf'))
  plotDemographicSummary(result$performanceEvaluationTest, 
                         fileName=file.path(filename, 'plots','demographicSummary.pdf'))
  plotSparseCalibration(result$performanceEvaluationTest, 
                        fileName=file.path(filename, 'plots','sparseCalibration.pdf'))
  plotPredictionDistribution(result$performanceEvaluationTest, 
                             fileName=file.path(filename, 'plots','predictionDistribution.pdf'))
  
  plotVariableScatterplot(result$covariateSummary, 
                          fileName=file.path(filename, 'plots','variableScatterplot.pdf'))
  plotGenerlizabilityOutcome(result$covariateSummary, 
                             fileName=file.path(filename, 'plots','generlizabilityOutcome.pdf'))
  plotGenerlizabilityNoOutcome(result$covariateSummary, 
                               fileName=file.path(filename, 'plots','generlizabilityNoOutcome.pdf'))
  
  return(TRUE)
}

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




#=============
# THERSHOLDSUMMARY PLOTS 
#=============


#' Plot the ROC curve using the sparse thresholdSummary data frame
#'
#' @details
#' Create a plot showing the Receiver Operator Characteristics (ROC) curve.
#'
#' @param evaluation            A prediction object as generated using the
#'                              \code{\link{RunPlp}} function.
#' @param fileName              Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotSparseRoc <- function(evaluation, fileName=NULL){
  x<- evaluation$thresholdSummary[,c('falsePositiveRate','sensitivity')]
  
  # add the bit to get the step
  stepsExtra <- cbind(x[-1,1], x[-nrow(x),2])
  colnames( stepsExtra) <- colnames(x)
  x <- rbind(x, stepsExtra, c(0,0))
  x <- x[order(x$falsePositiveRate, x$sensitivity),]
  
  
  plot <- ggplot2::ggplot(x, ggplot2::aes(x$falsePositiveRate, x$sensitivity)) +
    ggplot2::geom_polygon(fill = "blue", alpha = 0.2) +
    ggplot2::geom_line(size=1) +
    ggplot2::geom_abline(intercept = 0, slope = 1,linetype = 2) +
    ggplot2::scale_x_continuous("1 - specificity", limits=c(0,1)) +
    ggplot2::scale_y_continuous("Sensitivity", limits=c(0,1))
    
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 5, height = 4.5, dpi = 400)
  return(plot)
}


#' Plot the Predicted probability density function, showing prediction overlap between true and false cases
#'
#' @details
#' Create a plot showing the predicted probability density function, showing prediction overlap between true and false cases
#'
#' @param evaluation            A prediction object as generated using the
#'                              \code{\link{RunPlp}} function.
#' @param fileName              Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotPredictedPDF <- function(evaluation, fileName=NULL){
  
  x<- evaluation$thresholdSummary[,c('predictionThreshold','truePositiveCount','trueNegativeCount',
                                     'falsePositiveCount','falseNegativeCount')]
  
  x$TC <- x$truePositiveCount+x$trueNegativeCount
  x$FC <- x$falsePositiveCount+x$falseNegativeCount
  
  x <- x[,c('predictionThreshold','TC','FC')]
  x <- reshape2::melt(x, id.vars='predictionThreshold')
  plot <- ggplot2::ggplot(x, ggplot2::aes(x=x$predictionThreshold,y= x$value,
                                          group=x$variable, color=x$variable)) +
    #ggplot2::geom_polygon(fill = "blue", alpha = 0.2) +
    ggplot2::geom_line(size=0.5) +
    ggplot2::geom_point(size=1) +
    ggplot2::scale_x_continuous("Prediction Threshold", limits=c(0,1)) +
    ggplot2::scale_y_continuous("Count") +
    ggplot2::scale_colour_manual(values = c("green4","red1"),
                                 guide = ggplot2::guide_legend(title = NULL),
                                 labels = c("True Predictions", "False Predictions"))
  
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 5, height = 4.5, dpi = 400)
  return(plot)
}



#' Plot the preference score probability density function, showing prediction overlap between true and false cases
#' #'
#' @details
#' Create a plot showing the preference score probability density function, showing prediction overlap between true and false cases
#' #'
#' @param evaluation            A prediction object as generated using the
#'                              \code{\link{RunPlp}} function.
#' @param fileName              Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotPreferencePDF <- function(evaluation, fileName=NULL){
  
  x<- evaluation$thresholdSummary[,c('preferenceThreshold','truePositiveCount','trueNegativeCount',
                                     'falsePositiveCount','falseNegativeCount')]
  
  x$TC <- x$truePositiveCount+x$trueNegativeCount
  x$FC <- x$falsePositiveCount+x$falseNegativeCount
  
  x <- x[,c('preferenceThreshold','TC','FC')]
  x <- reshape2::melt(x, id.vars='preferenceThreshold')
  plot <- ggplot2::ggplot(x, ggplot2::aes(x=x$preferenceThreshold,y= x$value,
                                          group=x$variable, color=x$variable)) +
    #ggplot2::geom_polygon(fill = "blue", alpha = 0.2) +
    ggplot2::geom_line(size=0.5) +
    ggplot2::geom_point(size=1) +
    ggplot2::scale_x_continuous("Preference Threshold", limits=c(0,1)) +
    ggplot2::scale_y_continuous("Count") +
    ggplot2::scale_colour_manual(values = c("green4","red1"),
                                 guide = ggplot2::guide_legend(title = NULL),
                                 labels = c("True Predictions", "False Predictions"))
  
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 5, height = 4.5, dpi = 400)
  return(plot)
}



#' Plot the precision-recall curve using the sparse thresholdSummary data frame
#'
#' @details
#' Create a plot showing the precision-recall curve using the sparse thresholdSummary data frame
#'
#' @param evaluation            A prediction object as generated using the
#'                              \code{\link{RunPlp}} function.
#' @param fileName              Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotPrecisionRecall <- function(evaluation, fileName=NULL){
  x<- evaluation$thresholdSummary[,c('positivePredictiveValue', 'sensitivity')]
  x <- rbind(c(0,1), x, c(1,0))
  
  plot <- ggplot2::ggplot(x, ggplot2::aes(x$positivePredictiveValue, x$sensitivity)) +
    ggplot2::geom_line(size=1) +
    ggplot2::scale_x_continuous("Recall", limits=c(0,1)) +
    ggplot2::scale_y_continuous("Precision", limits=c(0,1))
  
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 5, height = 4.5, dpi = 400)
  return(plot)
}



#' Plot the F1 measure efficiency frontier using the sparse thresholdSummary data frame
#'
#' @details
#' Create a plot showing the F1 measure efficiency frontier using the sparse thresholdSummary data frame
#'
#' @param evaluation            A prediction object as generated using the
#'                              \code{\link{RunPlp}} function.
#' @param fileName              Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotF1Measure <- function(evaluation, fileName=NULL){
  x<- evaluation$thresholdSummary[,c('predictionThreshold', 'f1Score')]
  #x <- rbind(c(0,1), x, c(1,0))
  
  plot <- ggplot2::ggplot(x, ggplot2::aes(x$predictionThreshold, x$f1Score)) +
    ggplot2::geom_line(size=1) +
    ggplot2::geom_point(size=1) +
    ggplot2::scale_x_continuous("predictionThreshold", limits=c(0,1)) +
    ggplot2::scale_y_continuous("F1Score", limits=c(0,1))
  
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 5, height = 4.5, dpi = 400)
  return(plot)
}



#=============
# DEMOGRAPHICSUMMARY PLOTS 
#=============
#' Plot the Observed vs. expected incidence, by age and gender
#'
#' @details
#' Create a plot showing the Observed vs. expected incidence, by age and gender
#' #'
#' @param evaluation            A prediction object as generated using the
#'                              \code{\link{RunPlp}} function.
#' @param fileName              Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotDemographicSummary <- function(evaluation, fileName=NULL){
  x<- evaluation$demographicSummary[,c('ageGroup','genGroup','averagePredictedProbability',
                                       'PersonCountAtRisk', 'PersonCountWithOutcome')]
  x$observed <- x$PersonCountWithOutcome/x$PersonCountAtRisk
  x <- x[c('ageGroup','genGroup','averagePredictedProbability','observed')]
  x <- reshape2::melt(x, id.vars=c('ageGroup','genGroup'))
  
  x$age <- gsub('Age group:','', x$ageGroup)
  x$age <- factor(x$age,levels=c(" 0-4"," 5-9"," 10-14",
                                " 15-19"," 20-24"," 25-29"," 30-34"," 35-39"," 40-44",
                                " 45-49"," 50-54"," 55-59"," 60-64"," 65-69"," 70-74",
                                " 75-79"," 80-84"," 85-89"," 90-94"," 95-99"),ordered=TRUE)
  
  plot <- ggplot2::ggplot(x, ggplot2::aes(x=age, y=value,
                                          fill=variable)) +
    ggplot2::geom_bar(stat="identity", position="dodge") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous("Fraction") +
    ggplot2::scale_x_discrete("Age") +
    ggplot2::facet_grid(.~ genGroup, scales = "free") +
    ggplot2::scale_fill_manual(values = c("gold2","royalblue4"),
                                 guide = ggplot2::guide_legend(title = NULL),
                                 labels = c("Expected", "Observed"))
  
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 5, height = 4.5, dpi = 400)
  return(plot)
}


#=============
# CALIBRATIONSUMMARY PLOTS 
#=============
#' Plot the calibration
#'
#' @details
#' Create a plot showing the calibration
#' #'
#' @param evaluation            A prediction object as generated using the
#'                              \code{\link{RunPlp}} function.
#' @param fileName              Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotSparseCalibration <- function(evaluation, fileName=NULL){
  x<- evaluation$calibrationSummary[,c('averagePredictedProbability','observedIncidence')]
  
  # TODO: CHECK INPUT
  plot <- ggplot2::ggplot(x,
                          ggplot2::aes(x=averagePredictedProbability, y=observedIncidence)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size=1) +
    ggplot2::scale_x_continuous("Average Predicted probability") +
    ggplot2::scale_y_continuous("Observed Incidence")
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 5, height = 3.5, dpi = 400)
  return(plot)
}




#=============
# PREDICTIONSUMMARY PLOTS 
#=============

#' Plot the side-by-side boxplots of prediction distribution, by class#'
#' @details
#' Create a plot showing the side-by-side boxplots of prediction distribution, by class
#' #'
#' @param evaluation            A prediction object as generated using the
#'                              \code{\link{RunPlp}} function.
#' @param fileName              Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotPredictionDistribution <- function(evaluation, fileName=NULL){
  x<- evaluation$predictionDistribution

  #(x=Class, y=predictedProbabllity sequence:  min->P05->P25->Median->P75->P95->max)
  
  plot <-   ggplot2::ggplot(x, ggplot2::aes(x=as.factor(class),
                                            ymin=MinPredictedProbability,
                                            lower=P25PredictedProbability,
                                            middle=MedianPredictedProbability,
                                            upper=P75PredictedProbability, 
                                            ymax=MaxPredictedProbability, 
                                            color=as.factor(class))) + 
    ggplot2::coord_flip() +
    ggplot2::geom_boxplot(stat="identity")  +
    ggplot2::scale_x_discrete("Class") + 
    ggplot2::scale_y_continuous("Predicted Probability") + 
    ggplot2::theme(legend.position="none") +
    ggplot2::geom_segment(ggplot2::aes(x = 0.9, y = x$P05PredictedProbability[x$class==0], 
                     xend = 1.1, yend = x$P05PredictedProbability[x$class==0]), color='red') +
    ggplot2::geom_segment(ggplot2::aes(x = 0.9, y = x$P95PredictedProbability[x$class==0], 
                                       xend = 1.1, yend = x$P95PredictedProbability[x$class==0]), color='red') +
  ggplot2::geom_segment(ggplot2::aes(x = 1.9, y = x$P05PredictedProbability[x$class==1], 
                                     xend = 2.1, yend = x$P05PredictedProbability[x$class==1])) +
    ggplot2::geom_segment(ggplot2::aes(x = 1.9, y = x$P95PredictedProbability[x$class==1], 
                                       xend = 2.1, yend = x$P95PredictedProbability[x$class==1]))
  
  
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 5, height = 4.5, dpi = 400)
  return(plot)
}



#=============
# COVARIATESUMMARY PLOTS 
#=============

#' Plot the variable importance scatterplot
#'
#' @details
#' Create a plot showing the variable importance scatterplot
#' #'
#' @param covariateSummary      A prediction object as generated using the
#'                              \code{\link{RunPlp}} function.
#' @param fileName              Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotVariableScatterplot <- function(covariateSummary, fileName=NULL){
 
  plot <- ggplot2::ggplot(covariateSummary, ggplot2::aes(CovariateMeanWithOutcome, CovariateMeanWithNoOutcome)) +
          ggplot2::geom_point(ggplot2::aes(color = abs(value))) + 
          ggplot2::scale_colour_gradient(low = "white", high="red") +
    ggplot2::scale_x_continuous("Outcome Incidence") +
    ggplot2::scale_y_continuous("Non-outcome Incidence") + 
    ggplot2::geom_abline(intercept = 0, slope = 1,linetype = 2) +
    ggplot2::theme(legend.position="none")
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 5, height = 3.5, dpi = 400)
  return(plot)
}


#' Plot the train/test generalizability diagnostic #1
#'
#' @details
#' Create a plot showing the train/test generalizability diagnostic #1
#' #'
#' @param covariateSummary      A prediction object as generated using the
#'                              \code{\link{RunPlp}} function.
#' @param fileName              Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotGenerlizabilityOutcome <- function(covariateSummary, fileName=NULL){
  
  covariateSummary$TrainCovariateMeanWithOutcome[is.na(covariateSummary$TrainCovariateMeanWithOutcome)] <- 0
  covariateSummary$TestCovariateMeanWithOutcome[is.na(covariateSummary$TestCovariateMeanWithOutcome)] <- 0
  
  plot <- ggplot2::ggplot(covariateSummary, 
                          ggplot2::aes(TrainCovariateMeanWithOutcome, 
                                       TestCovariateMeanWithOutcome)) +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous("Train set Incidence") +
    ggplot2::scale_y_continuous("Test Set Incidence") + 
    ggplot2::theme(legend.position="none")
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 5, height = 3.5, dpi = 400)
  return(plot)
}

#' Plot the train/test generalizability diagnostic #2
#'
#' @details
#' Create a plot showing the train/test generalizability diagnostic #2
#' #'
#' @param covariateSummary      A prediction object as generated using the
#'                              \code{\link{RunPlp}} function.
#' @param fileName              Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotGenerlizabilityNoOutcome <- function(covariateSummary, fileName=NULL){
  
  covariateSummary$TrainCovariateMeanWithNoNOutcome[is.na(covariateSummary$TrainCovariateMeanWithNoOutcome)] <- 0
  covariateSummary$TestCovariateMeanWithNoOutcome[is.na(covariateSummary$TestCovariateMeanWithNoOutcome)] <- 0
  
  plot <- ggplot2::ggplot(covariateSummary, 
                          ggplot2::aes(TrainCovariateMeanWithNoOutcome, 
                                       TestCovariateMeanWithNoOutcome)) +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous("Train set Incidence") +
    ggplot2::scale_y_continuous("Test Set Incidence") + 
    ggplot2::theme(legend.position="none")
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 5, height = 3.5, dpi = 400)
  return(plot)
}