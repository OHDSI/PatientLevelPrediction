# @file Plotting.R
#
# Copyright 2017 Observational Health Data Sciences and Informatics
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
#' @param result                Object returned by the runPlp() function
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
  plotSparseRoc(result$performanceEvaluation, 
                fileName=file.path(filename, 'plots','sparseROC.pdf'))
  plotPredictedPDF(result$performanceEvaluation, 
                   fileName=file.path(filename, 'plots','predictedPDF.pdf'))
  plotPreferencePDF(result$performanceEvaluation, 
                    fileName=file.path(filename, 'plots','preferencePDF.pdf'))
  plotPrecisionRecall(result$performanceEvaluation, 
                      fileName=file.path(filename, 'plots','precisionRecall.pdf'))
  plotF1Measure(result$performanceEvaluation, 
                fileName=file.path(filename, 'plots','f1Measure.pdf'))
  plotDemographicSummary(result$performanceEvaluation, 
                         fileName=file.path(filename, 'plots','demographicSummary.pdf'))
  plotSparseCalibration(result$performanceEvaluation, 
                        fileName=file.path(filename, 'plots','sparseCalibration.pdf'))
  plotSparseCalibration2(result$performanceEvaluation, 
                        fileName=file.path(filename, 'plots','sparseCalibrationConventional.pdf'))
  plotPredictionDistribution(result$performanceEvaluation, 
                             fileName=file.path(filename, 'plots','predictionDistribution.pdf'))
  
  plotVariableScatterplot(result$covariateSummary, 
                          fileName=file.path(filename, 'plots','variableScatterplot.pdf'))
  plotGeneralizability(result$covariateSummary, 
                             fileName=file.path(filename, 'plots','generalizability.pdf'))
  
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
#'                              \code{\link{runPlp}} function.
#' @param type                  options: 'train' or test'
#' @param fileName              Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotSparseRoc <- function(evaluation,type='test', fileName=NULL){
  ind <- evaluation$thresholdSummary$Eval==type
  
  x<- evaluation$thresholdSummary[ind,c('falsePositiveRate','sensitivity')]
  x <- x[order(x$falsePositiveRate, x$sensitivity),]
  
  # add the bit to get the step
  stepsExtra <- cbind(x[-1,1], x[-nrow(x),2])
  colnames( stepsExtra) <- colnames(x)
  x <- rbind(c(1,1), x, stepsExtra, c(0,0))
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
#'                              \code{\link{runPlp}} function.
#' @param type                  options: 'train' or test'
#' @param fileName              Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotPredictedPDF <- function(evaluation, type='test', fileName=NULL){
  ind <- evaluation$thresholdSummary$Eval==type
  
  x<- evaluation$thresholdSummary[ind,c('predictionThreshold','truePositiveCount','trueNegativeCount',
                                     'falsePositiveCount','falseNegativeCount')]
  x<- x[order(x$predictionThreshold,-x$truePositiveCount, -x$falsePositiveCount),]
  x$out <- c(x$truePositiveCount[-length(x$truePositiveCount)]-x$truePositiveCount[-1], x$truePositiveCount[length(x$truePositiveCount)])
  x$nout <- c(x$falsePositiveCount[-length(x$falsePositiveCount)]-x$falsePositiveCount[-1], x$falsePositiveCount[length(x$falsePositiveCount)])
  
  vals <- c()
  for(i in 1:length(x$predictionThreshold)){
    if(i!=length(x$predictionThreshold)){
      upper <- x$predictionThreshold[i+1]} else {upper <- min(x$predictionThreshold[i]+0.01,1)}
  val <- x$predictionThreshold[i]+runif(x$out[i])*(upper-x$predictionThreshold[i])
  vals <- c(val, vals)
  }
  vals[!is.na(vals)]
  
  vals2 <- c()
  for(i in 1:length(x$predictionThreshold)){
    if(i!=length(x$predictionThreshold)){
      upper <- x$predictionThreshold[i+1]} else {upper <- min(x$predictionThreshold[i]+0.01,1)}
    val2 <- x$predictionThreshold[i]+runif(x$nout[i])*(upper-x$predictionThreshold[i])
    vals2 <- c(val2, vals2)
  }
  vals2[!is.na(vals2)]
  
  x <- rbind(data.frame(variable=rep('outcome',length(vals)), value=vals),
             data.frame(variable=rep('No outcome',length(vals2)), value=vals2)
  )
  
  plot <- ggplot2::ggplot(x, ggplot2::aes(x=x$value,
                                          group=x$variable,
                                          fill=x$variable)) +
    ggplot2::geom_density(ggplot2::aes(x=x$value, fill=x$variable), alpha=.3) +
    ggplot2::scale_x_continuous("Prediction Threshold")+#, limits=c(0,1)) +
    ggplot2::scale_y_continuous("Density") + 
    ggplot2::guides(fill=ggplot2::guide_legend(title="Class"))
  
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
#'                              \code{\link{runPlp}} function.
#' @param type                  options: 'train' or test'
#' @param fileName              Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotPreferencePDF <- function(evaluation, type='test', fileName=NULL){
  ind <- evaluation$thresholdSummary$Eval==type
  
  x<- evaluation$thresholdSummary[ind,c('preferenceThreshold','truePositiveCount','trueNegativeCount',
                                     'falsePositiveCount','falseNegativeCount')]
  x<- x[order(x$preferenceThreshold,-x$truePositiveCount),]
  x$out <- c(x$truePositiveCount[-length(x$truePositiveCount)]-x$truePositiveCount[-1], x$truePositiveCount[length(x$truePositiveCount)])
  x$nout <- c(x$falsePositiveCount[-length(x$falsePositiveCount)]-x$falsePositiveCount[-1], x$falsePositiveCount[length(x$falsePositiveCount)])
  
  vals <- c()
  for(i in 1:length(x$preferenceThreshold)){
    if(i!=length(x$preferenceThreshold)){
    upper <- x$preferenceThreshold[i+1]} else {upper <- 1}
    val <- x$preferenceThreshold[i]+runif(x$out[i])*(upper-x$preferenceThreshold[i])
    vals <- c(val, vals)
  }
  vals[!is.na(vals)]
  
  vals2 <- c()
  for(i in 1:length(x$preferenceThreshold)){
    if(i!=length(x$preferenceThreshold)){
      upper <- x$preferenceThreshold[i+1]} else {upper <- 1}
    val2 <- x$preferenceThreshold[i]+runif(x$nout[i])*(upper-x$preferenceThreshold[i])
    vals2 <- c(val2, vals2)
  }
  vals2[!is.na(vals2)]
  
  x <- rbind(data.frame(variable=rep('outcome',length(vals)), value=vals),
             data.frame(variable=rep('No outcome',length(vals2)), value=vals2)
  )
  
  plot <- ggplot2::ggplot(x, ggplot2::aes(x=x$value,
                                          group=x$variable,
                                          fill=x$variable)) +
    ggplot2::geom_density(ggplot2::aes(x=x$value, fill=x$variable), alpha=.3) +
    ggplot2::scale_x_continuous("Preference Threshold")+#, limits=c(0,1)) +
    ggplot2::scale_y_continuous("Density") + 
    ggplot2::guides(fill=ggplot2::guide_legend(title="Class"))
  
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
#'                              \code{\link{runPlp}} function.
#' @param type                  options: 'train' or test'
#' @param fileName              Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotPrecisionRecall <- function(evaluation, type='test', fileName=NULL){
  ind <- evaluation$thresholdSummary$Eval==type
  
  x<- evaluation$thresholdSummary[ind,c('positivePredictiveValue', 'sensitivity')]
  #x <- rbind(c(0,1), x, c(1,0))
  
  plot <- ggplot2::ggplot(x, ggplot2::aes(x$positivePredictiveValue, x$sensitivity)) +
    ggplot2::geom_line(size=1) +
    ggplot2::scale_x_continuous("Recall")+#, limits=c(0,1)) +
    ggplot2::scale_y_continuous("Precision")#, limits=c(0,1))
  
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
#'                              \code{\link{runPlp}} function.
#' @param type                  options: 'train' or test'
#' @param fileName              Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotF1Measure <- function(evaluation,type='test', fileName=NULL){
  ind <- evaluation$thresholdSummary$Eval==type
  
  x<- evaluation$thresholdSummary[ind,c('predictionThreshold', 'f1Score')]
  #x <- rbind(c(0,1), x, c(1,0))
  
  plot <- ggplot2::ggplot(x, ggplot2::aes(x$predictionThreshold, x$f1Score)) +
    ggplot2::geom_line(size=1) +
    ggplot2::geom_point(size=1) +
    ggplot2::scale_x_continuous("predictionThreshold")+#, limits=c(0,1)) +
    ggplot2::scale_y_continuous("F1Score")#, limits=c(0,1))
  
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
#'                              \code{\link{runPlp}} function.
#' @param type                  options: 'train' or test'
#' @param fileName              Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotDemographicSummary <- function(evaluation, type='test', fileName=NULL){
  if (!all(is.na(evaluation$demographicSummary$averagePredictedProbability))){
    ind <- evaluation$demographicSummary$Eval==type
    x<- evaluation$demographicSummary[ind,colnames(evaluation$demographicSummary)%in%c('ageGroup','genGroup','averagePredictedProbability',
                                            'PersonCountAtRisk', 'PersonCountWithOutcome')]

    x$observed <- x$PersonCountWithOutcome/x$PersonCountAtRisk
    
    
    x <- x[,colnames(x)%in%c('ageGroup','genGroup','averagePredictedProbability','observed')]
    
    # if age or gender missing add 
    if(sum(colnames(x)=='ageGroup')==1 && sum(colnames(x)=='genGroup')==0  ){
      x$genGroup = rep('Non', nrow(x))
      evaluation$demographicSummary$genGroup = rep('Non', nrow(evaluation$demographicSummary))
    } 
    if(sum(colnames(x)=='ageGroup')==0 && sum(colnames(x)=='genGroup')==1  ){
      x$ageGroup = rep('-1', nrow(x))
      evaluation$demographicSummary$ageGroup = rep('-1', nrow(evaluation$demographicSummary))
      
    }

    x <- reshape2::melt(x, id.vars=c('ageGroup','genGroup'))
    
    # 1.96*StDevPredictedProbability
    ci <- evaluation$demographicSummary[,colnames(evaluation$demographicSummary)%in%c('ageGroup','genGroup','averagePredictedProbability','StDevPredictedProbability')]
    ci$StDevPredictedProbability[is.na(ci$StDevPredictedProbability)] <- 1
    ci$lower <- ci$averagePredictedProbability-1.96*ci$StDevPredictedProbability
    ci$lower[ci$lower <0] <- 0
    ci$upper <- ci$averagePredictedProbability+1.96*ci$StDevPredictedProbability
    ci$upper[ci$upper >1] <- max(ci$upper[ci$upper <1])
    
    x$age <- gsub('Age group:','', x$ageGroup)
    x$age <- factor(x$age,levels=c(" 0-4"," 5-9"," 10-14",
                                   " 15-19"," 20-24"," 25-29"," 30-34"," 35-39"," 40-44",
                                   " 45-49"," 50-54"," 55-59"," 60-64"," 65-69"," 70-74",
                                   " 75-79"," 80-84"," 85-89"," 90-94"," 95-99","-1"),ordered=TRUE)
    
    x <- merge(x, ci[,c('ageGroup','genGroup','lower','upper')], by=c('ageGroup','genGroup'))
    
    plot <- ggplot2::ggplot(data=x, 
                            ggplot2::aes(x=age, group=variable*genGroup)) +

      ggplot2::geom_line(ggplot2::aes(y=value, group=variable,
                                      color=variable,
                                      linetype = variable))+
      ggplot2::geom_ribbon(data=x[x$variable!='observed',],
                           ggplot2::aes(ymin=lower, ymax=upper
                                        , group=genGroup), 
                           fill="blue", alpha="0.2") +
      ggplot2::facet_grid(.~ genGroup, scales = "free") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
      #ggplot2::coord_flip() +
      ggplot2::scale_y_continuous("Fraction") +
      ggplot2::scale_x_discrete("Age") +
      ggplot2::scale_color_manual(values = c("royalblue4","red"),
                                  guide = ggplot2::guide_legend(title = NULL),
                                  labels = c("Expected", "Observed")) +

      ggplot2::guides(linetype=FALSE)
    
    if (!is.null(fileName))
      ggplot2::ggsave(fileName, plot, width = 7, height = 4.5, dpi = 400)
    return(plot)
  }
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
#'                              \code{\link{runPlp}} function.
#' @param type                  options: 'train' or test'
#' @param fileName              Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotSparseCalibration <- function(evaluation, type='test', fileName=NULL){
  ind <- evaluation$calibrationSummary$Eval==type
  
  x<- evaluation$calibrationSummary[ind,c('averagePredictedProbability','observedIncidence')]
  maxVal <- max(x$averagePredictedProbability,x$observedIncidence)
  model <- stats::lm(observedIncidence~averagePredictedProbability, data=x)
  res <- model$coefficients
  names(res) <- c('Intercept','Gradient')
  
  # confidence int
  interceptConf <- stats::confint(model)[1,]
  gradientConf <- stats::confint(model)[2,]
  
  cis <- data.frame(lci = interceptConf[1]+seq(0,1,length.out = nrow(x))*gradientConf[1],
                    uci = interceptConf[2]+seq(0,1,length.out = nrow(x))*gradientConf[2],
                    x=seq(0,1,length.out = nrow(x)))
  
  x <- cbind(x, cis)
  # TODO: CHECK INPUT
  plot <- ggplot2::ggplot(data=x,
                          ggplot2::aes(x=averagePredictedProbability, y=observedIncidence
                                       )) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin=lci,ymax=uci, x=x), 
                         fill="blue", alpha="0.2") +
    ggplot2::geom_point(size=1, color='darkblue') +
    ggplot2::coord_cartesian(ylim = c(0, maxVal), xlim =c(0,maxVal)) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 2, size=1,
                         show.legend = TRUE) +
    ggplot2::geom_abline(intercept = res['Intercept'], slope = res['Gradient'],
                         linetype = 1,show.legend = TRUE,
                         color='darkblue') +
    ggplot2::scale_x_continuous("Average Predicted Probability") +
    ggplot2::scale_y_continuous("Observed Fraction With Outcome")

    
  
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 5, height = 3.5, dpi = 400)
  return(plot)
}

#=============
# CALIBRATIONSUMMARY PLOTS 2
#=============
#' Plot the conventional calibration
#'
#' @details
#' Create a plot showing the calibration
#' #'
#' @param evaluation            A prediction object as generated using the
#'                              \code{\link{runPlp}} function.
#' @param type                  options: 'train' or test'
#' @param fileName              Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotSparseCalibration2 <- function(evaluation, type='test', fileName=NULL){
  ind <- evaluation$calibrationSummary$Eval==type
  
  x<- evaluation$calibrationSummary[ind,c('averagePredictedProbability','observedIncidence', 'PersonCountAtRisk')]
  
  
  cis <- apply(x, 1, function(x) binom.test(x[2]*x[3], x[3], alternative = c("two.sided"), conf.level = 0.95)$conf.int)
  x$lci <- cis[1,]  
  x$uci <- cis[2,]
  
  maxes <- max(max(x$averagePredictedProbability), max(x$observedIncidence))*1.1
  
  # TODO: CHECK INPUT
  limits <- ggplot2::aes(ymax = x$uci, ymin= x$lci)
  
  plot <- ggplot2::ggplot(data=x,
                          ggplot2::aes(x=averagePredictedProbability, y=observedIncidence
                          )) +
    ggplot2::geom_point(size=2, color='black') +
    ggplot2::geom_errorbar(limits) +
    #ggplot2::geom_smooth(method=lm, se=F, colour='darkgrey') +
    ggplot2::geom_line(colour='darkgrey') +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 5, size=0.4,
                         show.legend = TRUE) +
    ggplot2::scale_x_continuous("Average Predicted Probability") +
    ggplot2::scale_y_continuous("Observed Fraction With Outcome") +
    ggplot2::coord_cartesian(xlim = c(0, maxes), ylim=c(0,maxes)) 
  
  
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
#'                              \code{\link{runPlp}} function.
#' @param type                  options: 'train' or test'
#' @param fileName              Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotPredictionDistribution <- function(evaluation, type='test', fileName=NULL){
  ind <- evaluation$predictionDistribution$Eval==type
  x<- evaluation$predictionDistribution[ind,]

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
#'                              \code{\link{runPlp}} function.
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
  
  # remove the non-incidence variables from the plot
  covariateSummary <- covariateSummary[covariateSummary$CovariateMeanWithNoOutcome <= 1,]
  
  covariateSummary$size <- rep(0.1, nrow(covariateSummary))
  covariateSummary$size[covariateSummary$covariateValue!=0] <- 0.5
 
  plot <- ggplot2::ggplot(covariateSummary, ggplot2::aes(y=CovariateMeanWithOutcome, 
                                                         x=CovariateMeanWithNoOutcome,
                                                         #size=abs(covariateValue)+0.1
                                                         size=size
                                                         )) +
          ggplot2::geom_point(ggplot2::aes(color = size)) +
          ggplot2::scale_size(range = c(0, 1)) +
          ggplot2::scale_colour_gradient2(low = "red",mid = "blue", high="green") +
    ggplot2::scale_y_continuous("Outcome Covariate Mean") +
    ggplot2::scale_x_continuous("Non-outcome Covariate Mean") + 
    ggplot2::geom_abline(intercept = 0, slope = 1,linetype = 2) +
    ggplot2::theme(legend.position="none")
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 5, height = 3.5, dpi = 400)
  return(plot)
}


#' Plot the train/test generalizability diagnostic
#'
#' @details
#' Create a plot showing the train/test generalizability diagnostic
#' #'
#' @param covariateSummary      A prediction object as generated using the
#'                              \code{\link{runPlp}} function.
#' @param fileName              Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotGeneralizability<- function(covariateSummary, fileName=NULL){
  
  covariateSummary$TrainCovariateMeanWithOutcome[is.na(covariateSummary$TrainCovariateMeanWithOutcome)] <- 0
  covariateSummary$TestCovariateMeanWithOutcome[is.na(covariateSummary$TestCovariateMeanWithOutcome)] <- 0
  
  covariateSummary <- covariateSummary[covariateSummary$TrainCovariateMeanWithOutcome <= 1,]
  
  plot1 <- ggplot2::ggplot(covariateSummary, 
                          ggplot2::aes(TrainCovariateMeanWithOutcome, 
                                       TestCovariateMeanWithOutcome)) +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous("Train set Mean") +
    ggplot2::scale_y_continuous("Test Set Mean") + 
    ggplot2::theme(legend.position="none") +
    ggplot2::geom_abline(intercept = 0, slope = 1,linetype = 2)+
    ggplot2::ggtitle("Outcome")
    
 
  covariateSummary$TrainCovariateMeanWithNoNOutcome[is.na(covariateSummary$TrainCovariateMeanWithNoOutcome)] <- 0
  covariateSummary$TestCovariateMeanWithNoOutcome[is.na(covariateSummary$TestCovariateMeanWithNoOutcome)] <- 0
  
  plot2 <- ggplot2::ggplot(covariateSummary, 
                          ggplot2::aes(TrainCovariateMeanWithNoOutcome, 
                                       TestCovariateMeanWithNoOutcome)) +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous("Train set Mean") +
    ggplot2::scale_y_continuous("Test Set Mean") + 
    ggplot2::theme(legend.position="none") +
    ggplot2::geom_abline(intercept = 0, slope = 1,linetype = 2) +
    ggplot2::ggtitle("No Outcome")
  
  plot <- gridExtra::grid.arrange(plot1, plot2, ncol=2)
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 5, height = 3.5, dpi = 400)
  return(plot)
}

#' Plot the plearning curve using the data frame
#'
#' @details
#' Create a plot showing thelearnign curve using output from createLearningCurve
#'
#' @param learningcurve         A results object as generated using the
#'                              \code{\link{createLearningCurve}} function.
#' @param title                 plot title
#' @param fileName              Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotLearningCurve <- function(learningCurve, title=" ", xlabel="Training Size", fileName=NULL){
  plot<- ggplot2::ggplot(learningCurve, ggplot2::aes(x)) +
    ggplot2::geom_line(ggplot2::aes(y=as.numeric(trainAUC)),
              colour="red") +
    ggplot2::geom_line(ggplot2::aes(y=as.numeric(testAUC)),
              colour="green")+
    ggplot2::xlab(xlabel) +
    ggplot2::ylab("AUC") +
    ggplot2::ggtitle(title) + ggplot2::coord_cartesian(ylim=c(0.5, 1)) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 5, height = 4.5, dpi = 400)
  return(plot)
}