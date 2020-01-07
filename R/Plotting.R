# @file Plotting.R
#
# Copyright 2019 Observational Health Data Sciences and Informatics
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


#' Plot the outcome incidence over time
#'
#' @details
#' This creates a survival plot that can be used to pick a suitable time-at-risk period
#'
#' @param plpData               The plpData object returned by running getPlpData()
#' @param outcomeId             The cohort id corresponding to the outcome 
#' @param removeSubjectsWithPriorOutcome  Remove patients who have had the outcome before their target cohort index date from the plot
#' @param riskWindowStart       (integer) The time-at-risk starts at target cohort index date plus this value
#' @param riskWindowEnd       (integer) The time-at-risk ends at target cohort index date plus this value 
#' @param riskTable           (binary) Whether to include a table at the bottom  of the plot showing the number of people at risk over time
#' @param confInt             (binary) Whether to include a confidence interval
#' @param yLabel              (string) The label for the y-axis         
#'
#' @return
#' TRUE if it ran 
#'
#' @export
outcomeSurvivalPlot <- function(plpData, outcomeId,
                                removeSubjectsWithPriorOutcome = T,  
                                riskWindowStart = 1, 
                                riskWindowEnd = 3650,
                                riskTable = T,
                                confInt= T, 
                                yLabel = 'Fraction of those who are outcome free in target population'){
  
  ensure_installed("survminer")
  if(missing(plpData)){
    stop('plpData missing')
  }
  if(missing(outcomeId)){
    stop('outcomeId missing')
  }
  if(class(plpData)!='plpData'){
    stop('Incorrect plpData object')
  }
  if(!outcomeId%in%unique(plpData$outcomes$outcomeId)){
    stop('outcome id not in data')
  }
  
  
  pop <- createStudyPopulation(plpData = plpData, outcomeId = outcomeId, 
                                                       removeSubjectsWithPriorOutcome = removeSubjectsWithPriorOutcome, 
                                                       requireTimeAtRisk = F, 
                                                       riskWindowStart = riskWindowStart, riskWindowEnd = riskWindowEnd)
  pop$daysToEvent[is.na(pop$daysToEvent)] <- pop$survivalTime[is.na(pop$daysToEvent)]
  sv <- survival::survfit(survival::Surv(daysToEvent, outcomeCount)~cohortId,#riskDecile, 
                          pop, 
                          conf.int = TRUE)
  res <- survminer::ggsurvplot(fit=sv, 
                               data = pop,
                               risk.table = riskTable,
                               pval = F,
                               xlim = c(0,3650), ylim=c(min(sv$surv)*0.95,1),
                               conf.int = confInt,
                               ggtheme = ggplot2::theme_minimal(),
                               risk.table.y.text.col = T,
                               risk.table.y.text = FALSE,
                               ylab = yLabel
  )
  return(res)
}


#' Plot all the PatientLevelPrediction plots
#'
#' @details
#' Create a directory with all the plots
#'
#' @param result                Object returned by the runPlp() function
#' @param filename              Name of the file where the plot should be saved, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#' @param type                  Evaluation data type either 'test', 'val' or 'train'                            
#'
#' @return
#' TRUE if it ran 
#'
#' @export
plotPlp <- function(result, filename, type='test'){
  
  # check inputs
  
  if(!dir.exists(file.path(filename, 'plots'))){dir.create(file.path(filename, 'plots'), recursive =T)}
  
  # run each of the plots:
  plotSparseRoc(result$performanceEvaluation, 
                fileName=file.path(filename, 'plots','sparseROC.pdf'), 
                type = type)
  plotPredictedPDF(result$performanceEvaluation, 
                   fileName=file.path(filename, 'plots','predictedPDF.pdf'),
                   type = type)
  plotPreferencePDF(result$performanceEvaluation, 
                    fileName=file.path(filename, 'plots','preferencePDF.pdf'), 
                    type = type)
  plotPrecisionRecall(result$performanceEvaluation, 
                      fileName=file.path(filename, 'plots','precisionRecall.pdf'), 
                      type = type)
  plotF1Measure(result$performanceEvaluation, 
                fileName=file.path(filename, 'plots','f1Measure.pdf'), 
                type = type)
  plotDemographicSummary(result$performanceEvaluation, 
                         fileName=file.path(filename, 'plots','demographicSummary.pdf'),
                         type = type)
  plotSparseCalibration(result$performanceEvaluation, 
                        fileName=file.path(filename, 'plots','sparseCalibration.pdf'), 
                        type = type)
  plotSparseCalibration2(result$performanceEvaluation, 
                        fileName=file.path(filename, 'plots','sparseCalibrationConventional.pdf'), 
                        type = type)
  plotPredictionDistribution(result$performanceEvaluation, 
                             fileName=file.path(filename, 'plots','predictionDistribution.pdf'), 
                             type = type)
  
  plotVariableScatterplot(result$covariateSummary, 
                          fileName=file.path(filename, 'plots','variableScatterplot.pdf'))
  if(type%in%c('test','train')){
    plotGeneralizability(result$covariateSummary, 
                               fileName=file.path(filename, 'plots','generalizability.pdf'))
  }
  
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
  
  if(is.null(evaluation$thresholdSummary$Eval)){
    evaluation$thresholdSummary$Eval <- type
  }
  ind <- evaluation$thresholdSummary$Eval==type
  
  x<- evaluation$thresholdSummary[ind,c('falsePositiveRate','sensitivity')]
  x <- x[order(x$falsePositiveRate, x$sensitivity),]
  
  # add the bit to get the step
  stepsExtra <- cbind(x[-1,1], x[-nrow(x),2])
  colnames( stepsExtra) <- colnames(x)
  x <- rbind(c(1,1), x, stepsExtra, c(0,0))
  x <- x[order(x$falsePositiveRate, x$sensitivity),]
  
  #plot <- ggplot2::ggplot(x, ggplot2::aes(x$falsePositiveRate, x$sensitivity)) +
  plot <- ggplot2::ggplot(x, ggplot2::aes(falsePositiveRate, sensitivity)) +
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
  if(is.null(evaluation$thresholdSummary$Eval)){
    evaluation$thresholdSummary$Eval <- type
  }
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
  
  plot <- ggplot2::ggplot(x, ggplot2::aes(x=value,
                                          group=variable,
                                          fill=variable)) +
    ggplot2::geom_density(ggplot2::aes(x=value, fill=variable), alpha=.3) +
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
  if(is.null(evaluation$thresholdSummary$Eval)){
    evaluation$thresholdSummary$Eval <- type
  }
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
  
  plot <- ggplot2::ggplot(x, ggplot2::aes(x=value,
                                          group=variable,
                                          fill=variable)) +
    ggplot2::geom_density(ggplot2::aes(x=value, fill=variable), alpha=.3) +
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
  if(is.null(evaluation$thresholdSummary$Eval)){
    evaluation$thresholdSummary$Eval <- type
  }
  ind <- evaluation$thresholdSummary$Eval==type
  
  N <- sum(evaluation$calibrationSummary$PersonCountAtRisk, na.rm = T)
  O <- sum(evaluation$calibrationSummary$PersonCountWithOutcome, na.rm=T)
  inc <- O/N
  
  x<- evaluation$thresholdSummary[ind,c('positivePredictiveValue', 'sensitivity')]
  #x <- rbind(c(0,1), x, c(1,0))
  
  plot <- ggplot2::ggplot(x, ggplot2::aes(sensitivity, positivePredictiveValue)) +
    ggplot2::geom_line(size=1) +
    ggplot2::scale_x_continuous("Recall")+#, limits=c(0,1)) +
    ggplot2::scale_y_continuous("Precision") + #, limits=c(0,1))
    ggplot2::geom_hline(yintercept = inc, linetype="dashed", 
                        color = "red", size=1) 
  
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
  if(is.null(evaluation$thresholdSummary$Eval)){
    evaluation$thresholdSummary$Eval <- type
  }
  ind <- evaluation$thresholdSummary$Eval==type
  
  x<- evaluation$thresholdSummary[ind,c('predictionThreshold', 'f1Score')]
  #x <- rbind(c(0,1), x, c(1,0))
  
  plot <- ggplot2::ggplot(x, ggplot2::aes(predictionThreshold, f1Score)) +
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
    if(is.null(evaluation$demographicSummary$Eval)){
      evaluation$demographicSummary$Eval <- type
    }
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
  if(is.null(evaluation$calibrationSummary$Eval)){
    evaluation$calibrationSummary$Eval <- type
  }
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
  if(is.null(evaluation$calibrationSummary$Eval)){
    evaluation$calibrationSummary$Eval <- type
  }
  ind <- evaluation$calibrationSummary$Eval==type
  
  x<- evaluation$calibrationSummary[ind,c('averagePredictedProbability','observedIncidence', 'PersonCountAtRisk')]
  
  
  cis <- apply(x, 1, function(x) binom.test(x[2]*x[3], x[3], alternative = c("two.sided"), conf.level = 0.95)$conf.int)
  x$lci <- cis[1,]  
  x$uci <- cis[2,]
  
  maxes <- max(max(x$averagePredictedProbability), max(x$observedIncidence))*1.1
  
  # limits <- ggplot2::aes(ymax = x$uci, ymin= x$lci)
  limits <- ggplot2::aes(ymax = uci, ymin= lci)
  
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
# SMOOTHCALIBRATION plot
#=============
#' Plot the smooth calibration as detailed in Calster et al. "A calibration heirarchy for risk models
#' was defined: from utopia to empirical data" (2016)
#'
#' @details
#' Create a plot showing the smoothed calibration #'
#' @param result     The result of running \code{\link{runPlp}} function. An object containing the
#'                   model or location where the model is save, the data selection settings, the
#'                   preprocessing and training settings as well as various performance measures
#'                   obtained by the model.
#'
#' @param smooth     options: 'loess' or 'rcs'
#' @param span       This specifies the width of span used for loess. This will allow for faster
#'                   computing and lower memory usage.
#' @param nKnots     The number of knots to be used by the rcs evaluation. Default is 5
#' @param scatter    plot the decile calibrations as points on the graph. Default is False
#' @param type       Whether to use train or test data, default is test.
#' @param bins       The number of bins for the histogram. Default is 20.
#' @param zoom       Zoom in on the region containing the deciles or on the data. If not specified
#'                   shows the entire space.
#' @param fileName   Name of the file where the plot should be saved, for example 'plot.png'. See the
#'                   function \code{ggsave} in the ggplot2 package for supported file formats.
#' @return
#' A cowplot object. Use the \code{cowplot::save_plot} function to save to file in a different format.
#'
#' @export

plotSmoothCalibration <- function(result,
                                  smooth = c("loess", "rcs"),
                                  span = 1,
                                  nKnots = 5,
                                  scatter = F,
                                  type = "test",
                                  bins = 20,
                                  zoom = c("none", "deciles", "data"),
                                  fileName = NULL) {
  prediction <- result$prediction
  evaluation <- result$performanceEvaluation
  if(is.null(result$performanceEvaluation$calibrationSummary$Eval)){
    result$performanceEvaluation$calibrationSummary$Eval <- type
  }
  ind <- result$performanceEvaluation$calibrationSummary$Eval == type
  x <- evaluation$calibrationSummary[ind, c("averagePredictedProbability", "observedIncidence")]
  maxVal <- max(x$averagePredictedProbability, x$observedIncidence)
  maxes <- max(max(x$averagePredictedProbability), max(x$observedIncidence))
  type <- ifelse(is.null(prediction$indexes), "apply", "test")

  if (type == "test")
    ind <- prediction$indexes < 0  # select the cases in the test set
 else ind <- rep(T, length(prediction$outcomeCount))  # in case of applyMOdel

  if (smooth == "rcs") {

    if (missing(nKnots)) {

      message("Number of knots for the restricted cubic spline smoothing was automatically set to 5")

    }

    if (!nKnots %in% 3:5)
      stop("Number of knots must be between 3 and 5")
  }


  y <- prediction$outcomeCount[ind]
  p <- prediction$value[ind]

  nma <- !is.na(p + y)  # select only non-missing cases
  sumNA <- sum(!nma)
  if (sumNA > 0)
    warning(paste(sumNA, "observations deleted due to NA probabilities or outcomes"))
  y <- y[nma]
  p <- p[nma]

  logit <- log(p/(1 - p))  # delete cases with 0 and 1 probs
  nonInf <- !is.infinite(logit)
  sumNonInf <- sum(!nonInf)
  if (sumNonInf > 0)
    warning(paste(sumNonInf, "observations deleted due to probabilities of 0 or 1"))
  y <- y[nonInf]
  p <- p[nonInf]

  y <- y[order(p)]
  p <- p[order(p)]


  if (smooth == "loess") {
    # loess

    if (length(y) > 5000)
      message("Number of observations above 5000. Restricted cubic splines smoothing would be preferable")

    smoothData <- data.frame(y, p)
    # limits for zoom functionality
    if (zoom == "data") {
      # xlim <- c(min(smoothData$p), max(smoothData$p))
      fit <- stats::loess(y ~ p, degree = 2)
      maxes <- max(max(smoothData$p), max(fit$fitted))
      xlim <- c(0, maxes)
      ylim <- c(0, maxes)
    } else if (zoom == "deciles") {
      xlim <- c(0, maxes)
      ylim <- c(0, maxes)
    } else {
      xlim <- c(0, 1)
      ylim <- c(0, 1)
    }
    # the main plot object, this one uses Loess regression
    smooth_plot <- ggplot2::ggplot(data = smoothData, ggplot2::aes(x = p, y = y)) +
                   ggplot2::stat_smooth(ggplot2::aes(color = "Loess", linetype = "Loess"),
                                        method = "loess",
                                        se = TRUE,
                                        span = span,
                                        size = 1,
                                        show.legend = F) +
                   ggplot2::geom_segment(ggplot2::aes(x = 0,
                                                      xend = 1,
                                                      y = 0,
                                                      yend = 1,
                                                      color = "Ideal",
                                                      linetype = "Ideal")) +
                   # ggplot2::scale_y_continuous(limits = c(0,1)) +

    ggplot2::coord_cartesian(xlim = xlim,
                             ylim = ylim) + ggplot2::scale_linetype_manual(name = "Models",
                                                                                        values = c(Loess = "solid",
                                                                                                   Ideal = "dashed")) + ggplot2::scale_color_manual(name = "Models", values = c(Loess = "blue", Ideal = "red")) + ggplot2::labs(x = "Predicted Probability", y = "Observed Probability")
  } else {
    # Restricted cubic splines

    expit <- function(x) exp(x)/(1 + exp(x))
    dd <- data.frame(y, p)

    if (nKnots == 5) {
      smoothFit <- tryCatch(invisible(utils::capture.output(rms::lrm(y ~ rms::rcs(p, 5),
                                                                     x = T,
                                                                     y = T))), error = function(e) {
        warning("Setting number of Knots to 5 led to estimation problems. Switching to nKnots = 4",
                immediate. = T)
        tryCatch(invisible(utils::capture.output(rms::lrm(y ~ rms::rcs(p, 4), x = T, y = T))),
                 error = function(e) {
          warning("Setting number of Knots to 4 led to estimation problems. Switching to nKnots = 3",
                  immediate. = T)
          rms::lrm(y ~ rms::rcs(p, 3), x = T, y = T)
          })
      })
    } else if (nKnots == 4) {
      smoothFit <- tryCatch(invisible(utils::capture.output(rms::lrm(y ~ rms::rcs(p, 4),
                                                                     x = T,
                                                                     y = T))), error = function(e) {
        warning("Setting number of Knots to 4 led to estimation problems. Switching to nKnots = 3",
                immediate. = T)
        rms::lrm(y ~ rms::rcs(p, 3), x = T, y = T)
      })

    } else {
      smoothFit <- rms::lrm(y ~ rms::rcs(p, 3), x = T, y = T)
    }

    # create the rcs mapping
    xRange <- seq(0, p[length(p)], length.out = 1000)
    pred <- stats::predict(smoothFit, xRange, se.fit = T, type = "lp")
    predXRange <- expit(pred$linear.predictors)

    # construct the zoom limits
    if (zoom == "data") {
      maxes <- max(max(xRange), max(predXRange))
      xlim <- c(0, maxes)
      ylim <- c(0, maxes)
    } else if (zoom == "deciles") {
      xlim <- c(0, maxes)
      ylim <- c(0, maxes)
    } else {
      xlim <- c(0, 1)
      ylim <- c(0, 1)
    }

    # confidence intervals
    ciSmooth <- data.frame(lci = expit(pred$linear.predictors - 1.96 * pred$se.fit),
                           uci = expit(pred$linear.predictors + 1.96 * pred$se.fit))

    smoothData <- cbind(xRange, predXRange, ciSmooth)

    # the main plot object, this one uses RCS
    smooth_plot <- ggplot2::ggplot(data = smoothData, ggplot2::aes(x = xRange, y = predXRange)) +
                   ggplot2::geom_line(ggplot2::aes(color = "rcs", linetype = "rcs")) +
                   ggplot2::geom_ribbon(ggplot2::aes(ymin = lci,
                                                     ymax = uci), fill = "blue", alpha = "0.2") +
                   ggplot2::geom_segment(ggplot2::aes(x = 0,
                                                      xend = 1,
                                                      y = 0,
                                                      yend = 1,
                                                      color = "Ideal",
                                                      linetype = "Ideal")) +
                   ggplot2::scale_color_manual(name = "Models",
                                               values = c(rcs = "blue", Ideal = "red")) +
                   ggplot2::scale_linetype_manual(name = "Models",
                                                  values = c(rcs = "solid", Ideal = "dashed")) +
                   # ggplot2::scale_y_continuous(limits = c(0,1)) +

    ggplot2::coord_cartesian(xlim = xlim,
                             ylim = ylim) + ggplot2::labs(x = "", y = "Observed Probability")


  }
  # construct the plot grid
  if (scatter) {
    smooth_plot <- smooth_plot + ggplot2::geom_point(data = x,
                                                     ggplot2::aes(x = averagePredictedProbability,
                                                                  y = observedIncidence),
                                                     color = "black",
                                                     size = 2)
  }

  # Histogram object detailing the distibution of event/noevent for each probability interval

  hist_plot <- ggplot2::ggplot() +
               ggplot2::geom_histogram(data = subset(prediction, value <= xlim[2]),
                                       ggplot2::aes(value, y = ..count.., fill = as.character(outcomeCount)),
                                       bins = bins,
                                       position = "stack",
                                       alpha = 0.5,
                                       boundary = 0,
                                       closed = "left") +
               ggplot2::facet_grid(outcomeCount ~ ., scales = "free_y") +
               ggplot2::scale_fill_discrete(name = "Outcome") +
               ggplot2::theme(strip.background = ggplot2::element_blank(),
                              strip.text = ggplot2::element_blank()) +
               ggplot2::labs(x = "Predicted Probability") +
               ggplot2::coord_cartesian(xlim = xlim)

  plot <- cowplot::plot_grid(smooth_plot,
                             hist_plot,
                             ncol = 1,
                             axis = "lr",
                             align = "v",
                             rel_heights = c(1, 0.6))
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 5, height = 4.5, dpi = 400)
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
  if(is.null(evaluation$predictionDistribution$Eval)){
    evaluation$predictionDistribution$Eval <- type
  }
  ind <- evaluation$predictionDistribution$Eval==type
  x<- evaluation$predictionDistribution[ind,]

  #(x=Class, y=predictedProbabllity sequence:  min->P05->P25->Median->P75->P95->max)
  
  
  non05 <- x$P05PredictedProbability[x$class==0]
  non95 <- x$P95PredictedProbability[x$class==0]
  one05 <- x$P05PredictedProbability[x$class==1]
  one95 <- x$P95PredictedProbability[x$class==1]
  
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
    ggplot2::geom_segment(ggplot2::aes(x = 0.9, y = non05, 
                     xend = 1.1, yend = non05), color='red') +
    ggplot2::geom_segment(ggplot2::aes(x = 0.9, y = non95, 
                                       xend = 1.1, yend = non95), color='red') +
  ggplot2::geom_segment(ggplot2::aes(x = 1.9, y = one05, 
                                     xend = 2.1, yend = one05)) +
    ggplot2::geom_segment(ggplot2::aes(x = 1.9, y = one95, 
                                       xend = 2.1, yend = one95))
  
  
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

#' @title plotLearningCurve
#'
#' @description Create a plot of the learning curve using the object returned
#' from \code{createLearningCurve}.
#'
#' @param learningCurve An object returned by \code{\link{createLearningCurve}}
#'   function.
#' @param metric Specifies the metric to be plotted:
#'   \itemize{
#'     \item{\code{'AUROC'} - use the area under the Receiver Operating
#'       Characteristic curve}
#'     \item{\code{'AUPRC'} - use the area under the Precision-Recall curve}
#'     \item{\code{'sBrier'} - use the scaled Brier score}
#'   }
#' @param abscissa Specify the abscissa metric to be plotted:
#'   \itemize{
#'     \item{\code{'observations'} - use number of observations}
#'     \item{\code{'outcomes'} - use number of positive outcomes}
#'   }
#' @param plotTitle Title of the learning curve plot.
#' @param plotSubtitle Subtitle of the learning curve plot.
#' @param fileName Filename of plot to be saved, for example \code{'plot.png'}.
#'   See the function \code{ggsave} in the ggplot2 package for supported file 
#'   formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to 
#' file in a different format.
#' 
#' @examples
#' \dontrun{
#' # create learning curve object
#' learningCurve <- createLearningCurve(population,
#'                                      plpData,
#'                                      modelSettings)
#' # plot the learning curve
#' plotLearningCurve(learningCurve)
#' }
#' 
#' @export
plotLearningCurve <- function(learningCurve,
                              metric = "AUROC",
                              abscissa = "observations",
                              plotTitle = "Learning Curve", 
                              plotSubtitle = NULL,
                              fileName = NULL){
  
  # rename dataframe columns
  colnames(learningCurve) <- c("Fraction", "Observations", "Occurrences",
                               "Time", "TrainROC", "TestROC", "TrainPR",
                               "TestPR", "TrainBrierScore", "TestBrierScore",
                               "TrainBrierScaled", "TestBrierScaled",
                               "TrainCalibrationIntercept",
                               "TestCalibrationIntercept",
                               "TrainCalibrationSlope", "TestCalibrationSlope")
  tidyLearningCurve <- NULL
  yAxisRsnge <- NULL
  y <- NULL
  
  # check for performance metric to plot
  if(metric == "AUROC") {
    # tidy up dataframe
    tidyLearningCurve <- learningCurve %>%
      dplyr::rename(Training = TrainROC, Testing = TestROC) %>%
      tidyr::gather(Dataset, AUROC, c(Training, Testing), factor_key = FALSE)
    
    # define plot properties
    yAxisRange <- c(0.5, 1.0)
    y <- "AUROC"
    
  } else if (metric == "AUPRC") {
    # tidy up dataframe
    tidyLearningCurve <- learningCurve %>%
      dplyr::rename(Training = TrainPR, Testing = TestPR) %>%
      tidyr::gather(Dataset, AUPRC, c(Training, Testing), factor_key = FALSE)
    
    # define plot properties
    yAxisRange <- c(0.0, 1.0)
    y <- "AUPRC"
    
  } else if (metric == "sBrier") {
    # tidy up dataframe
    tidyLearningCurve <- learningCurve %>%
      dplyr::rename(Training = TrainBrierScaled, Testing = TestBrierScaled) %>%
      tidyr::gather(Dataset, sBrier, c(Training, Testing), factor_key = FALSE)
    
    # define plot properties
    yAxisRange <- c(0.0, 1.0)
    y <- "sBrier"
  } else {
    stop("An incorrect metric has been specified.")
  }
  
  if (abscissa == "observations") {
    abscissa <- "Observations"
    abscissaLabel <- "Training set size"
  } else if (abscissa == "outcomes") {
    abscissa <- "Occurrences"
    abscissaLabel <- "Positive outcomes"
  } else {
    stop("An incorrect abscissa has been specified.")
  }
  
  # create plot object
  plot <- tidyLearningCurve %>%
    ggplot2::ggplot(ggplot2::aes_string(x = abscissa, y = y,
                                        col = "Dataset")) +
    ggplot2::geom_line() +
    ggplot2::coord_cartesian(ylim = yAxisRange, expand = FALSE) +
    ggplot2::labs(title = plotTitle, subtitle = plotSubtitle, 
                  x = abscissaLabel) +
    ggplot2::theme_light()
  
  # save plot, if fucntion call provides a file name
  if ((!is.null(fileName)) & (is.character(fileName))) {
    ggplot2::ggsave(fileName, plot, width = 5, height = 4.5, dpi = 400)
  }
  
  return(plot)
}
