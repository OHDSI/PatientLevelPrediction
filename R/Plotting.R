# @file Plotting.R
#
# Copyright 2021 Observational Health Data Sciences and Informatics
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
#' @param populationSettings    The population settings created using \code{createStudyPopulationSettings}
#' @param riskTable           (binary) Whether to include a table at the bottom  of the plot showing the number of people at risk over time
#' @param confInt             (binary) Whether to include a confidence interval
#' @param yLabel              (string) The label for the y-axis         
#'
#' @return
#' TRUE if it ran 
#'
#' @export
outcomeSurvivalPlot <- function(
  plpData, 
  outcomeId,
  populationSettings = createStudyPopulationSettings(
    binary = T,
    includeAllOutcomes = T,
    firstExposureOnly = FALSE,
    washoutPeriod = 0,
    removeSubjectsWithPriorOutcome = TRUE,
    priorOutcomeLookback = 99999,
    requireTimeAtRisk = F,
    riskWindowStart = 1,
    startAnchor = 'cohort start',
    riskWindowEnd = 3650,
    endAnchor = "cohort start"
  ),
  riskTable = T,
  confInt= T, 
  yLabel = 'Fraction of those who are outcome free in target population'
)
{
  
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
  
  populationSettings$plpData <- plpData 
  populationSettings$outcomeId <- outcomeId
  
  population <- do.call(
    what = 'createStudyPopulation',
    args = list(
      plpData = plpData, 
      outcomeId = outcomeId, 
      populationSettings = populationSettings
      )
    )
  
  population$daysToEvent[is.na(population$daysToEvent)] <- population$survivalTime[is.na(population$daysToEvent)]
  survivalFit <- survival::survfit(
    survival::Surv(daysToEvent, outcomeCount)~cohortId,
    #riskDecile, 
    population, 
    conf.int = TRUE
  )
  
  if(!is.null(survivalFit$surv)){
    yliml <- min(survivalFit$surv)
  } else{
    yliml <- 0.5
  }
  
  result <- survminer::ggsurvplot(
    fit=survivalFit, 
    data = population,
    risk.table = riskTable,
    pval = F,
    xlim = c(0,populationSettings$riskWindowEnd), 
    ylim = c(yliml*0.95,1),
    conf.int = confInt,
    ggtheme = ggplot2::theme_minimal(),
    risk.table.y.text.col = T,
    risk.table.y.text = FALSE,
    ylab = yLabel
  )
  return(result)
}


#' Plot all the PatientLevelPrediction plots
#'
#' @details
#' Create a directory with all the plots
#'
#' @param plpResult               Object returned by the runPlp() function
#' @param saveLocation            Name of the directory where the plots should be saved (NULL means no saving)
#' @param typeColumn              The name of the column specifying the evaluation type 
#'                                (to stratify the plots)                           
#'
#' @return
#' TRUE if it ran 
#'
#' @export
plotPlp <- function(
  plpResult, 
  saveLocation = NULL, 
  typeColumn = 'evaluation'
  ){
  
  # check inputs
  if(!is.null(saveLocation)){
    if(!dir.exists(saveLocation)){
      dir.create(saveLocation, recursive =T)
    }
  }
  
  # run each of the plots:
  plotSparseRoc(
    plpResult = plpResult,
    typeColumn = typeColumn,
    saveLocation = saveLocation,
    fileName = 'sparseROC.pdf'
  )
  
  plotPredictedPDF(
    plpResult, 
    saveLocation = saveLocation, 
    fileName = 'predictedPDF.pdf',
    typeColumn = typeColumn)
  plotPreferencePDF(
    plpResult, 
    saveLocation = saveLocation, 
    fileName = 'preferencePDF.pdf', 
    typeColumn = typeColumn)
  plotPrecisionRecall(
    plpResult, 
    saveLocation = saveLocation, 
    fileName = 'precisionRecall.pdf', 
    typeColumn = typeColumn)
  plotF1Measure(
    plpResult, 
    saveLocation = saveLocation,
    fileName = 'f1Measure.pdf', 
    typeColumn = typeColumn)
  plotDemographicSummary(
    plpResult, 
    saveLocation = saveLocation, 
    fileName = 'demographicSummary.pdf',
    typeColumn = typeColumn)
  
  # add smooth calibration
  tryCatch({
  plotSmoothCalibration(plpResult = plpResult, smooth = 'loess', nKnots = 5, 
                        type = 'Test', zoom = 'data', typeColumn = typeColumn,
    saveLocation = saveLocation,
    fileName = 'smooothCalibration.pdf'
    )
    }, error = function(e) {
      return(NULL)
    }) 
  
  plotSparseCalibration(
    plpResult, 
    saveLocation = saveLocation, 
    fileName = 'sparseCalibration.pdf', 
    typeColumn = typeColumn)
  plotSparseCalibration2(
    plpResult,
    saveLocation = saveLocation, 
    fileName = 'sparseCalibrationConventional.pdf', 
    typeColumn = typeColumn)
  plotPredictionDistribution(
    plpResult, 
    saveLocation = saveLocation, 
    fileName = 'predictionDistribution.pdf', 
    typeColumn = typeColumn)
  
  plotVariableScatterplot(
    plpResult$covariateSummary,
    saveLocation = saveLocation, 
    fileName = 'variableScatterplot.pdf'
  )
  
  
  if(sum(c('TrainWithNoOutcome_CovariateMean', 'TestWithNoOutcome_CovariateMean') %in% colnames(plpResult$covariateSummary))==2){
    plotGeneralizability(
      plpResult$covariateSummary, 
      saveLocation = saveLocation, 
      fileName = 'generalizability.pdf'
    )
  }
  
  return(invisible(TRUE))
}



#=============
# THERSHOLDSUMMARY PLOTS 
#=============


#' Plot the ROC curve using the sparse thresholdSummary data frame
#'
#' @details
#' Create a plot showing the Receiver Operator Characteristics (ROC) curve.
#'
#' @param plpResult            A plp result object as generated using the \code{\link{runPlp}} function.
#' @param typeColumn            The name of the column specifying the evaluation type
#' @param saveLocation          Directory to save plot (if NULL plot is not saved)
#' @param fileName              Name of the file to save to plot, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotSparseRoc <- function(
  plpResult,
  typeColumn = 'evaluation',
  saveLocation = NULL,
  fileName = 'roc.png'
){
  
  evalTypes <- unique(plpResult$performanceEvaluation$thresholdSummary[,typeColumn])
  
  plots <- list()
  length(plots) <- length(evalTypes)
  
  for(i in 1:length(evalTypes)){
    evalType <- evalTypes[i]
    x <- plpResult$performanceEvaluation$thresholdSummary %>% 
      dplyr::filter(.data[[typeColumn]] == evalType) %>% 
      dplyr::select(.data$falsePositiveRate, .data$sensitivity)
    
    #x <- thresholdSummary[,c('falsePositiveRate','sensitivity')]
    x <- x[order(x$falsePositiveRate, x$sensitivity),]
    
    # add the bit to get the step
    stepsExtra <- cbind(x[-1,1], x[-nrow(x),2])
    colnames( stepsExtra) <- colnames(x)
    x <- rbind(c(1,1), x, stepsExtra, c(0,0))
    x <- x[order(x$falsePositiveRate, x$sensitivity),]
    
    plots[[i]] <- ggplot2::ggplot(
      x, 
      ggplot2::aes(
        .data$falsePositiveRate, 
        .data$sensitivity
        )
    ) +
      ggplot2::geom_polygon(fill = "blue", alpha = 0.2) +
      ggplot2::geom_line(size=1) +
      ggplot2::geom_abline(intercept = 0, slope = 1,linetype = 2) +
      ggplot2::scale_x_continuous("1 - specificity", limits=c(0,1)) +
      ggplot2::scale_y_continuous("Sensitivity", limits=c(0,1)) + 
      ggplot2::ggtitle(evalType)
  }
  
  plot <- gridExtra::marrangeGrob(plots, nrow=length(plots), ncol=1)
  
  if (!is.null(saveLocation)){
    if(!dir.exists(saveLocation)){
      dir.create(saveLocation, recursive = T)
    }
    ggplot2::ggsave(file.path(saveLocation, fileName), plot, width = 5, height = 4.5, dpi = 400)
  }
  return(plot)
}


#' Plot the Predicted probability density function, showing prediction overlap between true and false cases
#'
#' @details
#' Create a plot showing the predicted probability density function, showing prediction overlap between true and false cases
#'
#' @param plpResult            A plp result object as generated using the \code{\link{runPlp}} function.
#' @param typeColumn            The name of the column specifying the evaluation type
#' @param saveLocation          Directory to save plot (if NULL plot is not saved)
#' @param fileName              Name of the file to save to plot, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotPredictedPDF <- function(
  plpResult,
  typeColumn = 'evaluation',
  saveLocation = NULL, 
  fileName = 'PredictedPDF.png'
  )
  {
  
  evalTypes <- unique(plpResult$performanceEvaluation$thresholdSummary[,typeColumn])
  
  plots <- list()
  length(plots) <- length(evalTypes)
  
  for(ind in 1:length(evalTypes)){
    evalType <- evalTypes[ind]
    x <- plpResult$performanceEvaluation$thresholdSummary %>% 
      dplyr::filter(.data[[typeColumn]] == evalType) %>% 
      dplyr::select(
        .data$predictionThreshold,
        .data$truePositiveCount,
        .data$trueNegativeCount,
        .data$falsePositiveCount,
        .data$falseNegativeCount
      )
  
  x<- x[order(x$predictionThreshold,-x$truePositiveCount, -x$falsePositiveCount),]
  x$out <- c(x$truePositiveCount[-length(x$truePositiveCount)]-x$truePositiveCount[-1], x$truePositiveCount[length(x$truePositiveCount)])
  x$nout <- c(x$falsePositiveCount[-length(x$falsePositiveCount)]-x$falsePositiveCount[-1], x$falsePositiveCount[length(x$falsePositiveCount)])
  
  vals <- c()
  for(i in 1:length(x$predictionThreshold)){
    if(i!=length(x$predictionThreshold)){
      upper <- x$predictionThreshold[i+1]} else {upper <- min(x$predictionThreshold[i]+0.01,1)}
  val <- x$predictionThreshold[i]+stats::runif(x$out[i])*(upper-x$predictionThreshold[i])
  vals <- c(val, vals)
  } 
  vals <- vals[!is.na(vals)] #assigned
  
  vals2 <- c()
  for(i in 1:length(x$predictionThreshold)){
    if(i!=length(x$predictionThreshold)){
      upper <- x$predictionThreshold[i+1]} else {upper <- min(x$predictionThreshold[i]+0.01,1)}
    val2 <- x$predictionThreshold[i]+stats::runif(x$nout[i])*(upper-x$predictionThreshold[i])
    vals2 <- c(val2, vals2)
  }
  vals2 <- vals2[!is.na(vals2)] #assigned
  
  x <- rbind(data.frame(variable=rep('outcome',length(vals)), value=vals),
             data.frame(variable=rep('No outcome',length(vals2)), value=vals2)
  )
  
  plots[[ind]] <- ggplot2::ggplot(x, ggplot2::aes(x=.data$value,
                                          group=.data$variable,
                                          fill=.data$variable)) +
    ggplot2::geom_density(ggplot2::aes(x=.data$value, fill=.data$variable), alpha=.3) +
    ggplot2::scale_x_continuous("Prediction Threshold") + #, limits=c(0,1)) +
    ggplot2::scale_y_continuous("Density") + 
    ggplot2::guides(fill=ggplot2::guide_legend(title="Class")) + 
    ggplot2::ggtitle(evalType)
  }
  
  plot <- gridExtra::marrangeGrob(plots, nrow=length(plots), ncol=1)
  
  if (!is.null(saveLocation)){
    if(!dir.exists(saveLocation)){
      dir.create(saveLocation, recursive = T)
    }
    ggplot2::ggsave(file.path(saveLocation, fileName), plot, width = 5, height = 4.5, dpi = 400)
  }
  return(plot)
}



#' Plot the preference score probability density function, showing prediction overlap between true and false cases
#' #'
#' @details
#' Create a plot showing the preference score probability density function, showing prediction overlap between true and false cases
#' #'
#' @param plpResult            A plp result object as generated using the \code{\link{runPlp}} function.
#' @param typeColumn            The name of the column specifying the evaluation type
#' @param saveLocation          Directory to save plot (if NULL plot is not saved)
#' @param fileName              Name of the file to save to plot, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotPreferencePDF <- function(
  plpResult,
  typeColumn = 'evaluation',
  saveLocation = NULL,
  fileName = 'plotPreferencePDF.png'
){
  
  evalTypes <- unique(plpResult$performanceEvaluation$thresholdSummary[,typeColumn])
  
  plots <- list()
  length(plots) <- length(evalTypes)
  
  for(ind in 1:length(evalTypes)){
    evalType <- evalTypes[ind]
    x <- plpResult$performanceEvaluation$thresholdSummary %>% 
      dplyr::filter(.data[[typeColumn]] == evalType) %>% 
      dplyr::select(
        .data$preferenceThreshold,
        .data$truePositiveCount,
        .data$trueNegativeCount,
        .data$falsePositiveCount,
        .data$falseNegativeCount
      )
  
  x<- x[order(x$preferenceThreshold,-x$truePositiveCount, x$trueNegativeCount),]
  x$out <- c(x$truePositiveCount[-length(x$truePositiveCount)]-x$truePositiveCount[-1], x$truePositiveCount[length(x$truePositiveCount)])
  x$nout <- c(x$falsePositiveCount[-length(x$falsePositiveCount)]-x$falsePositiveCount[-1], x$falsePositiveCount[length(x$falsePositiveCount)])
  
  vals <- c()
  for(i in 1:length(x$preferenceThreshold)){
    if(i!=length(x$preferenceThreshold)){
    upper <- x$preferenceThreshold[i+1]} else {upper <- 1}
    val <- x$preferenceThreshold[i]+stats::runif(x$out[i])*(upper-x$preferenceThreshold[i])
    vals <- c(val, vals)
  }
  vals <- vals[!is.na(vals)]
  
  vals2 <- c()
  for(i in 1:length(x$preferenceThreshold)){
    if(i!=length(x$preferenceThreshold)){
      upper <- x$preferenceThreshold[i+1]} else {upper <- 1}
    val2 <- x$preferenceThreshold[i]+stats::runif(x$nout[i])*(upper-x$preferenceThreshold[i])
    vals2 <- c(val2, vals2)
  }
  vals2 <- vals2[!is.na(vals2)]
  
  x <- rbind(data.frame(variable=rep('outcome',length(vals)), value=vals),
             data.frame(variable=rep('No outcome',length(vals2)), value=vals2)
  )
  
  plots[[ind]] <- ggplot2::ggplot(x, ggplot2::aes(x=.data$value,
                                          group=.data$variable,
                                          fill=.data$variable)) +
    ggplot2::geom_density(ggplot2::aes(x=.data$value, fill=.data$variable), alpha=.3) +
    ggplot2::scale_x_continuous("Preference Threshold")+#, limits=c(0,1)) +
    ggplot2::scale_y_continuous("Density") + 
    ggplot2::guides(fill=ggplot2::guide_legend(title="Class")) +
    ggplot2::ggtitle(evalType)
  
  }
  
  plot <- gridExtra::marrangeGrob(plots, nrow=length(plots), ncol=1)
  
  if (!is.null(saveLocation)){
    if(!dir.exists(saveLocation)){
      dir.create(saveLocation, recursive = T)
    }
    ggplot2::ggsave(file.path(saveLocation, fileName), plot, width = 5, height = 4.5, dpi = 400)
  }
  return(plot)
}



#' Plot the precision-recall curve using the sparse thresholdSummary data frame
#'
#' @details
#' Create a plot showing the precision-recall curve using the sparse thresholdSummary data frame
#'
#' @param plpResult            A plp result object as generated using the \code{\link{runPlp}} function.
#' @param typeColumn            The name of the column specifying the evaluation type
#' @param saveLocation          Directory to save plot (if NULL plot is not saved)
#' @param fileName              Name of the file to save to plot, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotPrecisionRecall <- function(
  plpResult,
  typeColumn = 'evaluation',
  saveLocation = NULL,
  fileName = 'roc.png'
){
  
  evalTypes <- unique(plpResult$performanceEvaluation$thresholdSummary[,typeColumn])
  
  plots <- list()
  length(plots) <- length(evalTypes)
  
  for(i in 1:length(evalTypes)){
    evalType <- evalTypes[i]
    
    N <- max(plpResult$performanceEvaluation$thresholdSummary %>% 
        dplyr::filter(.data[[typeColumn]] == evalType) %>% 
        dplyr::select(.data$falseCount) %>% 
        dplyr::pull(), na.rm = T)

    
    O <- max(plpResult$performanceEvaluation$thresholdSummary %>% 
        dplyr::filter(.data[[typeColumn]] == evalType) %>% 
        dplyr::select(.data$trueCount) %>% 
        dplyr::pull(), na.rm = T)
    
    inc <- O/(O + N)
    
    x <- plpResult$performanceEvaluation$thresholdSummary %>% 
      dplyr::filter(.data[[typeColumn]] == evalType) %>% 
      dplyr::select(.data$positivePredictiveValue, .data$sensitivity)
    
    plots[[i]] <- ggplot2::ggplot(x, ggplot2::aes(.data$sensitivity, .data$positivePredictiveValue)) +
      ggplot2::geom_line(size=1) +
      ggplot2::scale_x_continuous("Recall")+#, limits=c(0,1)) +
      ggplot2::scale_y_continuous("Precision") + #, limits=c(0,1))
      ggplot2::geom_hline(yintercept = inc, linetype="dashed", 
        color = "red", size=1)  +
      ggplot2::ggtitle(evalType)
  }
  
  plot <- gridExtra::marrangeGrob(plots, nrow=length(plots), ncol=1)
  
  if (!is.null(saveLocation)){
    if(!dir.exists(saveLocation)){
      dir.create(saveLocation, recursive = T)
    }
    ggplot2::ggsave(file.path(saveLocation, fileName), plot, width = 5, height = 4.5, dpi = 400)
  }
  return(plot)
}



#' Plot the F1 measure efficiency frontier using the sparse thresholdSummary data frame
#'
#' @details
#' Create a plot showing the F1 measure efficiency frontier using the sparse thresholdSummary data frame
#'
#' @param plpResult            A plp result object as generated using the \code{\link{runPlp}} function.
#' @param typeColumn            The name of the column specifying the evaluation type
#' @param saveLocation          Directory to save plot (if NULL plot is not saved)
#' @param fileName              Name of the file to save to plot, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotF1Measure <- function(
  plpResult,
  typeColumn = 'evaluation',
  saveLocation = NULL,
  fileName = 'roc.png'
){
  evalTypes <- unique(plpResult$performanceEvaluation$thresholdSummary[,typeColumn])
  
  plots <- list()
  length(plots) <- length(evalTypes)
  
  for(i in 1:length(evalTypes)){
    
    evalType <- evalTypes[i]
    
    x <- plpResult$performanceEvaluation$thresholdSummary %>% 
      dplyr::filter(.data[[typeColumn]] == evalType) %>% 
      dplyr::select(.data$predictionThreshold, .data$f1Score)
    
  if(sum(is.nan(x$f1Score))>0){
    x <- x[!is.nan(x$f1Score),]
    if(nrow(x)==0){return(NULL)}
  }
  
  plots[[i]] <- ggplot2::ggplot(x, ggplot2::aes(.data$predictionThreshold, .data$f1Score)) +
    ggplot2::geom_line(size=1) +
    ggplot2::geom_point(size=1) +
    ggplot2::scale_x_continuous("predictionThreshold")+#, limits=c(0,1)) +
    ggplot2::scale_y_continuous("F1Score") +#, limits=c(0,1))
    ggplot2::ggtitle(evalType)
  }
  
  plot <- gridExtra::marrangeGrob(plots, nrow=length(plots), ncol=1)
  
  if (!is.null(saveLocation)){
    if(!dir.exists(saveLocation)){
      dir.create(saveLocation, recursive = T)
    }
    ggplot2::ggsave(file.path(saveLocation, fileName), plot, width = 5, height = 4.5, dpi = 400)
  }
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
#' @param plpResult            A plp result object as generated using the \code{\link{runPlp}} function.
#' @param typeColumn            The name of the column specifying the evaluation type
#' @param saveLocation          Directory to save plot (if NULL plot is not saved)
#' @param fileName              Name of the file to save to plot, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotDemographicSummary <- function(
  plpResult,
  typeColumn = 'evaluation',
  saveLocation = NULL,
  fileName = 'roc.png'
)
{
  
  evalTypes <- unique(plpResult$performanceEvaluation$demographicSummary[,typeColumn])
  
  plots <- list()
  length(plots) <- length(evalTypes)
  
  for(i in 1:length(evalTypes)){
    evalType <- evalTypes[i]
    x <- plpResult$performanceEvaluation$demographicSummary %>% 
      dplyr::filter(.data[[typeColumn]] == evalType) %>% 
      dplyr::select(
        .data$ageGroup,
        .data$genGroup,
        .data$averagePredictedProbability,
        .data$PersonCountAtRisk, 
        .data$PersonCountWithOutcome
        )
  
    # remove -1 values:
    x$averagePredictedProbability[is.na(x$averagePredictedProbability)] <- 0
    x <- x[x$PersonCountWithOutcome != -1,]
    if(nrow(x)==0){
      return(NULL)
    }
    
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

    x <- tidyr::pivot_longer(
      data = x, 
      cols = colnames(x)[!colnames(x) %in% c('ageGroup','genGroup')], 
      names_to = 'variable',
      values_to = "value"
      )
    #x <- reshape2::melt(x, id.vars=c('ageGroup','genGroup'))
    
    # 1.96*StDevPredictedProbability
    ci <- plpResult$performanceEvaluation$demographicSummary %>% 
      dplyr::filter(.data[[typeColumn]] == evalType) %>% 
      dplyr::select(
        .data$ageGroup,
        .data$genGroup,
        .data$averagePredictedProbability,
        .data$StDevPredictedProbability
      )
    
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
    x <- x[!is.na(x$value),]
    
    plots[[i]] <- ggplot2::ggplot(data=x, 
                            ggplot2::aes(x=.data$age, 
                                         group=interaction(.data$variable,.data$genGroup))) +

      ggplot2::geom_line(ggplot2::aes(y=.data$value, group=.data$variable,
                                      color=.data$variable,
                                      linetype = .data$variable))+
      ggplot2::geom_ribbon(data=x[x$variable!='observed',],
                           ggplot2::aes(ymin=.data$lower, ymax=.data$upper
                                        , group=.data$genGroup), 
                           fill="blue", alpha=0.2) +
      ggplot2::facet_grid(.~ .data$genGroup, scales = "free") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
      ggplot2::scale_y_continuous("Fraction") +
      ggplot2::scale_x_discrete("Age") +
      ggplot2::scale_color_manual(values = c("royalblue4","red"),
                                  guide = ggplot2::guide_legend(title = NULL),
                                  labels = c("Expected", "Observed")) +

      ggplot2::guides(linetype=FALSE) +
      ggplot2::ggtitle(evalType)
  }
  
  plot <- gridExtra::marrangeGrob(plots, nrow=length(plots), ncol=1)
  
  if (!is.null(saveLocation)){
    if(!dir.exists(saveLocation)){
      dir.create(saveLocation, recursive = T)
    }
    ggplot2::ggsave(file.path(saveLocation, fileName), plot, width = 5, height = 4.5, dpi = 400)
  }
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
#' @param plpResult            A plp result object as generated using the \code{\link{runPlp}} function.
#' @param typeColumn            The name of the column specifying the evaluation type
#' @param saveLocation          Directory to save plot (if NULL plot is not saved)
#' @param fileName              Name of the file to save to plot, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotSparseCalibration <- function(
  plpResult,
  typeColumn = 'evaluation',
  saveLocation = NULL,
  fileName = 'roc.png'
){
  
  evalTypes <- unique(plpResult$performanceEvaluation$calibrationSummary[,typeColumn])
  
  plots <- list()
  length(plots) <- length(evalTypes)
  
  for(i in 1:length(evalTypes)){
    evalType <- evalTypes[i]
    x <- plpResult$performanceEvaluation$calibrationSummary %>% 
      dplyr::filter(.data[[typeColumn]] == evalType) %>% 
      dplyr::select(.data$averagePredictedProbability, .data$observedIncidence)

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
  plots[[i]] <- ggplot2::ggplot(data=x,
                          ggplot2::aes(x=.data$averagePredictedProbability, 
                                       y=.data$observedIncidence
                                       )) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin=.data$lci,ymax=.data$uci, x=x), 
                         fill="blue", alpha=0.2) +
    ggplot2::geom_point(size=1, color='darkblue') +
    ggplot2::coord_cartesian(ylim = c(0, maxVal), xlim =c(0,maxVal)) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 2, size=1,
                         show.legend = TRUE) +
    ggplot2::geom_abline(intercept = res['Intercept'], slope = res['Gradient'],
                         linetype = 1,show.legend = TRUE,
                         color='darkblue') +
    ggplot2::scale_x_continuous("Average Predicted Probability") +
    ggplot2::scale_y_continuous("Observed Fraction With Outcome") +
    ggplot2::ggtitle(evalType)
  }

  plot <- gridExtra::marrangeGrob(plots, nrow=length(plots), ncol=1)
  
  if (!is.null(saveLocation)){
    if(!dir.exists(saveLocation)){
      dir.create(saveLocation, recursive = T)
    }
    ggplot2::ggsave(file.path(saveLocation, fileName), plot, width = 5, height = 4.5, dpi = 400)
  }
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
#' @param plpResult            A plp result object as generated using the \code{\link{runPlp}} function.
#' @param typeColumn            The name of the column specifying the evaluation type
#' @param saveLocation          Directory to save plot (if NULL plot is not saved)
#' @param fileName              Name of the file to save to plot, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotSparseCalibration2 <- function(
  plpResult,
  typeColumn = 'evaluation',
  saveLocation = NULL,
  fileName = 'roc.png'
){
  
  evalTypes <- unique(plpResult$performanceEvaluation$calibrationSummary[,typeColumn])
  
  plots <- list()
  length(plots) <- length(evalTypes)
  
  for(i in 1:length(evalTypes)){
    evalType <- evalTypes[i]
    x <- plpResult$performanceEvaluation$calibrationSummary %>% 
      dplyr::filter(.data[[typeColumn]] == evalType) %>% 
      dplyr::select(.data$averagePredictedProbability, .data$observedIncidence, .data$PersonCountAtRisk)

  cis <- apply(x, 1, function(x) stats::binom.test(round(x[2]*x[3]), x[3], alternative = c("two.sided"), conf.level = 0.95)$conf.int)
  x$lci <- cis[1,]  
  x$uci <- cis[2,]
  
  maxes <- max(max(x$averagePredictedProbability), max(x$observedIncidence))*1.1
  
  limits <- ggplot2::aes(ymax = .data$uci, ymin= .data$lci)
  
  plots[[i]] <- ggplot2::ggplot(data=x,
                          ggplot2::aes(x=.data$averagePredictedProbability, y=.data$observedIncidence
                          )) +
    ggplot2::geom_point(size=2, color='black') +
    ggplot2::geom_errorbar(limits) +
    ggplot2::geom_line(colour='darkgrey') +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 5, size=0.4,
                         show.legend = TRUE) +
    ggplot2::scale_x_continuous("Average Predicted Probability") +
    ggplot2::scale_y_continuous("Observed Fraction With Outcome") +
    ggplot2::coord_cartesian(xlim = c(0, maxes), ylim=c(0,maxes)) +
    ggplot2::ggtitle(evalType)
  }
  
  plot <- gridExtra::marrangeGrob(plots, nrow=length(plots), ncol=1)
  
  if (!is.null(saveLocation)){
    if(!dir.exists(saveLocation)){
      dir.create(saveLocation, recursive = T)
    }
    ggplot2::ggsave(file.path(saveLocation, fileName), plot, width = 5, height = 4.5, dpi = 400)
  }
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
#' @param plpResult  The result of running \code{\link{runPlp}} function. An object containing the
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
#' @param sample     If using loess then by default 20,000 patients will be sampled to save time                   
#' @param typeColumn            The name of the column specifying the evaluation type
#' @param saveLocation          Directory to save plot (if NULL plot is not saved)
#' @param fileName              Name of the file to save to plot, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#' @return
#' A ggplot object.
#'
#' @export
plotSmoothCalibration <- function(plpResult,
                                  smooth = c("loess", "rcs"),
                                  span = 1,
                                  nKnots = 5,
                                  scatter = F,
                                  type = "test",
                                  bins = 20,
                                  zoom = c("none", "deciles", "data"),
                                  sample = T,
  typeColumn = 'evaluation',
  saveLocation = NULL,
                                  fileName = NULL) {
  
  evalTypes <- unique(plpResult$performanceEvaluation$calibrationSummary[,typeColumn])
  
  plots <- list()
  length(plots) <- length(evalTypes)
  
  for(i in 1:length(evalTypes)){
    evalType <- evalTypes[i]
  
  
  if('prediction'%in%names(plpResult)){
    
    x <- plpResult$performanceEvaluation$calibrationSummary %>% 
      dplyr::filter(.data[[typeColumn]] == evalType) %>% 
      dplyr::select(.data$averagePredictedProbability, .data$observedIncidence)
    
    prediction <-  plpResult$prediction %>% dplyr::filter(.data[[typeColumn]] == evalType)
  
  maxVal <- max(x$averagePredictedProbability, x$observedIncidence)
  maxes <- max(max(x$averagePredictedProbability), max(x$observedIncidence))

  if (smooth == "rcs") {

    if (missing(nKnots)) {

      ParallelLogger::logInfo("Number of knots for the restricted cubic spline smoothing was automatically set to 5")

    }

    if (!nKnots %in% 3:20)
      stop("Number of knots must be between 3 and 20")
  }


  y <- prediction$outcomeCount
  p <- prediction$value

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
    
    if(sample){
      
      if(length(p)>40000){
      inds <- unique(c(0,seq(0, length(p), by = floor(length(p)/20000)), 
                       length(p)))
      p <- p[inds]
      y <- y[inds]
      } else if(length(p)>20000){
        inds <- sample(length(p), 20000)
        p <- p[inds]
        y <- y[inds] 
      }
      
    }
    # loess

    #if (length(y) > 5000)
    #  message("Number of observations above 5000. Restricted cubic splines smoothing would be preferable")

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
    smooth_plot <- ggplot2::ggplot(data = smoothData, ggplot2::aes(x = .data$p, y = .data$y)) +
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
      
      ggplot2::coord_cartesian(
        xlim = xlim,
        ylim = ylim
        ) + 
      ggplot2::scale_linetype_manual(
        name = "Models",
          values = c(
            Loess = "solid",
            Ideal = "dashed")
        ) + 
      ggplot2::scale_color_manual(
        name = "Models", 
        values = c(Loess = "blue", Ideal = "red")
        ) + 
      ggplot2::labs(
        x = "Predicted Probability", 
        y = "Observed Probability"
        )
  
  } else {
    # Restricted cubic splines

    expit <- function(x) exp(x)/(1 + exp(x))
    dd <- data.frame(y, p)
    
    smoothFit <- NULL
    while(nKnots>=3 && is.null(smoothFit)){
      
      if(nKnots>3){
      errorMessage <- paste0("Setting number of Knots to ",nKnots," led to estimation problems. Switching to nKnots = ",nKnots-1)
      } else{
        errorMessage <- paste0('Unable to fit model')
      }
      
      formSmooth <- paste0('y ~ rms::rcs(p, nKnots =',nKnots,')')
      smoothFit <- tryCatch(rms::lrm(stats::as.formula(formSmooth), data = dd, x = T,y = T), 
                            error = function(e) {
                              ParallelLogger::logInfo(e)
                              ParallelLogger::logInfo(errorMessage)
                              return(NULL)
      
    })
      nKnots <- nKnots-1
    }
    
    if(is.null(smoothFit)){
      return(NULL)
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
    smooth_plot <- ggplot2::ggplot(data = smoothData, ggplot2::aes(x = .data$xRange, y = .data$predXRange)) +
                   ggplot2::geom_line(ggplot2::aes(color = "rcs", linetype = "rcs")) +
                   ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lci,
                                                     ymax = .data$uci), fill = "blue", alpha = 0.2) +
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
                                                     ggplot2::aes(x = .data$averagePredictedProbability,
                                                                  y = .data$observedIncidence),
                                                     color = "black",
                                                     size = 2)
  }
  
  # Histogram object detailing the distibution of event/noevent for each probability interval
  count <- NULL
  hist_plot <- ggplot2::ggplot() +
    ggplot2::geom_histogram(data = prediction[prediction$value <= xlim[2],],
                            ggplot2::aes(.data$value, y = ggplot2::after_stat(count), 
                                         fill = as.character(.data$outcomeCount)), #MAYBE ISSUE
                            bins = bins,
                            position = "stack",
                            alpha = 0.5,
                            boundary = 0,
                            closed = "left") +
    ggplot2::facet_grid(.data$outcomeCount ~ ., scales = "free_y") +
    ggplot2::scale_fill_discrete(name = "Outcome") +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_blank()) +
    ggplot2::labs(x = "Predicted Probability") +
    ggplot2::coord_cartesian(xlim = xlim)
  
  }else{
    # use calibrationSummary
    sparsePred <- plpResult$performanceEvaluation$calibrationSummary %>% 
      dplyr::filter(.data[[typeColumn]] == evalType)
    
    limVal <- max(max(sparsePred$averagePredictedProbability),max(sparsePred$observedIncidence))
    
    smooth_plot <- ggplot2::ggplot(data = sparsePred, ggplot2::aes(x = .data$averagePredictedProbability, 
                                                                   y = .data$observedIncidence)) +
      ggplot2::stat_smooth(ggplot2::aes(color = "Loess", linetype = "Loess"),
                           method = "loess",
                           se = TRUE,
                           #span = span,
                           size = 1,
                           show.legend = F) +
      ggplot2::geom_segment(ggplot2::aes(x = 0,
                                         xend = 1,
                                         y = 0,
                                         yend = 1,
                                         color = "Ideal",
                                         linetype = "Ideal")) +
      ggplot2::coord_cartesian(xlim = c(0,limVal),
                               ylim = c(0,limVal)) + 
      ggplot2::scale_linetype_manual(name = "Models",
                                     values = c(Loess = "solid",
                                                Ideal = "dashed")) + 
      ggplot2::scale_color_manual(name = "Models", values = c(Loess = "blue", Ideal = "red")) + 
      ggplot2::labs(x = "Predicted Probability", y = "Observed Probability")
    
    # construct the plot grid
    if (scatter) {
      smooth_plot <- smooth_plot + ggplot2::geom_point(data = sparsePred,
                                                       ggplot2::aes(x = .data$averagePredictedProbability,
                                                                    y = .data$observedIncidence),
                                                       color = "black",
                                                       size = 2)
    }
    
    # Histogram object detailing the distibution of event/noevent for each probability interval
    
    popData1 <- sparsePred[,c('averagePredictedProbability', 'PersonCountWithOutcome')]
    popData1$Label <- "Outcome"
    colnames(popData1) <- c('averagePredictedProbability','PersonCount',"Label")
    popData2 <- sparsePred[,c('averagePredictedProbability', 'PersonCountAtRisk')]
    popData2$Label <- "No Outcome"
    popData2$PersonCountAtRisk <- -1*(popData2$PersonCountAtRisk -popData1$PersonCount)
    colnames(popData2) <- c('averagePredictedProbability','PersonCount',"Label")
    popData <- rbind(popData1, popData2)
    popData$averagePredictedProbability <- factor(popData$averagePredictedProbability)
    hist_plot <- ggplot2::ggplot(popData, ggplot2::aes(y = .data$averagePredictedProbability, x = .data$PersonCount, 
                                                       fill = .data$Label)) + 
      ggplot2::geom_bar(data = popData[popData$Label == "Outcome",], stat = "identity") + 
      ggplot2::geom_bar(data = popData[popData$Label == "No Outcome",], stat = "identity") + 
      ggplot2::geom_bar(stat = "identity") + 
      ggplot2::scale_x_continuous(labels = abs) + 
      #ggplot2::scale_fill_brewer(palette = "Set1") + 
      ggplot2::coord_flip( ) +
      ggplot2::theme_bw() + 
      ggplot2::theme(axis.title.x=ggplot2::element_blank(),
                     axis.text.x=ggplot2::element_blank(),
                     axis.ticks.x=ggplot2::element_blank())
    
    
  }
  
  plots[[i]] <- gridExtra::grid.arrange(smooth_plot,
                                  hist_plot,
                                  ncol = 1,
                                  heights=c(2,1))
  
  }
  
  if (!is.null(saveLocation)){
    if(!dir.exists(saveLocation)){
      dir.create(saveLocation, recursive = T)
    }
    ggplot2::ggsave(file.path(saveLocation, fileName), plot, width = 5, height = 4.5, dpi = 400)
  }
  return(plot)
}


#=============
# PREDICTIONSUMMARY PLOTS 
#=============

#' Plot the side-by-side boxplots of prediction distribution, by class#'
#' @details
#' Create a plot showing the side-by-side boxplots of prediction distribution, by class
#' #'
#' @param plpResult            A plp result object as generated using the \code{\link{runPlp}} function.
#' @param typeColumn            The name of the column specifying the evaluation type
#' @param saveLocation          Directory to save plot (if NULL plot is not saved)
#' @param fileName              Name of the file to save to plot, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotPredictionDistribution <- function(
  plpResult,
  typeColumn = 'evaluation',
  saveLocation = NULL,
  fileName = 'PredictionDistribution.png'
){
  
  evalTypes <- unique(plpResult$performanceEvaluation$predictionDistribution[,typeColumn])
  
  plots <- list()
  length(plots) <- length(evalTypes)
  
  for(i in 1:length(evalTypes)){
    evalType <- evalTypes[i]
    x <- plpResult$performanceEvaluation$predictionDistribution %>% 
      dplyr::filter(.data[[typeColumn]] == evalType) 
  
  non05 <- x$P05PredictedProbability[x$class==0]
  non95 <- x$P95PredictedProbability[x$class==0]
  one05 <- x$P05PredictedProbability[x$class==1]
  one95 <- x$P95PredictedProbability[x$class==1]
  
  plots[[i]] <-   ggplot2::ggplot(x, ggplot2::aes(x=as.factor(.data$class),
                                            ymin=.data$MinPredictedProbability,
                                            lower=.data$P25PredictedProbability,
                                            middle=.data$MedianPredictedProbability,
                                            upper=.data$P75PredictedProbability, 
                                            ymax=.data$MaxPredictedProbability, 
                                            color=as.factor(.data$class))) + 
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
                                       xend = 2.1, yend = one95)) +
    ggplot2::ggtitle(evalType)
  }
  
  plot <- gridExtra::marrangeGrob(plots, nrow=length(plots), ncol=1)
  
  if (!is.null(saveLocation)){
    if(!dir.exists(saveLocation)){
      dir.create(saveLocation, recursive = T)
    }
    ggplot2::ggsave(file.path(saveLocation, fileName), plot, width = 5, height = 4.5, dpi = 400)
  }
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
#' @param saveLocation          Directory to save plot (if NULL plot is not saved)
#' @param fileName              Name of the file to save to plot, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotVariableScatterplot <- function(
  covariateSummary, 
  saveLocation = NULL,
  fileName = 'VariableScatterplot.png'
  )
  {
  
  # remove the non-incidence variables from the plot
  covariateSummary <- covariateSummary[covariateSummary$WithNoOutcome_CovariateMean <= 1,]
  
  covariateSummary$size <- rep(0.1, nrow(covariateSummary))
  covariateSummary$size[covariateSummary$covariateValue!=0] <- 0.5
 
  plot <- ggplot2::ggplot(covariateSummary, ggplot2::aes(y=.data$WithOutcome_CovariateMean, 
                                                         x=.data$WithNoOutcome_CovariateMean,
                                                         #size=abs(covariateValue)+0.1
                                                         size=.data$size
                                                         )) +
          ggplot2::geom_point(ggplot2::aes(color = .data$size)) +
          ggplot2::scale_size(range = c(0, 1)) +
          ggplot2::scale_colour_gradient2(low = "red",mid = "blue", high="green") +
    ggplot2::scale_y_continuous("Outcome Covariate Mean") +
    ggplot2::scale_x_continuous("Non-outcome Covariate Mean") + 
    ggplot2::geom_abline(intercept = 0, slope = 1,linetype = 2) +
    ggplot2::theme(legend.position="none")
  
  if (!is.null(saveLocation)){
    suppressWarnings(ggplot2::ggsave(file.path(saveLocation, fileName), plot, width = 5, height = 3.5, dpi = 400))
  }
  return(plot)
}


#' Plot the train/test generalizability diagnostic
#'
#' @details
#' Create a plot showing the train/test generalizability diagnostic
#' #'
#' @param covariateSummary      A prediction object as generated using the
#'                              \code{\link{runPlp}} function.
#' @param saveLocation          Directory to save plot (if NULL plot is not saved)
#' @param fileName              Name of the file to save to plot, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#'
#' @export
plotGeneralizability <- function(
  covariateSummary, 
  saveLocation = NULL,
  fileName = 'Generalizability.png'){
  
  covariateSummary$TrainWithOutcome_CovariateMean[is.na(covariateSummary$TrainWithOutcome_CovariateMean)] <- 0
  covariateSummary$TestWithOutcome_CovariateMean[is.na(covariateSummary$TestWithOutcome_CovariateMean)] <- 0
  
  covariateSummary <- covariateSummary[covariateSummary$TrainWithOutcome_CovariateMean <= 1,]
  
  plot1 <- ggplot2::ggplot(covariateSummary, 
                          ggplot2::aes(.data$TrainWithOutcome_CovariateMean, 
                                       .data$TestWithOutcome_CovariateMean)) +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous("Train set Mean") +
    ggplot2::scale_y_continuous("Test Set Mean") + 
    ggplot2::theme(legend.position="none") +
    ggplot2::geom_abline(intercept = 0, slope = 1,linetype = 2)+
    ggplot2::ggtitle("Outcome")
    
 
  covariateSummary$TrainWithNoOutcome_CovariateMean[is.na(covariateSummary$TrainWithNoOutcome_CovariateMean)] <- 0
  covariateSummary$TestWithNoOutcome_CovariateMean[is.na(covariateSummary$TestWithNoOutcome_CovariateMean)] <- 0
  
  plot2 <- ggplot2::ggplot(covariateSummary, 
                          ggplot2::aes(.data$TrainWithNoOutcome_CovariateMean, 
                                       .data$TestWithNoOutcome_CovariateMean)) +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous("Train set Mean") +
    ggplot2::scale_y_continuous("Test Set Mean") + 
    ggplot2::theme(legend.position="none") +
    ggplot2::geom_abline(intercept = 0, slope = 1,linetype = 2) +
    ggplot2::ggtitle("No Outcome")
  
  plot <- gridExtra::grid.arrange(plot1, plot2, ncol=2)
  
  if (!is.null(saveLocation))
    ggplot2::ggsave(file.path(saveLocation, fileName), plot, width = 5, height = 3.5, dpi = 400)
  return(plot)
}


