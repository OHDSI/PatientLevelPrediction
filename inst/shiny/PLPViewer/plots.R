#============  DYNAMIC PLOTS ======================
#++++++++++++++++++++++++++++++++++++++++++++++++++

plotShiny <- function(eval, pointOfInterest){
  
  data <- eval$thresholdSummary[eval$thresholdSummary$Eval%in%c('test','validation'),]
  # pointOfInterest # this is a threshold
  pointOfInterest <- data[pointOfInterest,]
  rocobject <- plotly::plot_ly(x = 1-c(0,data$specificity,1)) %>%
    plotly::add_lines(y = c(1,data$sensitivity,0),name = "hv", 
                      text = paste('Risk Threshold:',c(0,data$predictionThreshold,1)),
                      line = list(shape = "hv",
                                  color = 'rgb(22, 96, 167)'),
                      fill = 'tozeroy') %>%
    plotly::add_trace(x= c(0,1), y = c(0,1),mode = 'lines',
                      line = list(dash = "dash"), color = I('black'),
                      type='scatter') %>%
    plotly::add_trace(x= 1-pointOfInterest$specificity, y=pointOfInterest$sensitivity, 
                      mode = 'markers', symbols='x') %>%  # change the colour of this!
    plotly::add_lines(x=c(1-pointOfInterest$specificity, 1-pointOfInterest$specificity),
                      y = c(0,1),
                      line = list(dash ='solid',
                                  color = 'black')) %>%
    plotly::layout(title = "ROC Plot",
                   xaxis = list(title = "1-specificity"),
                   yaxis = list (title = "Sensitivity"),
                   showlegend = FALSE)
  
  popAv <- data$trueCount[1]/(data$trueCount[1] + data$falseCount[1])
  probject <- plotly::plot_ly(x = data$sensitivity) %>%
    plotly::add_lines(y = data$positivePredictiveValue, name = "hv", 
                      text = paste('Risk Threshold:',data$predictionThreshold),
                      line = list(shape = "hv",
                                  color = 'rgb(22, 96, 167)'),
                      fill = 'tozeroy') %>%
    plotly::add_trace(x= c(0,1), y = c(popAv,popAv),mode = 'lines',
                      line = list(dash = "dash"), color = I('red'),
                      type='scatter') %>%
    plotly::add_trace(x= pointOfInterest$sensitivity, y=pointOfInterest$positivePredictiveValue, 
                      mode = 'markers', symbols='x') %>%  
    plotly::add_lines(x=c(pointOfInterest$sensitivity, pointOfInterest$sensitivity),
                      y = c(0,1),
                      line = list(dash ='solid',
                                  color = 'black')) %>%
    plotly::layout(title = "PR Plot",
                   xaxis = list(title = "Recall"),
                   yaxis = list (title = "Precision"),
                   showlegend = FALSE)
  
  # add F1 score
  f1object <- plotly::plot_ly(x = data$predictionThreshold) %>%
    plotly::add_lines(y = data$f1Score, name = "hv", 
                      text = paste('Risk Threshold:',data$predictionThreshold),
                      line = list(shape = "hv",
                                  color = 'rgb(22, 96, 167)'),
                      fill = 'tozeroy') %>%
    plotly::add_trace(x= pointOfInterest$predictionThreshold, y=pointOfInterest$f1Score, 
                      mode = 'markers', symbols='x') %>%  
    plotly::add_lines(x=c(pointOfInterest$predictionThreshold, pointOfInterest$predictionThreshold),
                      y = c(0,1),
                      line = list(dash ='solid',
                                  color = 'black')) %>%
    plotly::layout(title = "F1-Score Plot",
                   xaxis = list(title = "Prediction Threshold"),
                   yaxis = list (title = "F1-Score"),
                   showlegend = FALSE)
  # create 2x2 table with TP, FP, TN, FN and threshold
  threshold <- pointOfInterest$predictionThreshold
  TP <- pointOfInterest$truePositiveCount
  TN <- pointOfInterest$trueNegativeCount
  FP <- pointOfInterest$falsePositiveCount
  FN <- pointOfInterest$falseNegativeCount
  preferenceThreshold <- pointOfInterest$preferenceThreshold
  
  return(list(roc = rocobject,
              pr = probject,
              f1score = f1object,
              threshold = threshold, prefthreshold=preferenceThreshold,
              TP = TP, TN=TN,
              FP= FP, FN=FN))
}

plotCovariateSummary <- function(covariateSummary){
  
  #writeLines(paste(colnames(covariateSummary)))
  #writeLines(paste(covariateSummary[1,]))
  # remove na values 
  covariateSummary$CovariateMeanWithNoOutcome[is.na(covariateSummary$CovariateMeanWithNoOutcome)] <- 0
  covariateSummary$CovariateMeanWithOutcome[is.na(covariateSummary$CovariateMeanWithOutcome)] <- 0
  if(!'covariateValue'%in%colnames(covariateSummary)){
    covariateSummary$covariateValue <- 1
  }
  if(sum(is.na(covariateSummary$covariateValue))>0){
    covariateSummary$covariateValue[is.na(covariateSummary$covariateValue)] <- 0
  }
  
  # SPEED EDIT remove the none model variables
  covariateSummary <- covariateSummary[covariateSummary$covariateValue!=0,]
  
  # save dots based on coef value 
  covariateSummary$size <- abs(covariateSummary$covariateValue)
  covariateSummary$size[is.na(covariateSummary$size)] <- 4
  covariateSummary$size <- 4+4*covariateSummary$size/max(covariateSummary$size)
  
  # color based on analysis id
  covariateSummary$color <- sapply(covariateSummary$covariateName, function(x) ifelse(is.na(x), '', strsplit(as.character(x), ' ')[[1]][1]))
  
  l <- list(x = 0.01, y = 1,
            font = list(
              family = "sans-serif",
              size = 10,
              color = "#000"),
            bgcolor = "#E2E2E2",
            bordercolor = "#FFFFFF",
            borderwidth = 1)
  
  #covariateSummary$annotation <- sapply(covariateSummary$covariateName, getName)
  covariateSummary$annotation <- covariateSummary$covariateName
  
  
  ind <- covariateSummary$CovariateMeanWithNoOutcome <=1 & covariateSummary$CovariateMeanWithOutcome <= 1
  # create two plots -1 or less or g1
  binary <- plotly::plot_ly(x = covariateSummary$CovariateMeanWithNoOutcome[ind],
                            #size = covariateSummary$size[ind],
                            showlegend = F) %>%
    plotly::add_markers(y = covariateSummary$CovariateMeanWithOutcome[ind],
                        color=factor(covariateSummary$color[ind]),
                        text = paste(covariateSummary$annotation[ind]),
                        showlegend = T
    ) %>%
    plotly::add_trace(x= c(0,1), y = c(0,1),mode = 'lines',
                      line = list(dash = "dash"), color = I('black'),
                      type='scatter', showlegend = FALSE) %>%
    plotly::layout(#title = 'Prevalance of baseline predictors in persons with and without outcome',
      xaxis = list(title = "Prevalance in persons without outcome",
                   range = c(0, 1)),
      yaxis = list(title = "Prevalance in persons with outcome",
                   range = c(0, 1)),
      legend = l, showlegend = T)
  
  if(sum(!ind)>0){
    maxValue <- max(c(covariateSummary$CovariateMeanWithNoOutcome[!ind],
                      covariateSummary$CovariateMeanWithOutcome[!ind]), na.rm = T)
    meas <- plotly::plot_ly(x = covariateSummary$CovariateMeanWithNoOutcome[!ind] ) %>%
      plotly::add_markers(y = covariateSummary$CovariateMeanWithOutcome[!ind],
                          text = paste(covariateSummary$annotation[!ind])) %>%
      plotly::add_trace(x= c(0,maxValue), y = c(0,maxValue),mode = 'lines',
                        line = list(dash = "dash"), color = I('black'),
                        type='scatter', showlegend = FALSE) %>%
      plotly::layout(#title = 'Prevalance of baseline predictors in persons with and without outcome',
        xaxis = list(title = "Mean in persons without outcome"),
        yaxis = list(title = "Mean in persons with outcome"),
        showlegend = FALSE)
  } else {
    meas <- NULL
  }
  
  return(list(binary=binary,
              meas = meas))
}






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




plotDemographicSummary <- function(evaluation, type='test', fileName=NULL){
  if (!all(is.na(evaluation$demographicSummary$averagePredictedProbability))){
    ind <- evaluation$demographicSummary$Eval==type
    x<- evaluation$demographicSummary[ind,colnames(evaluation$demographicSummary)%in%c('ageGroup','genGroup','averagePredictedProbability',
                                                                                       'PersonCountAtRisk', 'PersonCountWithOutcome')]
    
    # remove -1 values:
    x <- x[x$PersonCountWithOutcome != -1,]
    if(nrow(x)==0){
      return(NULL)
    }
    # end remove -1 values
    
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