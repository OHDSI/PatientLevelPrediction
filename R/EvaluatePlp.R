# @file Evaluate.R
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

#' evaluatePlp
#'
#' @description
#' Evaluates the performance of the patient level prediction model
#' @details
#' The function calculates various metrics to measure the performance of the model
#' @param prediction                         The patient level prediction model's prediction
#' @return
#' A list containing the performance values
#'

#' @export
evaluatePlp <- function(prediction, silent=F){
  if(!silent)
    writeLines('Checking Inputs')
  type <- attr(prediction, "metaData")$predictionType
  if (type != "binary")
    stop("Currently only support binary classification models")
  if(is.null(prediction$outcomeCount)) stop('No outcomeCount column present')
  if(length(unique(prediction$value))==1) stop('Cannot evaluate as predictions all the same value')

  #============= AUC ======================
  if(!silent)
    writeLines(paste0('Calculating auc started at: ', Sys.time()))
  if(nrow(prediction) < 100000){
    auc <- computeAuc(prediction, confidenceInterval = T)
  } else{
    # speed issues with big data so using AUC package
    auc <- AUC:::auc(AUC:::roc(prediction$value, factor(prediction$outcomeCount)))
  }
  if(!silent)
    writeLines(paste0('Calculating auc ended at: ', Sys.time()))
  #=========================================
  
  # calibration linear fit- returns gradient, intercept
  #=====================================================
  if(!silent)
    writeLines(paste0('Calculating calibration line started at: ', Sys.time()))
  calLine10 <- calibrationLine(prediction, numberOfStrata = 10)
  calLine100 <- calibrationLine(prediction, numberOfStrata = 100)
  if(!silent)
    writeLines(paste0('Calculating calibration line ended at: ', Sys.time()))
  #=====================================================
  
  if(!silent)
    writeLines(paste0('Calculating calibration plot started at: ', Sys.time()))
  calPlot <- plotCalibration(prediction,
                             numberOfStrata = 10,
                             truncateFraction = 0.01,
                             fileName = NULL)
  if(!silent)
    writeLines(paste0('Calculating calibration plot ended at: ', Sys.time()))
  
  #=====================================================
  # brier scores-returnss; brier, brierScaled
  if(!silent)
    writeLines(paste0('Calculating brier score started at: ', Sys.time()))
  brier <- brierScore(prediction)
  if(!silent)
    writeLines(paste0('Calculating brier score ended at: ', Sys.time()))
  #=====================================================
  
  
  #=====================================================
  # boxplot and quantiles:
  if(!silent)
    writeLines(paste0('Calculating quantiles started at: ', Sys.time()))
  quantiles <- quantiles(prediction)  
  if(!silent)
    writeLines(paste0('Calculating quantiles ended at: ', Sys.time()))
  #=====================================================
  
  #=====================================================
  # preference score sparse:
  if(!silent)
    writeLines(paste0('Calculating preference score at: ', Sys.time()))
  prefScore <- computePreferenceScore(prediction) 
  if(!silent)
    writeLines(paste0('Calculating preference ended at: ', Sys.time()))
  #=====================================================
  
  
  #=====================================================
  # now calculate 100 point tpr, fpr
  if(!silent)
    writeLines(paste0('Calculating sparse ROC started at: ', Sys.time()))
  roc.sparse <-rocSparse(prediction)
  if(!silent)
    writeLines(paste0('Calculating ROC sparse ended at: ', Sys.time()))
  #=====================================================
  
  
  
  #=====================================================
  # Average Precision
  aveP.val <- averagePrecision(prediction)
  #=====================================================
  
  result <- list(auc=auc,aveP=aveP.val, brier=brier$brier, brierScaled=brier$brierScaled,
                 calibrationIntercept10=calLine10$lm[1], calibrationGradient10 = calLine10$lm[2],
                 calibrationIntercept100=calLine100$lm[1], calibrationGradient100 = calLine100$lm[2],
                 hosmerlemeshow = calLine10$hosmerlemeshow,
                 roc = roc.sparse[,c('FPR','TPR')],
                 raw = roc.sparse[,c('TP','FP','TN','FN','FOR','accuracy')],
                 precision.recall = roc.sparse[,c('TPR','PPV')],
                 F.measure = roc.sparse[,c('Fmeasure')],
                 preferenceScores = prefScore$sparsePrefScore,
                 calSparse =calPlot$strataData,
                 calSparse2_10 = calLine10$aggregateLmData,
                 calSparse2_100 = calLine100$aggregateLmData,
                 quantiles = quantiles$quantiles,
                 calPlot =calPlot$plot, prefScorePlot = prefScore$plot, # remove plots?
                 boxPlot = quantiles$plot,
                 preference3070_0 = prefScore$similar_0,
                 preference3070_1 = prefScore$similar_1
  )
  class(result) <- 'metric.sparse'
  return(result)
  
}

#' Compute the area under the ROC curve
#'
#' @details
#' Computes the area under the ROC curve for the predicted probabilities, given the true observed
#' outcomes.
#'
#' @param prediction            A prediction object as generated using the
#'                              \code{\link{predict}} functions.
#' @param confidenceInterval    Should 95 percebt confidence intervals be computed?
#'
#' @export
computeAuc <- function(prediction,
                       confidenceInterval = FALSE) {
  if (attr(prediction, "metaData")$predictionType != "binary")
    stop("Computing AUC is only implemented for binary classification models")

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
#'                              \code{\link{predict}} functions.
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
                            numberOfStrata = 10,
                            truncateFraction = 0.01,
                            fileName = NULL) {
  if (attr(prediction, "metaData")$predictionType != "binary")
    stop("Plotting the calibration is only implemented for binary classification models")

  q <- unique(quantile(prediction$value, (1:(numberOfStrata - 1))/numberOfStrata))
  prediction$strata <- cut(prediction$value,
                           breaks = unique(c(0, q, max(prediction$value))),
                           labels = FALSE)
  computeStratumStats <- function(data) {
    return(data.frame(minx = min(data$value),
                      maxx = max(data$value),
                      fraction = sum(data$outcomeCount)/nrow(data)))
  }
  # strataData <- plyr::ddply(prediction, prediction$strata, computeStratumStats)
  counts <- aggregate(outcomeCount ~ strata, data = prediction, sum)
  names(counts)[2] <- "counts"
  backgroundCounts <- aggregate(rowId ~ strata, data = prediction, length)
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
  return(list(plot=plot, strataData=strataData))
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


averagePrecision <- function(prediction){
  lab.order <- prediction$outcomeCount[order(-prediction$value)]
  n <- nrow(prediction)
  P <- sum(prediction$outcomeCount>0)
  val <- rep(0, n)
  val[lab.order>0] <- 1:P
  return(sum(val/(1:n))/P)
}

rocSparse <- function(prediction){
  if(nrow(prediction)<99){
    warning('sparse roc not calculated due to small dataset')
    return(NULL)
  }
    
  lab.order <- prediction$outcomeCount[order(-prediction$value)]
  n <- nrow(prediction)
  P <- sum(prediction$outcomeCount>0)
  N <- n - P
  # find points where there is a change (0 to 1)
  change.ind <-which(abs(lab.order[-length(lab.order)]-lab.order[-1])==1)
  
  if(length(change.ind)<=101){
    ind.oi <- change.ind
  } else {
  #change.ind <- which(lab.order==0)
    ind.oi <- change.ind[c(seq(1,length(change.ind)-1,
                               floor((length(change.ind)-1)/99)), length(change.ind))]
  }
  # improve speed create vector with cum sum 
  temp.cumsum <- rep(0, length(lab.order))
  temp.cumsum[lab.order==0] <- 1:sum(lab.order==0)
  TP <- sapply(ind.oi, function(x) x-temp.cumsum[x])
  FP <- sapply(ind.oi, function(x) temp.cumsum[x])
  
  TN <- N-FP
  FN <- P-TP
  
  TPR <- TP/P
  FPR <- FP/N
  accuracy <- (TP+TN)/n
  PPV<- TP/(TP+FP)
  FOR <- FN/(FN+TN)
  Fmeasure <- 2*(PPV*TPR)/(PPV+TPR)
  
  return(data.frame(TP,FP,TN,FN, TPR, FPR,PPV,FOR, accuracy, Fmeasure))
  
}


brierScore <- function(prediction, ...){

  brier <- sum((prediction$outcomeCount -prediction$value)^2)/nrow(prediction)
  brierMax <- mean(prediction$value)*(1-mean(prediction$value))
  brierScaled <- 1-brier/brierMax
  return(list(brier=brier,brierScaled=brierScaled))
}

# linear regression a+b values
calibrationLine <- function(prediction,numberOfStrata=10, ...){
  outPpl <- unique(prediction$rowId)

  q <- unique(quantile(prediction$value, c((1:(numberOfStrata - 1))/numberOfStrata, 1)))
  
  if(length(unique(c(0,q)))==2){
    warning('Prediction not spread')
    #res <- c(0,0)
    #lmData <- NULL
    #hosmerlemeshow <-  c(0,0,0)
    prediction$strata <- cut(prediction$value,
                             breaks = c(0,0.5,1), #,max(prediction$value)),
                             labels = FALSE)
  } else {
    prediction$strata <- cut(prediction$value,
                             breaks = unique(c(0,q)), #,max(prediction$value)),
                             labels = FALSE)
  }
    
    # get observed events:
    obs.Points <- aggregate(prediction$outcomeCount, by=list(prediction$strata), FUN=mean)
    colnames(obs.Points) <- c('group','obs')
    pred.Points <- aggregate(prediction$value, by=list(prediction$strata), FUN=mean)
    colnames(pred.Points) <- c('group','pred')
    
    # hosmer-lemeshow-goodness-of-fit-test
    obs.count <- aggregate(prediction$outcomeCount, by=list(prediction$strata), FUN=sum)
    colnames(obs.count) <- c('group','observed')
    expected.count <- aggregate(prediction$value, by=list(prediction$strata), FUN=sum)
    colnames(expected.count) <- c('group','expected')
    hoslem <- merge(obs.count, expected.count, by='group')
    obs.count2 <- aggregate(1-prediction$outcomeCount, by=list(prediction$strata), FUN=sum)
    colnames(obs.count2) <- c('group','observed')
    expected.count2 <- aggregate(1-prediction$value, by=list(prediction$strata), FUN=sum)
    colnames(expected.count2) <- c('group','expected')
    nhoslem <- merge(obs.count2, expected.count2, by='group')
    Xsquared <- sum((hoslem$observed-hoslem$expected)^2/hoslem$expected) +
      sum((nhoslem$observed-nhoslem$expected)^2/nhoslem$expected)
    pvalue <- pchisq(Xsquared, df=numberOfStrata-2, lower.tail = F)
    hosmerlemeshow <- data.frame(Xsquared=Xsquared, df=numberOfStrata-2, pvalue=pvalue)
    
    # linear model fitting obs to pred:
    lmData <- merge(obs.Points, pred.Points, by='group')
    model <- lm(obs ~pred, data=lmData)
    
    plot(lmData$pred, lmData$obs)
    abline(a = model$coefficients[1], b = model$coefficients[2], col='red')
    res <- model$coefficients
    names(res) <- c('Intercept','Gradient')
  #
  
  result <- list(lm=res,
                 aggregateLmData = lmData,
                 hosmerlemeshow = hosmerlemeshow)
  return(result)
}



quantiles <- function(prediction){
  boxPlot <- ggplot2::ggplot(prediction, ggplot2::aes(x=as.factor(outcomeCount), y=value, 
                                                      fill=as.factor(outcomeCount))) + ggplot2::geom_boxplot() +
    ggplot2::guides(fill=FALSE) + ggplot2::xlab("Class") + ggplot2::ylab("Prediction") 
  quantiles <- aggregate(prediction$value, list(prediction$outcomeCount), 
                         function(x) quantile(x, probs = c(0.00,0.1, 0.25, 0.5, 0.75,0.9, 1.00))
  )
  return(list(plot=boxPlot,
              quantiles=quantiles))                      
}


computePreferenceScore <- function(prediction) {
  proportion <- sum(prediction$outcomeCount)/nrow(prediction) 
  x <- exp(log(prediction$value/(1 - prediction$value)) - log(proportion/(1 - proportion)))
  prediction$preferenceScore <- x/(x + 1)
  
  df1 <-  transform(prediction[,c('rowId','outcomeCount','preferenceScore')], group=cut(prediction$preferenceScore, 
                                                                                        breaks=seq(-0.005,1.005,0.01),
                                                                                        labels=NULL))
  
  
  res <- do.call(data.frame,aggregate(preferenceScore~group+outcomeCount, df1, 
                                      FUN=function(x) c(density=length(x))))
  
  res <- merge(res, data.frame(outcomeCount=c(0,1), N=c(sum(prediction$outcomeCount==0),
                                                        sum(prediction$outcomeCount==1))))
  res$density <- res$preferenceScore/res$N
  
  res$groupVal <- unlist(lapply(gsub(']','', gsub('\\(', '',res$group)), 
                                function(x) mean(as.double(strsplit(x, ',')[[1]]))))
  
  plot <- ggplot2::ggplot(res, ggplot2::aes(x=groupVal, y=density, 
                                            group=as.factor(outcomeCount), col=as.factor(outcomeCount),
                                            fill=as.factor(outcomeCount))) +
    ggplot2::geom_line() + ggplot2::xlab("Preference") + ggplot2::ylab("Density") +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::geom_vline(xintercept = 0.3) + ggplot2::geom_vline(xintercept = 0.7)
  
  # get the density between 0.3-0.7 for outcome and non-outcome
  prediction$indif <- prediction$preferenceScore <= 0.7 & prediction$preferenceScore >= 0.3
  count <- aggregate(prediction$indif, list(prediction$outcomeCount), sum)
  colnames(count) <- c('Outcome','total0307')
  countN <- aggregate(prediction$indif, list(prediction$outcomeCount),length)
  colnames(countN) <- c('Outcome','total')
  
  similar3070 <- merge(count, countN)
  similar3070$density <- similar3070$total0307/similar3070$total
  
  result <- list(sparsePrefScore = res,
                 plot = plot,
                 similar_1=similar3070$density[2],
                 similar_0=similar3070$density[1]
  )
  
  return(result)
}