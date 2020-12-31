# @file Evaluate.R
#
# Copyright 2020 Observational Health Data Sciences and Informatics
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
#' @param plpData                            The patient level prediction data
#' @return
#' A list containing the performance values
#'

#' @export
evaluatePlp <- function(prediction, plpData){

  # check logger
  if(length(ParallelLogger::getLoggers())==0){
    logger <- ParallelLogger::createLogger(name = "SIMPLE",
                                        threshold = "INFO",
                                        appenders = list(ParallelLogger::createConsoleAppender(layout = ParallelLogger::layoutTimestamp)))
    ParallelLogger::registerLogger(logger)
  }

  # checking inputs
  #========================================
  type <- attr(prediction, "metaData")$predictionType
  if (!type %in% c("binary","survival")) {
    stop('Currently only support binary or survival classification models')
  }
  
  if(is.null(prediction$outcomeCount)){
    stop('No outcomeCount column present')
  }
  if(length(unique(prediction$value))==1){
    stop('Cannot evaluate as predictions all the same value')
  }
  
  if(type == "binary"){
    
    # auc
    ParallelLogger::logTrace('Calculating AUC')
    if(sum(prediction$outcomeCount>0) < 1000){
      auc <- computeAuc(prediction, confidenceInterval = T)
      ParallelLogger::logInfo(sprintf('%-20s%.2f', 'AUC: ', auc[1]*100))
      ParallelLogger::logInfo(sprintf('%-20s%.2f', '95% lower AUC: ', auc[2]*100))
      ParallelLogger::logInfo(sprintf('%-20s%.2f', '95% upper AUC: ', auc[3]*100))
    } else{
      # speed issues with big data so using AUC package
      auc <- data.frame(auc = AUC::auc(AUC::roc(prediction$value, factor(prediction$outcomeCount))),
                        auc_lb95ci = NA,
                        auc_ub95ci = NA)
      ParallelLogger::logInfo(sprintf('%-20s%.2f', 'AUC: ', auc[1]*100))
    }
    
    # auprc
    ParallelLogger::logTrace('Calculating AUPRC')
    positive <- prediction$value[prediction$outcomeCount == 1]
    negative <- prediction$value[prediction$outcomeCount == 0]
    pr <- PRROC::pr.curve(scores.class0 = positive, scores.class1 = negative)
    auprc <- pr$auc.integral
    ParallelLogger::logInfo(sprintf('%-20s%.2f', 'AUPRC: ', auprc*100))
    
    # brier scores-returnss; brier, brierScaled
    ParallelLogger::logTrace('Calculating Brier Score')
    brier <- brierScore(prediction)
    ParallelLogger::logInfo(sprintf('%-20s%.2f', 'Brier: ', brier$brier))
    
    # using rms::val.prob
    valProb <- tryCatch(rms::val.prob(prediction$value, prediction$outcomeCount), 
                        error = function(e){ParallelLogger::logInfo(e); return(list(Eavg = 0, 
                                                                                    E90 = 0, 
                                                                                    Emax = 0))})
    
    
    # 2) thresholdSummary
    # need to update thresholdSummary this with all the requested values
    ParallelLogger::logTrace(paste0('Calulating Threshold summary Started @ ',Sys.time()))
    thresholdSummary <- getThresholdSummary(prediction) # rename and edit this
    
    ParallelLogger::logTrace(paste0('Completed @ ',Sys.time()))
    
    # 3) demographicSummary
    ParallelLogger::logTrace(paste0('Calulating Demographic Based Evaluation Started @ ',Sys.time()))
    demographicSummary <- tryCatch(getDemographicSummary(prediction, plpData),
                                   error= function(cond){return(NULL)})
    ParallelLogger::logTrace(paste0('Completed @ ',Sys.time()))
    
    
    # calibration linear fit- returns gradient, intercept
    ParallelLogger::logTrace('Calculating Calibration-in-large')
    calinlarge <- calibrationInLarge(prediction)
    ParallelLogger::logInfo(paste0('Calibration in large- Mean predicted risk ', round(calinlarge$meanPredictionRisk, digits = 4), ' : observed risk ',round(calinlarge$observedRisk, digits = 4)))
    
    ParallelLogger::logTrace('Calculating Weak Calibration')
    weakCal <- calibrationWeak(prediction)
    ParallelLogger::logInfo(paste0('Weak calibration intercept: ', 
                                   round(weakCal$intercept, digits = 4), 
                                   ' - gradient:',round(weakCal$gradient, digits = 4)))
    
    ParallelLogger::logTrace('Calculating Hosmer-Lemeshow Calibration Line')
    calLine10 <- calibrationLine(prediction, numberOfStrata = 10)
    ParallelLogger::logInfo(sprintf('%-20s%.2f%-20s%.2f', 'Hosmer-Lemeshow calibration gradient: ', calLine10$lm[2], ' intercept: ',calLine10$lm[1]))
    # 4) calibrationSummary
    ParallelLogger::logTrace(paste0('Calculating Calibration Summary Started @ ',Sys.time()))
    calibrationSummary <- getCalibration(prediction,
                                         numberOfStrata = 100,
                                         truncateFraction = 0.01)
    ParallelLogger::logTrace(paste0('Completed @ ',Sys.time()))
    
    
    # 5) predictionDistribution - done
    ParallelLogger::logTrace(paste0('Calculating Quantiles Started @ ',Sys.time()))
    predictionDistribution <- getPredictionDistribution(prediction)
    ParallelLogger::logTrace(paste0('Completed @ ',Sys.time()))
    
    # Extra: Average Precision
    aveP.val <- averagePrecision(prediction)
    ParallelLogger::logInfo(sprintf('%-20s%.2f', 'Average Precision: ', aveP.val))
    
    # evaluationStatistics:
    evaluationStatistics <- list(analysisId= attr(prediction, "metaData")$analysisId,
                                 populationSize = nrow(prediction),
                                 outcomeCount = sum(prediction$outcomeCount),
                                 # need to add analysisId to metaData!
                                 AUC= auc,
                                 AUPRC = auprc,
                                 BrierScore = brier$brier,	
                                 BrierScaled= brier$brierScaled,	
                                 CalibrationIntercept= weakCal$intercept,	
                                 CalibrationSlope = weakCal$gradient,
                                 CalibrationInLarge = calinlarge$meanPredictionRisk/calinlarge$observedRisk,
                                 Emean = valProb['Eavg'],
                                 E90 = valProb['E90'],
                                 Emax = valProb['Emax'])
    
    result <- list(evaluationStatistics= evaluationStatistics,
                   thresholdSummary= thresholdSummary,
                   demographicSummary = demographicSummary,
                   calibrationSummary = calibrationSummary,
                   predictionDistribution = predictionDistribution
    )
  }
  
  
  if(type == "survival"){
    
    if(is.null(prediction$survivalTime)){
      stop('No survival time column present')
    }

    #============================
    
    timepoint <- attr(prediction, 'metaData')$timepoint #max(prediction$survivalTime)
    ParallelLogger::logInfo(paste0('Evaluating survival model at time: ', timepoint, ' days'))
    
    #t <- apply(cbind(prediction$daysToCohortEnd, prediction$survivalTime), 1, min)
    t <- prediction$survivalTime
    y <- ifelse(prediction$outcomeCount > 0, 1, 0)

    S<- survival::Surv(t, y) 
    p <- prediction$value
    
    
    out <- tryCatch({summary(survival::survfit(survival::Surv(t, y) ~ 1), times = timepoint)},
                    error = function(e){ParallelLogger::logError(e); return(NULL)})
    survVal <- 1-out$surv
    meanSurvivalTime <- mean(t)
    
    # add c-stat
    ParallelLogger::logTrace('Calculating C-statistic')
  
    conc <- tryCatch({survival::concordance(S~p, reverse=TRUE)},
                     error = function(e){ParallelLogger::logError(e); return(NULL)})
    cStatistic <- 0
    cStatistic_l95CI <- 0
    cStatistic_u95CI <- 0
    
    if(!is.null(conc)){
      cStatistic <- round(conc$concordance,5)
      c.se<-sqrt(conc$var)
      cStatistic_l95CI <- round(conc$concordance+stats::qnorm(.025)*c.se,3)
      cStatistic_u95CI <- round(conc$concordance+stats::qnorm(.975)*c.se,3)
    }
    ParallelLogger::logInfo(paste0('C-statistic: ',cStatistic, ' (',cStatistic_l95CI ,'-', cStatistic_u95CI ,')'))


    # add e-stat
    w<- tryCatch({rms::val.surv(est.surv=1-p,S=S,
                                u=timepoint, 
                                fun=function(pr)log(-log(pr)))},
                 error = function(e){ParallelLogger::logError(e); return(NULL)})
    
    eStatistic <- -1
    eStatistic90 <- -1
    if(!is.null(w)){
      eStatistic<-mean(abs(w$actual - w$p))
      eStatistic90<-stats::quantile((abs(w$actual - w$p)),0.9)
      
    }
    ParallelLogger::logInfo(paste0('E-statistic: ',eStatistic))
    ParallelLogger::logInfo(paste0('E-statistic 90%: ',eStatistic90))

    
    # add netbenefit
    preddat <- data.frame(p = p/max(p), t=t, y=y)
    results = tryCatch({stdca(data=preddat, outcome="y", ttoutcome="t", timepoint=timepoint,  
                              predictors="p", xstart = 0.001, xstop = min(max(p),0.99), xby = 0.001, smooth=F)},
                       error = function(e){ParallelLogger::logError(e); return(NULL)})
    nbSummary <- NULL
    if(!is.null(results)){
      nbSummary <- results$net.benefit
    }
    
    # add calibration
    # add in calibration for  survival 
    S<- survival::Surv(t, y) 
    groups<-Hmisc::cut2(prediction$value,g=100)
    n.groups<-length(levels(groups))
    pred<-tapply(prediction$value,groups,mean)
    obs.q<-NULL
    obs.lower.q<-NULL
    obs.upper.q<-NULL
    avPred <- NULL
    for (q in 1:n.groups){
      KM<-survival::survfit(S ~ 1,sub=groups==levels(groups)[q])
      obs.q<-c(obs.q,max(1-KM$surv))  # maximum OK because of prediction_horizon
      obs.lower.q<-c(obs.lower.q,obs.lower<-max(1-KM$upper))
      obs.upper.q<-c(obs.upper.q,obs.upper<-max(1-KM$lower))
    }
    
    midpoints <- function(x, dp=6){
      if(length(grep(',',x))>0){
      lower <- as.double(gsub('\\[','',gsub('\\(','',strsplit(x, ',')[[1]][1])))
      upper <- as.double(gsub('\\]','',gsub('\\)','',strsplit(x, ',')[[1]][2])))
      return(round(lower+(upper-lower)/2, dp))
      } else{
        return(as.double(x))
      }
    }
    
    calibrationSummary <- data.frame(predictionThreshold = unlist(lapply(levels(groups), midpoints)),
                                     averagePredictedProbability = pred,
                                     observedIncidence = obs.q,
                                     observedIncidenceLB = obs.lower.q,
                                     observedIncidenceUB = obs.upper.q)
    
    # add demographic calibration
    demographicSummary <- NULL
    demographicSummary <- getDemographicSummary(prediction, plpData, 
                                                type = 'survival', timepoint = timepoint)

    
    evaluationStatistics <- list(analysisId= attr(prediction, "metaData")$analysisId,
                                 timepoint = timepoint,
                                   populationSize = nrow(prediction),
                                   outcomeCount = sum(prediction$outcomeCount),
                                   meanSurvivalTime = meanSurvivalTime,
                                   survival = survVal,
                                   cStatistic= cStatistic,
                                 cStatistic_l95CI = cStatistic_l95CI,
                                 cStatistic_u95CI = cStatistic_u95CI,
                                   eStatistic = eStatistic,
                                   eStatistic90 = eStatistic90)
      
    
    result <- list(evaluationStatistics= evaluationStatistics,
                   demographicSummary = demographicSummary,
                   calibrationSummary = calibrationSummary,
                   thresholdSummary = nbSummary
    )
    
  }
  
  
  class(result) <- 'plpEvaluation'
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
    auc <- aucWithCi(prediction$value, prediction$outcomeCount)
    return(data.frame(auc = auc[1], auc_lb95ci = auc[2], auc_ub95ci = auc[3])) # edited 3rd to be ub?
  } else {
    auc <- aucWithoutCi(prediction$value, prediction$outcomeCount)
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
      auc <- aucWithCi(prediction, status)
      return(data.frame(auc = auc[1], auc_lb95ci = auc[2], auc_lb95ci = auc[3]))
    } else {
      auc <- aucWithoutCi(prediction, status)
      return(auc)
    }
  }
}

#' brierScore
#'
#' @details
#' Calculates the brierScore from prediction object
#'
#' @param prediction            A prediction object as generated using the
#'                              \code{\link{predictProbabilities}} function.
#'
#' @return
#' A list containing the brier score and the scaled brier score
#'
#' @export
brierScore <- function(prediction){

  brier <- sum((prediction$outcomeCount -prediction$value)^2)/nrow(prediction)
  brierMax <- mean(prediction$value)*(1-mean(prediction$value))
  brierScaled <- 1-brier/brierMax
  return(list(brier=brier,brierScaled=brierScaled))
}

#' calibrationLine
#'
#' @param prediction            A prediction object as generated using the
#'                              \code{\link{predictProbabilities}} function.
#' @param numberOfStrata        The number of groups to split the prediction into
#'
#' @details
#' Calculates the calibration from prediction object
#'
#' @export
calibrationLine <- function(prediction,numberOfStrata=10){
  outPpl <- unique(prediction$rowId)

  q <- unique(stats::quantile(prediction$value, c((1:(numberOfStrata - 1))/numberOfStrata, 1)))

  if(length(unique(c(0,q)))==2){
    warning('Prediction not spread')
    #res <- c(0,0)
    #lmData <- NULL
    #hosmerlemeshow <-  c(0,0,0)
    prediction$strata <- cut(prediction$value,
                             breaks = c(-0.1,0.5,1), #,max(prediction$value)),
                             labels = FALSE)
  } else {
    prediction$strata <- cut(prediction$value,
                             breaks = unique(c(-0.1,q)), #,max(prediction$value)),
                             labels = FALSE)
  }

  # get observed events:
  obs.Points <- stats::aggregate(prediction$outcomeCount, by=list(prediction$strata), FUN=mean)
  colnames(obs.Points) <- c('group','obs')
  pred.Points <- stats::aggregate(prediction$value, by=list(prediction$strata), FUN=mean)
  colnames(pred.Points) <- c('group','pred')

  # hosmer-lemeshow-goodness-of-fit-test
  obs.count <- stats::aggregate(prediction$outcomeCount, by=list(prediction$strata), FUN=sum)
  colnames(obs.count) <- c('group','observed')
  expected.count <- stats::aggregate(prediction$value, by=list(prediction$strata), FUN=sum)
  colnames(expected.count) <- c('group','expected')
  hoslem <- merge(obs.count, expected.count, by='group')
  obs.count2 <- stats::aggregate(1-prediction$outcomeCount, by=list(prediction$strata), FUN=sum)
  colnames(obs.count2) <- c('group','observed')
  expected.count2 <- stats::aggregate(1-prediction$value, by=list(prediction$strata), FUN=sum)
  colnames(expected.count2) <- c('group','expected')
  nhoslem <- merge(obs.count2, expected.count2, by='group')
  Xsquared <- sum((hoslem$observed-hoslem$expected)^2/hoslem$expected) +
    sum((nhoslem$observed-nhoslem$expected)^2/nhoslem$expected)
  pvalue <- stats::pchisq(Xsquared, df=numberOfStrata-2, lower.tail = F)
  hosmerlemeshow <- data.frame(Xsquared=Xsquared, df=numberOfStrata-2, pvalue=pvalue)

  # linear model fitting obs to pred:
  lmData <- merge(obs.Points, pred.Points, by='group')
  model <- stats::lm(obs ~pred, data=lmData)

  ##graphics::plot(lmData$pred, lmData$obs)
  ##graphics::abline(a = model$coefficients[1], b = model$coefficients[2], col='red')
  res <- model$coefficients
  names(res) <- c('Intercept','Gradient')
  #

  result <- list(lm=res,
                 aggregateLmData = lmData,
                 hosmerlemeshow = hosmerlemeshow)
  return(result)
}

#' Calculate the average precision
#'
#' @details
#' Calculates the average precision from a predition object
#'
#' @param prediction            A prediction object as generated using the
#'                              \code{\link{predictProbabilities}} function.
#'
#' @return
#' The average precision
#'
#' @export
averagePrecision <- function(prediction){
  lab.order <- prediction$outcomeCount[order(-prediction$value)]
  n <- nrow(prediction)
  P <- sum(prediction$outcomeCount>0)
  val <- rep(0, n)
  val[lab.order>0] <- 1:P
  return(sum(val/(1:n))/P)
}



calibrationInLarge <- function(prediction){
  
  result <- data.frame(meanPredictionRisk = mean(prediction$value),
                       observedRisk = sum(prediction$outcomeCount)/nrow(prediction),
                       N = nrow(prediction)
  )
  
  return(result)
}


calibrationWeak <- function(prediction){
  
  #do invert of log function:
  # log(p/(1-p))
  
  # edit the 0 and 1 values
  prediction$value[prediction$value==0] <- 0.000000000000001
  prediction$value[prediction$value==1] <- 1-0.000000000000001
  
  inverseLog <- log(prediction$value/(1-prediction$value))
  y <- ifelse(prediction$outcomeCount>0, 1, 0) 
  
  intercept <- suppressWarnings(stats::glm(y ~ offset(1*inverseLog), family = 'binomial'))
  intercept <- intercept$coefficients[1]
  gradient <- suppressWarnings(stats::glm(y ~ inverseLog+0, family = 'binomial',
                         offset = rep(intercept,length(inverseLog))))
  gradient <- gradient$coefficients[1]
  
  result <- data.frame(intercept = intercept, gradient = gradient)
  
  return(result)
}

#' Get a sparse summary of the calibration
#'
#' @details
#' Generates a sparse summary showing the predicted probabilities and the observed fractions. Predictions are
#' stratefied into equally sized bins of predicted probabilities.
#'
#' @param prediction            A prediction object as generated using the
#'                              \code{\link{predict}} functions.
#' @param numberOfStrata        The number of strata in the plot.
#' @param truncateFraction      This fraction of probability values will be ignored when plotting, to
#'                              avoid the x-axis scale being dominated by a few outliers.
#'
#' @return
#' A dataframe with the calibration summary
#'
#' @export
getCalibration <- function(prediction,
                           numberOfStrata = 10,
                           truncateFraction = 0.01) {
  if (attr(prediction, "metaData")$predictionType != "binary")
    stop("Plotting the calibration is only implemented for binary classification models")

  q <- unique(stats::quantile(prediction$value, (1:(numberOfStrata - 1))/numberOfStrata))
  prediction$predictionThresholdId <- cut(prediction$value,
                                          breaks = unique(c(-0.00001, q, max(prediction$value))),
                                          labels = FALSE)

  prediction <- merge(prediction,
                      data.frame(predictionThresholdId=1:(length(q)+1), predictionThreshold=c(0, q)),
                      by='predictionThresholdId', all.x=T)

  computeStratumStats <- function(data) {
    return(data.frame(minx = min(data$value),
                      maxx = max(data$value),
                      fraction = sum(data$outcomeCount)/nrow(data)))
  }

  # count the number of persons with the age/gender strata
  PersonCountAtRisk <- stats::aggregate(rowId ~ predictionThreshold, data = prediction, length)
  names(PersonCountAtRisk)[2] <- "PersonCountAtRisk"

  # count the number of persons with the age/gender strata in T also in O at time-at-risk
  PersonCountWithOutcome <- stats::aggregate(outcomeCount ~ predictionThreshold, data = prediction, sum)
  names(PersonCountWithOutcome)[2] <- "PersonCountWithOutcome"

  strataData <- merge(PersonCountAtRisk,
                      PersonCountWithOutcome)

  # Select all persons within the predictionThreshold, compute their average predicted probability
  averagePredictedProbability <- stats::aggregate(prediction$value, list(prediction$predictionThreshold),
                                                  mean)
  colnames(averagePredictedProbability) <- c('predictionThreshold', 'averagePredictedProbability')
  strataData <- merge(strataData,averagePredictedProbability)

  StDevPredictedProbability <- stats::aggregate(prediction$value, list(prediction$predictionThreshold),
                                                sd)
  colnames(StDevPredictedProbability) <- c('predictionThreshold', 'StDevPredictedProbability')
  strataData <- merge(strataData, StDevPredictedProbability)

  # MinPredictedProbability, P25PredictedProbability, MedianPredictedProbability,
  # P75PredictedProbability, MaxPredictedProbability
  quantiles <- stats::aggregate(prediction$value, list(prediction$predictionThreshold),
                                function(x) stats::quantile(x, probs = c(0,0.25,0.5,0.75,1))
  )
  quantiles <- as.matrix(quantiles)
  colnames(quantiles) <- c('predictionThreshold', 'MinPredictedProbability',
                           'P25PredictedProbability', 'MedianPredictedProbability',
                           'P75PredictedProbability', 'MaxPredictedProbability')
  strataData <- merge(strataData, quantiles)

  # From among all persons in T within the age/gender strata, compute the proportion in O during the time-at-risk  (PersonCountWithOutcome / PersonCountAtRisk)
  # TODO: need to make sure PersonCountAtRisk is not zero - how to handle zeros?
  strataData$observedIncidence <- strataData$PersonCountWithOutcome/strataData$PersonCountAtRisk

  attr(strataData,'lims') <- stats::quantile(prediction$value, c(truncateFraction, 1 - truncateFraction))

  return(strataData)
}


#' Calculate all measures for sparse ROC
#'
#' @details
#' Calculates the TP, FP, TN, FN, TPR, FPR, accuracy, PPF, FOR and Fmeasure
#' from a predition object
#'
#' @param prediction            A prediction object as generated using the
#'                              \code{\link{predictProbabilities}} function.
#'
#' @return
#' A data.frame with all the measures
#'
#' @export
getThresholdSummary <- function(prediction){

  # do input checks
  if(nrow(prediction)<1){
    warning('sparse roc not calculated due to empty dataset')
    return(NULL)
  }

  n <- nrow(prediction)
  P <- sum(prediction$outcomeCount>0)
  N <- n - P

  if(N==0){
    warning('Number of negatives is zero')
    return(NULL)
  }
  if(P==0){
    warning('Number of positives is zero')
    return(NULL)
  }

  # add the preference score:
  proportion <- sum(prediction$outcomeCount)/nrow(prediction)
  # ISSUE WITH CAL # remove any predictions of 1
  prediction$value[prediction$value==1] <- 0.99999999
  x <- exp(log(prediction$value/(1 - prediction$value)) - log(proportion/(1 - proportion)))
  prediction$preferenceScore <- x/(x + 1)

  # get 100 points of distribution:
  # get the predictionThreshold and preferenceThreshold
  # add top 100 risk as well (for high ppv)
  predictionThreshold <- sort(c(prediction$value[order(-prediction$value)][1:100], 
                              stats::quantile(prediction$value, probs=seq(0,0.99,0.01),na.rm=TRUE)))
  preferenceThreshold <- sort(c(prediction$preferenceScore[order(-prediction$preferenceScore)][1:100],
                              stats::quantile(prediction$preferenceScore, seq(0,0.99,0.01),na.rm=TRUE)))
  
  # fix quantile bug when not enought unique values - this seems to get rid of issue
  for (val in names(table(predictionThreshold))[table(predictionThreshold)>1])
    predictionThreshold[predictionThreshold==val] <- as.double(val)

  # get the outcomeCount ordered by predicted value
  lab.order <- prediction$outcomeCount[order(-prediction$value)]
  valueOrdered <- prediction$value[order(-prediction$value)]
  # fix some rounding issue bug
  valueOrdered <-round(valueOrdered, digits = 15)
  predictionThreshold <- round(predictionThreshold, digits=15)
  # get the indexes for the predictionThreshold
  indexesOfInt <- sapply(predictionThreshold, function(x) min(which(valueOrdered<=x)))

  #TODO: FIGURE THIS BUG OUT # remove infs caused by R bug?
  if(sum(is.infinite(indexesOfInt))>0){
    imputeInd <- sapply(which(is.infinite(indexesOfInt)), function(x) max(which(which(!is.infinite(indexesOfInt))<x)))
    indexesOfInt[which(is.infinite(indexesOfInt))] <- indexesOfInt[imputeInd]
  }
  ##indexesOfInt <- indexesOfInt[!is.infinite(indexesOfInt)]

  # improve speed create vector with cum sum
  temp.cumsum <- rep(0, length(lab.order))
  temp.cumsum[lab.order==0] <- 1:sum(lab.order==0)
  temp.cumsum[lab.order==1] <- which(lab.order==1, arr.ind=T)-1:sum(lab.order==1)
  TP <- sapply(indexesOfInt, function(x) x-temp.cumsum[x])
  FP <- sapply(indexesOfInt, function(x) temp.cumsum[x])

  TN <- N-FP
  FN <- P-TP

  positiveCount <- TP+FP
  negativeCount <- TN+FN
  trueCount <- TP+FN
  falseCount <- TN + FP
  truePositiveCount <- TP
  trueNegativeCount <- TN
  falsePositiveCount <- FP
  falseNegativeCount <- FN

  f1Score <- f1Score(TP,TN,FN,FP)
  accuracy <- accuracy(TP,TN,FN,FP)
  sensitivity <- sensitivity(TP,TN,FN,FP)
  falseNegativeRate <- falseNegativeRate(TP,TN,FN,FP)
  falsePositiveRate <- falsePositiveRate(TP,TN,FN,FP)
  specificity <-  specificity(TP,TN,FN,FP)
  positivePredictiveValue <- positivePredictiveValue(TP,TN,FN,FP)
  falseDiscoveryRate <- falseDiscoveryRate(TP,TN,FN,FP)
  negativePredictiveValue <- negativePredictiveValue(TP,TN,FN,FP)
  falseOmissionRate <- falseOmissionRate(TP,TN,FN,FP)
  positiveLikelihoodRatio <- positiveLikelihoodRatio(TP,TN,FN,FP)
  negativeLikelihoodRatio <- negativeLikelihoodRatio(TP,TN,FN,FP)
  diagnosticOddsRatio <- diagnosticOddsRatio(TP,TN,FN,FP)

  return(data.frame(predictionThreshold=predictionThreshold,
                    preferenceThreshold=preferenceThreshold,
                    positiveCount=positiveCount,
                    negativeCount=negativeCount,
                    trueCount=trueCount, falseCount=falseCount,
                    truePositiveCount=truePositiveCount,
                    trueNegativeCount=trueNegativeCount,
                    falsePositiveCount=falsePositiveCount,
                    falseNegativeCount=falseNegativeCount,
                    f1Score=f1Score, accuracy=accuracy,
                    sensitivity=sensitivity,
                    falseNegativeRate=falseNegativeRate,
                    falsePositiveRate=falsePositiveRate,
                    specificity=specificity,
                    positivePredictiveValue=positivePredictiveValue,
                    falseDiscoveryRate=falseDiscoveryRate,
                    negativePredictiveValue=negativePredictiveValue,
                    falseOmissionRate=falseOmissionRate,
                    positiveLikelihoodRatio=positiveLikelihoodRatio,
                    negativeLikelihoodRatio=negativeLikelihoodRatio,
                    diagnosticOddsRatio=diagnosticOddsRatio
  ))

}




#' Calculates the prediction distribution
#'
#' @details
#' Calculates the quantiles from a predition object
#'
#' @param prediction            A prediction object as generated using the
#'                              \code{\link{predictProbabilities}} function.
#'
#' @return
#' The 0.00, 0.1, 0.25, 0.5, 0.75, 0.9, 1.00 quantile pf the prediction,
#' the mean and standard deviation per class
#'
#' @export
getPredictionDistribution <- function(prediction){

  # PersonCount - count the number of persons in the class
  predictionDistribution <- stats::aggregate(prediction$value, list(prediction$outcomeCount),
                                             length)
  colnames(predictionDistribution) <- c('class', 'PersonCount')

  # averagePredictedProbability	StDevPredictedProbability
  averagePredictedProbability <- stats::aggregate(prediction$value, list(prediction$outcomeCount),
                                                  mean)
  colnames(averagePredictedProbability) <- c('class', 'averagePredictedProbability')
  StDevPredictedProbability <- stats::aggregate(prediction$value, list(prediction$outcomeCount),
                                                sd)
  colnames(StDevPredictedProbability) <- c('class', 'StDevPredictedProbability')

  predictionDistribution <- merge(predictionDistribution,averagePredictedProbability )
  predictionDistribution <- merge(predictionDistribution,StDevPredictedProbability )

  quantiles <- stats::aggregate(prediction$value, list(prediction$outcomeCount),
                                function(x) stats::quantile(x, probs = c(0.00,0.05, 0.25, 0.5, 0.75,0.95, 1.00))
  )
  quantiles <- as.matrix(quantiles)
  colnames(quantiles) <- c('class', 'MinPredictedProbability', 'P05PredictedProbability',
                           'P25PredictedProbability', 'MedianPredictedProbability',
                           'P75PredictedProbability', 'P95PredictedProbability',
                           'MaxPredictedProbability')
  predictionDistribution <- merge(predictionDistribution,quantiles )

  return(predictionDistribution)
}

getDemographicSummary <- function(prediction, plpData, type = 'binary', timepoint = NULL){
  
  demographicData <- plpData$cohorts %>% dplyr::mutate(ageId = floor(ageYear/5),
                                                       ageGroup = paste0('Age group: ', floor(ageYear/5)*5, '-',floor(ageYear/5)*5+4),
                                                       genId = gender,
                                                       genGroup = ifelse(gender==8507, 'Male', 'Female')) %>%
    dplyr::select(rowId,ageId,ageGroup,genId,genGroup ) %>%
    dplyr::inner_join(prediction[,c('rowId', 'value','outcomeCount','survivalTime','daysToCohortEnd')], by='rowId')
  
  if(type == 'binary'){
    demographicData <- demographicData %>%
      dplyr::group_by(ageGroup,genGroup)  %>%
      dplyr::summarise(PersonCountAtRisk = length(outcomeCount), 
                       PersonCountWithOutcome = sum(outcomeCount),
                       averagePredictedProbability = mean(value, na.rm = T),
                       StDevPredictedProbability = sd(value, na.rm = T),
                       MinPredictedProbability =stats::quantile(value, probs = 0),
                       P25PredictedProbability =stats::quantile(value, probs = 0.25),
                       P50PredictedProbability =stats::quantile(value, probs = 0.50),
                       P75PredictedProbability =stats::quantile(value, probs = 0.75),
                       MaxPredictedProbability =stats::quantile(value, probs = 1),
      )
  } else{
    
    if(is.null(timepoint)){
      timepoint <- max(demographicData$survivalTime)
    }
    demographicSum <- demographicData %>% dplyr::mutate(t = survivalTime,#t = apply(cbind(daysToCohortEnd, survivalTime), 1, min),
                                      y = ifelse(outcomeCount > 0, 1, 0))
    
    gen <- unique(demographicData$genGroup)
    ageGroup <- unique(demographicData$ageGroup)
    
    demographicData <- NULL
    for(gen in gen){
      for(age in ageGroup){
        
        tempDemo <- demographicSum %>% dplyr::filter(genGroup == gen & ageGroup == age)
        
        if(nrow(tempDemo)>0){
          t1 <- tempDemo %>% dplyr::select(t)
          y1 <- tempDemo %>% dplyr::select(y)
          p1 <- tempDemo %>% dplyr::select(value)
          
          out <- tryCatch({summary(survival::survfit(survival::Surv(t1$t, y1$y) ~ 1), times = timepoint)},
                          error = function(e){ParallelLogger::logError(e); return(NULL)})
          demoTemp <- c(genGroup = gen, ageGroup = age, 
                        PersonCountAtRisk = length(p1$value),
                        PersonCountWithOutcome = round(length(p1$value)*(1-out$surv)),
                        observedRisk = 1-out$surv, 
                        averagePredictedProbability = mean(p1$value, na.rm = T),
                        StDevPredictedProbability = sd(p1$value, na.rm = T))
          
          demographicData <- rbind(demographicData, demoTemp)
        }
        
      }
      
    }
    demographicData <- as.data.frame(demographicData)
    demographicData$averagePredictedProbability <- as.double(as.character(demographicData$averagePredictedProbability ))
    demographicData$StDevPredictedProbability <- as.double(as.character(demographicData$StDevPredictedProbability ))
    demographicData$PersonCountAtRisk <- as.double(as.character(demographicData$PersonCountAtRisk ))
    demographicData$PersonCountWithOutcome <- as.double(as.character(demographicData$PersonCountWithOutcome ))
    
  }
  
  demographicData <- as.data.frame(demographicData)
  
  return(demographicData)
  
}



# making all this single for easy unit testing
#' Calculate the f1Score
#'
#' @details
#' Calculate the f1Score
#'
#' @param TP                Number of true positives
#' @param TN                Number of true negatives
#' @param FN                Number of false negatives
#' @param FP                Number of false positives
#'
#' @return
#' f1Score value
#'
#' @export
f1Score <- function(TP,TN,FN,FP){
  if(sum(TP<0)>0) stop('TP < 0')
  if(sum(FP<0)>0) stop('FP < 0')
  if(sum(TN<0)>0) stop('TN < 0')
  if(sum(FN<0)>0) stop('FN < 0')
  if(class(TP)!='numeric') stop('Incorrect TP class')
  if(class(FP)!='numeric') stop('Incorrect FP class')
  if(class(TN)!='numeric') stop('Incorrect TN class')
  if(class(FN)!='numeric') stop('Incorrect FN class')
  return(2*(TP/(TP+FP))*(TP/(TP+FN))/((TP/(TP+FP))+(TP/(TP+FN))))
         }
#' Calculate the accuracy
#'
#' @details
#' Calculate the accuracy
#'
#' @param TP                Number of true positives
#' @param TN                Number of true negatives
#' @param FN                Number of false negatives
#' @param FP                Number of false positives
#'
#' @return
#' accuracy value
#'
#' @export
accuracy <- function(TP,TN,FN,FP){
  if(sum(TP<0)>0) stop('TP < 0')
  if(sum(FP<0)>0) stop('FP < 0')
  if(sum(TN<0)>0) stop('TN < 0')
  if(sum(FN<0)>0) stop('FN < 0')
  if(class(TP)!='numeric') stop('Incorrect TP class')
  if(class(FP)!='numeric') stop('Incorrect FP class')
  if(class(TN)!='numeric') stop('Incorrect TN class')
  if(class(FN)!='numeric') stop('Incorrect FN class')
  (TP+TN)/(TP+TN+FP+FN)}
#' Calculate the sensitivity
#'
#' @details
#' Calculate the sensitivity
#'
#' @param TP                Number of true positives
#' @param TN                Number of true negatives
#' @param FN                Number of false negatives
#' @param FP                Number of false positives
#'
#' @return
#' sensitivity value
#'
#' @export
sensitivity <- function(TP,TN,FN,FP){
  if(sum(TP<0)>0) stop('TP < 0')
  if(sum(FP<0)>0) stop('FP < 0')
  if(sum(TN<0)>0) stop('TN < 0')
  if(sum(FN<0)>0) stop('FN < 0')
  if(class(TP)!='numeric') stop('Incorrect TP class')
  if(class(FP)!='numeric') stop('Incorrect FP class')
  if(class(TN)!='numeric') stop('Incorrect TN class')
  if(class(FN)!='numeric') stop('Incorrect FN class')
  TP/(TP+FN)}
#' Calculate the falseNegativeRate
#'
#' @details
#' Calculate the falseNegativeRate
#'
#' @param TP                Number of true positives
#' @param TN                Number of true negatives
#' @param FN                Number of false negatives
#' @param FP                Number of false positives
#'
#' @return
#' falseNegativeRate  value
#'
#' @export
falseNegativeRate <- function(TP,TN,FN,FP){
  if(sum(TP<0)>0) stop('TP < 0')
  if(sum(FP<0)>0) stop('FP < 0')
  if(sum(TN<0)>0) stop('TN < 0')
  if(sum(FN<0)>0) stop('FN < 0')
  if(class(TP)!='numeric') stop('Incorrect TP class')
  if(class(FP)!='numeric') stop('Incorrect FP class')
  if(class(TN)!='numeric') stop('Incorrect TN class')
  if(class(FN)!='numeric') stop('Incorrect FN class')
  FN/(TP+FN)}
#' Calculate the falsePositiveRate
#'
#' @details
#' Calculate the falsePositiveRate
#'
#' @param TP                Number of true positives
#' @param TN                Number of true negatives
#' @param FN                Number of false negatives
#' @param FP                Number of false positives
#'
#' @return
#' falsePositiveRate  value
#'
#' @export
falsePositiveRate <- function(TP,TN,FN,FP){
  if(sum(TP<0)>0) stop('TP < 0')
  if(sum(FP<0)>0) stop('FP < 0')
  if(sum(TN<0)>0) stop('TN < 0')
  if(sum(FN<0)>0) stop('FN < 0')
  if(class(TP)!='numeric') stop('Incorrect TP class')
  if(class(FP)!='numeric') stop('Incorrect FP class')
  if(class(TN)!='numeric') stop('Incorrect TN class')
  if(class(FN)!='numeric') stop('Incorrect FN class')
  FP/(FP+TN)}
#' Calculate the specificity
#'
#' @details
#' Calculate the specificity
#'
#' @param TP                Number of true positives
#' @param TN                Number of true negatives
#' @param FN                Number of false negatives
#' @param FP                Number of false positives
#'
#' @return
#' specificity value
#'
#' @export
specificity <- function(TP,TN,FN,FP){
  if(sum(TP<0)>0) stop('TP < 0')
  if(sum(FP<0)>0) stop('FP < 0')
  if(sum(TN<0)>0) stop('TN < 0')
  if(sum(FN<0)>0) stop('FN < 0')
  if(class(TP)!='numeric') stop('Incorrect TP class')
  if(class(FP)!='numeric') stop('Incorrect FP class')
  if(class(TN)!='numeric') stop('Incorrect TN class')
  if(class(FN)!='numeric') stop('Incorrect FN class')
  TN/(FP+TN)}
#' Calculate the positivePredictiveValue
#'
#' @details
#' Calculate the positivePredictiveValue
#'
#' @param TP                Number of true positives
#' @param TN                Number of true negatives
#' @param FN                Number of false negatives
#' @param FP                Number of false positives
#'
#' @return
#' positivePredictiveValue value
#'
#' @export
positivePredictiveValue <- function(TP,TN,FN,FP){
  if(sum(TP<0)>0) stop('TP < 0')
  if(sum(FP<0)>0) stop('FP < 0')
  if(sum(TN<0)>0) stop('TN < 0')
  if(sum(FN<0)>0) stop('FN < 0')
  if(class(TP)!='numeric') stop('Incorrect TP class')
  if(class(FP)!='numeric') stop('Incorrect FP class')
  if(class(TN)!='numeric') stop('Incorrect TN class')
  if(class(FN)!='numeric') stop('Incorrect FN class')
  TP/(TP+FP)}
#' Calculate the falseDiscoveryRate
#'
#' @details
#' Calculate the falseDiscoveryRate
#'
#' @param TP                Number of true positives
#' @param TN                Number of true negatives
#' @param FN                Number of false negatives
#' @param FP                Number of false positives
#'
#' @return
#' falseDiscoveryRate value
#'
#' @export
falseDiscoveryRate <- function(TP,TN,FN,FP){
  if(sum(TP<0)>0) stop('TP < 0')
  if(sum(FP<0)>0) stop('FP < 0')
  if(sum(TN<0)>0) stop('TN < 0')
  if(sum(FN<0)>0) stop('FN < 0')
  if(class(TP)!='numeric') stop('Incorrect TP class')
  if(class(FP)!='numeric') stop('Incorrect FP class')
  if(class(TN)!='numeric') stop('Incorrect TN class')
  if(class(FN)!='numeric') stop('Incorrect FN class')
  FP/(TP+FP)}
#' Calculate the negativePredictiveValue
#'
#' @details
#' Calculate the negativePredictiveValue
#'
#' @param TP                Number of true positives
#' @param TN                Number of true negatives
#' @param FN                Number of false negatives
#' @param FP                Number of false positives
#'
#' @return
#' negativePredictiveValue value
#'
#' @export
negativePredictiveValue <- function(TP,TN,FN,FP){
  if(sum(TP<0)>0) stop('TP < 0')
  if(sum(FP<0)>0) stop('FP < 0')
  if(sum(TN<0)>0) stop('TN < 0')
  if(sum(FN<0)>0) stop('FN < 0')
  if(class(TP)!='numeric') stop('Incorrect TP class')
  if(class(FP)!='numeric') stop('Incorrect FP class')
  if(class(TN)!='numeric') stop('Incorrect TN class')
  if(class(FN)!='numeric') stop('Incorrect FN class')
  TN/(FN+TN)}
#' Calculate the falseOmissionRate
#'
#' @details
#' Calculate the falseOmissionRate
#'
#' @param TP                Number of true positives
#' @param TN                Number of true negatives
#' @param FN                Number of false negatives
#' @param FP                Number of false positives
#'
#' @return
#' falseOmissionRate value
#'
#' @export
falseOmissionRate <- function(TP,TN,FN,FP){
  if(sum(TP<0)>0) stop('TP < 0')
  if(sum(FP<0)>0) stop('FP < 0')
  if(sum(TN<0)>0) stop('TN < 0')
  if(sum(FN<0)>0) stop('FN < 0')
  if(class(TP)!='numeric') stop('Incorrect TP class')
  if(class(FP)!='numeric') stop('Incorrect FP class')
  if(class(TN)!='numeric') stop('Incorrect TN class')
  if(class(FN)!='numeric') stop('Incorrect FN class')
  FN/(FN+TN)}

#' Calculate the positiveLikelihoodRatio
#'
#' @details
#' Calculate the positiveLikelihoodRatio
#'
#' @param TP                Number of true positives
#' @param TN                Number of true negatives
#' @param FN                Number of false negatives
#' @param FP                Number of false positives
#'
#' @return
#' positiveLikelihoodRatio value
#'
#' @export
positiveLikelihoodRatio <- function(TP,TN,FN,FP){
  if(sum(TP<0)>0) stop('TP < 0')
  if(sum(FP<0)>0) stop('FP < 0')
  if(sum(TN<0)>0) stop('TN < 0')
  if(sum(FN<0)>0) stop('FN < 0')
  if(class(TP)!='numeric') stop('Incorrect TP class')
  if(class(FP)!='numeric') stop('Incorrect FP class')
  if(class(TN)!='numeric') stop('Incorrect TN class')
  if(class(FN)!='numeric') stop('Incorrect FN class')
  (TP/(TP+FN))/(FP/(FP+TN))}

#' Calculate the negativeLikelihoodRatio
#'
#' @details
#' Calculate the negativeLikelihoodRatio
#'
#' @param TP                Number of true positives
#' @param TN                Number of true negatives
#' @param FN                Number of false negatives
#' @param FP                Number of false positives
#'
#' @return
#' negativeLikelihoodRatio value
#'
#' @export
negativeLikelihoodRatio <- function(TP,TN,FN,FP){
  if(sum(TP<0)>0) stop('TP < 0')
  if(sum(FP<0)>0) stop('FP < 0')
  if(sum(TN<0)>0) stop('TN < 0')
  if(sum(FN<0)>0) stop('FN < 0')
  if(class(TP)!='numeric') stop('Incorrect TP class')
  if(class(FP)!='numeric') stop('Incorrect FP class')
  if(class(TN)!='numeric') stop('Incorrect TN class')
  if(class(FN)!='numeric') stop('Incorrect FN class')
  (FN/(TP+FN))/(TN/(FP+TN))}


#' Calculate the diagnostic odds ratio
#'
#' @details
#' Calculate the diagnostic odds ratio
#'
#' @param TP                Number of true positives
#' @param TN                Number of true negatives
#' @param FN                Number of false negatives
#' @param FP                Number of false positives
#'
#' @return
#' diagnosticOddsRatio value
#'
#' @export
diagnosticOddsRatio <- function(TP,TN,FN,FP){
  if(sum(TP<0)>0) stop('TP < 0')
  if(sum(FP<0)>0) stop('FP < 0')
  if(sum(TN<0)>0) stop('TN < 0')
  if(sum(FN<0)>0) stop('FN < 0')
  if(class(TP)!='numeric') stop('Incorrect TP class')
  if(class(FP)!='numeric') stop('Incorrect FP class')
  if(class(TN)!='numeric') stop('Incorrect TN class')
  if(class(FN)!='numeric') stop('Incorrect FN class')
  ((TP/(TP+FN))/(FP/(FP+TN)))/((FN/(TP+FN))/(TN/(FP+TN)))}





# taken from the dca package @ http://www.danieldsjoberg.com/dca/articles/survival-outcomes.html

stdca <- function (data, outcome, ttoutcome, timepoint, predictors, xstart = 0.01, 
                   xstop = 0.99, xby = 0.01, ymin = -0.05, probability = NULL, 
                   harm = NULL, graph = TRUE, intervention = FALSE, interventionper = 100, 
                   smooth = FALSE, loess.span = 0.1, cmprsk = FALSE) 
{
  data = data[stats::complete.cases(data[c(outcome, ttoutcome, 
                                           predictors)]), c(outcome, ttoutcome, predictors)]
  if ((length(data[!(data[outcome] == 0 | data[outcome] == 
                     1), outcome]) > 0) & cmprsk == FALSE) {
    stop("outcome must be coded as 0 and 1")
  }
  if (class(data) != "data.frame") {
    stop("Input data must be class data.frame")
  }
  if (xstart < 0 | xstart > 1) {
    stop("xstart must lie between 0 and 1")
  }
  if (xstop < 0 | xstop > 1) {
    stop("xstop must lie between 0 and 1")
  }
  if (xby <= 0 | xby >= 1) {
    stop("xby must lie between 0 and 1")
  }
  if (xstart >= xstop) {
    stop("xstop must be larger than xstart")
  }
  pred.n = length(predictors)
  if (length(probability) > 0 & pred.n != length(probability)) {
    stop("Number of probabilities specified must be the same as the number of predictors being checked.")
  }
  if (length(harm) > 0 & pred.n != length(harm)) {
    stop("Number of harms specified must be the same as the number of predictors being checked.")
  }
  if (length(harm) == 0) {
    harm = rep(0, pred.n)
  }
  if (length(probability) == 0) {
    probability = rep(TRUE, pred.n)
  }
  if (length(predictors[predictors == "all" | predictors == 
                        "none"])) {
    stop("Prediction names cannot be equal to all or none.")
  }
  for (m in 1:pred.n) {
    if (probability[m] != TRUE & probability[m] != FALSE) {
      stop("Each element of probability vector must be TRUE or FALSE")
    }
    if (probability[m] == TRUE & (max(data[predictors[m]]) > 
                                  1 | min(data[predictors[m]]) < 0)) {
      stop(paste(predictors[m], "must be between 0 and 1 OR sepcified as a non-probability in the probability option", 
                 sep = " "))
    }
    if (probability[m] == FALSE) {
      model = NULL
      pred = NULL
      model = survival::coxph(survival::Surv(data.matrix(data[ttoutcome]), 
                                             data.matrix(data[outcome])) ~ data.matrix(data[predictors[m]]))
      surv.data = data.frame(0)
      pred = data.frame(1 - c(summary(survival::survfit(model, 
                                                        newdata = surv.data), time = timepoint)$surv))
      names(pred) = predictors[m]
      data = cbind(data[names(data) != predictors[m]], 
                   pred)
      print(paste(predictors[m], "converted to a probability with Cox regression. Due to linearity and proportional hazards assumption, miscalibration may occur.", 
                  sep = " "))
    }
  }
  N = dim(data)[1]
  if (cmprsk == FALSE) {
    km.cuminc = survival::survfit(survival::Surv(data.matrix(data[ttoutcome]), #fixed missing survival::
                                                 data.matrix(data[outcome])) ~ 1)
    pd = 1 - summary(km.cuminc, times = timepoint)$surv
  }
  else {
    #cr.cuminc = cmprsk::cuminc(data[[ttoutcome]], data[[outcome]])
    #pd = cmprsk::timepoints(cr.cuminc, times = timepoint)$est[1]
    stop('not supported')
  }
  nb = data.frame(seq(from = xstart, to = xstop, by = xby))
  names(nb) = "threshold"
  interv = nb
  error = NULL
  nb["all"] = pd - (1 - pd) * nb$threshold/(1 - nb$threshold)
  nb["none"] = 0
  for (m in 1:pred.n) {
    nb[predictors[m]] = NA
    for (t in 1:length(nb$threshold)) {
      px = sum(data[predictors[m]] > nb$threshold[t])/N
      if (px == 0) {
        error = rbind(error, paste(predictors[m], ": No observations with risk greater than ", 
                                   nb$threshold[t] * 100, "%", sep = ""))
        break
      }
      else {
        if (cmprsk == FALSE) {
          km.cuminc = survival::survfit(survival::Surv(data.matrix(data[data[predictors[m]] > 
                                                                          nb$threshold[t], ttoutcome]), data.matrix(data[data[predictors[m]] > 
                                                                                                                           nb$threshold[t], outcome])) ~ 1)
          pdgivenx = (1 - summary(km.cuminc, times = timepoint)$surv)
          if (length(pdgivenx) == 0) {
            error = rbind(error, paste(predictors[m], 
                                       ": No observations with risk greater than ", 
                                       nb$threshold[t] * 100, "% that have followup through the timepoint selected", 
                                       sep = ""))
            break
          }
        }
        else {
          #cr.cuminc = cmprsk::cuminc(data[[ttoutcome]][data[[predictors[m]]] > 
          #                                               nb$threshold[t]], data[[outcome]][data[[predictors[m]]] > 
          #                                                                                   nb$threshold[t]])
          #pdgivenx = cmprsk::timepoints(cr.cuminc, times = timepoint)$est[1]
          #if (is.na(pdgivenx)) {
          #  error = rbind(error, paste(predictors[m], 
          #                             ": No observations with risk greater than ", 
          #                             nb$threshold[t] * 100, "% that have followup through the timepoint selected", 
          #                             sep = ""))
          #  break
          stop('not supported')
        }
      }
      nb[t, predictors[m]] = pdgivenx * px - (1 - pdgivenx) * 
        px * nb$threshold[t]/(1 - nb$threshold[t]) - 
        harm[m]
    }
  interv[predictors[m]] = (nb[predictors[m]] - nb["all"]) * 
    interventionper/(interv$threshold/(1 - interv$threshold))
}
if (length(error) > 0) {
  print(paste(error, ", and therefore net benefit not calculable in this range.", 
              sep = ""))
}
for (m in 1:pred.n) {
  if (smooth == TRUE) {
    lws = stats::loess(data.matrix(nb[!is.na(nb[[predictors[m]]]), 
                                      predictors[m]]) ~ data.matrix(nb[!is.na(nb[[predictors[m]]]), 
                                                                       "threshold"]), span = loess.span)
    nb[!is.na(nb[[predictors[m]]]), paste(predictors[m], 
                                          "_sm", sep = "")] = lws$fitted
    lws = stats::loess(data.matrix(interv[!is.na(nb[[predictors[m]]]), 
                                          predictors[m]]) ~ data.matrix(interv[!is.na(nb[[predictors[m]]]), 
                                                                               "threshold"]), span = loess.span)
    interv[!is.na(nb[[predictors[m]]]), paste(predictors[m], 
                                              "_sm", sep = "")] = lws$fitted
  }
}
if (graph == TRUE) {
  if (intervention == TRUE) {
    legendlabel <- NULL
    legendcolor <- NULL
    legendwidth <- NULL
    legendpattern <- NULL
    ymax = max(interv[predictors], na.rm = TRUE)
    plot(x = nb$threshold, y = nb$all, type = "n", 
         xlim = c(xstart, xstop), ylim = c(ymin, ymax), 
         xlab = "Threshold probability", ylab = paste("Net reduction in interventions per", 
                                                      interventionper, "patients"))
    for (m in 1:pred.n) {
      if (smooth == TRUE) {
        lines(interv$threshold, data.matrix(interv[paste(predictors[m], 
                                                         "_sm", sep = "")]), col = m, 
              lty = 2)
      }
      else {
        lines(interv$threshold, data.matrix(interv[predictors[m]]), 
              col = m, lty = 2)
      }
      legendlabel <- c(legendlabel, predictors[m])
      legendcolor <- c(legendcolor, m)
      legendwidth <- c(legendwidth, 1)
      legendpattern <- c(legendpattern, 2)
    }
  }
  else {
    legendlabel <- c("None", "All")
    legendcolor <- c(17, 8)
    legendwidth <- c(2, 2)
    legendpattern <- c(1, 1)
    ymax = max(nb[names(nb) != "threshold"], na.rm = TRUE)
    graphics::plot(x = nb$threshold, y = nb$all, type = "l", 
                   col = 8, lwd = 2, xlim = c(xstart, xstop), ylim = c(ymin, 
                                                                       ymax), xlab = "Threshold probability", 
                   ylab = "Net benefit")
    graphics::lines(x = nb$threshold, y = nb$none, lwd = 2)
    for (m in 1:pred.n) {
      if (smooth == TRUE) {
        lines(nb$threshold, data.matrix(nb[paste(predictors[m], 
                                                 "_sm", sep = "")]), col = m, 
              lty = 2)
      }
      else {
        lines(nb$threshold, data.matrix(nb[predictors[m]]), 
              col = m, lty = 2)
      }
      legendlabel <- c(legendlabel, predictors[m])
      legendcolor <- c(legendcolor, m)
      legendwidth <- c(legendwidth, 1)
      legendpattern <- c(legendpattern, 2)
    }
  }
  graphics::legend("topright", legendlabel, cex = 0.8, 
                   col = legendcolor, lwd = legendwidth, lty = legendpattern)
}
results = list()
results$N = N
results$predictors = data.frame(cbind(predictors, harm, probability))
names(results$predictors) = c("predictor", "harm.applied", 
                              "probability")
results$interventions.avoided.per = interventionper
results$net.benefit = nb
results$interventions.avoided = interv
return(results)
}
