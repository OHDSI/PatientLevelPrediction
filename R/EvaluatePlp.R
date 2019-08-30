# @file Evaluate.R
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
  if (type != "binary") {
    stop('Currently only support binary classification models')
  }

  if(is.null(prediction$outcomeCount)){
    stop('No outcomeCount column present')
  }
  if(length(unique(prediction$value))==1){
    stop('Cannot evaluate as predictions all the same value')
  }
  #============================

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

  # 2) thresholdSummary
  # need to update thresholdSummary this with all the requested values
  ParallelLogger::logTrace(paste0('Calulating Threshold summary Started @ ',Sys.time()))
  thresholdSummary <-getThresholdSummary(prediction) # rename and edit this

  ParallelLogger::logTrace(paste0('Completed @ ',Sys.time()))
  
  # 3) demographicSummary
  ParallelLogger::logTrace(paste0('Calulating Demographic Based Evaluation Started @ ',Sys.time()))
  demographicSummary <- tryCatch(getDemographicSummary(prediction, plpData),
                                 error= function(cond){return(NULL)})
  ParallelLogger::logTrace(paste0('Completed @ ',Sys.time()))


  # calibration linear fit- returns gradient, intercept
  ParallelLogger::logTrace('Calculating Calibration Line')
  calLine10 <- calibrationLine(prediction, numberOfStrata = 10)
  ParallelLogger::logInfo(sprintf('%-20s%.2f%-20s%.2f', 'Calibration gradient: ', calLine10$lm[2], ' intercept: ',calLine10$lm[1]))
  # 4) calibrationSummary
  ParallelLogger::logTrace(paste0('Calculating Calibration Summary Started @ ',Sys.time()))
  calibrationSummary <- getCalibration(prediction,
                                       numberOfStrata = 10,
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
                               CalibrationIntercept= calLine10$lm[1],	
                               CalibrationSlope = calLine10$lm[2])

  result <- list(evaluationStatistics= evaluationStatistics,
                 thresholdSummary= thresholdSummary,
                 demographicSummary = demographicSummary,
                 calibrationSummary = calibrationSummary,
                 predictionDistribution = predictionDistribution
  )
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
  predictionThreshold <- stats::quantile(prediction$value, probs=seq(0,0.99,0.01),na.rm=TRUE)
  preferenceThreshold <- stats::quantile(prediction$preferenceScore, seq(0,0.99,0.01),na.rm=TRUE)
  
  # fix quantile bug when not enought unique values - this seems to get rid of issue
  for (val in names(table(predictionThreshold))[table(predictionThreshold)>1])
    predictionThreshold[predictionThreshold==val] <- as.double(val)

  # get the outcomeCount ordered by predicted value
  lab.order <- prediction$outcomeCount[order(-prediction$value)]
  valueOrdered <- prediction$value[order(-prediction$value)]
  # fix some rounding issue bug
  valueOrdered <-round(valueOrdered, digits = 10)
  predictionThreshold <- round(predictionThreshold, digits=10)
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

getDemographicSummary <- function(prediction, plpData){

  if(sum(ffbase::`%in%`(plpData$covariates$covariateId, c(8507001,8532001)))==0  |
     sum(ffbase::`%in%`(plpData$covariateRef$analysisId, 3))==0){
    
    # check for gender
    if(sum(ffbase::`%in%`(plpData$covariates$covariateId, c(8507001,8532001)))!=0 ){
      #run without age
      demographicData <-  data.frame(demographicId=1:2,
                                     genId = c(rep(8507001,1), rep(8532001,1)),
                                     genGroup = c(rep('Male',1), rep('Female',1)))
      
     # missingGender <- plpData$metaData$deletedCovariateIds[plpData$metaData$deletedCovariateIds%in%c(8507,8532)]
      #if(length(missingGender)==1){
      #  otherGen <- c(8507,8532)[!c(8507,8532)%in%missingGender]
      #} else {
      #  otherGen <- 8507
      #  missingGender <- 8532
      #}
      
      genderCovariates <- plpData$covariates[ffbase::`%in%`(plpData$covariates$covariateId, c(8507001,8532001)), ]
      genderCovariates <- genderCovariates[ffbase::`%in%`(genderCovariates$rowId, prediction$rowId), ]
      genderCovariates <- ff::as.ram(genderCovariates)
      prediction$genId <- 0
      prediction$genId[match(genderCovariates$rowId, prediction$rowId)] <- genderCovariates$covariateId
      
      prediction <- merge(prediction, demographicData[, c("demographicId", "genId")], all.x = TRUE)
      
      val1 <- stats::aggregate(prediction$outcomeCount, list(prediction$demographicId),
                               function(x) c(length(x), sum(x)))
      val1 <- as.matrix(val1)
      colnames(val1) <- c('demographicId','PersonCountAtRisk','PersonCountWithOutcome')
      
      val2 <- stats::aggregate(prediction$value, list(prediction$demographicId),
                               function(x) c(mean(x), sd(x)))
      val2 <- as.matrix(val2)
      colnames(val2) <- c('demographicId','averagePredictedProbability',
                          'StDevPredictedProbability')
      
      val3 <- stats::aggregate(prediction$value, list(prediction$demographicId),
                               function(x) stats::quantile(x, probs = c(0,0.25,0.5,0.75,1))
      )
      val3 <- as.matrix(val3)
      colnames(val3) <- c('demographicId','MinPredictedProbability',
                          'P25PredictedProbability','MedianPredictedProbability',
                          'P75PredictedProbability','MaxPredictedProbability')
      
      demographicData <- merge(demographicData, val1, all.x=T)
      demographicData <- merge(demographicData, val2, all.x=T)
      demographicData <- merge(demographicData, val3, all.x=T)
      return(demographicData)
      
    }
    
    # check for age
    if(sum(ffbase::`%in%`(plpData$covariateRef$analysisId, 3))!=0 ){
      # run for age only
      demographicData <-  data.frame(demographicId=1:20,
                                     ageId=paste0(0:19,'003'),
                                     ageGroup = c('Age group: 0-4','Age group: 5-9','Age group: 10-14','Age group: 15-19',
                                                      'Age group: 20-24', 'Age group: 25-29', 'Age group: 30-34', 'Age group: 35-39',
                                                      'Age group: 40-44', 'Age group: 45-49', 'Age group: 50-54', 'Age group: 55-59',
                                                      'Age group: 60-64', 'Age group: 65-69', 'Age group: 70-74', 'Age group: 75-79',
                                                      'Age group: 80-84', 'Age group: 85-89', 'Age group: 90-94', 'Age group: 95-99')
                                     )
      
      ageCovariates <- plpData$covariates[ffbase::`%in%`(plpData$covariates$covariateId, 
                                                         plpData$covariateRef$covariateId[plpData$covariateRef$analysisId==3]), ]
      ageCovariates <- ff::as.ram(ageCovariates)[ff::as.ram(ageCovariates$rowId%in%prediction$rowId),]
      
      prediction$ageId <- 0
      prediction$ageId[match(ageCovariates$rowId, prediction$rowId)] <- ageCovariates$covariateId
      
      prediction <- merge(prediction, demographicData[, c("demographicId", "ageId")], all.x = TRUE)
      
      val1 <- stats::aggregate(prediction$outcomeCount, list(prediction$demographicId),
                               function(x) c(length(x), sum(x)))
      val1 <- as.matrix(val1)
      if(nrow(val1)==0)
        return(NULL)
      colnames(val1) <- c('demographicId','PersonCountAtRisk','PersonCountWithOutcome')
      
      val2 <- stats::aggregate(prediction$value, list(prediction$demographicId),
                               function(x) c(mean(x), sd(x)))
      val2 <- as.matrix(val2)
      colnames(val2) <- c('demographicId','averagePredictedProbability',
                          'StDevPredictedProbability')
      
      val3 <- stats::aggregate(prediction$value, list(prediction$demographicId),
                               function(x) stats::quantile(x, probs = c(0,0.25,0.5,0.75,1))
      )
      val3 <- as.matrix(val3)
      colnames(val3) <- c('demographicId','MinPredictedProbability',
                          'P25PredictedProbability','MedianPredictedProbability',
                          'P75PredictedProbability','MaxPredictedProbability')
      
      demographicData <- merge(demographicData, val1, all.x=T)
      demographicData <- merge(demographicData, val2, all.x=T)
      demographicData <- merge(demographicData, val3, all.x=T)
      return(demographicData)
    }

    return(NULL)
  } else {
    # run for age and gender

    demographicData <-  data.frame(demographicId=1:40,
                                   ageId=rep(paste0(0:19,'003'),2),
                                   ageGroup = rep(c('Age group: 0-4','Age group: 5-9','Age group: 10-14','Age group: 15-19',
                                                    'Age group: 20-24', 'Age group: 25-29', 'Age group: 30-34', 'Age group: 35-39',
                                                    'Age group: 40-44', 'Age group: 45-49', 'Age group: 50-54', 'Age group: 55-59',
                                                    'Age group: 60-64', 'Age group: 65-69', 'Age group: 70-74', 'Age group: 75-79',
                                                    'Age group: 80-84', 'Age group: 85-89', 'Age group: 90-94', 'Age group: 95-99'),2),
                                   genId = c(rep(8507001,20), rep(8532001,20)),
                                   genGroup = c(rep('Male',20), rep('Female',20)))

    genderCovariates <- plpData$covariates[ffbase::`%in%`(plpData$covariates$covariateId, c(8507001,8532001)), ]
    genderCovariates <- genderCovariates[ffbase::`%in%`(genderCovariates$rowId, prediction$rowId), ]
    genderCovariates <- ff::as.ram(genderCovariates)
    prediction$genId <- 0
    prediction$genId[match(genderCovariates$rowId, prediction$rowId)] <- genderCovariates$covariateId


    ageCovariates <- plpData$covariates[ffbase::`%in%`(plpData$covariates$covariateId, plpData$covariateRef$covariateId[plpData$covariateRef$analysisId==3]), ]
    ageCovariates <- ageCovariates[ffbase::`%in%`(ageCovariates$rowId, prediction$rowId), ]
    ageCovariates <- ff::as.ram(ageCovariates)
    prediction$ageId <- 0
    prediction$ageId[match(ageCovariates$rowId, prediction$rowId)] <- ageCovariates$covariateId


    prediction <- merge(prediction, demographicData[, c("demographicId", "ageId", "genId")], all.x = TRUE)

    val1 <- stats::aggregate(prediction$outcomeCount, list(prediction$demographicId),
                             function(x) c(length(x), sum(x)))
    val1 <- as.matrix(val1)
    colnames(val1) <- c('demographicId','PersonCountAtRisk','PersonCountWithOutcome')

    val2 <- stats::aggregate(prediction$value, list(prediction$demographicId),
                             function(x) c(mean(x), sd(x)))
    val2 <- as.matrix(val2)
    colnames(val2) <- c('demographicId','averagePredictedProbability',
                        'StDevPredictedProbability')

    val3 <- stats::aggregate(prediction$value, list(prediction$demographicId),
                             function(x) stats::quantile(x, probs = c(0,0.25,0.5,0.75,1))
    )
    val3 <- as.matrix(val3)
    colnames(val3) <- c('demographicId','MinPredictedProbability',
                        'P25PredictedProbability','MedianPredictedProbability',
                        'P75PredictedProbability','MaxPredictedProbability')

    demographicData <- merge(demographicData, val1, all.x=T)
    demographicData <- merge(demographicData, val2, all.x=T)
    demographicData <- merge(demographicData, val3, all.x=T)
    return(demographicData)
  }

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
