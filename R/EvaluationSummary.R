getEvaluationStatistics <- function(
  prediction, 
  predictionType,
  typeColumn = 'evaluation'
){
  
  evaluation <- do.call(
    what = paste0('getEvaluationStatistics_', predictionType), 
    args = list(
      prediction = prediction, 
      evalColumn = typeColumn,
      timepoint = attr(prediction, 'metaData')$timepoint
    )
  )
  
  return(evaluation)
}

# get all the standard metrics for a given evaluation type
# function to calculate evaluation summary data.frame with columns: evaluation, metric, value
getEvaluationStatistics_binary <- function(prediction, evalColumn, ...){
  
  result <- c()
  evalTypes <- unique(as.data.frame(prediction)[,evalColumn])

  for(evalType in evalTypes){
    
    predictionOfInterest <- prediction %>% dplyr::filter(.data[[evalColumn]] == evalType)
    
    result <- rbind(
      result,
      c(evalType,  'populationSize', nrow(predictionOfInterest)),
      c(evalType,  'outcomeCount', sum(predictionOfInterest$outcomeCount))
    )
    
    # auc
    ParallelLogger::logInfo(paste0('Calculating Performance for ', evalType))
    ParallelLogger::logInfo('=============')
    
    ParallelLogger::logTrace('Calculating AUC')
    auc <- computeAuc(predictionOfInterest, confidenceInterval = T)
    
    result <- rbind(
      result, 
      c(evalType, 'AUROC', auc[1]),
      c(evalType, '95% lower AUROC', auc[2]),
      c(evalType, '95% upper AUROC', auc[3])
    )
    ParallelLogger::logInfo(sprintf('%-20s%.2f', 'AUC', auc[1]*100))
    ParallelLogger::logInfo(sprintf('%-20s%.2f', '95% lower AUC: ', auc[2]*100))
    ParallelLogger::logInfo(sprintf('%-20s%.2f', '95% upper AUC: ', auc[3]*100))
    
    # auprc
    ParallelLogger::logTrace('Calculating AUPRC')
    positive <- predictionOfInterest$value[predictionOfInterest$outcomeCount == 1]
    negative <- predictionOfInterest$value[predictionOfInterest$outcomeCount == 0]
    pr <- PRROC::pr.curve(scores.class0 = positive, scores.class1 = negative)
    auprc <- pr$auc.integral
    result <- rbind(
      result, 
      c(evalType, 'AUPRC', auprc)
    )
    ParallelLogger::logInfo(sprintf('%-20s%.2f', 'AUPRC: ', auprc*100))
    
    # brier scores-returnss; brier, brierScaled
    ParallelLogger::logTrace('Calculating Brier Score')
    brier <- brierScore(predictionOfInterest)
    result <- rbind(
      result, 
      c(evalType, 'brier score', brier$brier),
      c(evalType, 'brier score scaled', brier$brierScaled)
    )
    ParallelLogger::logInfo(sprintf('%-20s%.2f', 'Brier: ', brier$brier))
    
    # using rms::val.prob
    indValProb <- predictionOfInterest$value>0 & predictionOfInterest$value < 1
    valProb <- tryCatch(rms::val.prob(predictionOfInterest$value[indValProb], predictionOfInterest$outcomeCount[indValProb]), 
      error = function(e){ParallelLogger::logInfo(e); return(c(Eavg = 0, 
        E90 = 0, 
        Emax = 0))})
    result <- rbind(
      result, 
      c(evalType, 'Eavg', valProb['Eavg']),
      c(evalType, 'E90', valProb['E90']),
      c(evalType, 'Emax', valProb['Emax'])
    )
    ParallelLogger::logInfo(sprintf('%-20s%.2f', 'Eavg: ', round(valProb['Eavg'], digits = 4)))
    
    
    # Removing for now as too slow...
    #ici <- ici(prediction)
    #result <- rbind(
    #  result, 
    #  c(evalType, 'ici', ifelse(is.null(ici), 'NA', ici))
    #)
    #ParallelLogger::logInfo(paste0('ICI ', round(ifelse(is.null(ici), 'NA', ici), digits = 4)))
    
    
    # calibration linear fit- returns gradient, intercept
    ParallelLogger::logTrace('Calculating Calibration-in-large')
    calinlarge <- calibrationInLarge(predictionOfInterest)
    result <- rbind(
      result, 
      c(evalType, 'calibrationInLarge mean prediction', calinlarge$meanPredictionRisk),
      c(evalType, 'calibrationInLarge observed risk', calinlarge$observedRisk)
    )
    ParallelLogger::logInfo(paste0('Calibration in large- Mean predicted risk ', round(calinlarge$meanPredictionRisk, digits = 4), ' : observed risk ',round(calinlarge$observedRisk, digits = 4)))
    
    calinlargeInt <- calibrationInLargeIntercept(predictionOfInterest)
    result <- rbind(
      result, 
      c(evalType, 'calibrationInLarge intercept', calinlargeInt)
    )
    ParallelLogger::logInfo(paste0('Calibration in large- Intercept ', round(calinlargeInt, digits = 4)))
    
    
    ParallelLogger::logTrace('Calculating Weak Calibration')
    weakCal <- calibrationWeak(predictionOfInterest)
    result <- rbind(
      result, 
      c(evalType, 'weak calibration intercept', weakCal$intercept),
      c(evalType, 'weak calibration gradient', weakCal$gradient)
    )
    ParallelLogger::logInfo(paste0('Weak calibration intercept: ', 
      round(weakCal$intercept, digits = 4), 
      ' - gradient:',round(weakCal$gradient, digits = 4)))
    
    ParallelLogger::logTrace('Calculating Hosmer-Lemeshow Calibration Line')
    calLine10 <- calibrationLine(predictionOfInterest, numberOfStrata = 10)
    result <- rbind(
      result, 
      c(evalType, 'Hosmer-Lemeshow calibration intercept', calLine10$lm[1]),
      c(evalType, 'Hosmer-Lemeshow calibration gradient', calLine10$lm[2])
    )
    ParallelLogger::logInfo(sprintf('%-20s%.2f%-20s%.2f', 'Hosmer-Lemeshow calibration gradient: ', calLine10$lm[2], ' intercept: ',calLine10$lm[1]))
    
    # Extra: Average Precision
    aveP.val <- averagePrecision(predictionOfInterest)
    result <- rbind(
      result, 
      c(evalType, 'Average Precision', aveP.val)
    )
    ParallelLogger::logInfo(sprintf('%-20s%.2f', 'Average Precision: ', aveP.val))
    
  }
  
  result <- as.data.frame(result)
  colnames(result) <- c('evaluation','metric','value')
  
  return(result)
}

getEvaluationStatistics_survival <- function(prediction, evalColumn, timepoint, ...){ 
  
  if(is.null(prediction$survivalTime)){
    stop('No survival time column present')
  }
  
  result <- c()
  evalTypes <- unique(as.data.frame(prediction)[,evalColumn])
  
  for(evalType in evalTypes){
    
    predictionOfInterest <- prediction %>% dplyr::filter(.data[[evalColumn]] == evalType)
    
    result <- rbind(
      result,
      c(evalType, timepoint, 'populationSize', nrow(predictionOfInterest)),
      c(evalType, timepoint, 'outcomeCount', sum(predictionOfInterest$outcomeCount))
    )
    
    #============================
    
    ##timepoint <- attr(prediction, 'metaData')$timepoint #max(prediction$survivalTime)
    ParallelLogger::logInfo(paste0('Evaluating survival model at time: ', timepoint, ' days'))
    
    t <- predictionOfInterest$survivalTime
    y <- ifelse(predictionOfInterest$outcomeCount > 0, 1, 0)
    
    S <- survival::Surv(t, y) 
    p <- predictionOfInterest$value
    
    out <- tryCatch({summary(survival::survfit(survival::Surv(t, y) ~ 1), times = timepoint)},
      error = function(e){ParallelLogger::logError(e); return(NULL)})
    survVal <- 1-out$surv
    meanSurvivalTime <- mean(t)
    
    result <- rbind(
      result, 
      c(evalType, timepoint, 'Survival', survVal),
      c(evalType, timepoint, 'Mean survival time', meanSurvivalTime)
    )
    
    # add c-stat
    ParallelLogger::logTrace('Calculating C-statistic')
    
    conc <- tryCatch({survival::concordance(S~p, reverse=TRUE)},
      error = function(e){ParallelLogger::logError(e); return(NULL)})
    cStatistic <- 0
    cStatistic_l95CI <- 0
    cStatistic_u95CI <- 0
    
    if(!is.null(conc)){
      cStatistic <- round(conc$concordance,5)
      c.se <- sqrt(conc$var)
      cStatistic_l95CI <- round(conc$concordance+stats::qnorm(.025)*c.se,3)
      cStatistic_u95CI <- round(conc$concordance+stats::qnorm(.975)*c.se,3)
    }
    result <- rbind(
      result, 
      c(evalType, timepoint, 'C-statistic', cStatistic),
      c(evalType, timepoint, 'C-statistic lower 95% CI', cStatistic_l95CI),
      c(evalType, timepoint, 'C-statistic upper 95% CI', cStatistic_u95CI)
    )
    ParallelLogger::logInfo(paste0('C-statistic: ',cStatistic, ' (',cStatistic_l95CI ,'-', cStatistic_u95CI ,')'))
    
    
    # add e-stat
    w <- tryCatch(
      {
        rms::val.surv(
          est.surv = 1-p, 
          S = S,
          u = timepoint, 
          fun = function(pr)log(-log(pr))
        )
      },
      error = function(e){ParallelLogger::logError(e); return(NULL)}
    )
    
    eStatistic <- -1
    eStatistic90 <- -1
    if(!is.null(w)){
      eStatistic<-mean(abs(w$actual - w$p))
      eStatistic90<-stats::quantile((abs(w$actual - w$p)),0.9)
      
    }
    result <- rbind(
      result, 
      c(evalType, timepoint, 'E-statistic', eStatistic),
      c(evalType, timepoint, 'E-statistic 90%', eStatistic90)
    )
    ParallelLogger::logInfo(paste0('E-statistic: ',eStatistic))
    ParallelLogger::logInfo(paste0('E-statistic 90%: ',eStatistic90))
  }
  
  result <- as.data.frame(result)
  colnames(result) <- c('evaluation','timepoint','metric','value')
  
  return(result)
}



#==================================
# Fucntions for the summary
#==================================

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
  if (attr(prediction, "metaData")$modelType != "binary")
    stop("Computing AUC is only implemented for binary classification models")
  
  if (confidenceInterval) {
    return(aucWithCi(prediction = prediction$value, truth = prediction$outcomeCount))
  } else {
    return(aucWithoutCi(prediction = prediction$value, truth = prediction$outcomeCount))
  }
}

aucWithCi <- function(prediction, truth){
  auc <- pROC::auc(as.factor(truth), prediction, direction="<")
  aucci <-pROC::ci(auc)
  return(data.frame(auc = aucci[2], auc_lb95ci = aucci[1], auc_ub95ci = aucci[3]))
}

aucWithoutCi <- function(prediction, truth){
  auc <- pROC::auc(as.factor(truth), prediction, direction="<")
  return(as.double(auc))
}


#' brierScore
#'
#' @details
#' Calculates the brierScore from prediction object
#'
#' @param prediction            A prediction object
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
#' @param prediction            A prediction object
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
#' @param prediction            A prediction object
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

calibrationInLargeIntercept <- function(prediction){
  
  #do invert of log function:
  # log(p/(1-p))
  
  # edit the 0 and 1 values
  prediction$value[prediction$value==0] <- 0.000000000000001
  prediction$value[prediction$value==1] <- 1-0.000000000000001
  
  inverseLog <- log(prediction$value/(1-prediction$value))
  y <- ifelse(prediction$outcomeCount>0, 1, 0) 
  
  intercept <- suppressWarnings(stats::glm(y ~ offset(1*inverseLog), family = 'binomial'))
  intercept <- intercept$coefficients[1]
  
  return(intercept)
}


calibrationWeak <- function(prediction){
  
  #do invert of log function:
  # log(p/(1-p))
  
  # edit the 0 and 1 values
  prediction$value[prediction$value==0] <- 0.000000000000001
  prediction$value[prediction$value==1] <- 1-0.000000000000001
  
  inverseLog <- log(prediction$value/(1-prediction$value))
  y <- ifelse(prediction$outcomeCount>0, 1, 0) 
  
  #intercept <- suppressWarnings(stats::glm(y ~ offset(1*inverseLog), family = 'binomial'))
  #intercept <- intercept$coefficients[1]
  #gradient <- suppressWarnings(stats::glm(y ~ inverseLog+0, family = 'binomial',
  #                       offset = rep(intercept,length(inverseLog))))
  #gradient <- gradient$coefficients[1]
  
  vals <- suppressWarnings(stats::glm(y ~ inverseLog, family = 'binomial'))
  
  result <- data.frame(intercept = vals$coefficients[1], 
    gradient = vals$coefficients[2])
  
  return(result)
}

#' Calculate the Integrated Calibration Information from Austin and Steyerberg
#' https://onlinelibrary.wiley.com/doi/full/10.1002/sim.8281
#' 
#' @details
#' Calculate the Integrated Calibration Information
#'
#' @param prediction         the prediction object found in the plpResult object
#' 
#' @return
#' Integrated Calibration Information
#'
#' @export
ici <- function(prediction){
  #remove na
  if(sum(!is.finite(prediction$value))>0){
    prediction <- prediction[is.finite(prediction$value),]
  }
  loess.calibrate <- tryCatch({stats::loess(prediction$outcomeCount ~ prediction$value)}, 
    warning = function(w){ParallelLogger::logInfo(w)},
    error = function(e){ParallelLogger::logInfo(e); return(NULL)}
  )
  if(!is.null(loess.calibrate)){
    # Estimate loess-based smoothed calibration curve
    P.calibrate <- tryCatch({stats::predict(loess.calibrate, newdata = prediction$value)}, 
      warning = function(w){ParallelLogger::logInfo(w)},
      error = function(e){ParallelLogger::logInfo(e); return(NULL)}
    )
    if(!is.null(P.calibrate)){
      # This is the point on the loess calibration curve corresponding to a given predicted probability.
      ICI <- mean (abs(P.calibrate - prediction$value))
      return(ICI)
    }
  }
  return(-1)
}
