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
  OptimalThresholdTrain <- 0.5

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
    #result <- rbind(
      #result, 
      #c(evalType, 'brier score', brier$brier),
     # c(evalType, 'brier score scaled', brier$brierScaled)
    #)
    ParallelLogger::logInfo(sprintf('%-20s%.2f', 'Brier: ', brier))

    
    # using rms::val.prob
    indValProb <- predictionOfInterest$value>0 & predictionOfInterest$value < 1
    valProb <- tryCatch(
      calculateEStatisticsBinary(prediction = predictionOfInterest[indValProb, ]),
      error = function(e) {
        ParallelLogger::logInfo(e); return(
          c(
            Eavg = 0, 
            E90 = 0, 
            Emax = 0
          )
        )
      }
    )
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
    
    # Extra: Accuracy
    acc.val <- accuracyScore(predictionOfInterest, optimalThreshold = OptimalThresholdTrain)
    result <- rbind(
      result, 
      c(evalType, 'Accuracy', acc.val)
    )
    ParallelLogger::logInfo(sprintf('%-20s%.2f', 'Accuracy: ', acc.val))
    
    # Extra: Precision
    prec.val <- precisionScore(predictionOfInterest, optimalThreshold = OptimalThresholdTrain)
    result <- rbind(
      result, 
      c(evalType, 'Precision', prec.val)
    )
    ParallelLogger::logInfo(sprintf('%-20s%.2f', 'precision: ', prec.val))
    
    # Extra: Recall
    rec.val <- recallScore(predictionOfInterest, optimalThreshold = OptimalThresholdTrain)
    result <- rbind(
      result, 
      c(evalType, 'recall', rec.val)
    )
    ParallelLogger::logInfo(sprintf('%-20s%.2f', 'recall: ', rec.val))
    
    # Extra: f1
    f1.val <- f1Scores(predictionOfInterest, optimalThreshold = OptimalThresholdTrain)
    result <- rbind(
      result, 
      c(evalType, 'f1-score', f1.val)
    )
    ParallelLogger::logInfo(sprintf('%-20s%.2f', 'f1-score: ', f1.val))
    
    
    # Extra: gmean
    gmean.val <- gMeanScore(predictionOfInterest, optimalThreshold = OptimalThresholdTrain)
    result <- rbind(
      result, 
      c(evalType, 'gMean score', gmean.val)
    )
    ParallelLogger::logInfo(sprintf('%-20s%.2f', 'gmean score: ', gmean.val))
    
    # Extra: mcc
    mcc.val <- mccScore(predictionOfInterest, optimalThreshold = OptimalThresholdTrain)
    result <- rbind(
      result, 
      c(evalType, 'mcc score', mcc.val)
    )
    ParallelLogger::logInfo(sprintf('%-20s%.2f', 'mcc score: ', mcc.val))
    
    # Extra: log-loss
    ll.val <- logLossScore(predictionOfInterest)
    result <- rbind(
      result, 
      c(evalType, 'log-loss score', ll.val)
    )
    ParallelLogger::logInfo(sprintf('%-20s%.2f', 'log-loss score: ', ll.val))
    
    # Extra: specificity
    speci.val <- specificityScore(predictionOfInterest, optimalThreshold = OptimalThresholdTrain)
    result <- rbind(
      result, 
      c(evalType, 'specificity', speci.val)
    )
    ParallelLogger::logInfo(sprintf('%-20s%.2f', 'specificity: ', speci.val))
    
    # Extra: balanced accuracy
    bacc.val <- balancedAccuracyScore(predictionOfInterest, optimalThreshold = OptimalThresholdTrain)
    result <- rbind(
      result, 
      c(evalType, 'balanced accuracy', bacc.val)
    )
    ParallelLogger::logInfo(sprintf('%-20s%.2f', 'balanced accuracy: ', bacc.val))
    
    # Extra: kappa score
    kap.val <- kappaScore(predictionOfInterest, optimalThreshold = OptimalThresholdTrain)
    result <- rbind(
      result, 
      c(evalType, 'kappa score', kap.val)
    )
    ParallelLogger::logInfo(sprintf('%-20s%.2f', 'kappa score: ', kap.val))
    
    # Extra: hamming loss
    hloss.val <- hammingLossScore(predictionOfInterest, optimalThreshold = OptimalThresholdTrain)
    result <- rbind(
      result, 
      c(evalType, 'hamming log-loss', hloss.val)
    )
    ParallelLogger::logInfo(sprintf('%-20s%.2f', 'hamming log-loss: ', hloss.val))
    
    # Extra: f2 score
    f2.val <- f2Score(predictionOfInterest, optimalThreshold = OptimalThresholdTrain)
    result <- rbind(
      result, 
      c(evalType, 'f2-score', f2.val)
    )
    ParallelLogger::logInfo(sprintf('%-20s%.2f', 'f2-score: ', f2.val))
    
    # Extra: rmse
    rmse.val <- rmseScore(predictionOfInterest)
    result <- rbind(
      result, 
      c(evalType, 'rmse', rmse.val)
    )
    ParallelLogger::logInfo(sprintf('%-20s%.2f', 'rmse: ', rmse.val))
    
    # Extra: mLogLoss
    mll.val <- mloglossScore(predictionOfInterest)
    result <- rbind(
      result, 
      c(evalType, 'mLogLoss', mll.val)
    )
    ParallelLogger::logInfo(sprintf('%-20s%.2f', 'mLogLoss: ', mll.val))
    
    # Extra: error
    err.val <- errorScore(predictionOfInterest, optimalThreshold = OptimalThresholdTrain)
    result <- rbind(
      result, 
      c(evalType, 'error', err.val)
    )
    ParallelLogger::logInfo(sprintf('%-20s%.2f', 'error: ', err.val))
    
    # Extra: mae
    mae.val <- maeScore(predictionOfInterest)
    result <- rbind(
      result, 
      c(evalType, 'mae', mae.val)
    )
    ParallelLogger::logInfo(sprintf('%-20s%.2f', 'mae: ', mae.val))
    
    # Extra: threshold used
    result <- rbind(
      result, 
      c(evalType, 'optimized threshold', OptimalThresholdTrain)
    )
    ParallelLogger::logInfo(sprintf('%-20s%.2f', 'optimized threshold: ', OptimalThresholdTrain))
    
    # Extra: mae
    Wacc.val <- weightedAccuracyScore(predictionOfInterest, optimalThreshold = OptimalThresholdTrain)
    result <- rbind(
      result, 
      c(evalType, 'weighted acc', Wacc.val)
    )
    ParallelLogger::logInfo(sprintf('%-20s%.2f', 'Weighted acc: ', Wacc.val))
    
    # Extra: confusion matrix
    cm.val <- confusionMatrix(predictionOfInterest, optimalThreshold = OptimalThresholdTrain)
    result <- rbind(
      result, 
      c(evalType, 'TP', cm.val[1])
    )
    result <- rbind(
      result, 
      c(evalType, 'FN', cm.val[2])
    )
    result <- rbind(
      result, 
      c(evalType, 'FP', cm.val[3])
    )
    result <- rbind(
      result, 
      c(evalType, 'TN', cm.val[4])
    )
    ParallelLogger::logInfo(sprintf('%-20s%.2f', 'TP: ', cm.val[1]))
    ParallelLogger::logInfo(sprintf('%-20s%.2f', 'FN: ', cm.val[2]))
    ParallelLogger::logInfo(sprintf('%-20s%.2f', 'FP: ', cm.val[3]))
    ParallelLogger::logInfo(sprintf('%-20s%.2f', 'TN: ', cm.val[4]))
    
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

    .validateSurvival <- function(p, S, timepoint) {
      estimatedSurvival <- 1 - p
      notMissing <- !is.na(estimatedSurvival + S[, 1] + S[, 2])
      estimatedSurvival   <- estimatedSurvival[notMissing]
      S <- S[notMissing, ]
      .curtail <- function(x) pmin(.9999, pmax(x, .0001))
      f <- polspline::hare(
        S[, 1],
        S[, 2],
        log(-log((.curtail(estimatedSurvival)))),
        maxdim = 5
      )
      actual <- 1 - polspline::phare(timepoint, log(-log(estimatedSurvival)), f)

      return(
        list(
          actual = actual,
          estimatedSurvival = estimatedSurvival
        )
      )
    }

    w <- tryCatch(
    {
      .validateSurvival(
        p = p,
        S = S,
        timepoint = timepoint
      )
    },
    error = function(e){ParallelLogger::logError(e); return(NULL)}
    )

    eStatistic <- eStatistic90 <- -1
    if (!is.null(w)) {
      eStatistic <- mean(abs(w$actual - w$estimatedSurvival)) 
      eStatistic90 <- stats::quantile(abs(w$actual - w$estimatedSurvival),
                                      probs = .9, na.rm = TRUE)
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


calculateEStatisticsBinary <- function(prediction) {
  risk <- prediction$value
  outcome <- prediction$outcomeCount
  notna <- ! is.na(risk + outcome)
  risk <- risk[notna]
  outcome <- outcome[notna]
  smoothFit <- stats::lowess(risk, outcome, iter = 0)
  smoothCalibration <- stats::approx(smoothFit, xout = risk, ties = mean)$y
  distance <- abs(risk - smoothCalibration)
  eavg <- mean(abs(risk - smoothCalibration))
  emax <- max(distance)
  e90 <- stats::quantile(distance, probs = .9)
  names(e90) <- NULL
  return(
    c(
      Eavg = eavg,
      E90 = e90,
      Emax = emax
    )
  )
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
computeAuc <- function(prediction = NULL, dtrain = NULL, confidenceInterval = FALSE, framework = 'xgboost') {
  if (!is.null(dtrain)) {
    if (framework == 'xgboost') {
      labels <- xgboost::getinfo(dtrain, "label")
    } else if (framework == 'lightgbm'){
      labels <- lightgbm::get_field(dtrain, 'label')
    } else{
      stop('unknown framework specified')
    }

    prediction <- data.frame(value = prediction, outcomeCount = labels)
  }
  
  if (is.null(prediction)) {
    stop("Either 'prediction' or 'dtrain' must be provided.")
  }
  
  # Check if the model is binary
  if (length(unique(prediction$outcomeCount)) != 2) {
    stop("Computing AUC is only implemented for binary classification models")
  }
  
  if (confidenceInterval) {
    return(aucWithCi(prediction = prediction$value, truth = prediction$outcomeCount))
  } else {
    if (is.null(dtrain)){
      return(aucWithoutCi(prediction = prediction$value, truth = prediction$outcomeCount))
    } else {
      return(aucWithoutCi(prediction = prediction$value, truth = prediction$outcomeCount, bgxboost = T, framework = framework))
    }
      
  }
}

aucWithCi <- function(prediction, truth) {
  auc <- pROC::auc(as.factor(truth), prediction, direction="<", quiet=TRUE)
  aucci <- pROC::ci(auc)
  return(data.frame(auc = aucci[2], auc_lb95ci = aucci[1], auc_ub95ci = aucci[3]))
}

aucWithoutCi <- function(prediction, truth, bgxboost = F, framework = 'xgboost') {
  auc <- pROC::auc(as.factor(truth), prediction, direction="<", quiet=TRUE)
  return(if (bgxboost == T & framework == 'lightgbm') 
    list( name = 'auc', value = auc, higher_better = TRUE) 
    else if (bgxboost == T & framework == 'xgboost') 
      list(metric = 'auc', value = auc) else auc) #minus auc for xgboost for because it standard minimizes
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
brierScore <- function(prediction = NULL, dtrain = NULL, framework = 'xgboost') {
  if (!is.null(dtrain)) {
    # Extract labels based on the specified framework
    if (framework == 'xgboost'){
      labels <- xgboost::getinfo(dtrain, "label")
    } else if (framework == 'lightgbm'){
      labels <- lightgbm::get_field(dtrain, "label")
    } else {
      stop("Unknown framework specified")
    }
    
    prediction <- data.frame(value = prediction, outcomeCount = labels)
  }
  
  if (!is.null(prediction)) {
    # Compute the Brier score
    brier <- mean((prediction$outcomeCount - prediction$value)^2)
    
    # Calculate Brier max and Brier scaled
    brierMax <- mean(prediction$value) * (1 - mean(prediction$value))
    brierScaled <- 1 - brier / brierMax
    
    # Return the results
    if (!is.null(dtrain)) {
      if (framework == 'lightgbm') {
        return(list(name = "brierScore", value = brier, higher_better = FALSE))
      } else {
        return(list(metric = "brierScore", value = brier))
      }
    } else {
      return(list(brier = brier))
    }
  }
  
  stop("Either 'prediction' or 'dtrain' must be provided.")
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
#' 
# averagePrecision <- function(prediction){
#   lab.order <- prediction$outcomeCount[order(-prediction$value)]
#   n <- nrow(prediction)
#   P <- sum(prediction$outcomeCount>0)
#   val <- rep(0, n)
#   val[lab.order>0] <- 1:P
#   return(sum(val/(1:n))/P)
# }

averagePrecision <- function(prediction = NULL, dtrain = NULL, framework = 'xgboost') {
  if (!is.null(dtrain)) {
    if (framework == 'xgboost'){
      labels <- xgboost::getinfo(dtrain, "label")
    } else if (framework == 'lightgbm'){
      labels <- lightgbm::get_field(dtrain, "label")
    } else {
      stop("Unknown framework specified")
    }
    
    prediction <- data.frame(value = prediction, outcomeCount = labels)
  }
  
  if (!is.null(prediction)) {
    # Sort the labels by the predicted values in descending order
    lab.order <- prediction$outcomeCount[order(-prediction$value)]
    n <- nrow(prediction)
    P <- sum(prediction$outcomeCount > 0)  # Total positives
    
    # Initialize a vector to store precision values
    val <- rep(0, n)
    val[lab.order > 0] <- 1:sum(lab.order > 0)  # For positive class, use non-zero indices
    
    # Compute the average precision
    average_precision <- sum(val / (1:n)) / P
    
    if (!is.null(dtrain)) {
      if (framework == 'lightgbm') {
        return(list(name = "averagePrecision", value = average_precision, higher_better = TRUE))
      } else {
        return(list(metric = "averagePrecision", value = average_precision)) 
      }
    } else {
      return(average_precision)
    }
  }
  
  stop("Either 'prediction' or 'dtrain' must be provided.")
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

accuracyScore <- function(prediction = NULL, dtrain = NULL, framework = 'xgboost', optimalThreshold = NULL) {
  # When called by XGBoost (dtrain is provided)
  if (!is.null(dtrain)) {
    if (framework == 'xgboost'){
      labels <- xgboost::getinfo(dtrain, "label")
    } else if (framework == 'lightgbm'){
      labels <- lightgbm::get_field(dtrain, "label")
    } else{
      stop("Unknown framework specified")
    }
    
    if (is.null(optimalThreshold)){
      optimalThreshold <- optimizeThreshold(prediction, labels)
    }
    
    predictedClasses <- ifelse(prediction > optimalThreshold, 1, 0)
    accuracy <- mean(predictedClasses == labels)
    
    if (framework == 'lightgbm'){
      return(list(name = 'accuracy', value = accuracy, higher_better = TRUE))
    } else{
      return(list(metric = "accuracy", value = accuracy))
    }
  }
  
  #Regular use case (prediction data frame is provided)
  if (!is.null(prediction)) {
    if (is.null(optimalThreshold)){
      optimalThreshold <- optimizeThreshold(prediction$value, prediction$outcomeCount)
    }
    predictedClasses <- ifelse(prediction$value > optimalThreshold, 1, 0)
    outcomeCount <- prediction$outcomeCount
    accuracy <- mean(predictedClasses == outcomeCount)

    return(accuracy)
  }
  
  # If no valid input is provided, return an error
  stop("Either 'prediction' or 'dtrain' must be provided.")
}


weightedAccuracyScore <- function(prediction = NULL, dtrain = NULL, framework = 'xgboost', optimalThreshold = NULL) {
  # When called by XGBoost (dtrain is provided)
  if (!is.null(dtrain)) {
    if (framework == 'xgboost'){
      labels <- xgboost::getinfo(dtrain, "label")
    } else if (framework == 'lightgbm'){
      labels <- lightgbm::get_field(dtrain, "label")
    } else {
      stop("Unknown framework specified")
    }
    if (is.null(optimalThreshold)){
      optimalThreshold <- optimizeThreshold(prediction, labels)
    }
    predictedClasses <- ifelse(prediction > optimalThreshold, 1, 0)
    
    # Calculate the weighted accuracy
    classCounts <- table(labels)
    correctPredictions <- table(labels[predictedClasses == labels])
    accuracyPerClass <- correctPredictions / classCounts
    weights <- classCounts / sum(classCounts)
    weightedAccuracy <- sum(accuracyPerClass * weights, na.rm = TRUE)
    
    if (framework == 'lightgbm'){
      return(list(name = 'weighted_accuracy', value = weightedAccuracy, higher_better = TRUE))
    } else {
      return(list(metric = "weighted_accuracy", value = weightedAccuracy)) 
    }
  }
  
  # Regular use case (prediction data frame is provided)
  if (!is.null(prediction)) {
    if (is.null(optimalThreshold)){
      optimalThreshold <- optimizeThreshold(prediction$value, prediction$outcomeCount)
    }
    predictedClasses <- ifelse(prediction$value > optimalThreshold, 1, 0)
    outcomeCount <- prediction$outcomeCount
    
    # Calculate the weighted accuracy
    classCounts <- table(outcomeCount)
    correctPredictions <- table(outcomeCount[predictedClasses == outcomeCount])
    accuracyPerClass <- correctPredictions / classCounts
    weights <- classCounts / sum(classCounts)
    weightedAccuracy <- sum(accuracyPerClass * weights, na.rm = TRUE)
    
    return(weightedAccuracy)
  }
  
  # If no valid input is provided, return an error
  stop("Either 'prediction' or 'dtrain' must be provided.")
}


#precision
precisionScore <- function(prediction = NULL, dtrain = NULL, framework = 'xgboost', optimalThreshold = NULL) {
  if (!is.null(dtrain)) {
    if (framework == 'xgboost'){
      labels <- xgboost::getinfo(dtrain, "label")
    } else if (framework == 'lightgbm'){
      labels <- lightgbm::get_field(dtrain, "label")
    } else{
      stop("Unknown framework specified")
    }
    
    if (is.null(optimalThreshold)){
      optimalThreshold <- optimizeThreshold(prediction, labels)
    }
    predictedClasses <- ifelse(prediction > optimalThreshold, 1, 0)
    tp <- sum(predictedClasses == 1 & labels == 1)
    fp <- sum(predictedClasses == 1 & labels == 0)
    precision <- if ((tp + fp) == 0) 0 else tp / (tp + fp)
    if (framework == 'lightgbm'){
      return(list(name = 'precision', value = precision, higher_better = TRUE))
    } else{
      return(list(metric = "precision", value = precision))
    }
  }
  
  if (!is.null(prediction)) {
    if (is.null(optimalThreshold)){
      optimalThreshold <- optimizeThreshold(prediction$value, prediction$outcomeCount)
    }
    predictedClasses <- ifelse(prediction$value > optimalThreshold, 1, 0)
    tp <- sum(predictedClasses == 1 & prediction$outcomeCount == 1)
    fp <- sum(predictedClasses == 1 & prediction$outcomeCount == 0)
    precision <- if ((tp + fp) == 0) 0 else tp / (tp + fp)
    return(precision)
  }
  
  stop("Either 'prediction' or 'dtrain' must be provided.")
}

#recall
recallScore <- function(prediction = NULL, dtrain = NULL, framework = 'xgboost', optimalThreshold = NULL) {
  if (!is.null(dtrain)) {
    if (framework == 'xgboost'){
      labels <- xgboost::getinfo(dtrain, "label")
    } else if (framework == 'lightgbm'){
      labels <- lightgbm::get_field(dtrain, "label")
    } else{
      stop("Unknown framework specified")
    }
    if (is.null(optimalThreshold)){
      optimalThreshold <- optimizeThreshold(prediction, labels)
    }
    predictedClasses <- ifelse(prediction > optimalThreshold, 1, 0)
    tp <- sum(predictedClasses == 1 & labels == 1)
    fn <- sum(predictedClasses == 0 & labels == 1)
    recall <- if ((tp + fn) == 0) 0 else tp / (tp + fn)
    if (framework == 'lightgbm'){
      return(list(name = 'recall', value = recall, higher_better = TRUE))
    } else{
      return(list(metric = "recall", value = recall))
    }
  }
  
  if (!is.null(prediction)) {
    if (is.null(optimalThreshold)){
      optimalThreshold <- optimizeThreshold(prediction$value, prediction$outcomeCount)
    }
    predictedClasses <- ifelse(prediction$value > optimalThreshold, 1, 0)
    tp <- sum(predictedClasses == 1 & prediction$outcomeCount == 1)
    fn <- sum(predictedClasses == 0 & prediction$outcomeCount == 1)
    recall <- if ((tp + fn) == 0) 0 else tp / (tp + fn)
    return(recall)
  }
  
  stop("Either 'prediction' or 'dtrain' must be provided.")
}


# F1 Score
f1Scores <- function(prediction = NULL, dtrain = NULL, framework = 'xgboost', optimalThreshold = NULL) {
  if (!is.null(dtrain)) {
    if (framework == 'xgboost'){
      labels <- xgboost::getinfo(dtrain, "label")
    } else if (framework == 'lightgbm'){
      labels <- lightgbm::get_field(dtrain, "label")
    } else{
      stop("Unknown framework specified")
    }
    if (is.null(optimalThreshold)){
      optimalThreshold <- optimizeThreshold(prediction, labels)
    }
    predictedClasses <- ifelse(prediction > optimalThreshold, 1, 0)
    tp <- sum(predictedClasses == 1 & labels == 1)
    fp <- sum(predictedClasses == 1 & labels == 0)
    fn <- sum(predictedClasses == 0 & labels == 1)
    precision <- if ((tp + fp) == 0) 0 else tp / (tp + fp)
    recall <- if ((tp + fn) == 0) 0 else tp / (tp + fn)
    f1 <- if ((precision + recall) == 0) 0 else 2 * (precision * recall) / (precision + recall)
    if (framework == 'lightgbm'){
      return(list(name = 'f1score', value = f1, higher_better = TRUE))
    } else{
      return(list(metric = "f1score", value = f1)) 
    }
  }
  
  if (!is.null(prediction)) {
    if (is.null(optimalThreshold)){
      precision <- precisionScore(prediction)
      recall <- recallScore(prediction)
      f1 <- if ((precision + recall) == 0) 0 else 2 * (precision * recall) / (precision + recall)
    }else{
      precision <- precisionScore(prediction, optimalThreshold = optimalThreshold)
      recall <- recallScore(prediction, optimalThreshold = optimalThreshold)
      f1 <- if ((precision + recall) == 0) 0 else 2 * (precision * recall) / (precision + recall)
    }
    return(f1)
  }
  
  stop("Either 'prediction' or 'dtrain' must be provided.")
}

# Log Loss
logLossScore <- function(prediction = NULL, dtrain = NULL, framework = 'xgboost') {
  epsilon <- 1e-15  # Small value to clip probabilities
  
  if (!is.null(dtrain)) {
    if (framework == 'xgboost'){
      labels <- xgboost::getinfo(dtrain, "label")
    } else if (framework == 'lightgbm'){
      labels <- lightgbm::get_field(dtrain, "label")
    } else{
      stop("Unknown framework specified")
    }
    
    # Clip predictions to avoid log(0)
    prediction <- pmax(pmin(prediction, 1 - epsilon), epsilon)
    
    logLoss <- -mean(labels * log(prediction) + (1 - labels) * log(1 - prediction))
    
    if (framework == 'lightgbm'){
      return(list(name = 'log-loss', value = logLoss, higher_better = FALSE))
    } else{
      return(list(metric = "log-loss", value = logLoss))
    }
  }
  
  if (!is.null(prediction)) {
    p <- prediction$value
    y <- prediction$outcomeCount
    
    # Clip predictions to avoid log(0)
    p <- pmax(pmin(p, 1 - epsilon), epsilon)
    
    logLoss <- -mean(y * log(p) + (1 - y) * log(1 - p))
    return(logLoss)
  }
  
  stop("Either 'prediction' or 'dtrain' must be provided.")
}

#specificity
specificityScore <- function(prediction = NULL, dtrain = NULL, framework = 'xgboost', optimalThreshold = NULL) {
  if (!is.null(dtrain)) {
    if (framework == 'xgboost'){
      labels <- xgboost::getinfo(dtrain, "label")
    } else if (framework == 'lightgbm'){
      labels <- lightgbm::get_field(dtrain, "label")
    } else{
      stop("Unknown framework specified")
    }
    if (is.null(optimalThreshold)){
      optimalThreshold <- optimizeThreshold(prediction, labels)
    }
    predictedClasses <- ifelse(prediction > optimalThreshold, 1, 0)
    tn <- sum(predictedClasses == 0 & labels == 0)
    fp <- sum(predictedClasses == 1 & labels == 0)
    specificity <- if ((tn + fp) == 0) 0 else tn / (tn + fp)
    if (framework == 'lightgbm'){
      return(list(name = 'specificity', value = specificity, higher_better = TRUE))
    } else{
      return(list(metric = "specificity", value = specificity)) 
    }
  }
  
  if (!is.null(prediction)) {
    if (is.null(optimalThreshold)){
      optimalThreshold <- optimizeThreshold(prediction$value, prediction$outcomeCount)
    }
    predictedClasses <- ifelse(prediction$value > optimalThreshold, 1, 0)
    tn <- sum(predictedClasses == 0 & prediction$outcomeCount == 0)
    fp <- sum(predictedClasses == 1 & prediction$outcomeCount == 0)
    specificity <- if ((tn + fp) == 0) 0 else tn / (tn + fp)
    return(specificity)
  }
  
  stop("Either 'prediction' or 'dtrain' must be provided.")
}

# Confusion Matrix #maybe not for xgboost
confusionMatrix <- function(prediction = NULL, dtrain = NULL, framework = 'xgboost', optimalThreshold = NULL) {
  if (!is.null(dtrain)) {
    if (framework == 'xgboost'){
      labels <- xgboost::getinfo(dtrain, "label")
    } else if (framework == 'lightgbm'){
      labels <- lightgbm::get_field(dtrain, "label")
    } else{
      stop("Unknown framework specified")
    }
    if (is.null(optimalThreshold)){
      optimalThreshold <- optimizeThreshold(prediction, labels)
    }
    predictedClasses <- ifelse(prediction > optimalThreshold, 1, 0)
    tp <- sum(predictedClasses == 1 & labels == 1)
    tn <- sum(predictedClasses == 0 & labels == 0)
    fp <- sum(predictedClasses == 1 & labels == 0)
    fn <- sum(predictedClasses == 0 & labels == 1)
  } else if (!is.null(prediction)) {
    if (is.null(optimalThreshold)){
      optimalThreshold <- optimizeThreshold(prediction$value, prediction$outcomeCount)
    }
    predictedClasses <- ifelse(prediction$value > optimalThreshold, 1, 0)
    tp <- sum(predictedClasses == 1 & prediction$outcomeCount == 1)
    tn <- sum(predictedClasses == 0 & prediction$outcomeCount == 0)
    fp <- sum(predictedClasses == 1 & prediction$outcomeCount == 0)
    fn <- sum(predictedClasses == 0 & prediction$outcomeCount == 1)
  } else {
    stop("Either 'prediction' or 'dtrain' must be provided.")
  }
  
  matrix <- matrix(c(tp, fn, fp, tn), nrow = 2, byrow = TRUE,
                   dimnames = list("Predicted" = c("Positive", "Negative"),
                                   "Actual" = c("Positive", "Negative")))
  return(matrix)
}

#Matthews Correlation Coefficient (MCC)
mccScore <- function(prediction = NULL, dtrain = NULL, framework = 'xgboost', optimalThreshold = NULL) {
  if (!is.null(dtrain)) {
    if (framework == 'xgboost'){
      labels <- xgboost::getinfo(dtrain, "label")
    } else if (framework == 'lightgbm'){
      labels <- lightgbm::get_field(dtrain, "label")
    } else{
      stop("Unknown framework specified")
    }
    if (is.null(optimalThreshold)){
      optimalThreshold <- optimizeThreshold(prediction, labels)
    }
    predictedClasses <- ifelse(prediction > optimalThreshold, 1, 0)
    
    tp <- sum(predictedClasses == 1 & labels == 1)
    tn <- sum(predictedClasses == 0 & labels == 0)
    fp <- sum(predictedClasses == 1 & labels == 0)
    fn <- sum(predictedClasses == 0 & labels == 1)
    
    numerator <- (tp * tn) - (fp * fn)
    denominator <- sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn))
    
    # Handle the case where the denominator is zero to avoid division by zero
    mcc <- if (denominator == 0) 0 else numerator / denominator
    if (framework == 'lightgbm'){
      return(list(name = 'mcc', value = mcc, higher_better = TRUE))
    } else{
      return(list(metric = "mcc", value = mcc)) 
    }
  }
  
  if (!is.null(prediction)) {
    if (is.null(optimalThreshold)){
      optimalThreshold <- optimizeThreshold(prediction$value, prediction$outcomeCount)
    }
    predictedClasses <- ifelse(prediction$value > optimalThreshold, 1, 0)
    labels <- prediction$outcomeCount
    
    tp <- sum(predictedClasses == 1 & labels == 1)
    tn <- sum(predictedClasses == 0 & labels == 0)
    fp <- sum(predictedClasses == 1 & labels == 0)
    fn <- sum(predictedClasses == 0 & labels == 1)
    
    numerator <- (tp * tn) - (fp * fn)
    denominator <- sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn))
    
    mcc <- if (denominator == 0) 0 else numerator / denominator
    return(mcc)
  }
  
  stop("Either 'prediction' or 'dtrain' must be provided.")
}


# Balanced Accuracy
balancedAccuracyScore <- function(prediction = NULL, dtrain = NULL, framework = 'xgboost', optimalThreshold = NULL) {
  if (!is.null(dtrain)) {
    if (framework == 'xgboost'){
      labels <- xgboost::getinfo(dtrain, "label")
    } else if (framework == 'lightgbm'){
      labels <- lightgbm::get_field(dtrain, "label")
    } else{
      stop("Unknown framework specified")
    }
    if (is.null(optimalThreshold)){
      optimalThreshold <- optimizeThreshold(prediction, labels)
    }
    predictedClasses <- ifelse(prediction > optimalThreshold, 1, 0)
    
    tp <- sum(predictedClasses == 1 & labels == 1)
    tn <- sum(predictedClasses == 0 & labels == 0)
    fp <- sum(predictedClasses == 1 & labels == 0)
    fn <- sum(predictedClasses == 0 & labels == 1)
    
    sensitivity <- if ((tp + fn) == 0) 0 else tp / (tp + fn)  # Recall for the positive class
    specificity <- if ((tn + fp) == 0) 0 else tn / (tn + fp)  # Recall for the negative class
    
    balancedAccuracy <- (sensitivity + specificity) / 2
    if (framework == 'lightgbm'){
      return(list(name = 'balanced-accuracy', value = balancedAccuracy, higher_better = TRUE))
    } else{
      return(list(metric = "balanced-accuracy", value = balancedAccuracy)) 
    }
  }
  
  if (!is.null(prediction)) {
    if (is.null(optimalThreshold)){
      optimalThreshold <- optimizeThreshold(prediction$value, prediction$outcomeCount)
    }
    predictedClasses <- ifelse(prediction$value > optimalThreshold, 1, 0)
    labels <- prediction$outcomeCount
    
    tp <- sum(predictedClasses == 1 & labels == 1)
    tn <- sum(predictedClasses == 0 & labels == 0)
    fp <- sum(predictedClasses == 1 & labels == 0)
    fn <- sum(predictedClasses == 0 & labels == 1)
    
    sensitivity <- if ((tp + fn) == 0) 0 else tp / (tp + fn)
    specificity <- if ((tn + fp) == 0) 0 else tn / (tn + fp)
    
    balancedAccuracy <- (sensitivity + specificity) / 2
    return(balancedAccuracy)
  }
  
  stop("Either 'prediction' or 'dtrain' must be provided.")
}


# Geometric Mean
gMeanScore <- function(prediction = NULL, dtrain = NULL, framework = 'xgboost', optimalThreshold = NULL) {
  if (!is.null(dtrain)) {
    if (framework == 'xgboost'){
      labels <- xgboost::getinfo(dtrain, "label")
    } else if (framework == 'lightgbm'){
      labels <- lightgbm::get_field(dtrain, "label")
    } else{
      stop("Unknown framework specified")
    }
    if (is.null(optimalThreshold)){
      optimalThreshold <- optimizeThreshold(prediction, labels)
    }
    predictedClasses <- ifelse(prediction > optimalThreshold, 1, 0)
    
    tp <- sum(predictedClasses == 1 & labels == 1)
    tn <- sum(predictedClasses == 0 & labels == 0)
    fp <- sum(predictedClasses == 1 & labels == 0)
    fn <- sum(predictedClasses == 0 & labels == 1)
    
    sensitivity <- if ((tp + fn) == 0) 0 else tp / (tp + fn)  # Recall for the positive class
    specificity <- if ((tn + fp) == 0) 0 else tn / (tn + fp)  # Recall for the negative class
    
    gMean <- sqrt(sensitivity * specificity)
    if (framework == 'lightgbm'){
      return(list(name = 'gMean', value = gMean, higher_better = TRUE))
    } else{
      return(list(metric = "gMean", value = gMean)) 
    }
  }

  if (!is.null(prediction)) {
    if (is.null(optimalThreshold)){
      optimalThreshold <- optimizeThreshold(prediction$value, prediction$outcomeCount)
    }
    predictedClasses <- ifelse(prediction$value > optimalThreshold, 1, 0)
    labels <- prediction$outcomeCount
    
    tp <- sum(predictedClasses == 1 & labels == 1)
    tn <- sum(predictedClasses == 0 & labels == 0)
    fp <- sum(predictedClasses == 1 & labels == 0)
    fn <- sum(predictedClasses == 0 & labels == 1)
    
    sensitivity <- if ((tp + fn) == 0) 0 else tp / (tp + fn)
    specificity <- if ((tn + fp) == 0) 0 else tn / (tn + fp)
    
    gMean <- sqrt(sensitivity * specificity)
    return(gMean)
  }
  
  stop("Either 'prediction' or 'dtrain' must be provided.")
}


# Cohen's Kappa
kappaScore <- function(prediction = NULL, dtrain = NULL, framework = 'xgboost', optimalThreshold = NULL) {
  if (!is.null(dtrain)) {
    if (framework == 'xgboost'){
      labels <- xgboost::getinfo(dtrain, "label")
    } else if (framework == 'lightgbm'){
      labels <- lightgbm::get_field(dtrain, "label")
    } else{
      stop("Unknown framework specified")
    }
    if (is.null(optimalThreshold)){
      optimalThreshold <- optimizeThreshold(prediction, labels)
    }
    predictedClasses <- ifelse(prediction > optimalThreshold, 1, 0)
    
    tp <- sum(predictedClasses == 1 & labels == 1)
    tn <- sum(predictedClasses == 0 & labels == 0)
    fp <- sum(predictedClasses == 1 & labels == 0)
    fn <- sum(predictedClasses == 0 & labels == 1)
    
    total <- tp + tn + fp + fn
    p_o <- (tp + tn) / total  # Observed accuracy
    
    p_yes <- ((tp + fn) / total) * ((tp + fp) / total)
    p_no  <- ((tn + fp) / total) * ((tn + fn) / total)
    p_e <- p_yes + p_no  # Expected accuracy by chance
    
    kappa <- if ((1 - p_e) == 0) 0 else (p_o - p_e) / (1 - p_e)
    if (framework == 'lightgbm'){
      return(list(name = 'kappa', value = kappa, higher_better = TRUE))
    } else{
      return(list(metric = "kappa", value = kappa)) 
    }
  }
  
  if (!is.null(prediction)) {
    if (is.null(optimalThreshold)){
      optimalThreshold <- optimizeThreshold(prediction$value, prediction$outcomeCount)
    }
    predictedClasses <- ifelse(prediction$value > optimalThreshold, 1, 0)
    labels <- prediction$outcomeCount
    
    tp <- sum(predictedClasses == 1 & labels == 1)
    tn <- sum(predictedClasses == 0 & labels == 0)
    fp <- sum(predictedClasses == 1 & labels == 0)
    fn <- sum(predictedClasses == 0 & labels == 1)
    
    total <- tp + tn + fp + fn
    p_o <- (tp + tn) / total  # Observed accuracy
    
    p_yes <- ((tp + fn) / total) * ((tp + fp) / total)
    p_no  <- ((tn + fp) / total) * ((tn + fn) / total)
    p_e <- p_yes + p_no  # Expected accuracy by chance
    
    kappa <- if ((1 - p_e) == 0) 0 else (p_o - p_e) / (1 - p_e)
    return(kappa)
  }
  
  stop("Either 'prediction' or 'dtrain' must be provided.")
}


# Hamming Loss
hammingLossScore <- function(prediction = NULL, dtrain = NULL, framework = 'xgboost', optimalThreshold = NULL) {
  if (!is.null(dtrain)) {
    if (framework == 'xgboost'){
      labels <- xgboost::getinfo(dtrain, "label")
    } else if (framework == 'lightgbm'){
      labels <- lightgbm::get_field(dtrain, "label")
    } else{
      stop("Unknown framework specified")
    }
    if (is.null(optimalThreshold)){
      optimalThreshold <- optimizeThreshold(prediction, labels)
    }
    predictedClasses <- ifelse(prediction > optimalThreshold, 1, 0)
    hammingLoss <- mean(predictedClasses != labels)
    if (framework == 'lightgbm'){
      return(list(name = 'hammingLoss', value = hammingLoss, higher_better = FALSE))
    } else{
      return(list(metric = "hammingLoss", value = hammingLoss))
    }
  }

  if (!is.null(prediction)) {
    if (is.null(optimalThreshold)){
      optimalThreshold <- optimizeThreshold(prediction$value, prediction$outcomeCount)
    }
    predictedClasses <- ifelse(prediction$value > optimalThreshold, 1, 0)
    hammingLoss <- mean(predictedClasses != prediction$outcomeCount)
    return(hammingLoss)
  }
  
  stop("Either 'prediction' or 'dtrain' must be provided.")
}

# F2 Score
f2Score <- function(prediction = NULL, dtrain = NULL, framework = 'xgboost', optimalThreshold = NULL) {
  if (!is.null(dtrain)) {
    if (framework == 'xgboost'){
      labels <- xgboost::getinfo(dtrain, "label")
    } else if (framework == 'lightgbm'){
      labels <- lightgbm::get_field(dtrain, "label")
    } else{
      stop("Unknown framework specified")
    }
    if (is.null(optimalThreshold)){
      optimalThreshold <- optimizeThreshold(prediction, labels)
    }
    predictedClasses <- ifelse(prediction > optimalThreshold, 1, 0)
    
    tp <- sum(predictedClasses == 1 & labels == 1)
    fp <- sum(predictedClasses == 1 & labels == 0)
    fn <- sum(predictedClasses == 0 & labels == 1)
    
    precision <- if ((tp + fp) == 0) 0 else tp / (tp + fp)
    recall <- if ((tp + fn) == 0) 0 else tp / (tp + fn)
    
    f2 <- if ((precision + recall) == 0) 0 else (1 + 2^2) * precision * recall / (2^2 * precision + recall)
    if (framework == 'lightgbm'){
      return(list(name = 'f2', value = f2, higher_better = TRUE))
    } else{
      return(list(metric = "f2", value = f2)) 
    }
  }
  
  if (!is.null(prediction)) {
    if (is.null(optimalThreshold)){
      optimalThreshold <- optimizeThreshold(prediction$value, prediction$outcomeCount)
    }
    predictedClasses <- ifelse(prediction$value > optimalThreshold, 1, 0)
    labels <- prediction$outcomeCount
    
    tp <- sum(predictedClasses == 1 & labels == 1)
    fp <- sum(predictedClasses == 1 & labels == 0)
    fn <- sum(predictedClasses == 0 & labels == 1)
    
    precision <- if ((tp + fp) == 0) 0 else tp / (tp + fp)
    recall <- if ((tp + fn) == 0) 0 else tp / (tp + fn)
    
    f2 <- if ((precision + recall) == 0) 0 else (1 + 2^2) * precision * recall / (2^2 * precision + recall)
    return(f2)
  }
  
  stop("Either 'prediction' or 'dtrain' must be provided.")
}

#RMSE (root mean square error)
rmseScore <- function(prediction = NULL, dtrain = NULL, framework = 'xgboost') {
  if (!is.null(dtrain)) {
    if (framework == 'xgboost'){
      labels <- xgboost::getinfo(dtrain, "label")
    } else if (framework == 'lightgbm'){
      labels <- lightgbm::get_field(dtrain, "label")
    } else{
      stop("Unknown framework specified")
    }
    
    rmse <- sqrt(mean((labels - prediction)^2))
    if (framework == 'lightgbm'){
      return(list(name = 'rmse', value = rmse, higher_better = FALSE))
    } else{
      return(list(metric = "rmse", value = rmse, FALSE))
    }
  }
  
  if (!is.null(prediction)) {
    labels <- prediction$outcomeCount
    rmse <- sqrt(mean((labels - prediction$value)^2))
    return(rmse)
  }
  
  stop("Either 'prediction' or 'dtrain' must be provided.")
}

#MLogLoss (multinomial Log Loss)
mloglossScore <- function(prediction = NULL, dtrain = NULL, framework = 'xgboost') {
  if (!is.null(dtrain)) {
    if (framework == 'xgboost'){
      labels <- xgboost::getinfo(dtrain, "label")
    } else if (framework == 'lightgbm'){
      labels <- lightgbm::get_field(dtrain, "label")
    } else{
      stop("Unknown framework specified")
    }
    
    eps <- 1e-15
    prediction <- pmin(pmax(prediction, eps), 1 - eps) # Clipping prediction to avoid log(0)
    mlogloss <- -mean(labels * log(prediction) + (1 - labels) * log(1 - prediction))
    
    if (framework == 'lightgbm'){
      return(list(name = 'mlogloss', value = mlogloss, higher_better = FALSE))
    } else{
      return(list(metric = "mlogloss", value = mlogloss))
    }
  }
  
  if (!is.null(prediction)) {
    labels <- prediction$outcomeCount
    eps <- 1e-15
    pred_probs <- pmin(pmax(prediction$value, eps), 1 - eps)
    mlogloss <- -mean(labels * log(pred_probs) + (1 - labels) * log(1 - pred_probs))
    return(mlogloss)
  }
  
  stop("Either 'prediction' or 'dtrain' must be provided.")
}

#Error (classification error)
errorScore <- function(prediction = NULL, dtrain = NULL, framework = 'xgboost', optimalThreshold = NULL) {
  if (!is.null(dtrain)) {
    if (framework == 'xgboost'){
      labels <- xgboost::getinfo(dtrain, "label")
    } else if (framework == 'lightgbm'){
      labels <- lightgbm::get_field(dtrain, "label")
    } else{
      stop("Unknown framework specified")
    }
    if (is.null(optimalThreshold)){
      optimalThreshold <- optimizeThreshold(prediction, labels)
    }
    predictedClasses <- ifelse(prediction > optimalThreshold, 1, 0)
    error <- mean(predictedClasses != labels)
    if (framework == 'lightgbm'){
      return(list(name = 'error', value = error, higher_better = FALSE))
    } else{
      return(list(metric = "error", value = error))
    }
  }
  
  if (!is.null(prediction)) {
    if (is.null(optimalThreshold)){
      optimalThreshold <- optimizeThreshold(prediction$value, prediction$outcomeCount)
    }
    predictedClasses <- ifelse(prediction$value > optimalThreshold, 1, 0)
    labels <- prediction$outcomeCount
    error <- mean(predictedClasses != labels)
    return(error)
  }
  
  stop("Either 'prediction' or 'dtrain' must be provided.")
}

#MAE (Mean Absolute Error)
maeScore <- function(prediction = NULL, dtrain = NULL, framework = 'xgboost') {
  if (!is.null(dtrain)) {
    if (framework == 'xgboost'){
      labels <- xgboost::getinfo(dtrain, "label")
    } else if (framework == 'lightgbm'){
      labels <- lightgbm::get_field(dtrain, "label")
    } else{
      stop("Unknown framework specified")
    }
    mae <- mean(abs(prediction - labels))
    if (framework == 'lightgbm'){
      return(list(name = 'mae', value = mae, higher_better = FALSE))
    } else{
      return(list(metric = "mae", value = mae))
    }
  }
  
  if (!is.null(prediction)) {
    labels <- prediction$outcomeCount
    mae <- mean(abs(prediction$value - labels))
    return(mae)
  }
  
  stop("Either 'prediction' or 'dtrain' must be provided.")
}


optimizeThreshold <- function(prediction, outcomeCount) {
  prediction = prediction
  outcomeCount = outcomeCount
  bestThreshold <- 0.5
  return(bestThreshold)
}

