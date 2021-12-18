#' Calculate all measures for sparse ROC
#'
#' @details
#' Calculates the TP, FP, TN, FN, TPR, FPR, accuracy, PPF, FOR and Fmeasure
#' from a prediction object
#'
#' @param prediction            A prediction object 
#' @param predictionType        The type of prediction (binary or survival)                             
#' @param typeColumn            A column that is used to stratify the results                           
#'
#' @return
#' A data.frame with all the measures
#' @export
getThresholdSummary <- function(
  prediction,
  predictionType,
  typeColumn = 'evaluation'
){
  evaluation <- do.call(
    what = paste0('getThresholdSummary_', predictionType), 
    args = list(
      prediction = prediction, 
      evalColumn = typeColumn,
      timepoint = attr(prediction, 'metaData')$timepoint
    )
  )
  
  return(evaluation)
}



#' Calculate all measures for sparse ROC when prediction is bianry classification
#'
#' @details
#' Calculates the TP, FP, TN, FN, TPR, FPR, accuracy, PPF, FOR and Fmeasure
#' from a prediction object
#'
#' @param prediction            A prediction object 
#' @param evalColumn            A column that is used to stratify the results                           
#' @param ...                   Other inputs
#'
#' @return
#' A data.frame with all the measures
#'
getThresholdSummary_binary <- function(prediction, evalColumn, ...){
  
  result <- c()
  evalTypes <- unique(as.data.frame(prediction)[,evalColumn])
  
  for(evalType in evalTypes){
    
    predictionOfInterest <- prediction %>% dplyr::filter(.data[[evalColumn]] == evalType)
    
    # do input checks
    if(nrow(predictionOfInterest)<1){
      warning('sparse threshold summary not calculated due to empty dataset')
      return(NULL)
    }
    
    n <- nrow(predictionOfInterest)
    P <- sum(predictionOfInterest$outcomeCount>0)
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
    proportion <- sum(predictionOfInterest$outcomeCount>0)/nrow(predictionOfInterest)
    # ISSUE WITH CAL # remove any predictions of 1
    predictionOfInterest$value[predictionOfInterest$value==1] <- 0.99999999
    x <- exp(log(predictionOfInterest$value/(1 - predictionOfInterest$value)) - log(proportion/(1 - proportion)))
    predictionOfInterest$preferenceScore <- x/(x + 1)
    
    # sort prediction
    predictionOfInterest <- predictionOfInterest[order(-predictionOfInterest$value),]
    
    # create indexes
    if(length(predictionOfInterest$preferenceScore)>100){
      indexesOfInt <- c(
        1:100,
        seq(
          1, 
          length(predictionOfInterest$preferenceScore),
          floor(length(predictionOfInterest$preferenceScore)/100 )
        ),
        length(predictionOfInterest$preferenceScore)
      )
    } else{
      indexesOfInt <- 1:length(predictionOfInterest$preferenceScore)
    }
    pt <- unique(predictionOfInterest$value[indexesOfInt])
    indexesOfInt <- unique(unlist(lapply(pt, function(pt) max(which(predictionOfInterest$value>=pt))))) # made this >= to match net benefit 
    
    # get thresholds
    predictionThreshold = predictionOfInterest$value[indexesOfInt]
    preferenceThreshold = predictionOfInterest$preferenceScore[indexesOfInt]
    
    # improve speed create vector with cum sum
    lab.order <- ifelse(predictionOfInterest$outcomeCount>0,1,0)
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
    
    result <- rbind(
      result, 
      data.frame(
        evaluation = evalType,
        predictionThreshold = predictionThreshold,
        preferenceThreshold = preferenceThreshold,
        positiveCount = positiveCount,
        negativeCount = negativeCount,
        trueCount = trueCount, 
        falseCount = falseCount,
        truePositiveCount = truePositiveCount,
        trueNegativeCount = trueNegativeCount,
        falsePositiveCount = falsePositiveCount,
        falseNegativeCount = falseNegativeCount,
        f1Score = f1Score, 
        accuracy = accuracy,
        sensitivity = sensitivity,
        falseNegativeRate = falseNegativeRate,
        falsePositiveRate = falsePositiveRate,
        specificity = specificity,
        positivePredictiveValue = positivePredictiveValue,
        falseDiscoveryRate = falseDiscoveryRate,
        negativePredictiveValue = negativePredictiveValue,
        falseOmissionRate = falseOmissionRate,
        positiveLikelihoodRatio = positiveLikelihoodRatio,
        negativeLikelihoodRatio = negativeLikelihoodRatio,
        diagnosticOddsRatio = diagnosticOddsRatio
      )
    )
    
  }
  
  result <- as.data.frame(result)
  return(result)
}

getThresholdSummary_survival <- function(prediction, evalColumn, timepoint, ...){
  
  result <- c()
  evalTypes <- unique(as.data.frame(prediction)[,evalColumn])
  
  for(evalType in evalTypes){
    
    predictionOfInterest <- prediction %>% 
      dplyr::filter(.data[[evalColumn]] == evalType)
    
    t <- predictionOfInterest$survivalTime
    y <- ifelse(predictionOfInterest$outcomeCount > 0, 1, 0)
    
    S <- survival::Surv(t, y) 
    p <- predictionOfInterest$value
    
    # add netbenefit
    preddat <- data.frame(
      p = p/max(p), 
      t=t, 
      y=y
    )
    
    nbSummary <- tryCatch(
      {
        stdca(
          data = preddat, 
          outcome = "y", 
          ttoutcome = "t", 
          timepoint = timepoint,  
          predictors = "p", 
          xstart = max(min(preddat$p),0.001), #0.001, 
          xstop = min(max(preddat$p),0.99), 
          xby = 0.001, 
          smooth=F
        )
      },
      error = function(e){ParallelLogger::logError(e); return(NULL)}
    )
    
    if(!is.null(nbSummary$net.benefit)){
      
      tempResult <- as.data.frame(nbSummary$net.benefit)
      tempResult$evaluation <- evalType
      
      result <- rbind(
        result, 
        tempResult
      )
    }
  }
    
  return(result)
    
}




###############################
#
#   FUNCTIONS TO HELP 
###############################

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
      graphics::plot(x = nb$threshold, y = nb$all, type = "n", 
        xlim = c(xstart, xstop), ylim = c(ymin, ymax), 
        xlab = "Threshold probability", ylab = paste("Net reduction in interventions per", 
          interventionper, "patients"))
      for (m in 1:pred.n) {
        if (smooth == TRUE) {
          graphics::lines(interv$threshold, data.matrix(interv[paste(predictors[m], 
            "_sm", sep = "")]), col = m, 
            lty = 2)
        }
        else {
          graphics::lines(interv$threshold, data.matrix(interv[predictors[m]]), 
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
          graphics::lines(nb$threshold, data.matrix(nb[paste(predictors[m], 
            "_sm", sep = "")]), col = m, 
            lty = 2)
        }
        else {
          graphics::lines(nb$threshold, data.matrix(nb[predictors[m]]), 
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
