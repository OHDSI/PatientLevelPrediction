#' Get a sparse summary of the calibration
#'
#' @details
#' Generates a sparse summary showing the predicted probabilities and the observed fractions. Predictions are
#' stratefied into equally sized bins of predicted probabilities.
#'
#' @param prediction            A prediction object as generated using the
#'                              \code{\link{predict}} functions.
#' @param predictionType        The type of prediction (binary or survival)                             
#' @param typeColumn            A column that is used to stratify the results  
#' @param numberOfStrata        The number of strata in the plot.
#' @param truncateFraction      This fraction of probability values will be ignored when plotting, to
#'                              avoid the x-axis scale being dominated by a few outliers.
#'
#' @return
#' A dataframe with the calibration summary
#' @export
getCalibrationSummary <- function(
  prediction,
  predictionType,
  typeColumn = 'evaluation',
  numberOfStrata = 100,
  truncateFraction = 0.05
  ){
  evaluation <- do.call(
    what = paste0('getCalibrationSummary_', predictionType), 
    args = list(
      prediction = prediction, 
      evalColumn = typeColumn,
      numberOfStrata = numberOfStrata,
      truncateFraction = truncateFraction,
      timepoint = attr(prediction, 'metaData')$timepoint
    )
  )
  
  return(evaluation)
}


getCalibrationSummary_binary <- function(
  prediction,
  evalColumn,
  numberOfStrata = 10,
  truncateFraction = 0.05,
  ...) {
  
  result <- c()
  evalTypes <- unique(as.data.frame(prediction)[,evalColumn])
  
  for(evalType in evalTypes){
    
    predictionOfInterest <- prediction %>% dplyr::filter(.data[[evalColumn]] == evalType)
    
    
    q <- unique(stats::quantile(predictionOfInterest$value, (1:(numberOfStrata - 1))/numberOfStrata))
    predictionOfInterest$predictionThresholdId <- cut(predictionOfInterest$value,
      breaks = unique(c(-0.00001, q, max(predictionOfInterest$value))),
      labels = FALSE)
    
    predictionOfInterest <- merge(predictionOfInterest,
      data.frame(predictionThresholdId=1:(length(q)+1), predictionThreshold=c(0, q)),
      by='predictionThresholdId', all.x=T)
    
    computeStratumStats <- function(data) {
      return(data.frame(minx = min(data$value),
        maxx = max(data$value),
        fraction = sum(data$outcomeCount)/nrow(data)))
    }
    
    # count the number of persons with the age/gender strata
    PersonCountAtRisk <- stats::aggregate(rowId ~ predictionThreshold, data = predictionOfInterest, length)
    names(PersonCountAtRisk)[2] <- "PersonCountAtRisk"
    
    # count the number of persons with the age/gender strata in T also in O at time-at-risk
    PersonCountWithOutcome <- stats::aggregate(outcomeCount ~ predictionThreshold, data = predictionOfInterest, sum)
    names(PersonCountWithOutcome)[2] <- "PersonCountWithOutcome"
    
    strataData <- merge(PersonCountAtRisk,
      PersonCountWithOutcome)
    
    # Select all persons within the predictionThreshold, compute their average predicted probability
    averagePredictedProbability <- stats::aggregate(predictionOfInterest$value, list(predictionOfInterest$predictionThreshold),
      mean)
    colnames(averagePredictedProbability) <- c('predictionThreshold', 'averagePredictedProbability')
    strataData <- merge(strataData,averagePredictedProbability)
    
    StDevPredictedProbability <- stats::aggregate(predictionOfInterest$value, list(predictionOfInterest$predictionThreshold),
      stats::sd)
    colnames(StDevPredictedProbability) <- c('predictionThreshold', 'StDevPredictedProbability')
    strataData <- merge(strataData, StDevPredictedProbability)
    
    # MinPredictedProbability, P25PredictedProbability, MedianPredictedProbability,
    # P75PredictedProbability, MaxPredictedProbability
    quantiles <- stats::aggregate(predictionOfInterest$value, list(predictionOfInterest$predictionThreshold),
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
    
    strataData$evaluation <- evalType
    
    # what is this used for?
    attr(strataData,'lims') <- stats::quantile(predictionOfInterest$value, c(truncateFraction, 1 - truncateFraction))
    
    result <- rbind(
      result, 
      strataData
    )
    
  }
  
  return(result)
}



getCalibrationSummary_survival <- function(
  prediction,
  evalColumn,
  numberOfStrata = 10,
  truncateFraction = 0.01,
  timepoint,
  ...) {
  
  result <- c()
  evalTypes <- unique(as.data.frame(prediction)[,evalColumn])
  
  for(evalType in evalTypes){
    
    predictionOfInterest <- prediction %>% dplyr::filter(.data[[evalColumn]] == evalType)
    
  # add in calibration for  survival 
    t <- predictionOfInterest$survivalTime
    y <- ifelse(predictionOfInterest$outcomeCount > 0, 1, 0)
    
  S <- survival::Surv(t, y) 
  if(length(unique(predictionOfInterest$value)) <= numberOfStrata){
    gval <- 10
  } else{
    gval <- numberOfStrata
  }
  groups<-Hmisc::cut2(predictionOfInterest$value,g=gval)
  n.groups<-length(levels(groups))
  pred<-tapply(predictionOfInterest$value,groups,mean)
  sizesN<-tapply(predictionOfInterest$value,groups,length)
  obs.q<-NULL
  obs.lower.q<-NULL
  obs.upper.q<-NULL
  avPred <- NULL
  for (q in 1:n.groups){
    KM <- tryCatch({survival::survfit(S ~ 1,sub=groups==levels(groups)[q])},
      error = function(e){ParallelLogger::logError(e); return(list(surv = 0,
        upper = 0,
        lower = 0 ))})
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
  
  calibrationSummary <- data.frame(
    evaluation = evalType,
    predictionThreshold = unlist(
      lapply(
        levels(groups), 
        midpoints
      )
    ),
    averagePredictedProbability = pred,
    observedIncidence = obs.q,
    observedIncidenceLB = obs.lower.q,
    observedIncidenceUB = obs.upper.q,
    PersonCountAtRisk = sizesN
  ) 
  
  result <- rbind(
    result,
    calibrationSummary
  )
  
  }
  
  return(result)
  
}
