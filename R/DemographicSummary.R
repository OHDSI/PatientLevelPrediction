#' Get a calibration per age/gender groups
#'
#' @details
#' Generates a data.frame with the calibration per each 5 year age group and gender group
#'
#' @param prediction            A prediction object 
#' @param predictionType        The type of prediction (binary or survival)                             
#' @param typeColumn            A column that is used to stratify the results  
#'
#' @return
#' A dataframe with the calibration summary
#' @export
getDemographicSummary <- function(
  prediction,
  predictionType,
  typeColumn = 'evaluation'
){
  evaluation <- do.call(
    what = paste0('getDemographicSummary_', predictionType), 
    args = list(
      prediction = prediction, 
      evalColumn = typeColumn,
      timepoint = attr(prediction, 'metaData')$timepoint
    )
  )
  
  return(evaluation)
}



getDemographicSummary_binary <- function(prediction, evalColumn , ...){
  
  result <- c()
  evalTypes <- unique(as.data.frame(prediction)[,evalColumn])
  
  for(evalType in evalTypes){
    
    predictionOfInterest <- prediction %>% dplyr::filter(.data[[evalColumn]] == evalType)
  
    demographicData <- predictionOfInterest[,c('rowId','ageYear','gender')] %>% 
      dplyr::mutate(
        ageId = floor(.data$ageYear/5),
        ageGroup = paste0('Age group: ', floor(.data$ageYear/5)*5, '-',floor(.data$ageYear/5)*5+4),
        genId = .data$gender,
        genGroup = ifelse(.data$gender==8507, 'Male', 'Female')) %>%
      dplyr::select(.data$rowId,.data$ageId,.data$ageGroup,.data$genId,.data$genGroup ) %>%
      dplyr::inner_join(predictionOfInterest[,colnames(predictionOfInterest)%in%c('rowId', 'value','outcomeCount','survivalTime')], by='rowId')
    
    demographicData <- demographicData %>%
      dplyr::group_by(.data$ageGroup,.data$genGroup)  %>%
      dplyr::summarise(
        PersonCountAtRisk = length(.data$outcomeCount), 
        PersonCountWithOutcome = sum(.data$outcomeCount),
        averagePredictedProbability = mean(.data$value, na.rm = T),
        StDevPredictedProbability = stats::sd(.data$value, na.rm = T),
        MinPredictedProbability =stats::quantile(.data$value, probs = 0),
        P25PredictedProbability =stats::quantile(.data$value, probs = 0.25),
        P50PredictedProbability =stats::quantile(.data$value, probs = 0.50),
        P75PredictedProbability =stats::quantile(.data$value, probs = 0.75),
        MaxPredictedProbability =stats::quantile(.data$value, probs = 1),
      )
    
    demographicData$evaluation = evalType
    
    result <- rbind(result, demographicData)
    
  }
  
  result <- as.data.frame(result)
  return(result)
  
}


getDemographicSummary_survival <- function(prediction, evalColumn, timepoint = NULL, ...){
  
  result <- c()
  evalTypes <- unique(as.data.frame(prediction)[,evalColumn])
  
  for(evalType in evalTypes){
    
    predictionOfInterest <- prediction %>% 
      dplyr::filter(.data[[evalColumn]] == evalType)
    
    demographicData <- predictionOfInterest[,c('rowId','ageYear','gender')] %>% 
      dplyr::mutate(ageId = floor(.data$ageYear/5),
        ageGroup = paste0('Age group: ', floor(.data$ageYear/5)*5, '-',floor(.data$ageYear/5)*5+4),
        genId = .data$gender,
        genGroup = ifelse(.data$gender==8507, 'Male', 'Female')) %>%
      dplyr::select(.data$rowId,.data$ageId,.data$ageGroup,.data$genId,.data$genGroup ) %>%
      dplyr::inner_join(predictionOfInterest[,colnames(predictionOfInterest)%in%c('rowId', 'value','outcomeCount','survivalTime')], by='rowId')
    
    
    if(is.null(timepoint)){
      timepoint <- max(demographicData$survivalTime)
    }
    demographicSum <- demographicData %>% 
      dplyr::mutate(
        t = .data$survivalTime,
        y = ifelse(.data$outcomeCount > 0, 1, 0)
      )
    
    gen <- unique(demographicData$genGroup)
    ageGroup <- unique(demographicData$ageGroup)
    
    demographicData <- NULL
    for(gen in gen){
      for(age in ageGroup){
        
        tempDemo <- demographicSum %>% 
          dplyr::filter( .data$genGroup == gen & .data$ageGroup == age )
        
        if(nrow(tempDemo)>0){
          t1 <- tempDemo %>% dplyr::select(.data$t)
          y1 <- tempDemo %>% dplyr::select(.data$y)
          p1 <- tempDemo %>% dplyr::select(.data$value)
          
          out <- tryCatch(
            {
              summary(
                survival::survfit(survival::Surv(t1$t, y1$y) ~ 1), 
                times = timepoint
              )
            },
            error = function(e){ParallelLogger::logError(e); return(NULL)}
          )
          
          if(!is.null(out)){
            demoTemp <- c(
              genGroup = gen, 
              ageGroup = age, 
              PersonCountAtRisk = length(p1$value),
              PersonCountWithOutcome = round(length(p1$value)*(1-out$surv)),
              observedRisk = 1-out$surv, 
              averagePredictedProbability = mean(p1$value, na.rm = T),
              StDevPredictedProbability = stats::sd(p1$value, na.rm = T)
              )
            
            demographicData <- rbind(demographicData, demoTemp)
          }
        }
        
      }
      
    }
    demographicData <- as.data.frame(demographicData)
    demographicData$averagePredictedProbability <- as.double(as.character(demographicData$averagePredictedProbability ))
    demographicData$StDevPredictedProbability <- as.double(as.character(demographicData$StDevPredictedProbability ))
    demographicData$PersonCountAtRisk <- as.double(as.character(demographicData$PersonCountAtRisk ))
    demographicData$PersonCountWithOutcome <- as.double(as.character(demographicData$PersonCountWithOutcome ))
    
    demographicData$evaluation <- evalType
    
    result <- rbind(
      result, 
      demographicData
    )
    
  }
  
  result <- as.data.frame(result)
  return(result)
}
