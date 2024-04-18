#auto-tuner
autoTuning <- function(
  trainData,
  self
){

  # function to generate hyper-parameters iteratively - grid/random/mc sampling/genetic algorithm
  # think about parallelization 
  
  summaryPerformance <- c()
  aggregatePerformances <- c()
  hyperparameterList <- list()
  
  start <- T
  repeat(
    { 
      
      hyperparameter <- self$hyperparameterGenerator$getNextHyperparameters(summaryPerformance);
      self$setHyperparameters(hyperparameter)
      
      performances <- list()
      for(i in 1:self$resamplingFunction$getInterationCount()){
        
        # function to split trainData into train/val - cv/booststrap
        dataIndexes <- self$resamplingFunction$getIndexes(
          data = trainData, 
          iteration = i
          ) # list of train/validation or indexes?
      
          validationPrediction <- self$fit(
            data = trainData,
            trainIndex = dataIndexes$trainRowIds,
            validationIndex= dataIndexes$validationRowIds,
            returnPredictionOnly = T
          )
          # user specified performance metric that takes prediction and spits out performance (could be multiple inputs)
          performanceTemp <- self$performanceFunction$metricFunction(validationPrediction)
          performances[[length(performances)+1]] <- performanceTemp
          
          summaryPerformance[[length(summaryPerformance) + 1]] <- list(
            hyperparameter = hyperparameter, 
            fold = i, 
            performance = performanceTemp
            )
      }
      
      aggregatePerformanceIteration <- self$performanceFunction$aggregateFunction(performances)
      
      summaryPerformance[[length(summaryPerformance) + 1]] <- list(
        hyperparameter = hyperparameter, 
        performances = performances,
        aggregatePerformance = aggregatePerformanceIteration
      )
      
      if(start){
        start <- F
        currentOptimal <- aggregatePerformanceIteration
        optimalHyperparameters <- hyperparameter
      }
      
      # performance selection function - take performance vector to identify best hyper-params (returns index)
      if(self$performanceFunction$maxmize){
        if(currentOptimal < aggregatePerformanceIteration){
          optimalHyperparameters = hyperparameter
          currentOptimal <- aggregatePerformanceIteration
        }
      } else{
        if(currentOptimal > aggregatePerformanceIteration){
          optimalHyperparameters = hyperparameter
          currentOptimal <- aggregatePerformanceIteration
        }
      }
  
    if( self$hyperparameterGenerator$converged){
      break
      } 
    })

  # return chosen hyper-parameters
  self$hyperparametersFinal <- optimalHyperparameters
  self$hyperparameterSummary <- summaryPerformance
  
  #hyperparameterResults <- list(
  #  optimalHyperparameters = optimalHyperparameters,
  #  summaryPerformance = summaryPerformance
  #)
}

LOOCV <- function(
    trainData,
    optimizationSettings, # nfold, seed, etc..
    hyperparameterSettings, # list of hyper-parameters
    modelSettings,
    analysisId,
    analysisPath
){
  
  fun <- eval(parse(text = modelSettings$fitFunction))
  
  performanceVals <- list()
  for(hyperparameters in hyperparameterSettings$hyperparameterList){
    
    performanceVal <- rep(0, length(hyperparameterSettings$hyperparameterList))
    for(i in 1:nrow(trainData)){
      args <- list(
        trainData = trainData[-i,],
        valData = trainData[i,],
        hyperparameters = hyperparameters,
        seed = optimizationSettings$seed
      )
      prediction <- do.call(fun, args) # prediction object
      performanceVal[i] <- optimizationSettings$performanceFunction(prediction)
    }
    
    performanceVals[[length(performanceVals) + 1]] <- performanceVal
  }
  
  performanceValsMeans <- unlist(lapply(x = performanceVals, FUN = mean))
  
  optimalInd <- hyperparameterSettings$optimalFunction(performanceValsMeans)
  
  
  hyperparameterResults <- list(
    optimalHyperparameters = hyperparameterSettings$hyperparameterList[[optimalInd]],
    summaryPerformance = createSummaryPerformanceDataFrame(
      hyperparameterSettings$hyperparameterList,
      performanceVals,
      performanceValsMeans
    )
  )
}


# add simple train/val split


# add genetic algorithm optimization 