# helper functions for tests

copyTrainData <- function(trainData) {
  newTrainData <- trainData
  
  # force andromeda to copy
  newTrainData$covariateData <- Andromeda::copyAndromeda(trainData$covariateData)

  class(newTrainData$covariateData) <- class(trainData$covariateData)
  return(newTrainData)
}