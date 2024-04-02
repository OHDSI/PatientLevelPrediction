# helper functions for tests

# copies trainData and makes sure andromeda object is copied correctly 
copyTrainData <- function(trainData) {
  newTrainData <- trainData
  
  # force andromeda to copy
  newTrainData$covariateData <- Andromeda::copyAndromeda(trainData$covariateData)

  class(newTrainData$covariateData) <- class(trainData$covariateData)
  return(newTrainData)
}

# create tiny dataset with subset of covariates based on lasso fit
createTinyPlpData <- function(plpData, plpResult, n= 20) {
  
  covariates <- plpResult$model$covariateImportance %>% 
    dplyr::slice_max(order_by = abs(.data$covariateValue),
                     n = n, with_ties = F) %>% 
    dplyr::pull(.data$covariateId)
  tinyPlpData <- plpData
  tinyPlpData$covariateData <- Andromeda::copyAndromeda(plpData$covariateData)
  
  tinyPlpData$covariateData$covariates <- plpData$covariateData$covariates %>% 
    dplyr::filter(covariateId %in% covariates)
  tinyPlpData$covariateData$covariateRef <- plpData$covariateData$covariateRef %>% 
    dplyr::filter(covariateId %in% covariates)
  
  attributes(tinyPlpData$covariateData)$metaData <- attributes(plpData$covariateData)$metaData
  class(tinyPlpData$covariateData) <- class(plpData$covariateData)
  attributes(tinyPlpData)$metaData <- attributes(plpData)$metaData
  class(tinyPlpData) <- class(plpData)
  return(tinyPlpData)
}