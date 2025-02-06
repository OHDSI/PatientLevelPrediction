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
createTinyPlpData <- function(plpData, plpResult, n = 20) {
  covariates <- plpResult$model$covariateImportance %>%
    dplyr::slice_max(
      order_by = abs(.data$covariateValue),
      n = n, with_ties = FALSE
    ) %>%
    dplyr::pull(.data$covariateId)
  tinyPlpData <- plpData
  tinyPlpData$covariateData <- Andromeda::copyAndromeda(plpData$covariateData)

  tinyPlpData$covariateData$covariates <- plpData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId %in% covariates)
  tinyPlpData$covariateData$covariateRef <- plpData$covariateData$covariateRef %>%
    dplyr::filter(.data$covariateId %in% covariates)

  rowIds <- tinyPlpData$covariateData$covariates %>%
    dplyr::pull(.data$rowId) %>%
    unique()
  tinyPlpData$cohorts <- plpData$cohorts %>%
    dplyr::filter(.data$rowId %in% rowIds)

  attributes(tinyPlpData$covariateData)$metaData <-
    attributes(plpData$covariateData)$metaData
  class(tinyPlpData$covariateData) <- class(plpData$covariateData)
  attributes(tinyPlpData)$metaData <- attributes(plpData)$metaData
  class(tinyPlpData) <- class(plpData)
  return(tinyPlpData)
}

createData <- function(observations, features, totalFeatures,
                       numCovs = FALSE,
                       outcomeRate = 0.5,
                       seed = 42) {
  rowId <- rep(1:observations, each = features)
  withr::with_seed(42, {
    columnId <- sample(1:totalFeatures, observations * features, replace = TRUE)
  })
  covariateValue <- rep(1, observations * features)
  covariates <- data.frame(rowId = rowId, columnId = columnId, covariateValue = covariateValue)
  if (numCovs) {
    numRow <- 1:observations
    numCol <- rep(totalFeatures + 1, observations)
    withr::with_seed(seed, {
      numVal <- runif(observations)
    })
    numCovariates <- data.frame(
      rowId = as.integer(numRow),
      columnId = as.integer(numCol),
      covariateValue = numVal
    )
    covariates <- rbind(covariates, numCovariates)
  }
  withr::with_seed(seed, {
    labels <- as.numeric(sample(0:1, observations, replace = TRUE, prob = c(1 - outcomeRate, outcomeRate)))
  })

  data <- list(covariates = covariates, labels = labels)
  return(data)
}
