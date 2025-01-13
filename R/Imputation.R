# @file Imputation.R
# Copyright 2025 Observational Health Data Sciences and Informatics
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

#' @title Create Iterative Imputer settings
#' @description This function creates the settings for an iterative imputer
#' which first removes features with more than `missingThreshold` missing values
#' and then imputes the missing values iteratively using chained equations
#' @param missingThreshold The threshold for missing values to remove a feature
#' @param method The method to use for imputation, currently only "pmm" is supported
#' @return The settings for the single imputer of class `featureEngineeringSettings`
#' @export
createIterativeImputer <- function(missingThreshold = 0.3,
                                   method = "pmm") {
  checkIsClass(missingThreshold, "numeric")
  checkInStringVector(method, c("pmm"))
  checkHigher(missingThreshold, 0)
  checkLower(missingThreshold, 1)
  featureEngineeringSettings <- list(
    missingThreshold = missingThreshold,
    method = method
  )
  if (method == "pmm") {
    # at the moment this requires glmnet
    rlang::check_installed("glmnet")
  }
  attr(featureEngineeringSettings, "fun") <- "iterativeImpute"

  class(featureEngineeringSettings) <- "featureEngineeringSettings"
  return(featureEngineeringSettings)
}

#' @title Create Simple Imputer settings
#' @description This function creates the settings for a simple imputer
#' which imputes missing values with the mean or median
#' @param method The method to use for imputation, either "mean" or "median"
#' @param missingThreshold The threshold for missing values to be imputed vs removed
#' @return The settings for the single imputer of class `featureEngineeringSettings`
#' @export
createSimpleImputer <- function(method = "mean",
                                missingThreshold = 0.3) {
  checkIsClass(method, "character")
  checkInStringVector(method, c("mean", "median"))
  checkIsClass(missingThreshold, "numeric")
  checkHigher(missingThreshold, 0)
  checkLower(missingThreshold, 1)
  featureEngineeringSettings <- list(
    method = method,
    missingThreshold = missingThreshold
  )
  attr(featureEngineeringSettings, "fun") <- "simpleImpute"

  class(featureEngineeringSettings) <- "featureEngineeringSettings"
  return(featureEngineeringSettings)
}

#' @title Simple Imputation
#' @description This function does single imputation with the mean or median
#' @param trainData The data to be imputed
#' @param featureEngineeringSettings The settings for the imputation
#' @param done Whether the imputation has already been done (bool)
#' @return The imputed data
#' @keywords internal
simpleImpute <- function(trainData, featureEngineeringSettings, done = FALSE) {
  if (!done) {
    missingInfo <- extractMissingInfo(trainData)
    trainData$covariateData$missingInfo <- missingInfo$missingInfo
    continuousFeatures <- missingInfo$continuousFeatures
    on.exit(trainData$covariateData$missingInfo <- NULL, add = TRUE)
    
    trainData$covariateData$covariates <- trainData$covariateData$covariates %>%
      dplyr::left_join(trainData$covariateData$missingInfo, by = "covariateId") %>%
      dplyr::filter(is.na(.data$missing) ||
        .data$missing <= featureEngineeringSettings$missingThreshold) %>%
      dplyr::select(-"missing")

    # separate the continuous and binary features
    featureData <- separateFeatures(trainData, continuousFeatures)
    numericData <- featureData[[1]]
    on.exit(numericData <- NULL, add = TRUE)
  
    allRowIds <- numericData$covariates %>%
      dplyr::pull(.data$rowId) %>%
      unique() %>%
      sort()
    allColumnIds <- numericData$covariates %>%
      dplyr::pull(.data$covariateId) %>%
      unique() %>%
      sort()
    completeIds <- expand.grid(rowId = allRowIds, covariateId = allColumnIds)
    numericData$covariates <- merge(completeIds, numericData$covariates,
      all.x = TRUE
    )

    if (featureEngineeringSettings$method == "mean") {
      numericData$imputedValues <- numericData$covariates %>%
          dplyr::group_by(.data$covariateId) %>%
          dplyr::summarise(imputedValues = mean(.data$covariateValue, na.rm = TRUE))
    } else if (featureEngineeringSettings$method == "median") {
      numericData$imputedValues <- numericData$covariates %>%
          dplyr::group_by(.data$covariateId) %>%
          dplyr::collect() %>% # median not possible in sql
          dplyr::summarise(imputedValues = median(.data$covariateValue, na.rm = TRUE))
    }
      
      
      numericData$imputedCovariates <- numericData$covariates %>%
        dplyr::left_join(numericData$imputedValues, by = "covariateId") %>%
        dplyr::group_by(.data$covariateId) %>%
        dplyr::mutate(imputedValue = ifelse(is.na(.data$covariateValue),
          .data$imputedValues,
          .data$covariateValue
        )) %>% 
        dplyr::select(-c("imputedValues"))
    Andromeda::appendToTable(
      trainData$covariateData$covariates,
      numericData$imputedCovariates %>%
        dplyr::filter(is.na(.data$covariateValue)) %>%
        dplyr::mutate(covariateValue = .data$imputedValue) %>%
        dplyr::select(-c("imputedValue"))
    )
    attr(featureEngineeringSettings, "missingInfo") <-
      trainData$covariateData$missingInfo %>%
      dplyr::collect()
    attr(featureEngineeringSettings, "imputer") <- 
      numericData$imputedValues %>% dplyr::collect()
    done <- TRUE
  } else {
    trainData$covariateData$missingInfo <- attr(
      featureEngineeringSettings,
      "missingInfo"
    )
    on.exit(trainData$covariateData$missingInfo <- NULL, add = TRUE)
    trainData$covariateData$covariates <- trainData$covariateData$covariates %>%
      dplyr::left_join(trainData$covariateData$missingInfo, by = "covariateId") %>%
      dplyr::filter(is.na(.data$missing) ||
        .data$missing <= featureEngineeringSettings$missingThreshold) %>%
      dplyr::select(-"missing")

    continuousFeatures <- trainData$covariateData$analysisRef %>%
      dplyr::filter(.data$isBinary == "N") %>%
      dplyr::select("analysisId") %>%
      dplyr::inner_join(trainData$covariateData$covariateRef, by = "analysisId") %>%
      dplyr::pull(.data$covariateId)
    featureData <- separateFeatures(trainData, continuousFeatures)
    numericData <- featureData[[1]]
    on.exit(numericData <- NULL, add = TRUE)
    # impute missing values
    allRowIds <- numericData$covariates %>%
      dplyr::pull(.data$rowId) %>%
      unique() %>%
      sort()
    allColumnIds <- numericData$covariates %>%
      dplyr::pull(.data$covariateId) %>%
      unique() %>%
      sort()
    completeIds <- expand.grid(rowId = allRowIds, covariateId = allColumnIds)
    numericData$covariates <- merge(completeIds, numericData$covariates,
      all.x = TRUE
    ) 
    numericData$imputedValues <- attr(featureEngineeringSettings, "imputer")
    numericData$imputedCovariates <- numericData$covariates %>%
        dplyr::left_join(numericData$imputedValues, by = "covariateId") %>%
        dplyr::group_by(.data$covariateId) %>%
        dplyr::mutate(imputedValue = ifelse(is.na(.data$covariateValue),
          .data$imputedValues,
          .data$covariateValue
        )) %>% 
        dplyr::select(-c("imputedValues"))
    Andromeda::appendToTable(
      trainData$covariateData$covariates,
      numericData$imputedCovariates %>%
        dplyr::filter(is.na(.data$covariateValue)) %>%
        dplyr::mutate(covariateValue = .data$imputedValue) %>%
        dplyr::select(-c("imputedValue"))
    )

  }
  featureEngineering <- list(
    funct = "simpleImpute",
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings,
      done = done
    )
  )
  attr(trainData, "metaData")$featureEngineering[["simpleImputer"]] <-
    featureEngineering
  return(trainData)
}


#' @title Imputation
#' @description This function does single imputation with predictive mean matchin
#' @param trainData The data to be imputed
#' @param featureEngineeringSettings The settings for the imputation
#' @param done Whether the imputation has already been done (bool)
#' @return The imputed data
#' @keywords internal
iterativeImpute <- function(trainData, featureEngineeringSettings, done = FALSE) {
  if (!done) {
    missingInfo <- extractMissingInfo(trainData)
    trainData$covariateData$missingInfo <- missingInfo$missingInfo
    continuousFeatures <- missingInfo$continuousFeatures
    on.exit(trainData$covariateData$missingInfo <- NULL, add = TRUE)

    trainData$covariateData$covariates <- trainData$covariateData$covariates %>%
      dplyr::left_join(trainData$covariateData$missingInfo, by = "covariateId") %>%
      dplyr::filter(is.na(.data$missing) ||
        .data$missing <= featureEngineeringSettings$missingThreshold) %>%
      dplyr::select(-"missing")

    # separate the continuous and binary features
    featureData <- separateFeatures(trainData, continuousFeatures)
    numericData <- featureData[[1]]
    binary <- featureData[[2]]
    on.exit(numericData <- NULL, add = TRUE)
    on.exit(binary <- NULL, add = TRUE)
  
    numericData <- initializeImputation(numericData, "mean")
    # add imputed values in data
    iterativeImputeResults <- iterativeChainedImpute(numericData,
      binary,
      trainData,
      featureEngineeringSettings,
      direction = "ascending",
      iterations = 5
    )

    Andromeda::appendToTable(
      trainData$covariateData$covariates,
      iterativeImputeResults$numericData$imputedCovariates %>%
        dplyr::filter(is.na(.data$covariateValue)) %>%
        dplyr::mutate(covariateValue = .data$imputedValue) %>%
        dplyr::select(-c("imputedValue"))
    )


    attr(featureEngineeringSettings, "missingInfo") <-
      trainData$covariateData$missingInfo %>%
      dplyr::collect()
    attr(featureEngineeringSettings, "imputer") <- iterativeImputeResults$modelInfo
    attr(featureEngineeringSettings, "kdeEstimates") <- iterativeImputeResults$kdeEstimates
    done <- TRUE
  } else {
    # remove data with more than missingThreshold
    trainData$covariateData$missingInfo <- attr(
      featureEngineeringSettings,
      "missingInfo"
    )
    on.exit(trainData$covariateData$missingInfo <- NULL, add = TRUE)
    trainData$covariateData$covariateIsBinary <- trainData$covariateData$covariateRef %>%
      dplyr::select("covariateId", "analysisId") %>%
      dplyr::inner_join(
        trainData$covariateData$analysisRef %>%
          dplyr::select("analysisId", "isBinary"),
        by = "analysisId"
      ) %>%
      dplyr::mutate(isBinary = .data$isBinary == "Y") %>%
      dplyr::select(.data$covariateId, .data$isBinary) %>%
      dplyr::compute()
    on.exit(trainData$covariateData$covariateIsBinary <- NULL, add = TRUE)
    trainData$covariateData$covariates <- trainData$covariateData$covariates %>%
      dplyr::left_join(trainData$covariateData$missingInfo, by = "covariateId") %>%
      dplyr::left_join(trainData$covariateData$covariateIsBinary, by = "covariateId") %>%
      dplyr::filter(
        (!is.na(.data$missing) && .data$missing <= featureEngineeringSettings$missingThreshold) ||
          (is.na(.data$missing) && .data$isBinary)
      ) %>%
      dplyr::select(-"missing", -"isBinary")

    continuousFeatures <- trainData$covariateData$analysisRef %>%
      dplyr::filter(.data$isBinary == "N") %>%
      dplyr::select("analysisId") %>%
      dplyr::inner_join(trainData$covariateData$covariateRef, by = "analysisId") %>%
      dplyr::pull(.data$covariateId)
    featureData <- separateFeatures(trainData, continuousFeatures)
    numericData <- featureData[[1]]
    binary <- featureData[[2]]
    on.exit(numericData <- NULL, add = TRUE)
    on.exit(binary <- NULL, add = TRUE)
    # impute missing values
    allRowIds <- numericData$covariates %>%
      dplyr::pull(.data$rowId) %>%
      unique() %>%
      sort()
    allColumnIds <- numericData$covariates %>%
      dplyr::pull(.data$covariateId) %>%
      unique() %>%
      sort()
    completeIds <- expand.grid(rowId = allRowIds, covariateId = allColumnIds)
    # now we have NAs for missing combinations
    numericData$covariates <- merge(completeIds, numericData$covariates,
      all.x = TRUE
    )

    # get index of NAs for every feature to be imputed
    numericData$missingIndex <- numericData$covariates %>%
      dplyr::filter(is.na(.data$covariateValue)) %>%
      dplyr::select(-c("covariateValue"))
    on.exit(numericData$missingIndex <- NULL, add = TRUE)

    numericData$imputedCovariates <- numericData$covariates %>%
      dplyr::group_by(.data$covariateId) %>%
      dplyr::mutate(imputedValue = .data$covariateValue)
    on.exit(numericData$imputedCovariates <- NULL, add = TRUE)


    varsToImpute <- numericData$missingIndex %>%
      dplyr::pull(.data$covariateId) %>%
      unique()
    for (varId in varsToImpute) {
      varName <- trainData$covariateData$covariateRef %>%
        dplyr::filter(.data$covariateId == varId) %>%
        dplyr::pull(.data$covariateName)
      ParallelLogger::logInfo("Imputing variable: ", varName)
      numericData$y <- numericData$covariates %>%
        dplyr::filter(.data$covariateId == varId) %>%
        dplyr::mutate(y = .data$covariateValue) %>%
        dplyr::select("y", "rowId")
      on.exit(numericData$y <- NULL, add = TRUE)
      missIdx <- which(is.na(numericData$y %>% dplyr::pull(.data$y)))
      numericData$X <- numericData$covariates %>%
        dplyr::filter(.data$covariateId != varId)
      on.exit(numericData$X <- NULL, add = TRUE)
      Andromeda::appendToTable(numericData$X, binary$covariates)
      numericData$xMiss <- numericData$X %>% 
        dplyr::filter(.data$rowId %in% !!allRowIds[missIdx])
      on.exit(numericData$xMiss <- NULL, add = TRUE)

      imputer <- attr(featureEngineeringSettings, "imputer")[[as.character(varId)]]
      pmmResults <- pmmPredict(numericData, k = 5, imputer)

      # update imputations in data
      numericData$imputedValues <- pmmResults$imputedValues
      on.exit(numericData$imputedValues <- NULL, add = TRUE)
      numericData$imputedCovariates <- numericData$imputedCovariates %>%
        dplyr::left_join(numericData$imputedValues,
          by = "rowId",
          suffix = c("", ".new")
        ) %>%
        dplyr::mutate(
          imputedValue =
            dplyr::if_else(.data$covariateId == varId &&
              !is.na(.data$imputedValue.new),
            .data$imputedValue.new,
            .data$imputedValue
            )
        ) %>%
        dplyr::select(-"imputedValue.new")
    }
    # add imputed values in data
    Andromeda::appendToTable(
      trainData$covariateData$covariates,
      numericData$imputedCovariates %>%
        dplyr::filter(is.na(.data$covariateValue)) %>%
        dplyr::mutate(covariateValue = .data$imputedValue) %>%
        dplyr::select(-c("imputedValue"))
    )
  }
  featureEngineering <- list(
    funct = "iterativeImpute",
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings,
      done = done
    )
  )
  attr(trainData, "metaData")$featureEngineering[["iterativeImputer"]] <-
    featureEngineering
  return(trainData)
}

#' @title Predictive mean matching using lasso
#' @param data An andromeda object with the following fields:
#'      xObs: covariates table for observed data
#'      xMiss: covariates table for missing data
#'      yObs: outcome variable that we want to impute
#' @param k The number of donors to use for matching (default 5)
#' @keywords internal
pmmFit <- function(data, k = 5) {
  rlang::check_installed("glmnet")
  data$rowMap <- data$xObs %>%
    dplyr::group_by(.data$rowId) %>%
    dplyr::summarise() %>%
    dplyr::mutate(
      oldRowId = .data$rowId,
      newRowId = dplyr::row_number()
    ) %>%
    dplyr::select(c("newRowId", "oldRowId")) %>%
    dplyr::compute()
  on.exit(data$rowMap <- NULL, add = TRUE)
  data$colMap <- data$xObs %>%
    dplyr::group_by(.data$covariateId) %>%
    dplyr::summarise() %>%
    dplyr::mutate(
      oldCovariateId = .data$covariateId,
      newCovariateId = dplyr::row_number()
    ) %>%
    dplyr::select(c("newCovariateId", "oldCovariateId"))
  on.exit(data$colMap <- NULL, add = TRUE)

  data$xObs <- data$xObs %>%
    dplyr::left_join(data$rowMap, by = c("rowId" = "oldRowId")) %>%
    dplyr::left_join(data$colMap, by = c("covariateId" = "oldCovariateId")) %>%
    dplyr::select(
      rowId = "newRowId",
      covariateId = "newCovariateId",
      covariateValue = "covariateValue"
    )

  xObs <- Matrix::sparseMatrix(
    i = data$xObs %>% dplyr::pull(.data$rowId),
    j = data$xObs %>% dplyr::pull(.data$covariateId),
    x = data$xObs %>% dplyr::pull(.data$covariateValue),
    dims = c(
      data$rowMap %>% dplyr::pull(.data$newRowId) %>% max(),
      data$colMap %>% dplyr::pull(.data$newCovariateId) %>% max()
    )
  )

  fit <- glmnet::cv.glmnet(xObs, data$yObs %>%
    dplyr::pull(.data$y), alpha = 1, nfolds = 3)

  # predict on both XObs and XMiss
  predsObs <- predict(fit, xObs, fit$lambda.min)
  data$xMiss <- data$xMiss %>%
    dplyr::left_join(
      data$xMiss %>%
        dplyr::group_by(.data$covariateId) %>%
        dplyr::summarise(
          n_unique = dplyr::n_distinct(.data$covariateValue),
          max = max(.data$covariateValue, na.rm = TRUE),
          min = min(.data$covariateValue, na.rm = TRUE),
        ),
      by = "covariateId"
    ) %>%
    dplyr::group_by(.data$covariateId) %>%
    dplyr::mutate(
      covariateValue = ifelse(.data$n_unique > 2 & (.data$max - .data$max) > 0,
        (.data$covariateValue - .data$min) / (.data$max - .data$min),
        .data$covariateValue
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c("n_unique", "min", "max"))
  data$rowMapMiss <- data$xMiss %>%
    dplyr::group_by(.data$rowId) %>%
    dplyr::summarise() %>%
    dplyr::mutate(
      oldRowId = .data$rowId,
      newRowId = dplyr::row_number()
    ) %>%
    dplyr::select(c("newRowId", "oldRowId")) %>%
    dplyr::compute()
  on.exit(data$rowMapMiss <- NULL, add = TRUE)
  data$xMiss <- data$xMiss %>%
    dplyr::left_join(data$rowMapMiss, by = c("rowId" = "oldRowId")) %>%
    dplyr::left_join(data$colMap, by = c("covariateId" = "oldCovariateId")) %>%
    dplyr::select(
      rowId = "newRowId",
      covariateId = "newCovariateId",
      covariateValue = "covariateValue"
    )

  xMiss <- Matrix::sparseMatrix(
    i = data$xMiss %>% dplyr::pull(.data$rowId),
    j = data$xMiss %>% dplyr::pull(.data$covariateId),
    x = data$xMiss %>% dplyr::pull(.data$covariateValue),
    dims = c(
      data$xMiss %>% dplyr::pull(.data$rowId) %>% max(),
      data$xMiss %>% dplyr::pull(.data$covariateId) %>% max()
    )
  )

  predsMiss <- predict(fit, xMiss, fit$lambda.min)

  # precompute mapping to use - straight from xId (row index) to
  # covariateValue of donor
  donorMapping <- data$rowMap %>%
    dplyr::inner_join(data$yObs, by = c("oldRowId" = "rowId"), copy = TRUE) %>%
    dplyr::pull(.data$y)
  # for each missing value, find the k closest observed values
  imputedValues <- numeric(nrow(xMiss))
  for (j in 1:nrow(xMiss)) {
    distances <- abs(predsObs - predsMiss[j])
    donorIndices <- order(distances)[1:k]
    donorValues <- donorMapping[donorIndices]
    imputedValues[j] <- sample(donorValues, 1)
  }

  results <- list()
  results$imputedValues <- data.frame(
    rowId = data$rowMapMiss %>%
      dplyr::pull(.data$oldRowId),
    imputedValue = imputedValues
  )
  bestIndex <- which(fit$lambda == fit$lambda.min)
  nonZero <- which(fit$glmnet.fit$beta[, bestIndex] != 0)
  nonZeroCovariateIds <- data$colMap %>%
    dplyr::filter(.data$newCovariateId %in% nonZero) %>%
    dplyr::pull(.data$oldCovariateId)
  results$model <- list(
    intercept = as.numeric(fit$glmnet.fit$a0[bestIndex]),
    coefficients = data.frame(
      covariateId = nonZeroCovariateIds,
      values = as.numeric(fit$glmnet.fit$beta[nonZero, bestIndex])
    ),
    predictions = data.frame(
      rowId = data$rowMap %>%
        dplyr::pull(.data$oldRowId),
      prediction = as.numeric(predsObs)
    )
  )
  return(results)
}

pmmPredict <- function(data, k = 5, imputer) {
  data$coefficients <- imputer$coefficients
  predictionMissing <- data$xMiss %>%
    dplyr::inner_join(data$coefficients, by = "covariateId") %>%
    dplyr::mutate(values = .data$covariateValue * .data$values) %>%
    dplyr::group_by(.data$rowId) %>%
    dplyr::summarise(value = sum(.data$values, na.rm = TRUE)) %>%
    dplyr::select("rowId", "value")
  predictionMissing <- as.data.frame(predictionMissing)
  predictionMissing$value <- predictionMissing$value + imputer$intercept


  # precompute mapping to use - straight from xId (row index) to
  # covariateValue of donor
  donorMapping <- imputer$predictions %>% dplyr::pull(.data$prediction)

  # for each missing value, find the k closest observed values
  nRows <- data$xMiss %>%
    dplyr::pull(.data$rowId) %>%
    dplyr::n_distinct()
  imputedValues <- numeric(nRows)
  predsObs <- imputer$predictions$prediction
  for (j in 1:nRows) {
    distances <- abs(predsObs - predictionMissing$value[j])
    donorIndices <- order(distances)[1:k]
    donorValues <- donorMapping[donorIndices]
    imputedValues[j] <- sample(donorValues, 1)
  }

  results <- list()
  results$imputedValues <- data.frame(
    rowId = predictionMissing %>%
      dplyr::pull(.data$rowId),
    imputedValue = imputedValues
  )
  return(results)
}

extractMissingInfo <- function(trainData) {
  total <- trainData$covariateData$covariates %>%
    dplyr::summarise(total = dplyr::n_distinct(.data$rowId)) %>%
    dplyr::pull()
  continuousFeatures <- trainData$covariateData$analysisRef %>%
    dplyr::filter(.data$isBinary == "N") %>%
    
    dplyr::select("analysisId") %>%
    dplyr::inner_join(trainData$covariateData$covariateRef, by = "analysisId") %>%
    dplyr::pull(.data$covariateId)

  missingInfo <- trainData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId %in% continuousFeatures) %>%
    dplyr::group_by(.data$covariateId) %>%
    dplyr::summarise(counts = dplyr::n()) %>%
    dplyr::collect() %>% # necessary because of integer division in sqlite
    dplyr::mutate(missing = 1 - .data$counts / total) %>%
    dplyr::select(c("covariateId", "missing"))
  results <- list(
    "missingInfo" = missingInfo,
    "continuousFeatures" = continuousFeatures
  )
  return(results)
}

separateFeatures <- function(trainData, continuousFeatures) {
    numericData <- Andromeda::andromeda()
    numericData$covariates <- trainData$covariateData$covariates %>%
      dplyr::filter(.data$covariateId %in% continuousFeatures)
    numericData$covariateRef <- trainData$covariateData$covariateRef %>%
      dplyr::filter(.data$covariateId %in% continuousFeatures)

    binaryData <- Andromeda::andromeda()
    binaryData$covariates <- trainData$covariateData$covariates %>%
      dplyr::filter(!.data$covariateId %in% !!continuousFeatures)
    binaryData$covariateRef <- trainData$covariateData$covariateRef %>%
      dplyr::filter(!.data$covariateId %in% !!continuousFeatures)
    return(list(numericData, binaryData))
}

initializeImputation <- function(numericData, method = "mean") {
    allRowIds <- numericData$covariates %>%
      dplyr::pull(.data$rowId) %>%
      unique() %>%
      sort()
    allColumnIds <- numericData$covariates %>%
      dplyr::pull(.data$covariateId) %>%
      unique() %>%
      sort()
    completeIds <- expand.grid(rowId = allRowIds, covariateId = allColumnIds)
    numericData$covariates <- merge(completeIds, numericData$covariates,
      all.x = TRUE
    )

    # get index of NAs for every feature to be imputed
    numericData$missingIndex <- numericData$covariates %>%
      dplyr::filter(is.na(.data$covariateValue)) %>%
      dplyr::select(-c("covariateValue"))

    if (method == "mean") {
    numericData$imputedCovariates <- numericData$covariates %>%
      dplyr::group_by(.data$covariateId) %>%
      dplyr::mutate(imputedValue = ifelse(is.na(.data$covariateValue),
        mean(.data$covariateValue, na.rm = TRUE),
        .data$covariateValue
      ))
    } else {
    stop("Unknown initialization method: ", method)
    }
    return(numericData)
}

# Main (M)ICE algorithm - iterative imputation with chained equations
iterativeChainedImpute <- function(numericData,
                            binaryData,
                            originalData,
                            featureEngineeringSettings,
                            direction = "ascending",
                            iterations = 5) {
    prevImputations <- list()
    allRowIds <- numericData$covariates %>%
      dplyr::pull(.data$rowId) %>%
      unique() %>%
      sort()
    maxIter <- iterations# TODO check
    varsToImpute <- numericData$missingIndex %>%
      dplyr::pull(.data$covariateId) %>%
      unique()
    convergenceParameters <- list()
    modelInfo <- list()

    for (iter in 1:maxIter) {
      ParallelLogger::logInfo("Imputation iteration: ", iter)
      currentImputations <- list()

      # TODO do in order from least missing to most missing
      for (varId in varsToImpute) {
        varName <- originalData$covariateData$covariateRef %>%
          dplyr::filter(.data$covariateId == varId) %>%
          dplyr::pull(.data$covariateName)
        ParallelLogger::logInfo("Imputing variable: ", varName)
        numericData$y <- numericData$covariates %>%
          dplyr::filter(.data$covariateId == varId) %>%
          dplyr::mutate(y = .data$covariateValue) %>%
          dplyr::select("y", "rowId")
        on.exit(numericData$y <- NULL, add = TRUE)
        obsIdx <- which(!is.na(numericData$y %>% dplyr::pull(.data$y)))
        missIdx <- which(is.na(numericData$y %>% dplyr::pull(.data$y)))
        numericData$yObs <- numericData$y %>%
          dplyr::filter(.data$rowId %in% !!allRowIds[obsIdx])
        on.exit(numericData$yObs <- NULL, add = TRUE)

        numericData$X <- numericData$imputedCovariates %>%
          dplyr::filter(.data$covariateId != varId) %>%
          dplyr::mutate(covariateValue = .data$imputedValue) %>%
          dplyr::select(-c("imputedValue"))
        on.exit(numericData$X <- NULL, add = TRUE)
        Andromeda::appendToTable(numericData$X, binaryData$covariates)
        numericData$xObs <- numericData$X %>% dplyr::filter(.data$rowId %in% !!allRowIds[obsIdx])
        on.exit(numericData$xObs <- NULL, add = TRUE)
        numericData$xMiss <- numericData$X %>% dplyr::filter(.data$rowId %in% !!allRowIds[missIdx])
        on.exit(numericData$xMiss <- NULL, add = TRUE)

        pmmResults <- pmmFit(numericData, k = 5)

        # update imputations in data
        numericData$imputedValues <- pmmResults$imputedValues
        on.exit(numericData$imputedValues <- NULL, add = TRUE)
        numericData$imputedCovariates <- numericData$imputedCovariates %>%
          dplyr::left_join(numericData$imputedValues,
            by = "rowId",
            suffix = c("", ".new")
          ) %>%
          dplyr::mutate(
            imputedValue =
              dplyr::if_else(.data$covariateId == varId &&
                !is.na(.data$imputedValue.new),
              .data$imputedValue.new,
              .data$imputedValue
              )
          ) %>%
          dplyr::select(-"imputedValue.new")

        # store current imputations for convergence check
        currentImputations[[as.character(varId)]] <- pmmResults$imputedValues$imputedValue

        # store pmm info for each variable
        modelInfo[[as.character(varId)]] <- pmmResults$model
      }

      # save values for convergence checking afterwards
      # store mean and variance of imputed values for each variable
      # as well as average change from previous iteration
      meanVector <- numeric(length(varsToImpute))
      varVector <- numeric(length(varsToImpute))
      idx <- 1
      for (varId in varsToImpute) {
        currentImputation <- currentImputations[[as.character(varId)]]
        meanVector[idx] <- mean(currentImputation)
        varVector[idx] <- stats::var(currentImputation)
        idx <- idx + 1
      }
      convergenceInfo <- list(
        meanVector = meanVector,
        varVector = varVector
      )
      if (iter > 1) {
        meanVarChange <- numeric(length(varsToImpute))
        for (varId in varsToImpute) {
          prevImputation <- prevImputations[[as.character(varId)]]
          currentImputation <- currentImputations[[as.character(varId)]]
          meanVarChange <- c(
            meanVarChange,
            mean(abs(currentImputation - prevImputation))
          )
        }
        convergenceInfo$meanVarChange <- meanVarChange
      }
      convergenceParameters[[iter]] <- convergenceInfo

      prevImputations <- currentImputations
    }

    # calculate kde estimates of imputed and observed distributions per imputed variable
    # and store in featureEngineeringSettings
    kdeEstimates <- list()
    for (varId in varsToImpute) {
      varName <- originalData$covariateData$covariateRef %>%
        dplyr::filter(.data$covariateId == varId) %>%
        dplyr::pull(.data$covariateName)
      rows <- numericData$missingIndex %>%
        dplyr::filter(.data$covariateId == varId) %>%
        dplyr::pull(.data$rowId)
      imputedValues <- numericData$imputedCovariates %>%
        dplyr::filter(
          .data$covariateId == varId,
          .data$rowId %in% rows
        ) %>%
        dplyr::pull(.data$imputedValue)
      observedValues <- numericData$covariates %>%
        dplyr::filter(
          .data$covariateId == varId,
          !is.na(.data$covariateValue)
        ) %>%
        dplyr::pull(.data$covariateValue)
      kdeEstimates[[as.character(varId)]] <- list(
        imputed = stats::density(imputedValues),
        observed = stats::density(observedValues)
      )
    }
  results <- list(
    "numericData" = numericData,
    "convergenceParameters" = convergenceParameters,
    "modelInfo" = modelInfo,
    "kdeEstimates" = kdeEstimates
  )
  return(results)
}
