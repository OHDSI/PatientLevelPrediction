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
#' @param methodSettings A list of settings for the imputation method to use.
#' Currently only "pmm" is supported with the following settings:
#' - k: The number of donors to use for matching
#' - iterations: The number of iterations to use for imputation
#' @return The settings for the iterative imputer of class `featureEngineeringSettings`
#' @examples
#' # create imputer to impute values with missingness less than 30% using 
#' # predictive mean matching in 5 iterations with 5 donors
#' createIterativeImputer(missingThreshold = 0.3, method = "pmm",
#'                        methodSettings = list(pmm = list(k = 5, iterations = 5)))
#' @export
createIterativeImputer <- function(missingThreshold = 0.3,
                                   method = "pmm",
                                   methodSettings = list(
                                      pmm = list(
                                        k = 5,
                                        iterations = 5
                                      )
                                   )) {
  ParallelLogger::logWarn("Imputation is experimental and may have bugs. 
    Please report any issues on the GitHub repository.")
  checkIsClass(missingThreshold, "numeric")
  checkInStringVector(method, c("pmm"))
  checkIsClass(methodSettings, "list")
  if (method == "pmm") {
    checkIsClass(methodSettings$pmm$k, "numeric")
    checkHigher(methodSettings$pmm$k, 0)
    checkIsClass(methodSettings$pmm$iterations, "numeric")
    checkHigher(methodSettings$pmm$iterations, 0)
  }
  checkHigher(missingThreshold, 0)
  checkLower(missingThreshold, 1)
  featureEngineeringSettings <- list(
    missingThreshold = missingThreshold,
    method = method,
    methodSettings = methodSettings[[method]]
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
#' @examples
#' # create imputer to impute values with missingness less than 10% using the median
#' # of observed values
#' createSimpleImputer(method = "median", missingThreshold = 0.10)
#' @export
createSimpleImputer <- function(method = "mean",
                                missingThreshold = 0.3) {
  ParallelLogger::logWarn("Imputation is experimental and may have bugs,
    please report any issues on the GitHub repository.")
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
  start <- Sys.time()
  if (!done) {
    ParallelLogger::logInfo("Imputing missing values with simpleImputer using: ",
      featureEngineeringSettings$method, " and missing threshold: ",
      featureEngineeringSettings$missingThreshold)
    outputData <- list(
      labels = trainData$labels,
      folds = trainData$folds,
      covariateData = Andromeda::copyAndromeda(trainData$covariateData)
    )
    class(outputData) <- "plpData"
    attributes(outputData) <- attributes(trainData)
    attributes(outputData$covariateData) <- attributes(trainData$covariateData)
    class(outputData$covariateData) <- "CovariateData"
    missingInfo <- extractMissingInfo(outputData)
    outputData$covariateData$missingInfo <- missingInfo$missingInfo
    continuousFeatures <- missingInfo$continuousFeatures
    on.exit(outputData$covariateData$missingInfo <- NULL, add = TRUE)

    outputData$covariateData$covariates <- outputData$covariateData$covariates %>%
      dplyr::left_join(outputData$covariateData$missingInfo, by = "covariateId") %>%
      dplyr::filter(is.na(.data$missing) ||
        .data$missing <= featureEngineeringSettings$missingThreshold) %>%
      dplyr::select(-"missing")

    # separate the continuous and binary features
    featureData <- separateFeatures(outputData, continuousFeatures)
    numericData <- featureData[[1]]
    on.exit(numericData <- NULL, add = TRUE)

    allRowIds <- trainData$labels$rowId
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
        dplyr::summarise(imputedValues = stats::median(.data$covariateValue, na.rm = TRUE))
    }

    allRowIds <- outputData$labels$rowId
    allColumnIds <- numericData$covariates %>%
      dplyr::pull(.data$covariateId) %>%
      unique() %>%
      sort()
    completeIds <- expand.grid(rowId = allRowIds, covariateId = allColumnIds)
    numericData$covariates <- merge(completeIds, numericData$covariates,
      all.x = TRUE
    )

    numericData$imputedCovariates <- numericData$covariates %>%
      dplyr::left_join(numericData$imputedValues, by = "covariateId") %>%
      dplyr::group_by(.data$covariateId) %>%
      dplyr::mutate(imputedValue = ifelse(is.na(.data$covariateValue),
        .data$imputedValues,
        .data$covariateValue
      )) %>%
      dplyr::select(-c("imputedValues"))
    Andromeda::appendToTable(
      outputData$covariateData$covariates,
      numericData$imputedCovariates %>%
        dplyr::filter(is.na(.data$covariateValue)) %>%
        dplyr::mutate(covariateValue = .data$imputedValue) %>%
        dplyr::select(-c("imputedValue"))
    )
    attr(featureEngineeringSettings, "missingInfo") <-
      outputData$covariateData$missingInfo %>%
      dplyr::collect()
    attr(featureEngineeringSettings, "imputer") <-
      numericData$imputedValues %>% dplyr::collect()
    done <- TRUE
  } else {
    ParallelLogger::logInfo("Applying imputation to test data with simpleImputer
      using method: ", featureEngineeringSettings$method, " and missing threshold: ",
      featureEngineeringSettings$missingThreshold)
    outputData <- list(
      labels = trainData$labels,
      covariateData = Andromeda::copyAndromeda(trainData$covariateData)
    )
    class(outputData) <- "plpData"
    attributes(outputData) <- attributes(trainData)
    attributes(outputData$covariateData) <- attributes(trainData$covariateData)
    class(outputData$covariateData) <- "CovariateData"
    outputData$covariateData$missingInfo <- attr(
      featureEngineeringSettings,
      "missingInfo"
    )
    on.exit(outputData$covariateData$missingInfo <- NULL, add = TRUE)
    outputData$covariateData$covariates <- outputData$covariateData$covariates %>%
      dplyr::left_join(outputData$covariateData$missingInfo, by = "covariateId") %>%
      dplyr::filter(is.na(.data$missing) ||
        .data$missing <= featureEngineeringSettings$missingThreshold) %>%
      dplyr::select(-"missing")

    continuousFeatures <- outputData$covariateData$analysisRef %>%
      dplyr::filter(.data$isBinary == "N") %>%
      dplyr::select("analysisId") %>%
      dplyr::inner_join(outputData$covariateData$covariateRef, by = "analysisId") %>%
      dplyr::pull(.data$covariateId)
    featureData <- separateFeatures(outputData, continuousFeatures)
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
      outputData$covariateData$covariates,
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
  attr(outputData$covariateData, "metaData")$featureEngineering[["simpleImputer"]] <-
    featureEngineering
  delta <- Sys.time() - start
  ParallelLogger::logInfo("Imputation done in time: ", signif(delta, 3), " ",
  attr(delta, "units"))
  return(outputData)
}


#' @title Imputation
#' @description This function does single imputation with predictive mean matchin
#' @param trainData The data to be imputed
#' @param featureEngineeringSettings The settings for the imputation
#' @param done Whether the imputation has already been done (bool)
#' @return The imputed data
#' @keywords internal
iterativeImpute <- function(trainData, featureEngineeringSettings, done = FALSE) {
  start <- Sys.time()
  if (!done) {
    ParallelLogger::logInfo("Imputing missing values with iterativeImputer using: ",
      featureEngineeringSettings$method, ", missing threshold: ",
      featureEngineeringSettings$missingThreshold, " and method settings: ",
      featureEngineeringSettings$methodSettings)
    outputData <- list(
      labels = trainData$labels,
      folds = trainData$folds,
      covariateData = Andromeda::copyAndromeda(trainData$covariateData)
    )
    class(outputData) <- "plpData"
    attributes(outputData) <- attributes(trainData)
    attributes(outputData$covariateData) <- attributes(trainData$covariateData)
    class(outputData$covariateData) <- "CovariateData"
    missingInfo <- extractMissingInfo(outputData)
    outputData$covariateData$missingInfo <- missingInfo$missingInfo
    continuousFeatures <- missingInfo$continuousFeatures
    on.exit(outputData$covariateData$missingInfo <- NULL, add = TRUE)

    outputData$covariateData$covariates <- outputData$covariateData$covariates %>%
      dplyr::left_join(outputData$covariateData$missingInfo, by = "covariateId") %>%
      dplyr::filter(is.na(.data$missing) ||
        .data$missing <= featureEngineeringSettings$missingThreshold) %>%
      dplyr::select(-"missing")

    # separate the continuous and binary features
    featureData <- separateFeatures(outputData, continuousFeatures)
    numericData <- featureData[[1]]
    binary <- featureData[[2]]
    on.exit(numericData <- NULL, add = TRUE)
    on.exit(binary <- NULL, add = TRUE)

    numericData <- initializeImputation(numericData, "mean",
      labels = outputData$labels
    )
    # add imputed values in data
    iterativeImputeResults <- iterativeChainedImpute(numericData,
      binary,
      outputData,
      featureEngineeringSettings,
      direction = "ascending",
      iterations = featureEngineeringSettings$methodSettings$iterations
    )

    Andromeda::appendToTable(
      outputData$covariateData$covariates,
      iterativeImputeResults$numericData$imputedCovariates %>%
        dplyr::filter(is.na(.data$covariateValue)) %>%
        dplyr::mutate(covariateValue = .data$imputedValue) %>%
        dplyr::select(-c("imputedValue"))
    )


    attr(featureEngineeringSettings, "missingInfo") <-
      outputData$covariateData$missingInfo %>%
      dplyr::collect()
    attr(featureEngineeringSettings, "imputer") <- iterativeImputeResults$modelInfo
    attr(featureEngineeringSettings, "kdeEstimates") <- iterativeImputeResults$kdeEstimates
    done <- TRUE
  } else {
    ParallelLogger::logInfo("Applying imputation to test data with iterativeImputer
      using method: ", featureEngineeringSettings$method, " and missing threshold: ",
      featureEngineeringSettings$missingThreshold)
    outputData <- list(
      labels = trainData$labels,
      covariateData = Andromeda::copyAndromeda(trainData$covariateData)
    )
    class(outputData) <- "plpData"
    attributes(outputData) <- attributes(trainData)
    attributes(outputData$covariateData) <- attributes(trainData$covariateData)
    class(outputData$covariateData) <- "CovariateData"
    # remove data with more than missingThreshold
    outputData$covariateData$missingInfo <- attr(
      featureEngineeringSettings,
      "missingInfo"
    )
    on.exit(outputData$covariateData$missingInfo <- NULL, add = TRUE)
    outputData$covariateData$covariateIsBinary <- outputData$covariateData$covariateRef %>%
      dplyr::select("covariateId", "analysisId") %>%
      dplyr::inner_join(
        outputData$covariateData$analysisRef %>%
          dplyr::select("analysisId", "isBinary"),
        by = "analysisId"
      ) %>%
      dplyr::mutate(isBinary = .data$isBinary == "Y") %>%
      dplyr::select("covariateId", "isBinary") %>%
      dplyr::compute()
    on.exit(outputData$covariateData$covariateIsBinary <- NULL, add = TRUE)
    outputData$covariateData$covariates <- outputData$covariateData$covariates %>%
      dplyr::left_join(outputData$covariateData$missingInfo, by = "covariateId") %>%
      dplyr::left_join(outputData$covariateData$covariateIsBinary, by = "covariateId") %>%
      dplyr::filter(
        (!is.na(.data$missing) && .data$missing <= featureEngineeringSettings$missingThreshold) ||
          (is.na(.data$missing) && .data$isBinary)
      ) %>%
      dplyr::select(-"missing", -"isBinary")

    continuousFeatures <- outputData$covariateData$analysisRef %>%
      dplyr::filter(.data$isBinary == "N") %>%
      dplyr::select("analysisId") %>%
      dplyr::inner_join(outputData$covariateData$covariateRef, by = "analysisId") %>%
      dplyr::pull(.data$covariateId)
    featureData <- separateFeatures(outputData, continuousFeatures)
    numericData <- featureData[[1]]
    binary <- featureData[[2]]
    on.exit(numericData <- NULL, add = TRUE)
    on.exit(binary <- NULL, add = TRUE)
    # impute missing values
    allRowIds <- outputData$labels$rowId
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
      varName <- outputData$covariateData$covariateRef %>%
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

      imputer <-
        attr(featureEngineeringSettings, "imputer")[[as.character(varId)]]
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
      outputData$covariateData$covariates,
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
  attr(outputData$covariateData, "metaData")$featureEngineering[["iterativeImputer"]] <-
    featureEngineering
  delta <- Sys.time() - start
  ParallelLogger::logInfo("Imputation done in time: ", signif(delta, 3), " ",
  attr(delta, "units"))
  return(outputData)
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
  predsObs <- stats::predict(fit, xObs, fit$lambda.min)
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

  predsMiss <- stats::predict(fit, xMiss, fit$lambda.min)

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
  if (length(nonZero) == 0) {
    ParallelLogger::logWarn("Imputation model only has intercept. It does not fit the data well")
  }
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
    dplyr::left_join(data$coefficients, by = "covariateId") %>%
    dplyr::mutate(values = .data$covariateValue * .data$values) %>%
    dplyr::group_by(.data$rowId) %>%
    dplyr::summarise(value = sum(.data$values, na.rm = TRUE)) %>%
    # rowId without any of nonzero coefficient will have NA
    # and should use only intercept for prediction
    dplyr::mutate(value = ifelse(is.na(.data$value), 0, .data$value)) %>%
    dplyr::select("rowId", "value")
  predictionMissing <- as.data.frame(predictionMissing)
  if (length(predictionMissing$value) == 0) {
    # prediction model for imputing only has intercept
    ParallelLogger::logWarn("Imputation model only has intercept,
      imputing with intercept. Something went wrong during fitting probably.")
    predictionMissing <- data.frame(
      rowId = data$xMiss %>%
        dplyr::pull(.data$rowId) %>% 
        unique(),
      value = imputer$intercept
    )
  } else {
    predictionMissing$value <- predictionMissing$value + imputer$intercept
  }

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
  ParallelLogger::logInfo("Calculating missingness in data")
  total <- trainData$covariateData$covariates %>%
    dplyr::summarise(total = dplyr::n_distinct(.data$rowId)) %>%
    dplyr::pull()
  continuousFeatures <- trainData$covariateData$analysisRef %>%
    dplyr::filter(.data$isBinary == "N",
                  .data$missingMeansZero == "N") %>%
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
  ParallelLogger::logInfo("Found ", nrow(missingInfo), " features with missing values")

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

initializeImputation <- function(numericData, method = "mean", labels) {
  allRowIds <- labels$rowId
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
  maxIter <- iterations # TODO check
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

      pmmResults <- pmmFit(numericData, k = featureEngineeringSettings$methodSettings$k)

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
