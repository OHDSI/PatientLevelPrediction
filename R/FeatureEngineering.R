# @file FeatureEngineering.R
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


featureEngineer <- function(data, featureEngineeringSettings) {
  start <- Sys.time()
  ParallelLogger::logInfo("Starting Feature Engineering")

  # if a single setting, make it a list
  if (inherits(featureEngineeringSettings, "featureEngineeringSettings")) {
    featureEngineeringSettings <- list(featureEngineeringSettings)
  }

  for (featureEngineeringSetting in featureEngineeringSettings) {
    fun <- attr(featureEngineeringSetting, "fun")
    args <- list(
      trainData = data,
      featureEngineeringSettings = featureEngineeringSetting
    )
    ParallelLogger::logInfo(paste0("Applying ", fun))
    data <- do.call(eval(parse(text = fun)), args)
  }

  attr(data, "metaData")$featureEngineeringSettings <- featureEngineeringSettings
  delta <- Sys.time() - start
  ParallelLogger::logInfo("Feature Engineering completed in ",
    signif(delta, 3), " ", attr(delta, "units"))
  return(data)
}

#' Create the settings for defining any feature engineering that will be done
#'
#' @details
#' Returns an object of class \code{featureEngineeringSettings} that specifies the sampling function that will be called and the settings
#'
#' @param type              (character) Choice of:  \itemize{
#'                                         \item'none' No feature engineering - this is the default
#'                                         }
#'
#' @return
#' An object of class \code{featureEngineeringSettings}
#' @examples
#' createFeatureEngineeringSettings(type = "none")
#' @export
createFeatureEngineeringSettings <- function(type = "none") {
  featureEngineeringSettings <- list()

  if (type == "none") {
    attr(featureEngineeringSettings, "fun") <- "sameData"
  }
  class(featureEngineeringSettings) <- "featureEngineeringSettings"
  return(featureEngineeringSettings)
}


#' Create the settings for defining any feature selection that will be done
#'
#' @details
#' Returns an object of class \code{featureEngineeringSettings} that specifies 
#' the function that will be called and the settings. Uses the scikit-learn
#' SelectKBest function with chi2 for univariate feature selection.
#'
#' @param k              This function returns the K features most associated 
#' (univariately) to the outcome
#'
#' @return
#' An object of class \code{featureEngineeringSettings}
#' @examplesIf rlang::is_installed("reticulate") && reticulate::py_module_available("sklearn")
#' # create a feature selection that selects the 100 most associated features
#' featureSelector <- createUnivariateFeatureSelection(k = 100) 
#' @export
createUnivariateFeatureSelection <- function(k = 100) {
  if (inherits(k, "numeric")) {
    k <- as.integer(k)
  }
  rlang::check_installed(
    "reticulate",
    reason = "This function requires the reticulate package to be installed"
  )
  tryCatch(
    {
      reticulate::import("sklearn")
    },
    error = function(e) {
      stop("This function requires the scikit-learn package to be installed")
    }
  )

  checkIsClass(k, "integer")
  checkHigherEqual(k, 0)

  featureEngineeringSettings <- list(k = k)

  attr(featureEngineeringSettings, "fun") <- "univariateFeatureSelection"
  class(featureEngineeringSettings) <- "featureEngineeringSettings"

  return(featureEngineeringSettings)
}

#' Create the settings for random foreat based feature selection
#'
#' @details
#' Returns an object of class `featureEngineeringSettings` that specifies the sampling function that will be called and the settings
#'
#' @param ntrees              number of tree in forest
#' @param maxDepth            MAx depth of each tree
#'
#' @return
#' An object of class \code{featureEngineeringSettings}
#' @examplesIf rlang::is_installed("reticulate") && reticulate::py_module_available("sklearn")
#' \donttest{ 
#' featureSelector <- createRandomForestFeatureSelection(ntrees = 2000, maxDepth = 10)
#' }
#' @export
createRandomForestFeatureSelection <- function(ntrees = 2000, maxDepth = 17) {
  rlang::check_installed(
    "reticulate",
    reason = "This function requires the reticulate package to be installed"
  )
  tryCatch(
    {
      reticulate::import("sklearn")
    },
    error = function(e) {
      stop("This function requires the scikit-learn package to be installed")
    }
  )
  checkIsClass(ntrees, c("numeric", "integer"))
  checkIsClass(maxDepth, c("numeric", "integer"))
  checkHigher(ntrees, 0)
  checkHigher(maxDepth, 0)

  featureEngineeringSettings <- list(
    ntrees = ntrees,
    max_depth = maxDepth
  )

  attr(featureEngineeringSettings, "fun") <- "randomForestFeatureSelection"
  class(featureEngineeringSettings) <- "featureEngineeringSettings"

  return(featureEngineeringSettings)
}

#' Create the settings for adding a spline for continuous variables
#'
#' @details
#' Returns an object of class \code{featureEngineeringSettings} that specifies the sampling function that will be called and the settings
#'
#' @param continousCovariateId     The covariateId to apply splines to
#' @param knots            Either number of knots of vector of split values
#' @param analysisId       The analysisId to use for the spline covariates
#'
#' @return
#' An object of class \code{featureEngineeringSettings}
#' @examples
#' # create splines for age (1002) with 5 knots
#' createSplineSettings(continousCovariateId = 1002, knots = 5, analysisId = 683)
#' @export
createSplineSettings <- function(
    continousCovariateId,
    knots,
    analysisId = 683) {
  checkIsClass(continousCovariateId, c("numeric", "integer"))
  checkIsClass(knots, c("numeric", "integer"))

  featureEngineeringSettings <- list(
    continousCovariateId = continousCovariateId,
    knots = knots,
    analysisId = analysisId
  )

  attr(featureEngineeringSettings, "fun") <- "splineCovariates"
  class(featureEngineeringSettings) <- "featureEngineeringSettings"

  return(featureEngineeringSettings)
}

splineCovariates <- function(
    trainData,
    featureEngineeringSettings,
    knots = NULL) {
  ParallelLogger::logInfo("Starting splineCovariates")

  if (is.null(knots)) {
    if (length(featureEngineeringSettings$knots) == 1) {
      measurements <- trainData$covariateData$covariates %>%
        dplyr::filter(.data$covariateId == !!featureEngineeringSettings$continousCovariateId) %>%
        as.data.frame()
      knots <- measurements$covariateValue %>%
        stats::quantile(seq(0.01, 0.99, length.out = featureEngineeringSettings$knots))
    } else {
      knots <- featureEngineeringSettings$knots
    }
  }

  # apply the spline mapping
  trainData <- splineMap(
    data = trainData,
    covariateId = featureEngineeringSettings$continousCovariateId,
    analysisId = featureEngineeringSettings$analysisId,
    knots = knots
  )

  featureEngineering <- list(
    funct = "splineCovariates",
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings,
      knots = knots
    )
  )

  # add the feature engineering in
  attr(trainData$covariateData, "metaData")$featureEngineering <- listAppend(
    attr(trainData$covariateData, "metaData")$featureEngineering,
    featureEngineering
  )
  ParallelLogger::logInfo("Finished splineCovariates")

  return(trainData)
}

# create the spline map to add spline columns
splineMap <- function(
    data,
    covariateId,
    analysisId,
    knots) {
  ParallelLogger::logInfo("Starting splineMap")
  measurements <- data$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == !!covariateId) %>%
    as.data.frame()

  designMatrix <- splines::bs(
    x = measurements$covariateValue,
    knots = knots[2:(length(knots) - 1)],
    Boundary.knots = knots[c(1, length(knots))]
  )

  data$covariateData$covariates <- data$covariateData$covariates %>%
    dplyr::filter(.data$covariateId != !!covariateId)

  # get the covariate name
  details <- data$covariateData$covariateRef %>%
    dplyr::filter(.data$covariateId == !!covariateId) %>%
    as.data.frame()
  covariateName <- details$covariateName

  data$covariateData$covariateRef <- data$covariateData$covariateRef %>%
    dplyr::filter(.data$covariateId != !!covariateId)

  # remove last 3 numbers as this was old analysis id
  covariateId <- floor(covariateId / 1000)

  # add the spline columns
  for (i in 1:ncol(designMatrix)) {
    Andromeda::appendToTable(
      tbl = data$covariateData$covariates,
      data = data.frame(
        rowId = measurements$rowId,
        covariateId = covariateId * 10000 + i * 1000 + analysisId,
        covariateValue = designMatrix[, i]
      )
    )
  }

  # add the covariates to the ref table
  Andromeda::appendToTable(
    tbl = data$covariateData$covariateRef,
    data = data.frame(
      covariateId =
        covariateId * 10000 + (1:(ncol(designMatrix))) * 1000 + analysisId,
      covariateName = paste(
        paste0(covariateName, " spline component "),
        1:ncol(designMatrix)
      ),
      conceptId = 0,
      analysisId = analysisId
    )
  )

  # add analysisRef for the first time a spline is added
  analysisRef <- data$covariateData$analysisRef %>% as.data.frame()
  if (!analysisId %in% analysisRef$analysisId) {
    Andromeda::appendToTable(
      tbl = data$covariateData$analysisRef,
      data = data.frame(
        analysisId = analysisId,
        analysisName = "splines",
        domainId = "feature engineering",
        startDay = 0,
        endDay = 0,
        isBinary = "N",
        missingMeansZero = "N"
      )
    )
  }
  ParallelLogger::logInfo("Finished splineMap")
  return(data)
}



#' Create the settings for using stratified imputation.
#'
#' @details
#' Returns an object of class \code{featureEngineeringSettings} that specifies 
#' how to do stratified imputation. This function splits the covariate into
#' age groups and fits splines to the covariate within each age group. The spline
#' values are then used to impute missing values.
#'
#' @param covariateId     The covariateId that needs imputed values
#' @param ageSplits       A vector of age splits in years to create age groups
#'
#' @return
#' An object of class \code{featureEngineeringSettings}
#' @examples
#' # create a stratified imputation settings for covariate 1050 with age splits 
#' # at 50 and 70
#' stratifiedImputationSettings <- 
#'   createStratifiedImputationSettings(covariateId = 1050, ageSplits = c(50, 70))
#' @export
createStratifiedImputationSettings <- function(
    covariateId,
    ageSplits = NULL) {
  checkIsClass(covariateId, c("numeric", "integer"))
  checkIsClass(ageSplits, c("numeric", "integer"))

  featureEngineeringSettings <- list(
    covariateId = covariateId,
    ageSplits = ageSplits
  )

  attr(featureEngineeringSettings, "fun") <- "stratifiedImputeCovariates"
  class(featureEngineeringSettings) <- "featureEngineeringSettings"

  return(featureEngineeringSettings)
}

stratifiedImputeCovariates <- function(
    trainData,
    featureEngineeringSettings,
    stratifiedMeans = NULL) {
  if (is.null(stratifiedMeans)) {
    stratifiedMeans <- calculateStratifiedMeans(
      trainData = trainData,
      featureEngineeringSettings = featureEngineeringSettings
    )
  }

  trainData <- imputeMissingMeans(
    trainData = trainData,
    covariateId = featureEngineeringSettings$covariateId,
    ageSplits = featureEngineeringSettings$ageSplits,
    stratifiedMeans = stratifiedMeans
  )

  return(trainData)
}

calculateStratifiedMeans <- function(
    trainData,
    featureEngineeringSettings) {
  if (is.null(featureEngineeringSettings$ageSplits)) {
    trainData$cohorts$ageGroup <- floor(trainData$cohorts$ageYear / 5)
  } else {
    trainData$cohorts$ageGroup <- rep(0, length(trainData$cohorts$ageYear))
    for (i in seq_along(featureEngineeringSettings$ageSplits)) {
      trainData$cohorts$ageGroup[trainData$cohorts$ageYear > featureEngineeringSettings$ageSplits[i]] <- i
    }
  }

  trainData$covariateData$cohorts <- trainData$cohorts[, c("rowId", "ageGroup", "gender")]

  stratifiedMeans <- trainData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == !!featureEngineeringSettings$covariateId) %>%
    dplyr::inner_join(
      y = trainData$covariateData$cohorts,
      by = c("rowId")
    ) %>%
    dplyr::group_by(.data$ageGroup, .data$gender) %>%
    dplyr::summarise(covariateValue = mean(.data$covariateValue, na.rm = TRUE)) %>%
    as.data.frame()

  return(stratifiedMeans)
}

imputeMissingMeans <- function(
    trainData,
    covariateId,
    ageSplits,
    stratifiedMeans) {
  if (is.null(ageSplits)) {
    trainData$cohorts$ageGroup <- floor(trainData$cohorts$ageYear / 5)
  } else {
    trainData$cohorts$ageGroup <- rep(0, length(trainData$cohorts$ageYear))
    for (i in seq_along(ageSplits)) {
      trainData$cohorts$ageGroup[trainData$cohorts$ageYear > ageSplits[i]] <- i
    }
  }

  rowIdsWithValues <- trainData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == !!covariateId) %>%
    dplyr::select("rowId") %>%
    dplyr::pull()
  rowIdsWithMissingValues <- trainData$cohorts$rowId[!trainData$cohorts$rowId %in% rowIdsWithValues]


  imputedData <- trainData$cohorts %>%
    dplyr::filter(.data$rowId %in% rowIdsWithMissingValues) %>%
    dplyr::select("rowId", "ageGroup", "gender") %>%
    dplyr::left_join(
      y = stratifiedMeans,
      by = c("ageGroup", "gender")
    ) %>%
    dplyr::mutate(
      covariateId = !!covariateId,
      covariateValue = .data$covariateValue
    ) %>%
    dplyr::select("rowId", "covariateId", "covariateValue")

  Andromeda::appendToTable(
    tbl = trainData$covariateData$covariates,
    data = imputedData
  )

  return(trainData)
}

univariateFeatureSelection <- function(
    trainData,
    featureEngineeringSettings,
    covariateIdsInclude = NULL) {
  if (is.null(covariateIdsInclude)) {
    # convert data into matrix:
    mappedData <- toSparseM(trainData, trainData$labels)

    matrixData <- mappedData$dataMatrix
    labels <- mappedData$labels
    covariateMap <- mappedData$covariateMap

    X <- reticulate::r_to_py(matrixData)
    y <- reticulate::r_to_py(labels[, "outcomeCount"])

    np <- reticulate::import("numpy")
    sklearn <- reticulate::import("sklearn")

    SelectKBest <- sklearn$feature_selection$SelectKBest
    chi2 <- sklearn$feature_selection$chi2

    kbest <- SelectKBest(chi2, k = featureEngineeringSettings$k)$fit(X, y$outcomeCount)
    kbest$scores_ <- np$nan_to_num(kbest$scores_)

    # taken from sklearn code, matches the application during transform call
    k <- featureEngineeringSettings$k
    mask <- np$zeros(length(kbest$scores_), dtype = "bool")
    mask[np$argsort(kbest$scores_, kind = "mergesort") + 1][(length(kbest$scores_) - k + 1):length(kbest$scores_)] <- TRUE

    covariateIdsInclude <- covariateMap[mask, ]$covariateId
  }

  trainData$covariateData$covariates <- trainData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId %in% covariateIdsInclude)

  trainData$covariateData$covariateRef <- trainData$covariateData$covariateRef %>%
    dplyr::filter(.data$covariateId %in% covariateIdsInclude)

  featureEngineering <- list(
    funct = "univariateFeatureSelection",
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings,
      covariateIdsInclude = covariateIdsInclude
    )
  )

  attr(trainData$covariateData, "metaData")$featureEngineering <- listAppend(
    attr(trainData$covariateData, "metaData")$featureEngineering,
    featureEngineering
  )

  return(trainData)
}


randomForestFeatureSelection <- function(
    trainData,
    featureEngineeringSettings,
    covariateIdsInclude = NULL) {
  if (is.null(covariateIdsInclude)) {
    # convert data into matrix:
    mappedData <- toSparseM(trainData)

    matrixData <- mappedData$dataMatrix
    labels <- mappedData$labels
    covariateMap <- mappedData$covariateMap

    X <- reticulate::r_to_py(matrixData)
    y <- reticulate::r_to_py(matrix(labels$outcomeCount, ncol = 1))

    sklearn <- reticulate::import("sklearn")

    ntrees <- featureEngineeringSettings$ntrees # 2000
    max_depth <- featureEngineeringSettings$max_depth # 17

    rf <- sklearn$ensemble$RandomForestClassifier(
      max_features = "sqrt",
      n_estimators = as.integer(ntrees),
      max_depth = as.integer(max_depth),
      min_samples_split = as.integer(2),
      random_state = as.integer(10), # make this an imput for consistency
      n_jobs = as.integer(-1),
      bootstrap = FALSE
    )

    rf <- rf$fit(X, y$ravel())

    inc <- rf$feature_importances_ > 0

    covariateIdsInclude <- covariateMap$covariateId[inc]
  }

  trainData$covariateData$covariates <- trainData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId %in% covariateIdsInclude)

  trainData$covariateData$covariateRef <- trainData$covariateData$covariateRef %>%
    dplyr::filter(.data$covariateId %in% covariateIdsInclude)


  featureEngeering <- list(
    funct = "randomForestFeatureSelection",
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings,
      covariateIdsInclude = covariateIdsInclude
    )
  )

  attr(trainData$covariateData, "metaData")$featureEngineering <- listAppend(
    attr(trainData$covariateData, "metaData")$featureEngineering,
    featureEngeering
  )

  return(trainData)
}

#' Create the settings for normalizing the data @param type The type of normalization to use, either "minmax" or "robust"
#' @return An object of class \code{featureEngineeringSettings}
#' @param type The type of normalization to use, either "minmax" or "robust"
#' @param settings A list of settings for the normalization. 
#' For robust normalization, the settings list can contain a boolean value for 
#' clip, which clips the values to be between -3 and 3 after normalization. See 
#' https://arxiv.org/abs/2407.04491 
#' @return An object of class \code{featureEngineeringSettings}'
#' @examples
#' # create a minmax normalizer that normalizes the data between 0 and 1
#' normalizer <- createNormalizer(type = "minmax")
#' # create a robust normalizer that normalizes the data by the interquartile range
#' # and squeezes the values to be between -3 and 3
#' normalizer <- createNormalizer(type = "robust", settings = list(clip = TRUE))
#' @export
createNormalizer <- function(type = "minmax",
                             settings = list()) {
  featureEngineeringSettings <- list(
    type = type
  )
  checkIsClass(type, "character")
  checkInStringVector(type, c("minmax", "robust"))
  if (type == "minmax") {
    attr(featureEngineeringSettings, "fun") <- "minMaxNormalize"
  } else if (type == "robust") {
    attr(featureEngineeringSettings, "fun") <- "robustNormalize"
    checkBoolean(settings$clip)
    featureEngineeringSettings$settings <- settings
  }

  class(featureEngineeringSettings) <- "featureEngineeringSettings"
  return(featureEngineeringSettings)
}

#' A function that normalizes continous features to have values between 0 and 1
#' @details uses value - min / (max - min) to normalize the data
#' @param trainData The training data to be normalized
#' @param featureEngineeringSettings The settings for the normalization
#' @param done Whether the data has already been normalized (bool)
#' @return The normalized data
#' @keywords internal
minMaxNormalize <- function(trainData, featureEngineeringSettings, done = FALSE) {
  start <- Sys.time()
  if (!done) {
    outData <- list(
      labels = trainData$labels,
      folds = trainData$folds,
      covariateData = Andromeda::copyAndromeda(trainData$covariateData)
    )
    class(outData) <- "plpData"
    attributes(outData) <- attributes(trainData)
    class(outData$covariateData) <- "CovariateData"
    attr(outData$covariateData, "metaData") <- 
      attr(trainData$covariateData, "metaData")
    ParallelLogger::logInfo("Starting min-max normalization of continuous features")
    # fit the normalization
    # find continuous features from trainData$covariateData$analysisRef
    continousFeatures <- outData$covariateData$analysisRef %>%
      dplyr::filter(.data$isBinary == "N") %>%
      dplyr::select("analysisId") %>%
      dplyr::inner_join(outData$covariateData$covariateRef, by = "analysisId") %>%
      dplyr::pull(.data$covariateId)

    # get max of each feature
    outData$covariateData$minMaxs <- outData$covariateData$covariates %>%
      dplyr::filter(.data$covariateId %in% continousFeatures) %>%
      dplyr::group_by(.data$covariateId) %>%
      dplyr::summarise(
        max = max(.data$covariateValue, na.rm = TRUE),
        min = min(.data$covariateValue, na.rm = TRUE)
      ) %>%
      dplyr::collect()
    on.exit(outData$covariateData$minMaxs <- NULL, add = TRUE)

    # save the normalization
    attr(featureEngineeringSettings, "minMaxs") <-
      outData$covariateData$minMaxs %>% dplyr::collect()

    # apply the normalization to trainData
    outData$covariateData$covariates <- outData$covariateData$covariates %>%
      dplyr::left_join(outData$covariateData$minMaxs, by = "covariateId") %>%
      # use ifelse to only normalize if min and max are not NA as is the case
      # for continous features, else return original value
      dplyr::mutate(covariateValue = ifelse(!is.na(min) & !is.na(max),
        (.data$covariateValue - min) / (max - min),
        .data$covariateValue
      )) %>%
      dplyr::select(-c("max", "min"))
    outData$covariateData$minMaxs <- NULL
    done <- TRUE
  } else {
    ParallelLogger::logInfo("Applying min-max normalization of continuous features to test data")
    outData <- list(
      labels = trainData$labels,
      covariateData = Andromeda::copyAndromeda(trainData$covariateData)
    )
    class(outData) <- "plpData"
    attributes(outData) <- attributes(trainData)
    class(outData$covariateData) <- "CovariateData"
    attr(outData$covariateData, "metaData") <- 
      attr(trainData$covariateData, "metaData")
    # apply the normalization to test data by using saved normalization values
    outData$covariateData$minMaxs <- attr(featureEngineeringSettings, "minMaxs")
    on.exit(outData$covariateData$minMaxs <- NULL, add = TRUE)
    outData$covariateData$covariates <- outData$covariateData$covariates %>%
      dplyr::left_join(outData$covariateData$minMaxs,
        by = "covariateId") %>%
      dplyr::mutate(covariateValue = ifelse(!is.na(min) & !is.na(max),
        (.data$covariateValue - min) / (max - min),
        .data$covariateValue
      )) %>%
      dplyr::select(-c("max", "min"))
  }
  featureEngineering <- list(
    funct = "minMaxNormalize",
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings,
      done = done
    )
  )

  attr(outData$covariateData, "metaData")$featureEngineering[["minMaxNormalize"]] <-
    featureEngineering
  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste0(
    "Finished min-max normalization of continuous features in ",
    signif(delta, 3), " ", attr(delta, "units")
  ))
  return(outData)
}

#' A function that normalizes continous by the interquartile range and 
#' optionally forces the resulting values to be between -3 and 3 with 
#' f(x) = x / sqrt(1 + (x/3)^2)
#' '@details uses (value - median) / iqr to normalize the data and then can
#' applies the function f(x) = x / sqrt(1 + (x/3)^2) to the normalized values.
#' This forces the values to be between -3 and 3 while preserving the relative
#' ordering of the values.
#' based on https://arxiv.org/abs/2407.04491 for more details
#' @param trainData The training data to be normalized
#' @param featureEngineeringSettings The settings for the normalization
#' @param done Whether the data has already been normalized (bool)
#' @return The `trainData` object with normalized data
#' @keywords internal
robustNormalize <- function(trainData, featureEngineeringSettings, done = FALSE) {
  start <- Sys.time()
  if (!done) {
    ParallelLogger::logInfo("Starting robust normalization of continuous features")
    outData <- list(
      labels = trainData$labels,
      folds = trainData$folds,
      covariateData = Andromeda::copyAndromeda(trainData$covariateData)
    )
    class(outData) <- "plpData"
    attributes(outData) <- attributes(trainData)
    class(outData$covariateData) <- "CovariateData"
    attr(outData$covariateData, "metaData") <- 
      attr(trainData$covariateData, "metaData")
    # find continuous features from trainData$covariateData$analysisRef
    continousFeatures <- outData$covariateData$analysisRef %>%
      dplyr::filter(.data$isBinary == "N") %>%
      dplyr::select("analysisId") %>%
      dplyr::inner_join(outData$covariateData$covariateRef, by = "analysisId") %>%
      dplyr::pull(.data$covariateId)

    # get (25, 75)% quantiles of each feature
    if (inherits(outData$covariateData, "SQLiteConnection")) {
      RSQLite::initExtension(outData$covariateData, "math")
      outData$covariateData$quantiles <- outData$covariateData$covariates %>%
        dplyr::filter(.data$covariateId %in% continousFeatures) %>%
        dplyr::group_by(.data$covariateId) %>%
        dplyr::summarise(
          q25 = dplyr::sql("lower_quartile(covariateValue)"), 
          q75 = dplyr::sql("upper_quartile(covariateValue)"),
          median = stats::median(.data$covariateValue, na.rm = TRUE)
        ) %>%
        dplyr::mutate(iqr = .data$q75 - .data$q25) %>%
        dplyr::select(-c("q75", "q25")) %>%
        dplyr::collect()
    } else {
      outData$covariateData$quantiles <- outData$covariateData$covariates %>%
        dplyr::filter(.data$covariateId %in% continousFeatures) %>%
        dplyr::group_by(.data$covariateId) %>%
        dplyr::summarise(
          q25 = stats::quantile(.data$covariateValue, 0.25, na.rm = TRUE), 
          q75 = stats::quantile(.data$covariateValue, 0.75, na.rm = TRUE),
          median = stats::median(.data$covariateValue, na.rm = TRUE)
        ) %>%
        dplyr::mutate(iqr = .data$q75 - .data$q25) %>%
        dplyr::select(-c("q75", "q25")) %>%
        dplyr::collect()
    }
    on.exit(outData$covariateData$quantiles <- NULL, add = TRUE)

    # save the normalization
    attr(featureEngineeringSettings, "quantiles") <-
      outData$covariateData$quantiles %>% dplyr::collect()

    # apply the normalization to trainData
    outData$covariateData$covariates <- outData$covariateData$covariates %>%
      dplyr::left_join(outData$covariateData$quantiles, by = "covariateId") %>%
      # use ifelse to only normalize continous features
      dplyr::mutate(covariateValue = ifelse(
        !is.na(.data$iqr) && !is.na(.data$median),
        (.data$covariateValue - .data$median) / .data$iqr,
        .data$covariateValue
      )) %>%
      # optionally if settings$clip is TRUE.
      # smoothly clip the range to [-3, 3] with  x / sqrt(1 + (x/3)^2)
      # ref: https://arxiv.org/abs/2407.04491
      dplyr::mutate(covariateValue = ifelse(!is.na(.data$iqr) &&
        !is.na(.data$median) && featureEngineeringSettings$settings$clip,
        .data$covariateValue / sqrt(1 + (.data$covariateValue / 3)^2),
        .data$covariateValue
      )) %>%
      dplyr::select(-c("median", "iqr"))
    done <- TRUE
  } else {
    ParallelLogger::logInfo("Applying robust normalization of continuous features to test data")
    outData <- list(
      labels = trainData$labels,
      covariateData = Andromeda::copyAndromeda(trainData$covariateData)
    )
    class(outData) <- "plpData"
    attributes(outData) <- attributes(trainData)
    class(outData$covariateData) <- "CovariateData"
    attr(outData$covariateData, "metaData") <- 
      attr(trainData$covariateData, "metaData")
    # apply the normalization to test data by using saved normalization values
    outData$covariateData$quantiles <- attr(featureEngineeringSettings, "quantiles")
    on.exit(outData$covariateData$quantiles <- NULL, add = TRUE)
    outData$covariateData$covariates <- outData$covariateData$covariates %>%
      dplyr::left_join(outData$covariateData$quantiles,
        by = "covariateId", copy = TRUE
      ) %>%
      dplyr::mutate(covariateValue = ifelse(!is.na(.data$iqr) && !is.na(.data$median),
        (.data$covariateValue - .data$median) / .data$iqr,
        .data$covariateValue
      )) %>%
      dplyr::mutate(covariateValue = ifelse(!is.na(.data$iqr) && 
        !is.na(.data$median) &&
        featureEngineeringSettings$settings$clip,
        .data$covariateValue / sqrt(1 + (.data$covariateValue / 3)^2),
        .data$covariateValue
      )) %>%
      dplyr::select(-c("median", "iqr"))
  }
  featureEngineering <- list(
    funct = "robustNormalize",
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings,
      done = done
    )
  )

  attr(outData$covariateData, "metaData")$featureEngineering[["robustNormalize"]] <-
    featureEngineering
  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste0(
    "Finished robust normalization in ",
    signif(delta, 3), " ", attr(delta, "units")
  ))
  return(outData)
}

#' Create the settings for removing rare features
#' @param threshold The minimum fraction of the training data that must have a
#' feature for it to be included
#' @return An object of class \code{featureEngineeringSettings}
#' @examplesIf rlang::is_installed("Eunomia") && rlang::is_installed("curl") && curl::has_internet()
#' \donttest{ \dontshow{ # takes too long }
#' # create a rare feature remover that removes features that are present in less
#' # than 1% of the population
#' rareFeatureRemover <- createRareFeatureRemover(threshold = 0.01)
#' plpData <- getEunomiaPlpData()
#' analysisId <- "rareFeatureRemover"
#' saveLocation <- file.path(tempdir(), analysisId)
#' results <- runPlp(
#'   plpData = plpData,
#'   featureEngineeringSettings = rareFeatureRemover,
#'   outcomeId = 3,
#'  executeSettings = createExecuteSettings(
#'    runModelDevelopment = TRUE,
#'    runSplitData = TRUE,
#'    runFeatureEngineering = TRUE),
#'  saveDirectory = saveLocation,
#'  analysisId = analysisId)
#' # clean up 
#' unlink(saveLocation, recursive = TRUE)
#' } 
#' @export
createRareFeatureRemover <- function(threshold = 0.001) {
  checkIsClass(threshold, c("numeric"))
  checkHigherEqual(threshold, 0)
  checkLower(threshold, 1)
  featureEngineeringSettings <- list(
    threshold = threshold
  )
  attr(featureEngineeringSettings, "fun") <- "removeRareFeatures"

  class(featureEngineeringSettings) <- "featureEngineeringSettings"
  return(featureEngineeringSettings)
}

#' A function that removes rare features from the data
#' @details removes features that are present in less than a certain fraction of the population
#' @param trainData The data to be normalized
#' @param featureEngineeringSettings The settings for the normalization
#' @param done Whether to find and remove rare features or remove them only (bool)
#' @return The data with rare features removed
#' @keywords internal
removeRareFeatures <- function(trainData, featureEngineeringSettings, done = FALSE) {
  start <- Sys.time()
  if (!done) {
    ParallelLogger::logInfo(
      "Removing features rarer than threshold: ", featureEngineeringSettings$threshold,
      " from the data"
    )
    outData <- list(
      labels = trainData$labels,
      folds = trainData$folds,
      covariateData = Andromeda::copyAndromeda(trainData$covariateData)
    )
    class(outData) <- "plpData"
    attributes(outData) <- attributes(trainData)
    class(outData$covariateData) <- "CovariateData"
    attr(outData$covariateData, "metaData") <- 
      attr(trainData$covariateData, "metaData")
    rareFeatures <- outData$covariateData$covariates %>%
      dplyr::group_by(.data$covariateId) %>%
      dplyr::summarise(count = dplyr::n()) %>%
      dplyr::collect()
    rareFeatures <- rareFeatures %>%
      dplyr::mutate(ratio = .data$count / (
        outData$covariateData$covariates %>%
          dplyr::summarise(popSize = dplyr::n_distinct(.data$rowId)) %>%
          dplyr::pull()
      )) %>%
      dplyr::filter(.data$ratio <= featureEngineeringSettings$threshold) %>%
      dplyr::pull(c("covariateId"))

    outData$covariateData$covariates <- outData$covariateData$covariates %>%
      dplyr::filter(!.data$covariateId %in% rareFeatures)
    outData$covariateData$covariateRef <- outData$covariateData$covariateRef %>%
      dplyr::filter(!.data$covariateId %in% rareFeatures)

    attr(featureEngineeringSettings, "rareFeatures") <- rareFeatures

    done <- TRUE
  } else {
    ParallelLogger::logInfo(
      "Applying rare feature removal with rate below: ",
      featureEngineeringSettings$threshold, " to test data"
    )
    outData <- list(
      labels = trainData$labels,
      covariateData = Andromeda::copyAndromeda(trainData$covariateData)
    )
    class(outData) <- "plpData"
    attributes(outData) <- attributes(trainData)
    class(outData$covariateData) <- "CovariateData"
    attr(outData$covariateData, "metaData") <- 
      attr(trainData$covariateData, "metaData")
    outData$covariateData$covariates <- outData$covariateData$covariates %>%
      dplyr::filter(
        !.data$covariateId %in% !!attr(featureEngineeringSettings, "rareFeatures")
      )
    outData$covariateData$covariateRef <- outData$covariateData$covariateRef %>%
      dplyr::filter(
        !.data$covariateId %in% !!attr(featureEngineeringSettings, "rareFeatures")
      )
  }
  featureEngineering <- list(
    funct = "removeRareFeatures",
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings,
      done = done
    )
  )
  attr(outData$covariateData, "metaData")$featureEngineering[["removeRare"]] <-
    featureEngineering
  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste0(
    "Finished rare feature removal in ",
    signif(delta, 3), " ", attr(delta, "units")
  ))
  return(outData)
}
