# @file FeatureEngineering.R
# Copyright 2021 Observational Health Data Sciences and Informatics
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

  ParallelLogger::logInfo("Done Feature Engineering")

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
#' Returns an object of class \code{featureEngineeringSettings} that specifies the sampling function that will be called and the settings
#'
#' @param k              This function returns the K features most associated (univariately) to the outcome
#'
#' @return
#' An object of class \code{featureEngineeringSettings}
#' @export
createUnivariateFeatureSelection <- function(k = 100) {
  if (inherits(k, "numeric")) {
    k <- as.integer(k)
  }

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
#' Returns an object of class \code{featureEngineeringSettings} that specifies the sampling function that will be called and the settings
#'
#' @param ntrees              number of tree in forest
#' @param maxDepth            MAx depth of each tree
#'
#' @return
#' An object of class \code{featureEngineeringSettings}
#' @export
createRandomForestFeatureSelection <- function(ntrees = 2000, maxDepth = 17) {
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
  attr(trainData, "metaData")$featureEngineering <- listAppend(
    attr(trainData, "metaData")$featureEngineering,
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
    x = measurements$covariateValue, # knots[1]:knots[length(knots)],
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
      covariateId = covariateId * 10000 + (1:(ncol(designMatrix))) * 1000 + analysisId,
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



#' Create the settings for adding a spline for continuous variables
#'
#' @details
#' Returns an object of class \code{featureEngineeringSettings} that specifies how to do stratified imputation
#'
#' @param covariateId     The covariateId that needs imputed values
#' @param ageSplits       A vector of age splits in years to create age groups
#'
#' @return
#' An object of class \code{featureEngineeringSettings}
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
    os <- reticulate::import("os")
    sys <- reticulate::import("sys")
    math <- reticulate::import("math")
    scipy <- reticulate::import("scipy")

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

  attr(trainData, "metaData")$featureEngineering <- listAppend(
    attr(trainData, "metaData")$featureEngineering,
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

    np <- reticulate::import("numpy")
    os <- reticulate::import("os")
    sys <- reticulate::import("sys")
    math <- reticulate::import("math")
    scipy <- reticulate::import("scipy")

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
      bootstrap = F
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

  attr(trainData, "metaData")$featureEngineering <- listAppend(
    attr(trainData, "metaData")$featureEngineering,
    featureEngeering
  )

  return(trainData)
}
