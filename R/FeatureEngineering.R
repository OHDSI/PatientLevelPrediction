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


# Training: `data` is the training set made by splitData() (and sampleData()),
# which already has its own Andromeda, so the steps change it in place.
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

# Andromeda tables are modified in place, so copying the data list alone does
# not isolate them from the caller. Feature engineering steps change the data
# they are given; applyFeatureEngineering() uses this to give them a copy.
isolateCovariateData <- function(data) {
  metaData <- attr(data$covariateData, "metaData")
  data$covariateData <- Andromeda::copyAndromeda(data$covariateData)
  class(data$covariateData) <- "CovariateData"
  attr(data$covariateData, "metaData") <- metaData
  return(data)
}

# Data frames stored as attributes come back from the settings JSON as a list of rows.
restoreSavedTable <- function(savedTable) {
  if (is.data.frame(savedTable)) {
    return(savedTable)
  }
  return(do.call(rbind, lapply(savedTable, as.data.frame)))
}

populationRowIds <- function(data) {
  if (!is.null(data$labels)) {
    return(unique(data$labels$rowId))
  }
  if (!is.null(data$cohorts)) {
    return(unique(data$cohorts$rowId))
  }
  return(NULL)
}

# Continuous features where a patient without a row has the value zero. Temporal
# data is excluded because which time points are implicit is not defined.
implicitZeroCovariateIds <- function(data) {
  covariateData <- data$covariateData
  if (is.null(populationRowIds(data)) ||
    !"missingMeansZero" %in% colnames(covariateData$analysisRef) ||
    "timeId" %in% colnames(covariateData$covariates)) {
    return(numeric(0))
  }
  covariateData$analysisRef %>%
    dplyr::filter(.data$isBinary == "N", .data$missingMeansZero == "Y") %>%
    dplyr::select("analysisId") %>%
    dplyr::inner_join(covariateData$covariateRef, by = "analysisId") %>%
    dplyr::pull(.data$covariateId)
}

# Stores the implicit zeros so that normalization statistics see the whole
# population and values that do not stay zero after normalization are kept.
addImplicitZeros <- function(data, covariateIds) {
  rowIds <- populationRowIds(data)
  if (length(covariateIds) == 0 || is.null(rowIds)) {
    return(invisible(NULL))
  }
  covariateData <- data$covariateData
  covariateData$populationRows <- data.frame(rowId = rowIds)
  on.exit(covariateData$populationRows <- NULL, add = TRUE)
  for (id in covariateIds) {
    zeros <- covariateData$populationRows %>%
      dplyr::anti_join(
        covariateData$covariates %>% dplyr::filter(.data$covariateId == !!id),
        by = "rowId"
      ) %>%
      dplyr::mutate(covariateId = !!id, covariateValue = 0)
    Andromeda::appendToTable(covariateData$covariates, zeros)
  }
  return(invisible(NULL))
}

removeStoredZeros <- function(covariateData, covariateIds) {
  if (length(covariateIds) == 0) {
    return(invisible(NULL))
  }
  covariateData$covariates <- covariateData$covariates %>%
    dplyr::filter(!(.data$covariateId %in% !!covariateIds & .data$covariateValue == 0))
  return(invisible(NULL))
}

# Fitted steps are stored as a list of list(funct, settings) entries, one per
# step in the order they ran. Models saved by earlier versions, and custom steps
# that record themselves with listAppend(), may also hold a step as a function
# name followed by its settings list directly in the list, under any names
# (funct and settings, funct.1 and settings.1 after the JSON round trip, or
# misaligned names), so steps are recognised by their structure.
featureEngineeringSteps <- function(featureEngineering) {
  isStep <- function(entry) is.list(entry) && !is.null(entry[["funct"]])
  steps <- list()
  i <- 1
  while (i <= length(featureEngineering)) {
    entry <- featureEngineering[[i]]
    nextEntry <- if (i < length(featureEngineering)) featureEngineering[[i + 1]]
    if (isStep(entry)) {
      steps[[length(steps) + 1]] <- list(funct = entry[["funct"]], settings = entry[["settings"]])
      i <- i + 1
    } else if (is.character(entry) && length(entry) == 1 && is.list(nextEntry) && !isStep(nextEntry)) {
      steps[[length(steps) + 1]] <- list(funct = entry, settings = nextEntry)
      i <- i + 2
    } else {
      warning("Saved feature engineering entry ", i, " is not a step and is not applied.")
      i <- i + 1
    }
  }
  return(steps)
}

#' Record a fitted feature engineering step so it is applied at prediction
#'
#' @description
#' Call this at the end of a feature engineering 'implement' function, before
#' returning `trainData`. When the model is applied to new data,
#' PatientLevelPrediction calls the recorded steps again, in the order they were
#' recorded.
#'
#' @details
#' At prediction the function named `funct` is called with `trainData` (a copy
#' of the data to transform) and the elements of `settings` as arguments. Put
#' everything the function needs to repeat the step in `settings`, such as the
#' `featureEngineeringSettings` and the values learned from the training data.
#' The model is saved as JSON, so `settings` should hold only numbers, strings,
#' logicals, data frames and lists of these.
#'
#' @param trainData The data the implement function received and changed.
#' @param funct The name of the implement function to call at prediction.
#' @param settings A named list of the other arguments for that call.
#' @return `trainData`, with the step added to
#'   `attr(trainData$covariateData, "metaData")$featureEngineering`.
#' @examples
#' covariateData <- Andromeda::andromeda(
#'   covariates = data.frame(rowId = 1:3, covariateId = 1, covariateValue = c(1, 5, 9))
#' )
#' trainData <- list(covariateData = covariateData)
#' trainData <- recordFeatureEngineeringStep(
#'   trainData,
#'   funct = "capCovariateValues",
#'   settings = list(featureEngineeringSettings = list(), cap = 5)
#' )
#' attr(trainData$covariateData, "metaData")$featureEngineering
#' Andromeda::close(covariateData)
#' @export
recordFeatureEngineeringStep <- function(trainData, funct, settings) {
  checkIsClass(funct, "character")
  if (length(funct) != 1) {
    stop("funct must be the name of one function")
  }
  checkIsClass(settings, "list")
  steps <- featureEngineeringSteps(attr(trainData$covariateData, "metaData")$featureEngineering)
  steps[[length(steps) + 1]] <- list(funct = funct, settings = settings)
  attr(trainData$covariateData, "metaData")$featureEngineering <- steps
  return(trainData)
}

#' Create the settings for defining any feature engineering that will be done
#'
#' @details
#' Returns an object of class \code{featureEngineeringSettings} that specifies the feature engineering function that will be called and its settings
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
#' @examples
#' \dontshow{ # dontrun reason: requires python and scikit-learn }
#' \dontrun{ #' # create a feature selection that selects the 100 most associated features
#' featureSelector <- createUnivariateFeatureSelection(k = 100) 
#' }
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
#' Returns an object of class `featureEngineeringSettings` that specifies the feature engineering function that will be called and its settings
#'
#' @param ntrees              number of tree in forest
#' @param maxDepth            MAx depth of each tree
#'
#' @return
#' An object of class \code{featureEngineeringSettings}
#' @examples
#' \dontshow{ # dontrun reason: requires python and scikit-learn }
#' \dontrun{ #' featureSelector <- createRandomForestFeatureSelection(ntrees = 2000, maxDepth = 10)
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
#' Returns an object of class \code{featureEngineeringSettings} that specifies the feature engineering function that will be called and its settings
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
    knots = NULL,
    componentIds = NULL) {
  ParallelLogger::logInfo("Starting splineCovariates")

  fitting <- is.null(knots)
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
  splineResult <- splineMap(
    data = trainData,
    covariateId = featureEngineeringSettings$continousCovariateId,
    analysisId = featureEngineeringSettings$analysisId,
    knots = knots,
    componentIds = componentIds,
    checkCollisions = fitting
  )
  trainData <- splineResult$data

  trainData <- recordFeatureEngineeringStep(
    trainData,
    funct = "splineCovariates",
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings,
      knots = knots,
      componentIds = splineResult$componentIds
    )
  )
  ParallelLogger::logInfo("Finished splineCovariates")

  return(trainData)
}

# Keeps the established IDs unless another spline already uses them. Saved
# models without a persisted mapping replay without the check, to reproduce the
# IDs they were trained on.
allocateSplineIds <- function(covariateId, analysisId, nComponents, existingIds) {
  for (prefix in c(floor(covariateId / 1000), covariateId)) {
    ids <- prefix * 10000 + seq_len(nComponents) * 1000 + analysisId
    if (max(ids) < 1e15 && !any(ids %in% existingIds)) {
      return(ids)
    }
  }
  stop(
    "Cannot create unique spline covariate IDs for covariateId ", covariateId,
    ". Use a different analysisId in createSplineSettings()."
  )
}

# create the spline map to add spline columns
splineMap <- function(
    data,
    covariateId,
    analysisId,
    knots,
    componentIds = NULL,
    checkCollisions = FALSE) {
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

  if (is.null(componentIds)) {
    existingIds <- if (checkCollisions) {
      data$covariateData$covariateRef %>% dplyr::pull(.data$covariateId)
    }
    componentIds <- allocateSplineIds(
      covariateId = covariateId,
      analysisId = analysisId,
      nComponents = ncol(designMatrix),
      existingIds = existingIds
    )
  }

  # add the spline columns
  for (i in 1:ncol(designMatrix)) {
    Andromeda::appendToTable(
      tbl = data$covariateData$covariates,
      data = data.frame(
        rowId = measurements$rowId,
        covariateId = componentIds[i],
        covariateValue = designMatrix[, i]
      )
    )
  }

  # add the covariates to the ref table
  Andromeda::appendToTable(
    tbl = data$covariateData$covariateRef,
    data = data.frame(
      covariateId = componentIds,
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
  return(list(data = data, componentIds = componentIds))
}



#' Create the settings for using stratified imputation
#'
#' @details
#' Returns an object of class \code{featureEngineeringSettings} that specifies
#' how to do stratified imputation. Patients without a value for the covariate
#' get the mean value of the training patients with the same age group and
#' gender. Patients in an age group and gender without training values, and
#' patients without an age or gender, get the mean over all training patients.
#' Stored missing (NA) values are imputed too. The means learned in training are
#' reused when the model is applied to new data.
#'
#' @param covariateId     The covariateId that needs imputed values
#' @param ageSplits       A vector of ages in years at which to split the age
#'   groups. If \code{NULL}, the default, age groups are 5 years wide.
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
  checkIsClass(ageSplits, c("numeric", "integer", "NULL"))

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
    stratifiedMeans = NULL,
    overallMean = NULL,
    done = FALSE) {
  covariateId <- featureEngineeringSettings$covariateId
  # a stored NA counts as a missing value
  trainData$covariateData$covariates <- trainData$covariateData$covariates %>%
    dplyr::filter(!(.data$covariateId == !!covariateId & is.na(.data$covariateValue)))
  if (!done) {
    stratifiedMeans <- calculateStratifiedMeans(
      trainData = trainData,
      featureEngineeringSettings = featureEngineeringSettings
    )
    overallMean <- trainData$covariateData$covariates %>%
      dplyr::filter(.data$covariateId == !!covariateId) %>%
      dplyr::summarise(overallMean = mean(.data$covariateValue, na.rm = TRUE)) %>%
      dplyr::pull()
    if (nrow(stratifiedMeans) == 0) {
      warning(
        "Stratified imputation found no values of covariate ", covariateId,
        " in the training data, so no values are imputed."
      )
    }
  }
  stratifiedMeans <- savedStratifiedMeans(stratifiedMeans)
  # NaN, NULL or "NA" (after a JSON round trip) when there were no training values
  overallMean <- suppressWarnings(as.numeric(unlist(overallMean)))
  if (length(overallMean) != 1 || !is.finite(overallMean)) {
    overallMean <- NA_real_
  }

  trainData <- imputeMissingMeans(
    trainData = trainData,
    covariateId = covariateId,
    ageSplits = featureEngineeringSettings$ageSplits,
    stratifiedMeans = stratifiedMeans,
    overallMean = overallMean
  )

  trainData <- recordFeatureEngineeringStep(
    trainData,
    funct = "stratifiedImputeCovariates",
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings,
      stratifiedMeans = stratifiedMeans,
      overallMean = overallMean,
      done = TRUE
    )
  )
  return(trainData)
}

# The saved group means with numeric columns, also when there are none.
savedStratifiedMeans <- function(stratifiedMeans) {
  means <- restoreSavedTable(stratifiedMeans)
  if (is.null(means) || nrow(means) == 0) {
    return(data.frame(ageGroup = numeric(0), gender = numeric(0), covariateValue = numeric(0)))
  }
  return(data.frame(
    ageGroup = as.numeric(means$ageGroup),
    gender = as.numeric(means$gender),
    covariateValue = as.numeric(means$covariateValue)
  ))
}

# Age and gender of the patients being processed: the labels during training,
# or the cohorts when a full plpData is processed.
stratificationPopulation <- function(trainData, ageSplits) {
  population <- trainData$labels %||% trainData$cohorts
  if (is.null(population) || !all(c("rowId", "ageYear", "gender") %in% colnames(population))) {
    stop("Stratified imputation needs rowId, ageYear and gender in the labels or cohorts.")
  }
  population <- as.data.frame(population)[, c("rowId", "ageYear", "gender")]
  if (is.null(ageSplits)) {
    population$ageGroup <- floor(population$ageYear / 5)
  } else {
    population$ageGroup <- 0
    for (i in seq_along(ageSplits)) {
      population$ageGroup[population$ageYear > ageSplits[i]] <- i
    }
    population$ageGroup[is.na(population$ageYear)] <- NA
  }
  population$ageGroup <- as.numeric(population$ageGroup)
  population$gender <- as.numeric(population$gender)
  return(population[, c("rowId", "ageGroup", "gender")])
}

calculateStratifiedMeans <- function(
    trainData,
    featureEngineeringSettings) {
  population <- stratificationPopulation(trainData, featureEngineeringSettings$ageSplits)
  # patients without an age or gender get the overall mean instead
  population <- population[!is.na(population$ageGroup) & !is.na(population$gender), ]
  stratifiedMeans <- trainData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == !!featureEngineeringSettings$covariateId) %>%
    dplyr::select("rowId", "covariateValue") %>%
    dplyr::collect() %>%
    dplyr::inner_join(population, by = "rowId") %>%
    dplyr::group_by(.data$ageGroup, .data$gender) %>%
    dplyr::summarise(covariateValue = mean(.data$covariateValue, na.rm = TRUE), .groups = "drop") %>%
    as.data.frame()

  return(stratifiedMeans)
}

imputeMissingMeans <- function(
    trainData,
    covariateId,
    ageSplits,
    stratifiedMeans,
    overallMean = NULL) {
  population <- stratificationPopulation(trainData, ageSplits)
  rowIdsWithValues <- trainData$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == !!covariateId) %>%
    dplyr::distinct(.data$rowId) %>%
    dplyr::pull()

  fallback <- overallMean %||% NA_real_
  imputedData <- population %>%
    dplyr::filter(!.data$rowId %in% rowIdsWithValues) %>%
    dplyr::left_join(stratifiedMeans, by = c("ageGroup", "gender")) %>%
    dplyr::mutate(
      covariateId = !!covariateId,
      covariateValue = dplyr::coalesce(.data$covariateValue, !!fallback)
    ) %>%
    # without a training mean there is nothing to impute
    dplyr::filter(!is.na(.data$covariateValue)) %>%
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

  trainData <- recordFeatureEngineeringStep(
    trainData,
    funct = "univariateFeatureSelection",
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings,
      covariateIdsInclude = covariateIdsInclude
    )
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

  trainData <- recordFeatureEngineeringStep(
    trainData,
    funct = "randomForestFeatureSelection",
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings,
      covariateIdsInclude = covariateIdsInclude
    )
  )

  return(trainData)
}

#' Create the settings for normalizing the data
#'
#' @param type The type of normalization to use, either "minmax" or "robust"
#' @param settings A list of settings for the normalization. 
#' For robust normalization, the settings list can contain a boolean value for 
#' clip, which clips the values to be between -3 and 3 after normalization. See 
#' https://arxiv.org/abs/2407.04491 
#' @return An object of class \code{featureEngineeringSettings}
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
#' @param trainData The data to normalize: the training data, or new data when
#'   `done = TRUE`. It is changed in place.
#' @param featureEngineeringSettings The settings for the normalization
#' @param done Whether the normalization has already been fitted (bool)
#' @return `trainData`, normalized, with the step recorded for prediction
#' @keywords internal
minMaxNormalize <- function(trainData, featureEngineeringSettings, done = FALSE) {
  start <- Sys.time()
  outData <- trainData
  if (!done) {
    ParallelLogger::logInfo("Starting min-max normalization of continuous features")
    implicitZeroIds <- implicitZeroCovariateIds(outData)
    addImplicitZeros(outData, implicitZeroIds)
    attr(featureEngineeringSettings, "implicitZeroCovariateIds") <- implicitZeroIds
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
        min = min(.data$covariateValue, na.rm = TRUE),
      ) %>%
      dplyr::mutate(
        range = .data$max - .data$min,
        max = dplyr::if_else(.data$range == 0, .data$min + 1, .data$max),
      ) %>%
      dplyr::select(-"range") %>% 
      dplyr::collect()
    on.exit(outData$covariateData$minMaxs <- NULL, add = TRUE)

    # save the normalization
    attr(featureEngineeringSettings, "minMaxs") <-
      outData$covariateData$minMaxs %>% dplyr::collect()

    # apply the normalization to trainData
    outData$covariateData$covariates <- outData$covariateData$covariates %>%
      dplyr::left_join(outData$covariateData$minMaxs, by = "covariateId") %>%
      # Only continuous features have min/max rows after the join.
      dplyr::mutate(covariateValue = dplyr::coalesce(
        (.data$covariateValue - .data$min) / (.data$max - .data$min),
        .data$covariateValue
      )) %>%
      dplyr::select(-c("max", "min"))
    removeStoredZeros(outData$covariateData, implicitZeroIds)
    outData$covariateData$minMaxs <- NULL
    done <- TRUE
  } else {
    ParallelLogger::logInfo("Applying min-max normalization of continuous features to test data")
    # NULL for models saved before implicit zeros were handled
    implicitZeroIds <- attr(featureEngineeringSettings, "implicitZeroCovariateIds")
    addImplicitZeros(outData, implicitZeroIds)
    # apply the normalization to test data by using saved normalization values
    outData$covariateData$minMaxs <- restoreSavedTable(attr(featureEngineeringSettings, "minMaxs"))
    on.exit(outData$covariateData$minMaxs <- NULL, add = TRUE)
    outData$covariateData$covariates <- outData$covariateData$covariates %>%
      dplyr::left_join(outData$covariateData$minMaxs,
        by = "covariateId") %>%
      dplyr::mutate(covariateValue = dplyr::coalesce(
        (.data$covariateValue - .data$min) / (.data$max - .data$min),
        .data$covariateValue
      )) %>%
      dplyr::select(-c("max", "min"))
    removeStoredZeros(outData$covariateData, implicitZeroIds)
  }
  outData <- recordFeatureEngineeringStep(
    outData,
    funct = "minMaxNormalize",
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings,
      done = done
    )
  )
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
#' @param trainData The data to normalize: the training data, or new data when
#'   `done = TRUE`. It is changed in place.
#' @param featureEngineeringSettings The settings for the normalization
#' @param done Whether the normalization has already been fitted (bool)
#' @return `trainData`, normalized, with the step recorded for prediction
#' @keywords internal
robustNormalize <- function(trainData, featureEngineeringSettings, done = FALSE) {
  start <- Sys.time()
  outData <- trainData
  if (!done) {
    ParallelLogger::logInfo("Starting robust normalization of continuous features")
    implicitZeroIds <- implicitZeroCovariateIds(outData)
    addImplicitZeros(outData, implicitZeroIds)
    attr(featureEngineeringSettings, "implicitZeroCovariateIds") <- implicitZeroIds
    # find continuous features from trainData$covariateData$analysisRef
    continousFeatures <- outData$covariateData$analysisRef %>%
      dplyr::filter(.data$isBinary == "N") %>%
      dplyr::select("analysisId") %>%
      dplyr::inner_join(outData$covariateData$covariateRef, by = "analysisId") %>%
      dplyr::pull(.data$covariateId)

    # get (25, 75)% quantiles of each feature
    if (inherits(outData$covariateData$covariates, "tbl_sql")) {
      outData$covariateData$quantiles <- outData$covariateData$covariates %>%
        dplyr::filter(.data$covariateId %in% continousFeatures) %>%
        dplyr::group_by(.data$covariateId) %>%
        dplyr::summarise(
          q25 = dbplyr::sql("quantile_cont(covariateValue, 0.25)"),
          q75 = dbplyr::sql("quantile_cont(covariateValue, 0.75)"),
          median = dbplyr::sql("median(covariateValue)")
        ) %>%
        dplyr::mutate(iqr = .data$q75 - .data$q25) %>%
        dplyr::mutate(
          iqr = dplyr::if_else(.data$iqr == 0, 1.0, .data$iqr) # if iqr is 0, set it to 1 to avoid division by 0
        ) %>%
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
        dplyr::mutate(
          iqr = dplyr::if_else(.data$iqr == 0, 1.0, .data$iqr), # if iqr is 0, set it to 1 to avoid division by 0
        ) %>%
        dplyr::select(-c("q75", "q25")) %>%
        dplyr::collect()
    }
    on.exit(outData$covariateData$quantiles <- NULL, add = TRUE)

    # save the normalization
    attr(featureEngineeringSettings, "quantiles") <-
      outData$covariateData$quantiles %>% dplyr::collect()
    clip <- featureEngineeringSettings$settings$clip

    # apply the normalization to trainData
    outData$covariateData$covariates <- outData$covariateData$covariates %>%
      dplyr::left_join(outData$covariateData$quantiles, by = "covariateId") %>%
      dplyr::mutate(normalizeCovariate = dplyr::coalesce(
        (.data$iqr == .data$iqr) & (.data$median == .data$median),
        FALSE
      )) %>%
      # Only continuous features have median/iqr rows after the join.
      dplyr::mutate(covariateValue = dplyr::coalesce(
        (.data$covariateValue - .data$median) / .data$iqr,
        .data$covariateValue
      )) %>%
      # optionally if settings$clip is TRUE.
      # smoothly clip the range to [-3, 3] with  x / sqrt(1 + (x/3)^2)
      # ref: https://arxiv.org/abs/2407.04491
      dplyr::mutate(covariateValue = ifelse(.data$normalizeCovariate & !!clip,
        .data$covariateValue / sqrt(1 + (.data$covariateValue / 3)^2),
        .data$covariateValue
      )) %>%
      dplyr::select(-c("median", "iqr", "normalizeCovariate"))
    removeStoredZeros(outData$covariateData, implicitZeroIds)
    done <- TRUE
  } else {
    ParallelLogger::logInfo("Applying robust normalization of continuous features to test data")
    # NULL for models saved before implicit zeros were handled
    implicitZeroIds <- attr(featureEngineeringSettings, "implicitZeroCovariateIds")
    addImplicitZeros(outData, implicitZeroIds)
    # apply the normalization to test data by using saved normalization values
    outData$covariateData$quantiles <- restoreSavedTable(attr(featureEngineeringSettings, "quantiles"))
    on.exit(outData$covariateData$quantiles <- NULL, add = TRUE)
    clip <- featureEngineeringSettings$settings$clip
    outData$covariateData$covariates <- outData$covariateData$covariates %>%
      dplyr::left_join(outData$covariateData$quantiles,
        by = "covariateId", copy = TRUE
      ) %>%
      dplyr::mutate(normalizeCovariate = dplyr::coalesce(
        (.data$iqr == .data$iqr) & (.data$median == .data$median),
        FALSE
      )) %>%
      dplyr::mutate(covariateValue = dplyr::coalesce(
        (.data$covariateValue - .data$median) / .data$iqr,
        .data$covariateValue
      )) %>%
      dplyr::mutate(covariateValue = ifelse(.data$normalizeCovariate & !!clip,
        .data$covariateValue / sqrt(1 + (.data$covariateValue / 3)^2),
        .data$covariateValue
      )) %>%
      dplyr::select(-c("median", "iqr", "normalizeCovariate"))
    removeStoredZeros(outData$covariateData, implicitZeroIds)
  }
  outData <- recordFeatureEngineeringStep(
    outData,
    funct = "robustNormalize",
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings,
      done = done
    )
  )
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
#' @param trainData The data to remove rare features from: the training data, or
#'   new data when `done = TRUE`. It is changed in place.
#' @param featureEngineeringSettings The settings for the rare feature removal
#' @param done Whether to find and remove rare features or remove them only (bool)
#' @return `trainData`, with rare features removed and the step recorded for
#'   prediction
#' @keywords internal
removeRareFeatures <- function(trainData, featureEngineeringSettings, done = FALSE) {
  start <- Sys.time()
  outData <- trainData
  if (!done) {
    ParallelLogger::logInfo(
      "Removing features rarer than threshold: ", featureEngineeringSettings$threshold,
      " from the data"
    )
    rowIds <- populationRowIds(outData)
    populationSize <- if (is.null(rowIds)) {
      outData$covariateData$covariates %>%
        dplyr::summarise(popSize = dplyr::n_distinct(.data$rowId)) %>%
        dplyr::pull()
    } else {
      length(rowIds)
    }
    rareFeatures <- outData$covariateData$covariates %>%
      dplyr::group_by(.data$covariateId) %>%
      dplyr::summarise(count = dplyr::n_distinct(.data$rowId)) %>%
      dplyr::collect()
    rareFeatures <- rareFeatures %>%
      dplyr::mutate(ratio = .data$count / populationSize) %>%
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
    outData$covariateData$covariates <- outData$covariateData$covariates %>%
      dplyr::filter(
        !.data$covariateId %in% !!attr(featureEngineeringSettings, "rareFeatures")
      )
    outData$covariateData$covariateRef <- outData$covariateData$covariateRef %>%
      dplyr::filter(
        !.data$covariateId %in% !!attr(featureEngineeringSettings, "rareFeatures")
      )
  }
  outData <- recordFeatureEngineeringStep(
    outData,
    funct = "removeRareFeatures",
    settings = list(
      featureEngineeringSettings = featureEngineeringSettings,
      done = done
    )
  )
  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste0(
    "Finished rare feature removal in ",
    signif(delta, 3), " ", attr(delta, "units")
  ))
  return(outData)
}
