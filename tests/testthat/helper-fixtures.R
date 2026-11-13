# Builders for small hand-made test data. The caller closes the Andromeda
# objects, for example with withr::defer(Andromeda::close(data$covariateData)).

# One covariateRef row per covariate, all in one analysis.
defaultCovariateRef <- function(covariates, analysisId = 1) {
  covariateIds <- sort(unique(covariates$covariateId))
  data.frame(
    covariateId = covariateIds,
    covariateName = paste0("covariate", covariateIds),
    analysisId = analysisId,
    conceptId = 0
  )
}

# One analysisRef row per analysis in covariateRef.
defaultAnalysisRef <- function(covariateRef, isBinary = "N", missingMeansZero = "Y") {
  analysisIds <- sort(unique(covariateRef$analysisId))
  data.frame(
    analysisId = analysisIds,
    analysisName = paste0("analysis", analysisIds),
    domainId = "test",
    startDay = NA_real_,
    endDay = NA_real_,
    isBinary = isBinary,
    missingMeansZero = missingMeansZero
  )
}

# A CovariateData object. Extra tables, such as timeRef, go in `...`.
makeCovariateData <- function(covariates,
                              covariateRef = defaultCovariateRef(covariates),
                              analysisRef = defaultAnalysisRef(covariateRef),
                              metaData = list(),
                              ...) {
  covariateData <- Andromeda::andromeda(
    covariates = covariates,
    covariateRef = covariateRef,
    analysisRef = analysisRef,
    ...
  )
  covariateData <- asCovariateData(covariateData)
  attr(covariateData, "metaData") <- metaData
  covariateData
}

# A plpData object: labels and folds for training code, cohorts and outcomes
# for extraction-level code, or any mix. NULL elements are left out.
makePlpData <- function(covariateData = NULL,
                        labels = NULL,
                        folds = NULL,
                        cohorts = NULL,
                        outcomes = NULL,
                        metaData = NULL) {
  plpData <- list(
    labels = labels,
    folds = folds,
    cohorts = cohorts,
    outcomes = outcomes,
    covariateData = covariateData
  )
  plpData <- plpData[!vapply(plpData, is.null, logical(1))]
  class(plpData) <- "plpData"
  if (!is.null(metaData)) {
    attr(plpData, "metaData") <- metaData
  }
  plpData
}

# Labels of n patients without outcomes, and all of them in fold 1.
zeroLabels <- function(rowIds) {
  data.frame(rowId = rowIds, outcomeCount = 0)
}
oneFold <- function(rowIds) {
  data.frame(rowId = rowIds, index = 1)
}

# Messages logged with ParallelLogger::logWarn() while evaluating expr.
capturedWarnings <- function(expr) {
  logged <- character(0)
  testthat::with_mocked_bindings(
    expr,
    logWarn = function(...) {
      logged <<- c(logged, paste0(...))
      invisible(NULL)
    },
    .package = "ParallelLogger"
  )
  logged
}

# The metaData of a training set built by hand for model fitting code.
syntheticTrainMetaData <- function() {
  list(
    targetId = 1L, outcomeId = 2L,
    populationSettings = createStudyPopulationSettings(),
    covariateSettings = FeatureExtraction::createCovariateSettings(useDemographicsAge = TRUE),
    splitSettings = createDefaultSplitSetting(splitSeed = 42),
    cdmDatabaseId = "synthetic", cdmDatabaseName = "synthetic"
  )
}
