# Persistence of fitted feature engineering. Each step stores its fitted state
# in the attributes of its settings; savePlpModel() must give it back exactly,
# and files written by earlier versions within their four-decimal rounding.
# The states are built by hand as the fit functions store them, so no step is
# fitted or applied here: applying a state is tested with each step, and end to
# end in "a reloaded model with feature engineering predicts exactly as the
# fitted model" (test-saveloadplp.R).

savedStep <- function(funct, settings, ...) {
  attrs <- list(...)
  for (name in names(attrs)) {
    attr(settings, name) <- attrs[[name]]
  }
  list(funct = funct, settings = list(featureEngineeringSettings = settings, done = TRUE))
}

indicatorInfo <- function() {
  list(
    map = data.frame(sourceCovariateId = c(10, 666), indicatorCovariateId = c(1010, 1666)),
    covariateRef = data.frame(
      covariateId = c(1010, 1666),
      covariateName = c("contFeature missing indicator", "fakeMissingVariable missing indicator"),
      analysisId = c(1010, 1666),
      conceptId = c(10, 666)
    ),
    analysisRef = data.frame(
      analysisId = c(1010, 1666),
      analysisName = c("continuous missing indicator", "missing missing indicator"),
      domainId = c("measurement", "missing"),
      startDay = NA_real_,
      endDay = NA_real_,
      isBinary = "Y",
      missingMeansZero = "Y"
    )
  )
}

emptyTable <- function(...) {
  columns <- list(...)
  as.data.frame(lapply(columns, function(type) vector(type, 0)), stringsAsFactors = FALSE)
}

# Values carry more than four decimals so that lost precision shows.
fittedStates <- function() {
  missingInfo <- data.frame(covariateId = c(666, 10), missing = c(0.3, 0.0123456789))
  list(
    simpleMean = savedStep(
      "simpleImpute",
      createSimpleImputer(method = "mean", missingThreshold = 0.5, addMissingIndicator = TRUE),
      missingIndicatorInfo = indicatorInfo(),
      missingInfo = missingInfo,
      imputer = data.frame(covariateId = c(10, 666), imputedValues = c(0.420371083, 0.446913427))
    ),
    simpleMedianEmpty = savedStep(
      "simpleImpute",
      createSimpleImputer(method = "median", missingThreshold = 0.5),
      missingInfo = emptyTable(covariateId = "numeric", missing = "numeric"),
      imputer = emptyTable(covariateId = "numeric", imputedValues = "numeric")
    ),
    iterative = savedStep(
      "iterativeImpute",
      createIterativeImputer(
        method = "pmm", missingThreshold = 0.5, addMissingIndicator = TRUE,
        methodSettings = list(pmm = list(k = 1, iterations = 1))
      ),
      missingIndicatorInfo = indicatorInfo(),
      initialValues = data.frame(covariateId = c(10, 666), initialValue = c(0.420371083, 0.446913427)),
      missingInfo = missingInfo,
      imputer = list(`666` = list(
        intercept = 0.443218765,
        coefficients = data.frame(covariateId = 10, values = 0.0123456789),
        predictions = data.frame(
          rowId = 1:4,
          prediction = c(0.4432188, 0.4434567, 0.4439876, 0.4441234),
          observedValue = c(0.1234567, 0.3456789, 0.5678901, 0.7890123)
        )
      ))
    ),
    minMax = savedStep(
      "minMaxNormalize",
      createNormalizer("minmax"),
      implicitZeroCovariateIds = numeric(0),
      minMaxs = data.frame(covariateId = c(10, 666), max = c(0.998765432, 0.991234567), min = c(0.012345678, 0.009512345))
    ),
    minMaxEmpty = savedStep(
      "minMaxNormalize",
      createNormalizer("minmax"),
      implicitZeroCovariateIds = numeric(0),
      minMaxs = emptyTable(covariateId = "numeric", max = "numeric", min = "numeric")
    ),
    robust = savedStep(
      "robustNormalize",
      createNormalizer("robust", settings = list(clip = TRUE)),
      implicitZeroCovariateIds = 666,
      quantiles = data.frame(covariateId = c(10, 666), median = c(0.390123456, 0.478912345), iqr = c(0.780123456, 0.434567891))
    ),
    robustEmpty = savedStep(
      "robustNormalize",
      createNormalizer("robust", settings = list(clip = FALSE)),
      implicitZeroCovariateIds = numeric(0),
      quantiles = emptyTable(covariateId = "numeric", median = "numeric", iqr = "numeric")
    ),
    rare = savedStep(
      "removeRareFeatures",
      createRareFeatureRemover(threshold = 0.01),
      rareFeatures = c(20, 30)
    ),
    rareNone = savedStep(
      "removeRareFeatures",
      createRareFeatureRemover(threshold = 0.01),
      rareFeatures = numeric(0)
    )
  )
}

# The steps as prediction reads them back from a file written with `writer`.
reloadedSteps <- function(state, writer) {
  jsonFile <- tempfile(fileext = ".json")
  on.exit(unlink(jsonFile))
  writer(list(featureEngineering = list(state)), jsonFile)
  featureEngineeringSteps(ParallelLogger::loadSettingsFromJson(jsonFile)$featureEngineering)
}

# What the JSON formats cannot keep, and the steps do not depend on, is made
# the same on both sides: an empty vector and an empty list outside tables both
# count as "empty"; columns of only NA are left out (JSON null has no type, and
# the row-by-row format of earlier versions drops them); and tables are plain
# data frames with default row names. Column types are otherwise compared.
canonicalState <- function(x) {
  if (is.data.frame(x)) {
    x <- as.data.frame(x)
    rownames(x) <- NULL
    allNa <- vapply(x, function(column) length(column) > 0 && all(is.na(column)), logical(1))
    return(x[, !allNa, drop = FALSE])
  }
  if (length(x) == 0 && (is.list(x) || is.atomic(x)) && is.null(attributes(x))) {
    return(NULL)
  }
  if (is.list(x)) {
    keptAttributes <- attributes(x)
    x <- lapply(x, canonicalState)
    attributes(x) <- keptAttributes
  }
  for (name in setdiff(names(attributes(x)), c("names", "class", "row.names"))) {
    attr(x, name) <- canonicalState(attr(x, name))
  }
  x
}

test_that("savePlpModel() gives back every fitted feature engineering state exactly", {
  for (name in names(fittedStates())) {
    state <- fittedStates()[[name]]
    expect_equal(
      canonicalState(reloadedSteps(state, saveSettingsToJsonFullPrecision)),
      canonicalState(featureEngineeringSteps(list(state))),
      tolerance = 0,
      info = name
    )
  }
})

test_that("fitted states saved by earlier versions come back within their rounding", {
  # earlier versions saved preprocessing with ParallelLogger::saveSettingsToJson(),
  # which wrote the learned tables row by row with four decimals: an absolute
  # error up to 5e-5, under 1% of the values used here (all above 0.009)
  for (name in grep("Empty$", names(fittedStates()), value = TRUE, invert = TRUE)) {
    state <- fittedStates()[[name]]
    expect_equal(
      canonicalState(reloadedSteps(state, ParallelLogger::saveSettingsToJson)),
      canonicalState(featureEngineeringSteps(list(state))),
      tolerance = 0.01,
      info = name
    )
  }
})
