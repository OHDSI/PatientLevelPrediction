# Data builders shared by the imputation test files.

# add a test numerical feature with missing values of certain percentage
subsetSplitData <- function(data, n = 250, seed = 1) {
  stopifnot(is.list(data), !is.null(data$labels), !is.null(data$covariateData))

  labels <- data$labels
  covariateRowIds <- dplyr::collect(
    dplyr::distinct(
      dplyr::select(data$covariateData$covariates, "rowId")
    )
  )$rowId
  rowIds <- intersect(labels$rowId, covariateRowIds)
  if (length(rowIds) > n) {
    rowIds <- withr::with_seed(seed, sample(rowIds, size = n))
  }

  out <- list(
    labels = dplyr::filter(labels, .data$rowId %in% !!rowIds),
    covariateData = Andromeda::andromeda(
      analysisRef = data$covariateData$analysisRef
    )
  )

  if (!is.null(data$folds)) {
    out$folds <- dplyr::filter(data$folds, .data$rowId %in% !!rowIds)
  }

  out$covariateData$covariates <- dplyr::filter(data$covariateData$covariates, .data$rowId %in% !!rowIds)
  out$covariateData$covariateRef <- data$covariateData$covariateRef

  attributes(out$covariateData)$metaData <- attributes(data$covariateData)$metaData
  class(out$covariateData) <- class(data$covariateData)

  attributes(out)$metaData <- attributes(data)$metaData
  class(out) <- class(data)
  out
}

makeToyImputationData <- function(n = 150, seed = 1) {
  labels <- data.frame(rowId = seq_len(n))
  folds <- data.frame(
    rowId = labels$rowId,
    index = 1L
  )
  covariates <- withr::with_seed(seed, {
    data.frame(
      rowId = rep(seq_len(n), each = 2),
      covariateId = rep(c(10, 20), times = n),
      covariateValue = c(
        runif(n),
        stats::rbinom(n, size = 1, prob = 0.3)
      )
    )
  })
  covariateRef <- data.frame(
    covariateId = c(10, 20),
    covariateName = c("contFeature", "binFeature"),
    analysisId = c(10, 20),
    conceptId = c(10, 20)
  )
  analysisRef <- data.frame(
    analysisId = c(10, 20),
    analysisName = c("continuous", "binary"),
    domainId = c("measurement", "feature engineering"),
    startDay = c(NA_real_, NA_real_),
    endDay = c(NA_real_, NA_real_),
    isBinary = c("N", "Y"),
    missingMeansZero = c("N", "Y")
  )
  covariateData <- Andromeda::andromeda(
    covariates = covariates,
    covariateRef = covariateRef,
    analysisRef = analysisRef
  )
  class(covariateData) <- "CovariateData"
  attr(covariateData, "metaData") <- list()

  trainData <- list(
    labels = labels,
    folds = folds,
    covariateData = covariateData
  )
  class(trainData) <- "plpData"
  attr(trainData, "metaData") <- list()

  trainData
}

createMissingData <- function(trainData, missingness, test = FALSE) {
  missingData <- list(
    labels = trainData$labels
  )
  if (!test) {
    missingData$folds <- trainData$folds
  }
  missingData$covariateData <- Andromeda::copyAndromeda(trainData$covariateData)
  rowIds <- missingData$labels$rowId
  nData <- floor(length(rowIds) * (1 - missingness))
  covariateId <- rep(666, nData)
  withr::with_seed(
    1234,
    covariateValue <- runif(n = nData)
  )
  Andromeda::appendToTable(
    missingData$covariateData$covariates,
    data.frame(
      rowId = rowIds[1:nData],
      covariateId = covariateId,
      covariateValue = covariateValue
    )
  )
  Andromeda::appendToTable(
    (missingData$covariateData$covariateRef),
    data.frame(
      covariateId = 666,
      covariateName = "fakeMissingVariable",
      analysisId = 666,
      conceptId = 666
    )
  )
  Andromeda::appendToTable(
    missingData$covariateData$analysisRef,
    data.frame(
      analysisId = 666,
      analysisName = "missing",
      domainId = "missing",
      startDay = NA,
      endDay = NA,
      isBinary = "N",
      missingMeansZero = "N"
    )
  )
  missingData
}

makeWideMissingImputationData <- function(
  n = 80,
  nFeatures = 25,
  missingness = 0.75,
  seed = 1,
  test = FALSE
) {
  stopifnot(nFeatures > 0, missingness >= 0, missingness < 1)
  out <- makeToyImputationData(n = n, seed = seed)
  if (test) {
    out$folds <- NULL
  }

  labels <- out$labels
  covariateIds <- seq_len(nFeatures) + 1000L
  nObserved <- max(1L, floor(n * (1 - missingness)))

  covariates <- withr::with_seed(seed, {
    lapply(covariateIds, function(covariateId) {
      observedRows <- sort(sample(labels$rowId, nObserved, replace = FALSE))
      data.frame(
        rowId = observedRows,
        covariateId = covariateId,
        covariateValue = runif(nObserved)
      )
    }) %>%
      dplyr::bind_rows()
  })

  covariateRef <- data.frame(
    covariateId = covariateIds,
    covariateName = paste0("contFeature", covariateIds),
    analysisId = covariateIds,
    conceptId = covariateIds
  )
  analysisRef <- data.frame(
    analysisId = covariateIds,
    analysisName = rep("continuous", length(covariateIds)),
    domainId = rep("measurement", length(covariateIds)),
    startDay = rep(NA_real_, length(covariateIds)),
    endDay = rep(NA_real_, length(covariateIds)),
    isBinary = rep("N", length(covariateIds)),
    missingMeansZero = rep("N", length(covariateIds))
  )
  Andromeda::appendToTable(
    out$covariateData$covariates,
    covariates
  )
  Andromeda::appendToTable(
    out$covariateData$covariateRef,
    covariateRef
  )
  Andromeda::appendToTable(
    out$covariateData$analysisRef,
    analysisRef
  )
  out
}

skip_if_no_sklearn_iterative <- function() {
  skip_if_not_installed("reticulate")
  available <- tryCatch(
    {
      reticulate::import("sklearn.experimental.enable_iterative_imputer")
      reticulate::import("sklearn")
      TRUE
    },
    error = function(e) FALSE
  )
  if (!available) {
    skip("scikit-learn IterativeImputer not available")
  }
}

makeImputationFixture <- function(rowIds,
                                  covariates,
                                  numericIds,
                                  binaryIds = numeric(0),
                                  analysisIds = c(numericIds, binaryIds),
                                  zeroMeansMissingIds = numeric(0),
                                  folds = FALSE) {
  covariateIds <- c(numericIds, binaryIds)
  analysisRef <- data.frame(
    analysisId = analysisIds,
    analysisName = paste0("analysis", analysisIds),
    domainId = "measurement",
    startDay = NA_real_,
    endDay = NA_real_,
    isBinary = ifelse(covariateIds %in% numericIds, "N", "Y"),
    missingMeansZero = ifelse(
      covariateIds %in% c(binaryIds, zeroMeansMissingIds),
      "Y",
      "N"
    )
  ) %>% dplyr::distinct()
  covariateData <- Andromeda::andromeda(
    covariates = covariates,
    covariateRef = data.frame(
      covariateId = covariateIds,
      covariateName = paste0("feature", covariateIds),
      analysisId = analysisIds,
      conceptId = covariateIds
    ),
    analysisRef = analysisRef
  )
  class(covariateData) <- "CovariateData"
  attr(covariateData, "metaData") <- list()
  data <- list(labels = data.frame(rowId = rowIds, outcomeCount = 0))
  if (folds) {
    data$folds <- data.frame(rowId = rowIds, index = 1L)
  }
  data$covariateData <- covariateData
  class(data) <- "plpData"
  attr(data, "metaData") <- list()
  data
}

covariateRowsOf <- function(data, covariateId) {
  data$covariateData$covariates %>%
    dplyr::filter(.data$covariateId == !!covariateId) %>%
    dplyr::collect() %>%
    dplyr::arrange(.data$rowId)
}

makeSavedPmmSettings <- function() {
  settings <- createIterativeImputer(
    missingThreshold = 0.8,
    methodSettings = list(pmm = list(k = 1, iterations = 1, alpha = 1))
  )
  attr(settings, "missingInfo") <- data.frame(
    covariateId = c(100, 200),
    missing = c(0.25, 0)
  )
  attr(settings, "initialValues") <- data.frame(
    covariateId = c(100, 200),
    initialValue = c(20, 20)
  )
  attr(settings, "imputer") <- list(
    "100" = list(
      intercept = 0,
      coefficients = data.frame(covariateId = 200, values = 1),
      predictions = data.frame(
        rowId = c(101, 102),
        prediction = c(10, 30),
        observedValue = c(10, 30)
      )
    )
  )
  settings
}
