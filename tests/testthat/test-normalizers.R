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

test_that("createNormalizer works", {
  normalizer <- createNormalizer(type = "minmax")
  expect_equal(normalizer$type, "minmax")
  expect_equal(attr(normalizer, "fun"), "minMaxNormalize")
  expect_s3_class(normalizer, "featureEngineeringSettings")

  normalizer <- createNormalizer(type = "robust", settings = list(clip = TRUE))
  expect_equal(normalizer$type, "robust")

  expect_error(createNormalizer(type = "mean"))
  expect_error(createNormalizer(type = "median"))
  expect_error(createNormalizer(type = "zscore"))
  expect_error(createNormalizer(type = "none"))
  expect_error(createNormalizer(
    type = "robust",
    settings = list(clip = 3)
  ))
})

# The normalization arithmetic on data frames. The steps run the same functions
# on Andromeda tables, which the implicit zero and saved normalizer tests below
# exercise.

# covariate 10 has an outlier, covariate 20 is constant
normalizerCovariates <- function() {
  data.frame(
    rowId = c(1:6, 1:3),
    covariateId = c(rep(10, 6), rep(20, 3)),
    covariateValue = c(-10, -2, 0, 3, 8, 50, 1, 1, 1)
  )
}
outlierValues <- c(-10, -2, 0, 3, 8, 50)

test_that("minMaxStatistics gives each covariate's range and a unit range for constants", {
  stats <- minMaxStatistics(normalizerCovariates(), c(10, 20))
  stats <- stats[order(stats$covariateId), ]
  expect_equal(stats$min, c(-10, 1))
  # a constant gets max = min + 1, so it normalizes to 0 instead of NaN
  expect_equal(stats$max, c(50, 2))
  expect_equal(minMaxStatistics(normalizerCovariates(), 10)$covariateId, 10)
})

test_that("applyMinMax scales covariates with statistics and leaves the others", {
  covariates <- rbind(normalizerCovariates(), data.frame(rowId = 1, covariateId = 30, covariateValue = 7))
  stats <- minMaxStatistics(covariates, c(10, 20))
  scaled <- applyMinMax(covariates, stats)
  value <- function(id) scaled$covariateValue[scaled$covariateId == id]
  expect_equal(value(10), (outlierValues + 10) / 60)
  expect_equal(value(20), c(0, 0, 0))
  expect_equal(value(30), 7)
  # new data outside the training range lands outside [0, 1]
  newData <- data.frame(rowId = 1, covariateId = 10, covariateValue = 110)
  expect_equal(applyMinMax(newData, stats)$covariateValue, 2)
})

test_that("robustStatistics gives median and IQR, and a unit IQR for constants", {
  stats <- robustStatistics(normalizerCovariates(), c(10, 20))
  stats <- stats[order(stats$covariateId), ]
  expect_equal(stats$median, c(median(outlierValues), 1))
  expect_equal(stats$iqr, c(IQR(outlierValues), 1))
})

test_that("the normalizers compute the same on an Andromeda table as on a data frame", {
  covariates <- rbind(normalizerCovariates(), data.frame(rowId = 1, covariateId = 30, covariateValue = 7))
  andromeda <- Andromeda::andromeda(covariates = covariates)
  withr::defer(Andromeda::close(andromeda))
  sortedRows <- function(x) {
    x <- as.data.frame(x)
    x <- x[do.call(order, x[intersect(c("covariateId", "rowId"), names(x))]), , drop = FALSE]
    rownames(x) <- NULL
    x
  }
  ids <- c(10, 20)

  # the steps store the statistics next to the covariates before applying them
  andromeda$minMaxs <- minMaxStatistics(andromeda$covariates, ids)
  expect_equal(sortedRows(andromeda$minMaxs), sortedRows(minMaxStatistics(covariates, ids)))
  expect_equal(
    sortedRows(applyMinMax(andromeda$covariates, andromeda$minMaxs)),
    sortedRows(applyMinMax(covariates, minMaxStatistics(covariates, ids)))
  )

  # DuckDB's quantile_cont and median against R's quantile and median
  andromeda$quantiles <- robustStatistics(andromeda$covariates, ids)
  expect_equal(sortedRows(andromeda$quantiles), sortedRows(robustStatistics(covariates, ids)))
  for (clip in c(FALSE, TRUE)) {
    expect_equal(
      sortedRows(applyRobust(andromeda$covariates, andromeda$quantiles, clip = clip)),
      sortedRows(applyRobust(covariates, robustStatistics(covariates, ids), clip = clip)),
      info = paste("clip:", clip)
    )
  }
})

test_that("applyRobust centres, scales and optionally clips covariates with statistics", {
  covariates <- rbind(normalizerCovariates(), data.frame(rowId = 1, covariateId = 30, covariateValue = 7))
  stats <- robustStatistics(covariates, c(10, 20))
  z <- (outlierValues - median(outlierValues)) / IQR(outlierValues)
  for (clip in c(FALSE, TRUE)) {
    scaled <- applyRobust(covariates, stats, clip = clip)
    value <- function(id) scaled$covariateValue[scaled$covariateId == id]
    expect_equal(value(10), if (clip) z / sqrt(1 + (z / 3)^2) else z, info = paste("clip:", clip))
    expect_equal(value(20), c(0, 0, 0), info = paste("clip:", clip))
    expect_equal(value(30), 7, info = paste("clip:", clip))
  }
  # clipping keeps even the outlier inside (-3, 3)
  clipped <- applyRobust(covariates, stats, clip = TRUE)
  expect_true(all(abs(clipped$covariateValue[clipped$covariateId == 10]) < 3))
})

createZeroData <- function(values, missingMeansZero = "Y", rowIds = seq_along(values), plain = FALSE) {
  stored <- values != 0
  covariateData <- makeCovariateData(
    covariates = data.frame(
      rowId = rowIds[stored],
      covariateId = 1001,
      covariateValue = values[stored]
    ),
    covariateRef = data.frame(
      covariateId = 1001,
      covariateName = "score",
      analysisId = 1,
      conceptId = 0
    ),
    analysisRef = data.frame(
      analysisId = 1,
      analysisName = "score",
      domainId = "Condition",
      startDay = 0,
      endDay = 0,
      isBinary = "N",
      missingMeansZero = missingMeansZero
    ),
    metaData = list(populationSize = length(rowIds))
  )
  if (plain) {
    return(makePlpData(covariateData, cohorts = data.frame(rowId = rowIds), outcomes = data.frame()))
  }
  makePlpData(covariateData, labels = zeroLabels(rowIds), folds = oneFold(rowIds))
}

matrixValues <- function(data, rowIds) {
  mapped <- toSparseM(data, data.frame(rowId = rowIds))
  values <- as.numeric(mapped$dataMatrix[, 1])
  values[order(mapped$labels$originalRowId)]
}

savedNormalizer <- function(data, name) {
  fittedFeatureEngineeringSettings(data, name)
}

test_that("min-max normalization includes implicit zeros", {
  data <- createZeroData(c(0, 1, 2))
  on.exit(Andromeda::close(data$covariateData), add = TRUE)

  normalized <- minMaxNormalize(data, createNormalizer("minmax"))

  expect_equal(matrixValues(normalized, 1:3), c(0, 0.5, 1))
  # zeros that stay zero are not stored
  expect_equal(normalized$covariateData$covariates %>% dplyr::tally() %>% dplyr::pull(), 2)
  expect_equal(
    data$covariateData$covariates %>% dplyr::tally() %>% dplyr::pull(),
    2
  )
})

test_that("robust normalization includes implicit zeros and stores non-zero results", {
  data <- createZeroData(c(0, 1, 2))
  on.exit(Andromeda::close(data$covariateData), add = TRUE)

  normalized <- robustNormalize(data, createNormalizer("robust", settings = list(clip = FALSE)))

  expect_equal(matrixValues(normalized, 1:3), c(-1, 0, 1))
  rows <- as.data.frame(normalized$covariateData$covariates)
  expect_equal(rows$covariateValue[rows$rowId == 1], -1)
})

test_that("robust normalization clips the implicit zero value when requested", {
  data <- createZeroData(c(0, 1, 2))
  on.exit(Andromeda::close(data$covariateData), add = TRUE)

  normalized <- robustNormalize(data, createNormalizer("robust", settings = list(clip = TRUE)))

  expect_equal(matrixValues(normalized, 1:3)[1], -1 / sqrt(1 + (-1 / 3)^2))
})

test_that("saved normalizers apply implicit zeros to new data", {
  for (norm in list(
    list(create = createNormalizer("minmax"), fun = minMaxNormalize, name = "minMaxNormalize", expected = c(0.5, 0, 1)),
    list(
      create = createNormalizer("robust", settings = list(clip = FALSE)),
      fun = robustNormalize, name = "robustNormalize", expected = c(0, -1, 1)
    )
  )) {
    train <- createZeroData(c(0, 1, 2))
    on.exit(Andromeda::close(train$covariateData), add = TRUE)
    fitted <- norm$fun(train, norm$create)
    settings <- savedNormalizer(fitted, norm$name)

    test <- createZeroData(c(1, 0, 2))
    on.exit(Andromeda::close(test$covariateData), add = TRUE)
    applied <- norm$fun(test, settings, done = TRUE)

    expect_equal(matrixValues(applied, 1:3), norm$expected)
  }
})

test_that("saved normalizers apply implicit zeros to plain plpData", {
  train <- createZeroData(c(0, 1, 2))
  on.exit(Andromeda::close(train$covariateData), add = TRUE)
  fitted <- minMaxNormalize(train, createNormalizer("minmax"))
  settings <- savedNormalizer(fitted, "minMaxNormalize")

  plain <- createZeroData(c(0, 1, 2), plain = TRUE)
  on.exit(Andromeda::close(plain$covariateData), add = TRUE)
  applied <- minMaxNormalize(plain, settings, done = TRUE)

  expect_equal(names(applied), names(plain))
  expect_equal(matrixValues(applied, 1:3), c(0, 0.5, 1))
})

test_that("normalizers treat absent rows as missing when missingMeansZero is N", {
  data <- createZeroData(c(0, 1, 2), missingMeansZero = "N")
  on.exit(Andromeda::close(data$covariateData), add = TRUE)

  normalized <- minMaxNormalize(data, createNormalizer("minmax"))
  rows <- as.data.frame(normalized$covariateData$covariates)

  # statistics come from stored rows, so min=1 and max=2
  expect_equal(rows$covariateValue[order(rows$rowId)], c(0, 1))
  expect_equal(nrow(rows), 2)
})

test_that("models saved before implicit zeros were handled replay unchanged", {
  data <- createZeroData(c(0, 1, 2))
  on.exit(Andromeda::close(data$covariateData), add = TRUE)
  settings <- createNormalizer("minmax")
  attr(settings, "minMaxs") <- data.frame(covariateId = 1001, max = 2, min = 1)

  applied <- minMaxNormalize(data, settings, done = TRUE)
  rows <- as.data.frame(applied$covariateData$covariates)

  expect_equal(nrow(rows), 2)
  expect_equal(rows$covariateValue[order(rows$rowId)], c(0, 1))
})

test_that("implicit zero covariates survive a JSON round trip of the settings", {
  train <- createZeroData(c(0, 1, 2))
  on.exit(Andromeda::close(train$covariateData), add = TRUE)
  fitted <- robustNormalize(train, createNormalizer("robust", settings = list(clip = FALSE)))
  jsonFile <- tempfile(fileext = ".json")
  on.exit(unlink(jsonFile), add = TRUE)
  ParallelLogger::saveSettingsToJson(
    attr(fitted$covariateData, "metaData")$featureEngineering,
    jsonFile
  )
  loaded <- ParallelLogger::loadSettingsFromJson(jsonFile)

  test <- createZeroData(c(1, 0, 2))
  on.exit(Andromeda::close(test$covariateData), add = TRUE)
  applied <- applyFeatureEngineering(test, loaded)

  expect_equal(matrixValues(applied, 1:3), c(0, -1, 1))
})

test_that("implicit zeros of several covariates are stored in one append", {
  covariateData <- makeCovariateData(
    data.frame(rowId = c(1, 2, 3), covariateId = c(1001, 1002, 1002), covariateValue = c(5, 6, 7)),
    metaData = list(populationSize = 3)
  )
  data <- makePlpData(covariateData, labels = zeroLabels(1:3), folds = oneFold(1:3))
  withr::defer(Andromeda::close(covariateData))
  appends <- 0
  appendToTable <- Andromeda::appendToTable
  local_mocked_bindings(
    appendToTable = function(...) {
      appends <<- appends + 1
      appendToTable(...)
    },
    .package = "Andromeda"
  )

  addImplicitZeros(data, c(1001, 1002))

  rows <- as.data.frame(covariateData$covariates)
  rows <- rows[order(rows$covariateId, rows$rowId), ]
  expect_equal(appends, 1)
  expect_equal(rows$rowId, c(1, 2, 3, 1, 2, 3))
  expect_equal(rows$covariateValue, c(5, 0, 0, 0, 6, 7))
})
