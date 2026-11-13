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

test_that("createTuningMetric requires an explicit name", {
  dummyMetric <- function(prediction) mean(prediction$value)

  expect_error(
    createTuningMetric(fun = dummyMetric),
    "argument \"name\" is missing",
    fixed = FALSE
  )
})

test_that("createTuningMetric handles custom metric functions and funArgs", {
  customMetric <- function(prediction, scale) sum(prediction$value) * scale

  metric <- createTuningMetric(
    fun = customMetric,
    maximize = FALSE,
    name = "ScaledSum",
    funArgs = list(scale = 0.5)
  )

  prediction <- data.frame(value = c(1, 2, 3))
  expect_equal(metric$fun(prediction), sum(prediction$value) * 0.5)
  expect_false(metric$maximize)
  expect_equal(metric$name, "ScaledSum")
})

test_that("auprcMetric wraps averagePrecision", {
  prediction <- data.frame(
    value = c(0.1, 0.6, 0.4, 0.8),
    outcomeCount = c(0, 1, 1, 0)
  )
  auprcFun <- function(prediction) {
    positive <- prediction$value[prediction$outcomeCount == 1]
    negative <- prediction$value[prediction$outcomeCount == 0]
    pr <- PRROC::pr.curve(scores.class0 = positive, scores.class1 = negative)
    auprc <- pr$auc.integral
  }

  expect_equal(auprcMetric$name, "AUPRC")
  expect_true(auprcMetric$maximize)
  expect_equal(auprcMetric$fun(prediction), auprcFun(prediction))
})

test_that("createHyperparameterSettings accepts generator objects with lifecycle hooks", {
  generator <- list(
    initialize = function(definition, settings) invisible(NULL),
    getNext = function(history) NULL,
    finalize = function(history) invisible(NULL)
  )

  settings <- createHyperparameterSettings(generator = generator)

  expect_equal(settings$search, "custom")
  expect_identical(settings$generator, generator)
})

test_that("createHyperparameterSettings accepts generator objects without finalize", {
  generator <- list(
    initialize = function(definition, settings) invisible(NULL),
    getNext = function(history) NULL
  )

  settings <- createHyperparameterSettings(generator = generator)

  expect_equal(settings$search, "custom")
  expect_identical(settings$generator, generator)
})

test_that("createHyperparameterSettings rejects invalid generator objects", {
  expect_error(
    createHyperparameterSettings(generator = list(getNext = function(history) NULL)),
    "`generator` must be a function or an object with initialize and getNext methods",
    fixed = TRUE
  )
  expect_error(
    createHyperparameterSettings(generator = list(
      initialize = function(definition, settings) invisible(NULL),
      getNext = function(history) NULL,
      finalize = "not a function"
    )),
    "`generator` must be a function or an object with initialize and getNext methods",
    fixed = TRUE
  )
  expect_error(
    createHyperparameterSettings(generator = 1),
    "`generator` must be a function or an object with initialize and getNext methods",
    fixed = TRUE
  )
})

test_that("prepareHyperparameterGrid returns sequential combinations for grid search", {
  paramDefinition <- list(alpha = list(0.1, 0.2), lambda = list(1L, 2L))
  iterator <- prepareHyperparameterGrid(
    paramDefinition,
    createHyperparameterSettings(search = "grid")
  )

  combo1 <- iterator$getNext(NULL)
  combo2 <- iterator$getNext(NULL)
  combo3 <- iterator$getNext(NULL)
  combo4 <- iterator$getNext(NULL)

  expect_equal(combo1$alpha, 0.1)
  expect_equal(combo1$lambda, 1L)
  expect_equal(combo2$alpha, 0.2)
  expect_equal(combo2$lambda, 1L)
  expect_equal(combo3$alpha, 0.1)
  expect_equal(combo3$lambda, 2L)
  expect_equal(combo4$alpha, 0.2)
  expect_equal(combo4$lambda, 2L)
  expect_null(iterator$getNext(NULL))
})

test_that("prepareHyperparameterGrid respects random sampling size", {
  paramDefinition <- list(alpha = list(1L, 2L, 3L, 4L))
  settings <- createHyperparameterSettings(
    search = "random",
    sampleSize = 2,
    randomSeed = NULL
  )

  withr::local_seed(100)
  iterator <- prepareHyperparameterGrid(paramDefinition, settings)

  combos <- list()
  repeat {
    candidate <- iterator$getNext(NULL)
    if (is.null(candidate)) {
      break
    }
    combos[[length(combos) + 1]] <- candidate
  }

  expect_length(combos, 2)
  alphas <- vapply(combos, `[[`, numeric(1), "alpha")
  expect_true(all(alphas %in% c(1, 2, 3, 4)))
  expect_equal(length(unique(alphas)), length(alphas))
})

test_that("prepareHyperparameterGrid handles complex GBM-style grids", {
  paramDefinition <- list(
    ntrees = list(100, 300),
    earlyStopRound = list(25),
    maxDepth = list(4, 6, 8),
    minChildWeight = list(1),
    learnRate = list(0.05, 0.1, 0.3),
    lambda = list(1),
    alpha = list(0),
    scalePosWeight = list(1)
  )

  iterator <- prepareHyperparameterGrid(paramDefinition, createHyperparameterSettings(search = "grid"))

  combos <- list()
  repeat {
    candidate <- iterator$getNext(NULL)
    if (is.null(candidate)) {
      break
    }
    combos[[length(combos) + 1]] <- candidate
  }

  expect_length(combos, 2 * 1 * 3 * 1 * 3 * 1 * 1 * 1)
  expect_equal(names(combos[[1]]), names(paramDefinition))
  expect_equal(combos[[1]]$ntrees, 100)
  expect_equal(combos[[1]]$maxDepth, 4)
  expect_equal(combos[[1]]$learnRate, 0.05)
  expect_setequal(unique(vapply(combos, `[[`, numeric(1), "ntrees")), c(100, 300))
  expect_setequal(unique(vapply(combos, `[[`, numeric(1), "maxDepth")), c(4, 6, 8))
  expect_setequal(unique(vapply(combos, `[[`, numeric(1), "learnRate")), c(0.05, 0.1, 0.3))
  expect_true(all(vapply(combos, function(x) x$scalePosWeight, numeric(1)) == 1))
  expect_null(iterator$getNext(NULL))
})

test_that("prepareHyperparameterGrid accepts custom generator functions", {
  recorded <- NULL
  customGenerator <- function(definition, expanded, settings) {
    recorded <<- list(
      definition = definition,
      expanded = expanded,
      settings = settings
    )
    list(list(alpha = 99), list(alpha = 100))
  }

  settings <- createHyperparameterSettings(
    search = "custom",
    generator = customGenerator
  )
  iterator <- prepareHyperparameterGrid(list(alpha = list(1, 2)), settings)

  expect_equal(iterator$getNext(NULL)$alpha, 99)
  expect_equal(iterator$getNext(NULL)$alpha, 100)
  expect_null(iterator$getNext(NULL))
  expect_equal(recorded$definition, list(alpha = list(1, 2)))
  expect_equal(length(recorded$expanded), 2)
  expect_equal(recorded$settings$search, "custom")
})

test_that("prepareHyperparameterGrid handles generator objects with lifecycle hooks", {
  events <- new.env(parent = emptyenv())
  generator <- local({
    pool <- list()
    list(
      initialize = function(definition, settings) {
        events$initialized <- list(definition = definition, settings = settings)
        pool <<- list(list(alpha = 10), list(alpha = 20))
      },
      getNext = function(history) {
        if (length(pool) == 0) {
          return(NULL)
        }
        nextItem <- pool[[1]]
        pool <<- pool[-1]
        nextItem
      },
      finalize = function(history) {
        events$finalized <- history
        "finished"
      }
    )
  })

  settings <- createHyperparameterSettings(search = "custom")
  settings$generator <- generator
  iterator <- prepareHyperparameterGrid(list(alpha = list(1, 2)), settings)

  expect_equal(iterator$getNext(NULL)$alpha, 10)
  expect_equal(iterator$getNext(NULL)$alpha, 20)
  expect_null(iterator$getNext(NULL))
  expect_equal(events$initialized$definition, list(alpha = list(1, 2)))
  expect_equal(events$initialized$settings$search, "custom")

  history <- list(best = 123)
  expect_equal(iterator$finalize(history), "finished")
  expect_identical(events$finalized, history)
})

test_that("prepareHyperparameterGrid accepts legacy expanded grids", {
  expanded <- list(
    list(alpha = 0.1, lambda = 1L),
    list(alpha = 0.2, lambda = 2L)
  )

  iterator <- prepareHyperparameterGrid(
    paramDefinition = expanded,
    createHyperparameterSettings(search = "grid")
  )

  combo1 <- iterator$getNext(NULL)
  combo2 <- iterator$getNext(NULL)
  combo3 <- iterator$getNext(NULL)

  expect_equal(combo1$alpha, 0.1)
  expect_equal(combo1$lambda, 1L)
  expect_equal(combo2$alpha, 0.2)
  expect_equal(combo2$lambda, 2L)
  expect_null(combo3)
})

test_that("listCartesian works", {
  allList <- list(a = list(1, 2), b = list(NULL, "auto"), c = list(-1))
  
  paramLists <- listCartesian(allList)
  
  expect_equal(length(paramLists), 2 * 2 * 1)
  expect_equal(names(paramLists[[1]]), c("a", "b", "c"))
  expect_equal(length(paramLists[[1]]), 3)
})

drainIterator <- function(iterator) {
  candidates <- list()
  repeat {
    candidate <- iterator$getNext(list())
    if (is.null(candidate)) {
      break
    }
    candidates[[length(candidates) + 1]] <- candidate
  }
  iterator$finalize(list())
  candidates
}

test_that("expanded legacy grids honour random and custom searches", {
  legacyGrid <- list(list(alpha = 1), list(alpha = 2), list(alpha = 3))
  jsonGrid <- stats::setNames(legacyGrid, c("1", "2", "3"))

  for (grid in list(legacyGrid, jsonGrid)) {
    sampled <- drainIterator(prepareHyperparameterGrid(
      grid,
      createHyperparameterSettings(search = "random", sampleSize = 1, randomSeed = 7)
    ))
    expect_length(sampled, 1)
    expect_true(sampled[[1]]$alpha %in% 1:3)

    recorded <- NULL
    fromFunction <- drainIterator(prepareHyperparameterGrid(
      grid,
      createHyperparameterSettings(generator = function(definition, expanded, settings) {
        recorded <<- expanded
        expanded[2]
      })
    ))
    expect_equal(fromFunction, list(list(alpha = 2)))
    expect_equal(recorded, legacyGrid)
  }

  served <- FALSE
  initialized <- NULL
  lifecycle <- list(
    initialize = function(definition, settings) {
      initialized <<- definition
      invisible(NULL)
    },
    getNext = function(history) {
      if (served) {
        return(NULL)
      }
      served <<- TRUE
      list(alpha = 3)
    }
  )
  fromLifecycle <- drainIterator(prepareHyperparameterGrid(
    legacyGrid, createHyperparameterSettings(generator = lifecycle)
  ))
  expect_equal(fromLifecycle, list(list(alpha = 3)))
  expect_equal(initialized, legacyGrid)
})

test_that("expanded legacy grids still run every candidate under grid search", {
  legacyGrid <- list(list(alpha = 1), list(alpha = 2))
  jsonGrid <- stats::setNames(legacyGrid, c("1", "2"))

  for (grid in list(legacyGrid, jsonGrid)) {
    candidates <- drainIterator(prepareHyperparameterGrid(
      grid, createHyperparameterSettings(search = "grid")
    ))
    expect_equal(candidates, legacyGrid)
  }
})

sampleAlphas <- function(ambientSeed, randomSeed, definition = list(alpha = as.list(1:12))) {
  withr::local_seed(ambientSeed)
  candidates <- drainIterator(prepareHyperparameterGrid(
    definition,
    createHyperparameterSettings(search = "random", sampleSize = 3, randomSeed = randomSeed)
  ))
  vapply(candidates, function(x) as.numeric(x$alpha), numeric(1))
}

test_that("random search candidates follow randomSeed, not the ambient RNG", {
  expect_identical(sampleAlphas(1, 42), sampleAlphas(2, 42))
  expect_false(identical(sampleAlphas(1, 42), sampleAlphas(1, 99)))

  legacy <- lapply(1:12, function(i) list(alpha = i))
  expect_identical(sampleAlphas(1, 42, legacy), sampleAlphas(2, 42, legacy))
})

test_that("random search without randomSeed still follows the ambient RNG", {
  expect_identical(sampleAlphas(1, NULL), sampleAlphas(1, NULL))
  expect_false(identical(sampleAlphas(1, NULL), sampleAlphas(2, NULL)))
})

test_that("seeded random search leaves the ambient RNG untouched", {
  withr::local_seed(11)
  expected <- stats::runif(1)

  withr::local_seed(11)
  sampleSettings <- createHyperparameterSettings(
    search = "random", sampleSize = 3, randomSeed = 42
  )
  prepareHyperparameterGrid(list(alpha = as.list(1:12)), sampleSettings)
  expect_identical(stats::runif(1), expected)

  withr::with_preserve_seed({
    rm(".Random.seed", envir = .GlobalEnv)
    prepareHyperparameterGrid(list(alpha = as.list(1:12)), sampleSettings)
    expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  })
})

test_that("auprcMetric treats outcome counts above one as positive", {
  skip_if_not_installed("PRROC")
  prediction <- data.frame(
    value = c(0.9, 0.8, 0.7, 0.4, 0.3, 0.2, 0.1),
    outcomeCount = c(2, 1, 0, 2, 0, 1, 0)
  )
  binaryPrediction <- prediction
  binaryPrediction$outcomeCount <- as.numeric(prediction$outcomeCount > 0)

  expect_equal(auprcMetric$fun(prediction), auprcMetric$fun(binaryPrediction))
})

tuningData <- function() {
  list(
    data = matrix(1:24, nrow = 12),
    labels = data.frame(
      rowId = 1:12,
      outcomeCount = rep(c(0, 1), 6),
      index = rep(1:3, each = 4)
    )
  )
}

countingHooks <- function() {
  calls <- new.env()
  calls$train <- list()
  list(
    calls = calls,
    settings = list(
      modelName = "counting",
      train = function(dataMatrix, labels, hyperParameters, settings) {
        calls$train[[length(calls$train) + 1]] <- sort(unique(labels$index))
        list(value = hyperParameters$value)
      },
      predict = function(plpModel, data, cohort) {
        data.frame(
          rowId = cohort$rowId,
          outcomeCount = cohort$outcomeCount,
          index = cohort$index,
          value = rep(plpModel$value, nrow(cohort))
        )
      }
    )
  )
}

meanValueSettings <- function(validation = "cv") {
  createHyperparameterSettings(
    tuningMetric = createTuningMetric(fun = function(prediction) mean(prediction$value), name = "meanValue"),
    validation = validation
  )
}

test_that("createHyperparameterSettings checks validation and defaults to cv", {
  expect_equal(createHyperparameterSettings()$validation, "cv")
  expect_equal(createHyperparameterSettings(validation = "trainValidation")$validation, "trainValidation")
  expect_error(createHyperparameterSettings(validation = "holdout"), "must be \"cv\" or \"trainValidation\"")
  expect_error(createHyperparameterSettings(validation = c("cv", "trainValidation")), "must be")
})

test_that("trainValidation fits each candidate once and keeps the last fold's predictions", {
  d <- tuningData()
  hooks <- countingHooks()
  res <- tuneHyperparameters(
    data = d$data, labels = d$labels,
    param = list(value = list(0.2, 0.4)),
    settings = hooks$settings,
    hyperparamSettings = meanValueSettings("trainValidation")
  )
  # one fit per candidate on folds 1 and 2, then the final fit on all folds
  expect_equal(hooks$calls$train, list(c(1, 2), c(1, 2), c(1, 2, 3)))
  expect_equal(res$finalParam$value, 0.4)
  cv <- res$prediction[res$prediction$evaluationType == "TrainValidation", ]
  expect_equal(sort(cv$rowId), 9:12)
  expect_false("CV" %in% res$prediction$evaluationType)
  expect_equal(res$paramSearch[[1]]$hyperSummary$fold, c("TrainValidation", "Fold3"))
})

test_that("cv fits each candidate once per fold, as before", {
  d <- tuningData()
  hooks <- countingHooks()
  res <- tuneHyperparameters(
    data = d$data, labels = d$labels,
    param = list(value = list(0.2, 0.4)),
    settings = hooks$settings,
    hyperparamSettings = meanValueSettings()
  )
  expect_length(hooks$calls$train, 2 * 3 + 1)
  expect_equal(sort(res$prediction$rowId[res$prediction$evaluationType == "CV"]), 1:12)
  expect_equal(res$paramSearch[[1]]$hyperSummary$fold, c("CV", "Fold1", "Fold2", "Fold3"))

  # settings saved by earlier versions have no validation and use cv
  oldSettings <- meanValueSettings()
  oldSettings$validation <- NULL
  hooks <- countingHooks()
  tuneHyperparameters(
    data = d$data, labels = d$labels, param = list(value = list(0.2)),
    settings = hooks$settings, hyperparamSettings = oldSettings
  )
  expect_length(hooks$calls$train, 3 + 1)
})

test_that("trainValidation needs at least two folds", {
  d <- tuningData()
  d$labels$index <- 1
  expect_error(
    tuneHyperparameters(
      data = d$data, labels = d$labels, param = list(value = list(0.2)),
      settings = countingHooks()$settings,
      hyperparamSettings = meanValueSettings("trainValidation")
    ),
    "needs at least 2 folds"
  )
})

test_that("the database form of hyperparameter settings writes validation only when not the default", {
  expect_null(sanitizeHyperparameterSettingsForDatabase(createHyperparameterSettings())$validation)
  expect_equal(
    sanitizeHyperparameterSettingsForDatabase(
      createHyperparameterSettings(validation = "trainValidation")
    )$validation,
    "trainValidation"
  )
})

test_that("Cyclops models say which hyperparameter settings they ignore", {
  # the warning is given before any data is touched
  expect_warning(
    try(fitCyclopsModel(trainData = NULL, modelSettings = list(),
      hyperparameterSettings = createHyperparameterSettings(validation = "trainValidation")
    ), silent = TRUE),
    "ignore these hyperparameterSettings: validation"
  )
  expect_warning(
    try(fitCyclopsModel(trainData = NULL, modelSettings = list(),
      hyperparameterSettings = createHyperparameterSettings(search = "random", sampleSize = 1)
    ), silent = TRUE),
    "ignore these hyperparameterSettings: search"
  )
  expect_no_warning(
    try(fitCyclopsModel(trainData = NULL, modelSettings = list(),
      hyperparameterSettings = createHyperparameterSettings()
    ), silent = TRUE)
  )
})
