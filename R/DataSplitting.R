# @file DataSplitting.R
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


#' Create the settings for defining how the plpData are split into
#' test/validation/train sets using default splitting functions 
#' (either random stratified by outcome, time or subject splitting)
#'
#' @details
#' Returns an object of class \code{splitSettings} that specifies the 
#' splitting function that will be called and the settings
#'
#' @param testFraction  (numeric) A real number between 0 and 1
#' indicating the test set fraction of the data
#' @param trainFraction (numeric) A real number between 0 and 1 indicating the 
#' train set fraction of the data. If not set train is equal to 1 - test
#' @param nfold             (numeric) An integer > 1 specifying the number of
#' folds used in cross validation
#' @param splitSeed   (numeric) A seed to use when splitting the data for 
#' reproducibility (if not set a random number will be generated)
#' @param type (character) Choice of: \itemize{
#'                                      \item'stratified' Each data point is 
#' randomly assigned into the test or a train fold set but this is done 
#' stratified such that the outcome rate is consistent in each partition
#'                                      \item'time' Older data are assigned 
#' into the training set and newer data are assigned into the test set
#'                                      \item'subject' Data are partitioned by
#' subject, if a subject is in the data more than once, all the data points for 
#' the subject are assigned either into the test data or into the train data 
#'(not both).
#'                                         }
#'
#' @return
#' An object of class \code{splitSettings}
#' @export
createDefaultSplitSetting <- function(testFraction = 0.25,
                                      trainFraction = 0.75,
                                      splitSeed = sample(100000, 1),
                                      nfold = 3,
                                      type = "stratified") {
  checkIsClass(testFraction, c("numeric", "integer"))
  checkHigherEqual(testFraction, 0)
  checkHigher(-1 * testFraction, -1)

  checkIsClass(trainFraction, c("numeric", "integer"))
  checkHigher(trainFraction, 0)
  checkHigherEqual(-1 * trainFraction, -1)

  checkIsClass(nfold, c("numeric", "integer"))
  checkHigher(nfold, 1)

  checkIsClass(splitSeed, c("numeric", "integer"))

  # add type check
  checkIsClass(type, c("character"))
  if (!type %in% c("stratified", "time", "subject")) {
    ParallelLogger::logError("Invalid type setting.
      Pick from: 'stratified','time','subject'")
    stop("Incorrect Type")
  }

  splitSettings <- list(
    test = testFraction,
    train = trainFraction,
    seed = splitSeed,
    nfold = nfold
  )

  if (type == "stratified") {
    attr(splitSettings, "fun") <- "randomSplitter"
  }
  if (type == "time") {
    attr(splitSettings, "fun") <- "timeSplitter"
  }
  if (type == "subject") {
    attr(splitSettings, "fun") <- "subjectSplitter"
  }
  class(splitSettings) <- "splitSettings"
  return(splitSettings)
}

#' Create the settings for defining how the plpData are split into
#' test/validation/train sets using an existing split - good to use for 
#' reproducing results from a different run 
#' @param splitIds (data.frame) A data frame with rowId and index columns of 
#' type integer/numeric. Index is -1 for test set, positive integer for train 
#' set folds
#' @return An object of class \code{splitSettings}
#' @export
createExistingSplitSettings <- function(splitIds) {
  checkIsClass(splitIds, "data.frame")
  checkColumnNames(splitIds, c("rowId", "index"))
  checkIsClass(splitIds$rowId, c("integer", "numeric"))
  checkIsClass(splitIds$index, c("integer", "numeric"))
  checkHigherEqual(splitIds$index, -1)

  splitSettings <- list(splitIds = splitIds)
  attr(splitSettings, "fun") <- "existingSplitter"
  class(splitSettings) <- "splitSettings"
  return(splitSettings)
}


#' Split the plpData into test/train sets using a splitting settings of class 
#' \code{splitSettings}
#'
#' @details
#' Returns a list containing the training data (Train) and optionally the test
#' data (Test). Train is an Andromeda object containing
#' \itemize{\item covariates: a table (rowId, covariateId, covariateValue)
#' containing the covariates for each data point in the train data
#'          \item covariateRef: a table with the covariate information
#'          \item labels: a table (rowId, outcomeCount, ...) for each data point
#' in the train data (outcomeCount is the class label)
#'          \item folds: a table (rowId, index) specifying which training 
#' fold each data point is in.
#'          }
#' Test is an Andromeda object containing
#' \itemize{\item covariates: a table (rowId, covariateId, covariateValue)
#' containing the covariates for each data point in the test data
#'          \item covariateRef: a table with the covariate information
#'          \item labels: a table (rowId, outcomeCount, ...) for each data
#' point in the test data (outcomeCount is the class label)
#'          }
#' @param plpData   An object of type \code{plpData} - the patient level 
#' prediction data extracted from the CDM.
#' @param population The population created using \code{createStudyPopulation} 
#' that define who will be used to develop the model
#' @param splitSettings An object of type \code{splitSettings} specifying the 
#' split - the default can be created using \code{createDefaultSplitSetting}
#'
#' @return
#' An object of class \code{splitSettings}
#' @export
splitData <- function(plpData = plpData,
                      population = population,
                      splitSettings = splitSettings) {
  fun <- attr(splitSettings, "fun")
  args <- list(
    population = population,
    splitSettings = splitSettings
  )
  splitId <- do.call(eval(parse(text = fun)), args)

  # now separate the data:
  if (sum(splitId$index < 0) == 0) {
    # NO TEST SET
    trainId <- splitId[splitId$index > 0, ]
    trainData <- list()
    class(trainData) <- "plpData"
    trainData$labels <- population %>% 
      dplyr::filter(.data$rowId %in% trainId$rowId)
    trainData$folds <- trainId

    # restrict to trainIds
    if (length(trainId$rowId) < 200000) {
      trainData$covariateData <- limitCovariatesToPopulation(
        plpData$covariateData,
        trainId$rowId
      )
    } else {
      trainData$covariateData <- batchRestrict(
        plpData$covariateData,
        data.frame(rowId = trainId$rowId),
        sizeN = 10000000
      )
    }
    metaData <- attr(population, "metaData")
    attr(trainData, "metaData") <- list(
      outcomeId = metaData$outcomeId,
      targetId = metaData$targetId,
      cdmDatabaseSchema = plpData$metaData$databaseDetails$cdmDatabaseSchema,
      cdmDatabaseName = plpData$metaData$databaseDetails$cdmDatabaseName,
      cdmDatabaseId = plpData$metaData$databaseDetails$cdmDatabaseId,
      restrictPlpDataSettings = metaData$restrictPlpDataSettings,
      covariateSettings = plpData$metaData$covariateSettings,
      populationSettings = metaData$populationSettings,
      attrition = metaData$attrition,
      splitSettings = splitSettings,
      populationSize = nrow(trainData$labels)
    )
    # add pop size to covariateData as used in tidyCovariates
    attr(trainData$covariateData, "metaData") <- 
      list(populationSize = nrow(trainData$labels))
    class(trainData$covariateData) <- "CovariateData"

    result <- list(Train = trainData)
  } else {
    trainId <- splitId[splitId$index > 0, ]
    trainData <- list()
    class(trainData) <- "plpData"
    trainData$labels <- population %>% 
      dplyr::filter(.data$rowId %in% trainId$rowId)
    trainData$folds <- trainId

    # restrict to trainIds
    if (length(trainId$rowId) < 200000) {
      trainData$covariateData <- limitCovariatesToPopulation(
        plpData$covariateData,
        trainId$rowId
      )
    } else {
      trainData$covariateData <- batchRestrict(
        plpData$covariateData,
        data.frame(rowId = trainId$rowId),
        sizeN = 10000000
      )
    }
    metaData <- attr(population, "metaData")
    attr(trainData, "metaData") <- list(
      outcomeId = metaData$outcomeId,
      targetId = metaData$targetId,
      cdmDatabaseSchema = plpData$metaData$databaseDetails$cdmDatabaseSchema,
      cdmDatabaseName = plpData$metaData$databaseDetails$cdmDatabaseName,
      cdmDatabaseId = plpData$metaData$databaseDetails$cdmDatabaseId,
      restrictPlpDataSettings = metaData$restrictPlpDataSettings,
      covariateSettings = plpData$metaData$covariateSettings,
      populationSettings = metaData$populationSettings,
      attrition = attr(population, "metaData")$attrition,
      splitSettings = splitSettings,
      populationSize = nrow(trainData$labels)
    )

    testId <- splitId[splitId$index < 0, ]
    testData <- list()
    class(testData) <- "plpData"
    testData$labels <- population %>%
      dplyr::filter(.data$rowId %in% testId$rowId)

    if (length(testId$rowId) < 200000) {
      testData$covariateData <- limitCovariatesToPopulation(
        plpData$covariateData,
        testId$rowId
      )
    } else {
      testData$covariateData <- batchRestrict(plpData$covariateData,
        data.frame(rowId = testId$rowId),
        sizeN = 10000000
      )
    }

    result <- list(
      Train = trainData,
      Test = testData
    )
  }

  class(result) <- "splitData"
  return(result)
}


dataSummary <- function(data) {
  ParallelLogger::logInfo("Train Set:")
  result <- data$Train$labels %>%
    dplyr::inner_join(data$Train$folds, by = "rowId") %>%
    dplyr::group_by(.data$index) %>%
    dplyr::summarise(
      N = length(.data$outcomeCount),
      outcomes = sum(.data$outcomeCount)
    ) %>%
    dplyr::collect()

  ParallelLogger::logInfo(paste("Fold ", result$index, " ", result$N,
    " patients with ", result$outcomes, " outcomes", sep = "", 
    collapse = " - "))


  result <- data$Train$covariateData$covariates %>%
    dplyr::distinct(.data$covariateId) %>%
    dplyr::count() %>%
    dplyr::pull()

  ParallelLogger::logInfo(paste0(result, " covariates in train data"))

  if ("Test" %in% names(data)) {
    ParallelLogger::logInfo("Test Set:")
    result <- data$Test$label %>%
      dplyr::collect()

    ParallelLogger::logInfo(paste0(nrow(result), " patients with ",
      sum(result$outcomeCount > 0), " outcomes"))
  }

  return(invisible(TRUE))
}


randomSplitter <- function(population, splitSettings) {
  test <- splitSettings$test
  train <- splitSettings$train
  nfold <- splitSettings$nfold
  seed <- splitSettings$seed

  checkInputsSplit(test, train, nfold, seed)

  # parameter checking
  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (length(table(population$outcomeCount)) <= 1 ||
    sum(population$outcomeCount > 0) < 10) {
    stop("Outcome only occurs in fewer than 10 people or only one class")
  }

  if (floor(sum(population$outcomeCount > 0) * train / nfold) < 5) {
    stop(paste0("Insufficient (", sum(population$outcomeCount > 0), ")
      outcomes for choosen nfold value, please reduce"))
  }


  ParallelLogger::logInfo(paste0(
    "Creating a ",
    test * 100,
    "% test and ",
    train * 100,
    "% train (into ",
    nfold,
    " folds) random stratified split by class"
  ))
  outPpl <- population$rowId[population$outcomeCount == 1]
  nonPpl <- population$rowId[population$outcomeCount == 0]

  nonPpl <- nonPpl[order(stats::runif(length(nonPpl)))]
  outPpl <- outPpl[order(stats::runif(length(outPpl)))]

  # reset all to not included (index=0)
  nonPplGroup <- rep(0, length(nonPpl))

  # set test set (index=-1)
  if (test > 0) {
    testInd <- 1:floor(length(nonPpl) * test)
    nonPplGroup[testInd] <- -1
  }

  # set train set (index>0)
  trainInd <- (floor(length(nonPpl) * test) + round(length(nonPpl) * 
    (1 - train - test)) + 1):length(nonPpl)
  reps <- floor(length(trainInd) / nfold)
  leftOver <- length(trainInd) %% nfold
  if (leftOver > 0) {
    nonPplGroup[trainInd] <- c(rep(1:nfold, each = reps), 1:leftOver)
  }
  if (leftOver == 0) {
    nonPplGroup[trainInd] <- rep(1:nfold, each = reps)
  }

  # same for outcome = 1
  outPplGroup <- rep(0, length(outPpl))
  if (test > 0) {
    testInd <- 1:floor(length(outPpl) * test)
    outPplGroup[testInd] <- -1
  }
  trainInd <- (floor(length(outPpl) * test) + round(length(outPpl) * 
    (1 - train - test)) + 1):length(outPpl)
  reps <- floor(length(trainInd) / nfold)
  leftOver <- length(trainInd) %% nfold
  if (leftOver > 0) {
    outPplGroup[trainInd] <- c(rep(1:nfold, each = reps), 1:leftOver)
  }
  if (leftOver == 0) {
    outPplGroup[trainInd] <- rep(1:nfold, each = reps)
  }

  split <- data.frame(rowId = c(nonPpl, outPpl), 
    index = c(nonPplGroup, outPplGroup))
  split <- split[order(-split$rowId), ]

  foldSizesTrain <- utils::tail(table(split$index), nfold)
  ParallelLogger::logInfo(paste0("Data split into ", sum(split$index < 0),
    " test cases and ", sum(split$index >
    0), " train cases", " (", toString(foldSizesTrain), ")"))
  if (test + train < 1) {
    ParallelLogger::logInfo(paste0(sum(split$index == 0),
      " were not used for training or testing"))
  }
  # return index vector
  return(split)
}

timeSplitter <- function(population, splitSettings) {
  test <- splitSettings$test
  train <- splitSettings$train
  nfold <- splitSettings$nfold
  seed <- splitSettings$seed

  checkInputsSplit(test, train, nfold, seed)

  # parameter checking
  if (!is.null(seed)) {
    set.seed(seed)
  }

  dates <- as.Date(population$cohortStartDate, format = "%Y-%m-%d")
  # find date that test frac have greater than - set dates older than this to this date
  datesOrd <- dates[order(dates)]
  if (test > 0) {
    testDate <- datesOrd[round(length(datesOrd) * (1 - test))]
  } else {
    testDate <- datesOrd[length(datesOrd)] # if no test use all for train
  }

  outPpl <- data.frame(
    rowId = population$rowId[population$outcomeCount == 1],
    date = dates[population$outcomeCount == 1]
  )
  nonPpl <- data.frame(
    rowId = population$rowId[population$outcomeCount == 0],
    date = dates[population$outcomeCount == 0]
  )

  ParallelLogger::logInfo(paste0(
    "Creating ", test * 100, "% test and ",
    train * 100, "% train (into ", nfold, " folds) stratified split at ",
    testDate
  ))
  # shuffle the data
  nonPpl <- nonPpl[order(stats::runif(nrow(nonPpl))), ]
  outPpl <- outPpl[order(stats::runif(nrow(outPpl))), ]

  nonPplGroup <- rep(-1, nrow(nonPpl))
  nonPplGroup[nonPpl$date <= testDate] <- rep(1:nfold,
    each = ceiling(sum(nonPpl$date <= testDate) * (train / (1 - test)) / nfold)
  )[1:sum(nonPpl$date <=
    testDate)]

  # Fill NA values with 0
  nonPplGroup[is.na(nonPplGroup)] <- 0

  outPplGroup <- rep(-1, nrow(outPpl))
  outPplGroup[outPpl$date <= testDate] <- rep(1:nfold,
    each = ceiling(sum(outPpl$date <= testDate) * (train / (1 - test)) / nfold)
  )[1:sum(outPpl$date <=
    testDate)]

  # Fill NA values with 0
  outPplGroup[is.na(outPplGroup)] <- 0

  split <- data.frame(rowId = c(nonPpl$rowId, outPpl$rowId),
    index = c(nonPplGroup, outPplGroup))
  split <- split[order(split$rowId), ]

  foldSizesTrain <- utils::tail(table(split$index), nfold)
  ParallelLogger::logInfo(paste0("Data split into ", sum(split$index < 0),
    " test cases and ", sum(split$index >
    0), " train samples", " (", toString(foldSizesTrain), ")"))
  if (test + train < 1) {
    ParallelLogger::logInfo(paste0(sum(split$index == 0),
      " were not used for training or testing"))
  }
  # return index vector
  return(split)
}


subjectSplitter <- function(population, splitSettings) {
  test <- splitSettings$test
  train <- splitSettings$train
  nfold <- splitSettings$nfold
  seed <- splitSettings$seed

  checkInputsSplit(test, train, nfold, seed)

  # parameter checking
  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (length(table(population$outcomeCount)) <= 1 ||
    sum(population$outcomeCount > 0) < 10) {
    stop("Outcome only occurs in fewer than 10 people or only one class")
  }

  if (floor(sum(population$outcomeCount > 0) * train / nfold) < 5) {
    stop("Insufficient outcomes for choosen nfold value, please reduce")
  }

  ParallelLogger::logInfo(paste0(
    "Creating a ", test * 100, "% test and ", train * 100, "% train (into ", 
    nfold, " folds) stratified split by subject"
  ))

  # get subejct ids for outcome and split them up
  outPpl <- unique(population$subjectId[population$outcomeCount == 1])

  # get subject id for non outcome and split them up
  nonPpl <- setdiff(unique(population$subjectId), outPpl)

  # give random number to all and shuffle then assign to test/train/cv
  nonPpl <- nonPpl[order(stats::runif(length(nonPpl)))]
  outPpl <- outPpl[order(stats::runif(length(outPpl)))]

  # default all to not included (index=0)
  nonPplGroup <- rep(0, length(nonPpl))

  if (test > 0) {
    # set test set (index=-1)
    # use floor(x + 0.5) to get rounding to nearest integer
    # instead of to nearest even number when x is .5
    testInd <- 1:floor(length(nonPpl) * test + 0.5)
    nonPplGroup[testInd] <- -1
  }

  # set train set (index>0)
  trainInd <- floor(length(nonPpl) * test +
    length(nonPpl) * (1 - train - test) + 1.5):length(nonPpl)
  reps <- floor(length(trainInd) / nfold)
  leftOver <- length(trainInd) %% nfold
  if (leftOver > 0) {
    nonPplGroup[trainInd] <- c(rep(1:nfold, each = reps), 1:leftOver)
  }
  if (leftOver == 0) {
    nonPplGroup[trainInd] <- rep(1:nfold, each = reps)
  }

  # same for outcome = 1
  outPplGroup <- rep(0, length(outPpl))
  if (test > 0) {
    testInd <- 1:floor(length(outPpl) * test + 0.5)
    outPplGroup[testInd] <- -1
  }
  trainInd <- floor(length(outPpl) * test +
    length(outPpl) * (1 - train - test) + 1.5):length(outPpl)
  reps <- floor(length(trainInd) / nfold)
  leftOver <- length(trainInd) %% nfold
  if (leftOver > 0) {
    outPplGroup[trainInd] <- c(rep(1:nfold, each = reps), 1:leftOver)
  }
  if (leftOver == 0) {
    outPplGroup[trainInd] <- rep(1:nfold, each = reps)
  }

  split <- data.frame(subjectId = c(nonPpl, outPpl),
    index = c(nonPplGroup, outPplGroup))

  split <- merge(population[, c("rowId", "subjectId")], split, by = "subjectId")
  split <- split[, c("rowId", "index")]
  split <- split[order(-split$rowId), ]

  foldSizesTrain <- utils::tail(table(split$index), nfold)
  ParallelLogger::logInfo(paste0("Data split into ", sum(split$index < 0),
    " test cases and ", sum(split$index >
    0), " train cases", " (", toString(foldSizesTrain), ")"))
  if (test + train < 1) {
    ParallelLogger::logInfo(paste0(sum(split$index == 0),
      " were not used for training or testing"))
  }

  # return index vector
  return(split)
}


# this is not needed for each function - just the setting where is it not used? (fix in future)
checkInputsSplit <- function(test, train, nfold, seed) {
  ParallelLogger::logDebug(paste0("test: ", test))
  checkIsClass(test, c("numeric", "integer"))
  checkHigherEqual(test, 0)
  checkHigher(-1 * test, -1)

  ParallelLogger::logDebug(paste0("train: ", train))
  checkIsClass(train, c("numeric", "integer"))
  checkHigherEqual(train, 0)
  checkHigherEqual(-1 * train, -1)

  ParallelLogger::logDebug(paste0("nfold: ", nfold))
  checkIsClass(nfold, c("numeric", "integer"))
  checkHigher(nfold, 1)

  ParallelLogger::logInfo(paste0("seed: ", seed))
  checkIsClass(seed, c("numeric", "integer"))
}

existingSplitter <- function(population, splitSettings) {
  splitIds <- splitSettings$splitIds
  # check all row Ids are in population
  if (sum(!splitIds$rowId %in% population$rowId) > 0) {
    stop("Not all rowIds in splitIds are in the population")
  }
  return(splitIds)
}
