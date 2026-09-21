# @file DataSplitting.R
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
#' @examples
#' createDefaultSplitSetting(testFraction=0.25, trainFraction=0.75, nfold=3,
#'                           splitSeed=42)
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
#' 
#' @param splitIds (data.frame) A data frame with rowId and index columns of 
#' type integer/numeric. Index is -1 for test set, positive integer for train 
#' set folds
#' @return An object of class \code{splitSettings}
#' @examples
#' # rowId 1 is in fold 1, rowId 2 is in fold 2, rowId 3 is in the test set
#' # rowId 4 is in fold 1, rowId 5 is in fold 2
#' createExistingSplitSettings(splitIds = data.frame(rowId = c(1, 2, 3, 4, 5),
#'                                                   index = c(1, 2, -1, 1, 2)))
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

#' Create outcome-limited split settings
#'
#' @description
#' Create split settings for large data sets where model training should use at
#' most a target number of outcome-positive rows. When the population has no more
#' outcome rows than the cap, ordinary 25 percent test and 75 percent train
#' splitting is used.
#'
#' John et al. found that, for LASSO logistic regression models fit on large
#' observational health care data sets, learning curves often plateau after a
#' certain number of outcome events. When a study has many more outcome events
#' than are needed to approach the full-data model performance, this split can be
#' useful to reduce computational cost while preserving a separate test set.
#'
#' In row-level stratified mode, the cap can be applied exactly. In subject-level
#' mode, whole subjects are assigned to either training or testing, so the
#' training outcome count can be below the cap. If the selected subjects cannot
#' leave outcome-positive rows for testing, splitting fails with an actionable
#' error.
#'
#' @param maxTrainingOutcomes Maximum number of outcome-positive rows to include
#' in the training set when the cap is triggered. There is no universal default:
#' the adequate number of outcomes depends on the prediction problem, database,
#' model, and acceptable performance loss. In subject-level mode this is
#' approximate because all rows for selected subjects are kept together.
#' @param splitSeed A seed to use when splitting the data for reproducibility.
#' @param nfold An integer > 1 specifying the number of folds used in cross
#' validation.
#' @param type Either `stratified` for row-level splitting or `subject` for
#' subject-level splitting.
#'
#' @return An object of class `splitSettings`.
#' @examples
#' createOutcomeLimitedSplitSettings(maxTrainingOutcomes = 1000, splitSeed = 42)
#' @references John LH, Kors JA, Reps JM, Ryan PB, Rijnbeek PR. Logistic
#' regression models for patient-level prediction based on massive observational
#' data: Do we need all data? International Journal of Medical Informatics.
#' 2022;163:104762. \doi{10.1016/j.ijmedinf.2022.104762}
#' @export
createOutcomeLimitedSplitSettings <- function(maxTrainingOutcomes,
                                              splitSeed = sample(100000, 1),
                                              nfold = 3,
                                              type = "stratified") {
  checkIsClass(maxTrainingOutcomes, c("numeric", "integer"))
  checkHigher(maxTrainingOutcomes, 0)
  checkIsWholeNumber(maxTrainingOutcomes)

  checkIsClass(splitSeed, c("numeric", "integer"))
  checkIsClass(nfold, c("numeric", "integer"))
  checkHigher(nfold, 1)
  checkIsWholeNumber(nfold)
  if (floor(maxTrainingOutcomes / nfold) < 5) {
    stop("Insufficient maxTrainingOutcomes for chosen nfold value, please reduce nfold or increase the cap")
  }

  checkIsClass(type, "character")
  checkInStringVector(type, c("stratified", "subject"))

  splitSettings <- list(
    maxTrainingOutcomes = maxTrainingOutcomes,
    seed = splitSeed,
    nfold = nfold,
    type = type
  )
  attr(splitSettings, "fun") <- "outcomeLimitedSplitter"
  class(splitSettings) <- "splitSettings"
  return(splitSettings)
}


#' Split the plpData into test/train sets using a splitting settings of class 
#' \code{splitSettings}
#'
#' @param plpData   An object of type \code{plpData} - the patient level 
#' prediction data extracted from the CDM.
#' @param population The population created using \code{createStudyPopulation} 
#' that define who will be used to develop the model
#' @param splitSettings An object of type \code{splitSettings} specifying the 
#' split - the default can be created using \code{createDefaultSplitSetting}
#'
#' @return
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
#' @examples 
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n = 1000, seed = 42)
#' population <- createStudyPopulation(plpData)
#' splitSettings <- createDefaultSplitSetting(testFraction = 0.50, 
#'                                            trainFraction = 0.50, nfold = 5)
#' data = splitData(plpData, population, splitSettings)
#' # test data should be ~500 rows (changes because of study population)
#' nrow(data$Test$labels)
#' # train data should be ~500 rows
#' nrow(data$Train$labels)
#' # should be five fold in the train data
#' length(unique(data$Train$folds$index))
#' @export
splitData <- function(plpData = plpData,
                      population = population,
                      splitSettings = createDefaultSplitSetting(splitSeed = 42)) {
  start <- Sys.time()
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
    trainData$covariateData <- limitCovariatesToPopulation(
      plpData$covariateData,
      trainId$rowId
    )

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
    trainData$covariateData <- limitCovariatesToPopulation(
      plpData$covariateData,
      trainId$rowId
    )
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

    testData$covariateData <- limitCovariatesToPopulation(
      plpData$covariateData,
      testId$rowId
    )

    result <- list(
      Train = trainData,
      Test = testData
    )
  }

  class(result) <- "splitData"
  delta <- Sys.time() - start
  ParallelLogger::logInfo("Data split in ", signif(delta, 3), " ", attr(delta, "units"))
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
  checkOutcomes(population, train, nfold)
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

outcomeLimitedSplitter <- function(population, splitSettings) {
  checkColumnNames(population, c("rowId", "outcomeCount"))

  maxTrainingOutcomes <- splitSettings$maxTrainingOutcomes
  nfold <- splitSettings$nfold
  seed <- splitSettings$seed
  type <- splitSettings$type

  if (!is.null(seed)) {
    set.seed(seed)
  }

  totalOutcomes <- sum(population$outcomeCount > 0)
  if (totalOutcomes <= maxTrainingOutcomes) {
    ParallelLogger::logWarn(
      paste0(
        "Outcome-limited split did not trigger because the population has ",
        totalOutcomes,
        " outcome rows, which is not greater than maxTrainingOutcomes = ",
        maxTrainingOutcomes,
        ". Using the default 25% test and 75% train split."
      )
    )
    fallbackSettings <- createDefaultSplitSetting(
      testFraction = 0.25,
      trainFraction = 0.75,
      splitSeed = seed,
      nfold = nfold,
      type = type
    )
    fallbackFun <- attr(fallbackSettings, "fun")
    return(do.call(eval(parse(text = fallbackFun)), list(
      population = population,
      splitSettings = fallbackSettings
    )))
  }

  if (type == "subject") {
    return(outcomeLimitedSubjectSplitter(population, splitSettings))
  }
  outcomeLimitedRowSplitter(population, splitSettings)
}

outcomeLimitedRowSplitter <- function(population, splitSettings) {
  outcomeRows <- population %>%
    dplyr::filter(.data$outcomeCount > 0) %>%
    dplyr::mutate(.randomOrder = stats::runif(dplyr::n())) %>%
    dplyr::arrange(.data$.randomOrder)
  nonOutcomeRows <- population %>%
    dplyr::filter(.data$outcomeCount == 0) %>%
    dplyr::mutate(.randomOrder = stats::runif(dplyr::n())) %>%
    dplyr::arrange(.data$.randomOrder)

  trainOutcomeIds <- outcomeRows %>%
    dplyr::slice_head(n = splitSettings$maxTrainingOutcomes) %>%
    dplyr::pull(.data$rowId)
  trainingFraction <- length(trainOutcomeIds) / nrow(outcomeRows)
  trainNonOutcomeCount <- floor(nrow(nonOutcomeRows) * trainingFraction)
  trainNonOutcomeIds <- nonOutcomeRows %>%
    dplyr::slice_head(n = trainNonOutcomeCount) %>%
    dplyr::pull(.data$rowId)

  trainSplit <- dplyr::bind_rows(
    createRowFoldSplit(trainOutcomeIds, splitSettings$nfold),
    createRowFoldSplit(trainNonOutcomeIds, splitSettings$nfold)
  )
  split <- population %>%
    dplyr::select("rowId") %>%
    dplyr::left_join(trainSplit, by = "rowId") %>%
    dplyr::mutate(index = dplyr::coalesce(.data$index, -1L)) %>%
    dplyr::arrange(dplyr::desc(.data$rowId)) %>%
    as.data.frame()

  logOutcomeLimitedSplit(
    split = split,
    splitSettings = splitSettings,
    trainingOutcomeRows = length(trainOutcomeIds)
  )
  split
}

outcomeLimitedSubjectSplitter <- function(population, splitSettings) {
  checkColumnNames(population, "subjectId")

  subjectSummary <- population %>%
    dplyr::group_by(.data$subjectId) %>%
    dplyr::summarise(outcomes = sum(.data$outcomeCount > 0), .groups = "drop")
  outcomeSubjects <- subjectSummary %>%
    dplyr::filter(.data$outcomes > 0) %>%
    dplyr::mutate(.randomOrder = stats::runif(dplyr::n())) %>%
    dplyr::arrange(.data$.randomOrder) %>%
    dplyr::select(-".randomOrder")
  nonOutcomeSubjects <- subjectSummary %>%
    dplyr::filter(.data$outcomes == 0) %>%
    dplyr::mutate(.randomOrder = stats::runif(dplyr::n())) %>%
    dplyr::arrange(.data$.randomOrder) %>%
    dplyr::pull(.data$subjectId)

  trainOutcomeSubjects <- selectOutcomeLimitedSubjects(
    outcomeSubjects = outcomeSubjects,
    maxTrainingOutcomes = splitSettings$maxTrainingOutcomes
  )
  checkOutcomeLimitedSubjectSelection(
    outcomeSubjects = outcomeSubjects,
    trainOutcomeSubjects = trainOutcomeSubjects
  )
  trainingFraction <- sum(trainOutcomeSubjects$outcomes) / sum(outcomeSubjects$outcomes)
  trainNonOutcomeCount <- floor(length(nonOutcomeSubjects) * trainingFraction)
  trainNonOutcomeSubjects <- nonOutcomeSubjects[seq_len(trainNonOutcomeCount)]

  trainSubjectSplit <- dplyr::bind_rows(
    createSubjectFoldSplit(trainOutcomeSubjects$subjectId, splitSettings$nfold),
    createSubjectFoldSplit(trainNonOutcomeSubjects, splitSettings$nfold)
  )

  split <- population %>%
    dplyr::select("rowId", "subjectId") %>%
    dplyr::left_join(trainSubjectSplit, by = "subjectId") %>%
    dplyr::mutate(index = dplyr::coalesce(.data$index, -1L)) %>%
    dplyr::select("rowId", "index") %>%
    dplyr::arrange(dplyr::desc(.data$rowId)) %>%
    as.data.frame()

  logOutcomeLimitedSplit(
    split = split,
    splitSettings = splitSettings,
    trainingOutcomeRows = sum(trainOutcomeSubjects$outcomes)
  )
  split
}

checkOutcomeLimitedSubjectSelection <- function(outcomeSubjects, trainOutcomeSubjects) {
  testOutcomeRows <- sum(outcomeSubjects$outcomes) - sum(trainOutcomeSubjects$outcomes)
  if (testOutcomeRows <= 0) {
    stop(paste0(
      "Outcome-limited subject split would leave no outcome-positive rows in the test set. ",
      "Reduce maxTrainingOutcomes, use type = 'stratified', or use a population with more outcome-positive subjects."
    ))
  }
}

selectOutcomeLimitedSubjects <- function(outcomeSubjects, maxTrainingOutcomes) {
  selected <- rep(FALSE, nrow(outcomeSubjects))
  selectedOutcomeCount <- 0L
  for (i in seq_len(nrow(outcomeSubjects))) {
    nextOutcomeCount <- outcomeSubjects$outcomes[i]
    if (selectedOutcomeCount == 0L && nextOutcomeCount > maxTrainingOutcomes) {
      selected[i] <- TRUE
      break
    }
    if (selectedOutcomeCount + nextOutcomeCount <= maxTrainingOutcomes) {
      selected[i] <- TRUE
      selectedOutcomeCount <- selectedOutcomeCount + nextOutcomeCount
    }
    if (selectedOutcomeCount == maxTrainingOutcomes) {
      break
    }
  }
  outcomeSubjects[selected, , drop = FALSE]
}

# Internal helpers for the outcome-limited splitters. Existing splitters keep
# their fold assignment logic to avoid changing legacy rounding behavior here.
createRowFoldSplit <- function(rowIds, nfold) {
  data.frame(rowId = rowIds, index = createFoldIndexes(length(rowIds), nfold))
}

createSubjectFoldSplit <- function(subjectIds, nfold) {
  data.frame(subjectId = subjectIds, index = createFoldIndexes(length(subjectIds), nfold))
}

createFoldIndexes <- function(n, nfold) {
  if (n == 0) {
    return(integer(0))
  }
  reps <- floor(n / nfold)
  leftOver <- n %% nfold
  folds <- integer(0)
  if (reps > 0) {
    folds <- rep(seq_len(nfold), each = reps)
  }
  if (leftOver > 0) {
    folds <- c(folds, seq_len(leftOver))
  }
  folds
}

logOutcomeLimitedSplit <- function(split, splitSettings, trainingOutcomeRows) {
  foldSizesTrain <- split %>%
    dplyr::filter(.data$index > 0) %>%
    dplyr::group_by(.data$index) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::right_join(data.frame(index = seq_len(splitSettings$nfold)), by = "index") %>%
    dplyr::arrange(.data$index) %>%
    dplyr::mutate(n = dplyr::coalesce(.data$n, 0L)) %>%
    dplyr::pull(.data$n)
  ParallelLogger::logInfo(paste0(
    "Data split into ",
    sum(split$index < 0),
    " test cases and ",
    sum(split$index > 0),
    " train cases with ",
    trainingOutcomeRows,
    " training outcome rows (maxTrainingOutcomes = ",
    splitSettings$maxTrainingOutcomes,
    "; folds: ",
    toString(foldSizesTrain),
    ")"
  ))
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

checkOutcomes <- function(population, train, nfold) {
  if (!is.null(options("plp.outcomes")[[1]])) {
    if (sum(population$outcomeCount > 0) < options("plp.outcomes")[[1]]) {
      stop("Outcome count is less than specified option plp.outcomes: ", 
        options("plp.outcomes")[[1]])
    }

  } else {
    # plp default minimum outcomes, less < 10 in total or less than 5 in train folds
    if (length(table(population$outcomeCount)) <= 1 ||
      sum(population$outcomeCount > 0) < 10) {
      stop("Outcome only occurs in fewer than 10 people or only one class")
    }

    if (floor(sum(population$outcomeCount > 0) * train / nfold) < 5) {
      stop(paste0("Insufficient (", sum(population$outcomeCount > 0), ")
        outcomes for choosen nfold value, please reduce"))
    } 
  }
  return(invisible(TRUE))
}
