# @file dataSplitting.R
# Copyright 2018 Observational Health Data Sciences and Informatics
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
#' Split data into random subsets stratified by class
#'
#' @details
#' Returns a dataframe of rowIds and indexes with a -1 index indicating the rowId belongs to the test
#' set and a positive integer index value indicating the rowId's cross valiation fold within the train
#' set.
#'
#' @param population   An object created using createStudyPopulation().
#' @param test         A real number between 0 and 1 indicating the test set fraction of the data
#' @param train        A real number between 0 and 1 indicating the train set fraction of the data.
#'                     If not set train is equal to 1 - test
#' @param nfold        An integer >= 1 specifying the number of folds used in cross validation
#' @param seed         If set a fixed seed is used, otherwise a random split is performed
#'
#' @return
#' A dataframe containing the columns: rowId and index
#' @export
personSplitter <- function(population, test = 0.3, train = NULL, nfold = 3, seed = NULL) {

  # check logger
  if(length(ParallelLogger::getLoggers())==0){
    logger <- ParallelLogger::createLogger(name = "SIMPLE",
                                        threshold = "INFO",
                                        appenders = list(ParallelLogger::createConsoleAppender(layout = ParallelLogger::layoutTimestamp)))
    ParallelLogger::registerLogger(logger)
  }

  # parameter checking
  if (!is.null(seed))
    set.seed(seed)

  if (!class(nfold) %in% c("numeric","integer") | nfold < 1) {
    stop("nfold must be an integer 1 or greater")
  }

  if (!class(test) %in% c("numeric","integer") | test <= 0 | test >= 1) {
    stop("test must be between 0 and 1")
  }
  
  if (is.null(train)) {
    train <- 1 - test
  }
  
  if (!class(train) %in% c("numeric","integer") | train <= 0 | train > 1-test) {
    stop("train must be between 0 and 1-test")
  }

  if (length(table(population$outcomeCount)) <= 1 | sum(population$outcomeCount > 0) < 10) {
    stop("nfold must be an integer 1 or greater")
  }

  if (floor(sum(population$outcomeCount > 0) * test/nfold) == 0) {
    stop("Insufficient outcomes for choosen nfold value, please reduce")
  }
  

  ParallelLogger::logInfo(paste0("Creating a ",
                   test * 100,
                   "% test and ",
                   train * 100,
                   "% train (into ",
                   nfold,
                   " folds) stratified split by person"))
  outPpl <- population$rowId[population$outcomeCount == 1]
  nonPpl <- population$rowId[population$outcomeCount == 0]

  # give random number to all and shuffle then assign to test/train/cv if using set.seed() then use
  # permutation <- stats::runif(length(nonPpl)+length(outPpl)) and nonPpl <-
  # nonPpl[order(permutation[1:length(nonPpl)])] and outPpl <-
  # outPpl[order(permutation[(1+length(nonPpl)):length(permutation)])]
  nonPpl <- nonPpl[order(stats::runif(length(nonPpl)))]
  outPpl <- outPpl[order(stats::runif(length(outPpl)))]

  # reset all to not included (index=0)
  nonPpl.group <- rep(0, length(nonPpl))
  
  # set test set (index=-1)
  test.ind <- 1:round(length(nonPpl) * test)
  nonPpl.group[test.ind] <- -1
  
  # set train set (index>0)
  train.ind <- round(length(nonPpl) * test + length(nonPpl) * (1-train-test) + 1):length(nonPpl) 
  reps <- floor(length(train.ind)/nfold)
  leftOver <- length(train.ind)%%nfold
  if (leftOver > 0)
    nonPpl.group[train.ind] <- c(rep(1:nfold, each = reps), 1:leftOver)
  if (leftOver == 0)
    nonPpl.group[train.ind] <- rep(1:nfold, each = reps)
    
  # same for outcome = 1
  outPpl.group <- rep(0, length(outPpl))
  test.ind <- 1:round(length(outPpl) * test)
  outPpl.group[test.ind] <- -1
  train.ind <- round(length(outPpl) * test + length(outPpl) * (1-train-test) + 1):length(outPpl)
  reps <- floor(length(train.ind)/nfold)
  leftOver <- length(train.ind)%%nfold
  if (leftOver > 0)
    outPpl.group[train.ind] <- c(rep(1:nfold, each = reps), 1:leftOver)
  if (leftOver == 0)
    outPpl.group[train.ind] <- rep(1:nfold, each = reps)

  split <- data.frame(rowId = c(nonPpl, outPpl), index = c(nonPpl.group, outPpl.group))
  split <- split[order(-split$rowId), ]

  foldSizesTrain <- utils::tail(table(split$index), nfold)
  ParallelLogger::logInfo(paste0("Data split into ", sum(split$index < 0), " test cases and ", sum(split$index >
    0), " train cases", " (", toString(foldSizesTrain), ")"))
  if (test+train<1)
    ParallelLogger::logInfo(paste0(sum(split$index == 0), " were not used for training or testing"))
  
  # return index vector
  return(split)
}


# @file timeSplitter.R Copyright 2018 Observational Health Data Sciences and Informatics This file is
# part of PatientLevelPrediction Licensed under the Apache License, Version 2.0 (the 'License'); you
# may not use this file except in compliance with the License.  You may obtain a copy of the License
# at http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law or agreed to in
# writing, software distributed under the License is distributed on an 'AS IS' BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific
# language governing permissions and limitations under the License.
#' Split test/train data by time and then partitions training set into random folds stratified by
#' class
#'
#' @details
#' Returns a dataframe of rowIds and indexes with a -1 index indicating the rowId belongs to the test
#' set and a positive integer index value indicating the rowId's cross valiation fold within the train
#' set.
#'
#' @param population   An object created using createStudyPopulation().
#' @param test         A real number between 0 and 1 indicating the test set fraction of the data
#' @param train        A real number between 0 and 1 indicating the training set fraction of the data
#' @param nfold        An integer >= 1 specifying the number of folds used in cross validation
#' @param seed         If set a fixed seed is used, otherwise a random split is performed
#'
#' @return
#' A dataframe containing the columns: rowId and index
#' @export
timeSplitter <- function(population, test = 0.3, train = NULL, nfold = 3, seed = NULL) {

  if(length(ParallelLogger::getLoggers())==0){
    logger <- ParallelLogger::createLogger(name = "SIMPLE",
                                        threshold = "INFO",
                                        appenders = list(ParallelLogger::createConsoleAppender(layout = ParallelLogger::layoutTimestamp)))
    ParallelLogger::registerLogger(logger)
  }
  
  # parameter checking
  if (!is.null(seed))
    set.seed(seed)
  if (class(nfold) != "numeric" | nfold < 1) {
    stop("nfold must be an integer 1 or greater")
  }
  if (class(test) != "numeric" | test <= 0 | test >= 1) {
    stop("test must be between 0 and ")
  }
  
  if (is.null(train)) {
    train <- 1 - test
  }
  
  dates <- as.Date(population$cohortStartDate, format = "%Y-%m-%d")
  # find date that test frac have greater than - set dates older than this to this date
  dates.ord <- dates[order(dates)]
  testDate <- dates.ord[round(length(dates.ord) * (1 - test))]

  outPpl <- data.frame(rowId = population$rowId[population$outcomeCount == 1],
                       date = dates[population$outcomeCount ==
    1])
  nonPpl <- data.frame(rowId = population$rowId[population$outcomeCount == 0],
                       date = dates[population$outcomeCount ==
    0])

  ParallelLogger::logInfo(paste0("Creating ",
                   test * 100,
                   "% test and ",
                   train * 100,
                   "% train (into ",
                   nfold,
                   " folds) stratified split at ",
                   testDate))
  # shuffle the data
  nonPpl <- nonPpl[order(stats::runif(nrow(nonPpl))), ]
  outPpl <- outPpl[order(stats::runif(nrow(outPpl))), ]

  nonPpl.group <- rep(-1, nrow(nonPpl))
  nonPpl.group[nonPpl$date <= testDate] <- rep(1:nfold,
                                               each = ceiling(sum(nonPpl$date <= testDate)*(train/(1-test))/nfold))[1:sum(nonPpl$date <=
    testDate)]
  
  # Fill NA values with 0 
  nonPpl.group[is.na(nonPpl.group)] <- 0
  
  outPpl.group <- rep(-1, nrow(outPpl))
  outPpl.group[outPpl$date <= testDate] <- rep(1:nfold,
                                               each = ceiling(sum(outPpl$date <= testDate)*(train/(1-test))/nfold))[1:sum(outPpl$date <=
    testDate)]
  
  # Fill NA values with 0 
  outPpl.group[is.na(outPpl.group)] <- 0

  split <- data.frame(rowId = c(nonPpl$rowId, outPpl$rowId), index = c(nonPpl.group, outPpl.group))
  split <- split[order(split$rowId), ]

  foldSizesTrain <- utils::tail(table(split$index), nfold)
  ParallelLogger::logInfo(paste0("Data split into ", sum(split$index < 0), " test cases and ", sum(split$index >
    0), " train samples", " (", toString(foldSizesTrain), ")"))
  if (test+train<1)
    ParallelLogger::logInfo(paste0(sum(split$index == 0), " were not used for training or testing"))
  # return index vector
  return(split)
}
