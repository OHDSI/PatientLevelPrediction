# @file dataSplitting.R
# Copyright 2019 Observational Health Data Sciences and Informatics
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
personSplitter <- function(population, test=0.3, train=NULL, nfold=3, seed=NULL) {
    # Check logger
    if(length(ParallelLogger::getLoggers())==0){
        logger <- ParallelLogger::createLogger(name = "SIMPLE",
                                            threshold = "INFO",
                                            appenders = list(ParallelLogger::createConsoleAppender(layout = ParallelLogger::layoutTimestamp)))
        ParallelLogger::registerLogger(logger)
    }
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
    
    if (train + test > 1) {
        stop('train + test must be less than 1')
    }
    
    leftover <- max(c(0, 1 - train - test))
    
    
    if (length(table(population$outcomeCount)) <= 1 | sum(population$outcomeCount > 0) < 10) {
        stop("Outcome only occurs in fewer than 10 people or only one class")
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
    
    # Get the unique set of subjects
    subject_df <- population %>% select(subjectId) %>% distinct
    
    # Shuffle the subjects
    subject_df <- subject_df %>% sample_n(nrow(subject_df), replace=FALSE)
    
    # Record the number of samples in each split
    num_test <- floor(test*nrow(subject_df))
    num_leftover <- max(c(0, floor(leftover*nrow(subject_df))))
    num_train <- nrow(subject_df) - num_test - num_leftover
    
    # Get the subjects in the test set
    test_subject_df <- subject_df %>% 
                    slice(1:num_test) %>% 
                    mutate(index=-1)

    # Get the subjects that are in neither train nor test
    if (num_leftover==0) {
        # We take a dataframe with zero rows if num_leftover==0
        leftover_subject_df <- subject_df %>% slice(0)
    } else {
        leftover_subject_df <- subject_df %>% 
        anti_join(test_subject_df) %>%
        slice(1:num_leftover) %>% 
        mutate(index=0)
    }
    
    # Assign fold ids to the training set
    train_subject_df <- subject_df %>% 
        anti_join(rbind(test_subject_df, leftover_subject_df)) %>%
        mutate(index = rep(seq.int(from = 1, to = nfold, by = 1), 
                           times = floor(num_train / nfold), 
                           length.out = num_train
                          )
              )
    # Combine the sets into a dataframe with columns (subjectId, index)
    subject_df <- rbind(test_subject_df, leftover_subject_df, train_subject_df)
    
    # Map the results to the population data frame
    split <- population %>% inner_join(subject_df)
    
    # Produce a summary
    summary_df <- split %>% 
                    group_by(index) %>% 
                    summarise(subject_count=length(unique(subjectId)), row_count=n())
    
    ParallelLogger::logInfo(paste0("Data split into ", 
                                   select(filter(summary_df, index==-1), subject_count), " test subjects, " , 
                                   select(filter(summary_df, index==-1), row_count), " test rows, ", 
                                   select(filter(summary_df, index>0), subject_count), " train subjects, ", 
                                   select(filter(summary_df, index>0), row_count), " train rows "
                                  )
                           )
    
    split <- split %>% select(rowId, index) %>% arrange(rowId)
    return(split)
}


# @file timeSplitter.R Copyright 2019 Observational Health Data Sciences and Informatics This file is
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
