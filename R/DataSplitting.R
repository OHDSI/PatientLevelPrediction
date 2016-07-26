# @file dataSplitting.R
# Copyright 2016 Observational Health Data Sciences and Informatics
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
#' Returns a dataframe of rowIds and indexes with a -1 index indicating the rowId belongs to the test set and 
#' a positive integer index value indicating the rowId's cross valiation fold within the train set.
#' 
#' @param population   An object created using createStudyPopulation().
#' @param test         A real number between 0 and 1 indicating the test set fraction of the data
#' @param nfold        An integer >= 1 specifying the number of folds used in cross validation
#' @param seed         If set a fixed seed is used, otherwise a random split is performed
#'
#' @return
#' A dataframe containing the columns: rowId and index
#' @export
personSplitter <- function(population, test=0.3, nfold=3, seed=NULL){

  # parameter checking
  if (!is.null(seed))
    set.seed(seed)
  
  if(class(nfold)!="numeric" | nfold<1){
    flog.error('nfold must be an integer 1 or greater')
    stop()
  }
  
  if(class(test)!="numeric" | test<=0 | test>=1){
    flog.error('test must be between 0 and ')
    stop()
  }
  
  if(length(table(population$outcomeCount))<=1 | sum(population$outcomeCount>0)<10){
    flog.error('nfold must be an integer 1 or greater')
    stop()
  }
    
  if(floor(sum(population$outcomeCount>0)*test/nfold)==0){
    flog.error('Insufficient outcomes for choosen nfold value, please reduce')
    stop()
  }
  
  flog.info(paste0('Creating a ',test*100,'% test and ',(1-test)*100,'% train (into ',nfold,' folds) stratified split by person'))
  outPpl <- population$rowId[population$outcomeCount==1]
  nonPpl <- population$rowId[population$outcomeCount==0]
  
  # give random number to all and shuffle then assign to test/train/cv
  ## if using set.seed() then use permutation <- runif(length(nonPpl)+length(outPpl))
  ## and nonPpl <- nonPpl[order(permutation[1:length(nonPpl)])]
  ## and outPpl <- outPpl[order(permutation[(1+length(nonPpl)):length(permutation)])]
  nonPpl <- nonPpl[order(runif(length(nonPpl)))]
  outPpl <- outPpl[order(runif(length(outPpl)))]
  
  nonPpl.group <- rep(-1, length(nonPpl))
  train.ind <- round(length(nonPpl)*test+1):length(nonPpl)
  reps <- floor(length(train.ind)/nfold)
  leftOver <- length(train.ind)%%nfold
  if(leftOver>0)
    nonPpl.group[train.ind] <- c(rep(1:nfold,each=reps), 1:leftOver)
  if(leftOver==0)
    nonPpl.group[train.ind] <- rep(1:nfold,each=reps)

  outPpl.group <- rep(-1, length(outPpl))
  train.ind <- round(length(outPpl)*test+1):length(outPpl)
  reps <- floor(length(train.ind)/nfold)
  leftOver <- length(train.ind)%%nfold
  
  if(leftOver>0)
    outPpl.group[train.ind ] <- c(rep(1:nfold,each=reps), 1:leftOver)
  if(leftOver==0)
    outPpl.group[train.ind ] <- rep(1:nfold,each=reps)
  
  
  split <- data.frame(rowId=c(nonPpl,outPpl), index=c(nonPpl.group,outPpl.group))
  split <- split[order(-split$rowId),]

  foldSizesTrain <-tail(table(split$index),nfold)
  flog.info(paste0('Data split into ',sum(split$index<0),' test cases and ',sum(split$index>0),' train cases',' (',toString(foldSizesTrain),')'))

  # return index vector
  return(split)
}


# @file timeSplitter.R
#
# Copyright 2016 Observational Health Data Sciences and Informatics
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
#' Split test/train data by time and then partitions training set into random folds stratified by class
#'
#' @details
#' Returns a dataframe of rowIds and indexes with a -1 index indicating the rowId belongs to the test set and 
#' a positive integer index value indicating the rowId's cross valiation fold within the train set.
#' 
#' @param population   An object created using createStudyPopulation().
#' @param test         A real number between 0 and 1 indicating the test set fraction of the data
#' @param nfold        An integer >= 1 specifying the number of folds used in cross validation
#' @param seed         If set a fixed seed is used, otherwise a random split is performed
#'
#' @return
#' A dataframe containing the columns: rowId and index
#' @export
timeSplitter <- function(population, test=0.3, nfold=3, seed=NULL){

  # parameter checking
  if (!is.null(seed))
    set.seed(seed)
  if(class(nfold)!="numeric" | nfold<1){
    flog.error('nfold must be an integer 1 or greater')
    stop()
  }
  if(class(test)!="numeric" | test<=0 | test>=1){
    flog.error('test must be between 0 and ')
    stop()
  }
  dates <-  as.Date(population$cohortStartDate, format = "%Y-%m-%d")
  # find date that test frac have greater than - set dates older than this to this date
  dates.ord <- dates[order(dates)]
  testDate <- dates.ord[round(length(dates.ord)*(1-test))]
  
  outPpl <- data.frame(rowId=population$rowId[population$outcomeCount==1],
                       date = dates[population$outcomeCount==1])
  nonPpl <- data.frame(rowId=population$rowId[population$outcomeCount==0], 
                       date = dates[population$outcomeCount==0])
  
  flog.info(paste0('Creating ',test*100,'% test and ',(1-test)*100,'% train (into ',nfold,' folds) stratified split at ', testDate))
  #shuffle the data
  nonPpl <- nonPpl[order(runif(nrow(nonPpl))),]
  outPpl <- outPpl[order(runif(nrow(outPpl))),]
  
  nonPpl.group <- rep(-1, nrow(nonPpl))
  nonPpl.group[nonPpl$date<=testDate] <- rep(1:nfold,each=ceiling(sum(nonPpl$date<=testDate)/nfold))[1:sum(nonPpl$date<=testDate)]
  
  outPpl.group <- rep(-1, nrow(outPpl))
  outPpl.group[outPpl$date<=testDate] <- rep(1:nfold,each=ceiling(sum(outPpl$date<=testDate)/nfold))[1:sum(outPpl$date<=testDate)]
  
  split <- data.frame(rowId=c(nonPpl$rowId,outPpl$rowId), index=c(nonPpl.group,outPpl.group))
  split <- split[order(split$rowId),]
  
  foldSizesTrain <-tail(table(split$index),nfold)
  flog.info(paste0('Data split into ',sum(split$index<0),' test cases and ',sum(split$index>0),' train samples',' (',toString(foldSizesTrain),')'))
  
  # return index vector
  return(split)
}
