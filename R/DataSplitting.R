# @file DataSplitting.R
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
#' @param test      A real number between 0 and 1 indicating the test set fraction of the data
#' @param nfold     An integer >= 1 specifying the number of folds used in cross validation
#' @param silent    Whether to turn off the progress reporting
#'
#' @return
#' A dataframe containing the columns: rowId and index
#' @export
personSplitter <- function(population, test=0.3, nfold=3, silent=F){
  if(length(table(population$outcomeCount))<=1 | sum(population$outcomeCount>0)<10)
    stop('Insufficient outcomes')
  if(!silent) writeLines(paste0('Creating ',test*100,'% test: ',(1-test)*100,'% train (into ',nfold,' folds) splits by random patient stratified splitting'))
  outPpl <- population$rowId[population$outcomeCount==1]
  nonPpl <- population$rowId[population$outcomeCount==0]
  
  # give random number to all and shuffle then assign to test/train/cv
  nonPpl <- nonPpl[order(runif(length(nonPpl)))]
  outPpl <- outPpl[order(runif(length(outPpl)))]
  
  nonPpl.group <- rep(-1, length(nonPpl))
  nonPpl.group[round(length(nonPpl)*0.3):length(nonPpl)] <- rep(1:nfold,
                                                                each=ceiling((length(nonPpl)-round(length(nonPpl)*0.3)+1)/nfold)
  )[1:(length(nonPpl)-round(length(nonPpl)*0.3)+1)]
  
  outPpl.group <- rep(-1, length(outPpl))
  outPpl.group[round(length(outPpl)*0.3):length(outPpl)] <- rep(1:nfold,each=ceiling((length(outPpl)-round(length(outPpl)*0.3)+1)/nfold))[1:(length(outPpl)-round(length(outPpl)*0.3)+1)]
  
  
  split <- data.frame(rowId=c(nonPpl,outPpl), index=c(nonPpl.group,outPpl.group))
  split <- split[order(-split$rowId),]
  if(!silent)
    writeLines(paste0('data split into ',sum(split$index<0),' test cases and ',sum(split$index>0),' train cases'))
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
#' @param test      A real number between 0 and 1 indicating the test set fraction of the data
#' @param nfold     An integer >= 1 specifying the number of folds used in cross validation
#' @param silent    Whether to turn off the progress reporting
#'
#' @return
#' A dataframe containing the columns: rowId and index
#' @export
timeSplitter <- function(population, test=0.3, nfold=3, silent=F){

  dates <-  as.Date(population$cohortStartDate, format = "%Y-%m-%d")
  
  outPpl <- data.frame(rowId=population$rowId[population$outcomeCount==1],
                       date = dates[population$outcomeCount==1])
  nonPpl <- data.frame(rowId=population$rowId[population$outcomeCount==0], 
                       date = dates[population$outcomeCount==0])
  
  # find date that test frac have greater than - set dates older than this to this date
  dates.ord <- dates[order(dates)]
  testDate <- dates.ord[round(length(dates.ord)*(1-test))]
  
  if(!silent) writeLines(paste0('Creating ',test*100,'% test: ',(1-test)*100,'% train (into ',nfold,' folds) splits by time split stratified sampling'))
  if(!silent) writeLines(paste0('Test/train split on date: ', testDate))
  
  # give random number to all and shuffle then assign to test/train/cv
  nonPpl <- nonPpl[order(runif(nrow(nonPpl))),]
  outPpl <- outPpl[order(runif(nrow(outPpl))),]
  
  nonPpl.group <- rep(-1, nrow(nonPpl))
  nonPpl.group[nonPpl$date<=testDate] <- rep(1:nfold,each=ceiling(sum(nonPpl$date<=testDate)/nfold))[1:sum(nonPpl$date<=testDate)]
  
  outPpl.group <- rep(-1, nrow(outPpl))
  outPpl.group[outPpl$date<=testDate] <- rep(1:nfold,each=ceiling(sum(outPpl$date<=testDate)/nfold))[1:sum(outPpl$date<=testDate)]
  
  
  split <- data.frame(rowId=c(nonPpl$rowId,outPpl$rowId), index=c(nonPpl.group,outPpl.group))
  split <- split[order(split$rowId),]
  
  if(!silent) writeLines(paste0('Split into ',sum(split$index<0),' test and ',sum(split$index>0), ' train samples'))
  
  # return index vector
  return(split)
}
