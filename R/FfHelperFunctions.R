# @file ffHelperFunctions.R
#
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

# Returns a logical vector indicating which elements in a have a value that is in b
in.ff <- function(a, b) {
  if (length(b) == 0)
    return(ff::as.ff(rep(FALSE, length(a)))) else return(ffbase::ffmatch(x = a,
                                                                         table = b,
                                                                         nomatch = 0L) > 0L)
}

# Return a logical value indicating whether any x is TRUE
any.ff <- function(x, ..., na.rm = FALSE, range = NULL) {
  any(..., sapply(chunk(x), function(i) {
    any(x[i], na.rm = na.rm)
  }))
}


# return prev of ffdf 
calculatePrevs <- function(plpData, population){
  #===========================
  # outcome prevs
  #===========================
  ppl <- ff::as.ff(population$rowId[population$outcomeCount==1])
  idx <- ffbase::ffmatch(x = plpData$covariates$rowId, table = ppl)
  idx <- ffbase::ffwhich(idx, !is.na(idx))
  if(length(idx)>0){
    covariates <- plpData$covariates[idx, ]
    grp_qty <- ffbase::ffdfdply(x=covariates[c("rowId","covariateId")], 
                                split=covariates$covariateId, 
                                FUN = function(data){
                                  ## This happens in RAM - containing **several** split elements so here we can use data.table which works fine for in RAM computing
                                  
                                  data <- as.data.frame(data)
                                  result <- stats::aggregate(data$covariateId, by=list(data$covariateId), FUN=length)
                                  as.data.frame(result)
                                })
    
    prev.out <- data.frame(covariateId=ff::as.ram(grp_qty$Group.1), 
                           prev.out=ff::as.ram(grp_qty$x))
    prev.out$prev.out <- prev.out$prev.out/length(ppl) 
  } else {
    prev.out <- data.frame(covariateId=0, 
                           prev.out=0)
  }
  
  #===========================
  # non-outcome prevs
  #===========================
  ppl <- ff::as.ff(population$rowId[population$outcomeCount==0])
  idx <- ffbase::ffmatch(x = plpData$covariates$rowId, table = ppl)
  idx <- ffbase::ffwhich(idx, !is.na(idx))
  if(length(idx)>0){
    covariates <- plpData$covariates[idx, ]
    grp_qty <- ffbase::ffdfdply(x=covariates[c("rowId","covariateId")], 
                                split=covariates$covariateId, 
                                FUN = function(data){
                                  ## This happens in RAM - containing **several** split elements so here we can use data.table which works fine for in RAM computing
                                  
                                  data <- as.data.frame(data)
                                  result <- stats::aggregate(data$covariateId, by=list(data$covariateId), FUN=length)
                                  as.data.frame(result)
                                })
    
    prev.noout <- data.frame(covariateId=ff::as.ram(grp_qty$Group.1), 
                             prev.noout=ff::as.ram(grp_qty$x))
    prev.noout$prev.noout <- prev.noout$prev.noout/(length(ppl) )
  } else {
    prev.noout <- data.frame(covariateId=0, 
                             prev.noout=0)
  }
  
  # now merge the predictors with prev.out and prev.noout
  prevs <- merge(prev.out,prev.noout, all=T)
  prevs[is.na(prevs)] <- 0
  prevs <- prevs[prevs$covariateId!=0,] #removing dummy that was added when no covs
  
  return(prevs)
}

#' @title clearffTempDir
#' 
#' @description Clears the temporary ff directory to free up disk space.
clearffTempDir <- function(){
  file.remove(dir(getOption("fftempdir"), full.names = TRUE))
}

#' Check if the fftempdir is writable
#'
#' @details
#' This function checks whether the fftempdir is writable.
#' If not, it will ask the use to specify a writable folder.
#' @export

checkffFolder <- function(){ 
  currentfftempdir <- getOption("fftempdir")
  while (file.access(currentfftempdir)==-1) {
    yourfftempdir <- readline(prompt="Specify a writable fftemp folder:")
    options(fftempdir = yourfftempdir)
    currentfftempdir <- getOption("fftempdir")
  }
  return(TRUE)
}
