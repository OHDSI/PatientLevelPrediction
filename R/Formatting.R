# @file formatting.R
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


toSparseM <- function(plpData,population, map=NULL, silent=T){
  cov <- ff::clone(plpData$covariates)
  covref <- ff::clone(plpData$covariateRef)
  
  plpData.mapped <- MapCovariates(covariates=cov, covariateRef=covref, 
                                  population, map)

  for (i in bit::chunk(plpData.mapped$covariateRef$covariateId)) {
    ids <- plpData.mapped$covariateRef$covariateId[i[1]:i[2]]
    ids <- plyr::mapvalues(ids, as.double(plpData.mapped$map$oldIds), as.double(plpData.mapped$map$newIds), warn_missing = FALSE)
    plpData.mapped$covariateRef$covariateId[i[1]:i[2]] <- ids
    # tested and working
  }
  for (i in bit::chunk(plpData.mapped$covariates$covariateId)) {
    ids <- plpData.mapped$covariates$covariateId[i[1]:i[2]]
    ids <- plyr::mapvalues(ids, as.double(plpData.mapped$map$oldIds), as.double(plpData.mapped$map$newIds), warn_missing = FALSE)
    plpData.mapped$covariates$covariateId[i[1]:i[2]] <- ids
  }
  writeLines(paste0('Max ',ffbase::max.ff(plpData.mapped$covariates$covariateId)))
  if(!silent)
    writeLines('Finished - Converting covariateIds to actual column numbers')
  
  #convert into sparseM
  if(!silent){
    writeLines(paste0('# cols: ', nrow(plpData.mapped$covariateRef)))
    writeLines(paste0('Max rowId: ', ffbase::max.ff(plpData.mapped$covariates$rowId)))
  }
  
  # chunk then add
  
  data <- Matrix::sparseMatrix(i=1,
                               j=1,
                               x=0,
                               dims=c(ffbase::max.ff(plpData.mapped$covariates$rowId), max(plpData.mapped$map$newIds))) # edit this to max(map$newIds)
  for (ind in bit::chunk(plpData.mapped$covariates$covariateId)) {
    writeLines(paste0('start:', ind[1],'- end:',ind[2]))
    temp <- tryCatch(Matrix::sparseMatrix(i=ff::as.ram(plpData.mapped$covariates$rowId[ind]),
                                          j=ff::as.ram(plpData.mapped$covariates$covariateId[ind]),
                                          x=ff::as.ram(plpData.mapped$covariates$covariateValue[ind]),
                                          dims=c(ffbase::max.ff(plpData.mapped$covariates$rowId), max(plpData.mapped$map$newIds))),
                     warning = function(w) writeLines(paste(w)),
                     error = function(e) writeLines(paste(e))
    )
    data <- data+ temp
  }
  if(!silent)
    writeLines(paste0('Sparse matrix with dimensionality: ', dim(data)))
  
  result <- list(data=data,
                 covariateRef=plpData.mapped$covariateRef,
                 map=plpData.mapped$map)
  return(result)
  
}

#' Convert the plpData in COO format into the sparse libSVM format
#' 
#' @description
#' Converts the standard plpData to libSVM format 
#' 
#' @details
#' This function converts the covariate file from ffdf in COO format into a libsvm format and
#' returns a plpData object with the covariate item representing the directory where the 
#' libsvm file is saved. 
#' @param plpData                       An object of type \code{plpData} with covariate in coo format - the patient level prediction 
#'                                      data extracted from the CDM.
#' @param filePath                       The path to the directory to output the libsvm files
#' @param silent                         Whether to turn off progress reporting
#' @examples    
#' #TODO
#' 
#' @return
#' Returns an object of type \code{plpData}, containing information on the cohorts, their
#' outcomes, and baseline covariates. Information about multiple outcomes can be captured at once for
#' efficiency reasons. This object is a list with the following components: \describe{
#' \item{outcomes}{A data frame listing the outcomes per person, including the time to event, and
#' the outcome id. Outcomes are not yet filtered based on risk window, since this is done at
#' a later stage.} \item{cohorts}{A data frame listing the persons in each cohort, listing their
#' exposure status as well as the time to the end of the observation period and time to the end of the
#' cohort (usually the end of the exposure era).} \item{covariates}{A character object pointing to a directory
#' containing a libsvm file with the baseline covariates per person in the cohort. This is done using a sparse representation:
#' covariates with a value of 0 are omitted to save space.} \item{covariateRef}{An ffdf object describing the covariates that have been extracted.}
#' \item{metaData}{A list of objects with information on how the cohortMethodData object was
#' constructed.} }
#'
#' @export
convertToLibsvm <- function(plpData,filePath=NULL,silent=F){
  if(missing(plpData) || is.null(plpData))
    stop('No plpData input')
  if(!'ffdf'%in%class(plpData$covariates))
    stop('plpData covriate object not in ffdf - maybe you already converted the data?')
  
  if(missing(filePath) || is.null(filePath)){
    timestamp <- gsub(' ', '', gsub('-','', gsub(':','',Sys.time())))
    filePath <- file.path(getwd(), paste0('libsvm_', timestamp))
    warning(paste0('filePath not specified so saving libsvm to: ', filePath))
  }
  
  if(!silent)
    writeLines('Completed input checks')
  
  start <- Sys.time()
  
  cov <- ff::clone(plpData$covariates)
  covref <- ff::clone(plpData$covariateRef)
  
  
  if(!silent)
    writeLines('Now converting the covariate data into libsvm...')
  
  
  oldIds <- ff::as.ram(plpData$covariateRef$covariateId)
  newIds <- 1:nrow(plpData$covariateRef)
  
  for (i in bit::chunk(covref$covariateId)) {
    ids <- covref$covariateId[i]
    ids <- plyr::mapvalues(ids, oldIds, newIds, warn_missing = FALSE)
    covref$covariateId[i] <- ids
  }
  for (i in bit::chunk(cov$covariateId)) {
    ids <- cov$covariateId[i]
    ids <- plyr::mapvalues(ids, oldIds, newIds, warn_missing = FALSE)
    cov$covariateId[i] <- ids
  }
  if(!silent)
    writeLines('Finished - Converting covariateIds to actual column numbers')
  
  # sort:
  if(!silent)
    writeLines('Starting to sort data...')
  cov <- ff::ffdfsort(cov)
  if(!silent)
    writeLines('Finished - sorting data')
  
  # then group
  if(!silent)
    writeLines('Starting to group covariateIds:covariateValue per person')
  all <- c()
  test_count <- 0
  for (i in bit::chunk(cov$rowId)) {
    test_count <- test_count +1
    writeLines(paste0(test_count))
    tempCov <- ff::as.ram(cov[i,])
    
    # now create covariateId:value
    test <- plyr::ddply(tempCov, "rowId", plyr::summarize,  
                        covString=paste(covariateId,covariateValue,
                                        sep=':', collapse=' ')) 
    all <- rbind(all, test) 
  } 
  if(!silent)
    writeLines('Finished grouping covariateIds:covariateValue per person')
  
  # add a dummy column of zeros as first column (this will be replaced in future)
  libsvm <- merge(plpData$cohorts[,c('rowId','subjectId')], all, all.x=T, by='rowId')
  libsvm[is.na(libsvm)] <- ''
  libsvm$subjectId <- 0
  
  timeTol <- difftime(Sys.time(),start, units = "mins")
  if(!silent)
    writeLines(paste0('Converting to libSVM completed - took: ', timeTol, ' mins'))
  
  if(!silent)
    writeLines('Saving libSVM file...')
  if(!dir.exists(file.path(filePath))) dir.create(file.path(filePath), recursive = T)
  start <- Sys.time()
  write.table(libsvm[,c('subjectId','covString')],
              quote=FALSE, sep= " ", eol = "\n", 
              row.names=FALSE,col.names=FALSE,
              file=file.path(filePath, paste0('covariate.txt')))
  write.table(ff::as.ram(plpData$covariateRef),
              quote=TRUE, sep= " ", eol = "\n", 
              row.names=FALSE,col.names=TRUE,
              file=file.path(filePath, 'covariateRef.txt')
  )
  write.table(libsvm[,'rowId'],
              quote=FALSE, sep= " ", eol = "\n", 
              row.names=FALSE,col.names=F,
              file=file.path(filePath, paste0('rowId.txt'))
  )
  timeTol <- difftime(Sys.time(),start, units = "mins")
  if(!silent)
    writeLines(paste0('Saving libSVM completed - took: ', timeTol, ' mins'))
  
  
  results <- list(cohorts=plpData$cohorts,
                  outcomes=plpData$outcomes,
                  covariates =  filePath,
                  covariateRef=ff::clone(plpData$covariateRef),
                  metaData = plpData$metaData
  )
  
  class(results) <- 'plpData.libsvm'
  
  return(results)
  
}

MapCovariates <- function(covariates, covariateRef, population, map, silent=F){
  writeLines(paste0('Max cov:', max(ff::as.ram(covariates$covariateId))))
  
  # restrict to popualtion for speed
  if(!silent)
    writeLines('restricting to population for speed...')
  idx <- ffbase::ffmatch(x = covariates$rowId, table = ff::as.ff(population$rowId))
  idx <- ffbase::ffwhich(idx, !is.na(idx))
  covariates <- covariates[idx, ]
  
  if(!silent)
    writeLines('Now converting covariateId...')
  oldIds <- as.double(ff::as.ram(covariateRef$covariateId))
  newIds <- 1:nrow(covariateRef)
  
  if(!is.null(map)){
    writeLines('restricting to model variables...')
    writeLines(paste0('oldIds: ',length(map[,'oldIds'])))
    writeLines(paste0('newIds:', max(as.double(map[,'newIds']))))
    ind <- ffbase::ffmatch(x=covariateRef$covariateId, table=ff::as.ff(as.double(map[,'oldIds'])))
    ind <- ffbase::ffwhich(ind, !is.na(ind))
    covariateRef <- covariateRef[ind,]
    
    ind <- ffbase::ffmatch(x=covariates$covariateId, table=ff::as.ff(as.double(map[,'oldIds'])))
    ind <- ffbase::ffwhich(ind, !is.na(ind))
    covariates <- covariates[ind,]
  }
  if(is.null(map))
    map <- data.frame(oldIds=oldIds, newIds=newIds)
  
  return(list(covariates=covariates,
              covariateRef=covariateRef,
              map=map))
} 