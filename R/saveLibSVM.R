# @file saveLibSVM.R
#
# Copyright 2015 Observational Health Data Sciences and Informatics
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

#' Save the plpData in the sparse libSVM format
#' 
#' @description
#' Converts the plpData to libSVM format and saves the data in the user specified directory 
#' 
#' @details
#' Given the plpData and a directory the libSVM format data will be saved into the directory.  The file plpData.txt 
#' contains the plpData in libSVM format, file covRef.txt is the covariate reference dataframe, the file rowId.txt is a 
#' vector of the rowIds of each row in the plpData file.  The plpData.txt can be loaded into h2o, oython or spark for 
#' running efficient machine learning techniques
#' @param population                       The population created using createStudyPopulation() who will be used to develop the model
#' @param plpData                          An object of type \code{plpData} - the patient level prediction 
#'                                         data extracted from the CDM.
#' @param filePath                       The path to the directory to output the files
#' @param mappings                       An ffdf containing originalCovariateId (old) and covariateId (new) columns specifying the old to new mapping
#' @param silent                         Whether to turn off progress reporting
#' @examples    
#' # To convert plpData into libSVM and save results to C:\plpData
#' 
#' @return
#' NULL
#'
#' @export
saveLibSVM <- function(population, plpData, filePath, mapping=NULL, silent=F){
  #cl <- makeCluster(20, type = "SOCK")
  #registerDoSNOW(cl)
  start <- Sys.time()

  if(!silent)
    writeLines('Starting to convert to libSVM format...')
  covs <- limitCovariatesToPopulation(plpData$covariates, ff::as.ff(population$rowId))
  covrefs <-ff::clone(plpData$covariateRef)
  
  # add originalCovariateId to coverfs
  covrefs$originalCovariateId <- ff::clone(covrefs$covariateId)
  
  if(!silent){
    writeLines(paste0(nrow(population),' people'))
    writeLines(paste0(nrow(covrefs),' features'))
  }
  
  # first redo id numbers:
  if(!silent)
    writeLines('Starting - Converting covariateIds to actual column numbers...')
  if(!is.null(mapping)){ # use existing mapping if given
    t <- ffbase::ffmatch(covrefs$covariateId, table=mapping$originalCovariateId)
    covrefs <- covrefs[ffbase::ffwhich(t, !is.na(t)),]
    t <- ffbase::ffmatch(covs$covariateId, table=mapping$originalCovariateId)
    covs <- covs[ffbase::ffwhich(t, !is.na(t)),]
    oldIds <- ff::as.ram(mapping$originalCovariateId)
    newIds <- ff::as.ram(mapping$covariateId)
  }else{
  oldIds = ff::as.ram(covrefs$covariateId)
  newIds = 1:(length(oldIds))
  }
  for (i in bit::chunk(covrefs$covariateId)) {
    ids <- covrefs$covariateId[i]
    ids <- plyr::mapvalues(ids, oldIds, newIds, warn_missing = FALSE)
    covrefs$covariateId[i] <- ids
  }
  for (i in bit::chunk(covs$covariateId)) {
    ids <- covs$covariateId[i]
    ids <- plyr::mapvalues(ids, oldIds, newIds, warn_missing = FALSE)
    covs$covariateId[i] <- ids
  }
  if(!silent)
    writeLines('Finished - Converting covariateIds to actual column numbers')
  
  
  # then group
  if(!silent)
    writeLines('Starting to group covariateIds:covariateValue per person')
  all <- c()
  for (i in bit::chunk(covs$rowId)) {
    #ids <- plpData$cohorts$rowId[i]
    #t <- ffbase::ffmatch(covs$rowId, table=as.ff(ids))
    #tempCov <- as.ram(covs[ffbase::ffwhich(t, !is.na(t)),])
    tempCov <- ff::as.ram(covs[i,])
    
    # now create covariateId:value
    test <- plyr::ddply(tempCov, "rowId", plyr::summarize,  
                        covString=paste(covariateId,covariateValue,
                                        sep=':', collapse=' ')
                        )#, .parallel = F)
    all <- rbind(all, test)
  }
  if(!silent)
    writeLines('Finished grouping covariateIds:covariateValue per person')
  
  # now merge with outcome
  if(!silent)
    writeLines('Adding outcome...')
  
  #all <- merge(all, plpData$outcomes[,c('rowId','outcomeId')], all.x=T, by='rowId')
  #all$outcomeCount <- 1
  #all$outcomeCount[is.na(all$outcomeId)] <- 0
  all <- merge(population[,c('rowId','outcomeCount')],all, by='rowId')

  timeTol <- difftime(Sys.time(),start, units = "mins")
  if(!silent)
    writeLines(paste0('Converting to libSVM completed - took: ', timeTol, ' mins'))
  #stopCluster(cl)
  
  
  if(!silent)
    writeLines('Saving libSVM file...')
  if(!dir.exists(file.path(filePath, 'libSVM'))) dir.create(file.path(filePath, 'libSVM'), recursive = T)
  start <- Sys.time()
  write.table(all[,c('outcomeCount','covString')],
              quote=FALSE, sep= " ", eol = "\n", 
              row.names=FALSE,col.names=FALSE,
              file=file.path(filePath,'libSVM', paste0('plpData.txt')))
  write.table(ff::as.ram(covrefs),
              quote=TRUE, sep= " ", eol = "\n", 
              row.names=FALSE,col.names=TRUE,
              file=file.path(filePath,'libSVM', 'covariateRef.txt')
  )
  write.table(all[,'rowId'],
              quote=FALSE, sep= " ", eol = "\n", 
              row.names=FALSE,col.names=F,
              file=file.path(filePath,'libSVM', paste0('rowId.txt'))
  )
  timeTol <- difftime(Sys.time(),start, units = "mins")
  if(!silent)
    writeLines(paste0('Saving libSVM completed - took: ', timeTol, ' mins'))
  
  return(covrefs)
}