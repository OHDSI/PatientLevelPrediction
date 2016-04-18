#' @export
createAnalysisSummary <- function (dirPath, save=F){
  analysisLocation <- dirPath
  modelInfo <- read.csv(file.path(analysisLocation, 'modelInfo.txt'), header=T)
  header <- strsplit(colnames(modelInfo), '\\.')[[1]]
  modelInfo <- t(unlist(apply(modelInfo, 1,
                              function(x) {temp <- strsplit(x, ' ' )[[1]];
                              c(paste(temp[1],temp[2]), temp[-(1:2)])
                              }   )))
  if(length(header)==ncol(modelInfo)-1){
    colnames(modelInfo) <- c(header, 'timeUnit')
  } else {
    colnames(modelInfo) <- header
  }

  performanceInfo <- read.csv(file.path(analysisLocation, 'performanceInfo.txt'), header=T)
  header <- read.table(file.path(analysisLocation, 'performanceInfo.txt'), nrows=1,
                       colClasses = "character")
  
  performanceInfo <- t(unlist(apply(performanceInfo, 1,
                                    function(x) {temp <- strsplit(as.character(x), ' ' )[[1]];
                                    c(paste(temp[1],temp[2]), round(as.double(temp[-(1:2)]), digits=3)   )
                                    }   )))
  colnames(performanceInfo) <- as.character(header)
  #performanceInfo[,-1] <- round(performanceInfo[,-1], digits=3)
  
  allInfo <- merge(modelInfo, performanceInfo)
  
  cohortInfo <- read.table(file.path(analysisLocation, 'analysis.txt'), header=T)
  
  y.match <- 'cohortId'
  x.match <- 'COHORT_DEFINITION_ID'
  if('outcomeId'%in%colnames(allInfo) & 'OUTCOME_ID'%in%colnames(cohortInfo)){
    y.match <- c('cohortId', 'outcomeId')
    x.match <- c('COHORT_DEFINITION_ID', 'OUTCOME_ID')
  }
  
  
  mainResult <- merge(cohortInfo, allInfo[,!colnames(allInfo)%in%c('modelLoc','populationLoc','modelTime')], 
                      by.y=y.match, by.x=x.match)
  
  if(save==T)
    write.csv(mainResult, file.path(analysisLocation,'largeScaleResults.csv'), 
              row.names = F, col.names = T)
  
  return(mainResult)
}

#' @export
getId <- function(results, cohortId, outcomeId=NULL){
  if('outcomeId'%in%colnames(results) | 'OUTCOME_ID'%in%colnames(results)){
   cid_ind <- results[,colnames(results)%in%c('COHORT_DEFINITION_ID','COHORT_ID')]==cohortId
   oid_ind <- results[,colnames(results)%in%c('outcomeId','OUTCOME_ID')]==outcomeId
   id <- gsub(' ','',gsub('-','',gsub(':','', results$datetime[cid_ind & oid_ind])))
  } else {
  id <- gsub(' ','',gsub('-','',gsub(':','', results$datetime[results[,colnames(results)%in%c('COHORT_DEFINITION_ID','COHORT_ID')]==cohortId])))
  }
  return(id)
}