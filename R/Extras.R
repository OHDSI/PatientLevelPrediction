#' @export
createAnalysisSummary <- function (dirPath, save=F){
  analysisLocation <- dirPath
  modelInfo <- read.table(file.path(analysisLocation, 'modelInfo.txt'), header=T)
  
  performanceInfo <- read.table(file.path(analysisLocation, 'performanceInfoTest.txt'), header=T)
  
  # train performance:
  performanceInfo2 <- read.table(file.path(analysisLocation, 'performanceInfoTrain.txt'), header=T)
  colnames(performanceInfo2) <- paste0('train_',colnames(performanceInfo2))
  performanceInfo <-merge(performanceInfo, performanceInfo2, by.x='modelId', by.y='train_modelId')
    
  allInfo <- merge(modelInfo, performanceInfo)
  
  if(file.exists(file.path(analysisLocation, 'analysis.txt'))){
    cohortInfo <- read.table(file.path(analysisLocation, 'analysis.txt'), header=T)
    
    
    y.match <- 'cohortId'
    x.match <- 'COHORT_DEFINITION_ID'
    if('outcomeId'%in%colnames(allInfo) & 'OUTCOME_ID'%in%colnames(cohortInfo)){
      y.match <- c('cohortId', 'outcomeId')
      x.match <- c('COHORT_DEFINITION_ID', 'OUTCOME_ID')
    }
    
    
    mainResult <- merge(cohortInfo, allInfo[,!colnames(allInfo)%in%c('populationLoc')], 
                        by.y=y.match, by.x=x.match)
  } else {
    mainResult <- allInfo[,!colnames(allInfo)%in%c('populationLoc')]
  }
  
  if(save==T)
    write.csv(mainResult, file.path(analysisLocation,'largeScaleResults.csv'), 
              row.names = F, col.names = T)
  
  return(mainResult)
}