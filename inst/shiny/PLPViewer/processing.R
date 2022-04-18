settingsNames <- c('analysisId','modelSettings','covariateSetting', 'targetName', 'outcomeName',
  'populationSetting','modelSettingName')

getSummary  <- function(result,inputType,validation){
  if(inputType == 'plpResult' || inputType == 'plpNoClass'){
    
    sumTab <- getSummaryFromObject(result)
    
    if(!is.null(validation)){ # what about a list of validations?
      sumTab <- rbind(sumTab, getSummaryFromObject(validation))
    }
    
  } else if( inputType == 'file') {
    sumTab <- summaryPlpAnalyses(result)
  } 
  
  #remove empty rows
  emptyInd <- is.na(sumTab[,'AUROC'])
  if(sum(emptyInd)>0){
    sumTab <- sumTab[!emptyInd,]
  }
  
  columnsOfInt <- c('analysisId',
    'devDatabase', 'valDatabase',
    'targetName', 'outcomeName',
    'modelSettingName','covariateSetting', 
    'TAR', 'AUROC','AUPRC',
    'populationSize', 'outcomeCount',
    'valPercent', 'incidence',
    'timeStamp', 'plpResultLocation', 'plpResultLoad'
    )
  
  # add missing columns
  if(sum(!columnsOfInt %in% colnames(sumTab))>0){
    missInd <- columnsOfInt[!columnsOfInt %in% colnames(sumTab)]
    for(i in 1:length(missInd)){
      sumTab[,missInd[i]] <- 'NA'
    }
  }
  
  sumTab <- sumTab[,columnsOfInt]
  
  colnames(sumTab) <- c(
    'Analysis','Dev', 'Val', 'T', 'O','Model','covariateSetting', 
    'TAR', 'AUROC','AUPRC', 'T Size','O Count','% used for Eval','O Incidence (%)',
    'timeStamp', 'plpResultLocation', 'plpResultLoad'
  )
  
  return(sumTab)
} 


getSummaryFromObject <- function(result, analysisId = NULL){
  
  timeV <- ifelse(is.null(result$executionSummary$ExecutionDateTime), '2000-01-01', result$executionSummary$ExecutionDateTime)

  TAR <- getTAR(result$model$settings$populationSettings)
  eval <- as.data.frame(result$performanceEvaluation$evaluationStatistics)
  
  for(i in 1:ncol(eval)){
    eval[,i] <- unlist(eval[,i])
  }
  
  if(is.null(eval$evaluation)){
    eval$evaluation <- 'Test'
  }
  if(length(unique(eval$evaluation)) == 1){
    eval$evaluation <- 'Test'
  }
  
  eval <- tidyr::pivot_wider( 
    data = eval %>% 
      dplyr::mutate(variable = paste(.data$evaluation, .data$metric, sep = '_')) %>% 
      dplyr::select(-.data$evaluation, -.data$metric), 
    names_from = 'variable', 
    values_from = 'value'
      ) 
  
  #eval <- reshape2::dcast(
  #  data = eval, 
  #  formula = . ~ evaluation + metric, 
  #  value.var = 'value' 
  #  )
  
  AUC <- paste0(
    signif(as.double(eval$Test_AUROC),3),
    ' (',
    signif(as.double(eval$`Test_95% lower AUROC`),3),
    '-',
    signif(as.double(eval$`Test_95% upper AUROC`),3),
    ')'
    )
  
  AUPRC <- signif(as.double(eval$Test_AUPRC),3)
  
  populationSize <- ifelse(is.null(eval$Train_populationSize), 0, as.double(eval$Train_populationSize)) + as.double(eval$Test_populationSize)
  outcomeCount <- ifelse(is.null(eval$Train_outcomeCount), 0, as.double(eval$Train_outcomeCount)) + as.double(eval$Test_outcomeCount)
  valPercent <- round(as.double(eval$Test_populationSize)/populationSize*100)
  incidence <- signif(100*outcomeCount/populationSize ,3)
  
  if(!is.null(result$model$trainDetails)){
    devDatabase <- ifelse(is.null(result$model$trainDetails$cdmDatabaseSchema),'Missing',result$model$trainDetails$cdmDatabaseSchema)
    valDatabase <- devDatabase
  } else{
    devDatabase <- ifelse(is.null(result$model$validationDetails$developmentDatabase),'Missing',result$model$validationDetails$developmentDatabase)
    valDatabase <- ifelse(is.null(result$model$validationDetails$cdmDatabaseSchema),'Missing',result$model$validationDetails$cdmDatabaseSchema)
}
  allRes <- data.frame(analysisId = ifelse(is.null(analysisId), ifelse(is.null(result$analysisRef$analysisId), 'None', result$analysisRef$analysisId), analysisId),
                       devDatabase = devDatabase,
                       valDatabase = valDatabase,
                       cohortName = 'T', # needed?
                       outcomeName = 'O', # needed?
                       modelSettingName = result$model$settings$modelSettings$model,
                       #covariateSetting = 1,
                       TAR = TAR,
                       AUROC = AUC,
                       AUPRC = AUPRC,
                       populationSize = populationSize,
                       outcomeCount = outcomeCount,
                       valPercent = valPercent,
                       incidence = incidence,
                       timeStamp = timeV,
                       plpResultLocation = 'NULL', 
                       plpResultLoad = 'NULL'
  )
  
  return(allRes)
}



summaryPlpAnalyses <- function(analysesLocation){ 
  # loads the analyses and validations to get summaries
  #========================================
  settings <- utils::read.csv(file.path(analysesLocation,'settings.csv'))
  settings <- settings[,!colnames(settings)%in%c('plpDataFolder','studyPopFile','plpResultFolder')]
  settings$analysisId <- gsub('Analysis_','', settings$analysisId) # fixing if Analysis_id in settings
  settings$analysisId <- paste0('Analysis_',  settings$analysisId)
  
  analysisIds <- dir(file.path(analysesLocation), recursive = F, full.names = T)
  analysisIds <- analysisIds[grep('Analysis_',analysisIds)]
  
  devPerformance <- do.call(rbind,lapply(file.path(analysisIds), getPerformance))
  
  # updated this
  devPerformance <- merge(settings[,settingsNames[settingsNames %in% colnames(settings)]],
                          devPerformance[, !colnames(devPerformance) %in% c('cohortName','outcomeName')[c('cohortName','outcomeName') %in% colnames(settings)]], by='analysisId', all.x=T)
  
  validationLocation <- file.path(analysesLocation,'Validation')
  if(length(dir(validationLocation))>0){
    valPerformances <- c()
    valDatabases <- dir(validationLocation, recursive = F, full.names = T)
    if(length(grep('plplog.txt', valDatabases))>0){
      valDatabases <- valDatabases[-grep('plplog.txt', valDatabases)]
    }
    for(valDatabase in valDatabases){
      
      valAnalyses <-  dir(valDatabase, recursive = F, full.names = T)
      valAnalyses <-  valAnalyses[grep('Analysis_', valAnalyses)]
      valPerformance <- do.call(rbind,lapply(file.path(valAnalyses), function(x) getPerformance(x)))
      
      valSettings <- settings[,settingsNames[settingsNames %in% colnames(settings)]] # removed TAR bits
      valPerformance <- merge(
        valSettings,
        valPerformance[, !colnames(valPerformance) %in% c('cohortName','outcomeName')[c('cohortName','outcomeName') %in% colnames(settings)]],
          by='analysisId'
      )
      valPerformance <- valPerformance[,colnames(devPerformance)] # make sure same order
      valPerformances <- rbind(valPerformances, valPerformance)
    }
    
    if(ncol(valPerformances)==ncol(devPerformance)){
      allPerformance <- rbind(devPerformance,valPerformances)
    } else{
      stop('Issue with dev and val performance data.frames')
    }
  } else {
    allPerformance <- devPerformance
  }
  
  return(allPerformance)
}

getPerformance <- function(analysisLocation){
  
  getType <- function(analysisLocation){
    
    if(file.exists(file.path(analysisLocation, 'plpResult.rds'))){
      return('rds')
    }
    
    if(file.exists(file.path(analysisLocation, 'validationResult.rds'))){
      return('rds')
    }
    
    if(dir.exists(file.path(analysisLocation, 'plpResult'))){
      return('runPlp')
    }
    
    if(dir.exists(file.path(analysisLocation, 'validationResult'))){
      return('runPlp')
    }
    
    
    if(dir.exists(file.path(analysisLocation, 'performanceEvaluation'))){
      return('csv')
    }
    
    return('none')
    
  }
  
  analysisId <- strsplit(analysisLocation, '/')[[1]]
  analysisId <- analysisId[length(analysisId)]
  
  type <- getType(analysisLocation) # csv, rds, runPlp
  print(type)
  
  if(type == 'csv'){
    
    require(PatientLevelPrediction)
    res <- PatientLevelPrediction::loadPlpShareable(file.path(analysisLocation))
    result <- getSummaryFromObject(result = res, analysisId = analysisId)
    location <- file.path(analysisLocation)
    plpResultLoad <- 'loadPlpShareable'
    
  } else if(type == 'rds'){
    
    # read rds here
    res <- readRDS(file.path(analysisLocation,'plpResult.rds'))
    result <- getSummaryFromObject(result = res, analysisId = analysisId)
    location <- file.path(analysisLocation, 'plpResult.rds')
    plpResultLoad <- 'readRDS'
    
  } else if(type == 'runPlp'){
    
    location <- file.path(analysisLocation, 'plpResult')
    if(!dir.exists(location)){
      location <- file.path(analysisLocation, 'validationResult')
    }
    
    require(PatientLevelPrediction)
    res <- loadPlpResult(location)
    result <- getSummaryFromObject(result = res, analysisId = analysisId)
    plpResultLoad <- 'loadPlpResult'
    
  } else{
    # return empty result
    analysisId <- strsplit(analysisLocation, '/')[[1]]
    result <- data.frame(
      AnalysisId = analysisId, 
      devDatabase = 'missing',
      valDatabase = 'missing',
      targetName = 'T', # NEEDED?
      outcomeName = 'O', # NEEDED?
      modelSettingName = 'none',
      #covariateSetting = 1,
      TAR = '?',
      AUROC = 0.000,
      AUPRC = 0,
      populationSize = 0,
      outcomeCount = 0,
      valPercent = 0,
      incidence = 0,
      timeStamp = as.Date('1900-01-01'),
      plpResultLocation = '', 
      plpResultLoad = 'loadPlpResult'
      )
    location <- ''
    plpResultLoad <- 'loadPlpResult'

  }
  
  result$plpResultLocation <- location
  result$plpResultLoad <- plpResultLoad
  
  #remove settings
  result[,!colnames(result) %in% settingsNames]
  
  return(result)
}


getTAR <- function(x){
  starttar <- unique(x$startAnchor)
  if(is.null(starttar)){
    starttar <- ifelse(unique(x$addExposureDaysToStart), 'cohort end','cohort start')
  }
  endtar <- unique(x$endAnchor)
  if(is.null(endtar)){
    endtar <- ifelse(unique(x$addExposureDaysToEnd), 'cohort end','cohort start')
  }
  TAR <- paste0('(',starttar,' + ',unique(x$riskWindowStart),') - (', endtar,' + ',unique(x$riskWindowEnd),')')
  return(TAR)
  #return('cohort start + 1 - cohort start + 365')
}

