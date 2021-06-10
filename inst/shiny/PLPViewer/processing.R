
# this checked whether input is valid analysis location or plpResult
checkPlpInput <- function(result){
  if(class(result)=='runPlp'){
    return('plpResult')
  } else if(ifelse(class(result)=='character', dir.exists(result),F)){
    return('file')
  } else if(sum(names(result)%in%c("prediction","performanceEvaluation","inputSetting","executionSummary","model","analysisRef","covariateSummary"))==7){
    return('plpNoClass')
  } else {
    stop('Incorrect class for input result')
  }
}



getSummary  <- function(result,inputType,validation){
  if(inputType == 'plpResult' || inputType == 'plpNoClass'){
    sumTab <- getSummaryFromObject(result,validation)
  } else if( inputType == 'file') {
    sumTab <- summaryPlpAnalyses(result)
  } 
  
  #remove empty rows
  emptyInd <- is.na(sumTab[,'AUC'])
  if(sum(emptyInd)>0){
    sumTab <- sumTab[!emptyInd,]
  }
  
  #sumTab <- sumTab[,c('analysisId','devDatabase','valDatabase','cohortName','outcomeName','modelSettingName','riskWindowStart', 'riskWindowEnd', 'AUC','AUPRC', 'populationSize','outcomeCount','incidence',
  #                    'addExposureDaysToStart','addExposureDaysToEnd','plpResultLocation', 'plpResultLoad')]
  #colnames(sumTab) <- c('Analysis','Dev', 'Val', 'T', 'O','Model', 'TAR start', 'TAR end', 'AUC','AUPRC', 'T Size','O Count','O Incidence (%)', 'addExposureDaysToStart','addExposureDaysToEnd', 'plpResultLocation', 'plpResultLoad')

  sumTab <- sumTab[,c('analysisId','devDatabase','valDatabase','cohortName','outcomeName','modelSettingName','covariateSettingId','TAR', 'AUC','AUPRC', 'populationSize','outcomeCount','valPercent','incidence','timeStamp',
                      'plpResultLocation', 'plpResultLoad')]
  colnames(sumTab) <- c('Analysis','Dev', 'Val', 'T', 'O','Model','covariateSettingId', 'TAR', 'AUC','AUPRC', 'T Size','O Count','% used for Eval','O Incidence (%)','timeStamp', 'plpResultLocation', 'plpResultLoad')

  
  return(sumTab)
} 


getSummaryFromObject <- function(result,validation=NULL){
  
  timeV <- result$executionSummary$ExecutionDateTime
  
  TAR <- getTAR(result$model$populationSettings)
  eval <- as.data.frame(result$performanceEvaluation$evaluationStatistics)
  #eval <- eval[eval$Eval %in% c('test',"validation"),]
  valInd <- eval$Eval %in% c('test',"validation")
  
  if(!is.null(eval$AUC.auc_lb95ci)){
    lb <- signif(as.double(as.character(eval$AUC.auc_lb95ci)),3)
  } else{
    lb <- signif(getbounds(n1=as.double(as.character(eval$Value[eval$Metric=='populationSize' & valInd])),
                    n2 = as.double(as.character(eval$Value[eval$Metric=='outcomeCount' & valInd])), 
                    auc = as.double(as.character(eval$Value[eval$Metric=='AUC.auc' & valInd])))$lb,3)
  }
  if(!is.null(eval$AUC.auc_ub95ci)){
    ub <- signif(as.double(as.character(eval$AUC.auc_ub95ci)),3)
  } else{
    ub <- signif(getbounds(n1=as.double(as.character(eval$Value[eval$Metric=='populationSize' & valInd])),
                    n2 = as.double(as.character(eval$Value[eval$Metric=='outcomeCount' & valInd])), 
                    auc = as.double(as.character(eval$Value[eval$Metric=='AUC.auc' & valInd])))$ub,3)
  }


  allRes <- data.frame(analysisId = 1,
                       devDatabase = ifelse(is.null(result$inputSetting$dataExtrractionSettings$cdmDatabaseSchema),'Missing',result$inputSetting$dataExtrractionSettings$cdmDatabaseSchema),
                       valDatabase = ifelse(is.null(result$inputSetting$dataExtrractionSettings$cdmDatabaseSchema),'Missing',result$inputSetting$dataExtrractionSettings$cdmDatabaseSchema),
                       cohortName = 'T',
                       outcomeName = 'O',
                       modelSettingName = result$model$modelSettings$model,
                       covariateSettingId = 1,
                       TAR = TAR,
                       AUC = paste0(signif(as.double(as.character(eval$Value[eval$Metric=='AUC.auc' & valInd])),3),
                                    ' (',lb,'-',ub,')'),
                       AUPRC = signif(as.double(as.character(eval$Value[eval$Metric=='AUPRC' & valInd])),3),
                       populationSize = sum(as.double(as.character(eval$Value[eval$Metric=='populationSize']))),
                       outcomeCount = sum(as.double(as.character(eval$Value[eval$Metric=='outcomeCount']))),
                       valPercent = round(100*sum(as.double(as.character(eval$Value[eval$Metric=='populationSize' & valInd])))/sum(as.double(as.character(eval$Value[eval$Metric=='populationSize'])))),
                       incidence = signif(100*sum(as.double(as.character(eval$Value[eval$Metric=='outcomeCount'])))/sum(as.double(as.character(eval$Value[eval$Metric=='populationSize']))),3),
                       timeStamp = timeV,
                       plpResultLocation = 'NULL', 
                       plpResultLoad = 'NULL'
  )
  

  if(!is.null(validation)){
    for(i in 1:length(validation$validation)){
      timeV <- validation$validation[[i]]$executionSummary$ExecutionDateTime
      TAR <- getTAR(validation$validation[[i]]$model$populationSettings)
      eval <- as.data.frame(validation$validation[[i]]$performanceEvaluation$evaluationStatistics)
      
      
      if(!is.null(eval$AUC.auc_lb95ci)){
        lb <- signif(as.double(as.character(eval$AUC.auc_lb95ci)),3)
      } else{
        lb <- signif(getbounds(n1=as.double(as.character(eval$Value[eval$Metric=='populationSize' & valInd])),
                               n2 = as.double(as.character(eval$Value[eval$Metric=='outcomeCount' & valInd])), 
                               auc = as.double(as.character(eval$Value[eval$Metric=='AUC.auc' & valInd])))$lb,3)
      }
      if(!is.null(eval$AUC.auc_ub95ci)){
        ub <- signif(as.double(as.character(eval$AUC.auc_ub95ci)),3)
      } else{
        ub <- signif(getbounds(n1=as.double(as.character(eval$Value[eval$Metric=='populationSize' & valInd])),
                               n2 = as.double(as.character(eval$Value[eval$Metric=='outcomeCount' & valInd])), 
                               auc = as.double(as.character(eval$Value[eval$Metric=='AUC.auc' & valInd])))$ub,3)
      }
      
      tempRes <-data.frame(analysisId = 1+i,
                           devDatabase = result$inputSetting$dataExtrractionSettings$cdmDatabaseSchema,
                           valDatabase = names(validation)[i],
                           cohortName = 'T',
                           outcomeName = 'O',
                           modelSettingName = result$model$modelSettings$model,
                           covariateSettingId =1,
                           TAR = TAR,
                           AUC = paste0(signif(as.double(as.character(eval$Value[eval$Metric=='AUC.auc' & valInd])),3),
                                        ' (',lb,'-',ub,')'),
                           AUPRC = signif(as.double(as.character(eval$Value[eval$Metric=='AUPRC'])),3),
                           populationSize = as.double(as.character(eval$Value[eval$Metric=='populationSize'])),
                           outcomeCount = as.double(as.character(eval$Value[eval$Metric=='outcomeCount'])),
                           valPercent = 100,
                           incidence = signif(100*as.double(as.character(eval$Value[eval$Metric=='outcomeCount']))/as.double(as.character(eval$Value[eval$Metric=='populationSize'])),3),
                           timeStemp = timeV,
                           plpResultLocation = 'NULL', 
                           plpResultLoad = 'NULL'
      )
      allRes <- rbind(tempRes, allRes)
    }
  }
  return(allRes)
}



# old functions:

summaryPlpAnalyses <- function(analysesLocation){ 
  # loads the analyses and validations to get summaries
  #========================================
  settings <- read.csv(file.path(analysesLocation,'settings.csv'))
  settings <- settings[,!colnames(settings)%in%c('plpDataFolder','studyPopFile','plpResultFolder')]
  settings$analysisId <- gsub('Analysis_','', settings$analysisId) # fixing if Analysis_id in settings
  settings$analysisId <- paste0('Analysis_',  settings$analysisId)
  
  analysisIds <- dir(file.path(analysesLocation), recursive = F, full.names = T)
  analysisIds <- analysisIds[grep('Analysis_',analysisIds)]
  if(is.null(settings$devDatabase)){
    settings$devDatabase <- 'Missing'
  }
  if(is.null(settings$valDatabase)){
    settings$valDatabase <- settings$devDatabase
  }
  devPerformance <- do.call(rbind,lapply(file.path(analysisIds), getPerformance))
  # updated this for TAR
  devPerformance <- merge(settings[,c('analysisId','modelSettingsId','covariateSettingId', 'cohortName', 'outcomeName',
                                      'populationSettingId','modelSettingName','devDatabase','valDatabase')],
                          devPerformance, by='analysisId', all.x=T)
  
  validationLocation <- file.path(analysesLocation,'Validation')
  if(length(dir(validationLocation))>0){
    valPerformances <- c()
    valDatabases <- dir(validationLocation, recursive = F, full.names = T)
    if(length(grep('plplog.txt', valDatabases))>0){
      valDatabases <- valDatabases[-grep('plplog.txt', valDatabases)]
    }
    for( valDatabase in valDatabases){
      
      valAnalyses <-  dir(valDatabase, recursive = F, full.names = T)
      valAnalyses <-  valAnalyses[grep('Analysis_', valAnalyses)]
      valPerformance <- do.call(rbind,lapply(file.path(valAnalyses), function(x) getValidationPerformance(x)))
      valSettings <- settings[,c('analysisId','modelSettingsId','covariateSettingId', 'cohortName', 'outcomeName',
                                 'populationSettingId','modelSettingName','devDatabase')] # removed TAR bits
      valPerformance <- merge(valSettings,
                              valPerformance, by='analysisId')
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
  
  #allPerformance$AUC <- as.double(allPerformance$AUC)
  #allPerformance$AUPRC <- as.double(allPerformance$AUPRC)
  #allPerformance$outcomeCount <- as.double(allPerformance$outcomeCount)
  #allPerformance$populationSize <- as.double(allPerformance$populationSize)
  #allPerformance$incidence <- as.double(allPerformance$incidence)
  return(allPerformance)
}

getPerformance <- function(analysisLocation){
  location <- file.path(analysisLocation, 'plpResult.rds')
  if(!file.exists(location)){
    # check for PLP file instead 
    locationPlp <- file.path(analysisLocation, 'plpResult')
    if(!dir.exists(locationPlp)){
      
      analysisId <- strsplit(analysisLocation, '/')[[1]]
      return(data.frame(analysisId=analysisId[length(analysisId)], 
                        AUC=0.000, AUPRC=0, outcomeCount=0,
                        populationSize=0,valPercent = 0,incidence=0, timeStamp = as.Date('1900-01-01'),
                        plpResultLocation=location, 
                        plpResultLoad='loadPlpResult', TAR = '?'))
    } else {
      require(PatientLevelPrediction)
      res <- loadPlpResult(file.path(analysisLocation,'plpResult'))
      timeV <- res$executionSummary$ExecutionDateTime
      TAR <- getTAR(res$model$populationSettings)
      res <- as.data.frame(res$performanceEvaluation$evaluationStatistics)
      location <- file.path(analysisLocation, 'plpResult')
      plpResultLoad <- 'loadPlpResult'
      
    }
  } else{
    # read rds here
    res <- readRDS(file.path(analysisLocation,'plpResult.rds'))
    timeV <- res$executionSummary$ExecutionDateTime
    TAR <- getTAR(res$model$populationSettings)
    res <- as.data.frame(res$performanceEvaluation$evaluationStatistics)
    plpResultLoad <- 'readRDS'
  }

  #mave into values:
  res$Value <- as.double(as.character(res$Value))

  #add valPercent
  testVal <- res$Value[res$Eval%in%c('test','validation') & res$Metric == 'populationSize'] 
  res$Value[res$Eval%in%c('test','validation') & res$Metric == 'outcomeCount'] <- ifelse(length(res$Value[res$Eval=='train' & res$Metric == 'outcomeCount'])==0, 0, res$Value[res$Eval=='train' & res$Metric == 'outcomeCount'])+res$Value[res$Eval%in%c('test','validation') & res$Metric == 'outcomeCount']
  res$Value[res$Eval%in%c('test','validation') & res$Metric == 'populationSize'] <- ifelse(length(res$Value[res$Eval=='train' & res$Metric == 'populationSize'])==0, 0, res$Value[res$Eval=='train' & res$Metric == 'populationSize'])+res$Value[res$Eval%in%c('test','validation') & res$Metric == 'populationSize']
  if(sum(res$Eval=='test' & res$Metric == 'populationSize')>0){
    valPercent <- round(100*testVal/res$Value[res$Eval%in%c('test','validation') & res$Metric == 'populationSize'])
  } else{
    valPercent <- 100 
  }
  res <- tryCatch(reshape2::dcast(res[res$Eval%in%c('test','validation'),], analysisId ~ Metric, value.var='Value'),
                  error = function(cont) return(NULL))
  if(is.null(res)){
    return(NULL) }

  res <- res[,!colnames(res)%in%c("BrierScore","BrierScaled")]
  res$incidence <- as.double(res$outcomeCount)/as.double(res$populationSize)*100
  res$valPercent <- valPercent
  #res[, !colnames(res)%in%c('analysisId','outcomeCount','populationSize')] <- 
  #  format(as.double(res[, !colnames(res)%in%c('analysisId','outcomeCount','populationSize')]), digits = 2, scientific = F) 
  res$TAR <- TAR
  #if(sum(colnames(res)=='AUC.auc_ub95ci')>0){
  #  res$AUC <- res$AUC.auc
  #}
  
  if(sum(colnames(res)=='AUC.auc')==0){
    res$AUC.auc <- res$AUC 
  }
  if(sum(is.null(res$AUC.auc_ub95ci))>0 | sum(is.na(as.double(as.character(res$AUC.auc_ub95ci))))>0){
    nullInd <- is.null(res$AUC.auc_ub95ci) | is.na(as.double(as.character(res$AUC.auc_ub95ci)))
    aucs <- as.double(as.character(res$AUC.auc[nullInd]))
    n1s <- as.double(as.character(res$populationSize[nullInd]))*valPercent/100
    n2s <- as.double(as.character(res$outcomeCount[nullInd]))*valPercent/100
    res$AUC.auc_ub95ci[nullInd] <- unlist(lapply(1:length(aucs), function(i){getbounds(n1=n1s[i],
                                                                                                  n2 = n2s[i], 
                                                                                                  auc = aucs[i])$ub}))
    res$AUC.auc_lb95ci[nullInd] <- unlist(lapply(1:length(aucs), function(i){getbounds(n1=n1s[i],
                                                                                                  n2 = n2s[i], 
                                                                                                  auc = aucs[i])$lb}))
    
  }
  res$AUC <- paste0(signif(as.double(as.character(res$AUC.auc)),3), ' (', signif(as.double(as.character(res$AUC.auc_lb95ci)),3),'-', signif(as.double(as.character(res$AUC.auc_ub95ci)),3),')')
  res$AUPRC <- signif(as.double(as.character(res$AUPRC,3)))
  res$incidence <- signif(as.double(as.character(res$incidence,3)))
  res$timeStamp <- as.Date(ifelse(is.null(timeV), '1900-01-01', as.character(timeV)))
  
  res$plpResultLocation <- location
  res$plpResultLoad <- plpResultLoad
  return(res[,c('analysisId', 'AUC', 'AUPRC', 'outcomeCount','populationSize','valPercent','incidence','timeStamp','plpResultLocation', 'plpResultLoad', 'TAR')])
}

getValidationPerformance <- function(validationLocation){
  val <- readRDS(file.path(validationLocation,'validationResult.rds'))
  if("performanceEvaluation"%in%names(val)){
    timeV <- val$executionSummary$ExecutionDateTime
    valPerformance <- reshape2::dcast(as.data.frame(val$performanceEvaluation$evaluationStatistics), 
                                      analysisId ~ Metric, value.var='Value')
    
    if(!is.null(val$model$populationSettings)){
      TAR <- getTAR(val$model$populationSettings)
    } else{
      TAR <- getTAR(val$inputSetting$populationSettings)
    }
    
  } else {
    timeV <- val[[1]]$executionSummary$ExecutionDateTime
    valPerformance <- reshape2::dcast(as.data.frame(val[[1]]$performanceEvaluation$evaluationStatistics), 
                                      analysisId ~ Metric, value.var='Value') 
    
    if(!is.null(val[[1]]$model$populationSettings)){
      TAR <- getTAR(val[[1]]$model$populationSettings)
    } else{
      TAR <- getTAR(val[[1]]$inputSetting$populationSettings)
    }
    
  }
  #mave into values:
  valPerformance$outcomeCount <- as.double(as.character(valPerformance$outcomeCount))
  valPerformance$populationSize <- as.double(as.character(valPerformance$populationSize))

  valPerformance$incidence <- signif(valPerformance$outcomeCount/valPerformance$populationSize*100,3)
  valPerformance$timeStamp <- as.Date(ifelse(is.null(timeV), '1900-01-01', as.character(timeV)))
  valPerformance$valPercent <- 100
  #valPerformance[, !colnames(valPerformance)%in%c('analysisId','outcomeCount','populationSize')] <- 
  #  format(as.double(valPerformance[, !colnames(valPerformance)%in%c('analysisId','outcomeCount','populationSize')]), digits = 2, scientific = F) 
  
  if(sum(colnames(valPerformance)=='AUC.auc')==0){
    valPerformance$AUC.auc <- valPerformance$AUC
  }
  # convert to double
  valPerformance$AUC.auc <- as.double(as.character(valPerformance$AUC.auc ))
  valPerformance$AUC.auc_ub95ci <- as.double(as.character(valPerformance$AUC.auc_ub95ci))
  valPerformance$AUC.auc_lb95ci <- as.double(as.character(valPerformance$AUC.auc_lb95ci))
  if(sum(is.null(valPerformance$AUC.auc_ub95ci))>0 | sum(is.na(as.double(as.character(valPerformance$AUC.auc_ub95ci))))>0){
    nullInd <- is.null(valPerformance$AUC.auc_ub95ci) | is.na(as.double(as.character(valPerformance$AUC.auc_ub95ci)))
    aucs <- as.double(as.character(valPerformance$AUC.auc[nullInd]))
    n1s <- as.double(as.character(valPerformance$populationSize[nullInd]))
    n2s <- as.double(as.character(valPerformance$outcomeCount[nullInd]))
    valPerformance$AUC.auc_ub95ci[nullInd] <- unlist(lapply(1:length(aucs), function(i){getbounds(n1=n1s[i],
                                                                                                  n2 = n2s[i], 
                                                                                                  auc = aucs[i])$ub}))
    valPerformance$AUC.auc_lb95ci[nullInd] <- unlist(lapply(1:length(aucs), function(i){getbounds(n1=n1s[i],
                                                                                                  n2 = n2s[i], 
                                                                                                  auc = aucs[i])$lb}))
    
  }
  valPerformance$AUC <- paste0(signif(valPerformance$AUC.auc,3), ' (', signif(valPerformance$AUC.auc_lb95ci,3),'-', signif(valPerformance$AUC.auc_ub95ci,3),')')
  valPerformance$AUPRC <- signif(as.double(as.character(valPerformance$AUPRC,3)))
  
  valPerformance$analysisId <- strsplit(validationLocation, '/')[[1]][[length(strsplit(validationLocation, '/')[[1]])]]
  valPerformance$valDatabase <- strsplit(validationLocation, '/')[[1]][[length(strsplit(validationLocation, '/')[[1]])-1]]
  valPerformance <- valPerformance[,c('analysisId','valDatabase', 'AUC', 'AUPRC', 'outcomeCount','populationSize','valPercent','incidence','timeStamp')]
  valPerformance$plpResultLocation <- file.path(validationLocation,'validationResult.rds')
  valPerformance$plpResultLoad <- 'readRDS'
  valPerformance$TAR <- TAR
  #valPerformance$rocplot <- file.path(validationLocation,'plots','sparseROC.pdf')
  #valPerformance$calplot <- file.path(validationLocation,'plots','sparseCalibrationConventional.pdf')
  return(valPerformance)
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


# estimate 95CI AUC if missing
getbounds <- function(n1,n2, auc){
  q0 <- auc*(1-auc)
  q1 <- auc/(2-auc)-auc^2
  q2 <- 2*auc^2/(1+auc)-auc^2
  
  se = sqrt((q0+(n1-1)*q1+(n2-1)*q2)/(n1*n2))
  
  list(lb=auc-1.96*se, ub=auc+1.96*se)
}
