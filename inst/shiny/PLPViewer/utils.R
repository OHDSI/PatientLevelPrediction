summaryPlpAnalyses <- function(analysesLocation){ 
  # loads the analyses and validations to get summaries
  #========================================
  settings <- read.csv(file.path(analysesLocation,'settings.csv'))
  settings <- settings[,!colnames(settings)%in%c('plpDataFolder','studyPopFile','plpResultFolder')]
  settings$analysisId <- paste0('Analysis_',  settings$analysisId)
  
  analysisIds <- dir(file.path(analysesLocation), recursive = F, full.names = T)
  analysisIds <- analysisIds[grep('Analysis_',analysisIds)]
  if(is.null(settings$devDatabase)){
    settings$devDatabase <- 'Missing'
  }
  settings$valDatabase <- settings$devDatabase
  devPerformance <- do.call(rbind,lapply(file.path(analysisIds), getPerformance))
  devPerformance <- merge(settings[,c('analysisId','modelSettingsId', 'cohortName', 'outcomeName',
                                      'populationSettingId','modelSettingName','addExposureDaysToStart',
                                      'riskWindowStart', 'addExposureDaysToEnd',
                                      'riskWindowEnd','devDatabase','valDatabase')],
                          devPerformance, by='analysisId', all.x=T)
  
  validationLocation <- file.path(analysesLocation,'Validation')
  if(length(dir(validationLocation))>0){
    valPerformances <- c()
    valDatabases <- dir(validationLocation, recursive = F, full.names = T)
    for( valDatabase in valDatabases){
      
      valAnalyses <-  dir(valDatabase, recursive = F, full.names = T)
      valAnalyses <-  valAnalyses[grep('Analysis_', valAnalyses)]
      valPerformance <- do.call(rbind,lapply(file.path(valAnalyses), function(x) getValidationPerformance(x)))
      valSettings <- settings[,c('analysisId','modelSettingsId', 'cohortName', 'outcomeName',
                                 'populationSettingId','modelSettingName','addExposureDaysToStart',
                                 'riskWindowStart', 'addExposureDaysToEnd',
                                 'riskWindowEnd')]
      valSettings$devDatabase <- settings$devDatabase[1]  
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
  
  allPerformance$AUC <- as.double(allPerformance$AUC)
  allPerformance$AUPRC <- as.double(allPerformance$AUPRC)
  allPerformance$outcomeCount <- as.double(allPerformance$outcomeCount)
  allPerformance$populationSize <- as.double(allPerformance$populationSize)
  allPerformance$incidence <- as.double(allPerformance$incidence)
  return(allPerformance)
}

getPerformance <- function(analysisLocation){
  location <- file.path(analysisLocation, 'plpResult.rds')
  if(!file.exists(location)){
    analysisId <- strsplit(analysisLocation, '/')[[1]]
    return(data.frame(analysisId=analysisId[length(analysisId)], 
                      AUC=0.000, AUPRC=0, outcomeCount=0,
                      populationSize=0,incidence=0,plpResultLocation=location, 
                      plpResultLoad='loadPlpResult'))
  }
  # read rds here
  res <- readRDS(file.path(analysisLocation,'plpResult.rds'))
  res <- as.data.frame(res$performanceEvaluation$evaluationStatistics)
  
  #if empty do edit?
  
  res <- tryCatch(reshape2::dcast(res[res$Eval=='test',], analysisId ~ Metric, value.var='Value'),
                  error = function(cont) return(NULL))
  if(is.null(res)){
    return(NULL) }
  res <- res[,!colnames(res)%in%c("BrierScore","BrierScaled")]
  res$incidence <- as.double(res$outcomeCount)/as.double(res$populationSize)*100
  res[, !colnames(res)%in%c('analysisId','outcomeCount','populationSize')] <- 
    format(as.double(res[, !colnames(res)%in%c('analysisId','outcomeCount','populationSize')]), digits = 2, scientific = F) 
  
  if(sum(colnames(res)=='AUC.auc_ub95ci')>0){
    res$AUC <- res$AUC.auc
    #res$AUC <- paste0(res$AUC.auc, ' (', res$AUC.auc_lb95ci,'-', res$AUC.auc_ub95ci,')')
  }
  
  res$plpResultLocation <- location
  res$plpResultLoad <- 'readRDS'#'loadPlpResult'
  return(res[,c('analysisId', 'AUC', 'AUPRC', 'outcomeCount','populationSize','incidence','plpResultLocation', 'plpResultLoad')])
}

getValidationPerformance <- function(validationLocation){
  val <- readRDS(file.path(validationLocation,'validationResult.rds'))
  if("performanceEvaluation"%in%names(val)){
    valPerformance <- reshape2::dcast(as.data.frame(val$performanceEvaluation$evaluationStatistics), 
                                      analysisId ~ Metric, value.var='Value')
  } else {
    valPerformance <- reshape2::dcast(as.data.frame(val[[1]]$performanceEvaluation$evaluationStatistics), 
                                      analysisId ~ Metric, value.var='Value')  
  }
  valPerformance$incidence <- as.double(valPerformance$outcomeCount)/as.double(valPerformance$populationSize)*100
  valPerformance[, !colnames(valPerformance)%in%c('analysisId','outcomeCount','populationSize')] <- 
    format(as.double(valPerformance[, !colnames(valPerformance)%in%c('analysisId','outcomeCount','populationSize')]), digits = 2, scientific = F) 
  
  if(sum(colnames(valPerformance)=='AUC.auc_ub95ci')>0){
    valPerformance$AUC <- valPerformance$AUC.auc
    #valPerformance$AUC <- paste0(valPerformance$AUC.auc, ' (', valPerformance$AUC.auc_lb95ci,'-', valPerformance$AUC.auc_ub95ci,')')
  }
  valPerformance$analysisId <- strsplit(validationLocation, '/')[[1]][[length(strsplit(validationLocation, '/')[[1]])]]
  valPerformance$valDatabase <- strsplit(validationLocation, '/')[[1]][[length(strsplit(validationLocation, '/')[[1]])-1]]
  valPerformance <- valPerformance[,c('analysisId','valDatabase', 'AUC', 'AUPRC', 'outcomeCount','populationSize','incidence')]
  valPerformance$plpResultLocation <- file.path(validationLocation,'validationResult.rds')
  valPerformance$plpResultLoad <- 'readRDS'
  #valPerformance$rocplot <- file.path(validationLocation,'plots','sparseROC.pdf')
  #valPerformance$calplot <- file.path(validationLocation,'plots','sparseCalibrationConventional.pdf')
  return(valPerformance)
}


#formatting
# format modelSettings
formatModSettings <- function(modelSettings){
  modelset <- data.frame(Setting = c('Model',names(modelSettings[[2]])),
                         Value = c(modelSettings[[1]], unlist(lapply(modelSettings[[2]], 
                                                                     function(x) paste0(x, collapse='')))))
  row.names(modelset) <- NULL
  return(modelset)
}

# format covariateSettings
formatCovSettings <- function(covariateSettings){
  if(class(covariateSettings)=='list'){
    #code for when multiple covariateSettings
    covariates <- c() 
    for(i in 1:length(covariateSettings)){
      if(attr(covariateSettings[[i]],'fun')=='getDbDefaultCovariateData'){
        covariatesTemp <- data.frame(covariateName = names(covariateSettings[[i]]), 
                                     SettingValue = unlist(lapply(covariateSettings[[i]], 
                                                                  function(x) paste0(x, 
                                                                                     collapse='-'))))
      } else{
        covariatesTemp <- data.frame(covariateName = covariateSettings[[i]]$covariateName,
                                     SettingValue = ifelse(sum(names(covariateSettings[[i]])%in%c("startDay","endDay"))>0,
                                                           paste(names(covariateSettings[[i]])[names(covariateSettings[[i]])%in%c("startDay","endDay")],
                                                                 covariateSettings[[i]][names(covariateSettings[[i]])%in%c("startDay","endDay")], sep=':', collapse = '-'),
                                                           "")
        )
        
      }
      covariates  <- rbind(covariates,covariatesTemp)
    }
  } else{
    covariates <- data.frame(covariateName = names(covariateSettings), 
                             SettingValue = unlist(lapply(covariateSettings, 
                                                          function(x) paste0(x, 
                                                                             collapse='-'))))
  }
  row.names(covariates) <- NULL
  return(covariates)
}

# format populationSettings
formatPopSettings <- function(populationSettings){
  population <- populationSettings
  population$attrition <- NULL # remove the attrition as result and not setting
  population <- data.frame(Setting = names(population), 
                           Value = unlist(lapply(population, 
                                                 function(x) paste0(x, 
                                                                    collapse='-')))
  ) 
  row.names(population) <- NULL
  return(population)
}



