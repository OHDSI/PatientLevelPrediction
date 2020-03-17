# uncomment if running standalone
##runPlp <- readRDS(file.path("data","results.rds"))
##validatePlp <- readRDS(file.path("data","extValidation.rds"))
source("utils.R")
analysesLocation <- file.path("data")
allPerformance <- summaryPlpAnalyses(analysesLocation)
plpResultLocation <- allPerformance[,c('plpResultLocation', 'plpResultLoad')]
#allPerformance$combinedModelSettingName <- paste0(allPerformance$modelSettingName,'-', allPerformance$modelSettingsId
formatPerformance <- allPerformance[,c('analysisId','devDatabase','valDatabase','cohortName','outcomeName','modelSettingName','riskWindowStart', 'riskWindowEnd', 'AUC','AUPRC', 'populationSize','outcomeCount','incidence',
                                       'addExposureDaysToStart','addExposureDaysToEnd')]
colnames(formatPerformance) <- c('Analysis','Dev', 'Val', 'T', 'O','Model', 'TAR start', 'TAR end', 'AUC','AUPRC', 'T Size','O Count','O Incidence (%)', 'addExposureDaysToStart','addExposureDaysToEnd')



