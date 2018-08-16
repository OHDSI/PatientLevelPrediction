# @file PackagePlp.R
#
# Copyright 2018 Observational Health Data Sciences and Informatics
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


#' exportPlpResult exports an object returned by runPlp into a network study package while removing 
#' sensitive information from the object
#' @description
#' This function should be used to export a prediciton model and performance information into a 
#' network study for others to implement on new data for external validation
#' @details
#' This is a helper function to perform external validation
#' 
#' @param plpResult          The model to be saved into the package
#' @param modelName          The name of the model
#' @param packageName        The network study package name
#' @param gitHubLocation     The github directory 
#' @param n                  If not null, the minimum number of people required for a row to be included
#' @param includeEvaluationStatistics  Whether to include includeEvaluationStatistics evaluation
#' @param includeThresholdSummary   Whether to include thresholdSummary evaluation
#' @param includeDemographicSummary   Whether to include demographicSummary evaluation
#' @param includeCalibrationSummary   Whether to include calibrationSummary evaluation
#' @param includePredictionDistribution   Whether to include predictionDistribution evaluation
#' @param includeCovariateSummary   Whether to include covariateSummary evaluation
#' 
#'
#' @return
#' The location of the saved model
#'
#' @export
exportPlpResult <- function(plpResult, modelName,
                            packageName, 
                            gitHubLocation,
                            n=NULL, 
                            includeEvaluationStatistics = T, 
                            includeThresholdSummary = T, 
                            includeDemographicSummary = T, 
                            includeCalibrationSummary = T, 
                            includePredictionDistribution = T, 
                            includeCovariateSummary = F
                            ){
  if(missing(plpResult)){
    stop('Must enter a plpResult - the object returned by runPlp()')
  }
  if(missing(packageName)){
    stop('Must enter packageName')
  }
  if(missing(gitHubLocation)){
    stop('Must enter gitHubLocation')
  }
  
  outputFolder <- file.path(gitHubLocation, packageName)
  if(!dir.exists(outputFolder)){
    stop('Incorrect gitHubLocation and packageName combination...')
  }
  
  if(!dir.exists(file.path(gitHubLocation, packageName,'inst/models'))){
    dir.create(file.path(gitHubLocation, packageName,'inst/models'), recursive = T)
  }
  
  transportPlp(plpResult = plpResult, 
               outputFolder = file.path(gitHubLocation, packageName,'inst/models', modelName), 
               n = n, 
               includeEvaluationStatistics = includeEvaluationStatistics, 
               includeThresholdSummary = includeThresholdSummary, 
               includeDemographicSummary = includeDemographicSummary, 
               includeCalibrationSummary = includeCalibrationSummary, 
               includePredictionDistribution = includePredictionDistribution, 
               includeCovariateSummary = includeCovariateSummary)
  
  return(file.path(gitHubLocation, packageName,'inst/models', modelName))
}


#' createCohort - Loads all the cohort sql in a network study and creates the cohorts
#'
#' @description
#' This function finds the sql files in a network study package, loads, renders and translates the sql then 
#' implements it 
#' @details
#' This is used by people running network studies using the package skeleton to enable users to create cohorts 
#' on their platform
#' 
#' @param cohortDetails             A dataframe containing two columns: cohortName and cohortId (if missing then the skeleton default is used when available)
#' @param cohortLocation            A string specifying the location of the cohort sql files (uses default skeleton location if missing)                                    
#' @param connectionDetails         The connection details 
#' @param cdmDatabaseSchema         A string specifying the CDM database schema e.g., database.dbo
#' @param cohortDatabaseSchema      A string specifying the cohort database schema e.g., cohort_database.dbo
#' @param cohortTable               A string specifying the cohort table 
#' @param oracleTempSchema          Temp oracle schema
#' @param package                  The name of the package 
#' 
#' @return
#' A data frame with the cohortName, cohortId, size
#'
#'
#' @export
createCohort <- function(cohortDetails, 
                         cohortLocation,
                         connectionDetails,
                         cdmDatabaseSchema,
                         cohortDatabaseSchema,
                         cohortTable,
                         oracleTempSchema= cdmDatabaseSchema,
                         package){
  if(missing(package)){
    stop('Need to enter the package...')
  }
  if(missing(cohortDetails)){
    cohorts <- system.file("extdata", "cohort_details.csv", package = package)
    if(!file.exists(cohorts)) stop('No cohortDetails or default cohort_detail.csv')
    
    cohortDetails <- read.csv(cohorts)
    
  } else{
    if(class(cohortDetails)!='data.frame'){stop('Incorrect cohortDetails')}
  }
  
  if(missing(cohortLocation)){
    pathToCohorts <- system.file("sql/sql_server", package = package)
    if(pathToCohorts==''){stop('No inst/sql/sql_server directory in package')}
  } else{
    if(class(cohortLocation)!='character') stop('cohortLocation must be character') 
  }
  
  connection <- DatabaseConnector::connect(connectionDetails)
  
  # now load, translate and render cohorts for each cohortName
  for(i in 1:length(cohortDetails$cohortName)){
  cohortName <- cohortDetails$cohortName[i]
  cohortId <- cohortDetails$cohortId[i]
  
  if(missing(cohortLocation)){
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename = paste0(cohortName,'.sql'), 
                                             packageName = package, 
                                             dbms = 'sql_server',
                                             cdm_database_schema = cdmDatabaseSchema,
                                             vocabulary_database_schema=cdmDatabaseSchema,
                                             target_database_schema = cohortDatabaseSchema,
                                             target_cohort_table = cohortTable,
                                             target_cohort_id = cohortId,
                                             oracleTempSchema = oracleTempSchema
                                             )
  } else {
    sql <- SqlRender::readSql(file.path(cohortLocation,cohortname))
    sql <- SqlRender::renderSql(sql, 
                                cdm_database_schema = cdmDatabaseSchema,
                                vocabulary_database_schema=cdmDatabaseSchema,
                                target_database_schema = cohortDatabaseSchema,
                                target_cohort_table = cohortTable,
                                target_cohort_id = cohortId,
                                oracleTempSchema = oracleTempSchema)$sql
  }
  
  sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
  
  writeLines(paste0('Inserting cohort ', cohortName, ' with cohort id: ', cohortId))
  DatabaseConnector::executeSql(connection, sql)
  }
  
  # get the final sumamry of the cohorts
  writeLines('Extracting cohort counts for each inserted cohort')
  sql <- "select cohort_definition_id as cohort_id, count(*) size from @cohortDatabaseSchema.@cohortTable where cohort_definition_id in (@cohortIds) group by cohort_definition_id"
  sql <- SqlRender::renderSql(sql, cohortDatabaseSchema = cohortDatabaseSchema, cohortTable=cohortTable,
                   cohortIds = paste(cohortDetails$cohortId, collapse=','))$sql
  sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
  counts <- DatabaseConnector::querySql(connection, sql)
  colnames(counts) <- SqlRender::snakeCaseToCamelCase(colnames(counts))
  
  result <- merge(cohortDetails, counts)
  
  return(result)
}



#' standardOutput - takes the output of runPlp or evaluatePlp and converts it into the standardised output
#' for a network study - three directories (plots, results, summary)
#'
#' @description
#' This function saves the plp study results into standardised output 
#' @details
#' This is used to ensure each study collects results consistently
#' 
#' @param result          The result of the network study
#' @param table1          the table1 result
#' @param outputLocation  The location where the results will be saved - need to have write access
#' @param studyName       The name of the network study
#' @param databaseName    The name of the cdm database 
#' @param cohortName      The name of the target population cohort
#' @param outcomeName     The name of the outcome cohort
#'
#' @return
#' The location of the saved results
#'
#' @export
standardOutput <- function(result, table1,
                           outputLocation,
                           studyName,
                           databaseName, 
                           cohortName, 
                           outcomeName){
  
  # input checks
  if(missing(result)){
    stop('Need to enter result')
  }
  
  if(sum(names(result)%in%c('performanceEvaluation', 'performance') )==0){
    stop('result missing performanceEvaluation or performance')
  }
  if(missing(outputLocation)){
    stop('Must enter outputLocation')
  }
  if(missing(studyName)){
    stop('Must enter studyName')
  }
  if(missing(databaseName)){
    stop('Must enter databaseName')
  }
  if(missing(cohortName)){
    stop('Must enter cohortName')
  }
  if(missing(outcomeName)){
    stop('Must enter outcomeName')
  }
  
  cohortId <- result$inputSetting$populationSettings$cohortId
  if(is.null(cohortId)){
    cohortId <- result$inputSetting$cohortId
  }
  outcomeId <- result$inputSetting$populationSettings$outcomeId
  if(is.null(outcomeId)){
    outcomeId <- result$inputSetting$outcomeId
  }
  mainFolder <- file.path(outputLocation, paste(studyName, databaseName,
                                                paste0('C',cohortId), 
                                                paste0('O',outcomeId), sep='_'))
  # create folders if needed
  if(!dir.exists(mainFolder)){
    dir.create(mainFolder, recursive = T)
  }
  
  #save detail to outputLocation
  predicitonDetails <- data.frame(studyName=studyName, databaseName=databaseName,
                                  cohortName=cohortName, cohortId=cohortId, 
                                  outcomeName=outcomeName, outcomeId=outcomeId,
                                  directory = paste(studyName, databaseName,
                                                    paste0('C',cohortId), 
                                                    paste0('O',outcomeId), sep='_'),
                                  fullDirectory= mainFolder)
  if(file.exists(file.path(outputLocation, 'predictionDetails.txt'))){
    write.table(predicitonDetails, file.path(outputLocation, 'predictionDetails.txt'), 
                sep = ",", col.names = F, append = T)
  } else{
    write.table(predicitonDetails, file.path(outputLocation, 'predictionDetails.txt'), 
                sep = ",", col.names = T, append = F)
  }


  if(!dir.exists(file.path(mainFolder,'plots'))){
    dir.create(file.path(mainFolder,'plots'))
  }
  #if(!dir.exists(file.path(mainFolder,'results'))){
  #  dir.create(file.path(mainFolder,'results'))
  #}
  if(!dir.exists(file.path(mainFolder,'summary'))){
    dir.create(file.path(mainFolder,'summary'))
  }
  
  if(class(result)=='runPlp'){
    savePlpResult(result, file.path(mainFolder,'results'))
    
    types <- as.character(unique(result$performanceEvaluation$thresholdSummary$Eval))
    
    for(type in types){
      if(!dir.exists(file.path(mainFolder, 'plots',type))){
        dir.create(file.path(mainFolder, 'plots',type))
      }
      plotSparseRoc(result$performanceEvaluation, 
                    fileName=file.path(mainFolder, 'plots',type,'sparseROC.pdf'), type = type)
      plotSparseCalibration2(result$performanceEvaluation, 
                             fileName=file.path(mainFolder, 'plots',type,'sparseCal.pdf'), type = type)
      plotPrecisionRecall(result$performanceEvaluation, 
                          fileName=file.path(mainFolder, 'plots',type,'precisionRecall.pdf'), type = type)
      plotDemographicSummary(evaluation = result$performanceEvaluation, type = type, 
                             fileName=file.path(mainFolder, 'plots',type,'demographicSummary.pdf'))
      plotF1Measure(evaluation = result$performanceEvaluation, type = type, 
                             fileName=file.path(mainFolder, 'plots',type,'F1Measure.pdf'))
      plotPredictionDistribution(evaluation = result$performanceEvaluation, type = type, 
                    fileName=file.path(mainFolder, 'plots',type,'boxplot.pdf'))
      plotPreferencePDF(evaluation = result$performanceEvaluation, type = type, 
                                 fileName=file.path(mainFolder, 'plots',type,'preferencePDF.pdf'))
    }
    
    # database, type (Eval), Tn, On, O%, AUC, ci, ...?
    # Eval ~ Metric var.value=Value
    summary <- as.data.frame(result$performanceEvaluation$evaluationStatistics)[,c('Eval','Metric','Value')]
    if(sum(summary$Metric=='AUC.auc_lb95ci')==0){
      extra <- data.frame(Eval=rep(types, 2), 
                          Metric=c(rep('AUC.auc_lb95ci',length(types)),rep('AUC.auc_ub95ci',length(types))), 
                          Value=rep(0,length(types)*2))
      summary <- rbind(summary, extra)
      summary$Metric <- as.character(summary$Metric)
      summary$Metric[summary$Metric=='AUC.auc'] <- 'auc'
    }
    summary <- reshape2::dcast(formula = Eval ~ Metric, data = summary, value.var = 'Value')
    summary$studyName <- studyName
    summary$databaseName <- databaseName
    summary$cohortName <- cohortName
    summary$outcomeName <- outcomeName
    write.csv(summary, file.path(mainFolder,'summary','summary.csv'), row.names = F)
  } else {
    
    if(class(result)=='validatePlp'){ result <- result$validation}
    
    tempResult <- result# list()
    tempResult$model <- list()
    tempResult$model$model <- 'none - validation'
    attr(tempResult$model, "type") <- 'validation'
    class(tempResult$model) <- 'plpModel'
    #tempResult$performanceEvaluation <- result$performanceEvaluation
    #tempResult$prediction <- result$prediction
    
    # add inputsetting info like cohortId and outcomeId? 
    #tempResult$inputSetting <- result$inputSetting

    
      
    savePlpResult(tempResult, file.path(mainFolder,'results'))
    #saveRDS(result, file.path(mainFolder,'results'))
    
    if(!dir.exists(file.path(mainFolder, 'plots','validation'))){
      dir.create(file.path(mainFolder, 'plots','validation'))
    }
    plotSparseRoc(tempResult$performanceEvaluation, 
                  fileName=file.path(mainFolder, 'plots','validation','sparseROC.pdf'), type = 'validation')
    plotSparseCalibration2(tempResult$performanceEvaluation, 
                           fileName=file.path(mainFolder, 'plots','validation','sparseCal.pdf'), type = 'validation')
    plotPrecisionRecall(tempResult$performanceEvaluation, 
                        fileName=file.path(mainFolder, 'plots','validation','precisionRecall.pdf'), type = 'validation')
    plotDemographicSummary(evaluation = tempResult$performanceEvaluation, type = 'validation', 
                           fileName=file.path(mainFolder, 'plots','validation','demographicSummary.pdf'))
    plotF1Measure(evaluation = tempResult$performanceEvaluation, type = 'validation', 
                  fileName=file.path(mainFolder, 'plots','validation','F1Measure.pdf'))
    plotPredictionDistribution(evaluation = tempResult$performanceEvaluation, type = 'validation', 
                               fileName=file.path(mainFolder, 'plots','validation','boxplot.pdf'))
    plotPreferencePDF(evaluation = tempResult$performanceEvaluation, type = 'validation', 
                      fileName=file.path(mainFolder, 'plots','validation','preferencePDF.pdf'))
    summary <- as.data.frame(tempResult$performanceEvaluation$evaluationStatistics)[,c('Eval','Metric','Value')]
    if(sum(summary$Metric=='AUV.auc_lb95ci')==0){
      extra <- data.frame(Eval=c('validation','validation'), 
                          Metric=c('AUC.auc_lb95ci','AUC.auc_ub95ci'), 
                          Value=rep(0,2))
      summary <- rbind(summary, extra)
      summary$Metric <- as.character(summary$Metric)
      summary$Metric[summary$Metric=='AUC.auc'] <- 'auc'
    }
    summary <- reshape2::dcast(formula = Eval ~ Metric, data = summary, value.var = 'Value')
    summary$studyName <- studyName
    summary$databaseName <- databaseName
    summary$cohortName <- cohortName
    summary$outcomeName <- outcomeName
    
    write.csv(summary, file.path(mainFolder,'summary','summary.csv'), row.names = F)
  }
  
  if(!missing(table1)){
    saveRDS(table1, file.path(mainFolder, 'summary', 'table1.RDS'))
  }
    
  return(mainFolder)
}


#' Package the results for sharing with OHDSI researchers
#'
#' @details
#' This function packages the results.
#'
#' @param mainFolder   The location of the folder with the standard output
#' @param includeROCplot   Whether to include ROC plot
#' @param includeCalibrationPlot   Whether to include calibration plot
#' @param includePRPlot   Whether to include precision recall plot
#' @param includeTable1   Whether to include table1
#' @param includeThresholdSummary   Whether to include thresholdSummary evaluation
#' @param includeDemographicSummary   Whether to include demographicSummary evaluation
#' @param includeCalibrationSummary   Whether to include calibrationSummary evaluation
#' @param includePredictionDistribution   Whether to include predictionDistribution evaluation
#' @param includeCovariateSummary   Whether to include covariateSummary evaluation
#' @param removeLessThanN           Whether to remove any entry with less than N people
#' @param N                         If removeLessThanN is TRUE the value for N
#'
#' @export
packageResults <- function(mainFolder, 
                           includeROCplot= T,
                           includeCalibrationPlot = T,
                           includePRPlot = T,
                           includeTable1 = T,
                           includeThresholdSummary =T,
                           includeDemographicSummary = T,
                           includeCalibrationSummary = T,
                           includePredictionDistribution =T,
                           includeCovariateSummary = T,
                           removeLessThanN = T,
                           N = 5
                           ) {
  if(missing(mainFolder)){
    stop('Missing mainFolder...')
  }
  
  #create export subfolder in workFolder
  exportFolder <- file.path(mainFolder, "export")
  if (!file.exists(exportFolder))
    dir.create(exportFolder, recursive = T)
  
  #copy all plots across
  if (!file.exists(file.path(exportFolder,'plots')))
    dir.create(file.path(exportFolder,'plots'), recursive = T)
  file.copy(file.path(mainFolder, 'plots'), file.path(exportFolder), recursive=TRUE)
  #copy all summary across
  if (!file.exists(file.path(exportFolder,'summary')))
    dir.create(file.path(exportFolder,'summary'), recursive = T)
  file.copy(file.path(mainFolder, 'summary'), file.path(exportFolder), recursive=TRUE)
  
  # delete files from exportFolder...
  if(!includeROCplot){
    file.remove(file.path(exportFolder,'plots','train','sparseROC.pdf'))
    file.remove(file.path(exportFolder,'plots','test','sparseROC.pdf'))
    file.remove(file.path(exportFolder,'plots','validation','sparseROC.pdf'))
  }
  if(!includeCalibrationPlot){
    file.remove(file.path(exportFolder,'plots','train','sparseCal.pdf'))
    file.remove(file.path(exportFolder,'plots','test','sparseCal.pdf'))
    file.remove(file.path(exportFolder,'plots','validation','sparseCal.pdf'))
  }
  if(!includePRPlot){
    file.remove(file.path(exportFolder,'plots','train','precisionRecall.pdf'))
    file.remove(file.path(exportFolder,'plots','test','precisionRecall.pdf'))
    file.remove(file.path(exportFolder,'plots','validation','precisionRecall.pdf'))
  }
  if(!includeTable1){
    file.remove(file.path(exportFolder,'summary','table1.rds'))
  }
  
  # depends on devel or eval...
  plpResult <- loadPlpResult(file.path(mainFolder, 'results'))
  
  if(removeLessThanN){
  transportPlp(plpResult,outputFolder=file.path(exportFolder, 'results'), 
               n=N,includeEvaluationStatistics=T,
                           includeThresholdSummary=includeThresholdSummary, 
                           includeDemographicSummary=includeDemographicSummary,
                           includeCalibrationSummary =includeCalibrationSummary, 
                           includePredictionDistribution=includePredictionDistribution,
                           includeCovariateSummary=includeCovariateSummary)
  } else {
    transportPlp(plpResult,outputFolder=file.path(exportFolder, 'results'), 
                 n=NULL,includeEvaluationStatistics=T,
                 includeThresholdSummary=includeThresholdSummary, 
                 includeDemographicSummary=includeDemographicSummary,
                 includeCalibrationSummary =includeCalibrationSummary, 
                 includePredictionDistribution=includePredictionDistribution,
                 includeCovariateSummary=includeCovariateSummary)
  }
  
  
  ### Add all to zip file ###
  zipName <- paste0(mainFolder, '.zip')
  OhdsiSharing::compressFolder(exportFolder, zipName)
  # delete temp folder
  unlink(exportFolder, recursive = T)
  
  writeLines(paste("\nStudy results are compressed and ready for sharing at:", zipName))
  return(zipName)
}


#' submitResults - sends a zipped folder to the OHDSI network study repository 
#'
#' @description
#' This function takes as input a zipped folder location and sends it to the OHDSI amazon repository
#' @details
#' This is used at the end of a network study to submit the results once a user has checked the folder 
#' 
#' @param exportFolder   The path to the folder containing the study results compressed file.
#' @param key            The key string as provided by the study coordinator
#' @param secret         The secret string as provided by the study coordinator
#'
#' @return
#' TRUE if the upload was successful.
#'
#' @export
submitResults <- function(exportFolder, key, secret) {
  zipName <- file.path(exportFolder)
  if (!file.exists(zipName)) {
    stop(paste("Cannot find file", zipName))
  }
  writeLines(paste0("Uploading file '", zipName, "' to study coordinating center"))
  result <- OhdsiSharing::putS3File(file = zipName,
                                    bucket = "ohdsi-study-plp",
                                    key = key,
                                    secret = secret,
                                    region = "us-east-1")
  if (result) {
    writeLines("Upload complete")
  } else {
    writeLines("Upload failed. Please contact the study coordinator")
  }
  invisible(result)
}
