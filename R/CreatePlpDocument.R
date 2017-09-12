# @file CreatePlpDocument.R
#
# Copyright 2017 Observational Health Data Sciences and Informatics
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

#' createPlpDocument
#'
#' @description
#' Creates a template for a prediction paper with the characteristics/results filled in
#' @details
#' The function creates a word document containing the analysis details, data summary and prediction model results.
#' @param plpResult                        An object of type \code{plpResult} returned by running runPlp()
#' @param plpData                          The plpData
#' @param targetName                       A string with the target description name
#' @param outcomeName                      A string with the outcome description name
#' @param characterisationSettings         A list containing the settings to determine what covariates are included into the data summary table
#' @param includeTrain                     Whether to include the train set performance
#' @param includeTest                      Whether to include the test set performance
#' @param includePredictionPicture         Whether to include a picture detailing the prediction problem
#' @param includeAttritionPlot             Whether to include the attrition plot
#' @param outputLocation                   The location to write the document to  
#' 
#' @return
#' A work document containing the selected outputs within the user's directory at location specified in outputLocation
#' @export
createPlpDocument <- function(plpResult=NULL, 
                              plpData = NULL,
                              targetName = '<target population>',
                              outcomeName = '<outcome>',
                              characterisationSettings=list(demo=T, utilization =T,
                                                            condition=T, conditionNumber=10,
                                                            drug=T, drugNumber=10,
                                                            observation=F, observationNumber=10,
                                                            procedure=F, procedureNumber=10,
                                                            measurement=F, measurementNumber=10,
                                                            include=NULL),
                              includeTrain=TRUE, includeTest=TRUE,
                              includePredictionPicture=TRUE, 
                              includeAttritionPlot=TRUE,
                              outputLocation=file.path(getwd(), 'plp_document.docx')){
  
  if(is.null(plpResult)){
    stop('plpResult needs to be input')
  }
  if(sum('plpModel'%in%class(plpResult))==0){
    stop('Incorrect plpResult class')
  }
  if(class(targetName)!='character'){
    stop('Incorrect targetName')
  }
  if(class(outcomeName)!='character'){
    stop('Incorrect outcomeName')
  }
  if(class(characterisationSettings)!='list'){
    stop('Incorrect characterisationSettings')
  }
  if(class(includeTrain)!="logical"){
    stop("Incorrect includeTrain")
  }
  if(class(includeTest)!="logical"){
    stop("Incorrect includeTest")
  }
  if(class(includePredictionPicture)!="logical"){
    stop("Incorrect includePredictionPicture")
  }  
  if(class(includeAttritionPlot)!="logical"){
    stop("Incorrect includeAttritionPlot")
  }  
  
  # create new word document
  title <- paste0('Predicting the outcome of ', outcomeName ,' in a target population of ', targetName)
  doc = ReporteRs::docx(title = paste0('<Add Paper Title (e.g., ',title,') Here>'))
  doc = ReporteRs::addTitle(doc, 'Background', level=1)
  doc = ReporteRs::addParagraph(doc, "<Add Background>" )
  
  
  #=============== BACKGROUND: characterisation ==============
  if(!is.null(plpData)){
    doc = ReporteRs::addTitle(doc, 'Characterisation', level=1)
    
    # Tab1: do characteristion and create/add table 
    populationSet <- plpResult$inputSetting$populationSettings
    populationSet$plpData <- plpData
    population <- do.call('createStudyPopulation', populationSet)
    
    charactTab <- characteriszation(plpData, population, characterisationSettings)
    charactTab <- ReporteRs::FlexTable(charactTab[,c('covariateName','CovariateCount', 
                                                     'CovariateCountWithOutcome', 'CovariateMeanWithOutcome',
                                                     'CovariateCountWithNoOutcome', 'CovariateMeanWithNoOutcome')])
    doc = ReporteRs::addFlexTable(doc, charactTab)
    
    # Tab1: Add paragraph describing data
    characterisationText <- paste0('Table 1 shows the key characteristic differences between',
                                   ' those with the outcome and those without the outcome for')
    if(characterisationSettings$demo)
      characterisationText <- paste0(characterisationText, '  the demographics,')
    if(characterisationSettings$utilization)
      characterisationText <- paste0(characterisationText, ' the patient medical utilization,')
    if(characterisationSettings$condition)
      characterisationText <- paste0(characterisationText, ' the top ',characterisationSettings$conditionNumber, ' conditions based on mean difference,')
    if(characterisationSettings$drug)
      characterisationText <- paste0(characterisationText, ' the top ',characterisationSettings$drugNumber, ' drugs based on mean difference,')
    if(characterisationSettings$observation)
      characterisationText <- paste0(characterisationText, ' the top ',characterisationSettings$observationNumber, ' observations based on mean difference,')
    if(characterisationSettings$procedure)
      characterisationText <- paste0(characterisationText, ' the top ',characterisationSettings$procedureNumber, ' procedures based on mean difference,')
    if(characterisationSettings$measurement)
      characterisationText <- paste0(characterisationText, ' the top ',characterisationSettings$measurementNumber, ' measurements based on mean difference,')
    
    #remove the last ,
    splitCharacterisationText <- strsplit(characterisationText,'')[[1]]
    characterisationText <- paste0(splitCharacterisationText[1:(length(splitCharacterisationText)-1)], collapse='')
    
    doc = ReporteRs::addParagraph(doc, characterisationText )
    
    doc = ReporteRs::addParagraph(doc, '<add comment of differences>' )
  }
  
  # Add plot of outcome vs non-outcome
  covSum <- PatientLevelPrediction::plotVariableScatterplot(plpResult$covariateSummary)
  ReporteRs::addPlot(doc, fun=print, x=covSum)
  doc = ReporteRs::addParagraph(doc, 'Figure 1 shows the scatter plot of the prevalence of each variable in the outcome vs non-outcome groups.' )
  
  #=============== METHOD: Prediction plot  ==============
  if(includePredictionPicture){
    # Pic1: add prediction plot 
    predictionPlot <- plotPlpProblem(plpResult)
    ReporteRs::addPlot(doc, fun=print, x=predictionPlot)
    #doc = ReporteRs::addPlot(predictionPlot)
    
    # Pic1: Add standardise paragraph describing prediction - use name inputs
    doc = ReporteRs::addParagraph(doc, 'Figure 2 shows the prediction visulisation...' )
  }
  
  #=============== METHOD: Analysis Information  ==============
  # Pic2: add analysis details
  ##doc = ReporteRs::addFlexTable(plpResult$model$hyperParamSearch)
  doc = ReporteRs::addParagraph(doc, textPlpAnalysis(plpResult) )
  
  
  #=============== RESULTS: attriction plot  ==============
  if(includeAttritionPlot){
    # Pic3: add attriction plot 
    attrPlot <- PatientLevelPrediction::drawAttritionDiagramPlp(attr(population,'metaData')$attrition)
    #doc = ReporteRs::addPlot(attrPlot)
    ReporteRs::addPlot(doc, fun=print, x=attrPlot)
    
    # Pic3: Add comments
    doc = ReporteRs::addParagraph(doc, "The attrition table shows..." )
  }
  
  
  #=============== RESULTS: ROC plot  ==============
  # Pic4: add test/train ROC plots 
  testROCPlot <- PatientLevelPrediction::plotSparseRoc(plpResult$performanceEvaluation, type='test')
  trainROCPlot <- PatientLevelPrediction::plotSparseRoc(plpResult$performanceEvaluation, type='train')
  if(includeTest)
    ReporteRs::addPlot(doc, fun=print, x=testROCPlot)
  #doc = ReporteRs::addPlot(testROCPlot)
  
  if(includeTrain)
    ReporteRs::addPlot(doc, fun=print, x=trainROCPlot)
  #doc = ReporteRs::addPlot(trainROCPlot)
  
  # Pic4: Add comments
  doc = ReporteRs::addParagraph(doc, "< add comments about test/train discrimination results>" )
  
  #=============== RESULTS: calibration plot  ==============
  # Pic5: add test/train calibration plots 
  testCalPlot <- PatientLevelPrediction::plotSparseCalibration2(plpResult$performanceEvaluation, type='test')
  trainCalPlot <- PatientLevelPrediction::plotSparseCalibration2(plpResult$performanceEvaluation, type='train')
  if(includeTest)
    ReporteRs::addPlot(doc, fun=print, x=testCalPlot)
  #doc = ReporteRs::addPlot(testCalPlot)
  if(includeTrain)
    ReporteRs::addPlot(doc, fun=print, x=trainCalPlot)
  #doc = ReporteRs::addPlot(trainCalPlot)
  # Pic5: Add comments
  doc = ReporteRs::addParagraph(doc, "< add comments about test/train calibration results>" )
  
  # write the document to file location
  ReporteRs::writeDoc( doc, file = file.path(outputLocation))
  
  return(TRUE)
  
}



characteriszation <- function(plpData, popualtion, characterisationSettings){
  summary <- covariateSummary(plpData, population)
  
  if(characterisationSettings$demo){
    # find demo covariates
    covs <- ff::as.ram(plpData$covariateRef$covariateId[plpData$covariateRef$analysisId<100])
    covs<- summary[summary$CovariateId%in%covs,]
    demoRows <- covs[order(-covs$CovariateCount),]
  }
  
  # add utlisation stuff?
  utilizationRows <- c()
  if(characterisationSettings$utilization){
    # find top N covariates
    covs <- ff::as.ram(plpData$covariateRef$covariateId[plpData$covariateRef$analysisId>1000 &
                                                          plpData$covariateRef$analysisId<1100])
    covs<- summary[summary$covariateId%in%covs,]
    utilizationRows <- covs[order(-covs$CovariateCount),]
  }
  
  conditionRows <- c()
  if(characterisationSettings$condition){
    # find top N covariates
    covs <- ff::as.ram(plpData$covariateRef$covariateId[ff::as.ram(plpData$covariateRef$analysisId>100 &                               
                                                                     plpData$covariateRef$analysisId<300)])
    covs<- summary[summary$covariateId%in%covs,]
    conditionRows <- covs[order(-covs$CovariateCount),][1:characterisationSettings$conditionNumber,]
  }
  
  drugRows <- c()
  if(characterisationSettings$drug){
    # find top N covariates
    covs <- ff::as.ram(plpData$covariateRef$covariateId[plpData$covariateRef$analysisId>400 &
                                                          plpData$covariateRef$analysisId<600])
    covs<- summary[summary$covariateId%in%covs,]
    drugRows <- covs[order(-covs$CovariateCount),][1:characterisationSettings$drugNumber,]
  }
  
  observationRows <- c()
  if(characterisationSettings$observation){
    # find top N covariates
    covs <- ff::as.ram(plpData$covariateRef$covariateId[plpData$covariateRef$analysisId>900 &
                                                          plpData$covariateRef$analysisId<1000])
    covs<- summary[summary$covariateId%in%covs,]
    observationRows <- covs[order(-covs$CovariateCount),][1:characterisationSettings$observationNumber,]
  }
  
  procedureRows <- c()
  if(characterisationSettings$procedure){
    # find top N covariates
    covs <- ff::as.ram(plpData$covariateRef$covariateId[plpData$covariateRef$analysisId>700 &
                                                          plpData$covariateRef$analysisId<800])
    covs<- summary[summary$covariateId%in%covs,]
    procedureRows <- covs[order(-covs$CovariateCount),][1:characterisationSettings$procedureNumber,]
  }
  
  measurementRows <- c()
  if(characterisationSettings$measurement){
    # find top N covariates
    covs <- ff::as.ram(plpData$covariateRef$covariateId[plpData$covariateRef$analysisId>900 &
                                                          plpData$covariateRef$analysisId<1000])
    covs<- summary[summary$covariateId%in%covs,]
    measurementRows <- covs[order(-covs$CovariateCount),][1:characterisationSettings$measurementNumber,]
  }  
  
  includeRows <- c()
  if(length(characterisationSettings$includes)>0){
    # find top N covariates
    covs <- characterisationSettings$includes
    covs<- summary[summary$covariateId%in%covs,]
    includeRows <- covs[order(-covs$CovariateCount),]
  }  
  
  result <- rbind(demoRows, utilizationRows,
                  conditionRows, drugRows, observationRows, procedureRows, measurementRows, 
                  includeRows)
  result <- merge(ff::as.ram(plpData$covariateRef), result, by='covariateId')
  return(result)
}


# this function plots a visulisation of the prediction problem using the outcome/target population
# and the tar information extracted from the population 
plotPlpProblem <- function(plpResult){
  
  pdf(NULL)
  dev.control(displaylist="enable")
  
  minTar <- plpResult$inputSetting$populationSettings$minTimeAtRisk
  target <- plpResult$inputSetting$populationSettings$cohortId
  outcome <- plpResult$inputSetting$populationSettings$outcomeId
  addExposureDaysToStart <- plpResult$inputSetting$populationSettings$addExposureDaysToStart
  riskWindowStart <- plpResult$inputSetting$populationSettings$riskWindowStart
  addExposureDaysToEnd <- plpResult$inputSetting$populationSettings$addExposureDaysToEnd
  riskWindowEnd <- plpResult$inputSetting$populationSettings$riskWindowEnd
  
  targetx <- 0.3
  widthTargetx <- 0.15
  par(mar = c(1, 1, 1, 1))
  diagram::openplotmat()
  lines(c(0,targetx-widthTargetx), c(0.5,0.5), type='l', lty = 1, lcol = 1)
  diagram::straightarrow(from = c(targetx-widthTargetx,0.55), to = c(0,0.55), lty = 1, lcol = 1)
  
  diagram::textempty(c(0.05, 0.6), 0.10, 0.05, lab = paste0(">= ",minTar," days"), cex = 0.8 )
  
  #diagram::straightarrow(from = c(0.2,0.5), to = c(1,0.5), lty = 3, lcol = 1)
  diagram::textrect(c(targetx,0.5), widthTargetx, 0.05,lab = paste0("Target:",target), box.col = "lightblue",
                    shadow.col = "darkblue", shadow.size = 0.005, cex = 1.2)
  
  lines(c(targetx+widthTargetx,1),c(0.5,0.5), type='l', lty = 3)
  diagram::straightarrow(from = c(0.95,0.5), to = c(1,0.5), lty = 3, lcol = 1)
  diagram::textempty(c(0.90, 0.5), 0.5, 0.05,lab = "Time", cex = 0.8 )
  
  lines(c(targetx-widthTargetx,targetx-widthTargetx),c(0.5-0.05,0.41), type='l', lty = 1)
  lines(c(targetx+widthTargetx,targetx+widthTargetx),c(0.5-0.05,0.36), type='l', lty = 1)
  diagram::textempty(c(targetx-widthTargetx, 0.4), 0.10, 0.05,lab = "start-date", cex = 0.8 )
  diagram::textempty(c(targetx+widthTargetx, 0.35), 0.10, 0.05,lab = "end-date", cex = 0.8 )
  # 0.127 -- 0.275
  
  tarstart <- targetx-widthTargetx + 2*widthTargetx*(riskWindowStart/max(riskWindowStart,riskWindowEnd))#riskWindowStart 
  if(addExposureDaysToStart)
    tarstart <- targetx+widthTargetx +  2*widthTargetx*(riskWindowStart/max(riskWindowStart,riskWindowEnd)) 
  
  tarend <- targetx-widthTargetx +  2*widthTargetx*(riskWindowEnd/max(riskWindowEnd,riskWindowEnd))
  if(addExposureDaysToEnd)
    tarend <- targetx+widthTargetx +  2*widthTargetx*(riskWindowEnd/max(riskWindowEnd,riskWindowEnd)) 
  
  if(tarend <tarstart)
    tarend <- tarstart + 0.001
  # add tar
  lines(c(tarstart,tarend),c(0.6,0.6), type='l', lty = 1)
  lines(c(tarstart,tarstart),c(0.59,0.61), type='l', lty = 1)
  startText <- ifelse(addExposureDaysToStart, paste0('end-date+ ',riskWindowStart ,'day/s'),paste0('start-date+ ',riskWindowStart ,'day/s'))
  diagram::textempty(c(tarstart-0.1,0.64), 0.10, 0.05,lab = startText, cex = 0.8 )
  
  lines(c(tarend,tarend),c(0.59,0.61), type='l', lty = 1)
  endText <- ifelse(addExposureDaysToEnd, paste0('end-date+ ',riskWindowEnd ,'day/s'),paste0('start-date+ ',riskWindowEnd ,'day/s'))
  diagram::textempty(c(tarend-0.1,0.64), 0.10, 0.05,lab = endText, cex = 0.8 )
  
  diagram::textrect(c(tarstart+(tarend-tarstart)/2,0.6), 0.05, 0.02,lab = 'TAR', cex = 0.8 )
  
  diagram::textellipse(c(tarstart+(tarend-tarstart)*0.6,0.5), 0.1, 0.03,lab = paste0('outcome:',outcome), box.col = "yellow", cex = 0.8 )
  
  result <- recordPlot()
  invisible(dev.off())
  
  return(result)
}



textPlpAnalysis <- function(plpResult){
  
  # r version
  rversion <- plpResult$executionSummary$PackageVersion$packageVersion
  
  # test fract
  testfrac <- plpResult$inputSetting$testFraction
  
  # nfold
  nfold <- plpResult$inputSetting$nfold
  
  # model name
  name <- plpResult$inputSetting$modelSettings$name
  
  # execution time
  execution <- plpResult$executionSummary$TotalExecutionElapsedTime
  
  result <- paste0("A ", name, " was trained using ",nfold, " cross-validation on a training dataset consisting of",
                   " ", (1-testfrac)*100,"% of the total dataset, with the remaining ",testfrac*100,"% of the dataset",
                   " held out to enable an internal validation of the model.  The PatientLevelPrediction R package version ",
                   rversion, " was used and the total training/valdiation time was ",execution,".")
  
  return(result)
  
}



#' createPlpJournalDocument
#'
#' @description
#' Creates a template for a prediction journal paper with the characteristics/results filled in
#' @details
#' The function creates a word document containing the analysis details, data summary and prediction model results.
#' @param plpResult                        An object of type \code{plpResult} returned by running runPlp()
#' @param plpData                          The plpData
#' @param targetName                       A string with the target description name
#' @param outcomeName                      A string with the outcome description name
#' @param characterisationSettings         A list containing the settings to determine what covariates are included into the data summary table
#' @param includeTrain                     Whether to include the train set performance
#' @param includeTest                      Whether to include the test set performance
#' @param includePredictionPicture         Whether to include a picture detailing the prediction problem
#' @param includeAttritionPlot            Whether to include the attriction plot
#' @param outputLocation                   The location to write the document to  
#' 
#' @return
#' A work document containing the selected outputs within the user's directory at location specified in outputLocation
#' @export
createPlpJournalDocument <- function(plpResult=NULL, 
                                     plpData = NULL,
                                     targetName = '<target population>',
                                     outcomeName = '<outcome>',
                                     characterisationSettings=list(demo=T, utilization =T,
                                                                   condition=T, conditionNumber=10,
                                                                   drug=T, drugNumber=10,
                                                                   observation=F, observationNumber=10,
                                                                   procedure=F, procedureNumber=10,
                                                                   measurement=F, measurementNumber=10,
                                                                   include=NULL),
                                     includeTrain=FALSE, includeTest=TRUE,
                                     includePredictionPicture=TRUE, 
                                     includeAttritionPlot=TRUE,
                                     outputLocation=file.path(getwd(), 'plp_journal_document.docx')){
  
  if(is.null(plpResult)){
    stop('plpResult needs to be input')
  }
  if(sum('plpModel'%in%class(plpResult))==0){
    stop('Incorrect plpResult class')
  }
  if(class(targetName)!='character'){
    stop('Incorrect targetName')
  }
  if(class(outcomeName)!='character'){
    stop('Incorrect outcomeName')
  }
  if(class(characterisationSettings)!='list'){
    stop('Incorrect characterisationSettings')
  }
  if(class(includeTrain)!="logical"){
    stop("Incorrect includeTrain")
  }
  if(class(includeTest)!="logical"){
    stop("Incorrect includeTest")
  }
  if(class(includePredictionPicture)!="logical"){
    stop("Incorrect includePredictionPicture")
  }  
  if(class(includeAttritionPlot)!="logical"){
    stop("Incorrect includeAttritionPlot")
  }  
  
  # rerun the pop
  populationSet <- plpResult$inputSetting$populationSettings
  populationSet$plpData <- plpData
  population <- do.call('createStudyPopulation', populationSet)
  
  # create new word document
  doc = ReporteRs::docx()
  
  target_size <- nrow(population)
  outcome_size <- sum(population$outcomeCount==1)
  if(populationSet$addExposureDaysToEnd & 
     populationSet$addExposureDaysToStart){
    time_at_risk <- paste0(populationSet$riskWindowStart,
                           " day/s from target end date  to ", populationSet$riskWindowEnd,
                           " days from target end date ")
  }
  if(!populationSet$addExposureDaysToEnd & 
     populationSet$addExposureDaysToStart){
    time_at_risk <- paste0(populationSet$riskWindowStart,
                           " day/s from target end date  to ", populationSet$riskWindowEnd,
                           " days from target start date ")
  }
  if(populationSet$addExposureDaysToEnd & 
     !populationSet$addExposureDaysToStart){
    time_at_risk <- paste0(populationSet$riskWindowStart,
                           " day/s from target start date  to ", populationSet$riskWindowEnd,
                           " days from target end date ")
  }
  if(!populationSet$addExposureDaysToEnd & 
     !populationSet$addExposureDaysToStart){
    time_at_risk <- paste0(populationSet$riskWindowStart,
                           " day/s from target start date to ", populationSet$riskWindowEnd,
                           " days from target start date ")
  }
  
  #============ TITLE ==========================================
  title <- paste0("Development of a multivariate model to predict ", outcomeName ,
                  " in a target population of ",targetName," during ",time_at_risk," using observational data")
  
  #title <- paste0('Predicting the outcome of ', outcomeName ,' in a target population of ', targetName)
  doc <- ReporteRs::addTitle( doc, title, level = 1 )
  
  
  #============ ABSTRACT ==========================================
  abstract <- c(paste0("Objective: To develop a model to predict ", outcomeName, 
                       " within a target population of ", targetName,
                       " during ",time_at_risk," and evaluate the model performance using calibration ",
                       "and discrimination performance measures."),
                
                paste0("Methods: In <add development database> mapped to the Observational Medical Outcome ",
                       "Partnership (OMOP) common data model ",target_size," people satisfied the ",
                       "atarget criteria and ",outcome_size," had the outcome during ",time_at_risk, 
                       "A ",plpResult$inputSetting$modelSettings$name," was trained using the predictors <add predictor variables> and ",
                       "externally validated by applying the model to <add external databases> mapped to",
                       " the OMOP common data model with <target/outcome sizes> respectively. "),
                
                paste0(" Results: The internal validation showed the model achieved < good/excellent>", 
                       " discrimination ability with an AUC of <auc value> and the calibration plots",
                       " indicates a <fair/well> calibrated model.  The external validation showed",
                       " the model <was/was not> transportable, with AUCs ranging between <auc range> on",
                       " the <databases> databases. "),
                
                paste0("Conclusions: This paper details the transparent development of a < good/excellent>",
                       " discriminative model for predicting ", outcomeName, " in <target details or external target",
                       " details>.  The model can be readily implemented to any observational healthcare",
                       " database in the OMOP common data model/development code and is available from",
                       " <add weblink>.")
  )
  
  doc = ReporteRs::addTitle(doc, 'Abstract', level=2)
  doc = ReporteRs::addParagraph(doc, abstract )
  
  
  #============ BACKGROUND ==========================================
  background <- c(paste0("<background on outcome: motivation for model, list existing models",
                         " (database developed on, external validation, performance)>"),
                  
                  paste0("The objective of this paper is to use the Observational Healthcare and Data Science",
                         " Informatics (OHDSI) Patient Level Prediction software, an open source R package,",
                         " to develope a <diagnostic/prognostic> model to predict ",outcomeName," within ",
                         targetName,".  The software implements a framework for developing ",
                         " diagnostic/prognostic models while addressing existing best practices towards ",
                         " ensuring models are clinically useful and transparent.  The model will be develop ",
                         " on <development database> and externally validated on <validation databases> to ",
                         " determine the transportability and generalizability of the model when applied to ",
                         " new data.  All the datasets will be in the Observational Medical Outcome ",
                         " Partnership (OMOP) common data model as having the datasets in a homogeneous data ",
                         " structure enables re-use of code between model development and validation to ensure ",
                         " the model can be externally validated efficiently and reduce model reproducibility errors.")
  )
  doc = ReporteRs::addTitle(doc, 'Background', level=2)
  doc = ReporteRs::addParagraph(doc, background )
  
  #=============== METHOD: Prediction plot  ==============
  if(includePredictionPicture){
    # Pic1: add prediction plot 
    predictionPlot <- plotPlpProblem(plpResult)
    ReporteRs::addPlot(doc, fun=print, x=predictionPlot)
    #doc = ReporteRs::addPlot(predictionPlot)
    
    # Pic1: Add standardise paragraph describing prediction - use name inputs
    doc = ReporteRs::addParagraph(doc, 'Figure 2 shows the prediction visulisation...' )
  }
  
  
  
  #=============== METHOD: Analysis Information  ==============
  # Pic2: add analysis details
  ##doc = ReporteRs::addFlexTable(plpResult$model$hyperParamSearch)
  doc = ReporteRs::addTitle(doc, 'Method', level=2)
  doc = ReporteRs::addTitle(doc, 'Source of data:', level=3)
  datasources <- c("<Truven MarketScan Medicare Supplemental Beneficiaries (MDCR)  this is a US insurance claims database containing 9,559,877 lives between the years 2000-01-01 to 2016-04-30>",
                   "<Truven MarketScan Medicaid (MDCD) this is a US insurance claims database containing 21,577,517 lives between the years 2006-01-01 to 2014-12-31>",
                   "<OptumInsights de-identified ClinformaticsTM  Datamart (Optum)  this is a US electronic healthcare database containing 73,969,539 lives between the years 2000-05-01 to 2016-03-31>",
                   "<Truven MarketScan Commercial Claims and Encounters (CCAE)  this is a US insurance claims database containing 131,533,722 lives between the years 2000-01-01 to 2016-04-30>"
  )
  doc <- ReporteRs::addParagraph( doc, value = datasources, stylename="BulletList" )
  
  doc = ReporteRs::addTitle(doc, 'Target population:', level=3)
  doc <- ReporteRs::addParagraph( doc, value = "<target definition>")
  
  doc = ReporteRs::addTitle(doc, 'Outcome:', level=3)
  doc <- ReporteRs::addParagraph( doc, value = "<outcome definition>")
  
  doc = ReporteRs::addTitle(doc, 'Predictors:', level=3)
  covset <- plpResult$inputSetting$dataExtrractionSettings$covariateSettings
  covs <- unlist(covset)[grep('use',names(unlist(covset)))]
  covs <- gsub('useCovariate','',names(covs)[covs==1])
  timeset <- paste0("Longterm days:",covset$longTermDays, "-",
                    "Mediumterm days:",covset$mediumDays, "-",
                    "Shortterm days:",covset$shortTermDays, "-",
                    "WindowEnd days:",covset$windowEndDays)
  
  doc <- ReporteRs::addParagraph( doc, value = c(covs,timeset) , stylename="BulletList")
  doc <- ReporteRs::addParagraph( doc, value = "<!Clarify about missing data>")
  
  doc = ReporteRs::addTitle(doc, 'Statistical analysis methods', level=3)
  doc = ReporteRs::addParagraph(doc, textPlpAnalysis(plpResult) )
  evaltext <- paste0("To evaluate the models the model discrimination is assessed using the area under",
                     " the receiver operating characteristic curve (AUC) and the model calibration is ",
                     "assessed by inspecting a calibration plot.")
  doc = ReporteRs::addParagraph(doc, evaltext )
  
  
  #=============== RESULTS: attriction plot  ==============
  doc = ReporteRs::addTitle(doc, 'Results', level=2)
  doc = ReporteRs::addTitle(doc, 'Target population summary', level=3)
  
  text <- paste0("The number of people eligible for inclusion into the target population, ",
                 "outcome count and the number of people lost due to each inclusion step are ",
                 "presented in Figure  ")
  doc = ReporteRs::addParagraph(doc, text )
  
  if(includeAttritionPlot){
    # Pic3: add attriction plot 
    attrPlot <- PatientLevelPrediction::drawAttritionDiagramPlp(attr(population,'metaData')$attrition)
    #doc = ReporteRs::addPlot(attrPlot)
    ReporteRs::addPlot(doc, fun=print, x=attrPlot)
    
    # Pic3: Add comments
    doc = ReporteRs::addParagraph(doc, "The attrition table shows..." )
  }
  
  #=============== characterisation ==============
  if(!is.null(plpData)){
    doc = ReporteRs::addTitle(doc, 'Characterisation', level=3)
    
    # Tab1: do characteristion and create/add table 
    charactTab <- characteriszation(plpData, population, characterisationSettings)
    charactTab <- ReporteRs::FlexTable(charactTab[,c('covariateName','CovariateCount', 
                                                     'CovariateCountWithOutcome', 'CovariateMeanWithOutcome',
                                                     'CovariateCountWithNoOutcome', 'CovariateMeanWithNoOutcome')])
    doc = ReporteRs::addFlexTable(doc, charactTab)
    
    # Tab1: Add paragraph describing data
    characterisationText <- paste0('Table 1 shows the key characteristic differences between',
                                   ' those with the outcome and those without the outcome for')
    if(characterisationSettings$demo)
      characterisationText <- paste0(characterisationText, '  the demographics,')
    if(characterisationSettings$utilization)
      characterisationText <- paste0(characterisationText, ' the patient medical utilization,')
    if(characterisationSettings$condition)
      characterisationText <- paste0(characterisationText, ' the top ',characterisationSettings$conditionNumber, ' conditions based on mean difference,')
    if(characterisationSettings$drug)
      characterisationText <- paste0(characterisationText, ' the top ',characterisationSettings$drugNumber, ' drugs based on mean difference,')
    if(characterisationSettings$observation)
      characterisationText <- paste0(characterisationText, ' the top ',characterisationSettings$observationNumber, ' observations based on mean difference,')
    if(characterisationSettings$procedure)
      characterisationText <- paste0(characterisationText, ' the top ',characterisationSettings$procedureNumber, ' procedures based on mean difference,')
    if(characterisationSettings$measurement)
      characterisationText <- paste0(characterisationText, ' the top ',characterisationSettings$measurementNumber, ' measurements based on mean difference,')
    
    #remove the last ,
    splitCharacterisationText <- strsplit(characterisationText,'')[[1]]
    characterisationText <- paste0(splitCharacterisationText[1:(length(splitCharacterisationText)-1)], collapse='')
    
    doc = ReporteRs::addParagraph(doc, characterisationText )
    
    doc = ReporteRs::addParagraph(doc, '<add comment of differences>' )
  }
  
  # Add plot of outcome vs non-outcome
  covSum <- PatientLevelPrediction::plotVariableScatterplot(plpResult$covariateSummary)
  ReporteRs::addPlot(doc, fun=print, x=covSum)
  doc = ReporteRs::addParagraph(doc, 'Figure 1 shows the scatter plot of the prevalence of each variable in the outcome vs non-outcome groups.' )
  
  
  text <- paste0("Table 1 presents the baseline characteristics of the development datasets and validation ",
                 " datasets <add text describing key features> ")
  doc = ReporteRs::addParagraph(doc, text )
  
  doc = ReporteRs::addTitle(doc, 'Model Specification', level=3)
  text <- paste0("The model developed on <database> with a target size of <target count> and outcome count ",
                 " of <outcome count> is available from <add link>.  The <coefficients/variable importance> ",
                 "for each predictor is available as a supplement.")
  doc = ReporteRs::addParagraph(doc, text )
  
  doc = ReporteRs::addTitle(doc, 'Model Performance', level=3)
  eval <- plpResult$performanceEvaluation$evaluationStatistics
  auc <- eval[eval[,'Eval']=='test' & eval[,'Metric']=='AUC.auc','Value']
  if(length(eval[eval[,'Eval']=='test' & eval[,'Metric']=='AUC.auc_lb95ci','Value'])>0)
    auc <- paste0(auc, '(',eval[eval[,'Eval']=='test' & eval[,'Metric']=='AUC.auc_lb95ci','Value'],
                  '-',eval[eval[,'Eval']=='test' & eval[,'Metric']=='AUC.auc_lb95ci.1','Value'],')')
  text <- paste0(" The internal validation of the model obtained an AUC of ",auc,
                 " the ROC plot is presented in Figure 2.  The calibration plot for the internal validation ",
                 "of the model is presented in Figure 3.")
  doc = ReporteRs::addParagraph(doc, text )
  
  #=============== RESULTS: ROC plot  ==============
  # Pic4: add test/train ROC plots 
  testROCPlot <- PatientLevelPrediction::plotSparseRoc(plpResult$performanceEvaluation, type='test')
  trainROCPlot <- PatientLevelPrediction::plotSparseRoc(plpResult$performanceEvaluation, type='train')
  if(includeTest)
    ReporteRs::addPlot(doc, fun=print, x=testROCPlot)
  #doc = ReporteRs::addPlot(testROCPlot)
  
  if(includeTrain)
    ReporteRs::addPlot(doc, fun=print, x=trainROCPlot)
  #doc = ReporteRs::addPlot(trainROCPlot)
  #=============== RESULTS: calibration plot  ==============
  # Pic5: add test/train calibration plots 
  testCalPlot <- PatientLevelPrediction::plotSparseCalibration2(plpResult$performanceEvaluation, type='test')
  trainCalPlot <- PatientLevelPrediction::plotSparseCalibration2(plpResult$performanceEvaluation, type='train')
  if(includeTest)
    ReporteRs::addPlot(doc, fun=print, x=testCalPlot)
  #doc = ReporteRs::addPlot(testCalPlot)
  if(includeTrain)
    ReporteRs::addPlot(doc, fun=print, x=trainCalPlot)
  
  text <- paste0(" The external validation on <dataset 1> consisting of a target population of ",
                 "<target count> and outcome count of <outcome count> obtained an AUC of <add auc> ",
                 "(<auc ci>).  [repeat for each dataset].  The external validation calibration plots ",
                 "can be found in Appendix 2.")
  doc = ReporteRs::addParagraph(doc, text )
  
  doc = ReporteRs::addTitle(doc, 'Discussion', level=2)
  doc = ReporteRs::addTitle(doc, 'Interpretation', level=3)
  text <-c(
    paste0("The discriminative ability of the model was <average/good/excellent>, obtaining an AUC of ",auc,
           " indicating the model can distinguish between people who will develop the outcome and those ",
           "who are unlike to develop the outcome and <compare with existing models if possible>."),
    paste0("The results show the model is <reasonably/well> calibrated on the development dataset <but/and> ",
           "is <not/is also> well calibrated on the validation datasets.  This shows "),
    paste0("The most predictive variables were <add interesting ones from top 20>.  The variables <add> ",
           "are known or suspected to be risk factors of <add outcome> but the model has highlighted ",
           "<add variables> as predictive but they have not been incorporated in previous models.  ",
           "These variables could be studied using conventional population level estimation to determine ",
           "whether they are causally related to the outcome.")
  )
  doc = ReporteRs::addParagraph(doc, text )
  
  doc = ReporteRs::addTitle(doc, 'Implications', level=3)
  text <-c(
    paste0("The results show that developing a model using <add database> data for the outcome ",outcomeName,
           " within ",targetName," resulted in a good discriminative ability and this model was validated ",
           "across several datasets and showed a consistently high external validation AUC.  This suggests ",
           "the model could be a useful tool to aid decision making for ..."),
    paste0("Inspecting the model variable importance may help to gain new insight into the disease dynamics ",
           "into the development of <outcome>.  As the model highlighted <new predictors> as potential new ",
           "risk factors, it would be useful in future research to determine whether these variables do in fact have a ",
           "biological relationship to the outcome.")
  )
  doc = ReporteRs::addParagraph(doc, text )
  
  doc = ReporteRs::addTitle(doc, 'Limitations', level=3)
  text <-c(
    paste0("In this study we have developed a model on one US observational healthcare database and ",
           "externally validated it across several other US databases to aim to determine the ",
           "generalizability of the model.  However, each database only includes a sample of the whole ",
           "US population and may not be representative of the whole US population.  "),
    paste0("It is also possible for predictors such as conditions or drugs to be missing from the databases ",
           "(e.g., over the counter medication) and missing data will result in no record for the condition ",
           "or drug and therefore be treated like an absence of the condition or drug.  Therefore the ",
           "datasets are likely to contain noise and this could potentially lead to misclassification."),
    paste0("Observational datasets often lack certain variables such as genetic factors or lifestyle factors ",
           "that may be highly predictive of the outcome being investigated.  This may results in models ",
           "that do not perform as well as models developed on datasets that contain variables on genetics ",
           "or lifestyle. However, observational datasets often contain thousands of variables that may be ",
           "used as proxies for genetic or lifestyle factors and observational data is often more readily ",
           "available.")
  )
  doc = ReporteRs::addParagraph(doc, text )
  
  doc = ReporteRs::addTitle(doc, 'Conclusion', level=2)
  test <- paste0("In this paper we developed a model for ",outcomeName," occurring within a target ",
                 "population consisting of ",targetName," during ",time_at_risk," on <development database> and ",
                 "externally validated the model on <validation datasets>.  The discriminative ability of ",
                 "the model was  and the model was  calibrated. <talk about clinical usefulness>.  ",
                 "In the future it would be useful to extend the external validation across the OHDS ",
                 "network and outside the OHDSI network and also determine the clinical usefulness of the ",
                 "model by implementing it retrospectively in a new dataset [ref].")
  doc = ReporteRs::addParagraph(doc, text )
  
  doc = ReporteRs::addTitle(doc, 'References', level=2)
  doc = ReporteRs::addParagraph(doc, "<add references>" )
  
  doc = ReporteRs::addTitle(doc, 'Appendix A', level=2)
  doc = ReporteRs::addParagraph(doc, "<atlas cohorts + concept sets>" )
  
  doc = ReporteRs::addTitle(doc, 'Appendix B', level=2)
  doc = ReporteRs::addParagraph(doc, "<calibration plots of external validation>" )
  
  # write the document to file location
  ReporteRs::writeDoc( doc, file = file.path(outputLocation))
  
  return(TRUE)
  
}
