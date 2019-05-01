# @file CreatePlpDocument.R
#
# Copyright 2019 Observational Health Data Sciences and Informatics
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

#' createPlpReport
#'
#' @description
#' Creates a word document report of the prediction
#' @details
#' The function creates a word document containing the analysis details, data summary and prediction model results.
#' @param plpResult                        An object of type \code{plpResult} returned by running runPlp()
#' @param plpValidation                    An object of type \code{validatePlp} returned by running externalValidatePlp()
#' @param plpData                          The plpData
#' @param targetName                       A string with the target description name
#' @param outcomeName                      A string with the outcome description name
#' @param targetDefinition                 The cohort details
#' @param outcomeDefinition                The cohort details
#' @param outputLocation                   The location to write the document to
#' @param save                             If false the output of the function of the function is the document rather than creating the document in outputLocation
#'
#' @return
#' A work document containing the selected outputs within the user's directory at location specified in outputLocation
#' @export
createPlpReport <- function(plpResult=NULL, plpValidation=NULL,
                            plpData = NULL,
                            targetName = '<target population>',
                            outcomeName = '<outcome>',
                            targetDefinition = NULL,
                            outcomeDefinition = NULL,
                            outputLocation=file.path(getwd(), 'plp_report.docx'),
                            save= T){

  if(is.null(plpResult)){
    stop('plpResult needs to be input')
  }
  if(sum('runPlp'%in%class(plpResult))==0){
    stop('Incorrect plpResult class')
  }
  if(class(targetName)!='character'){
    stop('Incorrect targetName')
  }
  if(class(outcomeName)!='character'){
    stop('Incorrect outcomeName')
  }

  if(!is.null(plpValidation)){
    if(class(plpValidation)!="validatePlp"){
      stop('Incorrect plpValidation')
    }
  }

  #================ CALCULATE KEY VARIABLES =========================
  # calcualte the auc
  eval <- plpResult$performanceEvaluation$evaluationStatistics
  auc <- formatDocNumbers(eval[eval[,'Eval']=='test' & eval[,'Metric']%in%c('AUC.auc',"AUC"),'Value'])
  if(length(eval[eval[,'Eval']=='test' & eval[,'Metric']=='AUC.auc_lb95ci','Value'])>0)
    auc <- paste0(auc, '(',formatDocNumbers(eval[eval[,'Eval']=='test' & eval[,'Metric']=='AUC.auc_lb95ci','Value']),
                  '-',formatDocNumbers(eval[eval[,'Eval']=='test' & eval[,'Metric']=='AUC.auc_ub95ci','Value']),')')

  # rerun the pop
  populationSet <- plpResult$inputSetting$populationSettings
  populationSet$plpData <- plpData
  population <- do.call('createStudyPopulation', populationSet)

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

  #-----------------------------------------------
  #+++++++++++++++++++++++++++++++++++++++++++++++

  #============== CREATE DOCUMENT =================
  # create new word document
  doc = officer::read_docx()
  #------------------------------------------------

  #============ TITLE ==========================================
  title <- paste0("Report: predicting ", outcomeName ,
                  " in a target population of ",targetName," during ",time_at_risk," using observational data")

  #title <- paste0('Predicting the outcome of ', outcomeName ,' in a target population of ', targetName)
  doc <- doc %>% officer::body_add_par(value = title, style = "heading 1")
  #------------------------------------------------


  #============ AIM ==========================================
  #  Add the aim of the prediction
  doc <- doc %>% officer::body_add_par(value = 'Aim', style = "heading 2")
  

  text <- paste0("Within the target popualtion of ",targetName," predict ", outcomeName,
                 " during ", time_at_risk,". See Appendix 1 for target popualtion and outcome ",
                 " cohort definitions."
                 )
  doc <- doc %>% officer::body_add_par(value = text, style = "Normal")
  
  #------------------------------------------------


  #============ Data ==========================================
  #  The data source used to develop the model and
  doc <- doc %>% officer::body_add_par(value = 'Data', style = "heading 2")
  

  doc <- doc %>% officer::body_add_par(value = 'Source of data', style = "heading 3")
  
  datasource <- "Add text about the database used to develop the model including the number of people and database date range"
  doc <- doc %>% officer::body_add_par(value = datasource, style = "Normal")
  
  # characterisation
  doc <- doc %>% officer::body_add_par(value = 'Data characterisation:', style = "heading 3")
  
  covSum <- PatientLevelPrediction::plotVariableScatterplot(plpResult$covariateSummary)
  doc <- doc %>% officer::body_add_gg(value = covSum)
  doc <- doc %>% officer::body_add_par(value = 'Figure 1 shows the scatter plot of the prevalence of each variable in the outcome vs non-outcome groups.', style = "Normal")
  
  textPar <- "The covariateSummary.csv contains the prevalance for each covariate overall, in the outcome group and in the non-outcome group."
  doc <- doc %>% officer::body_add_par(value = textPar, style = "Normal")
  
  doc <- doc %>% officer::body_add_par(value = 'Attrition:', style = "heading 3")
  
  # add table of attrition...
  doc <- doc %>% officer::body_add_table(value=plpResult$model$populationSettings$attrition)

  #------------------------------------------------

  #============ Settings  ==========================================
  #  The data source used to develop the model and
  doc <- doc %>% officer::body_add_par(value = 'Settings', style = "heading 2")
  
  textPar <- "This section contains all the settings used in the analysis"
  doc <- doc %>% officer::body_add_par(value = textPar, style = "Normal")
  
  doc <- doc %>% officer::body_add_par(value = 'Covariate Settings:', style = "heading 3")
  
  # add table of covariate settings
  covSet <- data.frame(setting=names(unlist(plpResult$model$metaData$call$covariateSettings)),
                       choice = unlist(plpResult$model$metaData$call$covariateSettings))
  rownames(covSet) <- NULL
  if(nrow(covSet)!=0){
    doc <- doc %>% officer::body_add_table(value = covSet)
  }
  
  doc <- doc %>% officer::body_add_par(value = 'Population Settings:', style = "heading 3")
  
  # add table of population settings
  plpResult$inputSetting$populationSettings$attrition <- NULL
  popSet <- data.frame(setting=names(unlist(plpResult$inputSetting$populationSettings)),
                       choice = unlist(plpResult$inputSetting$populationSettings))
  rownames(popSet) <- NULL
  doc <- doc %>% officer::body_add_table(value = popSet)

  doc <- doc %>% officer::body_add_par(value = 'Model Settings:', style = "heading 3")
  
  # add table of model settings
  #!!!!!!!!!=========== TODO - add model name and hyper-param search to doc
  modelName <- plpResult$inputSetting$modelSettings$name
  doc <- doc %>% officer::body_add_par(value = paste("Trained a ",modelName, "with default values and ",
                                                     "hyper-parameters in table below."), style = "Normal")
  
  # default parameters of model
  doc <- doc %>% officer::body_add_par(value = paste("The default model parameters:"), style = "Normal")
  
  defaultSet <- unlist(lapply((formals(get(gsub('fit','set',plpResult$inputSetting$modelSettings$model)))), function(x) paste(x, collapse=',', sep=',')))
  defaultSet <- data.frame(names(defaultSet), defaultSet)
  row.names(defaultSet) <- NULL
  doc <- doc %>% officer::body_add_table(value = defaultSet)
  
  # hyper-parameters other than default searched and performance
  doc <- doc %>% officer::body_add_par(value = paste("The hyper-parameters searched and the performance:"), style = "Normal")
  
  hyparamSet <-as.data.frame(plpResult$model$hyperParamSearch)
  doc <- doc %>% officer::body_add_table(value = hyparamSet)

  #------------------------------------------------



  #============ Results  ==========================================
  #  All the results
  doc <- doc %>% officer::body_add_par(value = 'Results', style = "heading 2")
  
  doc <- doc %>% officer::body_add_par(value = 'Evaluation Summary:', style = "heading 3")
  
  doc <- doc %>% officer::body_add_par(value = paste("The summary performance table:"), style = "Normal")
  
  evalSet <- plpResult$performanceEvaluation$evaluationStatistics
  rownames(evalSet) <- NULL
  evalSet <- as.data.frame(evalSet)
  doc <- doc %>% officer::body_add_table(value = evalSet)

  doc <- doc %>% officer::body_add_par(value = 'ROC Plots', style = "heading 3")
  
  # add test/train ROC plots
  doc <- doc %>% officer::body_add_par(value = paste("The overal discriminative performance:"), style = "Normal")
  
  testCalPlot <- PatientLevelPrediction::plotSparseRoc(plpResult$performanceEvaluation, type='test')
  trainCalPlot <- PatientLevelPrediction::plotSparseRoc(plpResult$performanceEvaluation, type='train')
  testCalPlot <- testCalPlot + ggplot2::labs(title=paste("Test"))
  trainCalPlot <- trainCalPlot + ggplot2::labs(title=paste("Train"))
  doc <- doc %>% officer::body_add_gg(value = testCalPlot)
  doc <- doc %>% officer::body_add_gg(value = trainCalPlot)
  
  doc <- doc %>% officer::body_add_par(value = 'Calibration Plots:', style = "heading 3")
  
  # add test/train calibration plots
  doc <- doc %>% officer::body_add_par(value = paste("The model calibration (how well the predicted risk matches the true risk):."), style = "Normal")
  
  testCalPlot <- PatientLevelPrediction::plotSparseCalibration2(plpResult$performanceEvaluation, type='test')
  trainCalPlot <- PatientLevelPrediction::plotSparseCalibration2(plpResult$performanceEvaluation, type='train')
  testCalPlot <- testCalPlot + ggplot2::labs(title=paste("Test"))
  trainCalPlot <- trainCalPlot + ggplot2::labs(title=paste("Train"))
  doc <- doc %>% officer::body_add_gg(value = testCalPlot)
  doc <- doc %>% officer::body_add_gg(value = trainCalPlot)
  
  doc <- doc %>% officer::body_add_par(value = 'Demographic Summary Plots:', style = "heading 3")
  
  doc <- doc %>% officer::body_add_par(value = paste("The calibration across age/gender groups:"), style = "Normal")
  
  if(!is.null(plpResult$performanceEvaluation$demographicSummary)){
    testCalPlot <- PatientLevelPrediction::plotDemographicSummary(plpResult$performanceEvaluation, type='test')
    trainCalPlot <- PatientLevelPrediction::plotDemographicSummary(plpResult$performanceEvaluation, type='train')
    testCalPlot <- testCalPlot + ggplot2::labs(title=paste("Test"))
    trainCalPlot <- trainCalPlot + ggplot2::labs(title=paste("Train"))
    doc <- doc %>% officer::body_add_gg(value = testCalPlot)
    doc <- doc %>% officer::body_add_gg(value = trainCalPlot)
  }
  

  doc <- doc %>% officer::body_add_par(value =  'Preference PDF Plots:', style = "heading 3")
  
  doc <- doc %>% officer::body_add_par(value =  paste("Scaled predicted risk distributions for the outcome and non-outcome people:"), style = "Normal")
  
  testCalPlot <- PatientLevelPrediction::plotPreferencePDF(plpResult$performanceEvaluation, type='test')
  trainCalPlot <- PatientLevelPrediction::plotPreferencePDF(plpResult$performanceEvaluation, type='train')
  testCalPlot <- testCalPlot + ggplot2::labs(title=paste("Test"))
  trainCalPlot <- trainCalPlot + ggplot2::labs(title=paste("Train"))
  doc <- doc %>% officer::body_add_gg(value = testCalPlot)
  doc <- doc %>% officer::body_add_gg(value = trainCalPlot)
  
  doc <- doc %>% officer::body_add_par(value =  'Predicted PDF Plots:', style = "heading 3")
  
  doc <- doc %>% officer::body_add_par(value =  paste("Predicted risk distributions for the outcome and non-outcome people:"))
  
  testCalPlot <- PatientLevelPrediction::plotPredictedPDF(plpResult$performanceEvaluation, type='test')
  trainCalPlot <- PatientLevelPrediction::plotPredictedPDF(plpResult$performanceEvaluation, type='train')
  testCalPlot <- testCalPlot + ggplot2::labs(title=paste("Test"))
  trainCalPlot <- trainCalPlot + ggplot2::labs(title=paste("Train"))
  doc <- doc %>% officer::body_add_gg(value = testCalPlot)
  doc <- doc %>% officer::body_add_gg(value = trainCalPlot)
  
  
  doc <- doc %>% officer::body_add_par(value =  'Predicted Distribution Plots:', style = "heading 3")
  
  doc <- doc %>% officer::body_add_par(value =  paste("Box plots summarising the predicted risk distributions for the outcome and non-outcome people:"))
  
  testCalPlot <- PatientLevelPrediction::plotPredictionDistribution(plpResult$performanceEvaluation, type='test')
  trainCalPlot <- PatientLevelPrediction::plotPredictionDistribution(plpResult$performanceEvaluation, type='train')
  testCalPlot <- testCalPlot + ggplot2::labs(title=paste("Test"))
  trainCalPlot <- trainCalPlot + ggplot2::labs(title=paste("Train"))
  doc <- doc %>% officer::body_add_gg(value = testCalPlot)
  doc <- doc %>% officer::body_add_gg(value = trainCalPlot)
  
  doc <- doc %>% officer::body_add_par(value =  'Precision Recall Plots:', style = "heading 3")
  
  doc <- doc %>% officer::body_add_par(value =   paste("Precision vs recall plots:"))
  
  testCalPlot <- PatientLevelPrediction::plotPrecisionRecall(plpResult$performanceEvaluation, type='test')
  trainCalPlot <- PatientLevelPrediction::plotPrecisionRecall(plpResult$performanceEvaluation, type='train')
  testCalPlot <- testCalPlot + ggplot2::labs(title=paste("Test"))
  trainCalPlot <- trainCalPlot + ggplot2::labs(title=paste("Train"))
  doc <- doc %>% officer::body_add_gg(value = testCalPlot)
  doc <- doc %>% officer::body_add_gg(value = trainCalPlot)
  
  doc <- doc %>% officer::body_add_par(value =  'F1 Measure Plots:', style = "heading 3")
  
  doc <- doc %>% officer::body_add_par(value =   paste("A measure combining sensitivity and specificity at each prediction threshold:"))
  
  testCalPlot <- PatientLevelPrediction::plotF1Measure(plpResult$performanceEvaluation, type='test')
  trainCalPlot <- PatientLevelPrediction::plotF1Measure(plpResult$performanceEvaluation, type='train')
  testCalPlot <- testCalPlot + ggplot2::labs(title=paste("Test"))
  trainCalPlot <- trainCalPlot + ggplot2::labs(title=paste("Train"))
  doc <- doc %>% officer::body_add_gg(value = testCalPlot)
  doc <- doc %>% officer::body_add_gg(value = trainCalPlot)
  
  #------------------------------------------------

  if(!is.null(plpValidation)){
    doc <- doc %>% officer::body_add_par(value =  'External Validation:', style = "heading 3")
    
    doc <- doc %>% officer::body_add_par(value = paste("The external validation performance is sumamried in the table below:"))
    
    exSum <-  plpValidation$summary
    exSum <- exSum[,c('Database','populationSize','outcomeCount', colnames(exSum)[grep('auc', tolower(colnames(exSum)))])]
    exSum[,4:ncol(exSum)] <- round(apply(exSum[,4:ncol(exSum)], 2, function(x) as.numeric(x)), digits = 3)
    doc <- doc %>% officer::body_add_table(value=exSum)
    
    doc <- doc %>% officer::body_add_par(value = paste("The roc plots are:"))
    
    for(i in 1:length(plpValidation$validation)){
      valPlot <- PatientLevelPrediction::plotSparseRoc(plpValidation$validation[[i]]$performanceEvaluation, type='validation')
      valPlot <- valPlot + ggplot2::labs(title=paste(names(plpValidation$validation)[i]))
      doc <- doc %>% officer::body_add_gg(value = valPlot)
    }
    
    doc <- doc %>% officer::body_add_par(value = paste("The calibration plots are:"))
    
    for(i in 1:length(plpValidation$validation)){
      valPlot <- PatientLevelPrediction::plotSparseCalibration2(plpValidation$validation[[i]]$performanceEvaluation, type='validation')
      valPlot <- valPlot + ggplot2::labs(title=paste(names(plpValidation$validation)[i]))
      doc <- doc %>% officer::body_add_gg(value = valPlot)
    }
    
    
  }
  
  

  #============ MODEL  ==========================================
  #  The non-zero covariates or variable importance
  doc <- doc %>% officer::body_add_par(value =  'Model', style = "heading 2")
  
  doc <- doc %>% officer::body_add_par(value = paste("The model covariates are listed below."))
  
  modelCov <- plpResult$covariateSummary
  modelCov$covariateValue[is.na(modelCov$covariateValue)] <- 0
  modelCov <- modelCov[modelCov$covariateValue!=0,c('covariateName','covariateValue')]
  modelCov <- modelCov[order(-abs(modelCov$covariateValue)),]
  doc <- doc %>% officer::body_add_table(value = modelCov)

  #------------------------------------------------


  # add appendix with cohort details...
  doc <- doc %>% officer::body_add_par(value =  'Appendix', style = "heading 2")
  

  doc <- doc %>% officer::body_add_par(value =  'Cohort Definitions', style = "heading 3")
  
  if(!is.null(targetDefinition)){
    doc <- doc %>% officer::body_add_par(value = paste("The target cohort definition:."))
    doc <- doc %>% officer::body_add_par(value = targetDefinition)
  }
  if(!is.null(targetDefinition)){
    doc <- doc %>% officer::body_add_par(value = paste("The outcome cohort definition:."))
    doc <- doc %>% officer::body_add_par(value = outcomeDefinition)
  }


  #======================= FINAL OUTPUT ========================
  if(save){
    # write the document to file location
    print(doc, target = file.path(outputLocation))
    return(TRUE)
  } else{
    return(doc)
  }

}


# this function plots a visulisation of the prediction problem using the outcome/target population
# and the tar information extracted from the population
plotPlpProblem <- function(plpResult){

  pdf(NULL)
  dev.control(displaylist="enable")

  minTar <- plpResult$inputSetting$populationSettings$minTimeAtRisk
  minObs <- plpResult$inputSetting$populationSettings$washoutPeriod
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
  lines(c(0,targetx-widthTargetx), c(0.5,0.5), type='l', lty = 1, col=1)
  tryCatch(
    diagram::straightarrow(from = c(targetx-widthTargetx,0.55), to = c(0,0.55), lty = 1, lcol=1)
  )

  diagram::textempty(c(0.05, 0.6), 0.10, 0.05, lab = paste0(">= ",minObs," days"), cex = 0.8)

  #diagram::straightarrow(from = c(0.2,0.5), to = c(1,0.5), lty = 3, lcol = 1)
  diagram::textrect(c(targetx,0.5), widthTargetx, 0.05,lab = paste0("Target:",target), box.col = "lightblue",
                    shadow.col = "darkblue", shadow.size = 0.005, cex = 1.2)

  lines(c(targetx+widthTargetx,1),c(0.5,0.5), type='l', lty = 3)
  tryCatch(
  diagram::straightarrow(from = c(0.95,0.5), to = c(1,0.5), lty = 3, lcol = 1)
  )
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
  execution <- as.double(plpResult$executionSummary$TotalExecutionElapsedTime, units='mins')

  result <- paste0("A ", name, " model was developed using ",testfrac*100,"% of the data for training and ",(1-testfrac)*100,"% for testing. ",
                   "Hyper-parameter training was performed using ",nfold,"-fold cross-validation on the training set. The PatientLevelPrediction R package version ",
                   rversion, " was used and the total training/valdiation time was ",format(as.double(execution), digits=3)," minutes.")
  
  return(result)

}

# helpter for formatting
formatDocNumbers <- function(x, dp=3){
  return(format(as.double(x), digits=3, nsmall=3))
}


#' createPlpJournalDocument
#'
#' @description
#' Creates a template for a prediction journal paper with the characteristics/results filled in
#' @details
#' The function creates a word document containing the analysis details, data summary and prediction model results.
#' @param plpResult                        An object of type \code{plpResult} returned by running runPlp()
#' @param plpValidation                    An object of type \code{validatePlp} returned by running externalValidatePlp()
#' @param plpData                          The plpData
#' @param targetName                       A string with the target description name
#' @param outcomeName                      A string with the outcome description name
#' @param table1                           Whether to include table1 (characteristics)
#' @param connectionDetails                The connection required to calcualte the characteristics
#' @param includeTrain                     Whether to include the train set performance
#' @param includeTest                      Whether to include the test set performance
#' @param includePredictionPicture         Whether to include a picture detailing the prediction problem
#' @param includeAttritionPlot            Whether to include the attriction plot
#' @param outputLocation                   The location to write the document to
#' @param save                            If false this fucntion returns the document and does not save to outputLocation 
#'
#' @return
#' A work document containing the selected outputs within the user's directory at location specified in outputLocation
#' @export
createPlpJournalDocument <- function(plpResult=NULL, plpValidation=NULL,
                                     plpData = NULL,
                                     targetName = '<target population>',
                                     outcomeName = '<outcome>',
                                     table1=F,
                                     connectionDetails=NULL,
                                     includeTrain=FALSE, includeTest=TRUE,
                                     includePredictionPicture=TRUE,
                                     includeAttritionPlot=TRUE,
                                     outputLocation=file.path(getwd(), 'plp_journal_document.docx'),
                                     save = T){

  if(is.null(plpResult)){
    stop('plpResult needs to be input')
  }
  if(sum('runPlp'%in%class(plpResult))==0){
    stop('Incorrect plpResult class')
  }
  if(class(targetName)!='character'){
    stop('Incorrect targetName')
  }
  if(class(outcomeName)!='character'){
    stop('Incorrect outcomeName')
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
  if(class(table1)!='logical'){
    stop('Incorrect table1 class')
  }
  if(table1){
    if(is.null(connectionDetails)){
      stop('Need to enter connection details for table 1')
    }
    if(is.null(plpData)){
      stop('Need to enter plpdata for table 1')
    }
  }

  if(!is.null(plpValidation)){
    if(class(plpValidation)!="validatePlp"){
      stop('Incorrect plpValidation')
    }
  }
  
  #!!================
  # TODO: add check to make sure the characterisation stuff is in data - otherwise add warning

  # calcualte the auc
  eval <- plpResult$performanceEvaluation$evaluationStatistics
  auc <- formatDocNumbers(eval[eval[,'Eval']=='test' & eval[,'Metric']%in%c('AUC.auc',"AUC"),'Value'])
  if(length(eval[eval[,'Eval']=='test' & eval[,'Metric']=='AUC.auc_lb95ci','Value'])>0)
    auc <- paste0(auc, '(',formatDocNumbers(eval[eval[,'Eval']=='test' & eval[,'Metric']=='AUC.auc_lb95ci','Value']),
                  '-',formatDocNumbers(eval[eval[,'Eval']=='test' & eval[,'Metric']=='AUC.auc_ub95ci','Value']),')')


  # rerun the pop
  populationSet <- plpResult$inputSetting$populationSettings
  if(is.null(plpResult$prediction)){
    populationSet$plpData <- plpData
    population <- do.call('createStudyPopulation', populationSet)
  } else{
    population <- plpResult$prediction
  }

  
  #============== STYLES =======================================================
  style_helper_text <- officer::shortcuts$fp_italic(color = "#FF8C00")
  
  # create new word document
  ###doc = ReporteRs::docx()
  doc = officer::read_docx() 
  
  doc <- doc %>% officer::set_doc_properties(title = 'Plp journal document', 
                                             subject = NULL, 
                                             creator = 'Plp OHDSI package',
                                             description = 'created using the runPlp resulting object')
  
  
  target_size <- nrow(population)
  outcome_size <- sum(population$outcomeCount==1)
  if(populationSet$addExposureDaysToEnd &
     populationSet$addExposureDaysToStart){
    
    totdays <- populationSet$riskWindowEnd-populationSet$riskWindowStart
    if(totdays%in%c(364,365)){
      time_at_risk <- '1-year after target cohort end date'
    } else if(totdays%in%c(365*2-1,365*2)){
      time_at_risk <- '2-year after target cohort end date'
    }else if(totdays%in%c(365*10-1,365*10)){
      time_at_risk <- '10-year after target cohort end date'
    } else {
    
    time_at_risk <- paste0(populationSet$riskWindowStart,
                           ifelse(populationSet$riskWindowStart==1," day", " day/s"),
                           " from target end date  to ", populationSet$riskWindowEnd,
                           " days from target end date ")
    }
  }
  if(!populationSet$addExposureDaysToEnd &
     populationSet$addExposureDaysToStart){
    time_at_risk <- paste0(populationSet$riskWindowStart,
                           ifelse(populationSet$riskWindowStart==1," day", " day/s"),
                           " from target end date  to ", populationSet$riskWindowEnd,
                           " days from target start date ")
  }
  if(populationSet$addExposureDaysToEnd &
     !populationSet$addExposureDaysToStart){
    time_at_risk <- paste0(populationSet$riskWindowStart,
                           ifelse(populationSet$riskWindowStart==1," day", " day/s"),
                           " from target start date  to ", populationSet$riskWindowEnd,
                           " days from target end date ")
  }
  if(!populationSet$addExposureDaysToEnd &
     !populationSet$addExposureDaysToStart){
    
    totdays <- populationSet$riskWindowEnd-populationSet$riskWindowStart
    if(totdays%in%c(364,365)){
      time_at_risk <- '1-year after target cohort start date'
    } else if(totdays%in%c(365*2-1, 365*2)){
      time_at_risk <- '2-year after target cohort start date'
    }else if(totdays%in%c(365*10-1, 365*10)){
      time_at_risk <- '10-year after target cohort start date'
    } else {
    time_at_risk <- paste0(populationSet$riskWindowStart,
                           ifelse(populationSet$riskWindowStart==1," day", " day/s"),
                           " from target start date to ", populationSet$riskWindowEnd,
                           " days from target start date ")
    }
  }

  #============ TITLE ==========================================
  title <- paste0("Development and validation of a multivariate model to predict ", outcomeName ,
                  " in a target population of ",targetName," during ",time_at_risk," using observational data")

  #title <- paste0('Predicting the outcome of ', outcomeName ,' in a target population of ', targetName)
  doc <- doc %>% officer::body_add_par(value = title, style = "heading 1")

  #============ ABSTRACT ==========================================
  # TRIPOD: Provide a summary of objectives, study design, setting, participants, 
  #         sample size, predictors, outcome, statistical analysis, results, and conclusions.
  abstract <- c(paste0("Objective: To develop and validate a model to predict ", outcomeName,
                       " within a target population of ", targetName,
                       " during ",time_at_risk,"."),
                
                paste0("Study design: A retrospective cohort style patient-level prediction using observational healthcare data."
                       ),

                paste0("Settings: The model was developed using <add development data set details> mapped to the Observational Medical Outcome ",
                       "Partnership (OMOP) common data model. The model was validated using <add valdiation data set details>"),
                
                paste0("Participants: <add target popualtion description>. ",target_size," people satisfied the ",
                       "target criteria and ",outcome_size," had the outcome during ",time_at_risk),
                
                paste0("Outcome: <add outcome description>."),

                paste0("Predictors: <add predictor variable summary>"),

                paste0("Statistical analysis: A", plpResult$inputSetting$modelSettings$name, " model was trained.",
                       " The model performance is evaluated using ",
                       "the area under the receiver operating characteristic (AUROC) curve ", 
                       "and inspecting the calibration plot."),
                
                paste0(" Results: The internal validation showed the model achieved an",
                       " AUC of ",auc ," and the calibration plots",
                       " indicates a <poorly/fair/well> calibrated model.  The external validation showed",
                       " the model <was/was not> transportable, with AUCs ranging between <auc range> on",
                       " the <databases> databases. "),

                paste0("Conclusions: This paper details the development and external validation of a ",
                       " model for predicting ", outcomeName, " in <target details or external target",
                       " details>.  The model can be readily implemented to any observational healthcare",
                       " database in the OMOP common data model/development code and is available from",
                       " <add weblink>.")
  )

  doc <- doc %>% officer::body_add_par(value = 'Abstract', style = "heading 2")
  for(i in 1:length(abstract)){
    doc <- doc %>% officer::body_add_par(value =  abstract[i] ) %>%
      officer::body_add_par("")
  }
  
  #============ BACKGROUND ==========================================
  doc <- doc %>% officer::body_add_par(value = 'Background', style = "heading 2") %>% 
    officer::body_add_par("")
  background <- paste0("<background on outcome: Explain the medical context and rationale for the multivariable prediction model, including references to existing models.",
                       " >")
  doc <- doc %>% officer::body_add_fpar(officer::fpar(officer::ftext(background, prop = style_helper_text))) %>% 
    officer::body_add_par("")
  background <- c(paste0("The objective of this paper is to build and validate a <diagnostic/prognostic> model to predict ",outcomeName," within ",
                         targetName," during  ",time_at_risk, ".  The model will be developed ",
                         " on <development database> and externally validated on <validation databases> to ",
                         " determine the transportability (how well it performs on different data) and ",
                         "generalizability (how well it performs on similar data) of the model when applied to ",
                         " new data.  All the datasets will be in the Observational Medical Outcome ",
                         " Partnership (OMOP) common data model as having the datasets in a homogeneous data ",
                         " structure enables re-use of code between model development and validation to ensure ",
                         " the model can be externally validated efficiently and reduce model reproducibility errors."),
                  
                  paste0("The model development and validation follow the standardized patient-level prediction framework [1]. ",
                         "This framework is based on best practices proposed by PROGRESS [2].",
                         "The TRIPOD statement [3] for reporting patient-level prediction development is followed in this paper.")
  )
  for(i in 1:length(background)){
    doc <- doc %>% officer::body_add_par(value =  background[i] ) %>% officer::body_add_par("")
  }
  #=============== METHOD: Prediction plot  ==============
  if(includePredictionPicture){
    # Pic1: add prediction plot
    predictionPlot <- plotPlpProblem(plpResult)
    ##ReporteRs::addPlot(doc, fun=print, x=predictionPlot)
    # how to add non-gg plot??
    # save predictionPlot?
    grDevices::png(filename='temp.png')
    print(predictionPlot)
    dev.off()
    doc <- doc %>% officer::body_add_img(src = 'temp.png', width = 4.5, height = 4)
    unlink('temp.png')
    
    #doc = ReporteRs::addPlot(predictionPlot)

    # Pic1: Add standardise paragraph describing prediction - use name inputs
    doc <- doc %>% officer::body_add_par(value =  'Figure 2 shows the prediction visualization...' ) %>%
      officer::body_add_par("")
    
    }



  #=============== METHOD: Analysis Information  ==============
  # Pic2: add analysis details
  ##doc = ReporteRs::addFlexTable(plpResult$model$hyperParamSearch)
  doc <- doc %>% officer::body_add_par(value = 'Method', style = "heading 2")
  
  doc <- doc %>% officer::body_add_par(value = 'Data sources:', style = "heading 3")
  
  datasources <- "<ADD DATASOURCE FOR DEVELOPMENT AND VALIDATION HERE - type of data and description>"
  doc <- doc %>% officer::body_add_fpar(officer::fpar(officer::ftext(datasources, prop = style_helper_text))) %>% 
    officer::body_add_par("")
  
  datasources <- "<Add IRB statement here>"
  doc <- doc %>% officer::body_add_fpar(officer::fpar(officer::ftext(datasources, prop = style_helper_text))) %>% 
    officer::body_add_par("")
  
  doc <- doc %>% officer::body_add_par(value = 'Target population:', style = "heading 3")
  
  doc <- doc %>% officer::body_add_fpar(officer::fpar(officer::ftext("<add target definition here>", prop = style_helper_text))) %>% 
    officer::body_add_par("")
  
  doc <- doc %>% officer::body_add_par(value = 'Outcome:', style = "heading 3")
  doc <- doc %>% officer::body_add_fpar(officer::fpar(officer::ftext("<add outcome definition here>", prop = style_helper_text))) %>% 
    officer::body_add_par("")

  doc <- doc %>% officer::body_add_par(value = 'Predictors:', style = "heading 3")
  covset <- plpResult$model$metaData$call$covariateSettings #plpResult$inputSetting$dataExtrractionSettings$covariateSettings
  if(class(plpResult$inputSetting$dataExtrractionSettings$covariateSettings)=="list"){
    covs <- unlist(covset)[grep('use',names(unlist(covset)))]
    covs <- gsub('useCovariate','',names(covs)[covs==1])
    covs <- as.data.frame(covs)
    colnames(covs) <- 'Predictor'
    timeset <- paste0("Longterm days:",covset$longTermDays, "-",
                      "Mediumterm days:",covset$mediumDays, "-",
                      "Shortterm days:",covset$shortTermDays, "-",
                      "WindowEnd days:",covset$windowEndDays)

    doc <- doc %>% officer::body_add_table(value = covs)
    doc <- doc %>% officer::body_add_par(value = timeset)
    
    doc <- doc %>% officer::body_add_fpar(officer::fpar(officer::ftext("<!Clarify about missing data>", prop = style_helper_text))) %>% 
      officer::body_add_par("")
     } else {
       if(!is.null(covset)){
         covs <- as.data.frame(unlist(covset)) #collapse covset values if vectors?
         covs <- data.frame(Covariate = row.names(covs), Value = covs)
         colnames(covs) <- c('Covariate','Value')
         #restrict to true
         covs <- covs[union(which(covs$Value!=0),grep('TermStartDays',covs$Covariate)),]
         doc <- doc %>% officer::body_add_table(value = covs)
         doc <- doc %>% officer::body_add_fpar(officer::fpar(officer::ftext("<!Clarify about missing data>", prop = style_helper_text))) %>% 
           officer::body_add_par("")
       }
     }    


  doc <- doc %>% officer::body_add_par(value = 'Statistical analysis methods', style = "heading 3")
  
  doc <- doc %>% officer::body_add_par(value =textPlpAnalysis(plpResult) ) %>% officer::body_add_par("")
  
  evaltext <- paste0("To evaluate the models the model discrimination is assessed using the area under",
                     " the receiver operating characteristic curve (AUC) and the model calibration is ",
                     "assessed by inspecting a calibration plot.")
  doc <- doc %>% officer::body_add_par(value = evaltext ) %>% officer::body_add_par("")
  


  #=============== RESULTS: attriction plot  ==============
  doc <- doc %>% officer::body_add_par(value = 'Results', style = "heading 2")
  
  doc <- doc %>% officer::body_add_par(value = 'Target population summary', 
                                       style = "heading 3")
  

  text <- paste0("The number of people eligible for inclusion into the target population, ",
                 "outcome count and the number of people lost due to each inclusion step are ",
                 "presented in Figure 1.")
  doc <- doc %>% officer::body_add_par(value = text )

  if(includeAttritionPlot){
    # Pic3: add attriction plot
    attrPlot <- PatientLevelPrediction::drawAttritionDiagramPlp(plpResult$inputSetting$populationSettings$attrition)#attr(population,'metaData')$attrition)
    #doc = ReporteRs::addPlot(attrPlot)
    doc <- doc %>% officer::body_add_gg(value = attrPlot)  # IS THIS GG??
    doc <- doc %>% officer::body_add_par(value = 'Figure 1 shows the attrition for the model development.' )
    

    # Pic3: Add comments
    doc <- doc %>% officer::body_add_fpar(officer::fpar(officer::ftext("<add comment on what the attrition table shows>", prop = style_helper_text))) %>% 
      officer::body_add_par("")
    }

  #=============== characterisation ==============

  if(table1){
    doc <- doc %>% officer::body_add_par(value = 'Characterisation' , style = 'heading 3')
    tab1 <- getPlpTable(cdmDatabaseSchema=plpData$metaData$call$cdmDatabaseSchema,
                        longTermStartDays = -9999, population=population, 
                        connectionDetails=connectionDetails,
                        cohortTable='#temp_person')

    #charactTab1 <- ReporteRs::FlexTable(tab1)
    #doc = ReporteRs::addFlexTable(doc, charactTab1)
    doc <- doc %>% officer::body_add_table(value = tab1)

    # Tab1: Add paragraph describing data
    characterisationText <- paste0('Table 1a shows the key characteristic for people with and without the outcome')

    doc <- doc %>% officer::body_add_par(value = characterisationText)
    
    doc <- doc %>% officer::body_add_fpar(officer::fpar(officer::ftext("<add comment of differences>", prop = style_helper_text))) %>% 
      officer::body_add_par("")
    }

  # Add plot of outcome vs non-outcome
  covSum <- PatientLevelPrediction::plotVariableScatterplot(plpResult$covariateSummary)
  doc <- doc %>% officer::body_add_gg(value = covSum)
  doc <- doc %>% officer::body_add_par(value = 'Figure 2 shows the scatter plot of the prevalence of each variable in the non-outcome vs outcome groups.' ) %>%
    officer::body_add_par("")
  

  text <- paste0("Table 1 presents the baseline characteristics of the development datasets and validation datasets")
  doc <- doc %>% officer::body_add_par(value = text )
  doc <- doc %>% officer::body_add_fpar(officer::fpar(officer::ftext("<add text describing key features>", prop = style_helper_text))) %>% 
    officer::body_add_par("")

  doc <- doc %>% officer::body_add_par(value = 'Model Specification', style='heading 3')
  text <- paste0("The model developed on <database> with a target size of ",nrow(population)," and outcome count ",
                 " of ",sum(population$outcomeCount>0)," is available from <add link>.  Out of ",nrow(plpResult$covariateSummary)," candidate predictors the final model",
                 " included ", sum(plpResult$covariateSummary$covariateValue!=0, na.rm = T) , ".",
                 " The <coefficients/variable importance> ",
                 "for each predictor is available as a supplement.")
  doc <- doc %>% officer::body_add_par(value = text ) %>% officer::body_add_par("")
  
  doc <- doc %>% officer::body_add_fpar(officer::fpar(officer::ftext("< add explaination of how to the use the prediction model>", prop = style_helper_text))) %>% 
    officer::body_add_par("")
  
  doc <- doc %>% officer::body_add_par(value = 'Internal model validation', style='heading 3')
  
  text <- paste0(" The discriminative performance of the model is described by the ROC curve in Figure 3. The AUC of the model was ",auc," .",
                 "The calibration of the model is presented in Figure 4.")
  
  doc <- doc %>% officer::body_add_par(value =  text )

  #=============== RESULTS: ROC plot  ==============
  # Pic4: add test/train ROC plots
  testROCPlot <- PatientLevelPrediction::plotSparseRoc(plpResult$performanceEvaluation, type='test')
  trainROCPlot <- PatientLevelPrediction::plotSparseRoc(plpResult$performanceEvaluation, type='train')
  if(includeTest){
     doc <- doc %>% officer::body_add_gg(value = testROCPlot) %>%
       officer::body_add_par(value =  'Figure 3 ROC plot for test set' )
  }
  #doc = ReporteRs::addPlot(testROCPlot)

  if(includeTrain)
     doc <- doc %>% officer::body_add_gg(value = trainROCPlot)
  #=============== RESULTS: calibration plot  ==============
  # Pic5: add test/train calibration plots
  testCalPlot <- PatientLevelPrediction::plotSparseCalibration2(plpResult$performanceEvaluation, type='test')
  trainCalPlot <- PatientLevelPrediction::plotSparseCalibration2(plpResult$performanceEvaluation, type='train')
  if(includeTest){
    doc <- doc %>% officer::body_add_gg(value = testCalPlot) %>%
      officer::body_add_par(value =  'Figure 4 calibration plot for test set' )
  }
  if(includeTrain)
    doc <- doc %>% officer::body_add_gg(value = trainCalPlot)

  
  if(!is.null(plpValidation)){
    if(length(plpValidation$summary$Database)>2){
      datasets <- paste0(paste0(plpValidation$summary$Database[-length(plpValidation$summary$Database)], collapse=', '), 
                         ' and ',
                         plpValidation$summary$Database[length(plpValidation$summary$Database)]
      )
      targetCounts <- paste0(paste0(plpValidation$summary$populationSize[-length(plpValidation$summary$populationSize)], collapse=', '), 
                             ' and ',
                             plpValidation$summary$populationSize[length(plpValidation$summary$populationSize)]
      )
      outcomeCounts <- paste0(paste0(plpValidation$summary$outcomeCount[-length(plpValidation$summary$outcomeCount)], collapse=', '), 
                              ' and ',
                              plpValidation$summary$outcomeCount[length(plpValidation$summary$outcomeCount)]
      )
 
      aucInd <- grep('auc',tolower(colnames(plpValidation$summary)))
      ciInd <- grep('95ci',tolower(colnames(plpValidation$summary)))
      aucInd <- aucInd[!aucInd%in%ciInd]
      
      plpValidation$summary[is.na(plpValidation$summary)] <- 0
      
      if(length(aucInd)>1){
        aucVals <- apply(plpValidation$summary[,aucInd],1,max)
      } else{
        aucVals <- plpValidation$summary[,aucInd]
      }
      
      aucv <- paste0(round(as.numeric(aucVals), digits=3), 
                     ' (', round(as.numeric(plpValidation$summary[,ciInd[1]]), digits=3) ,
                     '-',round(as.numeric(plpValidation$summary[,ciInd[2]]), digits = 3) , 
                     ')')
      aucs <- paste0(paste0(aucv[-length(aucv)],collapse = ', '), ' and ', aucv[length(aucv)])
      
    } else {
      datasets <- paste0(plpValidation$summary$Database, collapse=' and ')
      targetCounts <- paste0(plpValidation$summary$populationSize, collapse=' and ')
      outcomeCounts <- paste0(plpValidation$summary$outcomeCount, collapse=' and ')
      
      aucInd <- grep('auc',tolower(colnames(plpValidation$summary)))
      ciInd <- grep('95ci',tolower(colnames(plpValidation$summary)))
      aucInd <- aucInd[!aucInd%in%ciInd]
      aucv <- paste0(round(as.numeric(plpValidation$summary[,aucInd]), digits=3), 
                     ' (', round(as.numeric(plpValidation$summary[,ciInd[1]]), digits=3) ,
                     '-',round(as.numeric(plpValidation$summary[,ciInd[2]]), digits = 3) , 
                     ')')
      aucs <- paste0(aucv, collapse = ' and ')
    }
    text <- paste0(" The external validation on ",datasets," consisting of target population sizes of ",
                   targetCounts,
                   " and outcome counts of ",outcomeCounts," obtained an AUC of ",
                   aucs,
                   " respectively.  The external validation ROC and calibration plots ",
                   "can be found in Appendix B.")
    doc <- doc %>% officer::body_add_par(value = text )
    
  } else {
    text <- paste0(" The external validation on <dataset 1> consisting of a target population of ",
                   "<target count> and outcome count of <outcome count> obtained an AUC of <add auc> ",
                   "(<auc ci>).  [repeat for each dataset].  The external validation calibration plots ",
                   "can be found in Appendix B.")
    doc <- doc %>% officer::body_add_par(value = text )
  }
  

  doc <- doc %>% officer::body_add_par(value = 'Discussion', style = 'heading 2' )
  
  doc <- doc %>% officer::body_add_par(value = 'Interpretation', style = 'heading 3' )
  text <-c(
    paste0("The AUC, the discriminative ability of the model, was ",auc, ".",
           " <add statement indicating what this means e.g., the model can distinguish between people who will develop the outcome and those ",
           "who are unlike to develop the outcome and compare with existing models if possible>."),
    paste0("The results show the model is <poorly/reasonably/well> calibrated on the development dataset <but/and> ",
           "is <not/is also> well calibrated on the validation datasets.  <add statement about what calibration shows>"),
    paste0("The most predictive variables were <add interesting ones from top 20>.  The variables <add> ",
           "are known or suspected to be risk factors of <add outcome> but the model has highlighted ",
           "<add variables> as predictive but they have not been incorporated in previous models.  ",
           "These variables could be studied using conventional population level estimation to determine ",
           "whether they are causally related to the outcome."),
    paste0("< add statement about how the results compare to any existing similar models >")
  )
  for(i in 1:length(text)){
    doc <- doc %>% officer::body_add_fpar(officer::fpar(officer::ftext(text[i], prop = style_helper_text))) %>% 
      officer::body_add_par("")
  }

  doc <- doc %>% officer::body_add_par(value = 'Implications', style = 'heading 3')
  
  text <-c(
    paste0("< add statement about the clinical uses of the model and future research> "),
    paste0("Inspecting the model variable importance may help to gain new insight into the disease dynamics ",
           "into the development of <outcome>.  As the model highlighted <new predictors> as potential new ",
           "risk factors, it would be useful in future research to determine whether these variables do in fact have a ",
           "biological relationship to the outcome.")
  )
  for(i in 1:length(text)){
    doc <- doc %>% officer::body_add_fpar(officer::fpar(officer::ftext(text[i], prop = style_helper_text))) %>% 
        officer::body_add_par("")
  }
  doc <- doc %>% officer::body_add_par(value = 'Limitations', style = 'heading 3')
  
  text <- c(
    paste0("In this study we have developed a model on one <add database type> database and ",
           "externally validated it across several other <add external database types> databases to aim to determine the ",
           "generalizability of the model.  However, each database only includes a sample of the ",
           "population and may not be representative of the whole population.  "),
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
  for(i in 1:length(text)){
    doc <- doc %>% officer::body_add_par(value =  text[i] ) %>% officer::body_add_par("")
  }
  
  doc <- doc %>% officer::body_add_fpar(officer::fpar(officer::ftext("<add any other limitation of the study>", prop = style_helper_text))) %>% 
    officer::body_add_par("")
  
  doc <- doc %>% officer::body_add_par(value = 'Conclusion', style = 'heading 2')
  
  text <- paste0("In this paper we developed a model for ",outcomeName," occurring within a target ",
                 "population consisting of ",targetName," during ",time_at_risk," on <development database> and ",
                 "externally validated the model on <validation datasets>.  The discriminative ability of ",
                 "the model was ",auc," and the model was <poorly/well> calibrated. <talk about clinical usefulness>.  ",
                 "In the future it would be useful to extend the external validation across the OHDS ",
                 "network and outside the OHDSI network and also determine the clinical usefulness of the ",
                 "model by implementing it retrospectively in a new dataset.")
  doc <- doc %>% officer::body_add_par(value = text )

  doc <- doc %>% officer::body_add_par(value = 'References', style = 'heading 2')
  refs <- c(paste0('1. Reps, J.M., Schuemie, M.J., Suchard, M.A., Ryan, P.B. and Rijnbeek, P.R., 2018. Design and implementation of a standardized framework to generate and evaluate patient-level prediction models using observational healthcare data. Journal of the American Medical Informatics Association. 25(8), p.969-975.'),
            paste0('2. Steyerberg, E.W., Moons, K.G., van der Windt, D.A., Hayden, J.A., Perel, P., Schroter, S., Riley, R.D., Hemingway, H., Altman, D.G. and PROGRESS Group, 2013. Prognosis Research Strategy (PROGRESS) 3: prognostic model research. PLoS medicine, 10(2), p.e1001381.'),
            paste0('3. Collins, G.S., Reitsma, J.B., Altman, D.G. and Moons, K.G., 2015. Transparent reporting of a multivariable prediction model for individual prognosis or diagnosis (TRIPOD): the TRIPOD statement. BMC medicine, 13(1), p.1.'))
  for(i in 1:length(refs)){
    doc <- doc %>% officer::body_add_par(value =  refs[i] )
  }
  doc <- doc %>% officer::body_add_fpar(officer::fpar(officer::ftext("<add references>", prop = style_helper_text))) %>% 
    officer::body_add_par("")
  
  doc <- doc %>% officer::body_add_par(value = 'Appendix A', style = 'heading 2')
  
  doc <- doc %>% officer::body_add_fpar(officer::fpar(officer::ftext("<atlas cohorts + concept sets>", prop = style_helper_text))) %>% 
    officer::body_add_par("")
  
  doc <- doc %>% officer::body_add_par(value = 'Appendix B', style = 'heading 2')
  
  doc <- doc %>% officer::body_add_fpar(officer::fpar(officer::ftext("<Plots of external validation>", prop = style_helper_text))) %>% 
    officer::body_add_par("")
  
  if(!is.null(plpValidation)){
    doc <- doc %>% officer::body_add_par(value = paste("The roc plots are:"))
    for(i in 1:length(plpValidation$validation)){
      valPlot <- PatientLevelPrediction::plotSparseRoc(plpValidation$validation[[i]]$performanceEvaluation, type='validation')
      valPlot <- valPlot + ggplot2::labs(title=paste(names(plpValidation$validation)[i]))
      doc <- doc %>% officer::body_add_gg(value = valPlot)
    }
    
    doc <- doc %>% officer::body_add_par(value = paste("The calibration plots are:"))
    for(i in 1:length(plpValidation$validation)){
      valPlot <- PatientLevelPrediction::plotSparseCalibration2(plpValidation$validation[[i]]$performanceEvaluation, type='validation')
      valPlot <- valPlot + ggplot2::labs(title=paste(names(plpValidation$validation)[i]))
      doc <- doc %>% officer::body_add_gg(value = valPlot)
    }
    
    
  }
  
  # adding the model covaraites to the appendix
  doc <- doc %>% officer::body_add_par(value = 'Appendix C', style = 'heading 2')
  modelVar <- plpResult$model$varImp[plpResult$model$varImp$covariateValue!=0,c('covariateId','covariateName','covariateValue')]
  modelVar$covariateValue <- format(as.double(modelVar$covariateValue), nsmall = 3, digits = 3, scientific = F)
  doc <- doc %>% officer::body_add_table(value=modelVar)
  
  if(save){
    # write the document to file location
    print(doc, target = file.path(outputLocation))
    return(TRUE)
  } else {
    return(doc)
  }

}



#' Create a dataframe with the summary details of the population cohort for publications
#'
#' @details
#' This function is used to create a summary table for population to be inserted into publications
#'
#' @param cdmDatabaseSchema  The schema containing the OMOP CDM data
#' @param oracleTempSchema   The oracle schema if needed
#' @param covariateSettings  The covariateSettings if different from default
#' @param longTermStartDays  How far to look back when looking for the variables in the data
#' @param population     The population you want the summary table for
#' @param connectionDetails  The connection details used to connect to the CDM database
#' @param cohortTable     The name of the temp table that will store the popualtion cohort
#'
#' @examples
#' \dontrun{
#' getTable1 (plpData, population, connectionDetails)
#' }
#' @export
#'
getPlpTable <- function(cdmDatabaseSchema,
                        oracleTempSchema,
                        covariateSettings, longTermStartDays=-365,
                        population, connectionDetails,
                        cohortTable='#temp_person'){
  if(missing(cdmDatabaseSchema))
    stop('Need to enter cdmDatabaseSchema')
  if(missing(population))
    stop('Need to enter population')
  if(missing(connectionDetails))
    stop('Need to enter connectionDetails')

  if(class(population)!='data.frame')
    stop('wrong population class')
  if(sum(c('cohortId','subjectId','cohortStartDate')%in%colnames(population))!=3)  # need to remember column names
    stop('population missing required column')

  if(sum(population$outcomeCount>0)==0)
    stop('No outcomes')
  if(sum(population$outcomeCount==0)==0)
    stop('No non-outcomes')

  # add population to database in cohort table format
  connection <- DatabaseConnector::connect(connectionDetails)
  #insert pop table into '#temp_person'


  # create table of non-outcomes
  popCohort <- population[population$outcomeCount==0,c('cohortId','subjectId','cohortStartDate','cohortStartDate')]
  colnames(popCohort)[4] <- 'cohortEndDate'
  colnames(popCohort) <- SqlRender::camelCaseToSnakeCase(colnames(popCohort))
  DatabaseConnector::insertTable(connection = connection,
                                 tableName = cohortTable,
                                 data = popCohort,
                                 tempTable = T)

  settings <- list()
  if(!missing(covariateSettings)){
    settings$covariateSettings <- covariateSettings
  } else{
    settings$covariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T,
                                                                             useDemographicsAge = T,
                                                                             useDemographicsAgeGroup = T,
                                                                             useDemographicsRace = T,
                                                                             useDemographicsEthnicity = T,
                                                                             useConditionGroupEraLongTerm = T,
                                                                             useDrugGroupEraLongTerm = T,
                                                                             useCharlsonIndex = T,
                                                                             useChads2Vasc = T,
                                                                             useDcsi = T,
                                                                             longTermStartDays = longTermStartDays)
  }
  settings$aggregated <- T
  settings$cdmDatabaseSchema <- cdmDatabaseSchema
  if(!missing(oracleTempSchema)){settings$oracleTempSchema <-  oracleTempSchema}
  settings$cohortTable <- cohortTable
  settings$cohortId <- -1
  settings$cohortTableIsTemp <- T
  settings$connection <- connection

  covariateData1 <- do.call(FeatureExtraction::getDbCovariateData, settings)

  popCohort <- population[population$outcomeCount>0,c('cohortId','subjectId','cohortStartDate','cohortStartDate')]
  colnames(popCohort)[4] <- 'cohortEndDate'
  colnames(popCohort) <- SqlRender::camelCaseToSnakeCase(colnames(popCohort))
  DatabaseConnector::insertTable(connection = connection,
                                 tableName = cohortTable,
                                 data = popCohort,
                                 tempTable = T)
  covariateData2 <- do.call(FeatureExtraction::getDbCovariateData, settings)

  ##label, analysisId, covariateIds
  tabSpec <- FeatureExtraction::getDefaultTable1Specifications()
  tabSpec <- rbind(tabSpec, c(label='Age in years', analysisId=2, covariateIds=1002))
  tab1 <- FeatureExtraction::createTable1(covariateData1 = covariateData1,
                                          covariateData2 = covariateData2,
                                          specifications = tabSpec, output = 'two columns')

  return(tab1)
}


