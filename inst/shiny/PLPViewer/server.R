# @file server.R
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

library(shiny)
library(plotly)
library(shinycssloaders)

source("utils.R")
source("plots.R")

shiny::shinyServer(function(input, output, session) {
  session$onSessionEnded(stopApp)
  # reactive values - contains the location of the plpResult
  ##reactVars <- shiny::reactiveValues(resultLocation=NULL,
  ##                                   plpResult= NULL)
  #=============
  
  summaryData <- shiny::reactive({
    ind <- 1:nrow(allPerformance)
    if(input$devDatabase!='All'){
      ind <- intersect(ind,which(as.character(allPerformance$devDatabase)==input$devDatabase))
    }
    if(input$valDatabase!='All'){
      ind <- intersect(ind,which(as.character(allPerformance$valDatabase)==input$valDatabase))
    }
    if(input$T!='All'){
      ind <- intersect(ind,which(allPerformance$cohortName==input$T))
    }
    if(input$O!='All'){
      ind <- intersect(ind,which(allPerformance$outcomeName==input$O))
    }
    if(input$modelSettingName!='All'){
      ind <- intersect(ind,which(as.character(allPerformance$modelSettingName)==input$modelSettingName))
    }
    if(input$riskWindowStart!='All'){
      ind <- intersect(ind,which(allPerformance$riskWindowStart==input$riskWindowStart))
    }
    if(input$riskWindowEnd!='All'){
      ind <- intersect(ind,which(allPerformance$riskWindowEnd==input$riskWindowEnd))
    }
    
    ind
  })
  
  
  
  output$summaryTable <- DT::renderDataTable(DT::datatable(formatPerformance[summaryData(),!colnames(formatPerformance)%in%c('addExposureDaysToStart','addExposureDaysToEnd')],
                                                           rownames= FALSE, selection = 'single'))
  
  
  dataofint <- shiny::reactive({
    if(is.null(input$summaryTable_rows_selected[1])){
      ind <- 1
    }else{
      ind <- input$summaryTable_rows_selected[1]
    }
    
    loc <- plpResultLocation[summaryData(),][ind,]$plpResultLocation
    logLocation <- gsub('validationResult.rds','plpLog.txt',gsub('plpResult.rds','plpLog.txt', as.character(loc)))
    if(file.exists(logLocation)){
      txt <- readLines(logLocation)
    } else{
      txt <- 'log not available'
    }
    
    covariates <- NULL
    population <- NULL
    modelset <- NULL
    
    if(file.exists(as.character(loc))){
      eval <- readRDS(as.character(loc))
      # rounding values to 2dp
      for(coln in c('covariateValue','CovariateMeanWithOutcome','CovariateMeanWithNoOutcome')){
        eval$covariateSummary[,coln] <- format(round(eval$covariateSummary[,coln], 4), nsmall = 4)
        class(eval$covariateSummary[,coln]) <- "numeric"
      }
      
    } else{
      eval <- NULL
    }
    if(length(grep('/Validation',loc))>0){
      type <- 'validation' }else{
        type <- 'test'
      }
    
    if(!is.null(eval)){
      covariates <- eval$model$metaData$call$covariateSettings
      population <- eval$model$populationSettings
      modelset <- eval$model$modelSettings
    }
    
    return(list(eval=eval, type=type, 
                logtext = txt,
                logLocation=logLocation,
                covariates = covariates,
                population = population,
                modelset = modelset))
  })
  
  plotters <- shiny::reactive({
    
    eval <- dataofint()$eval$performanceEvaluation
    if(is.null(eval)){return(NULL)}
    
    calPlot <- NULL 
    rocPlot <- NULL
    prPlot <- NULL
    f1Plot <- NULL
    demoPlot <- NULL
    boxPlot <- NULL
    distPlot <- NULL
    txt <- 'Empty'
    predictionText <- c()
    
    if(!is.null(eval)){
      intPlot <- plotShiny(eval, input$slider1)
      rocPlot <- intPlot$roc
      prPlot <- intPlot$pr
      f1Plot <- intPlot$f1score
      threshold <- intPlot$threshold
      prefthreshold <- intPlot$prefthreshold
      TP <- intPlot$TP
      FP <- intPlot$FP
      TN <- intPlot$TN
      FN <- intPlot$FN
      prefdistPlot <- plotPreferencePDF(eval, type=dataofint()$type )
      prefdistPlot <- prefdistPlot + ggplot2::geom_vline(xintercept=prefthreshold)
      preddistPlot <- plotPredictedPDF(eval, type=dataofint()$type )
      preddistPlot <- preddistPlot + ggplot2::geom_vline(xintercept=threshold)
      boxPlot <-  plotPredictionDistribution(eval, type=dataofint()$type )
      
      calPlot <- plotSparseCalibration2(eval, type=dataofint()$type )
      demoPlot <- tryCatch(plotDemographicSummary(eval, type=dataofint()$type ),
                           error= function(cond){return(NULL)})
      
      if(is.null(input$summaryTable_rows_selected[1])){
        ind <- 1
      }else{
        ind <- input$summaryTable_rows_selected[1]
      }
      predictionText <- paste0('Within ', formatPerformance[summaryData(),'T'][ind],
                               ' predict who will develop ', formatPerformance[summaryData(),'O'][ind],
                               ' during ', formatPerformance[summaryData(),'TAR start'][ind], ' day/s',
                               ' after ', ifelse(formatPerformance[summaryData(),'addExposureDaysToStart'][ind]==0, ' cohort start ', ' cohort end '),
                               ' and ', formatPerformance[summaryData(),'TAR end'][ind], ' day/s',
                               ' after ', ifelse(formatPerformance[summaryData(),'addExposureDaysToEnd'][ind]==0, ' cohort start ', ' cohort end '))
      
    }
    
    twobytwo <- as.data.frame(matrix(c(FP,TP,TN,FN), byrow=T, ncol=2))
    colnames(twobytwo) <- c('Ground Truth Negative','Ground Truth Positive')
    rownames(twobytwo) <- c('Predicted Positive','Predicted Negative')
    
    performance <- data.frame(Incidence = (TP+FN)/(TP+TN+FP+FN),
                              Threshold = threshold,
                              Sensitivity = TP/(TP+FN),
                              Specificity = TN/(TN+FP),
                              PPV = TP/(TP+FP),
                              NPV = TN/(TN+FN))
    
    list(rocPlot= rocPlot, calPlot=calPlot, 
         prPlot=prPlot, f1Plot=f1Plot, 
         demoPlot=demoPlot, boxPlot=boxPlot,
         prefdistPlot=prefdistPlot,
         preddistPlot=preddistPlot, predictionText=predictionText,
         threshold = format(threshold, digits=5), 
         twobytwo=twobytwo,
         performance = performance )
  })
  
  output$performance <- shiny::renderTable(plotters()$performance, 
                                           rownames = F, digits = 3)
  output$twobytwo <- shiny::renderTable(plotters()$twobytwo, 
                                        rownames = T, digits = 0)
  
  # input tables
  output$modelTable <- DT::renderDataTable(formatModSettings(dataofint()$modelset))
  output$covariateTable <- DT::renderDataTable(formatCovSettings(dataofint()$covariates))
  output$populationTable <- DT::renderDataTable(formatPopSettings(dataofint()$population))
  
  
  output$info <- shiny::renderText(plotters()$predictionText)
  output$log <- shiny::renderText( paste(dataofint()$logtext, collapse="\n") )
  output$threshold <- shiny::renderText(plotters()$threshold)
  
  output$roc <- plotly::renderPlotly({
    plotters()$rocPlot
  })
  output$cal <- shiny::renderPlot({
    plotters()$calPlot
  })
  output$pr <- plotly::renderPlotly({
    plotters()$prPlot
  })
  output$f1 <- plotly::renderPlotly({
    plotters()$f1Plot
  })
  output$demo <- shiny::renderPlot({
    plotters()$demoPlot
  })
  output$box <- shiny::renderPlot({
    plotters()$boxPlot
  })
  output$preddist <- shiny::renderPlot({
    plotters()$preddistPlot
  })
  output$prefdist <- shiny::renderPlot({
    plotters()$prefdistPlot
  })
  
  
  covs <- shiny::reactive({
    if(is.null(dataofint()$eval))
      return(NULL)
    plotCovariateSummary(dataofint()$eval$covariateSummary)
  })
  
  output$covariateSummaryBinary <- plotly::renderPlotly({ covs()$binary })
  output$covariateSummaryMeasure <- plotly::renderPlotly({ covs()$meas })
  
  
  output$modelView <- DT::renderDataTable(dataofint()$eval$covariateSummary[,c('covariateName','covariateValue','CovariateMeanWithOutcome','CovariateMeanWithNoOutcome' )],
                                          colnames = c('Covariate Name', 'Value', 'Outcome Mean', 'Non-outcome Mean'))
  
  
  # dashboard
  
  output$performanceBoxIncidence <- renderInfoBox({
    infoBox(
      "Incidence", paste0(round(plotters()$performance$Incidence*100, digits=3),'%'), icon = icon("ambulance"),
      color = "green"
    )
  })
  
  output$performanceBoxThreshold <- renderInfoBox({
    infoBox(
      "Threshold", format((plotters()$performance$Threshold), scientific = F, digits=3), icon = icon("edit"),
      color = "yellow"
    )
  })
  
  output$performanceBoxPPV <- renderInfoBox({
    infoBox(
      "PPV", paste0(round(plotters()$performance$PPV*1000)/10, "%"), icon = icon("thumbs-up"),
      color = "orange"
    )
  })
  
  output$performanceBoxSpecificity <- renderInfoBox({
    infoBox(
      "Specificity", paste0(round(plotters()$performance$Specificity*1000)/10, "%"), icon = icon("bullseye"),
      color = "purple"
    )
  })
  
  output$performanceBoxSensitivity <- renderInfoBox({
    infoBox(
      "Sensitivity", paste0(round(plotters()$performance$Sensitivity*1000)/10, "%"), icon = icon("low-vision"),
      color = "blue"
    )
  })
  
  output$performanceBoxNPV <- renderInfoBox({
    infoBox(
      "NPV", paste0(round(plotters()$performance$NPV*1000)/10, "%"), icon = icon("minus-square"),
      color = "black"
    )
  })
  
  
  
  # Downloadable csv of model ----
  output$downloadData <- downloadHandler(
    filename = function(){'model.csv'},
    content = function(file) {
      write.csv(dataofint()$eval$covariateSummary[dataofint()$eval$covariateSummary$covariateValue!=0,c('covariateName','covariateValue','CovariateMeanWithOutcome','CovariateMeanWithNoOutcome' )]
                , file, row.names = FALSE)
    }
  )
  
  
  
  
  #=============  
  
})