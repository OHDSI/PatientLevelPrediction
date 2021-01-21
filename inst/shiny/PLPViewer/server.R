# @file server.R
#
# Copyright 2020 Observational Health Data Sciences and Informatics
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

source("helpers.R")
source("plots.R")

server <- shiny::shinyServer(function(input, output, session) {
  session$onSessionEnded(shiny::stopApp)
  filterIndex <- shiny::reactive({getFilter(summaryTable,input)})
  
  #print(summaryTable)
  
  # need to remove over columns:
  output$summaryTable <- DT::renderDataTable(DT::datatable(summaryTable[filterIndex(),!colnames(summaryTable)%in%c('Analysis','addExposureDaysToStart','addExposureDaysToEnd', 'plpResultLocation', 'plpResultLoad')],
                                                           rownames= FALSE, selection = 'single',
                                             extensions = 'Buttons', options = list(
                                               dom = 'Blfrtip' , 
                                               buttons = c(I('colvis'), 'copy', 'excel', 'pdf' ) 
                                               #pageLength = 100, lengthMenu=c(10, 50, 100,200)
                                             ),
                                             
                                             container = htmltools::withTags(table(
                                               class = 'display',
                                               thead(
                                                 #tags$th(title=active_columns[i], colnames(data)[i])
                                                 tr(apply(data.frame(colnames=c('Dev', 'Val', 'T','O', 'Model','Covariate setting',
                                                                                'TAR', 'AUC', 'AUPRC', 
                                                                                'T Size', 'O Count', 'O Incidence (%)'), 
                                                                     labels=c('Database used to develop the model', 'Database used to evaluate model', 'Target population - the patients you want to predict risk for','Outcome - what you want to predict', 
                                                                     'Model type','Id for the covariate/settings used','Time-at-risk period', 'Area under the reciever operating characteristics (test or validation)', 'Area under the precision recall curve (test or validation)',
                                                                     'Target population size of test or validation set', 'Outcome count in test or validation set', 'Percentage of target population that have outcome during time-at-risk')), 1,
                                                          function(x) th(title=x[2], x[1])))
                                               )
                                             ))
                                                          
                                             )
  )
                                             
  
  plpResult <- shiny::reactive({getPlpResult(result,validation,summaryTable, inputType,trueRow())})
  
  # covariate table
  output$modelView <- DT::renderDataTable(editCovariates(plpResult()$covariateSummary)$table,  
                                          colnames = editCovariates(plpResult()$covariateSummary)$colnames)
  
  
  output$modelCovariateInfo <- DT::renderDataTable(data.frame(covariates = nrow(plpResult()$covariateSummary),
                                                              nonZeroCount = sum(plpResult()$covariateSummary$covariateValue!=0)))
  # Download plpresult
  output$plpResult <- shiny::downloadHandler(
    filename = function(){
      "plpResult.rds"
    },
    content = function(file) {
      saveRDS(plpResult(), file)
    }
  )
  
  # Downloadable csv of model ----
  output$downloadData <- shiny::downloadHandler(
    filename = function(){'model.csv'},
    content = function(file) {
      write.csv(plpResult()$covariateSummary[,c('covariateName','covariateValue','CovariateCount','CovariateMeanWithOutcome','CovariateMeanWithNoOutcome' )]
                , file, row.names = FALSE)
    }
  )
  
  # input tables
  output$modelTable <- DT::renderDataTable(formatModSettings(plpResult()$model$modelSettings  ))
  output$covariateTable <- DT::renderDataTable(formatCovSettings(plpResult()$model$metaData$call$covariateSettings))
  output$populationTable <- DT::renderDataTable(formatPopSettings(plpResult()$model$populationSettings))
  
  
  
  
  # prediction text
  #output$info <- shiny::renderUI(shiny::HTML(paste0(shiny::strong('Model: '), summaryTable[trueRow(),'Model'], ' with covariate setting id ',summaryTable[trueRow(),'covariateSettingId'] , '<br/>',
  #                                                  shiny::strong('Question:'), ' Within ', summaryTable[trueRow(),'T'],
  #                                        ' predict who will develop ',  summaryTable[trueRow(),'O'],
  #                                        ' during ',summaryTable[trueRow(),'TAR'], '<br/>',
  #                                        ' Developed in database: ', shiny::strong(summaryTable[trueRow(),'Dev']), ' and ',
  #                                        ' validated in database:  ', shiny::strong(summaryTable[trueRow(),'Val'])
  #                                 ))
  #)
  
  output$sideSettings  <- shiny::renderTable(t(data.frame(Development = as.character(summaryTable[trueRow(),'Dev']), 
                                                        Validation = as.character(summaryTable[trueRow(),'Val']),
                                                        Model = as.character(summaryTable[trueRow(),'Model']))), rownames = T, colnames = F)
  
  output$sideSettings2  <- shiny::renderTable(t(data.frame(T = paste0(substring(as.character(summaryTable[trueRow(),'T']),0,25),'...') , 
                                                           O = paste0(substring(as.character(summaryTable[trueRow(),'O']),0,25),'...')  )), 
                                              rownames = T, colnames = F)
  
  
  # PLOTTING FUNCTION
  plotters <- shiny::reactive({
    
    eval <- plpResult()$performanceEvaluation
    if(is.null(eval)){return(NULL)}
    
    calPlot <- NULL 
    rocPlot <- NULL
    prPlot <- NULL
    f1Plot <- NULL
    
    if(!is.null(eval)){
      #intPlot <- plotShiny(eval, input$slider1) -- RMS
      intPlot <- plotShiny(eval)
      rocPlot <- intPlot$roc
      prPlot <- intPlot$pr
      f1Plot <- intPlot$f1score
      
      list(rocPlot= rocPlot,
           prPlot=prPlot, f1Plot=f1Plot)
    }
  })
  
  
  performance <- shiny::reactive({
    
    eval <- plpResult()$performanceEvaluation
    
    if(is.null(eval)){
      return(NULL)
    } else {
      intPlot <- getORC(eval, input$slider1)
      threshold <- intPlot$threshold
      prefthreshold <- intPlot$prefthreshold
      TP <- intPlot$TP
      FP <- intPlot$FP
      TN <- intPlot$TN
      FN <- intPlot$FN
    }
    
    twobytwo <- as.data.frame(matrix(c(FP,TP,TN,FN), byrow=T, ncol=2))
    colnames(twobytwo) <- c('Ground Truth Negative','Ground Truth Positive')
    rownames(twobytwo) <- c('Predicted Positive','Predicted Negative')
    
    list(threshold = threshold, 
         prefthreshold = prefthreshold,
         twobytwo = twobytwo,
         Incidence = (TP+FN)/(TP+TN+FP+FN),
         Threshold = threshold,
         Sensitivity = TP/(TP+FN),
         Specificity = TN/(TN+FP),
         PPV = TP/(TP+FP),
         NPV = TN/(TN+FN) )
  })
  
  
  # preference plot
  output$prefdist <- shiny::renderPlot({
    if(is.null(plpResult()$performanceEvaluation)){
      return(NULL)
    } else{
      plotPreferencePDF(plpResult()$performanceEvaluation) #+ 
        # ggplot2::geom_vline(xintercept=plotters()$prefthreshold) -- RMS
    }
  })
  
  output$preddist <- shiny::renderPlot({
    if(is.null(plpResult()$performanceEvaluation)){
      return(NULL)
    } else{
      plotPredictedPDF(plpResult()$performanceEvaluation) # + 
        #ggplot2::geom_vline(xintercept=plotters()$threshold) -- RMS     
    }
  })
  
  output$box <- shiny::renderPlot({
    if(is.null(plpResult()$performanceEvaluation)){
      return(NULL)
    } else{
      plotPredictionDistribution(plpResult()$performanceEvaluation)
    }
  })
  
  output$cal <- shiny::renderPlot({
    if(is.null(plpResult()$performanceEvaluation)){
      return(NULL)
    } else{
      plotSparseCalibration2(plpResult()$performanceEvaluation)
    }
  })
  
  output$demo <- shiny::renderPlot({
    if(is.null(plpResult()$performanceEvaluation)){
      return(NULL)
    } else{
      tryCatch(plotDemographicSummary(plpResult()$performanceEvaluation),
               error= function(cond){return(NULL)})
    }
  })
  
  # validation table and selection
  validationTable <- shiny::reactive(dplyr::filter(summaryTable[filterIndex(),],
                                                   Analysis == summaryTable[filterIndex(),'Analysis'][trueRow()]))
  
  output$validationTable <- DT::renderDataTable(dplyr::select(validationTable(),c(Analysis, Dev, Val, AUC)), rownames= FALSE)
  
  valFilterIndex <- shiny::reactive({getFilter(validationTable(), input)})
  valSelectedRow <- shiny::reactive({
    if(is.null(input$validationTable_rows_selected[1])){
      return(1)
    }else{
      # return(input$validationTable_rows_selected[1])
      return(input$validationTable_rows_selected)
    }
  })
  
  # plots for the validation section. todo: add the development?
  
  valResult <- shiny::reactive({
    valtemplist <- list()
    valTable <- validationTable()
    rows <- sort(valSelectedRow())
    names <- valTable[rows, "Val"]
    for (i in 1:length(rows)){
      valtemplist[[i]] <- getPlpResult(result,validation,valTable, inputType, rows[i])
    }
    list(valtemplist, names)
  })
  
  valPlots <- shiny::reactive({
    results <- valResult()
    if(is.null(results[[1]][[1]]$performanceEvaluation)){
      # list(valRocPlot= NULL, valCalPlot = NULL)
      return(NULL)
    } else{
      
      valCalPlot <- PredictionComparison::plotMultipleCal(results[[1]], names = results[[2]])
      valRocPlot <- PredictionComparison::plotMultipleRoc(results[[1]], names = results[[2]], grid = F)
      list(valRocPlot= valRocPlot, valCalPlot = valCalPlot)
      
    }
  })
  
  output$valRoc <- shiny::renderPlot({
    try(valPlots()$valRocPlot)
  })
  output$valCal <- shiny::renderPlot({
    try(valPlots()$valCalPlot)
  })
  
  
  # Do the tables and plots:
  
  output$performance <- shiny::renderTable(performance()$performance, 
                                           rownames = F, digits = 3)
  output$twobytwo <- shiny::renderTable(performance()$twobytwo, 
                                        rownames = T, digits = 0)
  
  
  output$threshold <- shiny::renderText(format(performance()$threshold,digits=5))
  
  output$roc <- plotly::renderPlotly({
    plotters()$rocPlot
  })
  
  output$pr <- plotly::renderPlotly({
    plotters()$prPlot
  })
  output$f1 <- plotly::renderPlotly({
    plotters()$f1Plot
  })
  
  
  
  
  
  
  # covariate model plots
  covs <- shiny::reactive({
    if(is.null(plpResult()$covariateSummary))
      return(NULL)
    plotCovariateSummary(formatCovariateTable(plpResult()$covariateSummary))
  })
  
  output$covariateSummaryBinary <- plotly::renderPlotly({ covs()$binary })
  output$covariateSummaryMeasure <- plotly::renderPlotly({ covs()$meas })
  
  # LOG
  output$log <- shiny::renderText( paste(plpResult()$log, collapse="\n") )
  
  # dashboard
  
  output$performanceBoxIncidence <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "Incidence", paste0(round(performance()$Incidence*100, digits=3),'%'), icon = shiny::icon("ambulance"),
      color = "green"
    )
  })
  
  output$performanceBoxThreshold <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "Threshold", format((performance()$Threshold), scientific = F, digits=3), icon = shiny::icon("edit"),
      color = "yellow"
    )
  })
  
  output$performanceBoxPPV <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "PPV", paste0(round(performance()$PPV*1000)/10, "%"), icon = shiny::icon("thumbs-up"),
      color = "orange"
    )
  })
  
  output$performanceBoxSpecificity <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "Specificity", paste0(round(performance()$Specificity*1000)/10, "%"), icon = shiny::icon("bullseye"),
      color = "purple"
    )
  })
  
  output$performanceBoxSensitivity <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "Sensitivity", paste0(round(performance()$Sensitivity*1000)/10, "%"), icon = shiny::icon("low-vision"),
      color = "blue"
    )
  })
  
  output$performanceBoxNPV <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "NPV", paste0(round(performance()$NPV*1000)/10, "%"), icon = shiny::icon("minus-square"),
      color = "black"
    )
  })
  
  
  # SELECTING RESULTS - for PERFORMANCE/MODEl
  ##selectedRow <- shiny::reactiveVal(value = 1)
  trueRow <- shiny::reactiveVal(value = 1)
  
  # row selection updates dropdowns
  shiny::observeEvent(input$summaryTable_rows_selected,{
    #selectedRow(input$summaryTable_rows_selected)
    trueRow(filterIndex()[input$summaryTable_rows_selected])
    shiny::updateSelectInput(session, "selectResult",
                           selected = myResultList[[trueRow()]]
                           )
  })
  
  #drop downs update row and other drop down
  sumProxy <- DT::dataTableProxy("summaryTable", session = session)

  shiny::observeEvent(input$selectResult,{
    val <- which(myResultList==input$selectResult)
    trueRow(val)
    DT::selectRows(sumProxy, which(filterIndex()==val)) # reset filter here?
  })
  

  
  # HELPER INFO
  showInfoBox <- function(title, htmlFileName) {
    shiny::showModal(shiny::modalDialog(
      title = title,
      easyClose = TRUE,
      footer = NULL,
      size = "l",
      shiny::HTML(readChar(htmlFileName, file.info(htmlFileName)$size) )
    ))
  }
  
  
  observeEvent(input$DescriptionInfo, {
    showInfoBox("Description", "html/Description.html")
  })
  observeEvent(input$SummaryInfo, {
    showInfoBox("Summary", "html/Summary.html")
  })
  observeEvent(input$PerformanceInfo, {
    showInfoBox("Performance", "html/Performance.html")
  })
  observeEvent(input$ModelInfo, {
    showInfoBox("Model", "html/Model.html")
  })
  observeEvent(input$LogInfo, {
    showInfoBox("Log", "html/Log.html")
  })
  observeEvent(input$SettingsInfo, {
    showInfoBox("Settings", "html/Settings.html")
  })
  observeEvent(input$DataInfoInfo, {
    showInfoBox("DataInfo", "html/DataInfo.html")
  })
  observeEvent(input$HelpInfo, {
    showInfoBox("HelpInfo", "html/Help.html")
  })
  
  
  observeEvent(input$rocHelp, {
    showInfoBox("ROC Help", "html/rocHelp.html")
  })
  observeEvent(input$prcHelp, {
    showInfoBox("PRC Help", "html/prcHelp.html")
  })
  observeEvent(input$f1Help, {
    showInfoBox("F1 Score Plot Help", "html/f1Help.html")
  })
  observeEvent(input$boxHelp, {
    showInfoBox("Box Plot Help", "html/boxHelp.html")
  })
  observeEvent(input$predDistHelp, {
    showInfoBox("Predicted Risk Distribution Help", "html/predDistHelp.html")
  })
  observeEvent(input$prefDistHelp, {
    showInfoBox("Preference Score Distribution Help", "html/prefDistHelp.html")
  })
  observeEvent(input$calHelp, {
    showInfoBox("Calibration Help", "html/calHelp.html")
  })
  observeEvent(input$demoHelp, {
    showInfoBox("Demographic Help", "html/demoHelp.html")
  })

  
  
})
