#' open a local shiny app for viewing the result of a multiple PLP analyses
#'
#' @details
#' Opens a shiny app for viewing the results of the models from various T,O, Tar and settings
#' settings.
#' @param analysesLocation  The directory containing the results (with the analysis_x folders)
#' 
#' @export
viewMultiplePlp <- function(analysesLocation){
  viewPlps(result = analysesLocation, validation=NULL)
}

#' viewPlp - Interactively view the performance and model settings
#'
#' @description
#' This is a shiny app for viewing interactive plots of the performance and the settings
#' @details
#' Either the result of runPlp and view the plots
#' @param runPlp             The output of runPlp() (an object of class 'runPlp')
#' @param validatePlp  The output of externalValidatePlp (on object of class 'validatePlp')
#' @return
#' Opens a shiny app for interactively viewing the results
#'
#' @export

viewPlp <- function(runPlp, validatePlp = NULL) {
  viewPlps(result = runPlp, validation=validatePlp)
}



# code for multiple and single together
# one shiny app 

viewPlps <- function(result, validation=NULL){
  ensure_installed("shiny")
  ensure_installed("shinydashboard")
  ensure_installed("shinycssloaders")
  ensure_installed("DT")
  ensure_installed("htmlwidgets")
  
  inputType <- checkPlpInput(result) # this function checks 
  if(!class(validation)%in%c('NULL', 'validatePlp')){
    stop('Incorrect validation class')
  }
  if(inputType == 'file' & !is.null(validation)){
    warning('Validation input ignored when result is a directory location')
  }
  
  summaryTable <- getSummary(result, inputType, validation)
  
  
  #   UI ================================
  # +++++++++++++++++++++++++++++++++++++++++
  ui <- shinydashboard::dashboardPage(skin = 'black',
                                      
                                      shinydashboard::dashboardHeader(title = "Multiple PLP Viewer", 
                                                                      
                                                                      shiny::tags$li(shiny::div(shiny::img(src = 'logo.png',
                                                                                                           title = "OHDSI PLP", height = "40px", width = "40px"),
                                                                                                style = "padding-top:0px; padding-bottom:0px;"),
                                                                                     class = "dropdown")
                                                                      
                                                                      
                                      ), 
                                      
                                      shinydashboard::dashboardSidebar(
                                        shinydashboard::sidebarMenu(
                                          shinydashboard::menuItem("Summary", tabName = "Summary", icon = shiny::icon("table")),
                                          shinydashboard::menuItem("Performance", tabName = "Performance", icon = shiny::icon("bar-chart")),
                                          shinydashboard::menuItem("Model", tabName = "Model", icon = shiny::icon("clipboard")),
                                          shinydashboard::menuItem("Log", tabName = "Log", icon = shiny::icon("list")),
                                          shinydashboard::menuItem("Help", tabName = "Help", icon = shiny::icon("info"))
                                        )
                                      ),
                                      
                                      shinydashboard::dashboardBody(
                                        shinydashboard::tabItems(
                                          
                                          # help tab
                                          shinydashboard::tabItem(tabName = "Help",
                                                                  shiny::h2("Information"),
                                                                  shiny::p("Click on a row to explore the results for that model.  When you wish to explore a different model, then select the new result row and the tabs will be updated."),
                                                                  shiny::a("Demo Video", href = 'https://youtu.be/StpV40yl1UE', target='_blank')
                                          ),
                                          
                                          # First tab content
                                          shinydashboard::tabItem(tabName = "Summary",
                                                                  
                                                                  shiny::fluidRow(
                                                                    shiny::column(2, 
                                                                                  shiny::h4('Filters'),
                                                                                  shiny::selectInput('devDatabase', 'Development Database', c('All',unique(as.character(summaryTable$Dev)))),
                                                                                  shiny::selectInput('valDatabase', 'Validation Database', c('All',unique(as.character(summaryTable$Val)))),
                                                                                  shiny::selectInput('T', 'Target Cohort', c('All',unique(as.character(summaryTable$T)))),
                                                                                  shiny::selectInput('O', 'Outcome Cohort', c('All',unique(as.character(summaryTable$O)))),
                                                                                  shiny::selectInput('riskWindowStart', 'Time-at-risk start:', c('All',unique(summaryTable$`TAR start`))),
                                                                                  shiny::selectInput('riskWindowEnd', 'Time-at-risk end:', c('All',unique(as.character(summaryTable$`TAR end`)))),
                                                                                  shiny::selectInput('modelSettingName', 'Model:', c('All',unique(as.character(summaryTable$Model))))
                                                                    ),  
                                                                    shiny::column(10, style = "background-color:#F3FAFC;",
                                                                                  
                                                                                  # do this inside tabs:
                                                                                  shiny::tabsetPanel(
                                                                                    
                                                                                    shiny::tabPanel("Results",
                                                                                                    shiny::div(DT::dataTableOutput('summaryTable'), 
                                                                                                               style = "font-size:70%")),
                                                                                    
                                                                                    shiny::tabPanel("Model Settings",
                                                                                                    shiny::h3('Model Settings: ', 
                                                                                                              shiny::a("help", href="https://ohdsi.github.io/PatientLevelPrediction/reference/index.html", target="_blank") 
                                                                                                    ),
                                                                                                    DT::dataTableOutput('modelTable')),
                                                                                    
                                                                                    shiny::tabPanel("Population Settings",
                                                                                                    shiny::h3('Population Settings: ', 
                                                                                                              shiny::a("help", href="https://ohdsi.github.io/PatientLevelPrediction/reference/createStudyPopulation.html", target="_blank") 
                                                                                                    ),
                                                                                                    DT::dataTableOutput('populationTable')),
                                                                                    
                                                                                    shiny::tabPanel("Covariate Settings",
                                                                                                    shiny::h3('Covariate Settings: ', 
                                                                                                              shiny::a("help", href="http://ohdsi.github.io/FeatureExtraction/reference/createCovariateSettings.html", target="_blank") 
                                                                                                    ),
                                                                                                    DT::dataTableOutput('covariateTable'))
                                                                                  )
                                                                                  
                                                                    )
                                                                    
                                                                  )),
                                          # second tab
                                          shinydashboard::tabItem(tabName = "Performance", 
                                                                  
                                                                  shiny::fluidRow(
                                                                    shinydashboard::tabBox(
                                                                      title = "Performance", 
                                                                      # The id lets us use input$tabset1 on the server to find the current tab
                                                                      id = "tabset1", height = "100%", width='100%',
                                                                      shiny::tabPanel("Summary", 
                                                                                      
                                                                                      shiny::fluidRow(
                                                                                        shiny::column(width = 4,
                                                                                                      shinydashboard::box(width = 12,
                                                                                                                          title = htmltools::tagList(shiny::icon("question"),"Prediction Question"), status = "info", solidHeader = TRUE,
                                                                                                                          shiny::textOutput('info')
                                                                                                      ),
                                                                                                      shinydashboard::box(width = 12,
                                                                                                                          title = htmltools::tagList(shiny::icon("gear"), "Input"), 
                                                                                                                          status = "info", solidHeader = TRUE,
                                                                                                                          shiny::splitLayout(
                                                                                                                            cellWidths = c('5%', '90%', '5%'),
                                                                                                                            shiny::h5(' '),
                                                                                                                            shiny::sliderInput("slider1", 
                                                                                                                                               shiny::h4("Threshold value slider: ", htmltools::strong(shiny::textOutput('threshold'))), 
                                                                                                                                               min = 1, max = 100, value = 50, ticks = F),
                                                                                                                            shiny::h5(' ')
                                                                                                                          ),
                                                                                                                          shiny::splitLayout(
                                                                                                                            cellWidths = c('5%', '90%', '5%'),
                                                                                                                            shiny::h5(htmltools::strong('0')),
                                                                                                                            shiny::h5(' '),
                                                                                                                            shiny::h5(htmltools::strong('1'))
                                                                                                                          ),
                                                                                                                          shiny::tags$script(shiny::HTML("
                                                                                                                                                $(document).ready(function() {setTimeout(function() {
                                                                                                                                                supElement = document.getElementById('slider1').parentElement;
                                                                                                                                                $(supElement).find('span.irs-max, span.irs-min, span.irs-single, span.irs-from, span.irs-to').remove();
                                                                                                                                                }, 50);})
                                                                                                                                                "))
                                                                                                      )
                                                                                                      
                                                                                        ),
                                                                                        
                                                                                        
                                                                                        shiny::column(width = 8,
                                                                                                      shinydashboard::box(width = 12,
                                                                                                                          title = "Dashboard",
                                                                                                                          status = "warning", solidHeader = TRUE,
                                                                                                                          shinydashboard::infoBoxOutput("performanceBoxThreshold"),
                                                                                                                          shinydashboard::infoBoxOutput("performanceBoxIncidence"),
                                                                                                                          shinydashboard::infoBoxOutput("performanceBoxPPV"),
                                                                                                                          shinydashboard::infoBoxOutput("performanceBoxSpecificity"),
                                                                                                                          shinydashboard::infoBoxOutput("performanceBoxSensitivity"),
                                                                                                                          shinydashboard::infoBoxOutput("performanceBoxNPV")
                                                                                                                          
                                                                                                      ),
                                                                                                      shinydashboard::box(width = 12,
                                                                                                                          title = "Cutoff Performance",
                                                                                                                          status = "warning", solidHeader = TRUE,
                                                                                                                          shiny::tableOutput('twobytwo')
                                                                                                                          #infoBoxOutput("performanceBox"),
                                                                                                      )
                                                                                        )
                                                                                      )
                                                                                      
                                                                                      
                                                                      ),
                                                                      shiny::tabPanel("Discrimination", 
                                                                                      
                                                                                      shiny::fluidRow(
                                                                                        shinydashboard::box( status = 'info',
                                                                                                             title = "ROC Plot", solidHeader = TRUE,
                                                                                                             shinycssloaders::withSpinner(plotly::plotlyOutput('roc'))),
                                                                                        shinydashboard::box(status = 'info',
                                                                                                            title = "Precision recall plot", solidHeader = TRUE,
                                                                                                            side = "right",
                                                                                                            shinycssloaders::withSpinner(plotly::plotlyOutput('pr')))),
                                                                                      
                                                                                      shiny::fluidRow(
                                                                                        shinydashboard::box(status = 'info',
                                                                                                            title = "F1 Score Plot", solidHeader = TRUE,
                                                                                                            shinycssloaders::withSpinner(plotly::plotlyOutput('f1'))),
                                                                                        shinydashboard::box(status = 'info',
                                                                                                            title = "Box Plot", solidHeader = TRUE,
                                                                                                            side = "right",
                                                                                                            shinycssloaders::withSpinner(shiny::plotOutput('box')))),
                                                                                      
                                                                                      shiny::fluidRow(
                                                                                        shinydashboard::box(status = 'info',
                                                                                                            title = "Prediction Score Distribution", solidHeader = TRUE,
                                                                                                            shinycssloaders::withSpinner(shiny::plotOutput('preddist'))),
                                                                                        shinydashboard::box(status = 'info',
                                                                                                            title = "Preference Score Distribution", solidHeader = TRUE,
                                                                                                            side = "right",
                                                                                                            shinycssloaders::withSpinner(shiny::plotOutput('prefdist'))))
                                                                                      
                                                                                      
                                                                      ),
                                                                      shiny::tabPanel("Calibration", 
                                                                                      shiny::fluidRow(
                                                                                        shinydashboard::box(status = 'info',
                                                                                                            title = "Calibration Plot", solidHeader = TRUE,
                                                                                                            shinycssloaders::withSpinner(shiny::plotOutput('cal'))),
                                                                                        shinydashboard::box(status = 'info',
                                                                                                            title = "Demographic Plot", solidHeader = TRUE,
                                                                                                            side = "right",
                                                                                                            shinycssloaders::withSpinner(shiny::plotOutput('demo')))
                                                                                      )
                                                                      )
                                                                    ))),
                                          
                                          # 3rd tab
                                          shinydashboard::tabItem(tabName = "Model", 
                                                                  shiny::fluidRow(
                                                                    shinydashboard::box( status = 'info',
                                                                                         title = "Binary", solidHeader = TRUE,
                                                                                         shinycssloaders::withSpinner(plotly::plotlyOutput('covariateSummaryBinary'))),
                                                                    shinydashboard::box(status = 'info',
                                                                                        title = "Measurements", solidHeader = TRUE,
                                                                                        side = "right",
                                                                                        shinycssloaders::withSpinner(plotly::plotlyOutput('covariateSummaryMeasure')))),
                                                                  
                                                                  shiny::fluidRow(width=12,
                                                                                  shinydashboard::box(status = 'info', width = 12,
                                                                                                      title = "Model Table", solidHeader = TRUE,
                                                                                                      shiny::downloadButton("downloadData", "Download Model"),
                                                                                                      DT::dataTableOutput('modelView')))
                                          ),
                                          
                                          # 4th tab
                                          shinydashboard::tabItem(tabName = "Log", 
                                                                  shiny::verbatimTextOutput('log')
                                          )
                                          
                                          
                                        )
                                      )
  )
  
  
  #   SERVER ================================
  # +++++++++++++++++++++++++++++++++++++++++
  server <- shiny::shinyServer(function(input, output, session) {
    session$onSessionEnded(shiny::stopApp)
    filterIndex <- shiny::reactive({getFilter(summaryTable,input)})
    
    # need to remove over columns:
    output$summaryTable <- DT::renderDataTable(DT::datatable(summaryTable[filterIndex(),!colnames(summaryTable)%in%c('addExposureDaysToStart','addExposureDaysToEnd', 'plpResultLocation', 'plpResultLoad')],
                                                             rownames= FALSE, selection = 'single'))
    
    selectedRow <- shiny::reactive({
      if(is.null(input$summaryTable_rows_selected[1])){
        return(1)
      }else{
        return(input$summaryTable_rows_selected[1])
      }
    })
    
    
    plpResult <- shiny::reactive({getPlpResult(result,validation,summaryTable, inputType,filterIndex(), selectedRow())})
    
    # covariate table
    output$modelView <- DT::renderDataTable(formatCovariateTable(plpResult()$covariateSummary[,c('covariateName','covariateValue','CovariateMeanWithOutcome','CovariateMeanWithNoOutcome' )]),
                                            colnames = c('Covariate Name', 'Value', 'Outcome Mean', 'Non-outcome Mean'))
    # Downloadable csv of model ----
    output$downloadData <- shiny::downloadHandler(
      filename = function(){'model.csv'},
      content = function(file) {
        write.csv(plpResult()$covariateSummary[plpResult()$covariateSummary$covariateValue!=0,c('covariateName','covariateValue','CovariateMeanWithOutcome','CovariateMeanWithNoOutcome' )]
                  , file, row.names = FALSE)
      }
    )
    
    # input tables
    output$modelTable <- DT::renderDataTable(formatModSettings(plpResult()$model$modelSettings  ))
    output$covariateTable <- DT::renderDataTable(formatCovSettings(plpResult()$model$metaData$call$covariateSettings))
    output$populationTable <- DT::renderDataTable(formatPopSettings(plpResult()$model$populationSettings))
    
    
    
    
    # prediction text
    output$info <- shiny::renderText(paste0('Within ', summaryTable[filterIndex(),'T'][selectedRow()],
                                            ' predict who will develop ',  summaryTable[filterIndex(),'O'][selectedRow()],
                                            ' during ',summaryTable[filterIndex(),'TAR start'][selectedRow()], ' day/s',
                                            ' after ', ifelse(summaryTable[filterIndex(),'addExposureDaysToStart'][selectedRow()]==0, ' cohort start ', ' cohort end '),
                                            ' and ', summaryTable[filterIndex(),'TAR end'][selectedRow()], ' day/s',
                                            ' after ', ifelse(summaryTable[filterIndex(),'addExposureDaysToEnd']==0, ' cohort start ', ' cohort end '))
    )
    
    # PLOTTING FUNCTION
    plotters <- shiny::reactive({
      
      eval <- plpResult()$performanceEvaluation
      if(is.null(eval)){return(NULL)}
      
      calPlot <- NULL 
      rocPlot <- NULL
      prPlot <- NULL
      f1Plot <- NULL
      
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
        FN <- intPlot$FN}
      
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
           threshold = threshold, 
           prefthreshold = prefthreshold,
           twobytwo=twobytwo,
           performance = performance )
    })
    
    
    # preference plot
    output$prefdist <- shiny::renderPlot({
      if(is.null(plpResult()$performanceEvaluation)){
        return(NULL)
      } else{
        plotPreferencePDF(plpResult()$performanceEvaluation, 
                          type=plpResult()$type ) + 
          ggplot2::geom_vline(xintercept=plotters()$prefthreshold)
      }
    })
    
    output$preddist <- shiny::renderPlot({
      if(is.null(plpResult()$performanceEvaluation)){
        return(NULL)
      } else{
        plotPredictedPDF(plpResult()$performanceEvaluation, 
                         type=plpResult()$type ) + 
          ggplot2::geom_vline(xintercept=plotters()$threshold)      
      }
    })
    
    output$box <- shiny::renderPlot({
      if(is.null(plpResult()$performanceEvaluation)){
        return(NULL)
      } else{
        plotPredictionDistribution(plpResult()$performanceEvaluation, type=plpResult()$type )
      }
    })
    
    output$cal <- shiny::renderPlot({
      if(is.null(plpResult()$performanceEvaluation)){
        return(NULL)
      } else{
        plotSparseCalibration2(plpResult()$performanceEvaluation, type=plpResult()$type )
      }
    })
    
    output$demo <- shiny::renderPlot({
      if(is.null(plpResult()$performanceEvaluation)){
        return(NULL)
      } else{
        tryCatch(plotDemographicSummary(plpResult()$performanceEvaluation, 
                                        type=plpResult()$type ),
                 error= function(cond){return(NULL)})
      }
    })
    
    
    
    # Do the tables and plots:
    
    output$performance <- shiny::renderTable(plotters()$performance, 
                                             rownames = F, digits = 3)
    output$twobytwo <- shiny::renderTable(plotters()$twobytwo, 
                                          rownames = T, digits = 0)
    
    
    output$threshold <- shiny::renderText(format(plotters()$threshold,digits=5))
    
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
        "Incidence", paste0(round(plotters()$performance$Incidence*100, digits=3),'%'), icon = shiny::icon("ambulance"),
        color = "green"
      )
    })
    
    output$performanceBoxThreshold <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "Threshold", format((plotters()$performance$Threshold), scientific = F, digits=3), icon = shiny::icon("edit"),
        color = "yellow"
      )
    })
    
    output$performanceBoxPPV <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "PPV", paste0(round(plotters()$performance$PPV*1000)/10, "%"), icon = shiny::icon("thumbs-up"),
        color = "orange"
      )
    })
    
    output$performanceBoxSpecificity <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "Specificity", paste0(round(plotters()$performance$Specificity*1000)/10, "%"), icon = shiny::icon("bullseye"),
        color = "purple"
      )
    })
    
    output$performanceBoxSensitivity <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "Sensitivity", paste0(round(plotters()$performance$Sensitivity*1000)/10, "%"), icon = shiny::icon("low-vision"),
        color = "blue"
      )
    })
    
    output$performanceBoxNPV <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "NPV", paste0(round(plotters()$performance$NPV*1000)/10, "%"), icon = shiny::icon("minus-square"),
        color = "black"
      )
    })
    
  })
  
  
  
  shiny::shinyApp(ui =ui, server = server)
  
  
}

# this checked whether input is valid analysis location or plpResult
checkPlpInput <- function(result){
  if(class(result)=='runPlp'){
    type <- 'plpResult'
  } else if(dir.exists(result)){
    type <- 'file'
  } else {
    stop('Incorrect class for input result')
  }
  return(type)
}


# this function finds the filter index
getFilter <- function(summaryTable,input){
  ind <- 1:nrow(summaryTable)
  if(input$devDatabase!='All'){
    ind <- intersect(ind,which(as.character(summaryTable$Dev)==input$devDatabase))
  }
  if(input$valDatabase!='All'){
    ind <- intersect(ind,which(as.character(summaryTable$Val)==input$valDatabase))
  }
  if(input$T!='All'){
    ind <- intersect(ind,which(summaryTable$T==input$T))
  }
  if(input$O!='All'){
    ind <- intersect(ind,which(summaryTable$O==input$O))
  }
  if(input$modelSettingName!='All'){
    ind <- intersect(ind,which(as.character(summaryTable$Model)==input$modelSettingName))
  }
  if(input$riskWindowStart!='All'){
    ind <- intersect(ind,which(summaryTable$`TAR start`==input$riskWindowStart))
  }
  if(input$riskWindowEnd!='All'){
    ind <- intersect(ind,which(summaryTable$`TAR end`==input$riskWindowEnd))
  }
  
  return(ind)
}



getSummary  <- function(result,inputType,validation){
  if(inputType == 'plpResult'){
    sumTab <- getSummaryFromObject(result,validation)
  } else if( inputType == 'file') {
    sumTab <- summaryPlpAnalyses(result)
  } 
  
  sumTab <- sumTab[,c('analysisId','devDatabase','valDatabase','cohortName','outcomeName','modelSettingName','riskWindowStart', 'riskWindowEnd', 'AUC','AUPRC', 'populationSize','outcomeCount','incidence',
                      'addExposureDaysToStart','addExposureDaysToEnd','plpResultLocation', 'plpResultLoad')]
  colnames(sumTab) <- c('Analysis','Dev', 'Val', 'T', 'O','Model', 'TAR start', 'TAR end', 'AUC','AUPRC', 'T Size','O Count','O Incidence (%)', 'addExposureDaysToStart','addExposureDaysToEnd', 'plpResultLocation', 'plpResultLoad')
  
  return(sumTab)
} 


getSummaryFromObject <- function(result,validation=NULL){
  
  eval <- as.data.frame(result$performanceEvaluation$evaluationStatistics)
  eval <- eval[eval$Eval %in% c('test',"validation"),]
  allRes <- data.frame(analysisId = 1,
                       devDatabase = ifelse(is.null(result$inputSetting$dataExtrractionSettings$cdmDatabaseSchema),'Missing',result$inputSetting$dataExtrractionSettings$cdmDatabaseSchema),
                       valDatabase = ifelse(is.null(result$inputSetting$dataExtrractionSettings$cdmDatabaseSchema),'Missing',result$inputSetting$dataExtrractionSettings$cdmDatabaseSchema),
                       cohortName = 'T',
                       outcomeName = 'O',
                       modelSettingName = result$model$modelSettings$model,
                       riskWindowStart = ifelse(is.null(result$model$populationSettings$riskWindowStart), 'Missing',result$model$populationSettings$riskWindowStart), 
                       riskWindowEnd = ifelse(is.null(result$model$populationSettings$riskWindowEnd), 'Missing',result$model$populationSettings$riskWindowEnd), 
                       AUC = as.double(as.character(eval$Value[eval$Metric=='AUC.auc'])),
                       AUPRC = as.double(as.character(eval$Value[eval$Metric=='AUPRC'])),
                       populationSize = as.double(as.character(eval$Value[eval$Metric=='populationSize'])),
                       outcomeCount = as.double(as.character(eval$Value[eval$Metric=='outcomeCount'])),
                       incidence = as.double(as.character(eval$Value[eval$Metric=='outcomeCount']))/as.double(as.character(eval$Value[eval$Metric=='populationSize'])),
                       addExposureDaysToStart = ifelse(is.null(result$model$populationSettings$addExposureDaysToStart),'Missing',result$model$populationSettings$addExposureDaysToStart),
                       addExposureDaysToEnd = ifelse(is.null(result$model$populationSettings$addExposureDaysToEnd), 'Missing', result$model$populationSettings$addExposureDaysToEnd),
                       plpResultLocation = 'NULL', 
                       plpResultLoad = 'NULL'
  )
  
  if(!is.null(validation)){
    for(i in 1:length(validation$validation)){
      eval <- as.data.frame(validation$validation[[i]]$performanceEvaluation$evaluationStatistics)
      tempRes <-data.frame(analysisId = 1+i,
                           devDatabase = result$inputSetting$dataExtrractionSettings$cdmDatabaseSchema,
                           valDatabase = names(validation)[i],
                           cohortName = 'T',
                           outcomeName = 'O',
                           modelSettingName = result$model$modelSettings$model,
                           riskWindowStart = result$model$populationSettings$riskWindowStart, 
                           riskWindowEnd = result$model$populationSettings$riskWindowEnd, 
                           AUC = as.double(as.character(eval$Value[eval$Metric=='AUC.auc'])),
                           AUPRC = as.double(as.character(eval$Value[eval$Metric=='AUPRC'])),
                           populationSize = as.double(as.character(eval$Value[eval$Metric=='populationSize'])),
                           outcomeCount = as.double(as.character(eval$Value[eval$Metric=='outcomeCount'])),
                           incidence = as.double(as.character(eval$Value[eval$Metric=='outcomeCount']))/as.double(as.character(eval$Value[eval$Metric=='populationSize'])),
                           addExposureDaysToStart = result$model$populationSettings$addExposureDaysToStart,
                           addExposureDaysToEnd = result$model$populationSettings$addExposureDaysToEnd,
                           plpResultLocation = 'NULL', 
                           plpResultLoad = 'NULL'
      )
      allRes <- rbind(tempRes, allRes)
    }
  }
  return(allRes)
}

getPlpResult <- function(result,validation,summaryTable, inputType,filterIndex, selectedRow){
  if(inputType == 'plpResult'){
    i <- filterIndex[selectedRow]
    if(i ==1){
      tempResult <- result
      tempResult$type <- 'test'
    }else{
      tempResult <- validation$validation[[i-1]]
      tempResult$type <- 'validation'
    }
    tempResult$log <- 'log not available'
  }else if( inputType == 'file') {
    tempResult <- NULL
    loc <- summaryTable[filterIndex,][selectedRow,]$plpResultLocation
    locLoaderFunc <- summaryTable[filterIndex,][selectedRow,]$plpResultLoad
    logLocation <- gsub('plpResult','plpLog.txt', gsub('validationResult.rds','plpLog.txt',gsub('plpResult.rds','plpLog.txt', as.character(loc))))
    if(file.exists(logLocation)){
      txt <- readLines(logLocation)
    } else{
      txt <- 'log not available'
    }
    if(file.exists(as.character(loc))){
      tempResult <- do.call(as.character(locLoaderFunc), list(as.character(loc)))
      tempResult$log <- txt
      tempResult$type <- ifelse(locLoaderFunc=='loadPlpResult','test','validation')
    }
  }else {
    stop('Incorrect class')
  }
  return(tempResult)
}


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
  covariates <- data.frame(covariateName = names(covariateSettings), 
                           SettingValue = unlist(lapply(covariateSettings, 
                                                        function(x) paste0(x, 
                                                                           collapse='-'))))
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


# format covariate summary table
formatCovariateTable <- function(covariateSummary){
  for(coln in c('covariateValue','CovariateMeanWithOutcome','CovariateMeanWithNoOutcome')){
    covariateSummary[,coln] <- format(round(covariateSummary[,coln], 4), nsmall = 4)
    class(covariateSummary[,coln]) <- "numeric"
  }
  return(covariateSummary)
}


# old functions:

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
    if(length(grep('plplog.txt', valDatabases))>0){
      valDatabases <- valDatabases[-grep('plplog.txt', valDatabases)]
    }
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
    
    # check for PLP file instead 
    locationPlp <- file.path(analysisLocation, 'plpResult')
    if(!dir.exists(locationPlp)){
      
      analysisId <- strsplit(analysisLocation, '/')[[1]]
      return(data.frame(analysisId=analysisId[length(analysisId)], 
                        AUC=0.000, AUPRC=0, outcomeCount=0,
                        populationSize=0,incidence=0,plpResultLocation=location, 
                        plpResultLoad='loadPlpResult'))
    } else {
      res <- loadPlpResult(file.path(analysisLocation,'plpResult'))
      res <- as.data.frame(res$performanceEvaluation$evaluationStatistics)
      location <- file.path(analysisLocation, 'plpResult')
      plpResultLoad <- 'loadPlpResult'
      
    }
  } else{
    # read rds here
    res <- readRDS(file.path(analysisLocation,'plpResult.rds'))
    res <- as.data.frame(res$performanceEvaluation$evaluationStatistics)
    plpResultLoad <- 'readRDS'
  }
  
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
  res$plpResultLoad <- plpResultLoad
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



#============  DYNAMIC PLOTS ======================
#++++++++++++++++++++++++++++++++++++++++++++++++++

plotShiny <- function(eval, pointOfInterest){
  
  data <- eval$thresholdSummary[eval$thresholdSummary$Eval%in%c('test','validation'),]
  # pointOfInterest # this is a threshold
  pointOfInterest <- data[pointOfInterest,]
  rocobject <- plotly::plot_ly(x = 1-c(0,data$specificity,1)) %>%
    plotly::add_lines(y = c(1,data$sensitivity,0),name = "hv", 
                      text = paste('Risk Threshold:',c(0,data$predictionThreshold,1)),
                      line = list(shape = "hv",
                                  color = 'rgb(22, 96, 167)'),
                      fill = 'tozeroy') %>%
    plotly::add_trace(x= c(0,1), y = c(0,1),mode = 'lines',
                      line = list(dash = "dash"), color = I('black'),
                      type='scatter') %>%
    plotly::add_trace(x= 1-pointOfInterest$specificity, y=pointOfInterest$sensitivity, 
                      mode = 'markers', symbols='x') %>%  # change the colour of this!
    plotly::add_lines(x=c(1-pointOfInterest$specificity, 1-pointOfInterest$specificity),
                      y = c(0,1),
                      line = list(dash ='solid',
                                  color = 'black')) %>%
    plotly::layout(title = "ROC Plot",
                   xaxis = list(title = "1-specificity"),
                   yaxis = list (title = "Sensitivity"),
                   showlegend = FALSE)
  
  popAv <- data$trueCount[1]/(data$trueCount[1] + data$falseCount[1])
  probject <- plotly::plot_ly(x = data$sensitivity) %>%
    plotly::add_lines(y = data$positivePredictiveValue, name = "hv", 
                      text = paste('Risk Threshold:',data$predictionThreshold),
                      line = list(shape = "hv",
                                  color = 'rgb(22, 96, 167)'),
                      fill = 'tozeroy') %>%
    plotly::add_trace(x= c(0,1), y = c(popAv,popAv),mode = 'lines',
                      line = list(dash = "dash"), color = I('red'),
                      type='scatter') %>%
    plotly::add_trace(x= pointOfInterest$sensitivity, y=pointOfInterest$positivePredictiveValue, 
                      mode = 'markers', symbols='x') %>%  
    plotly::add_lines(x=c(pointOfInterest$sensitivity, pointOfInterest$sensitivity),
                      y = c(0,1),
                      line = list(dash ='solid',
                                  color = 'black')) %>%
    plotly::layout(title = "PR Plot",
                   xaxis = list(title = "Recall"),
                   yaxis = list (title = "Precision"),
                   showlegend = FALSE)
  
  # add F1 score
  f1object <- plotly::plot_ly(x = data$predictionThreshold) %>%
    plotly::add_lines(y = data$f1Score, name = "hv", 
                      text = paste('Risk Threshold:',data$predictionThreshold),
                      line = list(shape = "hv",
                                  color = 'rgb(22, 96, 167)'),
                      fill = 'tozeroy') %>%
    plotly::add_trace(x= pointOfInterest$predictionThreshold, y=pointOfInterest$f1Score, 
                      mode = 'markers', symbols='x') %>%  
    plotly::add_lines(x=c(pointOfInterest$predictionThreshold, pointOfInterest$predictionThreshold),
                      y = c(0,1),
                      line = list(dash ='solid',
                                  color = 'black')) %>%
    plotly::layout(title = "F1-Score Plot",
                   xaxis = list(title = "Prediction Threshold"),
                   yaxis = list (title = "F1-Score"),
                   showlegend = FALSE)
  # create 2x2 table with TP, FP, TN, FN and threshold
  threshold <- pointOfInterest$predictionThreshold
  TP <- pointOfInterest$truePositiveCount
  TN <- pointOfInterest$trueNegativeCount
  FP <- pointOfInterest$falsePositiveCount
  FN <- pointOfInterest$falseNegativeCount
  preferenceThreshold <- pointOfInterest$preferenceThreshold
  
  return(list(roc = rocobject,
              pr = probject,
              f1score = f1object,
              threshold = threshold, prefthreshold=preferenceThreshold,
              TP = TP, TN=TN,
              FP= FP, FN=FN))
}

plotCovariateSummary <- function(covariateSummary){
  
  #writeLines(paste(colnames(covariateSummary)))
  #writeLines(paste(covariateSummary[1,]))
  # remove na values 
  covariateSummary$CovariateMeanWithNoOutcome[is.na(covariateSummary$CovariateMeanWithNoOutcome)] <- 0
  covariateSummary$CovariateMeanWithOutcome[is.na(covariateSummary$CovariateMeanWithOutcome)] <- 0
  if(!'covariateValue'%in%colnames(covariateSummary)){
    covariateSummary$covariateValue <- 1
  }
  if(sum(is.na(covariateSummary$covariateValue))>0){
    covariateSummary$covariateValue[is.na(covariateSummary$covariateValue)] <- 0
  }
  
  # SPEED EDIT remove the none model variables
  covariateSummary <- covariateSummary[covariateSummary$covariateValue!=0,]
  
  # save dots based on coef value 
  covariateSummary$size <- abs(covariateSummary$covariateValue)
  covariateSummary$size[is.na(covariateSummary$size)] <- 4
  covariateSummary$size <- 4+4*covariateSummary$size/max(covariateSummary$size)
  
  # color based on analysis id
  covariateSummary$color <- sapply(covariateSummary$covariateName, function(x) ifelse(is.na(x), '', strsplit(as.character(x), ' ')[[1]][1]))
  
  l <- list(x = 0.01, y = 1,
            font = list(
              family = "sans-serif",
              size = 10,
              color = "#000"),
            bgcolor = "#E2E2E2",
            bordercolor = "#FFFFFF",
            borderwidth = 1)
  
  #covariateSummary$annotation <- sapply(covariateSummary$covariateName, getName)
  covariateSummary$annotation <- covariateSummary$covariateName
  
  
  ind <- covariateSummary$CovariateMeanWithNoOutcome <=1 & covariateSummary$CovariateMeanWithOutcome <= 1
  # create two plots -1 or less or g1
  binary <- plotly::plot_ly(x = covariateSummary$CovariateMeanWithNoOutcome[ind],
                            #size = covariateSummary$size[ind],
                            showlegend = F) %>%
    plotly::add_markers(y = covariateSummary$CovariateMeanWithOutcome[ind],
                        color=factor(covariateSummary$color[ind]),
                        text = paste(covariateSummary$annotation[ind]),
                        showlegend = T
    ) %>%
    plotly::add_trace(x= c(0,1), y = c(0,1),mode = 'lines',
                      line = list(dash = "dash"), color = I('black'),
                      type='scatter', showlegend = FALSE) %>%
    plotly::layout(#title = 'Prevalance of baseline predictors in persons with and without outcome',
      xaxis = list(title = "Prevalance in persons without outcome",
                   range = c(0, 1)),
      yaxis = list(title = "Prevalance in persons with outcome",
                   range = c(0, 1)),
      legend = l, showlegend = T)
  
  if(sum(!ind)>0){
    maxValue <- max(c(covariateSummary$CovariateMeanWithNoOutcome[!ind],
                      covariateSummary$CovariateMeanWithOutcome[!ind]), na.rm = T)
    meas <- plotly::plot_ly(x = covariateSummary$CovariateMeanWithNoOutcome[!ind] ) %>%
      plotly::add_markers(y = covariateSummary$CovariateMeanWithOutcome[!ind],
                          text = paste(covariateSummary$annotation[!ind])) %>%
      plotly::add_trace(x= c(0,maxValue), y = c(0,maxValue),mode = 'lines',
                        line = list(dash = "dash"), color = I('black'),
                        type='scatter', showlegend = FALSE) %>%
      plotly::layout(#title = 'Prevalance of baseline predictors in persons with and without outcome',
        xaxis = list(title = "Mean in persons without outcome"),
        yaxis = list(title = "Mean in persons with outcome"),
        showlegend = FALSE)
  } else {
    meas <- NULL
  }
  
  return(list(binary=binary,
              meas = meas))
}