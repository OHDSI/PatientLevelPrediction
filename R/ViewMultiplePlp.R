#' open a local shiny app for viewing the result of a multiple PLP analyses
#'
#' @details
#' Opens a shiny app for viewing the results of the models from various T,O, Tar and settings
#' settings.
#' @param analysesLocation  The directory containing the results (with the analysis_x folders)
#' 
#' @export
viewMultiplePlp <- function(analysesLocation){
  ensure_installed("shiny")
  ensure_installed("DT")
  ensure_installed("htmlwidgets")
  
  allPerformance <- summaryPlpAnalyses(analysesLocation)
  plpResultLocation <- allPerformance[,c('plpResultLocation', 'plpResultLoad')]
  #allPerformance$combinedModelSettingName <- paste0(allPerformance$modelSettingName,'-', allPerformance$modelSettingsId
  formatPerformance <- allPerformance[,c('analysisId','devDatabase','valDatabase','cohortName','outcomeName','modelSettingName','riskWindowStart', 'riskWindowEnd', 'AUC','AUPRC', 'populationSize','outcomeCount','incidence',
                                         'addExposureDaysToStart','addExposureDaysToEnd')]
  colnames(formatPerformance) <- c('Analysis','Dev', 'Val', 'T', 'O','Model', 'TAR start', 'TAR end', 'AUC','AUPRC', 'T Size','O Count','O Incidence (%)', 'addExposureDaysToStart','addExposureDaysToEnd')
  
  
  #========================================
  
  # Define UI ----
  ui <- shiny::fluidPage(
    shiny::titlePanel("Multiple Patient-level Prediction Model Viewer"),
    
    shiny::tabsetPanel(type = "tabs",
                       shiny::tabPanel("Summary Table", 
                                shiny::fluidRow(
                                  shiny::column(3, 
                                                shiny::h4('Filters'),
                                                shiny::selectInput('devDatabase', 'Development Database', c('All',unique(as.character(allPerformance$devDatabase)))),
                                                shiny::selectInput('valDatabase', 'Validation Database', c('All',unique(as.character(allPerformance$valDatabase)))),
                                                shiny::selectInput('T', 'Target Cohort', c('All',unique(as.character(allPerformance$cohortName)))),
                                                shiny::selectInput('O', 'Outcome Cohort', c('All',unique(as.character(allPerformance$outcomeName)))),
                                                shiny::selectInput('riskWindowStart', 'Time-at-risk start:', c('All',unique(allPerformance$riskWindowStart))),
                                                shiny::selectInput('riskWindowEnd', 'Time-at-risk end:', c('All',unique(as.character(allPerformance$riskWindowEnd)))),
                                                shiny::selectInput('modelSettingName', 'Model:', c('All',unique(as.character(allPerformance$modelSettingName))))
                                                ),  
                                  shiny::column(8, style = "background-color:#F3FAFC;",
                                                
                                                shiny::div(DT::dataTableOutput('summaryTable'), 
                                                           style = "font-size:70%"),
                                                shiny::h3('Model Settings: ', shiny::actionLink("modelhelp", "help")),
                                                DT::dataTableOutput('modelTable'),
                                                shiny::h3('Population Settings: ', shiny::actionLink("pophelp", "help")),
                                                DT::dataTableOutput('populationTable'),
                                                shiny::h3('Covariate Settings: ', shiny::actionLink("covhelp", "help")),
                                                DT::dataTableOutput('covariateTable')
                                                
                                  )
                                  
                       )),
                       shiny::tabPanel("Performance Plots", 
                                shiny::h3('Problem:'),
                                shiny::textOutput('info'),
                                shiny::wellPanel(
                                  shiny::sliderInput("slider1", 
                                            shiny::h5("Threshold value slider: "), 
                                            min = 1, max = 100, value = 50, ticks = F),
                                shiny::tags$script(shiny::HTML("
        $(document).ready(function() {setTimeout(function() {
                                                supElement = document.getElementById('slider1').parentElement;
                                                $(supElement).find('span.irs-max, span.irs-min, span.irs-single, span.irs-from, span.irs-to').remove();
}, 50);})
                                                ")),
                                shiny::tableOutput('performance'),
                                shiny::tableOutput('twobytwo')),
                                
                                shiny::h4('ROC plot:'),
                                plotly::plotlyOutput('roc'),
                                shiny::h4('Precision recall plot:'),
                                plotly::plotlyOutput('pr'),
                                shiny::h4('F1 score plot:'),
                                plotly::plotlyOutput('f1'),
                                shiny::h4('Prediction score distribution:'),
                                shiny::plotOutput('preddist'),
                                shiny::h4('Preference score distribution:'),
                                shiny::plotOutput('prefdist'),
                                shiny::h4('Box plot:'),
                                shiny::plotOutput('box'),
                                shiny::h4('Calibration plot:'),
                                shiny::plotOutput('cal'),
                                shiny::h4('Demographic plot:'),
                                shiny::plotOutput('demo')),
                       shiny::tabPanel("Model Plot", 
                                       plotly::plotlyOutput('covariateSummaryBinary'),
                                       plotly::plotlyOutput('covariateSummaryMeasure')),
                       shiny::tabPanel("Log",
                                       shiny::verbatimTextOutput('log'))
                       #, tabPanel("CovariateSummary", covSet, val-T-pop shiny::plotOutput('covSummary'))
                       
                       
                       
                       
    )
  )
  
  # Define server logic ----
  server <- function(input, output) {
    
    # helpers
    #shiny::observeEvent(input$modelhelp, {
    # model <- dataofint()$modelset - get the model name to figure the set rd to show
    #  test <- ?PatientLevelPrediction::runPlp
    #  file.show(getRd(test))
    #})
    shiny::observeEvent(input$covhelp, {
      test <- ?FeatureExtraction::createCovariateSettings
      file.show(getRd(test))
    })
    shiny::observeEvent(input$pophelp, {
      test <- ?PatientLevelPrediction::createStudyPopulation
      file.show(getRd(test))
    })
    
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
                                                             rownames= FALSE))
    
    
    dataofint <- shiny::reactive({
      if(is.null(input$summaryTable_rows_selected[1])){
        ind <- 1
      }else{
          ind <- input$summaryTable_rows_selected[1]
      }
      
      loc <- plpResultLocation[summaryData(),][ind,]
      logLocation <- gsub('plpResult','plplog.txt', as.character(loc[1]))
      txt <- readLines(logLocation)
      
      covariates <- NULL
      population <- NULL
      modelset <- NULL
      if(loc[2]=='loadPlpResult'){
        eval <- tryCatch(do.call(as.character(loc[2]), list(dirPath=as.character(loc[1]))),
                         error = function(err) return(NULL))
        type <- 'test'
      } else {
        eval <- tryCatch(do.call(as.character(loc[2]), list(file=as.character(loc[1]))),
                         error = function(err) return(NULL))
        if(!'inputSetting'%in%names(eval)){
          eval <- eval[[1]]
        }
        type <- 'validation'
      }
      if(!is.null(eval)){
      covariates <- eval$inputSetting$dataExtrractionSettings$covariateSettings
      population <- eval$inputSetting$populationSettings
      covariates <- data.frame(covariateName = names(covariates), 
                               SettingValue = unlist(lapply(covariates, 
                                                            function(x) paste0(x, 
                                                                               collapse='-')))
      )
      population$attrition <- NULL # remove the attrition as result and not setting
      population <- data.frame(Setting = names(population), 
                               Value = unlist(lapply(population, 
                                                     function(x) paste0(x, 
                                                                        collapse='-')))
      )
      modelset <- data.frame(Setting = c('Model',names(eval$model$modelSettings[[2]])),
                             Value = c(eval$model$modelSettings[[1]], unlist(lapply(eval$model$modelSettings[[2]], 
                                                                                    function(x) paste0(x, collapse=''))))
      )
      
      row.names(covariates) <- NULL
      row.names(population) <- NULL
      row.names(modelset) <- NULL
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
        
        predictionText <- paste0('Within ', formatPerformance[summaryData(),'T'][1],
                                 ' predict who will develop ', formatPerformance[summaryData(),'O'][1],
                                 ' during ', formatPerformance[summaryData(),'TAR start'][1], ' day/s',
                                 ' after ', ifelse(formatPerformance[summaryData(),'addExposureDaysToStart'][1]==0, ' cohort start ', ' cohort end '),
                                 ' and ', formatPerformance[summaryData(),'TAR end'][1], ' day/s',
                                 ' after ', ifelse(formatPerformance[summaryData(),'addExposureDaysToEnd'][1]==0, ' cohort start ', ' cohort end '))
      
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
    
   output$modelTable <- DT::renderDataTable(dataofint()$modelset)
   output$covariateTable <- DT::renderDataTable(dataofint()$covariates)
   output$populationTable <- DT::renderDataTable(dataofint()$population)
   
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
    
  }
  
  shiny::shinyApp(ui, server)
  
  } 



getPerformance <- function(analysisLocation){
  location <- file.path(analysisLocation, 'plpResult')
  if(!file.exists(location)){
    analysisId <- strsplit(analysisLocation, '/')[[1]]
    return(data.frame(analysisId=analysisId[length(analysisId)], 
                      AUC=0.000, AUPRC=0, outcomeCount=0,
                      populationSize=0,incidence=0,plpResultLocation=location, 
                      plpResultLoad='loadPlpResult'))
  }
  res <- as.data.frame(loadPlpResult(location)$performanceEvaluation$evaluationStatistics)
  
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
  res$plpResultLoad <- 'loadPlpResult'
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

#' summarises the multiple PLP results into a dataframe
#'
#' @details
#' Loads all the study results contained in the analysesLocation and aggregates a summary of the results
#' 
#' @param analysesLocation  The directory containing the results (with the analysis_x folders)
#' 
#' @export
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
  
  writeLines(paste(colnames(covariateSummary)))
  writeLines(paste(covariateSummary[1,]))
  # remove na values 
  covariateSummary$CovariateMeanWithNoOutcome[is.na(covariateSummary$CovariateMeanWithNoOutcome)] <- 0
  covariateSummary$CovariateMeanWithOutcome[is.na(covariateSummary$CovariateMeanWithOutcome)] <- 0
  if(!'covariateValue'%in%colnames(covariateSummary)){
    covariateSummary$covariateValue <- 1
  }
  if(sum(is.na(covariateSummary$covariateValue))>0){
    covariateSummary$covariateValue[is.na(covariateSummary$covariateValue)] <- 0
  }
  
  # save dots based on coef value 
  covariateSummary$size <- abs(covariateSummary$covariateValue)
  covariateSummary$size[is.na(covariateSummary$size)] <- 4
  covariateSummary$size <- 4+4*covariateSummary$size/max(covariateSummary$size)
  
  # color based on analysis id
  covariateSummary$color <- as.factor(covariateSummary$analysisId)
  
  #covariateSummary$annotation <- sapply(covariateSummary$covariateName, getName)
  covariateSummary$annotation <- covariateSummary$covariateName
  
  
  ind <- covariateSummary$CovariateMeanWithNoOutcome <1 & covariateSummary$CovariateMeanWithOutcome < 1
  # create two plots -1 or less or g1
  binary <- plotly::plot_ly(x = covariateSummary$CovariateMeanWithNoOutcome[ind] ) %>%
    plotly::add_markers(y = covariateSummary$CovariateMeanWithOutcome[ind],
                        marker = list(size = covariateSummary$size[ind], 
                                      color=covariateSummary$color[ind]),
                        text = paste(covariateSummary$annotation[ind])) %>%
    plotly::add_trace(x= c(0,1), y = c(0,1),mode = 'lines',
                      line = list(dash = "dash"), color = I('black'),
                      type='scatter') %>%
    plotly::layout(title = 'Prevalance of baseline predictors in persons with and without outcome',
           xaxis = list(title = "Prevalance in persons without outcome"),
           yaxis = list(title = "Prevalance in persons with outcome"),
           showlegend = FALSE)
  
  if(sum(!ind)>0){
  meas <- plotly::plot_ly(x = covariateSummary$CovariateMeanWithNoOutcome[!ind] ) %>%
    plotly::add_markers(y = covariateSummary$CovariateMeanWithOutcome[!ind],
                        marker = list(size = covariateSummary$size[!ind], 
                                      color=covariateSummary$color[!ind]),
                        text = paste(covariateSummary$annotation[!ind])) %>%
    plotly::add_trace(x= c(0,1), y = c(0,1),mode = 'lines',
                      line = list(dash = "dash"), color = I('black'),
                      type='scatter') %>%
    plotly::layout(title = 'Prevalance of baseline predictors in persons with and without outcome',
                   xaxis = list(title = "Prevalance in persons without outcome"),
                   yaxis = list(title = "Prevalance in persons with outcome"),
                   showlegend = FALSE)
  } else {
    meas <- NULL
  }
  
  return(list(binary=binary,
         meas = meas))
}


getRd <- function(functionName){
  
  topic <- attr(functionName, "topic")
  type <- attr(functionName, "type")
  paths <- as.character(functionName) 
  file <- paths
  
  path <- dirname(file)
  dirpath <- dirname(path)
  pkgname <- basename(dirpath)
  RdDB <- file.path(path, pkgname)
  
  if(file.exists(paste(RdDB, "rdx", sep="."))) {
    rdo <- tools:::fetchRdDB(RdDB, basename(file))
  }
  
  temp <- tools::Rd2txt(rdo, out=paste0(tempfile("Rtxt"),'.txt'), package=pkgname)
  
  return(temp)
}