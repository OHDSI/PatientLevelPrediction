#' @export
viewMultiplePlp <- function(analysesLocation){
  
  allPerformance <- summaryPlpAnalyses(analysesLocation)
  plpResultLocation <- allPerformance[,c('plpResultLocation', 'plpResultLoad')]
  allPerformance$combinedModelSettingName <- paste0(allPerformance$modelSettingName,'-', allPerformance$modelSettingsId)
  formatPerformance <- allPerformance[,c('devDatabase','valDatabase','cohortName','outcomeName','combinedModelSettingName','riskWindowStart', 'riskWindowEnd', 'AUC','AUPRC', 'populationSize','outcomeCount','incidence',
                                         'addExposureDaysToStart','addExposureDaysToEnd')]
  colnames(formatPerformance) <- c('Dev', 'Val', 'T', 'O','Model', 'TAR start', 'TAR end', 'AUC','AUPRC', 'T Size','O Count','O Incidence (%)', 'addExposureDaysToStart','addExposureDaysToEnd')
  
  
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
                                                shiny::h3('Model Settings:'),
                                                DT::dataTableOutput('modelTable'),
                                                shiny::h3('Population Settings:'),
                                                DT::dataTableOutput('populationTable'),
                                                shiny::h3('Covariate Settings:'),
                                                DT::dataTableOutput('covariateTable')
                                                
                                  )
                                  
                       )),
                       shiny::tabPanel("Plots", 
                                shiny::h3('Problem:'),
                                shiny::textOutput('info'),
                                shiny::h4('ROC plot:'),
                                shiny::plotOutput('roc'),
                                shiny::h4('Calibration plot:'),
                                shiny::plotOutput('cal'),
                                shiny::h4('Precision recall plot:'),
                                shiny::plotOutput('pr'),
                                shiny::h4('F1 score plot:'),
                                shiny::plotOutput('f1'),
                                shiny::h4('Demographic plot:'),
                                shiny::plotOutput('demo'),
                                shiny::h4('Box plot:'),
                                shiny::plotOutput('box'),
                                shiny::h4('Preference score distribution:'),
                                shiny::plotOutput('dist'))
                       #, tabPanel("CovariateSummary", covSet, val-T-pop shiny::plotOutput('covSummary'))
                       
                       
                       
                       
    )
  )
  
  # Define server logic ----
  server <- function(input, output) {
    
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
    
    
    
    output$summaryTable <- DT::renderDataTable(formatPerformance[summaryData(),!colnames(formatPerformance)%in%c('addExposureDaysToStart','addExposureDaysToEnd')])
    
    plotters <- shiny::reactive({
      calPlot <- NULL 
      rocPlot <- NULL
      prPlot <- NULL
      f1Plot <- NULL
      demoPlot <- NULL
      boxPlot <- NULL
      distPlot <- NULL
      predictionText <- c()
      if(!is.null(input$summaryTable_rows_selected[1])){
        loc <- plpResultLocation[summaryData(),][input$summaryTable_rows_selected[1],]
        if(loc[2]=='loadPlpResult'){
          eval <- do.call(as.character(loc[2]), list(dirPath=as.character(loc[1])))$performanceEvaluation
          type <- 'test'
        } else {
          eval <- do.call(as.character(loc[2]), list(file=as.character(loc[1])))$performanceEvaluation
          type <- 'validation'
        }
        calPlot <- plotSparseCalibration2(eval, type=type )
        rocPlot <- plotSparseRoc(eval, type=type )
        prPlot <- plotPrecisionRecall(eval, type=type )
        f1Plot <- plotF1Measure(eval, type=type )
        demoPlot <- tryCatch(plotDemographicSummary(eval, type=type ),
                             error= function(cond){return(NULL)})
        boxPlot <-  plotPredictionDistribution(eval, type=type )
        distPlot <- plotPreferencePDF(eval, type=type )
        
        
        predictionText <- paste0('Within ', formatPerformance[summaryData(),'T'][1],
                                 ' predict who will develop ', formatPerformance[summaryData(),'O'][1],
                                 ' during ', formatPerformance[summaryData(),'TAR start'][1], ' day/s',
                                 ' after ', ifelse(formatPerformance[summaryData(),'addExposureDaysToStart'][1]==0, ' cohort start ', ' cohort end '),
                                 ' and ', formatPerformance[summaryData(),'TAR end'][1], ' day/s',
                                 ' after ', ifelse(formatPerformance[summaryData(),'addExposureDaysToEnd'][1]==0, ' cohort start ', ' cohort end '))
      }
      list(rocPlot= rocPlot, calPlot=calPlot, 
           prPlot=prPlot, f1Plot=f1Plot, 
           demoPlot=demoPlot, boxPlot=boxPlot, 
           distPlot=distPlot, predictionText=predictionText)
      #rocPlot
    })
    
   details <- shiny::reactive({
       loc <- plpResultLocation[summaryData(),][input$summaryTable_rows_selected[1],]
     if(nrow(loc)==0){return(NULL)}
     if(loc[2]=='loadPlpResult'){
       eval <- do.call(as.character(loc[2]), list(dirPath=as.character(loc[1])))
     } else {
       eval <- do.call(as.character(loc[2]), list(file=as.character(loc[1])))
     }
     covariates <- eval$inputSetting$dataExtrractionSettings$covariateSettings
     population <- eval$inputSetting$populationSettings
     covariates <- data.frame(covariateName = names(covariates), 
                              SettingValue = unlist(lapply(covariates, 
                                                   function(x) paste0(x, 
                                                                      collapse='-')))
                              )
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
     
     return(list(covariates = covariates,
                 population = population,
                 modelset = modelset))
    })
   
   output$modelTable <- DT::renderDataTable(details()$modelset)
   output$covariateTable <- DT::renderDataTable(details()$covariates)
   output$populationTable <- DT::renderDataTable(details()$population)
   
    output$info <- shiny::renderText(plotters()$predictionText)
    
    output$roc <- shiny::renderPlot({
      plotters()$rocPlot
      })
    output$cal <- shiny::renderPlot({
      plotters()$calPlot
      })
    output$pr <- shiny::renderPlot({
      plotters()$prPlot
    })
    output$f1 <- shiny::renderPlot({
      plotters()$f1Plot
    })
    output$demo <- shiny::renderPlot({
      plotters()$demoPlot
    })
    output$box <- shiny::renderPlot({
      plotters()$boxPlot
    })
    output$dist <- shiny::renderPlot({
      plotters()$distPlot
    })
  }
  
  shiny::shinyApp(ui, server)
  
  } 



getPerformance <- function(analysisLocation){
  location <- file.path(analysisLocation, 'plpResult')
  if(!file.exists(location)){
    return(NULL)
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
  res$AUC <- paste0(res$AUC.auc, ' (', res$AUC.auc_lb95ci,'-', res$AUC.auc_ub95ci,')')
  }
  
  res$plpResultLocation <- location
  res$plpResultLoad <- 'loadPlpResult'
  return(res[,c('analysisId', 'AUC', 'AUPRC', 'outcomeCount','populationSize','incidence','plpResultLocation', 'plpResultLoad')])
}

getValidationPerformance <- function(validationLocation){
    val <- readRDS(file.path(validationLocation,'validationResult.rds'))
    valPerformance <- reshape2::dcast(as.data.frame(val$performanceEvaluation$evaluationStatistics), 
                                      analysisId ~ Metric, value.var='Value')
    valPerformance$incidence <- as.double(valPerformance$outcomeCount)/as.double(valPerformance$populationSize)*100
    valPerformance[, !colnames(valPerformance)%in%c('analysisId','outcomeCount','populationSize')] <- 
      format(as.double(valPerformance[, !colnames(valPerformance)%in%c('analysisId','outcomeCount','populationSize')]), digits = 2, scientific = F) 
    
    if(sum(colnames(valPerformance)=='AUC.auc_ub95ci')>0){
      valPerformance$AUC <- paste0(valPerformance$AUC.auc, ' (', valPerformance$AUC.auc_lb95ci,'-', valPerformance$AUC.auc_ub95ci,')')
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
  
  return(allPerformance)
}