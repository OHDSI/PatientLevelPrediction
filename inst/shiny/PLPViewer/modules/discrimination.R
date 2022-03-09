discriminationViewer <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::div(
    
    # summary table
    shiny::fluidRow(
      shinydashboard::box(
        status = 'info', 
        width = 12,
        title = 'Summary',
        solidHeader = TRUE,
        shiny::p('Click on one of these rows to view corresponding plots:'),
        DT::dataTableOutput(ns('summaryTable'))
      )
    ),
    
    
    shiny::fluidRow(
      shinydashboard::box( 
        status = 'info',
        title = shiny::actionLink(
          ns("rocHelp"), 
          "ROC Plot", 
          icon = icon("info")
        ),
        solidHeader = TRUE,
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns('roc'))
        )
      ),
      shinydashboard::box(
        status = 'info',
        title = shiny::actionLink(
          ns("prcHelp"), 
          "Precision recall plot", 
          icon = icon("info")
        ),
        solidHeader = TRUE,
        side = "right",
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns('pr'))
        )
      )
    ),
    
    shiny::fluidRow(
      shinydashboard::box(
        status = 'info',
        title = shiny::actionLink(
          ns("f1Help"), 
          "F1 Score Plot", 
          icon = icon("info")
        ),
        solidHeader = TRUE,
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns('f1'))
        )
      ),
      shinydashboard::box(
        status = 'info',
        title = shiny::actionLink(
          ns("boxHelp"),
          "Box Plot", 
          icon = icon("info")
        ),
        solidHeader = TRUE,
        side = "right",
        shinycssloaders::withSpinner(
          shiny::plotOutput(ns('box'))
        )
      )
    ),
  
    shiny::fluidRow(
      shinydashboard::box(
        status = 'info',
        title = shiny::actionLink(
          ns("predDistHelp"),
          "Prediction Score Distribution", 
          icon = icon("info")
        ),
        solidHeader = TRUE,
        shinycssloaders::withSpinner(
          shiny::plotOutput(ns('preddist'))
        )
      ),
      shinydashboard::box(
        status = 'info',
        title = shiny::actionLink(
          ns("prefDistHelp"),
          "Preference Score Distribution", 
          icon = icon("info")
        ),
        solidHeader = TRUE,
        side = "right",
        shinycssloaders::withSpinner(
          shiny::plotOutput(ns('prefdist'))
        )
      )
    )
  )
}

discriminationServer <- function(id, plpResult) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      sumTable <- shiny::reactive({
        data <- plpResult()$performanceEvaluation$evaluationStatistics
        for(i in 1:ncol(data)){
          data[,i] <- unlist(data[,i])
        }
        data$value <- as.double(as.character(data$value))
        data$value <- format(data$value, digits = 4, scientific = F)
        ind <- data$metric %in% c('AUROC', 
                                  '95% lower AUROC',
                                  '95% upper AUROC',
                                  'AUPRC'
                                  
        )
        
        tidyr::pivot_wider(
          data = data[ind,], 
          names_from = 'metric', 
          values_from = 'value'
          )
        #reshape2::dcast(data[ind,], evaluation ~ metric, value.var = 'value')
        
      })
      
 
      output$summaryTable <- DT::renderDataTable({
        if(is.null(plpResult()$performanceEvaluation)){
          DT::datatable(NULL)
        } else{
          result <- sumTable()
          row.names(result) <- NULL
          DT::datatable(result,selection = 'single')
        }
      })
      
      
      plots <-  shiny::reactive({
        
          result <- list(roc = tryCatch({rocPlot(eval = plpResult()$performanceEvaluation)},
                                        error = function(cond){
                                          list(train = emptyPlot(title = 'No performanceEvaluation'))
                                          }),
                         pr = tryCatch({prPlot(eval = plpResult()$performanceEvaluation)},
                                       error = function(cond){
                           list(train = emptyPlot(title = 'No performanceEvaluation'))
                         }),
                         f1 = tryCatch({f1Plot(eval = plpResult()$performanceEvaluation)},
                                       error = function(cond){
                                         list(train = emptyPlot(title = 'No performanceEvaluation'))
                                       }),
                         prefpdf = tryCatch({plotPreferencePDF(plpResult()$performanceEvaluation)},
                                            error = function(cond){
                                              NULL
                                            }),
                         predpdf = tryCatch({plotPredictedPDF(plpResult()$performanceEvaluation)},
                                            error = function(cond){
                                              NULL
                                            }),
                         box = tryCatch({plotPredictionDistribution(plpResult()$performanceEvaluation)},
                                        error = function(cond){
                                          NULL
                                        })
          )
          
        return(result)
      }
      )
      
     output$roc <- plotly::renderPlotly({
       type <- trimws(sumTable()$evaluation[input$summaryTable_rows_selected])
       tryCatch({plots()$roc[[type]]}, error = function(err){emptyPlot(title = err)})
     })
      
      output$pr <- plotly::renderPlotly({
        type <- trimws(sumTable()$evaluation[input$summaryTable_rows_selected])
        tryCatch({plots()$pr[[type]]}, error = function(err){emptyPlot(title = err)})
     })
      
      output$f1 <- plotly::renderPlotly({
        type <- trimws(sumTable()$evaluation[input$summaryTable_rows_selected])
        tryCatch({plots()$f1[[type]]}, error = function(err){emptyPlot(title = err)})
      })
      
      # preference plot
      output$prefdist <- shiny::renderPlot({
        type <- trimws(sumTable()$evaluation[input$summaryTable_rows_selected])
        tryCatch({plots()$prefpdf[[type]]}, error = function(err){emptyPlot(title = err)})
      })
      
      output$preddist <- shiny::renderPlot({
        type <- trimws(sumTable()$evaluation[input$summaryTable_rows_selected])
        tryCatch({plots()$predpdf[[type]]}, error = function(err){emptyPlot(title = err)})
      })
      
      output$box <- shiny::renderPlot({
        type <- trimws(sumTable()$evaluation[input$summaryTable_rows_selected])
        tryCatch({plots()$box[[type]]}, error = function(err){emptyPlot(title = err)})
      })

      shiny::observeEvent(input$rocHelp, {
        showInfoBox("ROC Help", "html/rocHelp.html")
      })
      shiny::observeEvent(input$prcHelp, {
        showInfoBox("PRC Help", "html/prcHelp.html")
      })
      shiny::observeEvent(input$f1Help, {
        showInfoBox("F1 Score Plot Help", "html/f1Help.html")
      })
      shiny::observeEvent(input$boxHelp, {
        showInfoBox("Box Plot Help", "html/boxHelp.html")
      })
      shiny::observeEvent(input$predDistHelp, {
        showInfoBox("Predicted Risk Distribution Help", "html/predDistHelp.html")
      })
      shiny::observeEvent(input$prefDistHelp, {
        showInfoBox("Preference Score Distribution Help", "html/prefDistHelp.html")
      })
      
    
      
    }
  )
}



# pltting
rocPlot <- function(eval, type){
  
  types <- unique(eval$thresholdSummary$evaluation)
  rocobject <- list()
  length(rocobject) <- length(types)
  names(rocobject) <- types
  
  for(type in types){
    data <- eval$thresholdSummary[eval$thresholdSummary$evaluation%in%type,]

    rocobject[[type]] <- plotly::plot_ly(x = 1-c(0,data$specificity,1)) %>%
      plotly::add_lines(y = c(1,data$sensitivity,0),name = "hv", 
                        text = paste('Risk Threshold:',c(0,data$predictionThreshold,1)),
                        line = list(shape = "hv",
                                    color = 'rgb(22, 96, 167)'),
                        fill = 'tozeroy') %>%
      plotly::add_trace(x= c(0,1), y = c(0,1),mode = 'lines',
                        line = list(dash = "dash"), color = I('black'),
                        type='scatter') %>%
      plotly::layout(title = "ROC Plot",
                     xaxis = list(title = "1-specificity"),
                     yaxis = list (title = "Sensitivity"),
                     showlegend = FALSE)
  }
  return(rocobject)
}

prPlot <- function(eval, type){
  types <- unique(eval$thresholdSummary$evaluation)
  probject <- list()
  length(probject) <- length(types)
  names(probject) <- types
  
  for(type in types){
    data <- eval$thresholdSummary[eval$thresholdSummary$evaluation%in%type,]
    
    popAv <- data$trueCount[1]/(data$trueCount[1] + data$falseCount[1])
    probject[[type]]  <- plotly::plot_ly(x = data$sensitivity) %>%
      plotly::add_lines(y = data$positivePredictiveValue, name = "hv", 
                        text = paste('Risk Threshold:',data$predictionThreshold),
                        line = list(shape = "hv",
                                    color = 'rgb(22, 96, 167)'),
                        fill = 'tozeroy') %>%
      plotly::add_trace(x= c(0,1), y = c(popAv,popAv),mode = 'lines',
                        line = list(dash = "dash"), color = I('red'),
                        type='scatter') %>%
      plotly::layout(title = "PR Plot",
                     xaxis = list(title = "Recall"),
                     yaxis = list (title = "Precision"),
                     showlegend = FALSE)
 
  }
  return(probject)
}

f1Plot <- function(eval, type){
  types <- unique(eval$thresholdSummary$evaluation)
  f1object <- list()
  length(f1object) <- length(types)
  names(f1object) <- types
  
  for(type in types){
    data <- eval$thresholdSummary[eval$thresholdSummary$evaluation%in%type,]
    
    f1object[[type]]  <- plotly::plot_ly(x = data$predictionThreshold) %>%
      plotly::add_lines(y = data$f1Score, name = "hv", 
                        text = paste('Risk Threshold:',data$predictionThreshold),
                        line = list(shape = "hv",
                                    color = 'rgb(22, 96, 167)'),
                        fill = 'tozeroy') %>%
      plotly::layout(title = "F1-Score Plot",
                     xaxis = list(title = "Prediction Threshold"),
                     yaxis = list (title = "F1-Score"),
                     showlegend = FALSE)
    
  }
  return(f1object)
}






# adding plots from PLP temporarily as shiny deploy doesnt have PatientLevelPrediction

plotPredictedPDF <- function(evaluation, fileName=NULL){
  
  if(!is.null(evaluation$thresholdSummary$evaluation)){
    types <- unique(evaluation$thresholdSummary$evaluation)
  } else{
    evaluation$thresholdSummary$evaluation <- 'na'
    types <- 'na'
  }
  
  plotResult <- list()
  length(plotResult) <- length(types)
  names(plotResult) <- types
  
  for(type in types){
    
  ind <- 1:nrow(evaluation$thresholdSummary)
  if(!is.null(evaluation$thresholdSummary$evaluation)){
    ind <- evaluation$thresholdSummary$evaluation == type
  }
  
  
  x<- evaluation$thresholdSummary[ind,c('predictionThreshold','truePositiveCount','trueNegativeCount',
                                        'falsePositiveCount','falseNegativeCount')]
  x<- x[order(x$predictionThreshold,-x$truePositiveCount, -x$falsePositiveCount),]
  x$out <- c(x$truePositiveCount[-length(x$truePositiveCount)]-x$truePositiveCount[-1], x$truePositiveCount[length(x$truePositiveCount)])
  x$nout <- c(x$falsePositiveCount[-length(x$falsePositiveCount)]-x$falsePositiveCount[-1], x$falsePositiveCount[length(x$falsePositiveCount)])
  
  vals <- c()
  for(i in 1:length(x$predictionThreshold)){
    if(i!=length(x$predictionThreshold)){
      upper <- x$predictionThreshold[i+1]} else {upper <- min(x$predictionThreshold[i]+0.01,1)}
    val <- x$predictionThreshold[i]+runif(x$out[i])*(upper-x$predictionThreshold[i])
    vals <- c(val, vals)
  }
  vals[!is.na(vals)]
  
  vals2 <- c()
  for(i in 1:length(x$predictionThreshold)){
    if(i!=length(x$predictionThreshold)){
      upper <- x$predictionThreshold[i+1]} else {upper <- min(x$predictionThreshold[i]+0.01,1)}
    val2 <- x$predictionThreshold[i]+runif(x$nout[i])*(upper-x$predictionThreshold[i])
    vals2 <- c(val2, vals2)
  }
  vals2[!is.na(vals2)]
  
  x <- rbind(data.frame(variable=rep('outcome',length(vals)), value=vals),
             data.frame(variable=rep('No outcome',length(vals2)), value=vals2)
  )
  
  plot <- ggplot2::ggplot(x, ggplot2::aes(x=value,
                                          group=variable,
                                          fill=variable)) +
    ggplot2::geom_density(ggplot2::aes(x=value, fill=variable), alpha=.3) +
    ggplot2::scale_x_continuous("Prediction Threshold")+#, limits=c(0,1)) +
    ggplot2::scale_y_continuous("Density") + 
    ggplot2::guides(fill=ggplot2::guide_legend(title="Class"))
  
  plotResult[[type]] <- plot
}
return(plotResult)
}




plotPreferencePDF <- function(evaluation, fileName=NULL){
  
  if(!is.null(evaluation$thresholdSummary$evaluation)){
    types <- unique(evaluation$thresholdSummary$evaluation)
  } else{
    evaluation$thresholdSummary$evaluation <- 'na'
    types <- 'na'
  }
  
  plotResult <- list()
  length(plotResult) <- length(types)
  names(plotResult) <- types
  
  for(type in types){
  
  ind <- 1:nrow(evaluation$thresholdSummary)
  if(!is.null(evaluation$thresholdSummary$evaluation)){
    ind <- evaluation$thresholdSummary$evaluation == type
  }
  
  x <- evaluation$thresholdSummary[ind,c('preferenceThreshold','truePositiveCount','trueNegativeCount',
                                        'falsePositiveCount','falseNegativeCount')]
  x <- x[order(x$preferenceThreshold,-x$truePositiveCount, x$trueNegativeCount),]
  x$out <- c(x$truePositiveCount[-length(x$truePositiveCount)]-x$truePositiveCount[-1], x$truePositiveCount[length(x$truePositiveCount)])
  x$nout <- c(x$falsePositiveCount[-length(x$falsePositiveCount)]-x$falsePositiveCount[-1], x$falsePositiveCount[length(x$falsePositiveCount)])
  
  vals <- c()
  for(i in 1:length(x$preferenceThreshold)){
    if(i!=length(x$preferenceThreshold)){
      upper <- x$preferenceThreshold[i+1]} else {upper <- 1}
    val <- x$preferenceThreshold[i]+runif(x$out[i])*(upper-x$preferenceThreshold[i])
    vals <- c(val, vals)
  }
  vals[!is.na(vals)]
  
  vals2 <- c()
  for(i in 1:length(x$preferenceThreshold)){
    if(i!=length(x$preferenceThreshold)){
      upper <- x$preferenceThreshold[i+1]} else {upper <- 1}
    val2 <- x$preferenceThreshold[i]+runif(x$nout[i])*(upper-x$preferenceThreshold[i])
    vals2 <- c(val2, vals2)
  }
  vals2[!is.na(vals2)]
  
  x <- rbind(data.frame(variable=rep('outcome',length(vals)), value=vals),
             data.frame(variable=rep('No outcome',length(vals2)), value=vals2)
  )
  
  plot <- ggplot2::ggplot(x, ggplot2::aes(x=value,
                                          group=variable,
                                          fill=variable)) +
    ggplot2::geom_density(ggplot2::aes(x=value, fill=variable), alpha=.3) +
    ggplot2::scale_x_continuous("Preference Threshold")+#, limits=c(0,1)) +
    ggplot2::scale_y_continuous("Density") + 
    ggplot2::guides(fill=ggplot2::guide_legend(title="Class"))
  
  plotResult[[type]] <- plot
  
}

return(plotResult)
}

plotPredictionDistribution <- function(evaluation){
  
  if(!is.null(evaluation$predictionDistribution$evaluation)){
    types <- unique(evaluation$predictionDistribution$evaluation)
  } else{
    evaluation$predictionDistribution$evaluation <- 'na'
    types <- 'na'
  }
  
  plotResult <- list()
  length(plotResult) <- length(types)
  names(plotResult) <- types
  
  for(type in types){
  
  ind <- 1:nrow(evaluation$predictionDistribution)
  if(!is.null(evaluation$predictionDistribution$evaluation)){
    ind <- evaluation$predictionDistribution$evaluation == type
  }
  x<- evaluation$predictionDistribution[ind,]
  
  #(x=Class, y=predictedProbabllity sequence:  min->P05->P25->Median->P75->P95->max)
  
  non05 <- x$P05PredictedProbability[x$class==0]
  non95 <- x$P95PredictedProbability[x$class==0]
  one05 <- x$P05PredictedProbability[x$class==1]
  one95 <- x$P95PredictedProbability[x$class==1]
  
  plot <-   ggplot2::ggplot(x, 
    ggplot2::aes(
      x=as.factor(class),
      ymin=MinPredictedProbability,
      lower=P25PredictedProbability,
      middle=MedianPredictedProbability,
      upper=P75PredictedProbability, 
      ymax=MaxPredictedProbability, 
      color=as.factor(class)
    )
  ) + 
    ggplot2::coord_flip() +
    ggplot2::geom_boxplot(stat="identity")  +
    ggplot2::scale_x_discrete("Class") + 
    ggplot2::scale_y_continuous("Predicted Probability") + 
    ggplot2::theme(legend.position="none") +
    ggplot2::geom_segment(ggplot2::aes(x = 0.9, y = non05, 
                                       xend = 1.1, yend = non05), color='red') +
    ggplot2::geom_segment(ggplot2::aes(x = 0.9, y = non95, 
                                       xend = 1.1, yend = non95), color='red') +
    ggplot2::geom_segment(ggplot2::aes(x = 1.9, y = one05, 
                                       xend = 2.1, yend = one05)) +
    ggplot2::geom_segment(ggplot2::aes(x = 1.9, y = one95, 
                                       xend = 2.1, yend = one95))
  
  plotResult[[type]] <- plot
  
  }
  
 return(plotResult)
}

