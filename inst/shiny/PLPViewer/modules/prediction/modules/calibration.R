calibrationViewer <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    
    shiny::fluidRow(
      shinydashboard::box(
        status = 'info', width = 12,
        title = 'Summary',
        solidHeader = TRUE,
        shiny::p('Click on one of these rows to view corresponding plots:'),
        DT::dataTableOutput(ns('calTable')
        )
      )
    ),
    
    shiny::fluidRow(
      shinydashboard::box(
        status = 'info',
        title = shiny::actionLink(
          ns("calHelp"),
          "Calibration Plot", 
          icon = shiny::icon("info")
        ),
        solidHeader = TRUE,
        shinycssloaders::withSpinner(shiny::plotOutput(ns('cal')))
      ),
      shinydashboard::box(
        status = 'info',
        title = shiny::actionLink(
          ns("demoHelp"),
          "Demographic Plot", 
          icon = shiny::icon("info")
        ),
        solidHeader = TRUE,
        side = "right",
        shinycssloaders::withSpinner(shiny::plotOutput(ns('demo')))
      )
    )
  )
}

calibrationServer <- function(
  id, 
  resultTable, 
  rowId, 
  mySchema, 
  con,
  inputSingleView,
  myTableAppend, 
  targetDialect
  ) {
  shiny::moduleServer(
      id,
    function(input, output, session) {
      
      sumTable <- shiny::reactive({
        if(!is.null(rowId()) & inputSingleView() == 'Calibration'){
          print('Extracting calibration')
          data <- getResult(
            performanceId = resultTable$performanceId[rowId()], 
            con = con,
            tableName = paste0(myTableAppend,'evaluation_statistics'), 
            mySchema = mySchema, 
            targetDialect = targetDialect 
          )
        } else{
          data <- NULL
        }
        
        if(is.null(data)){
          return(NULL)
        }
        
        for(i in 1:ncol(data)){
          data[,i] <- unlist(data[,i])
        }

        data$value <- as.double(as.character(data$value))
        data$value <- format(data$value, digits = 4, scientific = F)
        ind <- data$metric %in% c(
          'calibrationInLarge intercept', 
          'weak calibration intercept',
          'weak calibration gradient',
          'calibrationInLarge mean prediction',
          'calibrationInLarge observed risk',
          'ici',
          'Emean',
          'E90',
          'Emax', 
          'correctionFactor',
          'adjustGradient',
          'adjustIntercept'
        )
        
        tidyr::pivot_wider(
          data[ind,],
          names_from = 'metric', 
          values_from = 'value'
          )
 
      })
      
      output$calTable <- DT::renderDataTable({
        if(is.null(sumTable())){
          NULL
        } else{
          sumTable()
        }
      },
      escape = FALSE, 
      selection = 'single',
      filter = 'top', 
      extensions = 'Buttons', 
      options = list(
        dom = 'Blfrtip', 
        scrollX = TRUE
      ),
      rownames= FALSE 
      )
      
      calibrationSummary <- shiny::reactiveVal(NULL)
      demographicSummary <- shiny::reactiveVal(NULL)
      
      shiny::observeEvent(
        inputSingleView(),
        {
          if(!is.null(rowId()) & inputSingleView() == 'Calibration'){
            
            value <- getResult(
              performanceId = resultTable$performanceId[rowId()], 
              con = con,
              tableName = paste0(myTableAppend,'calibration_summary'), 
              mySchema = mySchema, 
              targetDialect = targetDialect 
            )
            calibrationSummary(value)
            
            value <- getResult(
              performanceId = resultTable$performanceId[rowId()], 
              con = con,
              tableName = paste0(myTableAppend,'demographic_summary'), 
              mySchema = mySchema, 
              targetDialect = targetDialect 
            )
            demographicSummary(value)
          }
        })
      
      
      output$cal <- shiny::renderPlot({
        type <- trimws(sumTable()$evaluation[input$calTable_rows_selected])
        tryCatch(
          {plotSparseCalibration2(
            calibrationSummary = calibrationSummary(), 
            type =  type
            )
          },
          error = function(err){emptyPlot(title = err)}
          )
      })
      
      output$demo <- shiny::renderPlot({
        type <- trimws(sumTable()$evaluation[input$calTable_rows_selected])
        tryCatch(
          plotDemographicSummary(
            demographicSummary = demographicSummary(), 
            type = type
          ),
          error= function(cond){return(NULL)}
        )
      })
      
      
      shiny::observeEvent(input$calHelp, {
        shiny::showModal(shiny::modalDialog(
          title = "Calibration Help",
          easyClose = TRUE,
          footer = NULL,
          size = "l",
          shiny::HTML(readChar("www/calHelp.html", file.info("www/calHelp.html")$size) )
        ))
      })
      shiny::observeEvent(input$demoHelp, {
        shiny::showModal(shiny::modalDialog(
          title = "Demographic Help",
          easyClose = TRUE,
          footer = NULL,
          size = "l",
          shiny::HTML(readChar("www/demoHelp.html", file.info("www/demoHelp.html")$size) )
        ))
      })
      
      
    }
  )
}



plotDemographicSummary <- function(
  demographicSummary, 
  type = NULL
  ){
  if (!all(is.na(demographicSummary$averagePredictedProbability))){
    
    ind <- 1:nrow(demographicSummary)
    if(is.null(type)){
      if(!is.null(demographicSummary$evaluation)){
        ind <- demographicSummary$evaluation%in%c('Test','validation')
      }
    } else{
      ind <- demographicSummary$evaluation==type
    }
    
    x <- demographicSummary[
      ind,
      colnames(demographicSummary) %in% c(
        'ageGroup','genGroup','averagePredictedProbability',
        'personCountAtRisk', 'personCountWithOutcome'
        )
      ]
    
    
    # remove -1 values:
    x$averagePredictedProbability[is.na(x$averagePredictedProbability)] <- 0
    x <- x[x$personCountWithOutcome != -1,]
    if(nrow(x)==0){
      return(NULL)
    }
    
    x$observed <- x$personCountWithOutcome/x$personCountAtRisk
    
    
    x <- x[, 
           colnames(x) %in% 
             c('ageGroup','genGroup',
               'averagePredictedProbability','observed'
               )
           ]
    

    x <- tidyr::pivot_longer(
      data = x, 
      cols = !colnames(x)[colnames(x) %in% c('ageGroup','genGroup')], 
      names_to = 'variable', 
      values_to = 'value'
      )
    #x <- reshape2::melt(x, id.vars=c('ageGroup','genGroup'))
    
    # 1.96*StDevPredictedProbability
    ci <- demographicSummary[ind,colnames(demographicSummary)%in%c('ageGroup','genGroup','averagePredictedProbability','stDevPredictedProbability')]
    ci$stDevPredictedProbability[is.na(ci$stDevPredictedProbability)] <- 1
    ci$lower <- ci$averagePredictedProbability-1.96*ci$stDevPredictedProbability
    ci$lower[ci$lower <0] <- 0
    ci$upper <- ci$averagePredictedProbability+1.96*ci$stDevPredictedProbability
    ci$upper[ci$upper >1] <- max(ci$upper[ci$upper <1])
    
    x$age <- gsub('Age group:','', x$ageGroup)
    x$age <- factor(
      x$age,
      levels = c(
        " 0-4"," 5-9"," 10-14",
        " 15-19"," 20-24"," 25-29",
        " 30-34"," 35-39"," 40-44",
        " 45-49"," 50-54"," 55-59",
        " 60-64"," 65-69"," 70-74",
        " 75-79"," 80-84"," 85-89",
        " 90-94"," 95-99","-1"
        ),
      ordered = TRUE
      )
    

    x <- merge(
      x, 
      ci[,c('ageGroup','genGroup','lower','upper')], 
      by = c('ageGroup','genGroup')
      )
    x <- x[!is.na(x$value),]
    
    plot <- ggplot2::ggplot(data=x, 
                            ggplot2::aes(x=age, 
                                         group=interaction(variable,genGroup))) +
      
      ggplot2::geom_line(ggplot2::aes(y=value, group=variable,
                                      color=variable,
                                      linetype = variable))+
      ggplot2::geom_ribbon(data=x[x$variable!='observed',],
                           ggplot2::aes(ymin=lower, ymax=upper
                                        , group=genGroup), 
                           fill="blue", alpha=0.2) +
      ggplot2::facet_grid(.~ genGroup, scales = "free") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
      #ggplot2::coord_flip() +
      ggplot2::scale_y_continuous("Fraction") +
      ggplot2::scale_x_discrete("Age") +
      ggplot2::scale_color_manual(values = c("royalblue4","red"),
                                  guide = ggplot2::guide_legend(title = NULL),
                                  labels = c("Expected", "Observed")) +
      
      ggplot2::guides(linetype=FALSE)
    
    return(plot)
  }
}

plotSparseCalibration2 <- function(
  calibrationSummary,
  smooth = "loess",
  span = 1,
  nKnots = 5,
  scatter = T,
  bins = 20,
  zoom =  "data",
  sample = T,
  type = NULL
) {
  
  ind <- 1:nrow(calibrationSummary)
  
  if(is.null(type)){
    if(!is.null(calibrationSummary$evaluation)){
      ind <- calibrationSummary$evaluation %in% c('Test','validation')
    }
  } else{
    ind <- calibrationSummary$evaluation == type
  }
  
  if(sum(ind) == 0){
    return(NULL)
  }
  
  sparsePred <- calibrationSummary[ind,]

  limVal <- max(
    max(sparsePred$averagePredictedProbability),
    max(sparsePred$observedIncidence)
    )
  
  smooth_plot <- ggplot2::ggplot(data = sparsePred, ggplot2::aes(x = averagePredictedProbability, 
                                                                 y = observedIncidence)) +
    ggplot2::stat_smooth(ggplot2::aes(color = "Loess", linetype = "Loess"),
                         method = "loess",
                         se = TRUE,
                         #span = span,
                         size = 1,
                         show.legend = F) +
    ggplot2::geom_segment(ggplot2::aes(x = 0,
                                       xend = 1,
                                       y = 0,
                                       yend = 1,
                                       color = "Ideal",
                                       linetype = "Ideal")) +
    ggplot2::coord_cartesian(xlim = c(0,limVal),
                             ylim = c(0,limVal)) + 
    ggplot2::scale_linetype_manual(name = "Models",
                                   values = c(Loess = "solid",
                                              Ideal = "dashed")) + 
    ggplot2::scale_color_manual(name = "Models", values = c(Loess = "blue", Ideal = "red")) + 
    ggplot2::labs(x = "Predicted Probability", y = "Observed Probability")
  
  # construct the plot grid
  if (scatter) {
    smooth_plot <- smooth_plot + ggplot2::geom_point(data = sparsePred,
                                                     ggplot2::aes(x = averagePredictedProbability,
                                                                  y = observedIncidence),
                                                     color = "black",
                                                     size = 2)
  }
  
  # Histogram object detailing the distibution of event/noevent for each probability interval
  
  popData1 <- sparsePred[,c('averagePredictedProbability', 'personCountWithOutcome')]
  popData1$label <- "Outcome"
  colnames(popData1) <- c('averagePredictedProbability','personCount',"label")
  
  popData2 <- sparsePred[,c('averagePredictedProbability', 'personCountAtRisk')]
  popData2$label <- "No Outcome"
  popData2$personCountAtRisk <- -1*(popData2$personCountAtRisk -popData1$personCount)
  colnames(popData2) <- c('averagePredictedProbability','personCount',"label")
  
  popData <- rbind(popData1, popData2)
  popData$averagePredictedProbability <- factor(popData$averagePredictedProbability)
  
  hist_plot <- ggplot2::ggplot(
    popData, 
    ggplot2::aes(
      y = averagePredictedProbability, 
      x = personCount, 
      fill = label
      )
    ) + 
    ggplot2::geom_bar(
      data = subset(
        popData, 
        label == "Outcome"
        ), 
      stat = "identity"
      ) + 
    ggplot2::geom_bar(
      data = subset(
        popData, 
        label == "No Outcome"
        ), 
      stat = "identity") + 
    ggplot2::geom_bar(stat = "identity") + 
    ggplot2::scale_x_continuous(labels = abs) + 
    ggplot2::coord_flip( ) +
    ggplot2::theme_bw() + 
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )
  
  # testting whether this is installed in shinydeploy
  plot <- gridExtra::grid.arrange(
    smooth_plot,
    hist_plot,
    ncol = 1,
    heights=c(2,1)
  )
  
  return(plot)
}
