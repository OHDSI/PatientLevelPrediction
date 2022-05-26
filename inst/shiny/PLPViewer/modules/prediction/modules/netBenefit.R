nbViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    
    shiny::fluidRow(
      shinydashboard::box(
        status = 'info', 
        width = 12,
        title = 'Select net benefit type to view:',
        solidHeader = TRUE,
        shiny::uiOutput(ns('nbSelect'))
      )
    ),
  
    shiny::fluidRow(
      shinydashboard::box(
        status = 'info', 
        width = 6,
        title = 'Net Benefit Plot',
        solidHeader = TRUE,
        side = "right",
        shinycssloaders::withSpinner(
          shiny::plotOutput(ns('nbPlot'))
        )
      ),
      
      shinydashboard::box(
        status = 'info', 
        width = 6,
        title = 'Summary',
        solidHeader = TRUE,
        DT::dataTableOutput(ns('nbTable'))
      )
    )
  )
}

nbServer <- function(
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
      
      thresholdSummary <- shiny::reactiveVal()
      
      shiny::observeEvent(
        inputSingleView(),
        {
          if(!is.null(rowId()) & inputSingleView() == 'Net Benefit'){
           
            value <- getResult(
              performanceId = resultTable$performanceId[rowId()], 
              con = con,
              tableName = paste0(myTableAppend,'threshold_summary'), 
              mySchema = mySchema, 
              targetDialect = targetDialect 
            )
            thresholdSummary(value)
          }
        }
      )
      
      output$nbSelect = shiny::renderUI({
        shiny::selectInput(
          inputId = session$ns('nbSelectInput'), 
          label = 'Type:', 
          choices = unique(thresholdSummary()$evaluation), 
          multiple = F, 
          selectize=FALSE
        )
      })
      
      output$nbTable <- DT::renderDataTable({
        if(is.null(thresholdSummary)){
          return(NULL)
        } else{
          result <- extractNetBenefit(
            thresholdSummary = thresholdSummary(), 
            type=trimws(input$nbSelectInput)
          )
          #unique(result)
          result$treatAll <- format(result$treatAll, digits = 2, scientific = F)
          result$netBenefit <- format(result$netBenefit, digits = 2, scientific = F)
          result
        }
      })
      
      output$nbPlot <- shiny::renderPlot({
        if(is.null(thresholdSummary())){
          return(NULL)
        } else{
          result <- extractNetBenefit(
            thresholdSummary = thresholdSummary(), 
            type=trimws(input$nbSelectInput)
          )
          result <- unique(result)
          ind <- !is.na(result$netBenefit) & is.finite(result$netBenefit) & !is.null(result$netBenefit) & is.finite(result$pt)
          
          df2 <- tidyr::pivot_longer(
            data = result, 
            cols = colnames(result)[colnames(result) != 'pt'], 
            names_to = 'variable', 
            values_to = 'value'
          )
          
          
          ggplot2::ggplot(
            df2, 
            ggplot2::aes(
              x=pt, 
              y = value, 
              group = variable, 
              color = variable
            )
          ) +
            ggplot2::geom_line(ggplot2::aes(linetype = variable))+
            ggplot2::geom_point(ggplot2::aes(shape = variable))
        }
      })
      
    }
  )
}



extractNetBenefit <- function(thresholdSummary, type=NULL, modelId=NULL){
  data <- thresholdSummary
  
  if(!is.null(type)){
    if(!is.null(data$evaluation[1])){
      data <- data[data$evaluation==type,]
    }
  }
  
  pt <- data$predictionThreshold
  TP <- data$truePositiveCount
  FP <- data$falsePositiveCount
  n <- data$positiveCount + data$negativeCount
  
  treatAll <- data$trueCount/n-data$falseCount/n*(pt/(1-pt))
  
  if(!is.null(modelId[1])){
    netbenefit <- data.frame(modelId=modelId, pt=pt, netBenefit=TP/n-(FP/n)*(pt/(1-pt)),
                             treatAll=treatAll)
  }else{
    netbenefit <- data.frame(pt=pt, netBenefit=TP/n-(FP/n)*(pt/(1-pt)),
                             treatAll=treatAll)
  }
  
  return(netbenefit)
}
