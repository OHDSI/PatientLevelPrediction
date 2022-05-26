outcomeViewer <- function(id){
  ns <- shiny::NS(id)
  shiny::fluidPage(
    
    shinydashboard::box( 
      status = 'info',
      title = shiny::actionLink(
        ns("diagnostic_outcomeHelp"), 
        "Probast 3.1", 
        icon = icon("info")
      ),
      solidHeader = TRUE, width = '90%',
    
    shiny::p('Was the outcome determined appropriately? (Are age/sex/year/month trends expected?)'),
    shiny::p(''),
    
    shiny::selectInput(
      inputId = ns('outcomeParameters'),
      label = 'Select Parameter',
      multiple = F, 
      choices = c('missing')
    ),
    
    plotly::plotlyOutput(ns('outcomePlot'))
    
    )
  )
}

outcomeServer <- function(
  id, 
  summaryTable, 
  resultRow, 
  mySchema, 
  con,
  myTableAppend,
  targetDialect
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      shiny::observeEvent(input$diagnostic_outcomeHelp , {
        
        fileLoc <- file.path('modules', strsplit(as.character(session$ns('outcomeParameters')), '-')[[1]][1],'www', 'probast3p1.html')

        shiny::showModal(shiny::modalDialog(
          title = "Probast 3.1",
          easyClose = TRUE,
          footer = NULL,
          size = "l",
          shiny::HTML(readChar(fileLoc, file.info(fileLoc)$size) )
        ))
      })
      
      outcomeTable <- shiny::reactive({
        getOutcomesData(
          con = con, 
          mySchema = mySchema, 
          targetDialect = targetDialect, 
          myTableAppend = myTableAppend,
          diagnosticId = summaryTable[ifelse(is.null(resultRow()),1,resultRow()),'diagnosticId']
        )
      })
      
      shiny::observe({
        updateSelectInput(
          session = session, 
          inputId = "outcomeParameters", 
          label = "Select Parameter", 
          choices =   unique(outcomeTable()$aggregation))
      })
      
        #plot: xvalue, outcomepercent, group by type -- filter: aggregation
        output$outcomePlot <- plotly::renderPlotly({
          plotly::plot_ly(
            data = outcomeTable() %>%
              dplyr::filter(
                .data$aggregation == ifelse(
                  is.null(input$outcomeParameters),
                  unique(outcomeTable()$aggregation)[1],
                  input$outcomeParameters
                )
              ), 
            x = ~xvalue, 
            y = ~outcomepercent, 
            group = ~type,
            color = ~type,
            type = 'scatter', 
            mode = 'lines'
          ) %>%
            plotly::layout(
              title = "Outcome rate",
              xaxis = list(title = "Value"),
              yaxis = list (title = "Percent of cohort with outcome")
            )
        })
      
        
      
      
    }
  )
}

getOutcomesData <- function(
  con, 
  mySchema, 
  targetDialect, 
  myTableAppend = '',
  diagnosticId = 1
){
  
  ParallelLogger::logInfo("gettingDb outcome diagnostics")
  
  sql <- "SELECT *
          from 
          @my_schema.@my_table_appendDIAGNOSTIC_OUTCOME
          where DIAGNOSTIC_ID = @diagnostic_id"
  
  sql <- SqlRender::render(sql = sql, 
                           my_schema = mySchema,
                           my_table_append = myTableAppend,
                           diagnostic_id = diagnosticId)
  
  sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
  
  result <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
  colnames(result) <- SqlRender::snakeCaseToCamelCase(colnames(result))
  
  ParallelLogger::logInfo("fetched outcome diagnostics")
  
  return(result)
}
