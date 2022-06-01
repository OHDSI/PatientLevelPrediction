predictorViewer <- function(id){
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shinydashboard::box( 
      status = 'info',
      title = shiny::actionLink(
        ns("diagnostic_predictorsHelp"), 
        "Probast 2.2", 
        icon = icon("info")
      ),
      solidHeader = TRUE, width = '90%',
      shiny::p('Were predictor assessments made without knowledge of outcome data? (if outcome occur shortly after index this may be problematic)'),
      shiny::p(''),
      
      shiny::selectInput(
        inputId = ns('predictorParameters'),
        label = 'Select Parameter',
        multiple = F, 
        choices = c('missing')
      ),
      
      plotly::plotlyOutput(ns('predictorPlot'))
    )
  )
}

predictorServer <- function(
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
      
      predTable <- shiny::reactive({
        getPredictors(
          con = con, 
          mySchema = mySchema, 
          targetDialect = targetDialect, 
          myTableAppend = myTableAppend,
          diagnosticId = summaryTable[ifelse(is.null(resultRow()), 1, resultRow()),'diagnosticId']
        )
      })
      
      
      shiny::observe({
        
        print(predTable())
        
        shiny::updateSelectInput(
          session = session, 
          inputId = "predictorParameters", 
          label = "Select Setting", 
          choices = unique(predTable()$inputType),
          selected = unique(predTable()$inputType)[1]
        )
      })
      
      output$predictorPlot <- plotly::renderPlotly({
        
        tempPredTable <-  predTable() %>% 
          dplyr::filter(
            .data$inputType == ifelse(
              is.null(input$predictorParameters), 
              unique(predTable()$inputType)[1],
              input$predictorParameters
            )
          ) %>%
          dplyr::select(
            .data$daysToEvent, 
            .data$outcomeAtTime, 
            .data$observedAtStartOfDay
          ) %>%
          dplyr::mutate(
            survivalT = (.data$observedAtStartOfDay -.data$outcomeAtTime)/.data$observedAtStartOfDay
          ) %>%
          dplyr::filter(
            !is.na(.data$daysToEvent)
          )
        
        tempPredTable$probSurvT  <- unlist(
          lapply(
            1:length(tempPredTable$daysToEvent), 
            function(x){prod(tempPredTable$survivalT[tempPredTable$daysToEvent <= tempPredTable$daysToEvent[x]])}
          )
        )
        
        plotly::plot_ly(x = ~ tempPredTable$daysToEvent) %>% 
          plotly::add_lines(
            y = tempPredTable$probSurvT, 
            name = "hv", 
            line = list(shape = "hv")
          )
      })
      
      
    }
  )
}


getPredictors <- function(
  con, 
  mySchema, 
  targetDialect, 
  myTableAppend = '',
  diagnosticId = 1
){
  
  ParallelLogger::logInfo("gettingDb predictor diagnostics")
  
  sql <- "SELECT *
          from 
          @my_schema.@my_table_appendDIAGNOSTIC_PREDICTORS
          where DIAGNOSTIC_ID = @diagnostic_id"
  
  sql <- SqlRender::render(sql = sql, 
                           my_schema = mySchema,
                           my_table_append = myTableAppend,
                           diagnostic_id = diagnosticId)
  
  sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
  
  result <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
  colnames(result) <- SqlRender::snakeCaseToCamelCase(colnames(result))
  ParallelLogger::logInfo("fetched predictor diagnostics")
  
  return(result)
}