participantViewer <- function(id){
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::p('Probast 1.2: Were all inclusions and exclusions of participants appropriate? (differences caused by different inclusion criteria can be observed here)'),
    shiny::p(''),
    
    shiny::selectInput(
      inputId = ns('participantParameters'),
      label = 'Select Parameter',
      multiple = F, 
      choices = c('missing')
    ),
    
    DT::dataTableOutput(ns('participantTable'))
  )
}

participantServer <- function(
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
      
      
      participantTable <- shiny::reactive({
        
        participantTable <- getParticipants(
          con = con, 
          mySchema = mySchema, 
          targetDialect = targetDialect, 
          myTableAppend = myTableAppend,
          diagnosticId = summaryTable[ifelse(is.null(resultRow()),1,resultRow()),'diagnosticId']
        )
        
        participantTable$parameter <- unlist(
          lapply(
            participantTable$design, 
            function(x){strsplit(x, ':')[[1]][1]}
          )
        )
        participantTable$paramvalue <- unlist(
          lapply(
            participantTable$design, 
            function(x){gsub(' ', '', strsplit(x, ':')[[1]][2])}
          )
        )
        
        return(participantTable)
        
      })

      shiny::observe({
        shiny::updateSelectInput(
          session = session, 
          inputId = "participantParameters", 
          label = "Select Parameter", 
          choices = unique(participantTable()$parameter),
          selected = unique(participantTable()$parameter)[1]
          )
      })
      
      
      output$participantTable <- DT::renderDataTable(
                DT::datatable(
                  participantTable() %>% 
                    dplyr::filter(.data$parameter == ifelse(is.null(input$participantParameters), unique(participantTable()$parameter)[1], input$participantParameters)) %>%
                    dplyr::select(
                      .data$probastId,
                      .data$paramvalue,
                      .data$metric, 
                      .data$value
                    ) %>%
                    dplyr::mutate(
                      value = format(.data$value, nsmall = 2, )
                    )  %>%
                    tidyr::pivot_wider(
                      names_from = .data$paramvalue, 
                      values_from = .data$value
                    )
                  ,
                  rownames= FALSE, 
                  selection = 'single', 
                  filter = 'top',
                  extensions = 'Buttons', 
                  options = list(
                    dom = 'Blfrtip' , 
                    buttons = c(I('colvis'), 'copy', 'excel', 'pdf' ),
                    scrollX = TRUE
                  )
                )
              )

    }
  )
}


getParticipants <- function(
  con, 
  mySchema, 
  targetDialect, 
  myTableAppend = '',
  diagnosticId = 1
){
  
  ParallelLogger::logInfo("gettingDb participant diagnostics")
  
  sql <- "SELECT *
          from 
          @my_schema.@my_table_appendDIAGNOSTIC_PARTICIPANTS
          where DIAGNOSTIC_ID = @diagnostic_id"
  
  sql <- SqlRender::render(sql = sql, 
                           my_schema = mySchema,
                           my_table_append = myTableAppend,
                           diagnostic_id = diagnosticId)
  
  sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
  
  result <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
  colnames(result) <- SqlRender::snakeCaseToCamelCase(colnames(result))
  
  ParallelLogger::logInfo("fetched participant diagnostics")
  
  return(result)
}