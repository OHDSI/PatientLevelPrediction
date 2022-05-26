diagnosticsViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    
    shiny::column(width = 12,
                  
                  shinydashboard::box(
                    width = 12,
                    title = "Diagnostics Summary: ",
                    status = "info", 
                    solidHeader = TRUE,
                    DT::dataTableOutput('diagnosticSummaryTable')                
                  ),
                  
                  shinydashboard::box(
                    width = 12,
                    title = "Participants: ",
                    status = "info", 
                    solidHeader = TRUE,
                    DT::dataTableOutput('diagnosticParticipantsTable')                
                  ),
                  
                  shinydashboard::box(
                    width = 12,
                    title = "Predictors: ",
                    status = "info", 
                    solidHeader = TRUE,
                    DT::dataTableOutput('diagnosticPredictorsTable')                
                  ),
                  
                  shinydashboard::box(
                    width = 12,
                    title = "Outcomes: ",
                    status = "info", 
                    solidHeader = TRUE,
                    DT::dataTableOutput('diagnosticOutcomesTable')                
                  )
    )
  )
}

diagnosticsServer <- function(
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
      
      shiny::observe({
        if(!is.null(rowId()) & inputSingleView() == 'Diagnostics'){
          
          diagnostics <- getDiagnostics(
            modelDesignId = resultTable$modelDesignId[rowId()],
            database = resultTable$developmentDatabaseId[rowId()],
            mySchema, 
            con,
            myTableAppend, 
            targetDialect   
          )
          
          # input tables
          output$diagnosticSummaryTable <- DT::renderDataTable(
            diagnostics$summary
          )
          
          output$diagnosticParticipantsTable <- DT::renderDataTable(
            diagnostics$participants
          )
          
          output$diagnosticPredictorsTable <- DT::renderDataTable(
            diagnostics$predictors
          )
          
          output$diagnosticOutcomesTable <- DT::renderDataTable(
            diagnostics$outcomes
          )
        
        }
      })
      
    }
  )
}


# helpers


# get the data
getDiagnostics <- function(
  modelDesignId,
  databaseId,
  mySchema, 
  con,
  myTableAppend, 
  targetDialect   
){
  if(!is.null(modelDesignId)){
    print(paste0('model design: ', modelDesignId))
  }
  if(!is.null(databaseId)){
    print(paste0('database: ', databaseId))
  }

sql <- "SELECT * FROM @my_schema.@table_name WHERE model_design_id = @model_design_id
                                               and database_id = @database_id"
sql <- SqlRender::render(sql = sql, 
                         my_schema = mySchema,
                         table_name = 'diagnostics',
                         model_design_id = modelDesignId,
                         database_id = databaseId
                         )
sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
result <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
colnames(result) <- SqlRender::snakeCaseToCamelCase(colnames(result))

diagnosticId <- result$diagnosticId[1]
if(is.na(diagnosticId)){
  return(list())
}

sql <- "SELECT * FROM @my_schema.@table_name WHERE diagnostic_id = @diagnostic_id"
sql <- SqlRender::render(
  sql = sql, 
  my_schema = mySchema,
  table_name = 'diagnostic_summary',
  diagnostic_id = diagnosticId
)
sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
summary <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
colnames(summary) <- SqlRender::snakeCaseToCamelCase(colnames(summary))

sql <- "SELECT * FROM @my_schema.@table_name WHERE diagnostic_id = @diagnostic_id"
sql <- SqlRender::render(
  sql = sql, 
  my_schema = mySchema,
  table_name = 'diagnostic_predictors',
  diagnostic_id = diagnosticId
)
sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
predictors <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
colnames(predictors) <- SqlRender::snakeCaseToCamelCase(colnames(predictors))

sql <- "SELECT * FROM @my_schema.@table_name WHERE diagnostic_id = @diagnostic_id"
sql <- SqlRender::render(
  sql = sql, 
  my_schema = mySchema,
  table_name = 'diagnostic_participants',
  diagnostic_id = diagnosticId
)
sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
participants <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
colnames(participants) <- SqlRender::snakeCaseToCamelCase(colnames(participants))

sql <- "SELECT * FROM @my_schema.@table_name WHERE diagnostic_id = @diagnostic_id"
sql <- SqlRender::render(
  sql = sql, 
  my_schema = mySchema,
  table_name = 'diagnostic_outcomes',
  diagnostic_id = diagnosticId
)
sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
outcomes <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
colnames(outcomes) <- SqlRender::snakeCaseToCamelCase(colnames(outcomes))

result <- list(
  summary = summary,
  predictors = predictors,
  participants = participants,
  outcomes = outcomes
)

return(result)
  
}

