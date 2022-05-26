summaryDiagnosticViewer <- function(id){
  ns <- shiny::NS(id)
  shinydashboard::box( 
    status = 'info',
    title = shiny::actionLink(
      ns("diagnostic_summaryHelp"), 
      "Probast Summary", 
      icon = icon("info")
    ),
    solidHeader = TRUE, width = '90%',
    DT::dataTableOutput(ns('summaryTable'))
  )
}

summaryDiagnosticServer <- function(
  id, 
  con, 
  mySchema, 
  targetDialect,
  myTableAppend
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      
      summaryTable <- getDbSummary(
        con = con, 
        mySchema = mySchema, 
        targetDialect = targetDialect,
        myTableAppend = myTableAppend#,threshold1_2
      )
      
      # check if this makes drpdwn filter
      summaryTable$targetName <- as.factor(summaryTable$targetName)
      summaryTable$outcomeName <- as.factor(summaryTable$outcomeName)
      
      dataTab <- DT::datatable(
          summaryTable %>% dplyr::select(-c('diagnosticId')),
          rownames= FALSE, 
          selection = 'single', 
          filter = 'top',
          extensions = 'Buttons', 
          options = list(
            dom = 'Blfrtip' , 
            buttons = c(I('colvis'), 'copy', 'excel', 'pdf' ),
            scrollX = TRUE
            #pageLength = 100, lengthMenu=c(10, 50, 100,200)
          ),
          
          container = htmltools::withTags(
            table(
              class = 'display', 
              thead(
                #tags$th(title=active_columns[i], colnames(data)[i])
                tr(apply(
                  data.frame(
                    colnames = c(
                      'Database', 
                      'targetName',
                      'outcomeName',
                      '1.1',
                      '1.2',
                      '2.1',
                      '2.2',
                      '2.3',
                      '3.4',
                      '3.6',
                      '4.1'
                    ), 
                    labels = c('Database used to diagnose model design', 
                               'Target population - the patients you want to predict risk for',
                               'Outcome - what you want to predict',
                               '1.1 : Were appropriate data sources used, e.g. cohort, RCT or nested case-control study data?',
                               '1.2 : Were all inclusions and exclusions of participants appropriate?',
                               '2.1 : Were predictors defined and assessed in a similar way for all participants?',
                               '2.2 : Were predictor assessments made without knowledge of outcome data?',
                               '2.3 : Are all predictors available at the time the model is intended to be used?',
                               '3.4 : Was the outcome defined and determined in a similar way for all participants?',
                               '3.6 : Was the time interval between predictor assessment and outcome determination appropriate?',
                               '4.1 : Were there a reasonable number of participants with the outcome?'
                    )
                  ), 1,
                  function(x) th(title=x[2], x[1])
                )
                )
              )
            )
          )
          
        )
        
        for(probastName in colnames(summaryTable)[!colnames(summaryTable) %in% c('diagnosticId', 'targetName', 'outcomeName', 'Database')]){
          dataTab  <- dataTab  %>% 
            DT::formatStyle(
              columns = probastName,
              valueColumns = probastName,
              backgroundColor = DT::styleEqual(
                levels = c('Pass','Unknown','Fail'), 
                values = c("#DAF7A6","#FFC300","#FF5733")
              )
            )
        }
        
      output$summaryTable <- DT::renderDataTable(dataTab)
      
      selectedRow <- shiny::reactive({
        input$summaryTable_rows_selected
      })
      
      return(
        list(
          summaryTable = summaryTable, 
          resultRow = selectedRow
          )
        )
      
    }
  )
}



# get data
getDbSummary <- function(
  con, 
  mySchema, 
  targetDialect, 
  myTableAppend = '',
  threshold1_2 = 0.95
){
  ParallelLogger::logInfo("gettingDb summary")
  
  sql <- "SELECT distinct design.DIAGNOSTIC_ID,
          design.DATABASE,
          design.TARGET_JSON,
          design.OUTCOME_JSON,
          probast.PROBAST_ID,
          probast.RESULT
          
          from 
          @my_schema.@my_table_appendDIAGNOSTIC_DESIGN_SETTINGS design inner join
          @my_schema.@my_table_appendDIAGNOSTIC_PROBAST probast
          on design.DIAGNOSTIC_ID = probast.DIAGNOSTIC_ID"
  
  sql <- SqlRender::render(sql = sql, 
                           my_schema = mySchema,
                           my_table_append = myTableAppend)
  
  sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
  
  summaryTable <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
  colnames(summaryTable) <- SqlRender::snakeCaseToCamelCase(colnames(summaryTable))
  
  summaryTable$targetName <- unlist(
    lapply(
      summaryTable$targetJson, 
      function(x){jsonlite::unserializeJSON(x)$name}
    )
  )
  summaryTable$outcomeName <- unlist(
    lapply(
      summaryTable$outcomeJson, 
      function(x){jsonlite::unserializeJSON(x)$name}
    )
  )
  
  summary <- summaryTable %>% tidyr::pivot_wider(
    id_cols = c(
      'diagnosticId', 
      'database', 
      'targetName', 
      'outcomeName'
    ),
    names_from = 'probastId',
    values_from = 'result'
  ) %>% 
    dplyr::mutate(
      `1.2` = ifelse(
        .data$`1.2.1`>=threshold1_2 & 
          .data$`1.2.2`>=threshold1_2 &
          .data$`1.2.3`>=threshold1_2 & 
          .data$`1.2.4`>=threshold1_2,
        'Pass', 
        'Fail'
      )
    ) %>%
    dplyr::select(
      -c(
        '1.2.1', 
        '1.2.2', 
        '1.2.3', 
        '1.2.4'
      )
    ) %>%
    dplyr::relocate(.data$`1.2`, .after = .data$`1.1`)
  ParallelLogger::logInfo("got summary")
  return(summary)
}
