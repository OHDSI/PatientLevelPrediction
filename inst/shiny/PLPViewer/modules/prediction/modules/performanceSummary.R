performanceSummaryViewer <- function(id) {
  ns <- shiny::NS(id)
  DT::dataTableOutput(ns('performanceSummaryTable'))
}

performanceSummaryServer <- function(
  id, 
  con,
  mySchema,
  targetDialect,
  myTableAppend
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      resultTable <- getInternalPerformanceSummary(
        con = con, 
        mySchema = mySchema, 
        targetDialect = targetDialect, 
        myTableAppend = myTableAppend 
        )
      
      # check if this makes drpdwn filter
      resultTable$T <- as.factor(resultTable$T)
      resultTable$O <- as.factor(resultTable$O)
      
      shinyInput <- function(FUN,id,num,...) {
        inputs <- character(num)
        for (i in seq_len(num)) {
          inputs[i] <- as.character(FUN(paste0(id,i),label=NULL,...))
        }
        inputs
      }
      
      output$performanceSummaryTable <- DT::renderDataTable(
        DT::datatable(
          data = resultTable[,!colnames(resultTable)%in% c('performanceId', 'developmentDatabaseId', 'modelDesignId')],
      
          #cbind(
            #resultTable[,!colnames(resultTable)%in% c('performanceId', 'developmentDatabaseId', 'modelDesignId')]#,
            #diagnostic = shinyInput(shiny::checkboxInput,"srows_",nrow(resultTable),value=NULL,width=1)
          #),
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
                      'Dev', 
                      'Val', 
                      'T',
                      'O', 
                      'Model',
                      'Covariate setting',
                      'TAR', 
                      'AUROC', 
                      'AUPRC', 
                      'T Size', 
                      'O Count',
                      'Val (%)', 
                      'O Incidence (%)', 
                      'timeStamp'#,
                      #'diagnostic'
                      
                    ), 
                    labels = c('Database used to develop the model', 
                      'Database used to evaluate model', 
                      'Target population - the patients you want to predict risk for',
                      'Outcome - what you want to predict', 
                      'Model type',
                      'Id for the covariate/settings used',
                      'Time-at-risk period', 
                      'Area under the reciever operating characteristics (test or validation)', 
                      'Area under the precision recall curve (test or validation)',
                      'Target population size in the data', 
                      'Outcome count in the data',
                      'The percentage of data used to evaluate the model', 
                      'Percentage of target population that have outcome during time-at-risk',
                      'date and time of execution'#,
                      #'include diagnostic in report'
                      )
                  ), 1,
                  function(x) th(title=x[2], x[1])
                )
                )
              )
            )
          )
          
        )
      )
      
      #rowId <- shiny::reactiveVal(
      #  1
      #)
      #shiny::observeEvent(input$performanceSummaryTable_rows_selected, {
      #  rowId(input$performanceSummaryTable_rows_selected)
      #})
      
      rowId <- shiny::reactive({
          input$performanceSummaryTable_rows_selected
      })
      
      return(
        list(
          resultTable = resultTable,
          rowId = rowId
        )
      )
      
    }
  )
}



getInternalPerformanceSummary <- function(con, mySchema, targetDialect, myTableAppend = '' ){
  ParallelLogger::logInfo("gettingDb summary")
  
  sql <- "SELECT distinct 
     results.performance_id, 
     results.model_design_id, 
     results.development_database_id,
     d.database_acronym AS Dev, 
     d.database_acronym AS Val,
     targets.cohort_name AS T, 
     outcomes.cohort_name AS O,
       models.model_type AS model, 
       models.execution_date_time as time_stamp,
       model_designs.covariate_setting_id, 
       tars.tar_start_day, 
       tars.tar_start_anchor, 
       tars.tar_end_day, 
       tars.tar_end_anchor,
       ROUND(aucResult.auc, 3) as auc,
       ROUND(auprcResult.auprc,4) as auprc,
       nResult.population_size, 
       oResult.outcome_count,
       ROUND(nTest.test_size*100.0/nResult.population_size, 1) as eval_percent,
       ROUND(oResult.outcome_count*100.0/nResult.population_size,4) as outcome_percent
       
       FROM @my_schema.@my_table_appendperformances AS results INNER JOIN @my_schema.@my_table_appendmodels AS models 
          ON results.model_design_id = models.model_design_id and
             results.development_database_id = models.database_id
             
    inner join @my_schema.@my_table_appendmodel_designs as model_designs
    on model_designs.model_design_id = models.model_design_id and
    results.cohort_id = model_designs.cohort_id and 
             results.outcome_id = model_designs.outcome_id and 
             results.tar_id = model_designs.tar_id and
             results.population_setting_id = model_designs.population_setting_id
             -- and results.plp_data_setting_id = model_designs.plp_data_setting_id
             
        LEFT JOIN (SELECT cohort_id, cohort_name FROM @my_schema.@my_table_appendcohorts) AS targets ON results.cohort_id = targets.cohort_id
        LEFT JOIN (SELECT cohort_id, cohort_name FROM @my_schema.@my_table_appendcohorts) AS outcomes ON results.outcome_id = outcomes.cohort_id
        LEFT JOIN @my_schema.@my_table_appenddatabase_details AS d ON results.development_database_id = d.database_id 
        LEFT JOIN @my_schema.@my_table_appendtars AS tars ON results.tar_id = tars.tar_id
        LEFT JOIN (SELECT performance_id, value AS auc FROM @my_schema.@my_table_appendevaluation_statistics where metric = 'AUROC' and evaluation in ('Test','Validation') ) AS aucResult ON results.performance_id = aucResult.performance_id
        LEFT JOIN (SELECT performance_id, value AS auprc FROM @my_schema.@my_table_appendevaluation_statistics where metric = 'AUPRC' and evaluation in ('Test','Validation') ) AS auprcResult ON results.performance_id = auprcResult.performance_id
        LEFT JOIN (SELECT performance_id, sum(value) AS population_size FROM @my_schema.@my_table_appendevaluation_statistics where metric = 'populationSize' and evaluation in ('Test','Train') group by performance_id) AS nResult ON results.performance_id = nResult.performance_id
        LEFT JOIN (SELECT performance_id, sum(value) AS outcome_count FROM @my_schema.@my_table_appendevaluation_statistics where metric = 'outcomeCount' and evaluation in ('Test','Train') group by performance_id) AS oResult ON results.performance_id = oResult.performance_id
        LEFT JOIN (SELECT performance_id, value AS test_size FROM @my_schema.@my_table_appendevaluation_statistics where metric = 'populationSize' and evaluation = 'Test') AS nTest ON results.performance_id = nTest.performance_id;"
  
  sql <- SqlRender::render(sql = sql, 
                           my_schema = mySchema,
                           my_table_append = myTableAppend)
  
  sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
  
  summaryTable <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
  colnames(summaryTable) <- SqlRender::snakeCaseToCamelCase(colnames(summaryTable))
  
  summaryTable$t <- trimws(summaryTable$t)
  summaryTable$o <- trimws(summaryTable$o)
  
  summaryTable <- summaryTable %>% 
    dplyr::rename(`Covariate setting` = covariateSettingId) %>%
    dplyr::rename(`T Size` = populationSize) %>% 
    dplyr::rename(`O Count` = outcomeCount) %>%
    dplyr::rename(`Val (%)` = evalPercent) %>%
    dplyr::rename(`O Incidence (%)` = outcomePercent)
  
  summaryTable <- editTar(summaryTable)
  
  colnames(summaryTable) <- editColnames(cnames = colnames(summaryTable), 
                                         edits = c('AUC','AUPRC', 'T', 'O', 'Dev','Val', 'TAR', 'Model'))
  
  #summaryTable$timeStamp <- 0
  #summaryTable$Analysis <- summaryTable$analysisId
  ParallelLogger::logInfo("Got db summary")
  return(summaryTable[,c('Dev', 'Val', 'T','O', 'Model','Covariate setting',
                         'TAR', 'AUC', 'AUPRC', 
                         'T Size', 'O Count','Val (%)', 'O Incidence (%)', 'timeStamp', 'performanceId', 'modelDesignId', 'developmentDatabaseId')])
  
}

editTar <- function(summaryTable){
  
  summaryTable <- summaryTable %>% dplyr::mutate(TAR = paste0('(',trimws(.data$tarStartAnchor),' + ',.data$tarStartDay, ') - (',trimws(.data$tarEndAnchor),' + ',.data$tarEndDay, ')' )) %>%
    dplyr::select(-c(.data$tarStartAnchor, .data$tarStartDay, .data$tarEndAnchor, .data$tarEndDay))
  
  return(summaryTable)
}

editColnames <- function(cnames, edits){
  lwcnames <- tolower(cnames)
  
  for(edit in edits){
    if(tolower(edit)%in%lwcnames){
      cnames[tolower(edit)==lwcnames] <- edit
    }
  }
  return(cnames)
  
}
