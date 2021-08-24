summaryViewer <- function(id) {
  ns <- shiny::NS(id)
  DT::dataTableOutput(ns('summaryTable'))
}

summaryServer <- function(id, summaryTable, cNamesExclude = c('Analysis','analysisId','resultId','researcherId','addExposureDaysToStart','addExposureDaysToEnd', 'plpResultLocation', 'plpResultLoad')) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # check if this makes drpdwn filter
      summaryTable$T <- as.factor(summaryTable$T)
      summaryTable$O <- as.factor(summaryTable$O)

      output$summaryTable <- DT::renderDataTable(DT::datatable(summaryTable[,!colnames(summaryTable)%in%cNamesExclude],
                                                               rownames= FALSE, selection = 'single', filter = 'top',
                                                               extensions = 'Buttons', options = list(
                                                                 dom = 'Blfrtip' , 
                                                                 buttons = c(I('colvis'), 'copy', 'excel', 'pdf' ),
                                                                 scrollX = TRUE
                                                                 #pageLength = 100, lengthMenu=c(10, 50, 100,200)
                                                               ),
                                                               
                                                               container = htmltools::withTags(table(
                                                                 class = 'display',
                                                                 thead(
                                                                   #tags$th(title=active_columns[i], colnames(data)[i])
                                                                   tr(apply(data.frame(colnames=c('Dev', 'Val', 'T','O', 'Model','Covariate setting',
                                                                                                  'TAR', 'AUC', 'AUPRC', 
                                                                                                  'T Size', 'O Count','Val (%)', 'O Incidence (%)', 'timeStamp'), 
                                                                                       labels=c('Database used to develop the model', 'Database used to evaluate model', 'Target population - the patients you want to predict risk for','Outcome - what you want to predict', 
                                                                                                'Model type','Id for the covariate/settings used','Time-at-risk period', 'Area under the reciever operating characteristics (test or validation)', 'Area under the precision recall curve (test or validation)',
                                                                                                'Target population size in the data', 'Outcome count in the data','The percentage of data used to evaluate the model', 'Percentage of target population that have outcome during time-at-risk','date and time of execution')), 1,
                                                                            function(x) th(title=x[2], x[1])))
                                                                 )
                                                               ))
                                                               
      )
      )
      
      selectedRow <- shiny::reactive({
        input$summaryTable_rows_selected
      })
      
      return(selectedRow)
      
    }
  )
}