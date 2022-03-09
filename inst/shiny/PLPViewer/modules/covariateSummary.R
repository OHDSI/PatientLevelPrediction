covariateSummaryViewer <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
  shiny::fluidRow(
    shinydashboard::box( 
      status = 'info',
      title = "Binary", 
      solidHeader = TRUE,
      shinycssloaders::withSpinner(
        plotly::plotlyOutput(
          ns('covariateSummaryBinary')
        )
      )
    ),
    shinydashboard::box(
      status = 'info',
      title = "Measurements", 
      solidHeader = TRUE,
      side = "right",
      shinycssloaders::withSpinner(
        plotly::plotlyOutput(
          ns('covariateSummaryMeasure')
        )
      )
    )
  ),
    
    shiny::fluidRow(
      width=12,
      shinydashboard::box(
        status = 'info', width = 12,
        title = "Covariates", solidHeader = TRUE,
        DT::dataTableOutput(ns('modelCovariateInfo'))
      )
    ),
    shiny::fluidRow(
      width=12,
      shinydashboard::box(status = 'info', 
        width = 12,
        title = "Model Table", 
        solidHeader = TRUE,
        shiny::downloadButton("downloadData", "Download Model"),
        DT::dataTableOutput(ns('modelView'))
      )
    )
  )

}

covariateSummaryServer <- function(id, plpResult, summaryTable, resultRow, mySchema, con,
                                   inputSingleView,
                                   myTableAppend = '', 
                                   targetDialect = NULL) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
        covariateSummary <- shiny::reactive({
          if(inputSingleView == "Model"){
          if(is.null(plpResult()$covariateSummary)){
            covariateSummary <- tryCatch(
              {loadCovSumFromDb(summaryTable[resultRow(),], mySchema, con, myTableAppend, targetDialect)},
              error = function(e){return(NULL)}
              )
            print(colnames(covariateSummary)) # temp for debugging
            return(covariateSummary)
          } else{
            return(plpResult()$covariateSummary)
          }
          }
        })
        
      # covariate table
        output$modelView <- DT::renderDataTable(
          editCovariates(covariateSummary())$table,
          colnames = editCovariates(covariateSummary())$colnames
        )
      

      output$modelCovariateInfo <- DT::renderDataTable(
        data.frame(
          covariates = nrow(covariateSummary()),
          nonZeroCount = sum(covariateSummary()$covariateValue!=0, na.rm = T),
          intercept = getIntercept(plpResult())
          )
        )
      
      # covariate model plots
      covs <- shiny::reactive({
        if(is.null(covariateSummary()))
          return(NULL)
        plotCovariateSummary(formatCovariateTable(covariateSummary()))
      })
      
      output$covariateSummaryBinary <- plotly::renderPlotly({ covs()$binary })
      output$covariateSummaryMeasure <- plotly::renderPlotly({ covs()$meas })
      
      # Downloadable csv of model ----
      output$downloadData <- shiny::downloadHandler(
        filename = function(){'model.csv'},
        content = function(file) {
          utils::write.csv( 
            covariateSummary()[,colnames(covariateSummary()) %in% c('covariateName','covariateValue','CovariateCount','WithOutcomeCovariateMean','WithNoOutcomeCovariateMean','WithOutcome_CovariateMean','WithNoOutcome_CovariateMean' )],
            file, 
            row.names = FALSE
            )
        }
      )
      
    }
  )
}


# helpers

# format covariate summary table
formatCovariateTable <- function(covariateSummary){
  covariateSummary <- as.data.frame(covariateSummary)
  colnames(covariateSummary) <- gsub('_','', colnames(covariateSummary) )
  for(coln in c('covariateValue','WithOutcomeCovariateMean','WithNoOutcomeCovariateMean','StandardizedMeanDiff')){
    if(sum(colnames(covariateSummary)==coln)>0){
      covariateSummary[,coln] <- format(round(covariateSummary[,coln], 4), nsmall = 4)
      class(covariateSummary[,coln]) <- "numeric"
    }
  }
  return(covariateSummary)
}



editCovariates <- function(covs){
  colnames(covs) <- gsub('_','', colnames(covs) )
  if(!is.null(covs$StandardizedMeanDiff)){
    return(list(table = formatCovariateTable(covs[,c('covariateName','covariateValue','CovariateCount','WithOutcomeCovariateMean','WithNoOutcomeCovariateMean','StandardizedMeanDiff')]),
                colnames = c('Covariate Name', 'Value','Count', 'Outcome Mean', 'Non-outcome Mean','Std Mean Diff')
    ))
  } else{
    return(list(table = formatCovariateTable(covs[,c('covariateName','covariateValue','CovariateCount','WithOutcomeCovariateMean','WithNoOutcomeCovariateMean')]),
                colnames = c('Covariate Name', 'Value','Count', 'Outcome Mean', 'Non-outcome Mean')
    ))
  }
}



plotCovariateSummary <- function(covariateSummary){
  
  colnames(covariateSummary) <- gsub('_','', colnames(covariateSummary) )
  
  #writeLines(paste(colnames(covariateSummary)))
  #writeLines(paste(covariateSummary[1,]))
  # remove na values 
  covariateSummary$WithNoOutcomeCovariateMean[is.na(covariateSummary$WithNoOutcomeCovariateMean)] <- 0
  covariateSummary$WithOutcomeCovariateMean[is.na(covariateSummary$WithOutcomeCovariateMean)] <- 0
  if(!'covariateValue'%in%colnames(covariateSummary)){
    covariateSummary$covariateValue <- 1
  }
  if(sum(is.na(covariateSummary$covariateValue))>0){
    covariateSummary$covariateValue[is.na(covariateSummary$covariateValue)] <- 0
  }
  
  # SPEED EDIT remove the none model variables
  covariateSummary <- covariateSummary[covariateSummary$covariateValue!=0,]
  
  # save dots based on coef value 
  covariateSummary$size <- abs(covariateSummary$covariateValue)
  covariateSummary$size[is.na(covariateSummary$size)] <- 4
  covariateSummary$size <- 4+4*covariateSummary$size/max(covariateSummary$size)
  
  # color based on analysis id
  covariateSummary$color <- sapply(covariateSummary$covariateName, function(x) ifelse(is.na(x), '', strsplit(as.character(x), ' ')[[1]][1]))
  
  covariateSummary$times <- sapply(sapply(covariateSummary$covariateName, function(x) ifelse(is.na(x), '', strsplit(as.character(x), 'during day ')[[1]][2])),function(x) ifelse(is.na(x), '', strsplit(as.character(x), ': ')[[1]][1]))
  covariateSummary$desc <- sapply(covariateSummary$covariateName, function(x) ifelse(is.na(x), '', strsplit(as.character(x), ': ')[[1]][2]))
  
  
  l <- list(x = 0.01, y = 1,
            font = list(
              family = "sans-serif",
              size = 10,
              color = "#000"),
            bgcolor = "#E2E2E2",
            bordercolor = "#FFFFFF",
            borderwidth = 1)
  
  ind <- covariateSummary$WithNoOutcomeCovariateMean <=1 & covariateSummary$WithOutcomeCovariateMean <= 1
  # create two plots -1 or less or g1
  binary <- plotly::plot_ly(x = covariateSummary$WithNoOutcomeCovariateMean[ind],
                            #size = covariateSummary$size[ind],
                            showlegend = F) %>%
    plotly::add_markers(y = covariateSummary$WithOutcomeCovariateMean[ind],
                        color=factor(covariateSummary$color[ind]),
                        hoverinfo = 'text',
                        text = ~paste('</br> Type: ', covariateSummary$color[ind],
                                      '</br> Time: ', covariateSummary$times[ind],
                                      '</br> Name: ', covariateSummary$desc[ind]),
                        showlegend = T
    ) %>%
    plotly::add_trace(x= c(0,1), y = c(0,1),mode = 'lines',
                      line = list(dash = "dash"), color = I('black'),
                      type='scatter', showlegend = FALSE) %>%
    plotly::layout(#title = 'Prevalance of baseline predictors in persons with and without outcome',
      xaxis = list(title = "Prevalance in persons without outcome",
                   range = c(0, 1)),
      yaxis = list(title = "Prevalance in persons with outcome",
                   range = c(0, 1)),
      #legend = l, showlegend = T,
      legend = list(orientation = 'h', y = -0.3), showlegend = T)
  
  if(sum(!ind)>0){
    maxValue <- max(c(covariateSummary$WithNoOutcomeCovariateMean[!ind],
                      covariateSummary$WithOutcomeCovariateMean[!ind]), na.rm = T)
    meas <- plotly::plot_ly(x = covariateSummary$WithNoOutcomeCovariateMean[!ind] ) %>%
      plotly::add_markers(y = covariateSummary$WithOutcomeCovariateMean[!ind],
                          hoverinfo = 'text',
                          text = ~paste('</br> Type: ', covariateSummary$color[!ind],
                                        '</br> Time: ', covariateSummary$times[!ind],
                                        '</br> Name: ', covariateSummary$desc[!ind])) %>%
      plotly::add_trace(x= c(0,maxValue), y = c(0,maxValue),mode = 'lines',
                        line = list(dash = "dash"), color = I('black'),
                        type='scatter', showlegend = FALSE) %>%
      plotly::layout(#title = 'Prevalance of baseline predictors in persons with and without outcome',
        xaxis = list(title = "Mean in persons without outcome"),
        yaxis = list(title = "Mean in persons with outcome"),
        showlegend = FALSE)
  } else {
    meas <- NULL
  }
  
  return(list(binary=binary,
              meas = meas))
}



getIntercept <- function(plpResult){
  
  if('model'%in%names(plpResult)){
    
    if('model'%in%names(plpResult$model)){
      
      if('coefficients'%in%names(plpResult$model$model)){
        
        return(plpResult$model$model$coefficients[1])
        
      }
      
    }
  }
  return(0)
}


toFirstUpper <- function(vec){
  res <- lapply(vec, function(x) paste0(toupper(substr(x,1,1)), substr(x,2,nchar(x))))
  return(unlist(res))
}

# code for database covariate extract
loadCovSumFromDb <- function(chosenRow, mySchema, con, myTableAppend = '', targetDialect = 'redshift'){
  ParallelLogger::logInfo("starting covsum")
  resultId <- chosenRow$resultId
  sql <- "SELECT * FROM @my_schema.@my_table_appendcovariate_summary AS covariate_summary WHERE result_id = @result_id;" 
  
  sql <- SqlRender::render(sql = sql,
    my_schema = mySchema,
    result_id = resultId,
    my_table_append = myTableAppend)
  sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
  
  covariateSummary <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
  colnames(covariateSummary) <- SqlRender::snakeCaseToCamelCase(colnames(covariateSummary))
  
  # capitalize the first letter of column names 
  # except covariateId covariateName analysisId conceptId
  ind <- !colnames(covariateSummary) %in% c('covariateValue','covariateId', 'covariateName', 'analysisId', 'conceptId')
  colnames(covariateSummary)[ind] <- toFirstUpper(colnames(covariateSummary)[ind])
  
  ParallelLogger::logInfo("finishing covsum")
  return(covariateSummary)
}
