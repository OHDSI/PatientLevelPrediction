prettyHr <- function(x) {
  result <- sprintf("%.2f", x)
  result[is.na(x)] <- "NA"
  result <- suppressWarnings(format(as.numeric(result), big.mark=",")) # add thousands separator
  return(result)
}

addThousandsSeparator<-function(table){
  if(is.data.frame(table)){
    is.num <- sapply(table, is.numeric)
    table[is.num] <- lapply(table[is.num], function(x) format(as.numeric(x), big.mark=","))
    return(table)
  } else {
    is.not.na<- !sapply(suppressWarnings(as.numeric(table)), is.na)
    table[is.not.na] <- format(as.numeric(table[is.not.na]), big.mark=",")
    return(table)
  }
  
}

getHoveroverStyle <- function(left_px, top_px) {
  style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                   "left:",
                   left_px - 200,
                   "px; top:",
                   top_px - 130,
                   "px; width:400px;")
}



shinyServer(function(input, output, session) {
  
  
  # Tables
  
  output$incidenceTable <- renderDT({
    table <- incidence
    table$opercent = as.numeric(prettyHr(table$opercent))
    result<-datatable(table,
      filter="top",
      options = list(pageLenth=25,
                     scrollX = TRUE,
                     dom='Blfrtip', 
                     buttons=c('colvis','csv','excel')),
      extensions = 'Buttons',
      rownames = FALSE,
      escape = FALSE,
      class = "stripe nowrap compact")
    return(result)}
  )
  
  output$characterizationTable <- renderDT({
    table <- characterization %>% filter(analysisid == 1)
    table$covariatemeanwithoutcome = as.numeric(prettyHr(table$covariatemeanwithoutcome))
    table$covariatemeanwithnooutcome = as.numeric(prettyHr(table$covariatemeanwithnooutcome))
    result<-datatable(table,
                      filter="top",
                      options = list(pageLenth=25,
                                     scrollX = TRUE,
                                     dom ='Blfrtip', 
                                     buttons=c('colvis','csv','excel')),
                      extensions = 'Buttons',
                      rownames = FALSE,
                      escape = FALSE,
                      class = "stripe nowrap compact")
    return(result)}
  )
  
  output$distributionTable <- renderDataTable({
    table <- getPercentileTable(distribution,input$targetId,input$outcomeId,input$database,tolower(input$variable))
    options(digits = 2)
      
      selection = list(mode = "single", target = "row")
      table <- datatable(
        table,
        extensions = c('Buttons','FixedColumns'),
        options = list(
          aoColumnDefs = list(list(className= 'dt-left', targets = "_all")),
          pageLength = 50,
          ordering = FALSE,
          dom ='Blfrtip',
          scrollX = TRUE,
          fixedColumns = TRUE,
          buttons =
            list(
              'copy',
              'print',
              list(
                extend = 'collection',
                buttons = c('colvis','csv', 'excel'),
                text = 'Download'
              )
            )
        ),
        #options = options,
        selection = selection,
        rownames = FALSE,
        escape = FALSE,
        class = "stripe nowrap compact"
      )
      return(table)

  })
  
  # Plot titles
  output$distributionTimePlotTitle <- renderText(
   paste0(input$variable, " over the years in ",  paste(input$databases, collapse=', '))
  )
  
  # Plots

  output$distributionTimePlot <- renderPlotly({
    mySubplot <- function(myDatabasename){
      fig <- plot_ly()
      plotdata <-
        getPercentileTable(distribution,
                           input$targetId,
                           input$outcomeId,
                           NULL,
                           tolower(input$variable)) %>%
        filter(databasename == myDatabasename)
      date <- as.Date(paste(plotdata$year, "01", "01", sep = ""), "%Y%m%d")
      fig <- fig %>%
        add_trace(
          type = 'scatter',
          name = '',
          mode = 'lines+markers',
          x = date,
          y = as.numeric(plotdata$`50%`),
          showlegend = FALSE,
          hovertemplate = paste(myDatabasename,'<br>',
                                '<i>Median</i>: %{y:.2f}',
                                '<br><b>Year</b>: %{x}<br>')
        ) %>%
        
       add_trace(
          type = 'scatter',
          name = '',
          mode = 'lines+markers',
          x = date,
          y = as.numeric(plotdata$`95%`),
          showlegend = FALSE,
          hovertemplate = paste(myDatabasename,'<br>',
                                '<i>P95</i>: %{y:.2f}',
                                '<br><b>Year</b>: %{x}<br>')
        ) %>%
        layout(annotations = list(x = 0.5, y = 1.05, text=sprintf("<b>%s<b>",myDatabasename), xref='paper', yref='paper', showarrow=F))
    }
    
      subplots_list <- list()
      for(i in 1:length(input$databases)){
        subplots_list[[i]] <- mySubplot(input$databases[i])
      }
      
      fig <- subplot(subplots_list, nrows=length(input$databases),shareX = T, shareY =T)
    
    return(fig)
  })
  
  getdistributionTooltip <- function(
                                   top_px,
                                   point) {
    text <- 'Test hoover'
    #text <- gsub("-", "<", sprintf("<b>%s proportion: </b> %0.3f per 1000 persons", proportionType, point$proportion))
    # if (!is.na(point$ageGroup)) {
    #   text <- paste(text, sprintf("<b>Age group: </b> %s years", point$ageGroup), sep = "<br/>")
    #   top_px <- top_px - 15
    # }
    # if (!is.na(point$gender)) {
    #   text <- paste(text, sprintf("<b>Gender: </b> %s", point$gender), sep = "<br/>")
    #   top_px <- top_px - 15
    # }
    # if (!is.na(point$calendarYear)) {
    #   text <- paste(text, sprintf("<b>Calendar year: </b> %s", point$calendarYear), sep = "<br/>")
    #   top_px <- top_px - 15
    # }
    # if (!is.na(point$cohortCount)) {
    #   text <- paste(text, sprintf("<b>%s patients: </b> %s", proportionType, scales::comma(point$cohortCount)), sep = "<br/>")
    #   top_px <- top_px - 15
    # }
    # if (!is.na(point$numPersons)) {
    #   text <- paste(text, sprintf("<b>Denominator: </b> %s", scales::comma(point$numPersons)), sep = "<br/>")
    #   top_px <- top_px - 15
    # }
    # text <- paste(text, sprintf("<b>Database: </b> %s", point$databaseId), sep = "<br/>")
    return(list(top_px = top_px, text = text))
  }
  
  output$hoverInfoDistribution <- renderUI({
    data <- getPercentileTable(distribution,input$targetId,input$outcomeId,NULL,tolower(input$variable)) %>%
      filter(databasename %in% input$databases)
    if (is.null(data)) {
      return(NULL)
    }else {
      hover <- input$plotHoverDistribution
      point <- nearPoints(data, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) {
        return(NULL)
      }
      left_px <- hover$coords_css$x
      top_px <- hover$coords_css$y
      
      tooltip <- getProportionTooltip(top_px, point)
      style <- getHoveroverStyle(left_px = left_px, top_px = tooltip$top_px)
      div(
        style = "position: relative; width: 0; height: 0",
        wellPanel(
          style = style,
          p(HTML(tooltip$text))
        )
      )
    }
  }) 
  
  # Functionality for help messages
  showInfoBox <- function(title, htmlFileName) {
    showModal(modalDialog(
      title = title,
      easyClose = TRUE,
      footer = NULL,
      size = "l",
      HTML(readChar(htmlFileName, file.info(htmlFileName)$size) )
    ))
  }
  
  observeEvent(input$aboutInfo, {
    showInfoBox("About", "html/about.html")
  })
  
  observeEvent(input$incidenceInfo, {
    showInfoBox("Study Results", "html/incidenceInfo.html")
  })
  
  observeEvent(input$characterizationInfo, {
    showInfoBox("Study Results", "html/characterizationInfo.html")
  })
  
  observeEvent(input$distributionInfo, {
    showInfoBox("Study Results", "html/distributionInfo.html")
  })
  
})
