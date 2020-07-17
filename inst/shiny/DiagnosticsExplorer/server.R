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
  
  output$proportionTable <- renderDT({
    
    analysisId <- settings %>% filter(cdmdatabasename == input$pdatabase & 
                                        tar == input$ptar &
                                        outcomeid == getId(input$poutcomeName) &
                                        cohortid ==  getId(input$ptargetName)
    ) %>% select(analysisid)
    analysisId <-analysisId$analysisid[1]
    
    if(input$pxyear){
     proportionAll <- proportion %>% dplyr::filter(agegroup == 'all')
    }else{
      proportionAll <- proportion %>% dplyr::filter(year == 'all') 
    }
    
    table <- proportionAll %>% filter(analysisid == analysisId) %>%
      select(year,agegroup,gender,n,o,opercent)
    
    if(input$pgender){
      table <- table %>% filter(gender != -1)
    }else{
      table <- table %>% filter(opercent != -1) %>% 
        group_by(year,agegroup) %>%
        summarise(n = sum(as.double(as.character(n)), na.rm = T),
                  o = sum(as.double(as.character(o)), na.rm = T),
                  opercent = sum(o, na.rm = T)*100/sum(n, na.rm = T))
    }
    
    # get the correct columns
    if(input$pxyear & input$pgender){
      table <-table %>% ungroup() %>%
        select(year,gender,n,o,opercent)
    } else if(!input$pxyear & input$pgender){
      table <-table %>% ungroup() %>%
        select(agegroup,gender,n,o,opercent)
    } else if(!input$pxyear & !input$pgender){
      table <-table %>% ungroup() %>%
        select(agegroup,n,o,opercent)
    } else if(input$pxyear & !input$pgender){
      table <- table %>% ungroup() %>%
        select(year,n,o,opercent)
    }
    
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
  
  output$proportionPlot <- renderPlotly({
    mySubplot <- function(myanalysisId,myDatabasename, myTar){
      
      if(input$pxyear){
        proportionAll <- proportion %>% dplyr::filter(agegroup == 'all')
      }else{
        proportionAll <- proportion %>% dplyr::filter(year == 'all') 
      }
      
      plotdata <- proportionAll %>% filter(analysisid == myanalysisId)
      plotdataM <- plotdata %>% filter(gender == 8507)
      plotdataF <- plotdata %>% filter(gender == 8532)
      plotdataA <- plotdata %>% filter(opercent != -1) %>% 
        group_by(year,agegroup) %>%
        summarise(n = sum(as.double(as.character(n)), na.rm = T),
                  o = sum(as.double(as.character(o)), na.rm = T),
                  opercent = sum(o, na.rm = T)*100/sum(n, na.rm = T)) %>%
        ungroup() %>% data.frame()
      
      fig1 <- plot_ly()
      if(input$pxyear){
        levels <- c(2005:2020)
      } else{
        levels <- c('0 - 4','5 - 9','10 - 14','15 - 19', '20 - 24',
                    '25 - 29','30 - 34','35 - 39','40 - 44','45 - 49',
                    '50 - 54','55 - 59','60 - 64','65 - 69','70 - 74',
                    '75 - 79','80 - 84','85 - 89','90 - 94','95 - 99')
      }
      plotdataF$var <- factor(as.character(plotdataF[,ifelse(input$pxyear,'year','agegroup')]), 
                              levels = levels,ordered = TRUE)
      plotdataM$var <- factor(as.character(plotdataM[,ifelse(input$pxyear,'year','agegroup')]), 
                                           levels = levels, ordered = TRUE)
      plotdataA$var <- factor(as.character(plotdataA[,ifelse(input$pxyear,'year','agegroup')]), 
                                           levels = levels, ordered = TRUE)
      plotdataF <- plotdataF[order(plotdataF$var),]
      plotdataM <- plotdataM[order(plotdataM$var),]
      plotdataA <- plotdataA[order(plotdataA$var),]
      
      if(input$pgender){
      fig1 <- fig1 %>%
        add_trace(
          x = plotdataF$var,
          y = plotdataF$opercent,
          type = 'scatter',
          name = '',
          mode = 'lines+markers',
          showlegend = FALSE,
          hovertemplate = paste('<b>',ifelse(input$pxyear,'year','agegroup'),'</b>: ',plotdataF$var,
                                '<br><i>T size </i>',plotdataF$n,'<br>',
                                '<i>O percent</i>: %{y:.2f}',
                                '<br>')
        )
      
      fig1 <- fig1 %>%
        add_trace(
          x = plotdataM$var,
          y = plotdataM$opercent,
          type = 'scatter',
          name = '',
          mode = 'lines+markers',
          showlegend = FALSE, 
          hovertemplate = paste('<b>',ifelse(input$pxyear,'year','agegroup'),'</b>: ',plotdataM$var,
                                '<br><i>T size </i>',plotdataM$n,'<br>',
                                '<i>O percent</i>: %{y:.2f}',
                                '<br>')
          #markers=list(color='red',dash='dashed')
        )
      } else{
        fig1 <- fig1 %>%
          add_trace(
            x = plotdataA$var,
            y = plotdataA$opercent,
            type = 'scatter',
            name = '',
            mode = 'lines+markers',
            showlegend = FALSE, 
            hovertemplate = paste('<b>',ifelse(input$pxyear,'year','agegroup'),'</b>: ',plotdataA$var,
                                  '<br><i>T size </i>',plotdataA$n,'<br>',
                                  '<i>O percent</i>: %{y:.2f}',
                                  '<br>')
          )
      }
      
      fig1
    }
    
    subplots_list <- list()
    ind <- 1
    for(i in 1:length(input$databases)){
      
      analysisId <- settings %>% filter(cdmdatabasename == input$pdatabases[i] & 
                                          tar == input$ptar &
                                          outcomeid == getId(input$poutcomeName) &
                                          cohortid ==  getId(input$ptargetName)
      ) %>% select(analysisid)
      analysisId <-analysisId$analysisid[1]
      
      # if there is a result add it 
      if(!is.na(analysisId)){
        tempPlot <- mySubplot(analysisId, input$databases[i], input$ptar)
        subplots_list[[ind]] <- tempPlot
        ind <- ind+1
      }
    }
    
    
    
    fig <- subplot(subplots_list, nrows=length(input$databases),shareX = F, shareY =F)
    
    return(fig)
  })
  
  
  output$survivalPlot <- renderPlot({
    
    data <- survival %>% 
      dplyr::filter(outcomeid == getId(input$soutcomeName)) %>%
      dplyr::filter(cohortid == getId(input$stargetName)) %>%
      dplyr::filter(cdmdatabasename == input$sdatabase)
    
    data <- data %>% dplyr::mutate(decreaseP = events/(events+natrisk))
    
    yaxis <- lapply(unique(data$censoredtime), function(i) 1-sum(data %>% dplyr::filter(censoredtime <= i) %>% dplyr::select(decreaseP)))
    
    extra <- data.frame(censoredtime = unique(data$censoredtime),
                        yaxis = unlist(yaxis))
    
    data <- data %>% inner_join(extra, by = 'censoredtime')
    
    library(ggplot2)
    ggplot() +
      geom_step(data=data, mapping=aes(x=censoredtime, y=yaxis)) +
      #geom_step(data=d, mapping=aes(x=x, y=y), direction="vh", linetype=3) +
      geom_point(data=data, mapping=aes(x=censoredtime, y=yaxis), color="red") +
      geom_vline(xintercept = 365, linetype="dotted", 
                 color = "black", size=1) +
      geom_vline(xintercept = 2*365, linetype="dotted", 
                 color = "black", size=1) +
      geom_vline(xintercept = 3*365, linetype="dotted", 
                 color = "black", size=1) + 
      geom_vline(xintercept = 4*365, linetype="dotted",
                 color = "black", size=1) +
      geom_vline(xintercept = 5*365, linetype="dotted", 
                 color = "black", size=1) +
      geom_vline(xintercept = 10*365, linetype="dotted", 
                 color = "black", size=1) +
      ylim(0, 1) + labs(x = "Time from index (days)",
                        y = "Outcome free")
    
  })

  
  output$characterizationTable <- renderDT({
    
    analysisId <- settings %>% filter(cdmdatabasename == input$cdatabase & 
                                        tar == input$ctar &
                                        outcomeid == getId(input$coutcomeName) &
                                        cohortid ==  getId(input$ctargetName)
    ) %>% select(analysisid)
    analysisId <-analysisId$analysisid[1]
    
    table <- characterization %>% filter(analysisid == analysisId)
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
  
  
  output$characterizationPlot <- renderPlotly({
    mySubplot <- function(myanalysisId,myDatabasename, myTar){
      
      plotdata <- characterization %>% filter(analysisid == myanalysisId)
      
      plotdataMeas <- plotdata %>% filter(covariatemeanwithnooutcome > 1 | covariatemeanwithoutcome > 1)
      plotdata <- plotdata %>% filter(covariatemeanwithnooutcome < 1 & covariatemeanwithoutcome < 1)
      

      fig1 <- plot_ly()
      fig2 <- plot_ly()
      
      fig1 <- fig1 %>%
        add_trace(
          type = 'scatter',
          name = '',
          mode = 'markers',
          x = plotdata$covariatemeanwithnooutcome,
          y = plotdata$covariatemeanwithoutcome,
          showlegend = FALSE,
          hovertemplate = paste(plotdata$covariatename,'<br>',
                                '<i>Mean with outcome</i>: %{y:.2f}',
                                '<br><i>Mean with no outcome</i>: %{x}<br>')
        ) %>%
        add_trace(x=c(0, 1), y=c(0, 1), 
                  type="scatter", mode="lines", name='x = y') %>%
      
        layout(annotations = list(x = 0.5, y = 1.05, 
                                  text=sprintf("<b>Database: %s - TAR: %s<b>",myDatabasename,myTar ),
                                  xref='paper', yref='paper', showarrow=F),
               xaxis = list(title = "Non-outcomes"), 
               yaxis = list(title = "Outcomes"),
               showlegend = FALSE)
      
      if(nrow(plotdataMeas)>0){
        fig2 <- fig2 %>%
          add_trace(
            type = 'scatter',
            name = '',
            mode = 'markers',
            x = plotdataMeas$covariatemeanwithnooutcome,
            y = plotdataMeas$covariatemeanwithoutcome,
            showlegend = FALSE,
            hovertemplate = paste(plotdataMeas$covariatename,'<br>',
                                  '<i>Mean with outcome</i>: %{y:.2f}',
                                  '<br><i>Mean with no outcome</i>: %{x}<br>')
          ) %>%
          add_trace(x=c(0, max(c(plotdataMeas$covariatemeanwithnooutcome,plotdataMeas$covariatemeanwithoutcome))), 
                    y=c(0, max(c(plotdataMeas$covariatemeanwithnooutcome,plotdataMeas$covariatemeanwithoutcome))), 
                    type="scatter", mode="lines", name='x = y')  %>% 
          layout(#xaxis = list(title = "Varible mean of non-outcomes"), 
                 #yaxis = list(title = "Varible mean of outcomes"),
                 showlegend = FALSE)
      }
      
      list(fig1,fig2)
      
    }
    
    subplots_list <- list()
    ind <- 1
    for(i in 1:length(input$databases)){
      
      analysisId <- settings %>% filter(cdmdatabasename == input$cdatabases[i] & 
                                          tar == input$ctar &
                                          outcomeid == getId(input$coutcomeName) &
                                          cohortid ==  getId(input$ctargetName)
      ) %>% select(analysisid)
      analysisId <-analysisId$analysisid[1]
      
      # if there is a result add it 
      if(!is.na(analysisId)){
        tempPlot <- mySubplot(analysisId, input$databases[i], input$ctar)
        subplots_list[[ind]] <- tempPlot[[1]]
        ind <- ind+1
        subplots_list[[ind]] <- tempPlot[[2]]
        ind <- ind+1
      }
    }
    

    
    fig <- subplot(subplots_list, nrows=length(input$databases),shareX = F, shareY =F)
    
    return(fig)
  })
  
  
  
  
  
  
  output$distributionTable <- renderDataTable({
    table <- getPercentileTable(distribution,getId(input$targetName),getId(input$outcomeName),input$database,tolower(input$variable))
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
                           getId(input$targetName),
                           getId(input$outcomeName),
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
  
  output$distributionBoxPlot <- renderPlot({
    
      subplots_list <- list()
      for(i in 1:length(input$databases)){
        subplots_list[[i]] <- myBoxplot(input$databases[i],getId(input$targetName),getId(input$outcomeName),input$variable)
      }
      
      require(gridExtra)
      fig <- do.call("grid.arrange", c(subplots_list, ncol=length(input$databases)))
      
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
    data <- getPercentileTable(distribution,getId(input$targetName),getId(input$outcomeName),NULL,tolower(input$variable)) %>%
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
