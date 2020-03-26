getPercentileTable <- function(table,mytargetid,myoutcomeid,mydatabasename,columnName){
  myVar <- dplyr::sym(columnName) # giving error on my setup
  
  result <- table %>% 
  filter(targetid==mytargetid & 
         outcomeid==myoutcomeid)
  result<-  result %>% filter(type %in% c('N','0%','1%','5%','25%','50%','75%','95%','99%','100%')) 
  
  if (!is.null(mydatabasename)){
    result <- result %>% filter(databasename==mydatabasename) %>%
    select(!!c(year = 'year',myVar = columnName, type ='type')) %>%
    arrange(year) %>%
    spread(key=type,value=myVar,fill='NA') %>%
    select(year,N,'0%','1%','5%','25%','50%','75%','95%','99%','100%')
  } else {
    result <- result %>% select(!!c(databasename = 'databasename', year = 'year', myVar = columnName, type ='type')) %>%
      arrange(year) %>%
      spread(key=type,value=myVar,fill='NA') %>%
      select(databasename,year,N,'0%','1%','5%','25%','50%','75%','95%','99%','100%')
  }

  return(result)
}


# box plots 
myBoxplot <- function(myDatabasename,targetId,outcomeId,variable){
  
  plotdata <- getPercentileTable(distribution,
                                 targetId,
                                 outcomeId,
                                 NULL,
                                 tolower(variable)) %>%
    filter(databasename == myDatabasename)
  
  if(nrow(plotdata)==0){
    return(ggplot2::ggplot()) +
      ggplot2::labs(title=paste('Database:', myDatabasename))
  } else {
    
    plotdata[plotdata=='NA'] <- 0
    colnames(plotdata) <- c('databasename','year','N','p0','p1','p5','p25','p50','p75','p95','p99','p100')
    
    plotResult <- ggplot2::ggplot(plotdata, ggplot2::aes(x=as.factor(year),
                                                         ymin= ifelse(is.na(p0), 0, as.double(as.character(p0))),
                                                         lower= ifelse(is.na(p25), 0, as.double(as.character(p25))),
                                                         middle= ifelse(is.na(p50), 0, as.double(as.character(p50))),
                                                         upper= ifelse(is.na(p75), 0, as.double(as.character(p75))), 
                                                         ymax= ifelse(is.na(p100), 0, as.double(as.character(p100))), 
                                                         color= as.factor(databasename))) + 
      ggplot2::geom_hline(yintercept=365, linetype="dashed", color = "black") +
      ggplot2::geom_hline(yintercept=365*2, linetype="dashed", color = "black") +
      ggplot2::geom_hline(yintercept=365*3, linetype="dashed", color = "black") +
      ggplot2::geom_hline(yintercept=365*4, linetype="dashed", color = "black") +
      ggplot2::geom_hline(yintercept=365*5, linetype="dashed", color = "black") +
      #ggplot2::coord_flip() +
      ggplot2::geom_boxplot(stat="identity")  +
      ggplot2::scale_x_discrete("Year") + 
      ggplot2::scale_y_continuous("Time in Days") + 
      ggplot2::theme(legend.position="none") +
      ggplot2::labs(title=paste('Database:', myDatabasename))
    
    return(plotResult)
  }
}

