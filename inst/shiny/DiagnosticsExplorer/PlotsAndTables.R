getPercentileTable <- function(table,mytargetid,myoutcomeid,mydatabasename,variable){
  myVar <- sym(variable)

  result <- table %>% 
  filter(targetid==mytargetid & 
         outcomeid==myoutcomeid)
  result<-  result %>% filter(type %in% c('N','0%','1%','5%','25%','50%','75%','95%','99%','100%')) 
  
  if (!is.null(mydatabasename)){
    result <- result %>% filter(databasename==mydatabasename) %>%
    select(year,myVar,type) %>%
    arrange(year) %>%
    spread(key=type,value=myVar,fill='NA') %>%
    select(year,N,'0%','1%','5%','25%','50%','75%','95%','99%','100%')
  } else {
    result <- result %>% select(databasename,year,myVar,type) %>%
      arrange(year) %>%
      spread(key=type,value=myVar,fill='NA') %>%
      select(databasename,year,N,'0%','1%','5%','25%','50%','75%','95%','99%','100%')
  }

  return(result)
}
