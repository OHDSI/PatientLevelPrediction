# need to add mySchema and connectionDetails to input
getPlpResult <- function(result, 
                         validation,
                         summaryTable, 
                         inputType,
                         resultRow, 
                         val = F, 
                         mySchema = NULL, 
                         connectionDetails = NULL,
                         targetDialect = NULL, 
                         myTableAppend = NULL){
  
##ind <- resultRow()

  if(!is.null(resultRow())){
    print('Loading data')
    print(paste0('input: ', inputType))
    
  if(inputType == 'database'){
    tempResult <- loadPlpFromDb(summaryTable[resultRow(),], mySchema, con, val = val, targetDialect, myTableAppend)
    return(tempResult)
  } else if(inputType == 'plpResult'){
    i <- resultRow()
    if(i == 1){
      tempResult <- result
      tempResult$type <- 'test'
    }else{
      tempResult <- validation$validation[[i-1]]
      tempResult$type <- 'validation'
    }
  }else if(inputType == 'plpNoClass'){
    tempResult <- result
    tempResult$type <- 'validation'
  }else if( inputType == 'file') {
    
    # support rds, csv and runPlp objects
    tempResult <- NULL
    loc <- summaryTable[resultRow(),]$plpResultLocation
    locLoaderFunc <- summaryTable[resultRow(),]$plpResultLoad

    if(dir.exists(as.character(loc))){
      tempResult <- do.call(as.character(locLoaderFunc), list(as.character(loc)))
      tempResult$type <- ifelse(length(grep('/Validation',loc))>0,'validation','test')
    }
  }else {
    stop('Incorrect class')
  }
  return(tempResult)
  } else{
    return(NULL)
  }
}




addInfo <- function(item, infoId) {
  infoTag <- tags$small(class = "badge pull-right action-button",
                        style = "padding: 1px 6px 2px 6px; background-color: steelblue;",
                        type = "button", 
                        id = infoId,
                        "i")
  item$children[[1]]$children <- append(item$children[[1]]$children, list(infoTag))
  return(item)
}
    
showInfoBox <- function(title, htmlFileName) {
  shiny::showModal(shiny::modalDialog(
    title = title,
    easyClose = TRUE,
    footer = NULL,
    size = "l",
    shiny::HTML(readChar(htmlFileName, file.info(htmlFileName)$size) )
  ))
}
