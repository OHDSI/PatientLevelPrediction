# EDIT FOR REPO OR DATABASE
useDatabase <- .GlobalEnv$shinySettings$useDatabase
usePlpObject <- .GlobalEnv$shinySettings$usePlpObject
useFileSystem <- .GlobalEnv$shinySettings$useFileSystem

pathToMd <- "./www/shinyDescription.md"


# set default database values
connectionDetails <- NULL
mySchema <- NULL
targetDialect <- NULL
myTableAppend <- ''

# extract data if database 
if(useDatabase){
  
  ParallelLogger::logInfo('Extracting results from database')
  
  result <- 'database'
  inputType <- 'database'
  validation <- NULL
  
  source("databaseExtras.R")
  mySchema <- Sys.getenv("shinydbSchema")
  myServer <- Sys.getenv("shinydbServer")
  myUser <- Sys.getenv("shinydbUser")
  myPassword <- Sys.getenv("shinydbPw")
  targetDialect <- Sys.getenv("shinydbDbms")
  myPort <- Sys.getenv("shinydbPort")

  myTableAppend <- Sys.getenv("shinydbTableAppend")
  
  if(myPort != ""){
    ParallelLogger::logInfo('Port')
    ParallelLogger::logInfo(paste(myPort))
    con <- pool::dbPool(drv = DatabaseConnector::DatabaseConnectorDriver(),
                        dbms = targetDialect,
                        server = myServer,
                        user = myUser,
                        password = myPassword,
                        port = myPort)
    
  } else{
    ParallelLogger::logInfo('No Port')
    con <- pool::dbPool(drv = DatabaseConnector::DatabaseConnectorDriver(),
                        dbms = targetDialect,
                        server = myServer,
                        user = myUser,
                        password = myPassword)
    
  }
  
  onStop(function() {
    if (DBI::dbIsValid(con)) {
      ParallelLogger::logInfo("Closing connection pool")
      pool::poolClose(con)
    }
  })

  summaryTable <- getDbSummary(con = con, 
                               mySchema = mySchema, 
                               targetDialect = targetDialect,
                               myTableAppend = myTableAppend)
  
} 


# if plpObect
if(usePlpObject){
  source("processing.R")
  ParallelLogger::logInfo('Loading results from plpObject')
  
  if(!is.null(.GlobalEnv$shinySettings$result)){
    
    result <- .GlobalEnv$shinySettings$result
    
    if(class(result)=='runPlp'){
      inputType <-  'plpResult'
    } else if(sum(names(result)%in%c("prediction","performanceEvaluation","inputSetting","executionSummary","model","analysisRef","covariateSummary"))==7){
      inputType <- 'plpNoClass'
    } else {
      stop('Incorrect class for input result')
    } 
 
    validation <- .GlobalEnv$shinySettings$validation
    
    summaryTable <- getSummary(inputType = inputType, 
                               result = result,
                               validation = validation)
  }
  
}

# if fileSystem
if(useFileSystem){
  source("processing.R")
  ParallelLogger::logInfo('Loading results from file system')
  
  if(!is.null(.GlobalEnv$shinySettings$result)){
    
    valid <- ifelse(class(.GlobalEnv$shinySettings$result)=='character', dir.exists(.GlobalEnv$shinySettings$result),F)
    
    if(valid){
  
    result <- 'file'
    validation <- NULL
    inputType <- 'file'
    summaryTable <- getSummary(inputType = 'file', 
                               result = .GlobalEnv$shinySettings$result)
    
    }else{
      print(paste0('invalid directory: ', .GlobalEnv$shinySettings$result))
    }
  }
}


