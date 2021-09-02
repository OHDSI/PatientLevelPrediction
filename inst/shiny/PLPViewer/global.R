source("processing.R")
library(dplyr)
ParallelLogger::clearLoggers()
logger <- ParallelLogger::createLogger(name = "SIMPLE",
                                       threshold = "INFO",
                                       appenders = list(ParallelLogger::createConsoleAppender(layout = ParallelLogger::layoutTimestamp)))
ParallelLogger::registerLogger(logger)
# EDIT FOR REPO OR DATABASE
useDatabase <- T
pathToMd <- ifelse(useDatabase==F, "./www/shinyDescription.md" ,"./www/libraryDescription.md")

# set default
##Sys.getenv("shinydbDatabase")
mySchema <- Sys.getenv("covid19vaccinationplpdbSchema")

if(useDatabase){
  source("repositoryExtras.R")
  result <- 'database'
  validation <- NULL
  
  con <- pool::dbPool(drv = DatabaseConnector::DatabaseConnectorDriver(),
                                 dbms = "postgresql",
                                 server = Sys.getenv("shinydbServer"),
                                 # port = Sys.getenv("shinydbPort"),
                                 user = Sys.getenv("covid19vaccinationplpdbUser"),
                                 password = Sys.getenv("covid19vaccinationplpdbPw"))

  onStop(function() {
    if (DBI::dbIsValid(con)) {
      writeLines("Closing connection pool")
      pool::poolClose(con)
    }
  })
  
  summaryTable <- getDbSummary(con = con, mySchema = mySchema)

} else{
  if(is.null(.GlobalEnv$shinySettings$result)){
    result <- 'data'
    print('Extracting results from data folder')
  } else{
    result <- .GlobalEnv$shinySettings$result
    print('Extracting results from .GlobalEnv$shinySettings')
  }
  
  if(is.null(.GlobalEnv$shinySettings$validation)){
    validation <- NULL
  } else{
    validation <- .GlobalEnv$shinySettings$validation
  }
  
  inputType <- checkPlpInput(result) # this function checks 
  if(!class(validation)%in%c('NULL', 'validatePlp')){
    stop('Incorrect validation class')
  }
  if(inputType == 'file' & !is.null(validation)){
    warning('Validation input ignored when result is a directory location')
  }
  
  summaryTable <- getSummary(result, inputType, validation)
  
}


myResultList <- lapply(1:nrow(summaryTable), function(i){paste( 'Dev:', as.character(summaryTable$Dev[i]),
                                                                '- Val:',as.character(summaryTable$Val[i]),
                                                                 '-T:', as.character(summaryTable$T[i]),
                                                               '- O:',as.character(summaryTable$O[i]),
                                                               '- TAR:', as.character(summaryTable$TAR[i]),
                                                               '- Model:', as.character(summaryTable$Model[i]),
                                                               'Predictor:', as.character(summaryTable$covariateSettingId[i]))})


