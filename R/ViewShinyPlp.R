#' open a local shiny app for viewing the result of a multiple PLP analyses
#'
#' @details
#' Opens a shiny app for viewing the results of the models from various T,O, Tar and settings
#' settings.
#' @param analysesLocation  The directory containing the results (with the analysis_x folders)
#' 
#' @export
viewMultiplePlp <- function(analysesLocation){
  
  if(!file.exists(file.path(analysesLocation, 'sqlite', 'databaseFile.sqlite'))){
    stop('No database found')
  }
  
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = 'sqlite',
    server = file.path(analysesLocation, 'sqlite', 'databaseFile.sqlite')
  )
  
  databaseSettings <- list(
    connectionDetails = connectionDetails, 
    mySchema = 'main',
    myTableAppend = '',
    targetDialect = 'sqlite',
    myServer = file.path(analysesLocation, 'sqlite', 'databaseFile.sqlite'),
    myUser = NULL, 
    myPassword = NULL,
    myPort = NULL
  )
  
  viewPlps(databaseSettings)
}

#' viewPlp - Interactively view the performance and model settings
#'
#' @description
#' This is a shiny app for viewing interactive plots of the performance and the settings
#' @details
#' Either the result of runPlp and view the plots
#' @param runPlp             The output of runPlp() (an object of class 'runPlp')
#' @param validatePlp  The output of externalValidatePlp (on object of class 'validatePlp')
#' @param diagnosePlp  The output of diagnosePlp()
#' @return
#' Opens a shiny app for interactively viewing the results
#'
#' @export
viewPlp <- function(runPlp, validatePlp = NULL, diagnosePlp = NULL) {
  
  server <- insertRunPlpToSqlite(
    runPlp = runPlp, 
    externalValidatePlp = validatePlp,
    diagnosePlp = diagnosePlp
    )
  
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = 'sqlite',
    server = server
  )
  
  databaseSettings <- list(
    connectionDetails = connectionDetails, 
    mySchema = 'main',
    myTableAppend = '',
    targetDialect = 'sqlite',
    myServer = server,
    myUser = NULL, 
    myPassword = NULL,
    myPort = NULL
  )
  
  viewPlps(databaseSettings)

}


#' open a local shiny app for viewing the result of a PLP analyses from a database
#'
#' @details
#' Opens a shiny app for viewing the results of the models from a database
#' 
#' @param mySchema  Database result schema containing the result tables
#' @param myServer server with the result database 
#' @param myUser Username for the connection to the result database
#' @param myPassword Password for the connection to the result database
#' @param myDbms database management system for the result database
#' @param myPort Port for the connection to the result database
#' @param myTableAppend A string appended to the results tables (optional)
#' 
#' @export
viewDatabaseResultPlp <- function(
  mySchema, 
  myServer, 
  myUser, 
  myPassword, 
  myDbms, 
  myPort = NULL, 
  myTableAppend
  ){
  
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = myDbms,
    server =  myServer, 
    user = myUser, 
    password = myPassword, 
    port = myPort
  )
  
  databaseSettings <- list(
    connectionDetails = connectionDetails, 
    mySchema = mySchema,
    myTableAppend = myTableAppend,
    targetDialect = myDbms,
    myServer = myServer,
    myUser = myUser, 
    myPassword = myPassword,
    myPort = myPort
  )
  
  viewPlps(databaseSettings)
  
}



# code for multiple and single together
# one shiny app 

viewPlps <- function(databaseSettings){
  ensure_installed("OhdsiShinyModules")
  
  # set database settings into system variables
  Sys.setenv("plpDatabaseSettings" = as.character(ParallelLogger::convertSettingsToJson(databaseSettings)))

  appDir <- system.file("shiny", "PLPViewer", package = "PatientLevelPrediction")
  #appDir <- "/Users/jreps/Documents/github/PatientLevelPrediction/inst/shiny/PLPViewer"
  shiny::runApp(appDir) 
}

