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
  
  connectionDetailSettings <- list(
    dbms = 'sqlite',
    server = file.path(analysesLocation, 'sqlite', 'databaseFile.sqlite')
  )
  
  databaseSettings <- list(
    connectionDetailSettings = connectionDetailSettings, 
    schema = 'main',
    tablePrefix = '',
    dbms = 'sqlite',
    server = file.path(analysesLocation, 'sqlite', 'databaseFile.sqlite'),
    user = NULL, 
    password = NULL,
    port = NULL
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
  
  connectionDetailSettings <- list(
    dbms = 'sqlite',
    server = server
  )
  
  databaseSettings <- list(
    connectionDetailSettings = connectionDetailSettings, 
    schema = 'main',
    tablePrefix = '',
    dbms = 'sqlite',
    server = server,
    user = NULL, 
    password = NULL,
    port = NULL
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
  
  connectionDetailSettings <- list(
    dbms = myDbms,
    server =  myServer, 
    user = myUser, 
    password = myPassword, 
    port = myPort
  )
  
  databaseSettings <- list(
    connectionDetailSettings = connectionDetailSettings, 
    schema = mySchema,
    tablePrefix = myTableAppend,
    dbms = myDbms,
    server = myServer,
    user = myUser, 
    password = myPassword,
    port = myPort
  )
  
  viewPlps(databaseSettings)
  
}



# code for multiple and single together
# one shiny app 

viewPlps <- function(databaseSettings){
  ensure_installed("ShinyAppBuilder") 
  ensure_installed("ResultModelManager")
  
  connectionDetails <- do.call(
    DatabaseConnector::createConnectionDetails, 
    databaseSettings$connectionDetailSettings
    )
  connection <- ResultModelManager::ConnectionHandler$new(connectionDetails)
  databaseSettings$connectionDetailSettings <- NULL
  
  shinyAppVersion <- strsplit(x = as.character(utils::packageVersion('ShinyAppBuilder')), split = '\\.')[[1]]
  
  if((shinyAppVersion[1] <= 1 & shinyAppVersion[2] < 2)){
    # Old code to be backwards compatable
    config <- ParallelLogger::loadSettingsFromJson(
      fileName = system.file(
        'shinyConfig.json', 
        package = "PatientLevelPrediction"
      )
    )
    # set database settings into system variables
  Sys.setenv("resultDatabaseDetails_prediction" = as.character(ParallelLogger::convertSettingsToJson(databaseSettings)))
  ShinyAppBuilder::viewShiny(
    config = config, 
    connection = connection
    )
  } else{
    ohdsiModulesVersion <- strsplit(x = as.character(utils::packageVersion('OhdsiShinyModules')), split = '\\.')[[1]]
    if(paste0(ohdsiModulesVersion[1], ".", ohdsiModulesVersion[2])>= 1.2){
      config <- ParallelLogger::loadSettingsFromJson(
        fileName = system.file(
          'shinyConfigUpdate.json', 
          package = "PatientLevelPrediction"
        )
      )
      databaseSettings$plpTablePrefix = databaseSettings$tablePrefix
      databaseSettings$cgTablePrefix = databaseSettings$tablePrefix
      databaseSettings$databaseTable = 'database_meta_table'
      databaseSettings$databaseTablePrefix = databaseSettings$tablePrefix
    ShinyAppBuilder::viewShiny(
      config = config, 
      connection = connection, 
      resultDatabaseSettings = databaseSettings
        )
    } else{
      ParallelLogger::logWarn('Need to update package OhdsiShinyModules')
    }
    
  }
  
  
}