#' open a local shiny app for viewing the result of a multiple PLP analyses
#'
#' @details
#' Opens a shiny app for viewing the results of the models from various T,O, Tar and settings
#' settings.
#' @param analysesLocation  The directory containing the results (with the analysis_x folders)
#' 
#' @export
viewMultiplePlp <- function(analysesLocation){
  viewPlps(result = analysesLocation, 
           validation=NULL, 
           useDatabase = F,
           usePlpObject = F,
           useFileSystem = T)
}

#' viewPlp - Interactively view the performance and model settings
#'
#' @description
#' This is a shiny app for viewing interactive plots of the performance and the settings
#' @details
#' Either the result of runPlp and view the plots
#' @param runPlp             The output of runPlp() (an object of class 'runPlp')
#' @param validatePlp  The output of externalValidatePlp (on object of class 'validatePlp')
#' @return
#' Opens a shiny app for interactively viewing the results
#'
#' @export

viewPlp <- function(runPlp, validatePlp = NULL) {
  viewPlps(result = runPlp, 
           validation=validatePlp, 
           useDatabase = F,
           usePlpObject = T,
           useFileSystem = F)
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
viewDatabaseResultPlp <- function(mySchema, myServer, myUser, myPassword, myDbms, myPort = NULL, myTableAppend){
  
  ensure_installed('pool')
  ensure_installed('DBI')
  
  Sys.setenv("shinydbSchema" = mySchema)
  Sys.setenv("shinydbServer" = myServer)
  Sys.setenv("shinydbUser" = myUser)
  Sys.setenv("shinydbPw" = myPassword)
  Sys.setenv("shinydbDbms" = myDbms)
  if(!is.null(myPort)){
    Sys.setenv("shinydbPort" = myPort)
  }
  Sys.setenv("shinydbTableAppend" = myTableAppend)
  
  viewPlps(result = NULL, 
           validation=NULL, 
           useDatabase = T,
           usePlpObject = F,
           useFileSystem = F)
}



# code for multiple and single together
# one shiny app 

viewPlps <- function(result, 
                     validation=NULL, 
                     useDatabase = NULL, 
                     usePlpObject = NULL,
                     useFileSystem = NULL){
  ensure_installed("shiny")
  ensure_installed("shinydashboard")
  ensure_installed("shinycssloaders")
  ensure_installed("DT")
  ensure_installed("htmlwidgets")
  ensure_installed("shinyWidgets")
  ensure_installed("plotly")
 
  appDir <- system.file("shiny", "PLPViewer", package = "PatientLevelPrediction")
  shinySettings <- list(result = result, 
                        validation = validation, 
                        useDatabase = useDatabase,
                        usePlpObject = usePlpObject,
                        useFileSystem = useFileSystem)
  .GlobalEnv$shinySettings <- shinySettings
  on.exit(rm(shinySettings, envir = .GlobalEnv))
  shiny::runApp(appDir) 
}



#' Launch the Diagnostics Explorer Shiny app
#'
#' @param dataFolder       A folder where the exported zip files with the results are stored.  
#'                         Zip files containing results from multiple databases can be placed in the same
#'                         folder.
#' @param launch.browser   Should the app be launched in your default browser, or in a Shiny window.
#'                         Note: copying to clipboard will not work in a Shiny window.
#'
#' @details
#' Launches a Shiny app that allows the user to explore the diagnostics
#'
#' @export
launchDiagnosticsExplorer <- function(dataFolder, launch.browser = FALSE) {
  ensure_installed("DT")
  appDir <- system.file("shiny", "DiagnosticsExplorer", package = "PatientLevelPrediction")
  shinySettings <- list(dataFolder = dataFolder)
  .GlobalEnv$shinySettings <- shinySettings
  on.exit(rm(shinySettings, envir = .GlobalEnv))
  shiny::runApp(appDir)
}
