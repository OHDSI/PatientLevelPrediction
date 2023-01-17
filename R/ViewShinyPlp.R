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
  ensure_installed("shiny") # can remove this when ShinyAppBuilder is in HADES
  ensure_installed("shinydashboard") # can remove this when ShinyAppBuilder is in HADES
  ensure_installed("OhdsiShinyModules") # can remove this when ShinyAppBuilder is in HADES
  ensure_installed("ResultModelManager")
  
  connectionDetails <- do.call(
    DatabaseConnector::createConnectionDetails, 
    databaseSettings$connectionDetailSettings
    )
  connection <- ResultModelManager::ConnectionHandler$new(connectionDetails)
  databaseSettings$connectionDetailSettings <- NULL
  
  # set database settings into system variables
  Sys.setenv("plpDatabaseSettings" = as.character(ParallelLogger::convertSettingsToJson(databaseSettings)))

  app <- shiny::shinyApp(ui(), server(connection = connection))
  shiny::runApp(app) 
}



# shiny app code

server <- function(connection){
  return(
    shiny::shinyServer(function(input, output, session) {
      
      resultDatabaseSettings <- ParallelLogger::convertJsonToSettings(
        Sys.getenv('plpDatabaseSettings')
      )
      
      #============= 
      
      # sidebar menu 
      
      #============= 
      
      
      
      output$sidebarMenu <- shinydashboard::renderMenu( 
        
        shinydashboard::sidebarMenu( 
          
          id = "menu", 
          
          
          addInfo(
            item = shinydashboard::menuItem(
              text = "About", 
              tabName = "About", 
              icon = shiny::icon("info")
            ), 
            infoId = "AboutInfo"
          ) , 
          addInfo(
            item = shinydashboard::menuItem(
              text = "Prediction", 
              tabName = "Prediction", 
              icon = shiny::icon("table")
            ), 
            infoId = "PredictionInfo"
          ) 
        ) 
        
      ) 
      
      
      #=============
      # Helper
      #=============
      
      shiny::observeEvent(input$AboutInfo, {
        showInfoBox("About", OhdsiShinyModules::aboutHelperFile())
      })
      shiny::observeEvent(input$PredictionInfo, {
        showInfoBox("Prediction", OhdsiShinyModules::predictionHelperFile())
      })
      
      
      #=============
      # module severs
      #=============
      runServer <- shiny::reactiveValues( 
        About = 0, 
        Prediction = 0
      ) 
      shiny::observeEvent(input$menu,{ 
        
        runServer[[input$menu]] <- runServer[[input$menu]] +1 
        if(input$menu == "About" & runServer[["About"]]==1){
          OhdsiShinyModules::aboutServer(
            id = "about"
          )
        }
        if(input$menu == "Prediction" & runServer[["Prediction"]]==1){
          OhdsiShinyModules::predictionServer(
            #predictionServer(
            id = "prediction",
            resultDatabaseSettings = resultDatabaseSettings, 
            connectionHandler = connection
          )
        }
        
      }
      )
    }
    )
    
  )
}
# helper 


addInfo <- function(item, infoId) { 
  
  infoTag <- shiny::tags$small( 
    
    class = "badge pull-right action-button", 
    
    style = "padding: 1px 6px 2px 6px; background-color: steelblue;", 
    
    type = "button",  
    
    id = infoId, 
    
    "i" 
    
  ) 
  
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


ui <- function(){
  return(
    shinydashboard::dashboardPage( 
      
      skin = "black",  
      
      
      shinydashboard::dashboardHeader( 
        
        title = "OHDSI Analysis Viewer", 
        
        shiny::tags$li( 
          
          shiny::div( 
            
            shiny::img( 
              
              src = OhdsiShinyModules::getLogoImage(),#"logo.png", 
              
              title = "OHDSI", 
              
              height = "40px", 
              
              width = "40px" 
              
            ), 
            
            style = "padding-top:0px; padding-bottom:0px; 
"
          ),
          
          class = "dropdown" 
          
        ) 
        
      ), 
      
      
      
      shinydashboard::dashboardSidebar( 
        
        shinydashboard::sidebarMenuOutput("sidebarMenu") 
        
      ), # end sidebar 
      
      
      
      # ADD EACH MODULE SHINY AS A TAB ITEM 
      
      shinydashboard::dashboardBody( 
        
        shinydashboard::tabItems( 
          
          
          shinydashboard::tabItem( 
            
            tabName = "About", 
            
            OhdsiShinyModules::aboutViewer("about") 
            
          ),
          shinydashboard::tabItem( 
            
            tabName = "Prediction", 
            
            OhdsiShinyModules::predictionViewer("prediction") 
            
          )
        )
      )
    )
    
  )
}

