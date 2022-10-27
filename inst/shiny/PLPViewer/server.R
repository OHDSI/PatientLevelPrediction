
  server <- shiny::shinyServer(function(input, output, session) { 

  #session$onSessionEnded(shiny::stopApp) 

    resultDatabaseSettings <- ParallelLogger::convertJsonToSettings(
        Sys.getenv('plpDatabaseSettings')
      )
    resultDatabaseSettings$connectionDetails <- do.call(
      DatabaseConnector::createConnectionDetails, 
      resultDatabaseSettings$connectionDetailSettings
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
    resultDatabaseSettings = resultDatabaseSettings
 )
}

   }
  )
 }
)
# helper 


addInfo <- function(item, infoId) { 

  infoTag <- tags$small( 

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

  

