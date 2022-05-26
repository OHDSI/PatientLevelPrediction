
  server <- shiny::shinyServer(function(input, output, session) { 

  session$onSessionEnded(shiny::stopApp) 

  

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
) , 
addInfo(
 item = shinydashboard::menuItem(
 text = "Prediction Diagnostic", 
 tabName = "PredictionDiagnostic", 
 icon = shiny::icon("stethoscope")
 ), 
 infoId = "PredictionDiagnosticInfo"
) 
) 

) 

  
    #=============
    # Helper
    #=============
    
shiny::observeEvent(input$AboutInfo, {
  showInfoBox("About", "modules/about/www/About.html")
})
shiny::observeEvent(input$PredictionInfo, {
  showInfoBox("Prediction", "modules/prediction/www/Prediction.html")
})
shiny::observeEvent(input$PredictionDiagnosticInfo, {
  showInfoBox("PredictionDiagnostic", "modules/predictionDiagnostic/www/predictionDiagnostic.html")
})

  #=============
  # module severs
  #=============
runServer <- shiny::reactiveValues( 
About = 0, 
Prediction = 0, 
PredictionDiagnostic = 0
 ) 
shiny::observeEvent(input$menu,{ 

    runServer[[input$menu]] <- runServer[[input$menu]] +1 
if(input$menu == "About" & runServer[["About"]]==1){
  aboutServer(
    id = "about"
    )
}
if(input$menu == "Prediction" & runServer[["Prediction"]]==1){
  predictionServer(
    id = "prediction",
    resultDatabaseSettings = ParallelLogger::convertJsonToSettings(
      Sys.getenv('plpDatabaseSettings')
    )
 )
}
if(input$menu == "PredictionDiagnostic" & runServer[["PredictionDiagnostic"]]==1){
  predictionDiagnosticServer(
    id = "predictionDiagnostic",
    resultDatabaseSettings = ParallelLogger::convertJsonToSettings(
      Sys.getenv('plpDatabaseSettings')
    )
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

  

