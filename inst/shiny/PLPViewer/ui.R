source("modules/about/module.R") 
source("modules/prediction/module.R") 
source("modules/predictionDiagnostic/module.R") 
ui <- shinydashboard::dashboardPage( 

         skin = "black",  

  
  shinydashboard::dashboardHeader( 

    title = "OHDSI Analysis Viewer", 

    tags$li( 

      shiny::div( 

        shiny::img( 

          src = "logo.png", 

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

  aboutViewer("about") 

),
shinydashboard::tabItem( 

  tabName = "Prediction", 

  predictionViewer("prediction") 

),
shinydashboard::tabItem( 

  tabName = "PredictionDiagnostic", 

  predictionDiagnosticViewer("predictionDiagnostic") 

)
)
)
)
