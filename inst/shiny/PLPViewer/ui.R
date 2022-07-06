ui <- shinydashboard::dashboardPage( 

         skin = "black",  

  
  shinydashboard::dashboardHeader( 

    title = "OHDSI Analysis Viewer", 

    tags$li( 

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
