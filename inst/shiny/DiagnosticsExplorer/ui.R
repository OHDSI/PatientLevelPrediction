library(shinydashboard)
library(shiny)
library(DT)
library(plotly)
library(shinyWidgets)

addInfo <- function(item, infoId) {
  infoTag <- tags$small(
    class = "badge pull-right action-button",
    style = "padding: 1px 6px 2px 6px; background-color: steelblue;",
    type = "button",
    id = infoId,
    "i"
  )
  item$children[[1]]$children <-
    append(item$children[[1]]$children, list(infoTag))
  return(item)
}

dashboardPage(
  dashboardHeader(title = "Diagnostics Explorer"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      ## Tabs
      addInfo(menuItem("About", tabName = "about"), "aboutInfo"),
      addInfo(menuItem("Proportion", tabName = "proportion"), "proportionInfo"),
      addInfo(menuItem("Survival", tabName = "survival"), "survivalInfo"),
      addInfo(menuItem("Characterization", tabName = "characterization"), "characterizationInfo"),
      addInfo(menuItem("Distribution", tabName = "distribution"), "distributionInfo"),
      
      ## Option panel
      
      # propoortion options
      # characterization options
      conditionalPanel(
        condition = "input.tabs=='proportion'",
        selectInput("ptargetName", "Target", targetCohorts),
        selectInput("poutcomeName", "Outcome", outcomeCohorts),
        selectInput("ptar","Time-at-risk",tars),
        shinyWidgets::switchInput("pgender", "Gender Split",value = TRUE, width = '80%'),
        shinyWidgets::switchInput("pxyear",value = TRUE, "By year")
      ),
      conditionalPanel(
        condition = "input.tabs=='proportion' && input.proportionTabsetPanel == 'Tables'",
        selectInput("pdatabase", "Database", databases)),
      
      conditionalPanel(condition = "input.tabs == 'proportion' && (input.proportionTabsetPanel == 'Figure' )",
                       hr(),
                       checkboxGroupInput("pdatabases", "Database", databases, selected = databases[1])
      ),
      
      
      # survival
      conditionalPanel(
        condition = "input.tabs=='survival'",
        selectInput("stargetName", "Target", targetCohorts),
        selectInput("soutcomeName", "Outcome", outcomeCohorts),
        selectInput("sdatabase", "Database", databases)
      ),
      
      
      # distribution options
      conditionalPanel(
        condition = "input.tabs=='distribution' && input.distributionTabsetPanel == 'Tables'",
        selectInput("database", "Database", databases)),
      conditionalPanel(
         condition = "input.tabs=='distribution'",

         selectInput("targetName", "Target", targetCohorts),
         selectInput("outcomeName", "Outcome", outcomeCohorts),
         selectInput("variable","distributionVar",distributionVars)
      ),
      conditionalPanel(condition = "input.tabs == 'distribution' && (input.distributionTabsetPanel == 'Time Trend' | input.distributionTabsetPanel == 'Box Plot' )",
                       hr(),
                       checkboxGroupInput("databases", "Database", databases, selected = databases[1])
      ),
      # characterization options
      conditionalPanel(
        condition = "input.tabs=='characterization'",
        selectInput("ctargetName", "Target", targetCohorts),
        selectInput("coutcomeName", "Outcome", outcomeCohorts),
        selectInput("ctar","Time-at-risk",tars)
      ),
      conditionalPanel(
        condition = "input.tabs=='characterization' && input.characterizationTabsetPanel == 'Tables'",
        selectInput("cdatabase", "Database", databases)),
      
    conditionalPanel(condition = "input.tabs == 'characterization' && (input.characterizationTabsetPanel == 'Figure' )",
                     hr(),
                     checkboxGroupInput("cdatabases", "Database", databases, selected = databases[1])
    )
    )
    
  ),
  dashboardBody(
    
    tags$body(tags$div(id="ppitest", style="width:1in;visible:hidden;padding:0px")),
    tags$script('$(document).on("shiny:connected", function(e) {
                                    var w = window.innerWidth;
                                    var h = window.innerHeight;
                                    var d =  document.getElementById("ppitest").offsetWidth;
                                    var obj = {width: w, height: h, dpi: d};
                                    Shiny.onInputChange("pltChange", obj);
                                });
                                $(window).resize(function(e) {
                                    var w = $(this).width();
                                    var h = $(this).height();
                                    var d =  document.getElementById("ppitest").offsetWidth;
                                    var obj = {width: w, height: h, dpi: d};
                                    Shiny.onInputChange("pltChange", obj);
                                });
                            '),
    
    tabItems(
    tabItem(
      tabName = "about",
      br(),
      p(
        "This interactive web-based application provides Diagnostics for a Patient-Level Prediction study."
      ),
      h3("Rationale and background"),
      p(
        " The idea is to first generate Diagnostics for the prediction problem before the execution. [MORE TO ADD]"
      ),
      h3("External links"),
      HTML("<p>Below are links for study-related artifacts that have been made available as part of this study:</p>"),
      HTML("<ul>"),
      HTML("<li>The study is registered here (to add)"),
      HTML("<li>The full source code for the study will be made available once the study is finalized"),
      HTML("</ul>"),
      h3("Development Status"),
      p(
        " The results in this application are currently under review and should be treated as preliminary at this moment."
      )
    )
    ,
    tabItem(tabName = "proportion",
            tabsetPanel(
              id = "proportionTabsetPanel",
              tabPanel(
                "Figure",
                box(
                  width = 12,
                  br(),
                  shinycssloaders::withSpinner(plotlyOutput("proportionPlot"))
                )
              ),
              tabPanel(
                "Tables",
                shinycssloaders::withSpinner(dataTableOutput("proportionTable"))
              )
            )
    )
    ,
    
    tabItem(tabName = "survival",
                box(
                  width = 12,
                  br(),
                  shinycssloaders::withSpinner(plotOutput("survivalPlot"))
                )
              
            
    ),
    
    tabItem(tabName = "characterization",
            tabsetPanel(
              id = "characterizationTabsetPanel",
              tabPanel(
                "Tables",
                shinycssloaders::withSpinner(dataTableOutput("characterizationTable"))
              ),
              tabPanel(
                "Figure",
                box(
                  width = 12,
                  br(),
                  shinycssloaders::withSpinner(plotlyOutput("characterizationPlot"))
                )
              )
            )
    ),
    tabItem(tabName = "distribution",
            tabsetPanel(
              id = "distributionTabsetPanel",
              tabPanel(
                "Tables",
                 dataTableOutput("distributionTable")
              ),
              tabPanel(
                "Time Trend",
                box(
                  title = textOutput("distributionTimePlotTitle"),
                  width = 12,
                  br(),
                  shinycssloaders::withSpinner(plotlyOutput("distributionTimePlot"))
                )
              ),
              tabPanel(
                "Box Plot",
                box(
                  width = 12,
                  br(),
                  shinycssloaders::withSpinner(plotOutput("distributionBoxPlot"))
                )
              )
              
            )
    )

  )
  )
)
