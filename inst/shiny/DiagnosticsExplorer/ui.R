library(shinydashboard)
library(shiny)
library(DT)
library(plotly)

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
      addInfo(menuItem("Incidence", tabName = "incidence"), "incidenceInfo"),
      addInfo(menuItem("Characterization", tabName = "characterization"), "characterizationInfo"),
      addInfo(menuItem("Distribution", tabName = "distribution"), "distributionInfo"),
      
      ## Option panel
      conditionalPanel(
        condition = "input.tabs=='distribution' && input.distributionTabsetPanel == 'Tables'",
        selectInput("database", "Database", databases)),
      conditionalPanel(
         condition = "input.tabs=='distribution'",

         selectInput("targetId", "Target", targetCohorts),
         selectInput("outcomeId", "Outcome", outcomeCohorts),
         selectInput("variable","distributionVar",distributionVars)
      ),
      conditionalPanel(condition = "input.tabs == 'distribution' && input.distributionTabsetPanel == 'Figures'",
                       hr(),
                       checkboxGroupInput("databases", "Database", databases, selected = databases[1])
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
    tabItem(tabName = "incidence",
        DTOutput("incidenceTable")
    ),
    tabItem(tabName = "characterization",
        dataTableOutput("characterizationTable")
    ),
    tabItem(tabName = "distribution",
            tabsetPanel(
              id = "distributionTabsetPanel",
              tabPanel(
                "Tables",
                 dataTableOutput("distributionTable")
              ),
              tabPanel(
                "Figures",
                box(
                  title = textOutput("distributionTimePlotTitle"),
                  width = 10,
                  br(),
                  plotlyOutput("distributionTimePlot")
                )
              )
            )
    )

  )
  )
)