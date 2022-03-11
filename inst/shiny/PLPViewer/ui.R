# @file Ui.R
#
# Copyright 2021 Observational Health Data Sciences and Informatics
#
# This file is part of PatientLevelPrediction
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

source("modules/summaryTable.R")
source("modules/covariateSummary.R")
source("modules/settings.R")
source("modules/cutoff.R")
source("modules/discrimination.R")
source("modules/calibration.R")
source("modules/netBenefit.R")
source("modules/validation.R")
source("modules/download.R")

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

ui <- shinydashboard::dashboardPage(
  skin = 'black',
  
  shinydashboard::dashboardHeader(
    title = "PLP Viewer", 
    tags$li(
      div(
        img(
          src = 'logo.png',
          title = "OHDSI PLP", 
          height = "40px", 
          width = "40px"),
        style = "padding-top:0px; padding-bottom:0px;"
      ),
      class = "dropdown"
    )
  ), 
  
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenuOutput("sidebarMenu")
  ), # end sidebar
  
  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      
      # help tab
      shinydashboard::tabItem(
        tabName = "Help",
        shiny::h2("Information"),
        shiny::p("Click on a row to explore the results for that model.  When you wish to explore a different model, then select the new result row and the tabs will be updated."),
        shiny::a("Demo Video", href = 'https://youtu.be/StpV40yl1UE', target='_blank')
      ),
      
      # First tab content
      shinydashboard::tabItem(
        tabName = "Description",
        shiny::includeMarkdown(path = pathToMd)
      ),
      shinydashboard::tabItem(
        tabName = "DataInfo",
        shiny::includeMarkdown(path = "./www/dataInfo.md")
      ),
      shinydashboard::tabItem(
        tabName = "Summary",
        # do this inside tabs:
        shiny::tabsetPanel(
          id = 'allView',
          shiny::tabPanel(
            "All Models Summary",  
            summaryViewer('sumTab') 
          ),
          
          shiny::tabPanel(
            "Explore Selected Model",
            
            shiny::tabsetPanel(
              id = 'singleView',
              shiny::tabPanel(
                "Development Settings",
                settingsViewer('settings')
              ),
              
              shiny::tabPanel(
                "Model",
                covariateSummaryViewer('covariateSummary')
              ),
              
              shiny::tabPanel(
                "Threshold Dependant", 
                cutoffViewer('cutoff')
              ), 
              
              shiny::tabPanel(
                "Discrimination",  
                discriminationViewer('discrimination')
              ),
              
              shiny::tabPanel(
                "Calibration", 
                calibrationViewer('calibration')
              ),
              
              shiny::tabPanel(
                "Net Benefit", 
                nbViewer('netBenefit')
              ),
              
              shiny::tabPanel(
                "Validation",
                validationViewer('validation')
              ),
              
              shiny::tabPanel(
                "Developer Info",
                shinydashboard::box(status = 'info',
                  title = "Developer Info",
                  solidHeader = TRUE,
                  side = "right",
                  shiny::tableOutput('researcherInfo')
                )
              ),
              
              shiny::tabPanel(
                "Download Model",
                downloadViewer('download')
              )
              
            )
          )
          
          
          
          
        )
      )
      
      
      
      
      
    )
  )
)
