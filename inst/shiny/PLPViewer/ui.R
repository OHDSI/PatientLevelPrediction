# @file Ui.R
#
# Copyright 2020 Observational Health Data Sciences and Informatics
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

library(shiny)
library(plotly)
library(shinycssloaders)
library(shinydashboard)

addInfo <- function(item, infoId) {
  infoTag <- tags$small(class = "badge pull-right action-button",
                        style = "padding: 1px 6px 2px 6px; background-color: steelblue;",
                        type = "button", 
                        id = infoId,
                        "i")
  item$children[[1]]$children <- append(item$children[[1]]$children, list(infoTag))
  return(item)
}

ui <- shinydashboard::dashboardPage(skin = 'black',
                                    
                                    shinydashboard::dashboardHeader(title = "PLP Viewer", 
                                                                    
                                                                    tags$li(div(img(src = 'logo.png',
                                                                                    title = "OHDSI PLP", height = "40px", width = "40px"),
                                                                                style = "padding-top:0px; padding-bottom:0px;"),
                                                                            class = "dropdown")
                                                                    
                                                                    
                                    ), 
                                    
                                    shinydashboard::dashboardSidebar(
                                      shinydashboard::sidebarMenuOutput("sidebarMenu"),
                                      
                                      # scroller performanace - make conditional
                                      conditionalPanel(condition = "input.menu=='Performance'",
                                                       shiny::sliderInput("slider1", 
                                                                          shiny::span("Threshold: ", shiny::textOutput('threshold'), style="color:white;font-family: Arial;font-size:14px;"), 
                                                                          min = 1, max = 100, value = 50, ticks = F
                                                       )
                                      ),
                                      
                                      conditionalPanel(condition = "input.menu=='Performance' || input.menu=='Model' || input.menu=='Settings' || input.menu=='Log'",
                                                       
                                                       shinyWidgets::pickerInput("selectResult", "Result:",
                                                                                 choices = myResultList,
                                                                                 selected = myResultList[[1]],
                                                                                 options = shinyWidgets::pickerOptions(liveSearch = TRUE, dropupAuto = FALSE, header = 'Select a result here to view...'),
                                                                                 multiple = FALSE),
                                                       
                                                       shiny::tableOutput("sideSettings"),
                                                       shiny::tableOutput("sideSettings2")
                                                       
                                                       
                                                       #shiny::selectInput(
                                                       #  "selectResult",
                                                       #  label = shiny::h4("Result:"),
                                                       #  choices = myResultList
                                                       #)            
                                                       
                                      )
                                      
                                      
                                    ), # end sidebar
                                    
                                    shinydashboard::dashboardBody(
                                      shinydashboard::tabItems(
                                        
                                        # help tab
                                        shinydashboard::tabItem(tabName = "Help",
                                                                shiny::h2("Information"),
                                                                shiny::p("Click on a row to explore the results for that model.  When you wish to explore a different model, then select the new result row and the tabs will be updated."),
                                                                shiny::a("Demo Video", href = 'https://youtu.be/StpV40yl1UE', target='_blank')
                                        ),
                                        
                                        # First tab content
                                        shinydashboard::tabItem(tabName = "Description",
                                                                shiny::includeMarkdown(path = pathToMd)
                                        ),
                                        shinydashboard::tabItem(tabName = "DataInfo",
                                                                shiny::includeMarkdown(path = "./www/dataInfo.md")
                                                                
                                        ),
                                        shinydashboard::tabItem(tabName = "Summary",
                                                                
                                                                shiny::fluidRow(
                                                                  shiny::column(2,
                                                                                shiny::h4('Filters'),
                                                                                shiny::selectInput('modelSettingName', 'Model:', c('All',unique(as.character(summaryTable$Model)))),
                                                                                shiny::selectInput('devDatabase', 'Development Database', c('All',unique(as.character(summaryTable$Dev)))),
                                                                                shiny::selectInput('valDatabase', 'Validation Database', c('All',unique(as.character(summaryTable$Val)))),
                                                                                shiny::selectInput('T', 'Target Cohort', c('All',unique(as.character(summaryTable$`T`)))),
                                                                                shiny::selectInput('O', 'Outcome Cohort', c('All',unique(as.character(summaryTable$`O`)))),
                                                                                shiny::selectInput('TAR', 'Time-at-risk end:', c('All',unique(as.character(summaryTable$TAR))))
                                                                  ),
                                                                  
                                                                  shiny::column(10, style = "background-color:#F3FAFC;",
                                                                                
                                                                                # do this inside tabs:
                                                                      shiny::tabsetPanel(
                                                                                
                                                                              shiny::tabPanel("Results",                  
                                                                                shiny::div(DT::dataTableOutput('summaryTable'), 
                                                                                           style = "font-size:70%")),
                                                                              
                                                                              shiny::tabPanel("Development Settings",
                                                                                              shiny::h3('Model Settings: ',
                                                                                                        shiny::a("help", href="https://ohdsi.github.io/PatientLevelPrediction/reference/index.html", target="_blank")
                                                                                              ),
                                                                                              DT::dataTableOutput('modelTable'),

                                                                                              # shiny::tabPanel("Population Settings",
                                                                                              shiny::h3('Population Settings: ',
                                                                                                        shiny::a("help", href="https://ohdsi.github.io/PatientLevelPrediction/reference/createStudyPopulation.html", target="_blank")
                                                                                              ),
                                                                                              DT::dataTableOutput('populationTable'),

                                                                                              # shiny::tabPanel("Covariate Settings",
                                                                                              shiny::h3('Covariate Settings: ',
                                                                                                        shiny::a("help", href="http://ohdsi.github.io/FeatureExtraction/reference/createCovariateSettings.html", target="_blank")
                                                                                              ),
                                                                                              DT::dataTableOutput('covariateTable'),


                                                                                              shiny::h3("Hyper-parameters"),
                                                                                              DT::dataTableOutput('hpTable'),
                                                                                              shiny::h3("Attrition"),
                                                                                              DT::dataTableOutput('attritionTable')),
 

                                                                            shiny::tabPanel("Threshold Dependant", 
                                                                                              shiny::fluidRow(
                                                                                              shiny::column(width = 12,
                                                                                                shinydashboard::box(width = 12,
                                                                                                                 title = "Dashboard",
                                                                                                                 status = "warning", solidHeader = TRUE,
                                                                                                                 shinydashboard::infoBoxOutput("performanceBoxThreshold"),
                                                                                                                 shinydashboard::infoBoxOutput("performanceBoxIncidence"),
                                                                                                                 shinydashboard::infoBoxOutput("performanceBoxPPV"),
                                                                                                                 shinydashboard::infoBoxOutput("performanceBoxSpecificity"),
                                                                                                                 shinydashboard::infoBoxOutput("performanceBoxSensitivity"),
                                                                                                                 shinydashboard::infoBoxOutput("performanceBoxNPV")
                                                                                                                   
                                                                                               ),
                                                                                               shinydashboard::box(width = 12,
                                                                                                                   title = "Cutoff Performance",
                                                                                                                   status = "warning", solidHeader = TRUE,
                                                                                                                   shiny::tableOutput('twobytwo')
                                                                                                                   #infoBoxOutput("performanceBox"),
                                                                                               )
                                                                                 )
                                                                               )
                                                                             
                                                                                 
                                                                        ), # end summary
                                                                        tabPanel("Discrimination",  
                                                                              shiny::fluidRow(
                                                                               shinydashboard::box( status = 'info',
                                                                                                        title = actionLink("rocHelp", "ROC Plot", icon = icon("info")),
                                                                                                    solidHeader = TRUE,
                                                                                                    shinycssloaders::withSpinner(plotly::plotlyOutput('roc'))),
                                                                               shinydashboard::box(status = 'info',
                                                                                                   title = actionLink("prcHelp", "Precision recall plot", icon = icon("info")),
                                                                                                   solidHeader = TRUE,
                                                                                                   side = "right",
                                                                                                   shinycssloaders::withSpinner(plotly::plotlyOutput('pr')))),
                                                                             
                                                                              shiny::fluidRow(
                                                                               shinydashboard::box(status = 'info',
                                                                                                   title = actionLink("f1Help", "F1 Score Plot", icon = icon("info")),
                                                                                                   solidHeader = TRUE,
                                                                                                   shinycssloaders::withSpinner(plotly::plotlyOutput('f1'))),
                                                                               shinydashboard::box(status = 'info',
                                                                                                   title = actionLink("boxHelp","Box Plot", icon = icon("info")),
                                                                                                   solidHeader = TRUE,
                                                                                                   side = "right",
                                                                                                   shinycssloaders::withSpinner(shiny::plotOutput('box')))),
                                                                             
                                                                             shiny::fluidRow(
                                                                               shinydashboard::box(status = 'info',
                                                                                                   title = actionLink("predDistHelp","Prediction Score Distribution", icon = icon("info")),
                                                                                                   solidHeader = TRUE,
                                                                                                   shinycssloaders::withSpinner(shiny::plotOutput('preddist'))),
                                                                               shinydashboard::box(status = 'info',
                                                                                                   title = actionLink("prefDistHelp","Preference Score Distribution", icon = icon("info")),
                                                                                                   solidHeader = TRUE,
                                                                                                   side = "right",
                                                                                                   shinycssloaders::withSpinner(shiny::plotOutput('prefdist'))))
         
                                                                    ),
                                                                    tabPanel("Calibration", 
                                                                             
                                                                             shiny::fluidRow(
                                                                               shinydashboard::box(status = 'info', width = 3,
                                                                                                   title = 'Settings',
                                                                                                   solidHeader = TRUE,
                                                                                                   uiOutput('recalSelect')
                                                                               ),
                                                                               shinydashboard::box(status = 'info', width = 9,
                                                                                                   title = 'Summary',
                                                                                                   solidHeader = TRUE,
                                                                                                   shiny::tableOutput('calTable')
                                                                               )
                                                                             ),
                                                                             
                                                                             shiny::fluidRow(
                                                                               shinydashboard::box(status = 'info',
                                                                                                   title = actionLink("calHelp","Calibration Plot", icon = icon("info")),
                                                                                                   solidHeader = TRUE,
                                                                                                   shinycssloaders::withSpinner(shiny::plotOutput('cal'))),
                                                                               shinydashboard::box(status = 'info',
                                                                                                   title = actionLink("demoHelp","Demographic Plot", icon = icon("info")),
                                                                                                   solidHeader = TRUE,
                                                                                                   side = "right",
                                                                                                   shinycssloaders::withSpinner(shiny::plotOutput('demo')))
                                                                             )

                                                                    ),
                                                                    
                                                                    tabPanel("Net Benefit", 
                                                                             
                                                                             shiny::fluidRow(
                                                                               shinydashboard::box(status = 'info', width = 12,
                                                                                                   title = 'Settings',
                                                                                                   solidHeader = TRUE,
                                                                                                   uiOutput('nbSelect')
                                                                               )
                                                                               
                                                                             ),
                                                                             
                                                                             shiny::fluidRow(
                                                                               shinydashboard::box(status = 'info', width = 6,
                                                                                                   title = 'Net Benefit Plot',
                                                                                                   solidHeader = TRUE,
                                                                                                   side = "right",
                                                                                                   shinycssloaders::withSpinner(shiny::plotOutput('nbPlot'))),
                                                                               
                                                                               shinydashboard::box(status = 'info', width = 6,
                                                                                                   title = 'Summary',
                                                                                                   solidHeader = TRUE,
                                                                                                   shiny::tableOutput('nbTable')
                                                                               )
                                                                             )
                                                                             
                                                                    ),
                                                                    
                                                                    shiny::tabPanel("Validation",
                                                                                    
                                                                                    shiny::div(DT::dataTableOutput('validationTable'), 
                                                                                               style = "font-size:70%"),
                                                                                    
                                                                                    shiny::fluidRow(
                                                                                      shinydashboard::box(status = 'info',
                                                                                                          title = actionLink("rocHelp","Roc Plot", icon = icon("info")),
                                                                                                          solidHeader = TRUE,
                                                                                                          shinycssloaders::withSpinner(shiny::plotOutput('valRoc'))),
                                                                                      shinydashboard::box(status = 'info',
                                                                                                          title = actionLink("calHelp","Calibration Plot", icon = icon("info")),
                                                                                                          solidHeader = TRUE,
                                                                                                          side = "right",
                                                                                                          shinycssloaders::withSpinner(shiny::plotOutput('valCal')))
                                                                                    ),
                                                                                    shiny::div(DT::dataTableOutput('databaseInfo'),
                                                                                               style = "font-size:70%")
                                                                    ),
                                                                    # shiny::conditionalPanel("useDatabase == T", 
                                                                    #placeholder for developer info
                                                                                shiny::tabPanel("Developer Info",
                                                                                      shinydashboard::box(status = 'info',
                                                                                                          title = "Developer Info",
                                                                                                          solidHeader = TRUE,
                                                                                                          side = "right",
                                                                                                          shiny::tableOutput('researcherInfo')
                                                                                                          )))
                                        ))),

                                        
                                        # 3rd tab
                                        shinydashboard::tabItem(tabName = "Model", 
                                                                shiny::fluidRow(
                                                                  shinydashboard::box( status = 'info',
                                                                                       title = "Binary", solidHeader = TRUE,
                                                                                       shinycssloaders::withSpinner(plotly::plotlyOutput('covariateSummaryBinary'))),
                                                                  shinydashboard::box(status = 'info',
                                                                                      title = "Measurements", solidHeader = TRUE,
                                                                                      side = "right",
                                                                                      shinycssloaders::withSpinner(plotly::plotlyOutput('covariateSummaryMeasure')))),
                                                                shiny::fluidRow(width=12,
                                                                                shinydashboard::box(status = 'info', width = 12,
                                                                                                    title = "Covariates", solidHeader = TRUE,
                                                                                                    DT::dataTableOutput('modelCovariateInfo'))),
                                                                shiny::fluidRow(width=12,
                                                                                shinydashboard::box(status = 'info', width = 12,
                                                                                                    title = "Model Table", solidHeader = TRUE,
                                                                                                    shiny::downloadButton("downloadData", "Download Model"),
                                                                                                    DT::dataTableOutput('modelView')))
                                        ),
                                        
                                        # 4th tab
                                        shinydashboard::tabItem(tabName = "Log", 
                                                                shiny::verbatimTextOutput('log')
                                        )
                                        
                                        
                                      )
                                    )
)
