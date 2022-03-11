# @file server.R
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

source("helpers.R")
source("emptyPlot.R")

source("modules/summaryTable.R")
source("modules/covariateSummary.R")
source("modules/settings.R")
source("modules/cutoff.R")
source("modules/discrimination.R")
source("modules/calibration.R")
source("modules/netBenefit.R")
source("modules/validation.R")
source("modules/download.R")

server <- shiny::shinyServer(function(input, output, session) {
  session$onSessionEnded(shiny::stopApp)

  #=============
  # sidebar menu
  #=============
  if(useDatabase == F){
    
    output$sidebarMenu <- shinydashboard::renderMenu(shinydashboard::sidebarMenu(id ='menu',
                                                                                 addInfo(shinydashboard::menuItem("Description", tabName = "Description", icon = shiny::icon("home")), "DescriptionInfo"),
                                                                                 addInfo(shinydashboard::menuItem("Results", tabName = "Summary", icon = shiny::icon("table")), "SummaryInfo"),
                                                                                 #addInfo(shinydashboard::menuItem("Log", tabName = "Log", icon = shiny::icon("list")), "LogInfo"),
                                                                                 addInfo(shinydashboard::menuItem("Data Info", tabName = "DataInfo", icon = shiny::icon("database")), "DataInfoInfo"),
                                                                                 addInfo(shinydashboard::menuItem("Help", tabName = "Help", icon = shiny::icon("info")), "HelpInfo")
    ))
  } else {
    
    shiny::observe({
      studyId <- shiny::parseQueryString(session$clientData$url_search)[['studyId']]
      
      print(paste0('StudyId: ', studyId))
      if(!is.null(studyId)){
        summaryTable <- summaryTable[summaryTable$studyId == studyId, ]
      }

      })
    
    output$sidebarMenu <- shinydashboard::renderMenu(shinydashboard::sidebarMenu(id ='menu',
                                                                                 addInfo(shinydashboard::menuItem("Description", tabName = "Description", icon = shiny::icon("home")), "DescriptionInfo"),
                                                                                 addInfo(shinydashboard::menuItem("Results", tabName = "Summary", icon = shiny::icon("table")), "SummaryInfo"),
                                                                                addInfo(shinydashboard::menuItem("Data Info", tabName = "DataInfo", icon = shiny::icon("database")), "DataInfoInfo"),
                                                                                 addInfo(shinydashboard::menuItem("Help", tabName = "Help", icon = shiny::icon("info")), "HelpInfo")
    ))
  }
  
  # ===========================================
  #  RESULT viewer
  # ===========================================
  
  # use the summary module to select a result via row selection
  resultRow <- summaryServer('sumTab', summaryTable)
  
  # change to single model explore tab when summary table row is selected
  shiny::observeEvent(resultRow(), {
    shiny::updateTabsetPanel(session, "allView", selected = "Explore Selected Model")
  })
  
  # this loads all the results
  plpResult <- shiny::reactive({getPlpResult(result,
                                             validation,
                                             summaryTable, 
                                             inputType, 
                                             val = F, 
                                             resultRow,
                                             mySchema = mySchema, 
                                             connectionDetails = connectionDetails,
                                             targetDialect = targetDialect, 
                                             myTableAppend = myTableAppend)})
  
  
  # ===========================================
  #  Single Result Exploring Modules
  # ===========================================
  
  covariateSummaryServer('covariateSummary',
                         plpResult,
                         summaryTable, 
                         resultRow, 
                         mySchema, 
                         con,
                         inputSingleView = input$singleView,
                         myTableAppend = myTableAppend, 
                         targetDialect = targetDialect) 
  
  setingsServer('settings', 
                plpResult)
  
  cutoffServer('cutoff', 
               plpResult)
  
  discriminationServer('discrimination', 
                       plpResult)
  
  calibrationServer('calibration', 
                    plpResult) 
  
  nbServer('netBenefit', 
           plpResult) 
  
  validationServer('validation', 
                   result,
                   validation,
                   plpResult = plpResult,
                   inputType = inputType,
                   useDatabase = useDatabase,
                   summaryTable = summaryTable,
                   resultRow = resultRow,
                   con = con, 
                   mySchema = mySchema,
                   connectionDetails = connectionDetails,
                   myTableAppend = myTableAppend, 
                   targetDialect = targetDialect) 
  
  
  downloadServer('download')
  #=======================
  # get researcher info
  #=======================
  output$researcherInfo <- shiny::renderTable(plpResult()$researcherInfo)
  
  # HELPER INFO
  shiny::observeEvent(input$DescriptionInfo, {
    showInfoBox("Description", "html/Description.html")
  })
  shiny::observeEvent(input$SummaryInfo, {
    showInfoBox("Summary", "html/Summary.html")
  })
  shiny::observeEvent(input$PerformanceInfo, {
    showInfoBox("Performance", "html/Performance.html")
  })
  shiny::observeEvent(input$ModelInfo, {
    showInfoBox("Model", "html/Model.html")
  })
  shiny::observeEvent(input$LogInfo, {
    showInfoBox("Log", "html/Log.html")
  })
  shiny::observeEvent(input$SettingsInfo, {
    showInfoBox("Settings", "html/Settings.html")
  })
  shiny::observeEvent(input$DataInfoInfo, {
    showInfoBox("DataInfo", "html/DataInfo.html")
  })
  shiny::observeEvent(input$HelpInfo, {
    showInfoBox("HelpInfo", "html/Help.html")
  })
  
 
})
