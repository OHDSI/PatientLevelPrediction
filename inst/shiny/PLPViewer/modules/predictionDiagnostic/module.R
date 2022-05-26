# @file module.R
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


predictionDiagnosticViewer <- function(id=1) {
  ns <- shiny::NS(id)
  
  moduleFiles <- dir(file.path('modules',strsplit(id, '-')[[1]][1],"modules"), pattern = '.R', full.names = T)
  if(length(moduleFiles)>0){
    for(fileLoc in moduleFiles){
      source(
        file = fileLoc, 
        local = TRUE
        )
    }
  }
  
  shiny::tabsetPanel(
    id = ns('allViewD'),
    shiny::tabPanel(
      "All Diagnostic Summary",  
      summaryDiagnosticViewer(id = ns('sumTab'))
    ),
    
    shiny::tabPanel(
      "Explore Selected Diagnostics",
      
      shiny::tabsetPanel(
        id = ns('singleViewD'),
        shiny::tabPanel(
          "Participants",
          participantViewer(ns('participants'))
        ),
        
        shiny::tabPanel(
          "Predictors",
          predictorViewer(ns('predictors'))
        ),
        
        shiny::tabPanel(
          "Outcomes", 
          outcomeViewer(ns('outcomes'))
        )
        
      )
    )
    
  )
  
}

predictionDiagnosticServer <- function(
  id, 
  resultDatabaseSettings = list(myPort = 1)
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      diagnosticDatabaseSettings = resultDatabaseSettings
      
      moduleFiles <- dir(file.path('modules',strsplit(id, '-')[[1]][1],"modules"), pattern = '.R', full.names = T)
      if(length(moduleFiles)>0){
        for(fileLoc in moduleFiles){
          source(
            file = fileLoc, 
            local = TRUE
          )
        }
      }
      
      # connect
      if(F){
      if(diagnosticDatabaseSettings$myPort != ""){
        ParallelLogger::logInfo('Port')
        ParallelLogger::logInfo(paste(diagnosticDatabaseSettings$myPort))
        con <- pool::dbPool(drv = DatabaseConnector::DatabaseConnectorDriver(),
                            dbms = diagnosticDatabaseSettings$targetDialect,
                            server = diagnosticDatabaseSettings$myServer,
                            user = diagnosticDatabaseSettings$myUser,
                            password = diagnosticDatabaseSettings$myPassword,
                            port = diagnosticDatabaseSettings$myPort)
        
      } else{
        ParallelLogger::logInfo('No Port')
        con <- pool::dbPool(drv = DatabaseConnector::DatabaseConnectorDriver(),
                            dbms = diagnosticDatabaseSettings$targetDialect,
                            server = diagnosticDatabaseSettings$myServer,
                            user = diagnosticDatabaseSettings$myUser,
                            password = diagnosticDatabaseSettings$myPassword
                            )
        
      }
      
      onStop(function() {
        if (DBI::dbIsValid(con)) {
          ParallelLogger::logInfo("Closing connection pool")
          pool::poolClose(con)
        }
      })
      }
      
      # old connection 
      
      conDetails <- DatabaseConnector::createConnectionDetails(
        dbms = diagnosticDatabaseSettings$targetDialect, 
        server = diagnosticDatabaseSettings$server
        )
      con <- DatabaseConnector::connect(connectionDetails = conDetails)

      onStop(function() {
        if (DBI::dbIsValid(con)) {
          ParallelLogger::logInfo("Closing connection pool")
          DatabaseConnector::disconnect(con)
        }
      })
      
      # use the summary module to select a result via row selection
      summary <- summaryDiagnosticServer(
        id = 'sumTab',
        con = con, 
        mySchema = diagnosticDatabaseSettings$mySchema, 
        targetDialect = diagnosticDatabaseSettings$targetDialect,
        myTableAppend = diagnosticDatabaseSettings$myTableAppend
        )
      
      # change to single model explore tab when summary table row is selected
      shiny::observeEvent(summary$resultRow(), {
        shiny::updateTabsetPanel(session, "allViewD", selected = "Explore Selected Diagnostics")
      })
      
      # ===========================================
      #  Single Result Exploring Modules
      # ===========================================
      

        participantServer(
          id = 'participants',
          summary$summaryTable, 
          summary$resultRow, 
          mySchema = diagnosticDatabaseSettings$mySchema, 
          con,
          #inputSingleView = input$singleViewD,
          myTableAppend = diagnosticDatabaseSettings$myTableAppend, 
          targetDialect = diagnosticDatabaseSettings$targetDialect
        ) 
      
      predictorServer(
        id = 'predictors',
        summary$summaryTable, 
        summary$resultRow, 
        mySchema = diagnosticDatabaseSettings$mySchema, 
        con,
        #inputSingleView = input$singleViewD,
        myTableAppend = diagnosticDatabaseSettings$myTableAppend, 
        targetDialect = diagnosticDatabaseSettings$targetDialect
      ) 
      
      outcomeServer(
        id = 'outcomes',
        summaryTable = summary$summaryTable, 
        resultRow = summary$resultRow, 
        mySchema = diagnosticDatabaseSettings$mySchema, 
        con = con,
        #inputSingleView = input$singleViewD,
        myTableAppend = diagnosticDatabaseSettings$myTableAppend, 
        targetDialect = diagnosticDatabaseSettings$targetDialect
      ) 
      
      
      
      
    }
  )
}



