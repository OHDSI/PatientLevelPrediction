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

source("modules/prediction/modules/performanceSummary.R")
source("modules/prediction/modules/covariateSummary.R")
source("modules/prediction/modules/settings.R")
source("modules/prediction/modules/cutoff.R")
source("modules/prediction/modules/discrimination.R")
source("modules/prediction/modules/calibration.R")
source("modules/prediction/modules/netBenefit.R")
source("modules/prediction/modules/validation.R")
source("modules/prediction/modules/download.R")
source("modules/prediction/modules/diagnostic.R")

source("modules/prediction/getPerformance.R")
source("modules/prediction/emptyPlot.R")

predictionViewer <- function(id=1) {
  ns <- shiny::NS(id)
  
  shiny::tabsetPanel(
    id = ns('allView'),
    shiny::tabPanel(
      "All Models Summary",  
      performanceSummaryViewer(ns('performanceSummaryTab'))
    ),
    
    shiny::tabPanel(
      "Explore Selected Model",
      
      shiny::tabsetPanel(
        id = ns('singleView'),
        shiny::tabPanel(
          "Design Settings",
          settingsViewer(ns('settings'))
        ),
        
        shiny::tabPanel(
          "Diagnostics",
          diagnosticsViewer(ns('diagnostics'))
        ),
        
        shiny::tabPanel(
          "Model",
          covariateSummaryViewer(ns('covariateSummary'))
        ),
        
        shiny::tabPanel(
          "Threshold Dependant", 
          cutoffViewer(ns('cutoff'))
        ), 
        
        shiny::tabPanel(
          "Discrimination",  
          discriminationViewer(ns('discrimination'))
        ),
        
        shiny::tabPanel(
          "Calibration", 
          calibrationViewer(ns('calibration'))
        ),
        
        shiny::tabPanel(
          "Net Benefit", 
          nbViewer(ns('netBenefit'))
        ),
        
        shiny::tabPanel(
          "Validation",
          validationViewer(ns('validation'))
        ),
        
        shiny::tabPanel(
          "Download Model",
          downloadViewer(ns('download'))
        )
        
      )
    )
    
  )
  
}

predictionServer <- function(id, 
                             resultDatabaseSettings = list(myPort = 1)
                             ) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # connect
      if(F){
      if(resultDatabaseSettings$myPort != ""){
        ParallelLogger::logInfo('Port')
        ParallelLogger::logInfo(paste(resultDatabaseSettings$myPort))
        con <- pool::dbPool(drv = DatabaseConnector::DatabaseConnectorDriver(),
                            dbms = resultDatabaseSettings$targetDialect,
                            server = resultDatabaseSettings$myServer,
                            user = resultDatabaseSettings$myUser,
                            password = resultDatabaseSettings$myPassword,
                            port = resultDatabaseSettings$myPort)
        
      } else{
        ParallelLogger::logInfo('No Port')
        con <- pool::dbPool(drv = DatabaseConnector::DatabaseConnectorDriver(),
                            dbms = resultDatabaseSettings$targetDialect,
                            server = resultDatabaseSettings$myServer,
                            user = resultDatabaseSettings$myUser,
                            password = resultDatabaseSettings$myPassword
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
      connectionDetails <- DatabaseConnector::createConnectionDetails(
        dbms = resultDatabaseSettings$targetDialect,
        server = resultDatabaseSettings$myServer,
        user = resultDatabaseSettings$myUser,
        password = resultDatabaseSettings$myPassword,
        port = resultDatabaseSettings$myPort, 
        pathToDriver =  '/Users/jreps/Documents/drivers'
      )
      con <- DatabaseConnector::connect(connectionDetails)
      
      
      onStop(function() {
        if (DBI::dbIsValid(con)) {
          ParallelLogger::logInfo("Closing connection pool")
          DatabaseConnector::disconnect(con)
        }
      })
      
      singleViewValue <- shiny::reactive({
        input$singleView
      })
      
      # create the summaryTable and rowId
      performance <- performanceSummaryServer(
        id = 'performanceSummaryTab', 
        con = con, 
        mySchema = resultDatabaseSettings$mySchema, 
        targetDialect = resultDatabaseSettings$targetDialect,
        myTableAppend = resultDatabaseSettings$myTableAppend
        )
      
      # performance$resultTable
      # performance$rowId()
      
      # change to single model explore tab when summary table row is selected
      shiny::observeEvent(performance$rowId(), {
        print(performance$rowId())
        shiny::updateTabsetPanel(session, "allView", selected = "Explore Selected Model")
      })
      
    
      # ===========================================
      #  Single Result Exploring Modules
      # ===========================================
      
      covariateSummaryServer(
        id = 'covariateSummary',
        resultTable = performance$resultTable, 
        rowId = performance$rowId, 
        mySchema = resultDatabaseSettings$mySchema, 
        con = con,
        inputSingleView = singleViewValue,
        myTableAppend = resultDatabaseSettings$myTableAppend, 
        targetDialect = resultDatabaseSettings$targetDialect
      ) 
      
      setingsServer(
        id = 'settings', 
        resultTable = performance$resultTable, 
        rowId = performance$rowId, 
        mySchema = resultDatabaseSettings$mySchema, 
        con = con,
        inputSingleView = singleViewValue,
        myTableAppend = resultDatabaseSettings$myTableAppend, 
        targetDialect = resultDatabaseSettings$targetDialect
      )
      
      diagnosticsServer(
        id = 'diagnostics', 
        resultTable = performance$resultTable, 
        rowId = performance$rowId, 
        mySchema = resultDatabaseSettings$mySchema, 
        con = con,
        inputSingleView = singleViewValue,
        myTableAppend = resultDatabaseSettings$myTableAppend, 
        targetDialect = resultDatabaseSettings$targetDialect
      )
      

      cutoffServer(
        id = 'cutoff', 
        resultTable = performance$resultTable, 
        rowId = performance$rowId, 
        mySchema = resultDatabaseSettings$mySchema, 
        con = con,
        inputSingleView = singleViewValue,
        myTableAppend = resultDatabaseSettings$myTableAppend, 
        targetDialect = resultDatabaseSettings$targetDialect
        )
      
      discriminationServer(
        id = 'discrimination', 
        resultTable = performance$resultTable, 
        rowId = performance$rowId, 
        mySchema = resultDatabaseSettings$mySchema, 
        con = con,
        inputSingleView = singleViewValue,
        myTableAppend = resultDatabaseSettings$myTableAppend, 
        targetDialect = resultDatabaseSettings$targetDialect
        )
      
      calibrationServer(
        id = 'calibration', 
        resultTable = performance$resultTable, 
        rowId = performance$rowId, 
        mySchema = resultDatabaseSettings$mySchema, 
        con = con,
       inputSingleView = singleViewValue,
        myTableAppend = resultDatabaseSettings$myTableAppend, 
        targetDialect = resultDatabaseSettings$targetDialect
      ) 
      
      nbServer(
        id = 'netBenefit', 
        resultTable = performance$resultTable, 
        rowId = performance$rowId, 
        mySchema = resultDatabaseSettings$mySchema, 
        con = con,
        inputSingleView = singleViewValue,
        myTableAppend = resultDatabaseSettings$myTableAppend, 
        targetDialect = resultDatabaseSettings$targetDialect
        ) 
      
      validationServer(
        id = 'validation', 
        summaryTable = performance$resultTable,
        resultRow = performance$rowId,
        con = con, 
        inputSingleView = singleViewValue,
        mySchema = resultDatabaseSettings$mySchema,
        myTableAppend = resultDatabaseSettings$myTableAppend, 
        targetDialect = resultDatabaseSettings$targetDialect
      ) 
      
      
      downloadServer('download')

      
    }
  )
}
