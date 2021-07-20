# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of CohortDiagnostics
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

#' open a local shiny app for viewing the result of a multiple PLP analyses
#'
#' @details
#' Opens a shiny app for viewing the results of the models from various T,O, Tar and settings
#' settings.
#' @param analysesLocation  The directory containing the results (with the analysis_x folders)
#' 
#' @export
viewMultiplePlp <- function(analysesLocation){
  viewPlps(result = analysesLocation, validation=NULL)
}

#' viewPlp - Interactively view the performance and model settings
#'
#' @description
#' This is a shiny app for viewing interactive plots of the performance and the settings
#' @details
#' Either the result of runPlp and view the plots
#' @param runPlp             The output of runPlp() (an object of class 'runPlp')
#' @param validatePlp  The output of externalValidatePlp (on object of class 'validatePlp')
#' @return
#' Opens a shiny app for interactively viewing the results
#'
#' @export

viewPlp <- function(runPlp, validatePlp = NULL) {
  viewPlps(result = runPlp, validation=validatePlp)
}


# code for multiple and single together
# one shiny app 

viewPlps <- function(result, validation=NULL){
  ensure_installed("shiny")
  ensure_installed("shinydashboard")
  ensure_installed("shinycssloaders")
  ensure_installed("DT")
  ensure_installed("htmlwidgets")
  ensure_installed("shinyWidgets")
  ensure_installed("plotly")
  
  appDir <- system.file("shiny", "PLPViewer", package = "PatientLevelPrediction")
  shinySettings <- list(result = result, validation = validation)
  .GlobalEnv$shinySettings <- shinySettings
  on.exit(rm(shinySettings, envir = .GlobalEnv))
  shiny::runApp(appDir) 
}

#' Launch the Diagnostics Explorer Shiny app
#'
#' @param dataFolder       A folder where the exported zip files with the results are stored.  
#'                         Zip files containing results from multiple databases can be placed in the same
#'                         folder.
#' @param launch.browser   Should the app be launched in your default browser, or in a Shiny window.
#'                         Note: copying to clipboard will not work in a Shiny window.
#'
#' @details
#' Launches a Shiny app that allows the user to explore the diagnostics
#'
#' @export
launchDiagnosticsExplorer <- function(dataFolder, launch.browser = FALSE) {
  ensure_installed("DT")
  appDir <- system.file("shiny", "DiagnosticsExplorer", package = "PatientLevelPrediction")
  shinySettings <- list(dataFolder = dataFolder)
  .GlobalEnv$shinySettings <- shinySettings
  on.exit(rm(shinySettings, envir = .GlobalEnv))
  shiny::runApp(appDir)
}
