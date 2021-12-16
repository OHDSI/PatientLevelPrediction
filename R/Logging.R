# @file Logging.R
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

#' Create the settings for logging the progression of the analysis
#'
#' @details
#' Returns an object of class \code{logSettings} that specifies the logger settings
#'
#' @param verbosity                        Sets the level of the verbosity. If the log level is at or higher in priority than the logger threshold, a message will print. The levels are:
#'                                         \itemize{
#'                                         \item{DEBUG}{Highest verbosity showing all debug statements}
#'                                         \item{TRACE}{Showing information about start and end of steps}
#'                                         \item{INFO}{Show informative information (Default)}
#'                                         \item{WARN}{Show warning messages}
#'                                         \item{ERROR}{Show error messages}
#'                                         \item{FATAL}{Be silent except for fatal errors}
#'                                         }
#' @param timeStamp                        If TRUE a timestamp will be added to each logging statement. Automatically switched on for TRACE level.
#' @param logName                          A string reference for the logger
#' @return
#' An object of class \code{logSettings}
#' @export
createLogSettings <- function(
  verbosity = 'DEBUG',
  timeStamp = T,
  logName = 'runPlp Log'
  )
  {
  
  checkIsClass(verbosity, 'character')
  if(!verbosity%in%c("DEBUG","TRACE","INFO","WARN","FATAL","ERROR", "NONE")){
    ParallelLogger::logWarn('Incorrect verbosity string - using INFO')
    verbosity <- "INFO"
  }
  checkBoolean(timeStamp)
  ParallelLogger::logInfo(paste0('Use timeStamp: ', timeStamp))
  
  result <- list(
    verbosity = verbosity,
    timeStamp = timeStamp,
    logName = logName
    )
  
  class(result) <- 'logSettings'
  return(result)
}

createLog <- function(
  verbosity = "INFO",
  timeStamp = FALSE, 
  logName = "PLP Log",
  saveDirectory = getwd(),
  logFileName = paste0('plpLog',as.Date(Sys.Date(), "%Y%m%d"), '.txt')
)
{
  
  checkFileExists(saveDirectory, createIfNot = T)
  
  logFileName <- gsub("[[:punct:]]", "", logFileName) 
  
  if(verbosity!="NONE"){
    logger <- ParallelLogger::createLogger(
      name = logName,
      threshold = verbosity,
      appenders = list(
        ParallelLogger::createFileAppender(
          layout = ParallelLogger::layoutParallel,
          fileName = file.path(saveDirectory,paste0(logFileName, '.txt')),
          expirationTime = 60*60*48
        )
      )
    )

  }
  
  return(logger)
}

checkFileExists <- function(
  saveDirectory, 
  createIfNot = T)
{
  dirExists <- dir.exists(saveDirectory)
  if(!dirExists & createIfNot ){
    ParallelLogger::logInfo(paste0('Creating save directory at: ', saveDirectory))
    dir.create(saveDirectory, recursive = T)
  }
  return(invisible(dirExists))
}

closeLog <- function(logger){
  # stop logger
  ParallelLogger::unregisterLogger(logger)
}
