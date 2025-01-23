# @file Utilities.R
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

# Functions used to check parameter values

checkBoolean <- function(parameter) {
  name <- deparse(substitute(parameter))
  if (!is.logical(parameter)) {
    ParallelLogger::logError(paste0(name, " needs to be a boolean"))
    stop(paste0(name, " not defined correctly"))
  }
  return(TRUE)
}

checkHigherEqual <- function(parameter, value) {
  name <- deparse(substitute(parameter))
  if (!is.numeric(parameter) || any(parameter < value)) {
    ParallelLogger::logError(paste0(name, " needs to be >= ", value))
    stop(paste0(name, " needs to be >= ", value))
  }
  return(TRUE)
}

checkLowerEqual <- function(parameter, value) {
  name <- deparse(substitute(parameter))
  if (!is.numeric(parameter) || any(parameter > value)) {
    ParallelLogger::logError(paste0(name, " needs to be <= ", value))
    stop(paste0(name, " needs to be <= ", value))
  }
  return(TRUE)
}

checkHigher <- function(parameter, value) {
  name <- deparse(substitute(parameter))
  if (!is.numeric(parameter) || any(parameter <= value)) {
    ParallelLogger::logError(paste0(name, " needs to be > ", value))
    stop(paste0(name, " needs to be > ", value))
  }
  return(TRUE)
}

checkLower <- function(parameter, value) {
  name <- deparse(substitute(parameter))
  if (!is.numeric(parameter) || any(parameter >= value)) {
    ParallelLogger::logError(paste0(name, " needs to be < ", value))
    stop(paste0(name, " needs to be < ", value))
  }
  return(TRUE)
}

checkNotNull <- function(parameter) {
  name <- deparse(substitute(parameter))
  if (is.null(parameter)) {
    ParallelLogger::logError(paste0(name, " cannot be empty"))
    stop(paste0(name, " cannot be empty"))
  }
  return(TRUE)
}

checkIsClass <- function(parameter, classes) {
  name <- deparse(substitute(parameter))
  if (!inherits(x = parameter, what = classes)) {
    ParallelLogger::logError(paste0(name, " should be of class:", classes))
    stop(paste0(name, " is wrong class"))
  }
  return(TRUE)
}

checkInStringVector <- function(parameter, values) {
  name <- deparse(substitute(parameter))
  if (!parameter %in% values) {
    ParallelLogger::logError(paste0(name, " should be ", paste0(as.character(values), collapse = "or ")))
    stop(paste0(name, " has incorrect value"))
  }
  return(TRUE)
}

# check column names of dataframe
checkColumnNames <- function(parameter, columnNames) {
  name <- deparse(substitute(parameter))
  if (!all(columnNames %in% names(parameter))) {
    ParallelLogger::logError(paste0("Column names of ", name, " are not correct"))
    stop(paste0("Column names of ", name, " are not correct"))
  }
  return(TRUE)
}

checkIsEqual <- function(parameter, value) {
  name <- deparse(substitute(parameter))
  if (!identical(parameter, value)) {
    ParallelLogger::logError(paste0(name, " should be equal to ", value))
    stop(paste0(name, " is not equal to ", value))
  }
  return(TRUE)
}

checkFileType <- function(parameter, fileType) {
  name <- deparse(substitute(parameter))
  if (!grepl(fileType, parameter)) {
    ParallelLogger::logError(paste0(name, " should be a ", fileType, " file"))
    stop(paste0(name, " is not a ", fileType, " file"))
  }
  return(TRUE)
}

