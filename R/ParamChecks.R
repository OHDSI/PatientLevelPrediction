# @file ParamChecks.R
#
# Copyright 2025 Observational Health Data Sciences and Informatics
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

createDir <- function(
    saveDirectory,
    createIfNot = TRUE) {
  dirExists <- dir.exists(saveDirectory)
  if (!dirExists && createIfNot) {
    ParallelLogger::logInfo(paste0("Creating save directory at: ", saveDirectory))
    dir.create(saveDirectory, recursive = TRUE)
  }
  return(invisible(dirExists))
}

checkFileExists <- function(parameter) {
  name <- deparse(substitute(parameter))
  if (!file.exists(parameter)) {
    ParallelLogger::logError(paste0(name, " does not exist"))
    stop(paste0(name, " does not exist"))
  }
  return(TRUE)
}

checkDataframe <- function(parameter, columns, columnTypes) {
  name <- deparse(substitute(parameter))
  # Check if 'parameter' is a dataframe
  if (!is.data.frame(parameter)) {
    ParallelLogger::logError(paste0(name, " should be a dataframe"))
    stop(paste0(name, " is not a dataframe"))
  }

  # Check if all specified columns exist in the dataframe
  if (!all(columns %in% names(parameter))) {
    ParallelLogger::logError(paste0("Column names of ", name, " are not correct"))
    stop(paste0("Column names of ", name, " are not correct"))
  }

  # Ensure 'columnTypes' is a list with the same length as 'columns'
  if (length(columnTypes) != length(columns)) {
    stop("The length of 'columnTypes' must be equal to the length of 'columns'")
  }

  # Extract the classes of the specified columns
  colClasses <- sapply(parameter[columns], function(x) class(x)[1])

  # Check each column's class against its acceptable types
  typeCheck <- mapply(function(colClass, acceptableTypes) {
    colClass %in% acceptableTypes
  },
  colClass = colClasses,
  acceptableTypes = columnTypes)

  # If any column doesn't match its acceptable types, throw an error
  if (!all(typeCheck)) {
    errorCols <- columns[!typeCheck]
    expectedTypes <- columnTypes[!typeCheck]
    actualTypes <- colClasses[!typeCheck]

    # Construct detailed error messages for each problematic column
    errorMessages <- mapply(function(col, expTypes, actType) {
      paste0(
        "Column '", col, "' should be of type(s): ", paste(expTypes, collapse = ", "),
        " but is of type '", actType, "'."
      )
    },
    col = errorCols,
    expTypes = expectedTypes,
    actType = actualTypes,
    SIMPLIFY = FALSE)

    # Log and stop with the error messages
    ParallelLogger::logError(paste0("Column types of ", name, " are not correct"))
    stop(paste0("Column types of ", name, " are not correct.\n",
                paste(errorMessages, collapse = "\n")))
  }
  return(TRUE)
}
