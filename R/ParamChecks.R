# @file Utilities.R
#
# Copyright 2019 Observational Health Data Sciences and Informatics
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
  name = deparse(substitute(parameter))
  if (!is.logical(parameter)) {
    ParallelLogger::logError(paste0(name, ' needs to be a boolean'))      
    stop(paste0(name, ' not defined correctly'))
  }
  return(TRUE)
}  

checkHigherEqual <- function(parameter,value) {
  name = deparse(substitute(parameter))
  if (!is.numeric(parameter) | parameter<value) {
    ParallelLogger::logError(paste0(name, ' needs to be >= ',value))      
    stop(paste0(name, ' needs to be >= ', value))
  }
  return(TRUE)
} 

checkLowerEqual <- function(parameter,value) {
  name = deparse(substitute(parameter))
  if (!is.numeric(parameter) | parameter>value) {
    ParallelLogger::logError(paste0(name, ' needs to be <= ',value))      
    stop(paste0(name, ' needs to be <= ', value))
  }
  return(TRUE)
} 

checkHigher <- function(parameter,value) {
  name = deparse(substitute(parameter))
  if (!is.numeric(parameter) | parameter<=value) {
    ParallelLogger::logError(paste0(name, ' needs to be > ',value))      
    stop(paste0(name, ' needs to be > ', value))
  }
  return(TRUE)
}

checkLower <- function(parameter,value) {
  name = deparse(substitute(parameter))
  if (!is.numeric(parameter) | parameter>=value) {
    ParallelLogger::logError(paste0(name, ' needs to be < ',value))      
    stop(paste0(name, ' needs to be < ', value))
  }
  return(TRUE)
}

checkNotNull <- function(parameter) {
  name = deparse(substitute(parameter))
  if (is.null(parameter)) {
    ParallelLogger::logError(paste0(name, ' cannot be empty'))      
    stop(paste0(name, ' cannot be empty'))
  }
  return(TRUE)
}

checkIsClass<- function(parameter,classes) {
  name = deparse(substitute(parameter))
  if (!class(parameter)%in%classes) {
    ParallelLogger::logError(paste0(name, ' should be of class:', classes))      
    stop(paste0(name, ' is wrong class'))
  }
  return(TRUE)
}

checkInStringVector<- function(parameter,values) {
  name = deparse(substitute(parameter))
  if (!parameter%in%values) {
    ParallelLogger::logError(paste0(name, ' should be ', paste0(as.character(values), collapse="or ")))      
    stop(paste0(name, ' has incorrect value'))
  }
  return(TRUE)
}
