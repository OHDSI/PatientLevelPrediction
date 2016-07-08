# @file Utilities.R
#
# Copyright 2016 Observational Health Data Sciences and Informatics
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

checkBoolean <- function(parameter,log=NULL) {
  name = deparse(substitute(parameter))
  if (!is.logical(parameter)) {
    if (!is.null(log)) {
      write(paste0('#ERROR:', name, 'needs to be a boolean'), file=log, append=T)      
    }
    stop(paste0(name, ' not defined correctly'))
  }
  return(TRUE)
}  

checkNonNegative <- function(parameter,log=NULL) {
  name = deparse(substitute(parameter))
  if (!is.numeric(parameter) | parameter<0) {
    if (!is.null(log)) {
      write(paste0('#ERROR:', name, 'needs to be >= 0'), file=log, append=T)      
    }
    stop(paste0(name, ' needs to be >= 0'))
  }
  return(TRUE)
} 

checkNotNull <- function(parameter,log=NULL) {
  name = deparse(substitute(parameter))
  if (is.null(parameter)) {
    if (!is.null(log)) {
      write(paste0('#ERROR:', name, 'cannot be empty'), file=log, append=T)      
    }
    stop(paste0(name, ' cannot be empty'))
  }
  return(TRUE)
}
