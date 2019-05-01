# @file Parallel.R
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

#' @title registerParallelBackend
#'
#' @description Registers a parallel backend for multi core processing. The
#' number of cores will be detected automatically, unless specified otherwise.
#'
#' @param cores the number of cores to use for multi core processing
#' @param logical whether to consider logical or physical cores
#'
#' @examples
#' \dontrun{
#' # detect logical cores automatically
#' registerParallelBackend()
#'
#' # use four physical cores
#' numCores <- 4
#' registerParallelBackend(numCores, logical = FALSE)
#' }
registerParallelBackend <- function(cores = NULL, logical = TRUE) {
  # detect number of logical or physical cores
  numCores <- parallel::detectCores(logical = logical)
  
  if (!is.null(cores)) {
    if (numCores < cores) {
      # throw error if specified number of cores are unavailable
      stop("Number of cores specified is incorrect.")
    } else {
      numCores <- cores
    }
  } else {
    if (logical)
      message("Your system has ", numCores, " logical cores available.")
    else
      message("Your system has ", numCores, " physical cores available.")
  }
  
  # register parallel backend
  doParallel::registerDoParallel(cores = numCores)
}

#' @title registerSequentialBackend
#'
#' @description registerSequentialBackend registers a sequential backend for
#' single core processing.
#'
#' @examples
#' \dontrun{
#' # register a sequential backend
#' registerSequentialBackend()
#' }
registerSequentialBackend <- function() {
  # register a sequential backend
  foreach::registerDoSEQ()
}

setup_parallel <- function() {
  if (!requireNamespace("foreach", quietly = TRUE)) {
    # throw error if package is not in namespace
    stop("foreach package is required for parallel processing.")
  }
  if (foreach::getDoParWorkers() == 1) {
    # throw warning if no parallel backend is registered
    warning("No parallel backend is registered.")
  }
}
