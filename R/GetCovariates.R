# @file GetCovariates.R
#
# Copyright 2015 Observational Health Data Sciences and Informatics
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

#' Get covariate information from the database
#'
#' @description
#' Uses one or several covariate builder functions to construct covariates.
#'
#' @param covariateSettings   Either an object of type \code{covariateSettings} as created using one of
#'                            the createCovariate functions, or a list of such objects.
#' @param normalize           Should covariate values be normalized? If true, values will be divided by
#'                            the max value per covariate.
#'
#' @template GetCovarParams
#'
#' @export
getDbCovariateData <- function(connection,
                               oracleTempSchema = NULL,
                               cdmDatabaseSchema,
                               cdmVersion = "4",
                               cohortTempTable = "cohort_person",
                               rowIdField = "subject_id",
                               covariateSettings,
                               normalize = TRUE) {
  if (class(covariateSettings) == "covariateSettings") {
    fun <- attr(covariateSettings, "fun")
    args <- list(connection = connection,
                 oracleTempSchema = oracleTempSchema,
                 cdmDatabaseSchema = cdmDatabaseSchema,
                 cdmVersion = cdmVersion,
                 cohortTempTable = cohortTempTable,
                 rowIdField = rowIdField,
                 covariateSettings = covariateSettings)
    covariateData <- do.call(fun, args)
    
    if (nrow(covariateData$covariates) == 0) {
      warning("No data found")
    } else {
      open(covariateData$covariates)
      open(covariateData$covariateRef)
    }
  } else if (is.list(covariateSettings)) {
    covariateData <- NULL
    for (i in 1:length(covariateSettings)) {
      fun <- attr(covariateSettings[[i]], "fun")
      args <- list(connection = connection,
                   oracleTempSchema = oracleTempSchema,
                   cdmDatabaseSchema = cdmDatabaseSchema,
                   cdmVersion = cdmVersion,
                   cohortTempTable = cohortTempTable,
                   rowIdField = rowIdField,
                   covariateSettings = covariateSettings[[i]])
      tempCovariateData <- do.call(fun, args)
      
      if (is.null(tempCovariateData) || nrow(tempCovariateData$covariates) == 0) {
        warning("No data found")
      } else {
        if (is.null(covariateData)) {
          covariateData <- tempCovariateData
        } else {
          # TODO: handle overlap in covariate ID space
          covariateData$covariates <- ffbase::ffdfappend(covariateData$covariates,
                                                         tempCovariateData$covariates)
          covariateData$covariateRef <- ffbase::ffdfappend(covariateData$covariateRef,
                                                           tempCovariateData$covariateRef)
          covariateData$metaData <- mapply(c,
                                           covariateData$metaData,
                                           tempCovariateData$metaData,
                                           SIMPLIFY = FALSE)
        }
      }
    }
  }
  
  if (normalize) {
    writeLines("Normalizing covariates")
    covariateData$covariates <- normalizeCovariates(covariateData$covariates)
  }
  return(covariateData)
}


#' Save the covariate data to folder
#'
#' @description
#' \code{saveCovariateData} saves an object of type covariateData to folder.
#'
#' @param covariateData   An object of type \code{covariateData} as generated using
#'                        \code{getDbCovariateData}.
#' @param file            The name of the folder where the data will be written. The folder should not
#'                        yet exist.
#'
#' @details
#' The data will be written to a set of files in the folder specified by the user.
#'
#' @examples
#' # todo
#'
#' @export
saveCovariateData <- function(covariateData, file) {
  if (missing(covariateData))
    stop("Must specify covariateData")
  if (missing(file))
    stop("Must specify file")
  if (class(covariateData) != "covariateData")
    stop("Data not of class covariateData")
  
  covariates <- covariateData$covariates
  covariateRef <- covariateData$covariateRef
  ffbase::save.ffdf(covariates, covariateRef, dir = file)
  open(covariateData$covariates)
  open(covariateData$covariateRef)
  metaData <- covariateData$metaData
  save(metaData, file = file.path(file, "metaData.Rdata"))
}

#' Load the covariate data from a folder
#'
#' @description
#' \code{loadCovariateData} loads an object of type covariateData from a folder in the file system.
#'
#' @param file       The name of the folder containing the data.
#' @param readOnly   If true, the data is opened read only.
#'
#' @details
#' The data will be written to a set of files in the folder specified by the user.
#'
#' @return
#' An object of class covariateData
#'
#' @examples
#' # todo
#'
#' @export
loadCovariateData <- function(file, readOnly = FALSE) {
  if (!file.exists(file))
    stop(paste("Cannot find folder", file))
  if (!file.info(file)$isdir)
    stop(paste("Not a folder", file))
  
  temp <- setwd(file)
  absolutePath <- setwd(temp)
  
  e <- new.env()
  ffbase::load.ffdf(absolutePath, e)
  load(file.path(absolutePath, "metaData.Rdata"), e)
  result <- list(covariates = get("covariates", envir = e),
                 covariateRef = get("covariateRef", envir = e),
                 metaData = get("metaData", envir = e))
  # Open all ffdfs to prevent annoying messages later:
  open(result$covariates, readonly = readOnly)
  open(result$covariateRef, readonly = readOnly)
  
  class(result) <- "covariateData"
  rm(e)
  return(result)
}


#' @export
print.covariateData <- function(x, ...) {
  writeLines("CovariateData object")
  writeLines("")
  writeLines(paste("Cohort of interest concept ID(s):",
                   paste(x$metaData$cohortIds, collapse = ",")))
}

#' @export
summary.covariateData <- function(object, ...) {
  result <- list(metaData = object$metaData,
                 covariateCount = nrow(object$covariateRef),
                 covariateValueCount = nrow(object$covariates))
  class(result) <- "summary.covariateData"
  return(result)
}

#' @export
print.summary.covariateData <- function(x, ...) {
  writeLines("CovariateData object summary")
  writeLines("")
  writeLines(paste("Number of covariates:", x$covariateCount))
  writeLines(paste("Number of non-zero covariate values:", x$covariateValueCount))
}


#' Compute max of values binned by a second variable
#'
#' @param values   An ff object containing the numeric values to take the max of.
#' @param bins     An ff object containing the numeric values to bin by.
#'
#' @examples
#' values <- ff::as.ff(c(1, 1, 2, 2, 1))
#' bins <- ff::as.ff(c(1, 1, 1, 2, 2))
#' byMaxFf(values, bins)
#'
#' @export
byMaxFf <- function(values, bins) {
  .byMax(values, bins)
}

#' Normalize covariate values
#'
#' @details
#' Normalize covariate values by dividing by the max. This is to avoid numeric problems when fitting
#' models.
#'
#' @param covariates   An ffdf object as generated using the \code{\link{getDbCovariateData}}
#'                     function.#'
#'
#' @export
normalizeCovariates <- function(covariates) {
  if (nrow(covariates) == 0){
    return(covariates)
  } else {
    maxs <- byMaxFf(covariates$covariateValue, covariates$covariateId)
    names(maxs)[names(maxs) == "bins"] <- "covariateId"
    result <- ffbase::merge.ffdf(covariates, ff::as.ffdf(maxs))
    result$covariateValue <- result$covariateValue/result$maxs
    result$maxs <- NULL
    return(result)
  }
}
