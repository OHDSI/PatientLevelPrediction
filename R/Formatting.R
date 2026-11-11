# @file formatting.R
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

#' Convert the plpData in COO format into a sparse R matrix
#'
#' @description
#' Converts the standard plpData to a sparse matrix
#'
#' @details
#' This function converts the covariates `Andromeda` table in COO format into a sparse matrix from
#' the package Matrix. The matrix is built in one query on the covariates, without copying them.
#' @param plpData                       An object of type \code{plpData} with covariate in coo format - the patient level prediction
#'                                      data extracted from the CDM.
#' @param cohort                        If specified the plpData is restricted to the rowIds in the cohort (otherwise plpData$labels is used)
#' @param map                           A covariate map (telling us the column number for covariates), with
#'                                      columns covariateId and columnId, for example from a fitted model.
#'                                      Covariates that are not in the map are left out.
#'
#' @return
#' A list with the following components: \describe{
#' \item{dataMatrix}{A sparse matrix with a row for each person in the cohort, in the order of the
#' cohort, and a column for each covariate in the map.}
#' \item{labels}{The cohort, with rowId replaced by the matrix row and the original rowId in originalRowId.}
#' \item{covariateRef}{The covariateRef of the mapped covariates, with their columnId.}
#' \item{covariateMap}{A data.frame with the covariateId and columnId of each column.}
#' }
#' @examples
#' \donttest{ \dontshow{ # takes too long }
#' library(dplyr)
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n = 100, seed = 42)
#' # how many covariates are there before we convert to sparse matrix
#' plpData$covariateData$covariates %>% 
#'  dplyr::group_by(.data$covariateId) %>% 
#'  dplyr::summarise(n = n()) %>% 
#'  dplyr::collect() %>% nrow()
#' sparseData <- toSparseM(plpData, cohort=plpData$cohorts)
#' # how many covariates are there after we convert to sparse matrix'
#' sparseData$dataMatrix@Dim[2]
#' }
#' @export
toSparseM <- function(plpData, cohort = NULL, map = NULL) {
  start <- Sys.time()
  ParallelLogger::logInfo(paste0("starting toSparseM"))

  if (is.null(cohort)) {
    cohort <- plpData$labels
  }
  if (is.null(cohort)) {
    stop("toSparseM() needs a cohort, or labels in plpData, to define the rows of the matrix")
  }
  if (!is.data.frame(cohort)) {
    cohort <- dplyr::collect(cohort)
  }
  covariateData <- plpData$covariateData

  # matrix row of each patient, in the order of the cohort
  rowMap <- data.frame(rowId = cohort$rowId, xId = seq_len(nrow(cohort)))
  covariateData$rowMap <- rowMap
  on.exit(covariateData$rowMap <- NULL, add = TRUE)

  # matrix column of each covariate: from the map, or every covariate of the
  # cohort in covariateId order
  if (is.null(map)) {
    map <- data.frame(
      covariateId = covariateData$covariates %>%
        dplyr::inner_join(covariateData$rowMap, by = "rowId") %>%
        dplyr::distinct(.data$covariateId) %>%
        dplyr::arrange(.data$covariateId) %>%
        dplyr::pull()
    )
    map$columnId <- seq_len(nrow(map))
  }
  map <- as.data.frame(map)
  rownames(map) <- NULL
  covariateData$mapping <- map
  on.exit(covariateData$mapping <- NULL, add = TRUE)
  ParallelLogger::logDebug(paste0("Number of columns: ", nrow(map)))

  checkRam(covariateData) # estimates size of RAM required and prints it

  # one query for the row, column and value of every entry, so the three
  # always belong together
  entries <- covariateData$covariates %>%
    dplyr::inner_join(covariateData$mapping, by = "covariateId") %>%
    dplyr::inner_join(covariateData$rowMap, by = "rowId") %>%
    dplyr::select("xId", "columnId", "covariateValue") %>%
    dplyr::collect()
  data <- Matrix::sparseMatrix(
    i = entries$xId,
    j = entries$columnId,
    x = entries$covariateValue,
    dims = c(nrow(rowMap), max(c(0, map$columnId)))
  )
  ParallelLogger::logDebug(paste0("Sparse matrix with dimensionality: ", paste(dim(data), collapse = ",")))

  labels <- cohort %>%
    dplyr::inner_join(rowMap, by = "rowId") %>%
    dplyr::rename(originalRowId = "rowId", rowId = "xId") %>%
    dplyr::arrange(.data$rowId)
  covariateRef <- covariateData$mapping %>%
    dplyr::inner_join(covariateData$covariateRef, by = "covariateId") %>%
    dplyr::collect()

  ParallelLogger::logInfo(paste0("finishing toSparseM"))
  result <- list(
    dataMatrix = data,
    # a plain table: the attributes of a population (such as its metaData)
    # are not part of the labels
    labels = dplyr::as_tibble(unclass(labels)[names(labels)]),
    covariateRef = as.data.frame(covariateRef),
    covariateMap = map
  )
  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste0("toSparseM took ", delta, " ", attr(delta, "units")))
  return(result)
}

#' Map covariate and row Ids so they start from 1
#' @description this functions takes covariate data and a cohort/population and remaps
#' the covariate and row ids, restricts to pop and saves/creates mapping
#' @param covariateData a covariateData object
#' @param cohort        if specified rowIds restricted to the ones in cohort
#' @param mapping       A pre defined mapping to use
#' @returns a new `covariateData` object with remapped covariate and row ids
#' @examples
#' covariateData <- Andromeda::andromeda(
#'   covariates = data.frame(rowId = c(1, 3, 5, 7, 9), 
#'                           covariateId = c(10, 20, 10, 10, 20),
#'                           covariateValue = c(1, 1, 1, 1, 1)),
#'   covariateRef = data.frame(covariateId = c(10, 20), 
#'                               covariateNames = c("covariateA", 
#'                                                  "covariateB"),
#'                               analysisId = c(1, 1)))
#' mappedData <- MapIds(covariateData)
#' # columnId and rowId are now starting from 1 and are consecutive
#' mappedData$covariates
#' @export
MapIds <- function(
    covariateData,
    cohort = NULL,
    mapping = NULL) {
  ParallelLogger::logInfo(paste0("starting to map the columns and rows"))


  # change the rowIds in cohort (if exists)
  if (!is.null(cohort)) {
    rowMap <- data.frame(
      rowId = cohort %>% dplyr::select("rowId")
    )
    rowMap$xId <- 1:nrow(rowMap)
  } else {
    # in rowId order: the order of distinct() is not defined
    rowMap <- data.frame(
      rowId = covariateData$covariates %>%
        dplyr::distinct(.data$rowId) %>%
        dplyr::arrange(.data$rowId) %>%
        dplyr::pull()
    )
    rowMap$xId <- 1:nrow(rowMap)
  }

  covariateData$rowMap <- rowMap
  on.exit(covariateData$rowMap <- NULL)

  # change the rowIds in covariateData$covariates
  if (is.null(mapping)) {
    mapping <- data.frame(
      covariateId = covariateData$covariates %>%
        dplyr::inner_join(covariateData$rowMap, by = "rowId") %>% # first restrict the covariates to the rowMap$rowId
        dplyr::distinct(.data$covariateId) %>%
        dplyr::arrange(.data$covariateId) %>%
        dplyr::pull()
    )
    mapping$columnId <- 1:nrow(mapping)
  }
  covariateData$mapping <- mapping
  on.exit(covariateData$mapping <- NULL, add = TRUE)

  newCovariateData <- Andromeda::andromeda()
  # change the covariateIds and rowIds in covariates, in one copy
  newCovariateData$covariates <- covariateData$covariates %>%
    dplyr::inner_join(covariateData$mapping, by = "covariateId") %>%
    dplyr::inner_join(covariateData$rowMap, by = "rowId") %>%
    dplyr::select(-"rowId") %>%
    dplyr::rename(rowId = "xId")

  # change the covariateIds in covariateRef
  newCovariateData$covariateRef <- covariateData$mapping %>%
    dplyr::inner_join(covariateData$covariateRef, by = "covariateId")

  newCovariateData$rowMap <- rowMap

  if (!is.null(cohort)) {
    # change the rowId in labels
    newCovariateData$cohort <- cohort %>%
      dplyr::inner_join(rowMap, by = "rowId") %>%
      dplyr::rename(
        originalRowId = "rowId",
        rowId = "xId"
      ) %>%
      dplyr::arrange(.data$rowId) # make sure it is ordered lowest to highest
  }

  newCovariateData$mapping <- mapping
  newCovariateData$analysisRef <- covariateData$analysisRef
  if (!is.null(covariateData$timeRef)) {
    newCovariateData$timeRef <- covariateData$timeRef
  } 
  ParallelLogger::logInfo(paste0("finished MapCovariates"))

  return(newCovariateData)
}

checkRam <- function(covariateData) {
  nrowV <- covariateData$covariates %>%
    dplyr::summarise(size = dplyr::n()) %>%
    dplyr::collect()
  estRamB <- (nrowV$size / 1000000 * 24000984)

  ParallelLogger::logInfo(paste0("plpData size estimated to use ", round(estRamB / 1000000000, 1), "GBs of RAM"))

  return(invisible(TRUE))
}
