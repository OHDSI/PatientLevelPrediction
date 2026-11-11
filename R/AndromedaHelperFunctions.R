# @file AndromedaHelperFunctions.R
#
# Copyright 2025 Observational Health Data Sciences and Informatics
#
# This file is part of PatientLevelPrediction
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

# Gives an Andromeda object the CovariateData class of FeatureExtraction, as
# FeatureExtraction's own objects have it: with the package the class is defined
# in. Loading PatientLevelPrediction does not load FeatureExtraction, and until it
# is loaded R does not know the class, so Andromeda's methods ($, close()) do not
# apply to an object that has the class name only.
asCovariateData <- function(covariateData) {
  loadNamespace("FeatureExtraction")
  class(covariateData) <- "CovariateData"
  attr(class(covariateData), "package") <- "FeatureExtraction"
  covariateData
}

limitCovariatesToPopulation <- function(covariateData, rowIds) {
  ParallelLogger::logTrace(paste0("Starting to limit covariate data to population..."))

  metaData <- attr(covariateData, "metaData")

  newCovariateData <- Andromeda::andromeda(
    covariateRef = covariateData$covariateRef,
    analysisRef = covariateData$analysisRef
  )
  if (!is.null(covariateData$timeRef)) {
    newCovariateData$timeRef <- covariateData$timeRef
  }

  covariateData$pop <- data.frame(rowId = rowIds)
  on.exit(covariateData$pop <- NULL, add = TRUE)

  newCovariateData$covariates <- covariateData$covariates %>%
    dplyr::inner_join(covariateData$pop, by = "rowId")

  metaData$populationSize <- length(rowIds)
  attr(newCovariateData, "metaData") <- metaData
  newCovariateData <- asCovariateData(newCovariateData)
  ParallelLogger::logTrace(paste0("Finished limiting covariate data to population..."))
  return(newCovariateData)
}

# Closes the Andromeda objects in a list that are still open, each once (a step
# can return the object it was given). For the copies a function makes and does
# not return, which otherwise stay on disk until garbage collection.
closeAndromedas <- function(objects) {
  closed <- character(0)
  for (x in objects) {
    if (!Andromeda::isAndromeda(x) || !Andromeda::isValidAndromeda(x) || x@dbname %in% closed) {
      next
    }
    closed <- c(closed, x@dbname)
    Andromeda::close(x)
  }
  invisible(NULL)
}
