# @file FitModel.R
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

#' Fit a predictive model
#' 
#' @param covariateData    An object of type \code{covariateData}.
#' @param outcomeData      An object of type \code{outcomeData}.
#' @param modelType        The type of predictive model
#' @param prior            The prior used to fit the model. See \code{\link[Cyclops]{createPrior}} for details.
#' @param control          The control object used to control the cross-validation used to determine the 
#' hyperparameters of the prior (if applicable). See \code{\link[Cyclops]{createControl}}  for details.
#' 
#' @export
fitPredictiveModel <- function(cohortsData,
                               covariateData, 
                               outcomeData, 
                               modelType = "lr", 
                               cohortConceptId = NULL,
                               outcomeConceptId = NULL,
                               prior = createPrior("laplace", exclude = c(0), useCrossValidation = TRUE),
                               control = createControl(noiseLevel = "silent", cvType = "auto", startingVariance = 0.1)) {
  #TODO: deal with excluded persons
  #TODO: implement poisson and survival models
  
  if (is.null(cohortConceptId)){
    covariates <- ffbase::subset.ffdf(covariateData$covariates, select = c("personId", "cohortStartDate", "covariateId", "covariateValue"))
    cohorts <- ffbase::subset.ffdf(cohortsData$cohorts, select = c("personId", "cohortStartDate", "time"))
    outcomes <- ffbase::subset.ffdf(outcomeData$outcomes, select = c("personId", "cohortStartDate", "outcomeId", "outcomeCount", "timeToEvent"))
  } else {
    covariates <- ffbase::subset.ffdf(covariateData$covariates, cohortConceptId == cohortConceptId, select = c("personId", "cohortStartDate", "covariateId", "covariateValue"))
    cohorts <- ffbase::subset.ffdf(cohortsData$cohorts, cohortConceptId == cohortConceptId, select = c("personId", "cohortStartDate", "time"))
    outcomes <- ffbase::subset.ffdf(outcomeData$outcomes, cohortConceptId == cohortConceptId, select = c("personId", "cohortStartDate", "outcomeId", "outcomeCount", "timeToEvent"))
  }
  if (!is.null(outcomeConceptId)){
    outcomes <- ffbase::subset.ffdf(outcomes, outcomeId == outcomeConceptId)
  } 
  
  cohorts$rowId <- ff::ff(1:nrow(cohorts))
  covariates <- merge(covariates, cohorts, by=c("cohortStartDate", "personId"))
  outcomes$y <- ff::ff(1, length = nrow(outcomes), vmode = "double")
  outcomes <- merge(cohorts, outcomes, by=c("cohortStartDate", "personId"), all.x = TRUE)
  idx <- ffbase::is.na.ff(outcomes$y)
  idx <- ffbase::ffwhich(idx, idx == TRUE)
  outcomes$y <- ff::ffindexset(x=outcomes$y, index =idx, value = ff::ff(0, length=length(idx), vmode = "double")) 
  
  cyclopsData <- convertToCyclopsData(outcomes, 
                                      covariates, 
                                      modelType = "lr", 
                                      addIntercept = TRUE,
                                      checkSorting = FALSE, 
                                      checkRowIds = FALSE)
  
  cyclopsFit <- fitCyclopsModel(cyclopsData, prior = prior, control = control)
  
  predictiveModel <- list(cohortConceptId = cohortConceptId,
                          outcomeConceptId = outcomeConceptId,
                          modelType = modelType, 
                          coefficients = coef(cyclopsFit),
                          priorVariance = cyclopsFit$variance[1])
  class(predictiveModel) <- "predictiveModel"
  return(predictiveModel)
}

#' Get the predictive model details
#'
#' @description
#' \code{getModelDetails} shows the full model, so showing the betas of all variables
#' included in the model, along with the variable names
#' 
#' @param predictiveModel        An object of type \code{predictiveModel} as generated using he \code{\link{fitPredictiveModel}} function.
#' @param covariateData          An object of type \code{covariateData} as generated using \code{\link{getDbCovariateData}}.
#'
#' @details
#' Shows the coefficients and names of the covariates with non-zero coefficients.
#'  
#' @examples 
#' #todo
#' 
#' @export
getModelDetails <- function(predictiveModel, covariateData){
  cfs <- predictiveModel$coefficients
  
  cfs <- cfs[cfs != 0]
  attr(cfs,"names")[attr(cfs,"names") == "(Intercept)"] <- 0
  cfs <- data.frame(coefficient = cfs, id = as.numeric(attr(cfs,"names")))
  
  cfs <- merge(ff::as.ffdf(cfs), covariateData$covariateRef, by.x = "id", by.y = "covariateId", all.x = TRUE)
  cfs <- ff::as.ram(cfs[,c("coefficient","id","covariateName")])
  cfs$covariateName <- as.character(cfs$covariateName)
  cfs <- cfs[order(-abs(cfs$coefficient)),]
  cfs$covariateName[cfs$id == 0] <- "Intercept"
  return(cfs)
}