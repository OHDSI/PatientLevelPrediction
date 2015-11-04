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
#' @param plpData               An object of type \code{plpData}.
#' @param modelType             The type of predictive model. Options are "logistic", "poisson", and
#'                              "survival".
#' @param removeDropoutsForLr   If TRUE and modelType is "logistic", subjects that do not have the full
#'                              observation window (i.e. are censored earlier) and do not have the
#'                              outcome are removed prior to fitting the model.
#' @param cohortId              The ID of the specific cohort for which to fit a model.
#' @param outcomeId             The ID of the specific outcome for which to fit a model.
#' @param prior                 The prior used to fit the model. See \code{\link[Cyclops]{createPrior}}
#'                              for details.
#' @param control               The control object used to control the cross-validation used to
#'                              determine the hyperparameters of the prior (if applicable). See
#'                              \code{\link[Cyclops]{createControl}} for details.
#'
#' @export
fitPredictiveModel <- function(plpData,
                               modelType = "logistic",
                               removeDropoutsForLr = TRUE,
                               cohortId = NULL,
                               outcomeId = NULL,
                               prior = createPrior("laplace",
                                                   exclude = c(0),
                                                   useCrossValidation = TRUE),
                               control = createControl(noiseLevel = "silent",
                                                       cvType = "auto",
                                                       startingVariance = 0.1)) {
  if (is.null(cohortId) && length(plpData$metaData$cohortIds) != 1) {
    stop("No cohort ID specified, but multiple cohorts found")
  }
  if (is.null(outcomeId) && length(plpData$metaData$outcomeIds) != 1) {
    stop("No outcome ID specified, but multiple outcomes found")
  }
  if (!is.null(cohortId) && !(cohortId %in% plpData$metaData$cohortIds)) {
    stop("Cohort ID not found")
  }
  if (!is.null(outcomeId) && !(outcomeId %in% plpData$metaData$outcomeIds)) {
    stop("Outcome ID not found")
  }
  
  start <- Sys.time()
  
  covariates <- plpData$covariates
  cohorts <- plpData$cohorts
  outcomes <- plpData$outcomes
  
  if (!is.null(cohortId) && length(plpData$metaData$cohortIds) > 1) {
    # Filter by cohort ID:
    t <- cohorts$cohortId == cohortId
    if (!ffbase::any.ff(t)) {
      stop(paste("No cohorts with cohort ID", cohortId))
    }
    cohorts <- cohorts[ffbase::ffwhich(t, t == TRUE), ]
    
    idx <- ffbase::ffmatch(x = covariates$rowId, table = cohorts$rowId)
    idx <- ffbase::ffwhich(idx, !is.na(idx))
    covariates <- covariates[idx, ]
    
    # No need to filter outcomes since we'll merge outcomes with cohorts later
  }
  
  if (!is.null(outcomeId) && length(plpData$metaData$outcomeIds) > 1) {
    # Filter by outcome ID:
    t <- outcomes$outcomeId == outcomeId
    if (!ffbase::any.ff(t)) {
      stop(paste("No outcomes with outcome ID", outcomeId))
    }
    outcomes <- outcomes[ffbase::ffwhich(t, t == TRUE), ]
  }
  
  if (!is.null(plpData$exclude) && nrow(plpData$exclude) != 0) {
    # Filter subjects with previous outcomes:
    if (!is.null(outcomeId)) {
      exclude <- plpData$exclude
      t <- exclude$outcomeId == outcomeId
      if (ffbase::any.ff(t)) {
        exclude <- exclude[ffbase::ffwhich(t, t == TRUE), ]
        
        t <- ffbase::ffmatch(x = cohorts$rowId, table = exclude$rowId, nomatch = 0L) > 0L
        if (ffbase::any.ff(t)) {
          cohorts <- cohorts[ffbase::ffwhich(t, t == FALSE), ]
        }
        
        t <- ffbase::ffmatch(x = covariates$rowId, table = exclude$rowId, nomatch = 0L) > 0L
        if (ffbase::any.ff(t)) {
          covariates <- covariates[ffbase::ffwhich(t, t == FALSE), ]
        }
        
        # No need to filter outcomes since we'll merge outcomes with cohorts later
      }
    }
  }
  
  if (modelType == "logistic" | modelType == "survival") {
    outcomes$y <- ff::ff(1, length = nrow(outcomes), vmode = "double")
  } else {
    # modelType == 'Poisson'
    outcomes$y <- outcomes$outcomeCount
  }
  
  # Merge outcomes with cohorts so we also have the subjects with 0 outcomes:
  outcomes <- merge(cohorts, outcomes, by = c("rowId"), all.x = TRUE)
  idx <- ffbase::is.na.ff(outcomes$y)
  idx <- ffbase::ffwhich(idx, idx == TRUE)
  outcomes$y <- ff::ffindexset(x = outcomes$y,
                               index = idx,
                               value = ff::ff(0, length = length(idx), vmode = "double"))
  
  if (modelType == "survival") {
    # For survival analysis, we use a Poisson regression censored at the time of first event
    idx <- ffbase::is.na.ff(outcomes$timeToEvent)
    idx <- ffbase::ffwhich(idx, idx == FALSE)
    outcomes$time <- ff::ffindexset(x = outcomes$time,
                                    index = idx,
                                    value = outcomes$timeToEvent[idx])
  }
  
  if (modelType == "logistic" && removeDropoutsForLr) {
    # Select only subjects with observation spanning the full window, or with an outcome:
    fullWindowLength <- ffbase::max.ff(plpData$cohorts$time)
    t <- outcomes$y != 0 | outcomes$time == fullWindowLength
    outcomes <- outcomes[ffbase::ffwhich(t, t == TRUE), ]
    
    idx <- ffbase::ffmatch(x = covariates$rowId, table = outcomes$rowId)
    idx <- ffbase::ffwhich(idx, !is.na(idx))
    covariates <- covariates[idx, ]
  }
  
  if (modelType == "logistic") {
    cyclopsModelType <- "lr"
  } else {
    cyclopsModelType <- "pr"
    outcomes$time <- outcomes$time + 1  # Assume same day means duration of 1
  }
  cyclopsData <- convertToCyclopsData(outcomes,
                                      covariates,
                                      modelType = cyclopsModelType,
                                      addIntercept = TRUE,
                                      quiet = TRUE)
  cyclopsFit <- fitCyclopsModel(cyclopsData, prior = prior, control = control)
  if (is.null(cohortId))
    cohortId <- plpData$metaData$cohortIds
  if (is.null(outcomeId))
    outcomeId <- plpData$metaData$outcomeIds
  trainSetStatistics <- list(numberOfPeriods = nrow(outcomes),
                             numberOfPeriodsWithOutcomes = ffbase::sum.ff(outcomes$y !=
                                                                            0), numberOfOutcomes = ffbase::sum.ff(outcomes$y))
  predictiveModel <- list(cohortId = cohortId,
                          outcomeId = outcomeId,
                          modelType = modelType,
                          removeDropouts = (modelType ==
                                              "logistic" & removeDropoutsForLr), coefficients = coef(cyclopsFit), priorVariance = cyclopsFit$variance[1], trainSetStatistics = trainSetStatistics)
  class(predictiveModel) <- append(class(predictiveModel), "predictiveModel")
  delta <- Sys.time() - start
  writeLines(paste("Fitting model took", signif(delta, 3), attr(delta, "units")))
  return(predictiveModel)
}

#' Get the predictive model details
#'
#' @description
#' \code{getModelDetails} shows the full model, so showing the betas of all variables included in the
#' model, along with the variable names
#'
#' @param predictiveModel   An object of type \code{predictiveModel} as generated using he
#'                          \code{\link{fitPredictiveModel}} function.
#' @param plpData           An object of type \code{plpData} as generated using
#'                          \code{\link{getDbPlpData}}.
#'
#' @details
#' Shows the coefficients and names of the covariates with non-zero coefficients.
#'
#' @export
getModelDetails <- function(predictiveModel, plpData) {
  cfs <- predictiveModel$coefficients
  
  cfs <- cfs[cfs != 0]
  attr(cfs, "names")[attr(cfs, "names") == "(Intercept)"] <- 0
  cfs <- data.frame(coefficient = cfs, id = as.numeric(attr(cfs, "names")))
  
  cfs <- merge(ff::as.ffdf(cfs),
               plpData$covariateRef,
               by.x = "id",
               by.y = "covariateId",
               all.x = TRUE)
  cfs <- ff::as.ram(cfs[, c("coefficient", "id", "covariateName")])
  cfs$covariateName <- as.character(cfs$covariateName)
  cfs <- cfs[order(-abs(cfs$coefficient)), ]
  cfs$covariateName[cfs$id == 0] <- "Intercept"
  return(cfs)
}
