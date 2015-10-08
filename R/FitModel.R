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
#' @param cohortData      An object of type \code{cohortData}.
#' @param covariateData   An object of type \code{covariateData}.
#' @param outcomeData     An object of type \code{outcomeData}.
#' @param modelType       The type of predictive model. Options are "logistic", "poisson", and
#'                        "survival".
#' @param removeDropoutsForLr  If TRUE and modelType is "logistic", subjects that do not have the full 
#'                             observation window (i.e. are censored earlier) and do not have the outcome
#'                             are removed prior to fitting the model.
#' @param cohortId        The ID of the specific cohort for which to fit a model.
#' @param outcomeId       The ID of the specific outcome for which to fit a model.
#' @param prior           The prior used to fit the model. See \code{\link[Cyclops]{createPrior}} for
#'                        details.
#' @param control         The control object used to control the cross-validation used to determine the
#'                        hyperparameters of the prior (if applicable). See
#'                        \code{\link[Cyclops]{createControl}} for details.
#'
#' @export
fitPredictiveModel <- function(cohortData,
                               covariateData,
                               outcomeData,
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
  if (is.null(cohortId) && length(cohortData$metaData$cohortIds) != 1)
    stop("No cohort ID specified, but multiple cohorts found")
  if (is.null(outcomeId) && length(outcomeData$metaData$outcomeIds) != 1)
    stop("No outcome ID specified, but multiple outcomes found")

  covariates <- ffbase::subset.ffdf(covariateData$covariates, select = c("cohortId",
                                                                         "personId",
                                                                         "cohortStartDate",
                                                                         "covariateId",
                                                                         "covariateValue"))
  cohorts <- ffbase::subset.ffdf(cohortData$cohorts,
                                 select = c("cohortId","personId", "cohortStartDate", "time"))
  outcomes <- ffbase::subset.ffdf(outcomeData$outcomes, select = c("cohortId",
                                                                   "personId",
                                                                   "cohortStartDate",
                                                                   "outcomeId",
                                                                   "outcomeCount",
                                                                   "timeToEvent"))
  if (!is.null(cohortId)) {
    # Filter by cohort ID:
    t <- covariates$cohortId == cohortId
    if (!ffbase::any.ff(t)) {
      stop(paste("No covariates with cohort ID", cohortId))
    }
    covariates <- covariates[ffbase::ffwhich(t, t == TRUE), ]

    t <- cohorts$cohortId == cohortId
    if (!ffbase::any.ff(t)) {
      stop(paste("No cohorts with cohort ID", cohortId))
    }    
    cohorts <- cohorts[ffbase::ffwhich(t, t == TRUE), ]

    t <- outcomes$cohortId == cohortId
    if (!ffbase::any.ff(t)) {
      stop(paste("No outcomes with cohort ID", cohortId))
    }
    outcomes <- outcomes[ffbase::ffwhich(t, t == TRUE), ]
  }

  if (!is.null(outcomeId)) {
    # Filter by outcome ID:
    t <- outcomes$outcomeId == outcomeId
    if (!ffbase::any.ff(t)) {
      stop(paste("No outcomes with outcome ID", outcomeId))
    }
    outcomes <- outcomes[ffbase::ffwhich(t, t == TRUE), ]
  }
  
  if (!is.null(outcomeData$exclude) && nrow(outcomeData$exclude) != 0) {
    # Filter subjects with previous outcomes:
    exclude <- outcomeData$exclude
    if (!is.null(outcomeId)) {
      t <- exclude$outcomeId == outcomeId
      exclude <- exclude[ffbase::ffwhich(t, t == TRUE)]
    }
    exclude$dummy <- ff::ff(1, length = nrow(exclude), vmode = "double")
    cohorts <- merge(cohorts, exclude, all.x = TRUE)
    t <- cohorts$dummy == 1
    if (ffbase::any.ff(t)) {
      cohorts <- cohorts[ffbase::ffwhich(t, is.na(t)), ]
    }
    cohorts$dummy <- NULL
  }
  
  if (modelType == "logistic" | modelType == "survival") {
    outcomes$y <- ff::ff(1, length = nrow(outcomes), vmode = "double")
  } else {
    # modelType == 'Poisson'
    outcomes$y <- outcomes$outcomeCount
  }
  
  # Merge outcomes with cohorts so we also have the subjects with 0 outcomes:
  cohorts$rowId <- ff::ff(1:nrow(cohorts))
  outcomes <- merge(cohorts, outcomes, by = c("cohortStartDate", "personId"), all.x = TRUE)
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
    fullWindowLength <- ffbase::max.ff(cohorts$time) 
    t <- outcomes$y != 0 | outcomes$time == fullWindowLength
    outcomes <- outcomes[ffbase::ffwhich(t, t == TRUE),]
  }
  
  covariates <- merge(covariates, 
                      ffbase::subset.ffdf(outcomes, select = c("rowId", "cohortStartDate", "personId", "y")),
                      by = c("cohortStartDate", "personId"))
  
  if (modelType == "logistic") {
    cyclopsModelType <- "lr"
  } else {
    cyclopsModelType <- "pr"
  }
  cyclopsData <- convertToCyclopsData(outcomes,
                                      covariates,
                                      modelType = cyclopsModelType,
                                      addIntercept = TRUE,
                                      quiet = TRUE)
  cyclopsFit <- fitCyclopsModel(cyclopsData, prior = prior, control = control)
  if (is.null(cohortId))
    cohortId <- cohortData$metaData$cohortIds
  if (is.null(outcomeId))
    outcomeId <- outcomeData$metaData$outcomeIds
  trainSetStatistics <- list(numberOfSubjects = nrow(outcomes),
                             numberOfSubjectsWithOutcomes = ffbase::sum.ff(outcomes$y != 0),
                             numberOfOutcomes = ffbase::sum.ff(outcomes$y))
  predictiveModel <- list(cohortId = cohortId,
                          outcomeId = outcomeId,
                          modelType = modelType,
                          coefficients = coef(cyclopsFit),
                          priorVariance = cyclopsFit$variance[1],
                          trainSetStatistics = trainSetStatistics)
  class(predictiveModel) <- append(class(predictiveModel), "predictiveModel")
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
#' @param covariateData     An object of type \code{covariateData} as generated using
#'                          \code{\link{getDbCovariateData}}.
#'
#' @details
#' Shows the coefficients and names of the covariates with non-zero coefficients.
#'
#' @export
getModelDetails <- function(predictiveModel, covariateData) {
  cfs <- predictiveModel$coefficients

  cfs <- cfs[cfs != 0]
  attr(cfs, "names")[attr(cfs, "names") == "(Intercept)"] <- 0
  cfs <- data.frame(coefficient = cfs, id = as.numeric(attr(cfs, "names")))

  cfs <- merge(ff::as.ffdf(cfs),
               covariateData$covariateRef,
               by.x = "id",
               by.y = "covariateId",
               all.x = TRUE)
  cfs <- ff::as.ram(cfs[, c("coefficient", "id", "covariateName")])
  cfs$covariateName <- as.character(cfs$covariateName)
  cfs <- cfs[order(-abs(cfs$coefficient)), ]
  cfs$covariateName[cfs$id == 0] <- "Intercept"
  return(cfs)
}
