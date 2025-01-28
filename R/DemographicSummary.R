# @file DemographicSummary.R
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

#' Get a demographic summary
#'
#' @details
#' Generates a data.frame with a prediction summary per each 5 year age group 
#' and gender group
#'
#' @param prediction            A prediction object
#' @param predictionType        The type of prediction (binary or survival)
#' @param typeColumn            A column that is used to stratify the results
#'
#' @return
#' A dataframe with the demographic summary
#' @export
getDemographicSummary <- function(
    prediction,
    predictionType,
    typeColumn = "evaluation") {
  evaluation <- do.call(
    what = paste0("getDemographicSummary_", predictionType),
    args = list(
      prediction = prediction,
      evalColumn = typeColumn,
      timepoint = attr(prediction, "metaData")$timepoint
    )
  )

  return(evaluation)
}



getDemographicSummary_binary <- function(prediction, evalColumn, ...) {
  result <- c()
  evalTypes <- unique(as.data.frame(prediction)[, evalColumn])

  for (evalType in evalTypes) {
    predictionOfInterest <- prediction %>% dplyr::filter(.data[[evalColumn]] == evalType)

    demographicData <- predictionOfInterest[, c("rowId", "ageYear", "gender")] %>%
      dplyr::mutate(
        ageId = floor(.data$ageYear / 5),
        ageGroup = paste0("Age group: ", floor(.data$ageYear / 5) * 5, "-", floor(.data$ageYear / 5) * 5 + 4),
        genId = .data$gender,
        genGroup = ifelse(.data$gender == 8507, "Male", "Female")
      ) %>%
      dplyr::select("rowId", "ageId", "ageGroup", "genId", "genGroup") %>%
      dplyr::inner_join(predictionOfInterest[, colnames(predictionOfInterest) %in% c("rowId", "value", "outcomeCount", "survivalTime")], by = "rowId")

    demographicData <- demographicData %>%
      dplyr::group_by(.data$ageGroup, .data$genGroup) %>%
      dplyr::summarise(
        PersonCountAtRisk = length(.data$outcomeCount),
        PersonCountWithOutcome = sum(.data$outcomeCount),
        averagePredictedProbability = mean(.data$value, na.rm = TRUE),
        StDevPredictedProbability = stats::sd(.data$value, na.rm = TRUE),
        MinPredictedProbability = stats::quantile(.data$value, probs = 0),
        P25PredictedProbability = stats::quantile(.data$value, probs = 0.25),
        P50PredictedProbability = stats::quantile(.data$value, probs = 0.50),
        P75PredictedProbability = stats::quantile(.data$value, probs = 0.75),
        MaxPredictedProbability = stats::quantile(.data$value, probs = 1),
      )

    demographicData$evaluation <- evalType

    result <- rbind(result, demographicData)
  }

  result <- as.data.frame(result)
  return(result)
}


getDemographicSummary_survival <- function(prediction, evalColumn, timepoint = NULL, ...) {
  rlang::check_installed(
    pkg = c("survival"),
    reason = "getDemographicSummary_survival requires the survival package to be installed"
  )
  result <- c()
  evalTypes <- unique(as.data.frame(prediction)[, evalColumn])

  for (evalType in evalTypes) {
    predictionOfInterest <- prediction %>%
      dplyr::filter(.data[[evalColumn]] == evalType)

    demographicData <- predictionOfInterest[, c("rowId", "ageYear", "gender")] %>%
      dplyr::mutate(
        ageId = floor(.data$ageYear / 5),
        ageGroup = paste0("Age group: ", floor(.data$ageYear / 5) * 5, "-", floor(.data$ageYear / 5) * 5 + 4),
        genId = .data$gender,
        genGroup = ifelse(.data$gender == 8507, "Male", "Female")
      ) %>%
      dplyr::select("rowId", "ageId", "ageGroup", "genId", "genGroup") %>%
      dplyr::inner_join(predictionOfInterest[, colnames(predictionOfInterest) %in% c("rowId", "value", "outcomeCount", "survivalTime")], by = "rowId")


    if (is.null(timepoint)) {
      timepoint <- max(demographicData$survivalTime)
    }
    demographicSum <- demographicData %>%
      dplyr::mutate(
        t = .data$survivalTime,
        y = ifelse(.data$outcomeCount > 0, 1, 0)
      )

    genList <- unique(demographicData$genGroup)
    ageGroups <- unique(demographicData$ageGroup)

    demographicData <- NULL
    for (gen in genList) {
      for (age in ageGroups) {
        tempDemo <- demographicSum %>%
          dplyr::filter(.data$genGroup == gen & .data$ageGroup == age)

        if (nrow(tempDemo) > 1 && length(unique(tempDemo$y)) > 1) {
          t <- tempDemo$t
          y <- tempDemo$y
          value <- tempDemo$value

          out <- tryCatch(
            {
              summary(
                survival::survfit(survival::Surv(t, y) ~ 1),
                times = timepoint
              )
            },
            error = function(e) {
              ParallelLogger::logError(e)
              return(NULL)
            }
          )

          if (!is.null(out) && !is.null(out$surv)) {
            demoTemp <- c(
              genGroup = gen,
              ageGroup = age,
              PersonCountAtRisk = length(value),
              PersonCountWithOutcome = round(length(value) * (1 - out$surv)),
              observedRisk = 1 - out$surv,
              averagePredictedProbability = mean(value, na.rm = TRUE),
              StDevPredictedProbability = stats::sd(value, na.rm = TRUE)
            )

            demographicData <- rbind(demographicData, demoTemp)
          }
        }
      }
    }
    demographicData <- as.data.frame(demographicData)
    demographicData$averagePredictedProbability <- as.double(as.character(demographicData$averagePredictedProbability))
    demographicData$StDevPredictedProbability <- as.double(as.character(demographicData$StDevPredictedProbability))
    demographicData$PersonCountAtRisk <- as.double(as.character(demographicData$PersonCountAtRisk))
    demographicData$PersonCountWithOutcome <- as.double(as.character(demographicData$PersonCountWithOutcome))

    demographicData$evaluation <- evalType

    result <- rbind(
      result,
      demographicData
    )
  }

  result <- as.data.frame(result)
  return(result)
}
