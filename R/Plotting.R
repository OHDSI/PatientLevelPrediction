# @file Plotting.R
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


#' Plot the outcome incidence over time
#'
#' @details
#' This creates a survival plot that can be used to pick a suitable time-at-risk period
#'
#' @param plpData               The plpData object returned by running getPlpData()
#' @param outcomeId             The cohort id corresponding to the outcome
#' @param populationSettings    The population settings created using \code{createStudyPopulationSettings}
#' @param riskTable           (binary) Whether to include a table at the bottom  of the plot showing the number of people at risk over time
#' @param confInt             (binary) Whether to include a confidence interval
#' @param yLabel              (string) The label for the y-axis
#'
#' @return
#' A `ggsurvplot` object
#' @examplesIf rlang::is_installed("survminer")
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n = 999, seed = 42)
#' plotObject <- outcomeSurvivalPlot(plpData, outcomeId = 3)
#' print(plotObject)
#' @export
outcomeSurvivalPlot <- function(
    plpData,
    outcomeId,
    populationSettings = createStudyPopulationSettings(
      binary = TRUE,
      includeAllOutcomes = TRUE,
      firstExposureOnly = FALSE,
      washoutPeriod = 0,
      removeSubjectsWithPriorOutcome = TRUE,
      priorOutcomeLookback = 99999,
      requireTimeAtRisk = FALSE,
      riskWindowStart = 1,
      startAnchor = "cohort start",
      riskWindowEnd = 3650,
      endAnchor = "cohort start"
    ),
    riskTable = TRUE,
    confInt = TRUE,
    yLabel = "Fraction of those who are outcome free in target population") {
  rlang::check_installed(c("survival", "survminer"))
  if (missing(plpData)) {
    stop("plpData missing")
  }
  if (missing(outcomeId)) {
    stop("outcomeId missing")
  }
  if (!inherits(x = plpData, what = "plpData")) {
    stop("Incorrect plpData object")
  }
  if (!outcomeId %in% unique(plpData$outcomes$outcomeId)) {
    stop("outcome id not in data")
  }

  populationSettings$plpData <- plpData
  populationSettings$outcomeId <- outcomeId

  population <- do.call(
    what = "createStudyPopulation",
    args = list(
      plpData = plpData,
      outcomeId = outcomeId,
      populationSettings = populationSettings
    )
  )

  population$daysToEvent[is.na(population$daysToEvent)] <-
    population$survivalTime[is.na(population$daysToEvent)]
  survivalFit <- survival::survfit(
    survival::Surv(daysToEvent, outcomeCount) ~ targetId,
    # riskDecile,
    population,
    conf.int = TRUE
  )

  if (!is.null(survivalFit$surv)) {
    yliml <- min(survivalFit$surv)
  } else {
    yliml <- 0.5
  }

  result <- survminer::ggsurvplot(
    fit = survivalFit,
    data = population,
    risk.table = riskTable,
    pval = FALSE,
    xlim = c(0, populationSettings$riskWindowEnd),
    ylim = c(yliml * 0.95, 1),
    conf.int = confInt,
    ggtheme = ggplot2::theme_minimal(),
    risk.table.y.text.col = TRUE,
    risk.table.y.text = FALSE,
    ylab = yLabel
  )
  return(result)
}


#' Plot all the PatientLevelPrediction plots
#'
#' @details
#' Create a directory with all the plots
#'
#' @param plpResult               Object returned by the runPlp() function
#' @param saveLocation            Name of the directory where the plots should be saved (NULL means no saving)
#' @param typeColumn              The name of the column specifying the evaluation type
#'                                (to stratify the plots)
#'
#' @return
#' TRUE if it ran, plots are saved in the specified directory
#'
#' @examplesIf rlang::is_installed("ggplot2")
#' \donttest{ \dontshow{ # takes too long }
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n = 1000, seed = 42)
#' saveLoc <- file.path(tempdir(), "plotPlp")
#' results <- runPlp(plpData, outcomeId = 3, saveDirectory = saveLoc)
#' plotPlp(results)
#' # clean up
#' unlink(saveLoc, recursive = TRUE)
#' }
#' @export
plotPlp <- function(
    plpResult,
    saveLocation = NULL,
    typeColumn = "evaluation") {
  # check inputs
  if (!is.null(saveLocation)) {
    if (!dir.exists(saveLocation)) {
      dir.create(saveLocation, recursive = TRUE)
    }
  }
  rlang::check_installed(
    pkg = c("ggplot2", "gridExtra"),
    reason = "These packages are required for plotting"
  )

  # run each of the plots:
  tryCatch(
    {
      plotSparseRoc(
        plpResult = plpResult,
        typeColumn = typeColumn,
        saveLocation = saveLocation,
        fileName = "sparseROC.pdf"
      )
    },
    error = function(e) {
      ParallelLogger::logError("Issue with plotSparseRoc")
    }
  )

  tryCatch(
    {
      plotPredictedPDF(
        plpResult,
        saveLocation = saveLocation,
        fileName = "predictedPDF.pdf",
        typeColumn = typeColumn
      )
    },
    error = function(e) {
      ParallelLogger::logError("Issue with plotPredictedPDF")
    }
  )

  tryCatch(
    {
      plotPreferencePDF(
        plpResult,
        saveLocation = saveLocation,
        fileName = "preferencePDF.pdf",
        typeColumn = typeColumn
      )
    },
    error = function(e) {
      ParallelLogger::logError("Issue with plotPreferencePDF")
    }
  )

  tryCatch(
    {
      plotPrecisionRecall(
        plpResult,
        saveLocation = saveLocation,
        fileName = "precisionRecall.pdf",
        typeColumn = typeColumn
      )
    },
    error = function(e) {
      ParallelLogger::logError("Issue with plotPrecisionRecall")
    }
  )

  tryCatch(
    {
      plotF1Measure(
        plpResult,
        saveLocation = saveLocation,
        fileName = "f1Measure.pdf",
        typeColumn = typeColumn
      )
    },
    error = function(e) {
      ParallelLogger::logError("Issue with plotF1Measure")
    }
  )

  tryCatch(
    {
      plotDemographicSummary(
        plpResult,
        saveLocation = saveLocation,
        fileName = "demographicSummary.pdf",
        typeColumn = typeColumn
      )
    },
    error = function(e) {
      ParallelLogger::logError("Issue with plotDemographicSummary")
    }
  )

  # add smooth calibration
  tryCatch(
    {
      plotSmoothCalibration(
        plpResult = plpResult,
        smooth = "loess",
        typeColumn = typeColumn,
        saveLocation = saveLocation,
        fileName = "smoothCalibration.pdf"
      )
    },
    error = function(e) {
      ParallelLogger::logError("Issue with plotSmoothCalibration")
    }
  )

  tryCatch(
    {
      plotSparseCalibration(
        plpResult,
        saveLocation = saveLocation,
        fileName = "sparseCalibration.pdf",
        typeColumn = typeColumn
      )
    },
    error = function(e) {
      ParallelLogger::logError("Issue with plotSparseCalibration")
    }
  )

  tryCatch(
    {
      plotSparseCalibration2(
        plpResult,
        saveLocation = saveLocation,
        fileName = "sparseCalibrationConventional.pdf",
        typeColumn = typeColumn
      )
    },
    error = function(e) {
      ParallelLogger::logError("Issue with plotSparseCalibration2")
    }
  )

  tryCatch(
    {
      plotPredictionDistribution(
        plpResult,
        saveLocation = saveLocation,
        fileName = "predictionDistribution.pdf",
        typeColumn = typeColumn
      )
    },
    error = function(e) {
      ParallelLogger::logError("Issue with plotPredictionDistribution")
    }
  )

  tryCatch(
    {
      plotVariableScatterplot(
        plpResult$covariateSummary,
        saveLocation = saveLocation,
        fileName = "variableScatterplot.pdf"
      )
    },
    error = function(e) {
      ParallelLogger::logError("Issue with plotVariableScatterplot")
    }
  )


  if (sum(c("TrainWithNoOutcome_CovariateMean", "TestWithNoOutcome_CovariateMean") %in% colnames(plpResult$covariateSummary)) == 2) {
    tryCatch(
      {
        plotGeneralizability(
          plpResult$covariateSummary,
          saveLocation = saveLocation,
          fileName = "generalizability.pdf"
        )
      },
      error = function(e) {
        ParallelLogger::logError("Issue with plotGeneralizability")
      }
    )
  }

  return(invisible(TRUE))
}


#' Plot the ROC curve using the sparse thresholdSummary data frame
#'
#' @details
#' Create a plot showing the Receiver Operator Characteristics (ROC) curve.
#'
#' @param plpResult            A plp result object as generated using the \code{\link{runPlp}} function.
#' @param typeColumn            The name of the column specifying the evaluation type
#' @param saveLocation          Directory to save plot (if NULL plot is not saved)
#' @param fileName              Name of the file to save to plot, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#' @examplesIf rlang::is_installed("ggplot2")
#' \donttest{ \dontshow{ # takes too long }
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n = 1000, seed = 42)
#' saveLoc <- file.path(tempdir(), "plotSparseRoc")
#' results <- runPlp(plpData, outcomeId = 3, saveDirectory = saveLoc)
#' plotSparseRoc(results)
#' # clean up
#' unlink(saveLoc, recursive = TRUE)
#' }
#' @export
plotSparseRoc <- function(
    plpResult,
    typeColumn = "evaluation",
    saveLocation = NULL,
    fileName = "roc.png") {
  evalTypes <- unique(plpResult$performanceEvaluation$thresholdSummary[, typeColumn])

  plots <- list()
  length(plots) <- length(evalTypes)

  for (i in 1:length(evalTypes)) {
    evalType <- evalTypes[i]
    x <- plpResult$performanceEvaluation$thresholdSummary %>%
      dplyr::filter(.data[[typeColumn]] == evalType) %>%
      dplyr::select("falsePositiveRate", "sensitivity")

    x <- x[order(x$falsePositiveRate, x$sensitivity), ]

    # add the bit to get the step
    stepsExtra <- cbind(x[-1, 1], x[-nrow(x), 2])
    colnames(stepsExtra) <- colnames(x)
    x <- rbind(c(1, 1), x, stepsExtra, c(0, 0))
    x <- x[order(x$falsePositiveRate, x$sensitivity), ]

    plots[[i]] <- ggplot2::ggplot(
      x,
      ggplot2::aes(
        .data$falsePositiveRate,
        .data$sensitivity
      )
    ) +
      ggplot2::geom_polygon(fill = "blue", alpha = 0.2) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 2) +
      ggplot2::scale_x_continuous("1 - specificity", limits = c(0, 1)) +
      ggplot2::scale_y_continuous("Sensitivity", limits = c(0, 1)) +
      ggplot2::ggtitle(evalType)
  }

  plot <- gridExtra::marrangeGrob(plots, nrow = length(plots), ncol = 1)

  if (!is.null(saveLocation)) {
    if (!dir.exists(saveLocation)) {
      dir.create(saveLocation, recursive = TRUE)
    }
    ggplot2::ggsave(file.path(saveLocation, fileName), plot, width = 5, height = 4.5, dpi = 400)
  }
  return(plot)
}


#' Plot the Predicted probability density function, showing prediction overlap between true and false cases
#'
#' @details
#' Create a plot showing the predicted probability density function, showing prediction overlap between true and false cases
#'
#' @param plpResult            A plp result object as generated using the \code{\link{runPlp}} function.
#' @param typeColumn            The name of the column specifying the evaluation type
#' @param saveLocation          Directory to save plot (if NULL plot is not saved)
#' @param fileName              Name of the file to save to plot, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#' @examplesIf rlang::is_installed("ggplot2")
#' \donttest{ \dontshow{ # takes too long }
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n = 1000, seed = 42)
#' saveLoc <- file.path(tempdir(), "plotPredictedPDF")
#' results <- runPlp(plpData, outcomeId = 3, saveDirectory = saveLoc)
#' plotPredictedPDF(results)
#' # clean up
#' unlink(saveLoc, recursive = TRUE)
#' }
#' @export
plotPredictedPDF <- function(
    plpResult,
    typeColumn = "evaluation",
    saveLocation = NULL,
    fileName = "PredictedPDF.png") {
  evalTypes <- unique(plpResult$performanceEvaluation$thresholdSummary[, typeColumn])

  plots <- list()
  length(plots) <- length(evalTypes)

  for (ind in 1:length(evalTypes)) {
    evalType <- evalTypes[ind]
    x <- plpResult$performanceEvaluation$thresholdSummary %>%
      dplyr::filter(.data[[typeColumn]] == evalType) %>%
      dplyr::select(
        "predictionThreshold",
        "truePositiveCount",
        "trueNegativeCount",
        "falsePositiveCount",
        "falseNegativeCount"
      )

    x <- x[order(x$predictionThreshold, -x$truePositiveCount, -x$falsePositiveCount), ]
    x$out <- c(
      x$truePositiveCount[-length(x$truePositiveCount)] - x$truePositiveCount[-1],
      x$truePositiveCount[length(x$truePositiveCount)]
    )
    x$nout <- c(
      x$falsePositiveCount[-length(x$falsePositiveCount)] - x$falsePositiveCount[-1],
      x$falsePositiveCount[length(x$falsePositiveCount)]
    )

    vals <- c()
    for (i in 1:length(x$predictionThreshold)) {
      if (i != length(x$predictionThreshold)) {
        upper <- x$predictionThreshold[i + 1]
      } else {
        upper <- min(x$predictionThreshold[i] + 0.01, 1)
      }
      val <- x$predictionThreshold[i] + stats::runif(x$out[i]) * (upper - x$predictionThreshold[i])
      vals <- c(val, vals)
    }
    vals <- vals[!is.na(vals)] # assigned

    vals2 <- c()
    for (i in 1:length(x$predictionThreshold)) {
      if (i != length(x$predictionThreshold)) {
        upper <- x$predictionThreshold[i + 1]
      } else {
        upper <- min(x$predictionThreshold[i] + 0.01, 1)
      }
      val2 <- x$predictionThreshold[i] + stats::runif(x$nout[i]) * (upper - x$predictionThreshold[i])
      vals2 <- c(val2, vals2)
    }
    vals2 <- vals2[!is.na(vals2)] # assigned

    x <- rbind(
      data.frame(variable = rep("outcome", length(vals)), value = vals),
      data.frame(variable = rep("No outcome", length(vals2)), value = vals2)
    )

    plots[[ind]] <- ggplot2::ggplot(x, ggplot2::aes(
      x = .data$value,
      group = .data$variable,
      fill = .data$variable
    )) +
      ggplot2::geom_density(ggplot2::aes(x = .data$value, fill = .data$variable), alpha = .3) +
      ggplot2::scale_x_continuous("Prediction Threshold") + # , limits=c(0,1)) +
      ggplot2::scale_y_continuous("Density") +
      ggplot2::guides(fill = ggplot2::guide_legend(title = "Class")) +
      ggplot2::ggtitle(evalType)
  }

  plot <- gridExtra::marrangeGrob(plots, nrow = length(plots), ncol = 1)

  if (!is.null(saveLocation)) {
    if (!dir.exists(saveLocation)) {
      dir.create(saveLocation, recursive = TRUE)
    }
    ggplot2::ggsave(file.path(saveLocation, fileName), plot, width = 5, height = 4.5, dpi = 400)
  }
  return(plot)
}



#' Plot the preference score probability density function, showing prediction overlap between true and false cases
#' #'
#' @details
#' Create a plot showing the preference score probability density function, showing prediction overlap between true and false cases
#' #'
#' @param plpResult            A plp result object as generated using the \code{\link{runPlp}} function.
#' @param typeColumn            The name of the column specifying the evaluation type
#' @param saveLocation          Directory to save plot (if NULL plot is not saved)
#' @param fileName              Name of the file to save to plot, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#' @examplesIf rlang::is_installed("ggplot2")
#' \donttest{ \dontshow{ # takes too long }
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n = 1000, seed = 42)
#' saveLoc <- file.path(tempdir(), "plotPreferencePDF")
#' results <- runPlp(plpData, outcomeId = 3, saveDirectory = saveLoc)
#' plotPreferencePDF(results)
#' # clean up
#' unlink(saveLoc, recursive = TRUE)
#' }
#' @export
plotPreferencePDF <- function(
    plpResult,
    typeColumn = "evaluation",
    saveLocation = NULL,
    fileName = "plotPreferencePDF.png") {
  evalTypes <- unique(plpResult$performanceEvaluation$thresholdSummary[, typeColumn])

  plots <- list()
  length(plots) <- length(evalTypes)

  for (ind in 1:length(evalTypes)) {
    evalType <- evalTypes[ind]
    x <- plpResult$performanceEvaluation$thresholdSummary %>%
      dplyr::filter(.data[[typeColumn]] == evalType) %>%
      dplyr::select(
        "preferenceThreshold",
        "truePositiveCount",
        "trueNegativeCount",
        "falsePositiveCount",
        "falseNegativeCount"
      )

    x <- x[order(x$preferenceThreshold, -x$truePositiveCount, x$trueNegativeCount), ]
    x$out <- c(
      x$truePositiveCount[-length(x$truePositiveCount)] - x$truePositiveCount[-1],
      x$truePositiveCount[length(x$truePositiveCount)]
    )
    x$nout <- c(
      x$falsePositiveCount[-length(x$falsePositiveCount)] - x$falsePositiveCount[-1],
      x$falsePositiveCount[length(x$falsePositiveCount)]
    )

    vals <- c()
    for (i in 1:length(x$preferenceThreshold)) {
      if (i != length(x$preferenceThreshold)) {
        upper <- x$preferenceThreshold[i + 1]
      } else {
        upper <- 1
      }
      val <- x$preferenceThreshold[i] + stats::runif(x$out[i]) * (upper - x$preferenceThreshold[i])
      vals <- c(val, vals)
    }
    vals <- vals[!is.na(vals)]

    vals2 <- c()
    for (i in 1:length(x$preferenceThreshold)) {
      if (i != length(x$preferenceThreshold)) {
        upper <- x$preferenceThreshold[i + 1]
      } else {
        upper <- 1
      }
      val2 <- x$preferenceThreshold[i] + stats::runif(x$nout[i]) * (upper - x$preferenceThreshold[i])
      vals2 <- c(val2, vals2)
    }
    vals2 <- vals2[!is.na(vals2)]

    x <- rbind(
      data.frame(variable = rep("outcome", length(vals)), value = vals),
      data.frame(variable = rep("No outcome", length(vals2)), value = vals2)
    )

    plots[[ind]] <- ggplot2::ggplot(x, ggplot2::aes(
      x = .data$value,
      group = .data$variable,
      fill = .data$variable
    )) +
      ggplot2::geom_density(ggplot2::aes(x = .data$value, fill = .data$variable), alpha = .3) +
      ggplot2::scale_x_continuous("Preference Threshold") + # , limits=c(0,1)) +
      ggplot2::scale_y_continuous("Density") +
      ggplot2::guides(fill = ggplot2::guide_legend(title = "Class")) +
      ggplot2::ggtitle(evalType)
  }

  plot <- gridExtra::marrangeGrob(plots, nrow = length(plots), ncol = 1)

  if (!is.null(saveLocation)) {
    if (!dir.exists(saveLocation)) {
      dir.create(saveLocation, recursive = TRUE)
    }
    ggplot2::ggsave(file.path(saveLocation, fileName), plot, width = 5, height = 4.5, dpi = 400)
  }
  return(plot)
}



#' Plot the precision-recall curve using the sparse thresholdSummary data frame
#'
#' @details
#' Create a plot showing the precision-recall curve using the sparse thresholdSummary data frame
#'
#' @param plpResult            A plp result object as generated using the \code{\link{runPlp}} function.
#' @param typeColumn            The name of the column specifying the evaluation type
#' @param saveLocation          Directory to save plot (if NULL plot is not saved)
#' @param fileName              Name of the file to save to plot, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#' @examplesIf rlang::is_installed("ggplot2")
#' \donttest{ \dontshow{ # takes too long }
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n = 1000, seed = 42)
#' saveLoc <- file.path(tempdir(), "plotPrecisionRecall")
#' results <- runPlp(plpData, outcomeId = 3, saveDirectory = saveLoc)
#' plotPrecisionRecall(results)
#' # clean up
#' unlink(saveLoc, recursive = TRUE)
#' }
#' @export
plotPrecisionRecall <- function(
    plpResult,
    typeColumn = "evaluation",
    saveLocation = NULL,
    fileName = "roc.png") {
  evalTypes <- unique(plpResult$performanceEvaluation$thresholdSummary[, typeColumn])

  plots <- list()
  length(plots) <- length(evalTypes)

  for (i in 1:length(evalTypes)) {
    evalType <- evalTypes[i]

    N <- max(plpResult$performanceEvaluation$thresholdSummary %>%
      dplyr::filter(.data[[typeColumn]] == evalType) %>%
      dplyr::select("falseCount") %>%
      dplyr::pull(), na.rm = TRUE)


    O <- max(plpResult$performanceEvaluation$thresholdSummary %>%
      dplyr::filter(.data[[typeColumn]] == evalType) %>%
      dplyr::select("trueCount") %>%
      dplyr::pull(), na.rm = TRUE)

    inc <- O / (O + N)

    x <- plpResult$performanceEvaluation$thresholdSummary %>%
      dplyr::filter(.data[[typeColumn]] == evalType) %>%
      dplyr::select("positivePredictiveValue", "sensitivity")

    plots[[i]] <- ggplot2::ggplot(x, ggplot2::aes(.data$sensitivity, .data$positivePredictiveValue)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::scale_x_continuous("Recall") + # , limits=c(0,1)) +
      ggplot2::scale_y_continuous("Precision") + # , limits=c(0,1))
      ggplot2::geom_hline(
        yintercept = inc, linetype = "dashed",
        color = "red", linewidth = 1
      ) +
      ggplot2::ggtitle(evalType)
  }

  plot <- gridExtra::marrangeGrob(plots, nrow = length(plots), ncol = 1)

  if (!is.null(saveLocation)) {
    if (!dir.exists(saveLocation)) {
      dir.create(saveLocation, recursive = TRUE)
    }
    ggplot2::ggsave(file.path(saveLocation, fileName), plot, width = 5, height = 4.5, dpi = 400)
  }
  return(plot)
}



#' Plot the F1 measure efficiency frontier using the sparse thresholdSummary data frame
#'
#' @details
#' Create a plot showing the F1 measure efficiency frontier using the sparse thresholdSummary data frame
#'
#' @param plpResult            A plp result object as generated using the \code{\link{runPlp}} function.
#' @param typeColumn            The name of the column specifying the evaluation type
#' @param saveLocation          Directory to save plot (if NULL plot is not saved)
#' @param fileName              Name of the file to save to plot, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#' @examplesIf rlang::is_installed("ggplot2")
#' \donttest{ \dontshow{ # takes too long }
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n = 1000, seed = 42)
#' saveLoc <- file.path(tempdir(), "plotF1Measure")
#' results <- runPlp(plpData, outcomeId = 3, saveDirectory = saveLoc)
#' plotF1Measure(results)
#' # clean up
#' unlink(saveLoc, recursive = TRUE)
#' }
#' @export
plotF1Measure <- function(
    plpResult,
    typeColumn = "evaluation",
    saveLocation = NULL,
    fileName = "roc.png") {
  evalTypes <- unique(plpResult$performanceEvaluation$thresholdSummary[, typeColumn])

  plots <- list()
  length(plots) <- length(evalTypes)

  for (i in 1:length(evalTypes)) {
    evalType <- evalTypes[i]

    x <- plpResult$performanceEvaluation$thresholdSummary %>%
      dplyr::filter(.data[[typeColumn]] == evalType) %>%
      dplyr::select("predictionThreshold", "f1Score")

    if (sum(is.nan(x$f1Score)) > 0) {
      x <- x[!is.nan(x$f1Score), ]
      if (nrow(x) == 0) {
        return(NULL)
      }
    }

    plots[[i]] <- ggplot2::ggplot(x, ggplot2::aes(.data$predictionThreshold, .data$f1Score)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 1) +
      ggplot2::scale_x_continuous("predictionThreshold") + # , limits=c(0,1)) +
      ggplot2::scale_y_continuous("F1Score") + # , limits=c(0,1))
      ggplot2::ggtitle(evalType)
  }

  plot <- gridExtra::marrangeGrob(plots, nrow = length(plots), ncol = 1)

  if (!is.null(saveLocation)) {
    if (!dir.exists(saveLocation)) {
      dir.create(saveLocation, recursive = TRUE)
    }
    ggplot2::ggsave(file.path(saveLocation, fileName), plot, width = 5, height = 4.5, dpi = 400)
  }
  return(plot)
}


#' Plot the Observed vs. expected incidence, by age and gender
#'
#' @details
#' Create a plot showing the Observed vs. expected incidence, by age and gender
#' #'
#' @param plpResult            A plp result object as generated using the \code{\link{runPlp}} function.
#' @param typeColumn            The name of the column specifying the evaluation type
#' @param saveLocation          Directory to save plot (if NULL plot is not saved)
#' @param fileName              Name of the file to save to plot, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#' @examplesIf rlang::is_installed("ggplot2")
#' \donttest{
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n = 1000, seed = 42)
#' saveLoc <- file.path(tempdir(), "plotDemographicSummary")
#' plpResult <- runPlp(plpData, outcomeId = 3, saveDirectory = saveLoc)
#' plotDemographicSummary(plpResult)
#' # clean up 
#' unlink(saveLoc, recursive = TRUE)
#' }
#' @export
plotDemographicSummary <- function(
    plpResult,
    typeColumn = "evaluation",
    saveLocation = NULL,
    fileName = "roc.png") {
  evalTypes <- unique(plpResult$performanceEvaluation$demographicSummary[, typeColumn])

  plots <- list()
  length(plots) <- length(evalTypes)

  for (i in 1:length(evalTypes)) {
    evalType <- evalTypes[i]
    x <- plpResult$performanceEvaluation$demographicSummary %>%
      dplyr::filter(.data[[typeColumn]] == evalType) %>%
      dplyr::select(
        "ageGroup",
        "genGroup",
        "averagePredictedProbability",
        "PersonCountAtRisk",
        "PersonCountWithOutcome"
      )

    # remove -1 values:
    x$averagePredictedProbability[is.na(x$averagePredictedProbability)] <- 0
    x <- x[x$PersonCountWithOutcome != -1, ]
    if (nrow(x) == 0) {
      return(NULL)
    }

    x$observed <- x$PersonCountWithOutcome / x$PersonCountAtRisk

    x <- x[, colnames(x) %in% c("ageGroup", "genGroup", "averagePredictedProbability", "observed")]

    # if age or gender missing add
    if (sum(colnames(x) == "ageGroup") == 1 && sum(colnames(x) == "genGroup") == 0) {
      x$genGroup <- rep("Non", nrow(x))
      evaluation$demographicSummary$genGroup <- rep("Non", nrow(evaluation$demographicSummary))
    }
    if (sum(colnames(x) == "ageGroup") == 0 && sum(colnames(x) == "genGroup") == 1) {
      x$ageGroup <- rep("-1", nrow(x))
      evaluation$demographicSummary$ageGroup <- rep("-1", nrow(evaluation$demographicSummary))
    }

    x <- tidyr::pivot_longer(
      data = x,
      cols = colnames(x)[!colnames(x) %in% c("ageGroup", "genGroup")],
      names_to = "variable",
      values_to = "value"
    )
    ci <- plpResult$performanceEvaluation$demographicSummary %>%
      dplyr::filter(.data[[typeColumn]] == evalType) %>%
      dplyr::select(
        "ageGroup",
        "genGroup",
        "averagePredictedProbability",
        "StDevPredictedProbability"
      )

    ci$StDevPredictedProbability[is.na(ci$StDevPredictedProbability)] <- 1
    ci$lower <- ci$averagePredictedProbability - 1.96 * ci$StDevPredictedProbability
    ci$lower[ci$lower < 0] <- 0
    ci$upper <- ci$averagePredictedProbability + 1.96 * ci$StDevPredictedProbability
    ci$upper[ci$upper > 1] <- max(ci$upper[ci$upper < 1])

    x$age <- gsub("Age group:", "", x$ageGroup)
    x$age <- factor(x$age, levels = c(
      " 0-4", " 5-9", " 10-14",
      " 15-19", " 20-24", " 25-29", " 30-34", " 35-39", " 40-44",
      " 45-49", " 50-54", " 55-59", " 60-64", " 65-69", " 70-74",
      " 75-79", " 80-84", " 85-89", " 90-94", " 95-99", "-1"
    ), ordered = TRUE)

    x <- merge(x, ci[, c("ageGroup", "genGroup", "lower", "upper")], by = c("ageGroup", "genGroup"))
    x <- x[!is.na(x$value), ]

    plots[[i]] <- ggplot2::ggplot(
      data = x,
      ggplot2::aes(
        x = .data$age,
        group = interaction(.data$variable, .data$genGroup)
      )
    ) +
      ggplot2::geom_line(ggplot2::aes(
        y = .data$value, group = .data$variable,
        color = .data$variable,
        linetype = .data$variable
      )) +
      ggplot2::geom_ribbon(
        data = x[x$variable != "observed", ],
        ggplot2::aes(
          ymin = .data$lower, ymax = .data$upper,
          group = .data$genGroup
        ),
        fill = "blue", alpha = 0.2
      ) +
      ggplot2::facet_grid(. ~ .data$genGroup, scales = "free") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
      ggplot2::scale_y_continuous("Fraction") +
      ggplot2::scale_x_discrete("Age") +
      ggplot2::scale_color_manual(
        values = c("royalblue4", "red"),
        guide = ggplot2::guide_legend(title = NULL),
        labels = c("Expected", "Observed")
      ) +
      ggplot2::guides(linetype = "none") + # change from FALSE due to warning
      ggplot2::ggtitle(evalType)
  }

  plot <- gridExtra::marrangeGrob(plots, nrow = length(plots), ncol = 1)

  if (!is.null(saveLocation)) {
    if (!dir.exists(saveLocation)) {
      dir.create(saveLocation, recursive = TRUE)
    }
    ggplot2::ggsave(file.path(saveLocation, fileName), plot, width = 5, height = 4.5, dpi = 400)
  }
  return(plot)
}


#' Plot the calibration
#'
#' @details
#' Create a plot showing the calibration
#' #'
#' @param plpResult            A plp result object as generated using the \code{\link{runPlp}} function.
#' @param typeColumn            The name of the column specifying the evaluation type
#' @param saveLocation          Directory to save plot (if NULL plot is not saved)
#' @param fileName              Name of the file to save to plot, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#' @examplesIf rlang::is_installed("ggplot2")
#' \donttest{ \dontshow{ # takes too long }
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n = 1000, seed = 42)
#' saveLoc <- file.path(tempdir(), "plotSparseCalibration")
#' results <- runPlp(plpData, outcomeId = 3, saveDirectory = saveLoc)
#' plotSparseCalibration(results)
#' # clean up 
#' unlink(saveLoc, recursive = TRUE)
#' }
#' @export
plotSparseCalibration <- function(
    plpResult,
    typeColumn = "evaluation",
    saveLocation = NULL,
    fileName = "roc.png") {
  evalTypes <- unique(plpResult$performanceEvaluation$calibrationSummary[, typeColumn])

  plots <- list()
  length(plots) <- length(evalTypes)

  for (i in 1:length(evalTypes)) {
    evalType <- evalTypes[i]
    x <- plpResult$performanceEvaluation$calibrationSummary %>%
      dplyr::filter(.data[[typeColumn]] == evalType) %>%
      dplyr::select("averagePredictedProbability", "observedIncidence")

    maxVal <- max(x$averagePredictedProbability, x$observedIncidence)
    model <- stats::lm(observedIncidence ~ averagePredictedProbability, data = x)
    res <- model$coefficients
    names(res) <- c("Intercept", "Gradient")

    # confidence int
    interceptConf <- stats::confint(model)[1, ]
    gradientConf <- stats::confint(model)[2, ]

    cis <- data.frame(
      lci = interceptConf[1] + seq(0, 1, length.out = nrow(x)) * gradientConf[1],
      uci = interceptConf[2] + seq(0, 1, length.out = nrow(x)) * gradientConf[2],
      x = seq(0, 1, length.out = nrow(x))
    )

    x <- cbind(x, cis)
    # TODO: CHECK INPUT
    plots[[i]] <- ggplot2::ggplot(
      data = x,
      ggplot2::aes(
        x = .data$averagePredictedProbability,
        y = .data$observedIncidence
      )
    ) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lci, ymax = .data$uci, x = x),
        fill = "blue", alpha = 0.2
      ) +
      ggplot2::geom_point(size = 1, color = "darkblue") +
      ggplot2::coord_cartesian(ylim = c(0, maxVal), xlim = c(0, maxVal)) +
      ggplot2::geom_abline(
        intercept = 0, slope = 1, linetype = 2, linewidth = 1,
        show.legend = TRUE
      ) +
      ggplot2::geom_abline(
        intercept = res["Intercept"], slope = res["Gradient"],
        linetype = 1, show.legend = TRUE,
        color = "darkblue"
      ) +
      ggplot2::scale_x_continuous("Average Predicted Probability") +
      ggplot2::scale_y_continuous("Observed Fraction With Outcome") +
      ggplot2::ggtitle(evalType)
  }

  plot <- gridExtra::marrangeGrob(plots, nrow = length(plots), ncol = 1)

  if (!is.null(saveLocation)) {
    if (!dir.exists(saveLocation)) {
      dir.create(saveLocation, recursive = TRUE)
    }
    ggplot2::ggsave(file.path(saveLocation, fileName), plot, width = 5, height = 4.5, dpi = 400)
  }
  return(plot)
}

#' Plot the conventional calibration
#'
#' @details
#' Create a plot showing the calibration
#' #'
#' @param plpResult            A plp result object as generated using the \code{\link{runPlp}} function.
#' @param typeColumn            The name of the column specifying the evaluation type
#' @param saveLocation          Directory to save plot (if NULL plot is not saved)
#' @param fileName              Name of the file to save to plot, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#' @examplesIf rlang::is_installed("ggplot2")
#' \donttest{ \dontshow{ # takes too long }
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n = 1000, seed = 42)
#' saveLoc <- file.path(tempdir(), "plotSparseCalibration2")
#' results <- runPlp(plpData, outcomeId = 3, saveDirectory = saveLoc)
#' plotSparseCalibration2(results)
#' # clean up
#' unlink(saveLoc, recursive = TRUE)
#' }
#' @export
plotSparseCalibration2 <- function(
    plpResult,
    typeColumn = "evaluation",
    saveLocation = NULL,
    fileName = "roc.png") {
  evalTypes <- unique(plpResult$performanceEvaluation$calibrationSummary[, typeColumn])

  plots <- list()
  length(plots) <- length(evalTypes)

  for (i in 1:length(evalTypes)) {
    evalType <- evalTypes[i]
    x <- plpResult$performanceEvaluation$calibrationSummary %>%
      dplyr::filter(.data[[typeColumn]] == evalType) %>%
      dplyr::select("averagePredictedProbability", "observedIncidence", "PersonCountAtRisk")

    cis <- apply(x, 1, function(x) stats::binom.test(round(x[2] * x[3]), x[3], alternative = c("two.sided"), conf.level = 0.95)$conf.int)
    x$lci <- cis[1, ]
    x$uci <- cis[2, ]

    maxes <- max(max(x$averagePredictedProbability), max(x$observedIncidence)) * 1.1

    limits <- ggplot2::aes(ymax = .data$uci, ymin = .data$lci)

    plots[[i]] <- ggplot2::ggplot(
      data = x,
      ggplot2::aes(x = .data$averagePredictedProbability, y = .data$observedIncidence)
    ) +
      ggplot2::geom_point(size = 2, color = "black") +
      ggplot2::geom_errorbar(limits) +
      ggplot2::geom_line(colour = "darkgrey") +
      ggplot2::geom_abline(
        intercept = 0, slope = 1, linetype = 5, linewidth = 0.4,
        show.legend = TRUE
      ) +
      ggplot2::scale_x_continuous("Average Predicted Probability") +
      ggplot2::scale_y_continuous("Observed Fraction With Outcome") +
      ggplot2::coord_cartesian(xlim = c(0, maxes), ylim = c(0, maxes)) +
      ggplot2::ggtitle(evalType)
  }

  plot <- gridExtra::marrangeGrob(plots, nrow = length(plots), ncol = 1)

  if (!is.null(saveLocation)) {
    if (!dir.exists(saveLocation)) {
      dir.create(saveLocation, recursive = TRUE)
    }
    ggplot2::ggsave(file.path(saveLocation, fileName), plot, width = 5, height = 4.5, dpi = 400)
  }
  return(plot)
}

#' Plot the smooth calibration as detailed in Calster et al. "A calibration heirarchy for risk models
#' was defined: from utopia to empirical data" (2016)
#'
#' @details
#' Create a plot showing the smoothed calibration
#' @param plpResult       The result of running \code{\link{runPlp}} function. An object containing the
#'                        model or location where the model is save, the data selection settings, the
#'                        preprocessing and training settings as well as various performance measures
#'                        obtained by the model.
#' @param smooth          options: 'loess' or 'rcs'
#' @param span            This specifies the width of span used for loess. This will allow for faster
#'                        computing and lower memory usage.
#' @param nKnots          The number of knots to be used by the rcs evaluation. Default is 5
#' @param scatter         plot the decile calibrations as points on the graph. Default is False
#' @param bins            The number of bins for the histogram. Default is 20.
#' @param sample          If using loess then by default 20,000 patients will be sampled to save time
#' @param typeColumn      The name of the column specifying the evaluation type
#' @param saveLocation    Directory to save plot (if NULL plot is not saved)
#' @param fileName        Name of the file to save to plot, for example
#'                        'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                        supported file formats.
#' @return
#' A ggplot object.
#'
#' @examplesIf rlang::is_installed("ggplot2")
#' # generate prediction dataaframe with 1000 patients
#' predictedRisk <- stats::runif(1000)
#' # overconfident for high risk patients
#' actualRisk <- ifelse(predictedRisk < 0.5, predictedRisk, 0.5 + 0.5 * (predictedRisk - 0.5))
#' outcomeCount <- stats::rbinom(1000, 1, actualRisk)
#' # mock data frame
#' prediction <- data.frame(rowId = 1:1000,
#'                          value = predictedRisk, 
#'                          outcomeCount = outcomeCount,
#'                          evaluationType = "Test")
#' attr(prediction, "modelType") <- "binary"
#' calibrationSummary <- getCalibrationSummary(prediction, "binary",
#'                                             numberOfStrata = 10,
#'                                             typeColumn = "evaluationType")
#' plpResults <- list()
#' plpResults$performanceEvaluation$calibrationSummary <- calibrationSummary
#' plpResults$prediction <- prediction
#' plotSmoothCalibration(plpResults)
#' @export
plotSmoothCalibration <- function(plpResult,
                                  smooth = "loess",
                                  span = 0.75,
                                  nKnots = 5,
                                  scatter = FALSE,
                                  bins = 20,
                                  sample = TRUE,
                                  typeColumn = "evaluation",
                                  saveLocation = NULL,
                                  fileName = "smoothCalibration.pdf") {
  if (!smooth %in% c("loess", "rcs")) {
    stop(ParallelLogger::logError("Smooth type must be either 'loess' or 'rcs"))
  }
  if (nKnots < 3) {
    stop(ParallelLogger::logError("Number of knots must be larger than 3"))
  }
  if (smooth == "rcs") {
    rlang::check_installed("mgcv",
      reason = "mgcv is required for restricted cubic spline smoothing"
    )
  }

  evalTypes <-
    unique(plpResult$performanceEvaluation$calibrationSummary[, typeColumn])
  plots <- list()
  length(plots) <- length(evalTypes)

  failedEvalType <- rep(FALSE, length(evalTypes))
  names(failedEvalType) <- evalTypes
  for (i in seq_along(evalTypes)) {
    evalType <- evalTypes[i]
    ParallelLogger::logInfo(paste("Smooth calibration plot for "), evalType)

    if ("prediction" %in% names(plpResult)) {
      x <- plpResult$performanceEvaluation$calibrationSummary %>%
        dplyr::filter(.data[[typeColumn]] == evalType) %>%
        dplyr::select("averagePredictedProbability", "observedIncidence")

      prediction <- plpResult$prediction %>% dplyr::filter(.data$evaluationType == evalType)

      maxes <- max(max(x$averagePredictedProbability), max(x$observedIncidence))

      if (smooth == "rcs") {
        if (missing(nKnots)) {
          ParallelLogger::logInfo("Number of knots for the restricted cubic spline smoothing was automatically set to 5")
        }
      }

      y <- prediction$outcomeCount
      p <- prediction$value

      nma <- !is.na(p + y) # select only non-missing cases
      sumNA <- sum(!nma)
      if (sumNA > 0) {
        warning(ParallelLogger::logWarn(paste(sumNA, "observations deleted due to NA probabilities or outcomes")))
      }
      y <- y[nma]
      p <- p[nma]

      logit <- log(p / (1 - p)) # delete cases with 0 and 1 probs
      nonInf <- !is.infinite(logit)
      sumNonInf <- sum(!nonInf)
      if (sumNonInf > 0) {
        warning(ParallelLogger::logWarn(paste(sumNonInf, "observations deleted due to probabilities of 0 or 1")))
      }
      y <- y[nonInf]
      p <- p[nonInf]

      y <- y[order(p)]
      p <- p[order(p)]

      if (smooth == "loess") {
        if (sample) {
          if (length(p) > 40000) {
            inds <- unique(c(0, seq(0, length(p),
              by = floor(length(p) / 20000)
            ), length(p)))
            p <- p[inds]
            y <- y[inds]
          } else if (length(p) > 20000) {
            inds <- sample(length(p), 20000)
            p <- p[inds]
            y <- y[inds]
          }
        }
        # loess
        smoothData <- data.frame(y, p)
        smoothPlot <- plotSmoothCalibrationLoess(data = smoothData, span = span) +
          ggplot2::coord_cartesian(
            xlim = c(0, maxes),
            ylim = c(0, maxes)
          )
      } else {
        # Restricted cubic splines

        smoothData <- data.frame(y, p)
        smoothPlot <- plotSmoothCalibrationRcs(data = smoothData, numberOfKnots = nKnots)
        if (is.character(smoothPlot)) {
          plots[[i]] <- smoothPlot
          failedEvalType[evalTypes[i]] <- TRUE
          next
        }

        smoothPlot <- smoothPlot +
          ggplot2::coord_cartesian(
            xlim = c(0, maxes),
            ylim = c(0, maxes)
          )
      }
      # construct the plot grid
      if (scatter) {
        smoothPlot <- smoothPlot +
          ggplot2::geom_point(
            data = x,
            ggplot2::aes(
              x = .data$averagePredictedProbability,
              y = .data$observedIncidence
            ),
            color = "black",
            size = 2
          )
      }

      # Histogram object detailing the distibution of event/noevent for each probability interval
      count <- NULL
      histPlot <- ggplot2::ggplot() +
        ggplot2::geom_histogram(
          data = prediction,
          ggplot2::aes(
            x = .data$value,
            y = ggplot2::after_stat(count),
            fill = as.character(.data$outcomeCount) # MAYBE ISSUE
          ),
          bins = bins,
          position = "stack",
          alpha = 0.5,
          boundary = 0,
          closed = "left"
        ) +
        ggplot2::facet_grid(.data$outcomeCount ~ ., scales = "free_y") +
        ggplot2::scale_fill_discrete(name = "Outcome") +
        ggplot2::theme(
          strip.background = ggplot2::element_blank(),
          strip.text = ggplot2::element_blank()
        ) +
        ggplot2::labs(x = "Predicted Probability") +
        ggplot2::coord_cartesian(xlim = c(0, maxes))
    } else {
      # use calibrationSummary
      sparsePred <- plpResult$performanceEvaluation$calibrationSummary %>%
        dplyr::filter(.data[[typeColumn]] == evalType) %>%
        dplyr::mutate(
          y = .data$observedIncidence,
          p = .data$averagePredictedProbability
        )

      smoothPlot <- plotSmoothCalibrationLoess(data = sparsePred, span = span)

      # construct the plot grid
      if (scatter) {
        smoothPlot <- smoothPlot +
          ggplot2::geom_point(
            data = sparsePred,
            ggplot2::aes(
              x = .data$p,
              y = .data$y
            ),
            size = 1,
            color = "black",
            show.legend = FALSE
          )
      }

      # Histogram object detailing the distibution of event/noevent for each probability interval

      popData1 <- sparsePred[, c("averagePredictedProbability", "PersonCountWithOutcome")]
      popData1$Label <- "Outcome"
      colnames(popData1) <- c(
        "averagePredictedProbability", "PersonCount",
        "Label"
      )
      popData2 <- sparsePred[, c(
        "averagePredictedProbability",
        "PersonCountAtRisk"
      )]
      popData2$Label <- "No Outcome"
      popData2$PersonCountAtRisk <- -1 * (popData2$PersonCountAtRisk - popData1$PersonCount)
      colnames(popData2) <- c(
        "averagePredictedProbability", "PersonCount",
        "Label"
      )
      popData <- rbind(popData1, popData2)
      popData$averagePredictedProbability <-
        factor(popData$averagePredictedProbability)
      histPlot <- ggplot2::ggplot(
        data = popData,
        ggplot2::aes(
          y = .data$averagePredictedProbability,
          x = .data$PersonCount,
          fill = .data$Label
        )
      ) +
        ggplot2::geom_bar(
          data = popData[popData$Label == "Outcome", ],
          stat = "identity"
        ) +
        ggplot2::geom_bar(
          data = popData[popData$Label == "No Outcome", ],
          stat = "identity"
        ) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::scale_x_continuous(labels = abs) +
        ggplot2::coord_flip() +
        ggplot2::theme(
          axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank()
        )
    }

    if (!failedEvalType[i]) {
      plots[[i]]$smoothPlot <- smoothPlot
      plots[[i]]$histPlot <- histPlot
    }
  }

  names(plots) <- tolower(evalTypes)

  if (!is.null(saveLocation)) {
    if (!dir.exists(saveLocation)) {
      dir.create(saveLocation, recursive = TRUE)
    }
    for (i in seq_along(evalTypes)) {
      if (!failedEvalType[i]) {
        gridPlot <- gridExtra::grid.arrange(
          plots[[i]]$smoothPlot,
          plots[[i]]$histPlot,
          ncol = 1,
          nrow = 2,
          heights = c(2, 1)
        )
        fileNameComponents <- unlist(strsplit(fileName, split = "\\."))
        n <- length(fileNameComponents)
        if (n > 2) {
          actualFileName <- paste(
            fileNameComponents[1:(n - 1)],
            collapse = "."
          )
        } else {
          actualFileName <- fileNameComponents[1]
        }
        saveFileName <- paste0(
          actualFileName,
          evalTypes[i],
          ".",
          fileNameComponents[n]
        )
        ggplot2::ggsave(
          file.path(saveLocation, saveFileName),
          gridPlot,
          width = 5,
          height = 4.5,
          dpi = 400
        )
      }
    }
  }
  return(plots)
}

#' Plot the net benefit
#' @param plpResults list of (named) plpResult objects or a single plpResult as 
#' generated using the \code{\link{runPlp}} function.
#' @param modelNames (optional) names of the models to be used in the plot. If NULL, the names of the plpResults are used. Must have the same length as plpResults.
#' @param typeColumn The name of the column specifying the evaluation type
#' @param saveLocation Directory to save plot (if NULL plot is not saved)
#' @param showPlot If TRUE, the plot is shown on the screen, if FALSE the plot 
#' object is returned without plotting.
#' @param fileName Name of the file to save to plot, for example 'plot.png'. See the function \code{ggsave} in the ggplot2 package for supported file formats.
#' @param evalType Which evaluation type to plot for. For example `Test`, `Train`. If NULL everything is plotted
#' @param ylim The y limits for the plot, if NULL the limits are calculated from the data
#' @param xlim The x limits for the plot, if NULL the limits are calculated from the data
#' @return A list of ggplot objects or a single ggplot object if only one evaluation type is plotted
#' @examplesIf rlang::is_installed("ggplot2")
#' \donttest{ \dontshow{ # takes too long }
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n = 1000, seed = 42)
#' saveLoc <- file.path(tempdir(), "plotNetBenefit")
#' results <- runPlp(plpData, outcomeId = 3, saveDirectory = saveLoc)
#' plotNetBenefit(results)
#' # clean up
#' unlink(saveLoc, recursive = TRUE)
#' }
#' @export
plotNetBenefit <- function(plpResults,
                           modelNames = NULL,
                           typeColumn = "evaluation",
                           saveLocation = NULL,
                           showPlot = TRUE,
                           fileName = "netBenefit.png",
                           evalType = NULL,
                           ylim = NULL,
                           xlim = NULL) {
  if (inherits(plpResults, "runPlp")) plpResults <- list(plpResults)
  nModels <- length(plpResults)

  if (is.null(modelNames)) {
    modelNames <- names(plpResults)
    if (any(modelNames == "") || is.null(modelNames)) {
      modelNames <- paste0("Model ", seq_len(nModels))
    }
  }
  if (length(modelNames) != nModels) {
    stop("modelNames must have the same length as plpResults, ",
         "but found ", length(modelNames), " model names and ", nModels, " plpResults.")
  }
  
  extractNB <- function(results, evalType, modelName) {
    nb <- getNetBenefit(results, evalType)
    nb$model <- modelName
    nb$evalType <- evalType
    return(nb)
  }

  if (is.null(evalType)) {
    evalType <- unique(plpResults[[1]]$performanceEvaluation$thresholdSummary[, typeColumn])
  }

  nbNested <- lapply(seq_along(plpResults), function(i) {
    res <- plpResults[[i]]
    lapply(evalType, 
      extractNB,
      results = res,
      modelName = modelNames[i])
  })
  nbAll <- nbNested %>%
    unlist(recursive = FALSE) %>%
    dplyr::bind_rows()

  treatLines <- nbAll %>% 
    dplyr::select("threshold", "treatAll", "treatNone", "evalType") %>%
    dplyr::distinct()


  if (is.null(ylim)) {
    ylim <- range(
      nbAll$netBenefit
    )
  }
  if (is.null(xlim)) {
    allThresholds <- nbAll$threshold
    cal1 <- plpResults[[1]]$performanceEvaluation$calibrationSummary  
    maxCal <- max(cal1$averagePredictedProbability, cal1$observedIncidence, na.rm = TRUE)
    xlim <- c(min(allThresholds, na.rm = TRUE), maxCal)
  }

  modCols <- grDevices::hcl.colors(nModels, palette = "Dark 3")
  names(modCols) <- modelNames
  modCols[["Treat All"]] <- "red"
  modCols[["Treat None"]] <- "brown"

  ltVals <- c(setNames(rep("solid", nModels), modelNames),
              "Treat All" = "dashed",
              "Treat None" = "dashed")
  legendOrder <- c(modelNames, "Treat All", "Treat None")
  plot <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = treatLines,
      ggplot2::aes(.data$threshold, .data$treatAll, linetype = "Treat All", color = "Treat All")) +
    ggplot2::geom_line(
      data = treatLines,
      ggplot2::aes(.data$threshold, .data$treatNone, linetype = "Treat None", color = "Treat None")) +
    ggplot2::geom_line(
      data = nbAll,
      ggplot2::aes(.data$threshold, .data$netBenefit, color = .data$model, linetype = .data$model)
    ) +
    ggplot2::scale_color_manual(
      name = "Strategy",
      values = modCols,
      breaks = legendOrder) +
    ggplot2::scale_linetype_manual(
      name = "Strategy",
      values = ltVals,
      breaks = legendOrder) +
    ggplot2::labs(
      x = "Prediction Threshold",
      y = "Net Benefit"
    ) +
    ggplot2::facet_wrap(~ evalType) +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim) +
    ggplot2::ggtitle(evalType)

  if (!is.null(saveLocation)) {
    if (!dir.exists(saveLocation)) {
      dir.create(saveLocation, recursive = TRUE)
    }
    ggplot2::ggsave(file.path(saveLocation, fileName), plot, width = 5, height = 4.5, dpi = 400)
  }
  if (showPlot) {
    print(plot)
  }
  return(invisible(plot))
}

#' Calculate net benefit in an efficient way either from the thresholdSummary or the prediction dataframe
#' @param plpResult A plp result object as generated using the \code{\link{runPlp}} function.
#' @param evalType The evaluation type column
#' @return A data frame with the net benefit, treat all and treat none values
#' @noRd
#' @keywords internal
getNetBenefit <- function(plpResult, evalType) {
  if (is.null(plpResult$prediction)) {
    # get net benefit from thresholdSummary
    nbData <- getNetBenefitThresholdSummary(plpResult, evalType)
  } else {
    nbData <- getNetBenefitPredictions(plpResult, evalType)

  }
  return(nbData)
}

getNetBenefitThresholdSummary <- function(plpResult, evalType) {
  thresholdSummary <- plpResult$performanceEvaluation$thresholdSummary %>%
    dplyr::filter(.data$evaluation == evalType)

  if (nrow(thresholdSummary) == 0) {
    stop("No threshold summary data found for evaluation type ", evalType)
  }

  prediction <- thresholdSummary %>%
    dplyr::select(
      "predictionThreshold", "falsePositiveCount", "truePositiveCount",
    ) 
  n <- thresholdSummary$positiveCount[[1]] + thresholdSummary$negativeCount[[1]]
  evaluationStatistics <- plpResult$performanceEvaluation$evaluationStatistics %>%
    dplyr::filter(.data$evaluation == evalType)
  outcomeRate <- evaluationStatistics %>%
    dplyr::filter(.data$metric == "outcomeCount") %>%
    dplyr::pull(.data$value) %>% 
    as.numeric() / n
  nbData <- prediction %>%
    dplyr::mutate(
      threshold = .data$predictionThreshold,
      netBenefit = .data$truePositiveCount / n - (.data$falsePositiveCount / n) * (.data$predictionThreshold / (1 - .data$predictionThreshold)),
      treatAll = outcomeRate - (1 - outcomeRate) * .data$predictionThreshold / (1 - .data$predictionThreshold),
      treatNone = 0
    )
  return(nbData)
}

# get net benefit from prediction dataframe
getNetBenefitPredictions <- function(plpResult, evalType) {
  prediction <- plpResult$prediction %>% dplyr::filter(.data$evaluationType == evalType)

  if (nrow(prediction) == 0) {
    stop("No prediction data found for evaluation type ", evalType)
  }
  prediction <- prediction %>%
    dplyr::arrange(dplyr::desc(.data$value)) %>%
    dplyr::mutate(
      cumsumTrue = cumsum(.data$outcomeCount),
      cumsumFalse = cumsum(1 - .data$outcomeCount)
    )
  trueCount <- sum(prediction$outcomeCount)
  n <- nrow(prediction)
  falseCount <- n - trueCount
  outcomeRate <- trueCount / n

  nbData <- prediction %>%
    dplyr::group_by(.data$value) %>%
    dplyr::summarise(
      threshold = unique(.data$value),
      TP = max(.data$cumsumTrue),
      FP = max(.data$cumsumFalse),
    ) %>%
    dplyr::ungroup()

  nbData <- nbData %>%
    dplyr::mutate(
      netBenefit = (.data$TP / n) - (.data$FP / n) * (.data$threshold / (1 - .data$threshold)),
      treatAll = outcomeRate - (1 - outcomeRate) * .data$threshold / (1 - .data$threshold),
      treatNone = 0
    )
  return(nbData)
}

plotSmoothCalibrationLoess <- function(data, span = 0.75) {
  fit <- stats::loess(y ~ p, data = data, degree = 2, span = span)
  predictedFit <- stats::predict(fit, se = TRUE)
  data <- data %>%
    dplyr::mutate(
      calibration = predictedFit$fit,
      se = predictedFit$se,
      lci = .data$calibration - stats::qt(.975, predictedFit$df) * .data$se,
      uci = .data$calibration + stats::qt(.975, predictedFit$df) * .data$se
    )

  plot <- ggplot2::ggplot(
    data = data,
    ggplot2::aes(
      x = .data$p,
      y = .data$calibration
    )
  ) +
    ggplot2::geom_line(
      ggplot2::aes(
        color = "Loess",
        linetype = "Loess"
      )
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = .data$lci,
        ymax = .data$uci
      ),
      fill = "blue",
      alpha = 0.2
    ) +
    ggplot2::annotate(
      geom = "segment",
      x = 0,
      xend = 1,
      y = 0,
      yend = 1,
      color = "red",
      linetype = "dashed"
    ) +
    ggplot2::scale_linetype_manual(
      name = "Models",
      values = c(
        Loess = "solid",
        Ideal = "dashed"
      )
    ) +
    ggplot2::scale_color_manual(
      name = "Models",
      values = c(
        Loess = "blue",
        Ideal = "red"
      )
    ) +
    ggplot2::labs(
      x = "Predicted Probability",
      y = "Observed Probability"
    )

  return(plot)
}

plotSmoothCalibrationRcs <- function(data, numberOfKnots) {
  data <- data %>%
    dplyr::filter(!is.na(.data$y) & !is.na(.data$p))
  p <- data$p

  .defineKnots <- function(predictedProbabilities, numberOfKnots) {
    if (numberOfKnots == 3) {
      lowestQuantile <- .1
      highestQuantile <- .9
    } else if (numberOfKnots > 3 && numberOfKnots <= 6) {
      lowestQuantile <- .05
      highestQuantile <- .95
    } else if (numberOfKnots == 7) {
      lowestQuantile <- .025
      highestQuantile <- .975
    } else {
      # use mgcv defaults
      return(numberOfKnots)
    }
    knotQuantiles <- seq(
      lowestQuantile,
      highestQuantile,
      length.out = numberOfKnots
    )

    knotLocation <- stats::quantile(
      x = predictedProbabilities,
      probs = knotQuantiles,
      na.rm = TRUE
    )

    return(knotLocation)
  }

  for (k in numberOfKnots:3) {
    if (k > 7) {
      smoothFit <- tryCatch(
        expr = {
          mgcv::gam(
            y ~ s(p, bs = "cr", k = k, m = 2),
            data = data,
            family = stats::binomial()
          )
        },
        error = function(e) {
          return("Failed")
        }
      )
    } else {
      smoothFit <- tryCatch(
        expr = {
          mgcv::gam(
            y ~ s(p, bs = "cr", k = k, m = 2),
            data = data,
            knots = list(p = .defineKnots(p, k)),
            family = stats::binomial()
          )
        },
        error = function(e) {
          return("Failed")
        }
      )
    }
    if (is.character(smoothFit)) {
      if (k > 3) {
        ParallelLogger::logInfo(paste0("Setting number of Knots to ", k, " led to estimation problems. Switching to nKnots = ", k - 1))
      } else {
        ParallelLogger::logInfo(paste0("Unable to fit model"))
      }
    } else {
      break
    }
  }

  if (is.character(smoothFit)) {
    return("Failed")
  }

  xRange <- seq(min(p), max(p), length.out = 1e3)
  predictWithSe <- stats::predict(smoothFit, newdata = data.frame(p = xRange), se.fit = TRUE)
  smoothData <- data.frame(
    xRange = xRange,
    predXRange = stats::plogis(predictWithSe$fit),
    lci = stats::plogis(predictWithSe$fit - 1.96 * predictWithSe$se.fit),
    uci = stats::plogis(predictWithSe$fit + 1.96 * predictWithSe$se.fit)
  )
  plot <- ggplot2::ggplot(
    data = smoothData,
    ggplot2::aes(
      x = .data$xRange,
      y = .data$predXRange
    )
  ) +
    ggplot2::geom_line(
      ggplot2::aes(
        color = "rcs",
        linetype = "rcs"
      )
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = .data$lci,
        ymax = .data$uci
      ),
      fill = "blue",
      alpha = 0.2,
      show.legend = FALSE
    ) +
    ggplot2::geom_abline(
      mapping = ggplot2::aes(
        slope = 1,
        intercept = 0,
        color = "Ideal",
        linetype = "Ideal"
      ),
      show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(
      name = "Models",
      values = c(rcs = "blue", Ideal = "red")
    ) +
    ggplot2::scale_linetype_manual(
      name = "Models",
      values = c(rcs = "solid", Ideal = "dashed")
    ) +
    ggplot2::labs(x = "", y = "Observed Probability")

  return(plot)
}

#' Plot the side-by-side boxplots of prediction distribution, by class
#' @details
#' Create a plot showing the side-by-side boxplots of prediction distribution, by class
#' #'
#' @param plpResult            A plp result object as generated using the \code{\link{runPlp}} function.
#' @param typeColumn            The name of the column specifying the evaluation type
#' @param saveLocation          Directory to save plot (if NULL plot is not saved)
#' @param fileName              Name of the file to save to plot, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#' @examplesIf rlang::is_installed("ggplot2")
#' \donttest{ \dontshow{ # takes too long }
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n = 1000, seed = 42)
#' saveLoc <- file.path(tempdir(), "plotPredictionDistribution")
#' results <- runPlp(plpData, outcomeId = 3, saveDirectory = saveLoc)
#' plotPredictionDistribution(results)
#' # clean up
#' unlink(saveLoc, recursive = TRUE)
#' }
#' @export
plotPredictionDistribution <- function(
    plpResult,
    typeColumn = "evaluation",
    saveLocation = NULL,
    fileName = "PredictionDistribution.png") {
  evalTypes <- unique(plpResult$performanceEvaluation$predictionDistribution[, typeColumn])

  plots <- list()
  length(plots) <- length(evalTypes)

  for (i in 1:length(evalTypes)) {
    evalType <- evalTypes[i]
    x <- plpResult$performanceEvaluation$predictionDistribution %>%
      dplyr::filter(.data[[typeColumn]] == evalType)

    non05 <- x$P05PredictedProbability[x$class == 0]
    non95 <- x$P95PredictedProbability[x$class == 0]
    one05 <- x$P05PredictedProbability[x$class == 1]
    one95 <- x$P95PredictedProbability[x$class == 1]

    plots[[i]] <- ggplot2::ggplot(
      x,
      ggplot2::aes(
        x = as.factor(class),
        ymin = .data$MinPredictedProbability,
        lower = .data$P25PredictedProbability,
        middle = .data$MedianPredictedProbability,
        upper = .data$P75PredictedProbability,
        ymax = .data$MaxPredictedProbability,
        color = as.factor(.data$class)
      )
    ) +
      ggplot2::coord_flip() +
      ggplot2::geom_boxplot(stat = "identity") +
      ggplot2::scale_x_discrete("Class") +
      ggplot2::scale_y_continuous("Predicted Probability") +
      ggplot2::theme(legend.position = "none") +
      ggplot2::annotate("segment",
        x = 0.9, xend = 1.1, y = non05, yend = non05,
        color = "red"
      ) +
      ggplot2::annotate("segment",
        x = 0.9, xend = 1.1, y = non95, yend = non95,
        color = "red"
      ) +
      ggplot2::annotate("segment",
        x = 1.9, xend = 2.1, y = one05, yend = one05,
        color = "#00BFC4"
      ) +
      ggplot2::annotate("segment",
        x = 1.9, xend = 2.1, y = one95, yend = one95,
        color = "#00BFC4"
      ) +
      ggplot2::ggtitle(evalType)
  }

  plot <- gridExtra::marrangeGrob(plots, nrow = length(plots), ncol = 1)

  if (!is.null(saveLocation)) {
    if (!dir.exists(saveLocation)) {
      dir.create(saveLocation, recursive = TRUE)
    }
    ggplot2::ggsave(file.path(saveLocation, fileName), plot, width = 5, height = 4.5, dpi = 400)
  }
  return(plot)
}

#' Plot the variable importance scatterplot
#'
#' @details
#' Create a plot showing the variable importance scatterplot
#' #'
#' @param covariateSummary      A prediction object as generated using the
#'                              \code{\link{runPlp}} function.
#' @param saveLocation          Directory to save plot (if NULL plot is not saved)
#' @param fileName              Name of the file to save to plot, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#' @examplesIf rlang::is_installed("ggplot2")
#' \donttest{ \dontshow{ # takes too long }
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n = 1000, seed = 42)
#' saveLoc <- file.path(tempdir(), "plotVariableScatterplot")
#' results <- runPlp(plpData, outcomeId = 3, saveDirectory = saveLoc)
#' plotVariableScatterplot(results$covariateSummary)
#' # clean up
#' }
#' @export
plotVariableScatterplot <- function(
    covariateSummary,
    saveLocation = NULL,
    fileName = "VariableScatterplot.png") {
  # remove the non-incidence variables from the plot
  covariateSummary <- covariateSummary[covariateSummary$WithNoOutcome_CovariateMean <= 1, ]

  covariateSummary$size <- rep(0.1, nrow(covariateSummary))
  covariateSummary$size[covariateSummary$covariateValue != 0] <- 0.5

  plot <- ggplot2::ggplot(covariateSummary, ggplot2::aes(
    y = .data$WithOutcome_CovariateMean,
    x = .data$WithNoOutcome_CovariateMean,
    size = .data$size
  )) +
    ggplot2::geom_point(ggplot2::aes(color = .data$size)) +
    ggplot2::scale_size(range = c(0, 1)) +
    ggplot2::scale_colour_gradient2(low = "red", mid = "blue", high = "green") +
    ggplot2::scale_y_continuous("Outcome Covariate Mean") +
    ggplot2::scale_x_continuous("Non-outcome Covariate Mean") +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 2) +
    ggplot2::theme(legend.position = "none")

  if (!is.null(saveLocation)) {
    suppressWarnings(ggplot2::ggsave(file.path(saveLocation, fileName), plot, width = 5, height = 3.5, dpi = 400))
  }
  return(plot)
}


#' Plot the train/test generalizability diagnostic
#'
#' @details
#' Create a plot showing the train/test generalizability diagnostic
#' #'
#' @param covariateSummary      A prediction object as generated using the
#'                              \code{\link{runPlp}} function.
#' @param saveLocation          Directory to save plot (if NULL plot is not saved)
#' @param fileName              Name of the file to save to plot, for example
#'                              'plot.png'. See the function \code{ggsave} in the ggplot2 package for
#'                              supported file formats.
#'
#' @return
#' A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
#' format.
#' @examplesIf rlang::is_installed("ggplot2")
#' \donttest{ \dontshow{ # takes too long }
#' data("simulationProfile")
#' plpData <- simulatePlpData(simulationProfile, n = 1000, seed = 42)
#' population <- createStudyPopulation(plpData, outcomeId = 3)
#' data <- splitData(plpData, population = population)
#' strata <- data.frame(
#'  rowId = c(data$Train$labels$rowId, data$Test$labels$rowId),
#'  strataName = c(rep("Train", nrow(data$Train$labels)),
#'                 rep("Test", nrow(data$Test$labels))))
#' covariateSummary <- covariateSummary(plpData$covariateData, 
#'                                      cohort = dplyr::select(population, "rowId"),
#'  strata = strata, labels = population)
#' plotGeneralizability(covariateSummary)
#' }
#' @export
plotGeneralizability <- function(
    covariateSummary,
    saveLocation = NULL,
    fileName = "Generalizability.png") {
  covariateSummary$TrainWithOutcome_CovariateMean[is.na(covariateSummary$TrainWithOutcome_CovariateMean)] <- 0
  covariateSummary$TestWithOutcome_CovariateMean[is.na(covariateSummary$TestWithOutcome_CovariateMean)] <- 0

  covariateSummary <- covariateSummary[covariateSummary$TrainWithOutcome_CovariateMean <= 1, ]

  plot1 <- ggplot2::ggplot(
    covariateSummary,
    ggplot2::aes(
      .data$TrainWithOutcome_CovariateMean,
      .data$TestWithOutcome_CovariateMean
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous("Train set Mean") +
    ggplot2::scale_y_continuous("Test Set Mean") +
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 2) +
    ggplot2::ggtitle("Outcome")


  covariateSummary$TrainWithNoOutcome_CovariateMean[is.na(covariateSummary$TrainWithNoOutcome_CovariateMean)] <- 0
  covariateSummary$TestWithNoOutcome_CovariateMean[is.na(covariateSummary$TestWithNoOutcome_CovariateMean)] <- 0

  plot2 <- ggplot2::ggplot(
    covariateSummary,
    ggplot2::aes(
      .data$TrainWithNoOutcome_CovariateMean,
      .data$TestWithNoOutcome_CovariateMean
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous("Train set Mean") +
    ggplot2::scale_y_continuous("Test Set Mean") +
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 2) +
    ggplot2::ggtitle("No Outcome")

  plot <- gridExtra::grid.arrange(plot1, plot2, ncol = 2)

  if (!is.null(saveLocation)) {
    ggplot2::ggsave(file.path(saveLocation, fileName), plot, width = 5, height = 3.5, dpi = 400)
  }
  return(plot)
}
