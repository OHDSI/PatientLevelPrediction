% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EvaluationSummary.R
\name{computeAuc}
\alias{computeAuc}
\title{Compute the area under the ROC curve}
\usage{
computeAuc(prediction, confidenceInterval = FALSE)
}
\arguments{
\item{prediction}{A prediction object as generated using the
\code{\link{predict}} functions.}

\item{confidenceInterval}{Should 95 percebt confidence intervals be computed?}
}
\value{
A data.frame containing the AUC and optionally the 95\% confidence interval
}
\description{
Compute the area under the ROC curve
}
\details{
Computes the area under the ROC curve for the predicted probabilities, given the true observed
outcomes.
}
\examples{
prediction <- data.frame(
  value = c(0.1, 0.2, 0.3, 0.4, 0.5),
  outcomeCount = c(0, 1, 0, 1, 1))
computeAuc(prediction)
}
