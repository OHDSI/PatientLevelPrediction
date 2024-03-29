% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SklearnClassifier.R
\name{computeGridPerformance}
\alias{computeGridPerformance}
\title{Computes grid performance with a specified performance function}
\usage{
computeGridPerformance(prediction, param, performanceFunct = "computeAuc")
}
\arguments{
\item{prediction}{a dataframe with predictions and outcomeCount per rowId}

\item{param}{a list of hyperparameters}

\item{performanceFunct}{a string specifying which performance function to use
. Default ``'compute_AUC'``}
}
\value{
A list with overview of the performance
}
\description{
Computes grid performance with a specified performance function
}
