% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Ensemble.R
\name{createEnsemble}
\alias{createEnsemble}
\title{Combine models into an Ensemble}
\usage{
createEnsemble(runPlpList, weighted = F, weights = NULL)
}
\arguments{
\item{runPlpList}{The runPlp results for the different models to combine}

\item{weighted}{If F then mean across models is used, if T must input weights or AUC weighting is used}

\item{weights}{A vector of length(runPlpList) with the weights to assign each model}
}
\description{
Combine models into an Ensemble
}
