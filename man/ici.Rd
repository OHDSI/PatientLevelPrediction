% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EvaluationSummary.R
\name{ici}
\alias{ici}
\title{Calculate the Integrated Calibration Index from Austin and Steyerberg
https://onlinelibrary.wiley.com/doi/full/10.1002/sim.8281}
\usage{
ici(prediction)
}
\arguments{
\item{prediction}{the prediction object found in the plpResult object}
}
\value{
Integrated Calibration Index value or NULL if the calculation fails
}
\description{
Calculate the Integrated Calibration Index from Austin and Steyerberg
https://onlinelibrary.wiley.com/doi/full/10.1002/sim.8281
}
\details{
Calculate the Integrated Calibration Index
}
\examples{
prediction <- data.frame(rowId = 1:100, 
                        outcomeCount = stats::rbinom(1:100, 1, prob=0.5),
                        value = runif(100), 
                        evaluation = rep("Train", 100))
ici(prediction)
}
