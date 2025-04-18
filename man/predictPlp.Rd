% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Predict.R
\name{predictPlp}
\alias{predictPlp}
\title{predictPlp}
\usage{
predictPlp(plpModel, plpData, population, timepoint)
}
\arguments{
\item{plpModel}{An object of type \code{plpModel} - a patient level prediction model}

\item{plpData}{An object of type \code{plpData} - the patient level prediction
data extracted from the CDM.}

\item{population}{The population created using createStudyPopulation() who will have their risks predicted or a cohort without the outcome known}

\item{timepoint}{The timepoint to predict risk (survival models only)}
}
\value{
A data frame containing the predicted risk values
}
\description{
Predict the risk of the outcome using the input plpModel for the input plpData
}
\details{
The function applied the trained model on the plpData to make predictions
}
\examples{
coefficients <- data.frame(
  covariateId = c(1002),
  coefficient = c(0.05)
)
model <- createGlmModel(coefficients, intercept = -2.5)
data("simulationProfile")
plpData <- simulatePlpData(simulationProfile, n = 50)
prediction <- predictPlp(model, plpData, plpData$cohorts)
# see the predicted risk values
head(prediction)
}
