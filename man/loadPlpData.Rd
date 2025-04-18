% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SaveLoadPlp.R
\name{loadPlpData}
\alias{loadPlpData}
\title{Load the plpData from a folder}
\usage{
loadPlpData(file, readOnly = TRUE)
}
\arguments{
\item{file}{The name of the folder containing the data.}

\item{readOnly}{If true, the data is opened read only.}
}
\value{
An object of class plpData.
}
\description{
\code{loadPlpData} loads an object of type plpData from a folder in the file
system.
}
\details{
The data will be written to a set of files in the folder specified by the user.
}
\examples{
data("simulationProfile")
plpData <- simulatePlpData(simulationProfile, n = 500)
saveLoc <- file.path(tempdir(), "loadPlpData")
savePlpData(plpData, saveLoc)
dir(saveLoc)
# clean up
unlink(saveLoc, recursive = TRUE)
}
