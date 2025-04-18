% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ImportFromCsv.R
\name{insertCsvToDatabase}
\alias{insertCsvToDatabase}
\title{Function to insert results into a database from csvs}
\usage{
insertCsvToDatabase(
  csvFolder,
  connectionDetails,
  databaseSchemaSettings,
  modelSaveLocation,
  csvTableAppend = ""
)
}
\arguments{
\item{csvFolder}{The location to the csv folder with the plp results}

\item{connectionDetails}{A connection details for the plp results database that the csv results will be inserted into}

\item{databaseSchemaSettings}{A object created by \code{createDatabaseSchemaSettings} with all the settings specifying the result tables to insert the csv results into}

\item{modelSaveLocation}{The location to save any models from the csv folder - this should be the same location you picked when inserting other models into the database}

\item{csvTableAppend}{A string that appends the csv file names}
}
\value{
Returns a data.frame indicating whether the results were inported into the database
}
\description{
This function converts a folder with csv results into plp objects and loads
them into a plp result database
}
\details{
The user needs to have plp csv results in a single folder and an existing plp
result database
}
\examples{
\donttest{ \dontshow{ # takes too long }
# develop a simple model on simulated data
data("simulationProfile")
plpData <- simulatePlpData(simulationProfile, n=1000)
saveLoc <- file.path(tempdir(), "extractDatabaseToCsv")
results <- runPlp(plpData, outcomeId=3, saveDirectory=saveLoc)
# now upload the results to a sqlite database
databasePath <- insertResultsToSqlite(saveLoc)
# now extract the results to csv
connectionDetails <- 
 DatabaseConnector::createConnectionDetails(dbms = "sqlite", 
                                            server = databasePath)
extractDatabaseToCsv(connectionDetails = connectionDetails,
                     csvFolder = file.path(saveLoc, "csv"))
# show csv file
list.files(file.path(saveLoc, "csv"))
# now insert the csv results into a database
newDatabasePath <- file.path(tempdir(), "newDatabase.sqlite")
connectionDetails <- 
 DatabaseConnector::createConnectionDetails(dbms = "sqlite", 
                                            server = newDatabasePath)
insertCsvToDatabase(csvFolder = file.path(saveLoc, "csv"),
                     connectionDetails = connectionDetails,
                     databaseSchemaSettings = createDatabaseSchemaSettings(),
                     modelSaveLocation = file.path(saveLoc, "models"))
# clean up
unlink(saveLoc, recursive = TRUE)
}
}
