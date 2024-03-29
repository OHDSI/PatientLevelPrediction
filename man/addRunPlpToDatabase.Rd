% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uploadToDatabase.R
\name{addRunPlpToDatabase}
\alias{addRunPlpToDatabase}
\title{Function to add the run plp (development or validation) to database}
\usage{
addRunPlpToDatabase(
  runPlp,
  connectionDetails,
  databaseSchemaSettings,
  cohortDefinitions,
  modelSaveLocation,
  databaseList = NULL
)
}
\arguments{
\item{runPlp}{An object of class \code{runPlp} or class \code{externalValidatePlp}}

\item{connectionDetails}{A connection details created by using the
function \code{createConnectionDetails} in the
\code{DatabaseConnector} package.}

\item{databaseSchemaSettings}{A object created by \code{createDatabaseSchemaSettings} with all the settings specifying the result tables}

\item{cohortDefinitions}{A set of one or more cohorts extracted using ROhdsiWebApi::exportCohortDefinitionSet()}

\item{modelSaveLocation}{The location of the directory that models will be saved to}

\item{databaseList}{(Optional) If you want to change the database name then used \code{createDatabaseList} to specify the database settings but use the same cdmDatabaseId was model development/validation}
}
\value{
Returns a data.frame with the database details
}
\description{
This function adds a runPlp or external validation result into a database
}
\details{
This function is used when inserting results into the PatientLevelPrediction database results schema
}
