% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uploadToDatabaseDiagnostics.R
\name{addDiagnosePlpToDatabase}
\alias{addDiagnosePlpToDatabase}
\title{Insert a diagnostic result into a PLP result schema database}
\usage{
addDiagnosePlpToDatabase(
  diagnosePlp,
  connectionDetails,
  databaseSchemaSettings,
  cohortDefinitions,
  databaseList = NULL,
  overWriteIfExists = T
)
}
\arguments{
\item{diagnosePlp}{An object of class \code{diagnosePlp}}

\item{connectionDetails}{A connection details created by using the
function \code{createConnectionDetails} in the
\code{DatabaseConnector} package.}

\item{databaseSchemaSettings}{A object created by \code{createDatabaseSchemaSettings} with all the settings specifying the result tables}

\item{cohortDefinitions}{A set of one or more cohorts extracted using ROhdsiWebApi::exportCohortDefinitionSet()}

\item{databaseList}{(Optional) If you wish to overwrite the settings in the plp object use \code{createdatabaseList} to specify the databases}

\item{overWriteIfExists}{(default: T) Whether to delete existing results and overwrite them}
}
\value{
Returns NULL but uploads the diagnostic into the database schema specified in databaseSchemaSettings
}
\description{
This function inserts a diagnostic result into the result schema
}
\details{
This function can be used to upload a diagnostic result into a database
}
