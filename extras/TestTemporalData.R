library(SqlRender)
library(DatabaseConnector)
library(FeatureExtraction)
library(PatientLevelPrediction)
options(fftempdir = "/Users/Shared/tempff")

# Azure 
dbms <- "sql server"
user <- "prijnbeek@emif"
pw <- "secret"
server <- "emif.database.windows.net"
database <- "SYNPUF"
extraSettings <- "database=SYNPUF;encrypt=true;trustServerCertificate=false;hostNameInCertificate=*.database.windows.net;loginTimeout=30;"
port <- 1433
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port,
                                                                extraSettings = extraSettings)

# Postgres local 
dbms <- "postgresql"
user <- "postgres"
pw <- "secret"
server <- "localhost/SYNPUF1000"
port <- 5432
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)

# Postgres Server
dbms <- "postgresql"
user <- "postgres"
pw <- "secret"
server <- "Res-Srv-Lin-01/SYNPUF1000"
port <- 5432
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)

connection <- DatabaseConnector::connect(connectionDetails)

cdmDatabaseSchema <- "cdm"
workDatabaseSchema <- "scratch"
resultsDatabaseSchema <-"results"

sql <- loadRenderTranslateSql("coxibVsNonselVsGiBleed.sql",
                              packageName = "CohortMethod",
                              dbms = dbms,
                              cdmDatabaseSchema = cdmDatabaseSchema,
                              resultsDatabaseSchema = resultsDatabaseSchema)
DatabaseConnector::executeSql(connection, sql)

# Check number of subjects per cohort:
sql <- "SELECT cohort_definition_id, COUNT(*) AS count FROM results.coxibVsNonselVsGiBleed GROUP BY cohort_definition_id"
sql <- SqlRender::renderSql(sql, resultsDatabaseSchema = resultsDatabaseSchema)$sql
sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
DatabaseConnector::querySql(connection, sql)


settings <- createTemporalCovariateSettings(useCovariateConditionEraStart = FALSE,
                                            useCovariateConditionEraPresent = FALSE,
                                            useCovariateConditionGroup = FALSE,
                                            useCovariateConditionGroupMeddra = FALSE,
                                            useCovariateConditionGroupSnomed = FALSE,
                                            useCovariateDrugEraStart = TRUE,
                                            useCovariateDrugEraPresent = FALSE,
                                            useCovariateDrugGroup = TRUE,
                                            useCovariateMeasurementValue = FALSE,
                                            useCovariateMeasurementAbove = FALSE,
                                            useCovariateMeasurementBelow = FALSE,
                                            useCovariateProcedureOccurence = FALSE,
                                            useCovariateProcedureGroup = FALSE,
                                            useCovariateObservationOccurence = FALSE,
                                            useCovariateVisitOccurence = TRUE,
                                            useCovariateConceptCounts = FALSE)

covarData <- getDbCovariateData(connectionDetails = connectionDetails,
                                cdmDatabaseSchema = cdmDatabaseSchema,
                                cdmVersion = 5,
                                cohortDatabaseSchema = resultsDatabaseSchema,
                                cohortTable = "coxibVsNonselVsGiBleed",
                                cohortIds = 2,
                                rowIdField = "subject_id",
                                cohortTableIsTemp = FALSE,
                                covariateSettings = settings,
                                normalize = FALSE)

covarData <- getDbTemporalCovariateData(connection = connection,cdmDatabaseSchema = cdmDatabaseSchema,
                                        cohortTempTable = "results.coxibVsNonselVsGiBleed",
                                        rowIdField = "subject_id",
                                        covariateSettings = settings
)


covarData$covariates
covarData$covariateRef
covarData$timePeriods



plpData <- getPlpData(connectionDetails = connectionDetails,
                      cdmDatabaseSchema = cdmDatabaseSchema,
                      cohortDatabaseSchema = resultsDatabaseSchema,
                      cohortTable = "coxibVsNonselVsGiBleed",
                      cohortId = 1,
                      covariateSettings = settings,
                      outcomeDatabaseSchema = resultsDatabaseSchema,
                      outcomeTable = "gibleed",
                      outcomeIds = 3,
                      cdmVersion = 5)

population <- createStudyPopulation(plpData,
                                    outcomeId = 1,
                                    includeAllOutcomes = TRUE,
                                    firstExposureOnly = FALSE,
                                    washoutPeriod = 0,
                                    removeSubjectsWithPriorOutcome = FALSE,
                                    priorOutcomeLookback = 365,
                                    riskWindowStart = 1,
                                    requireTimeAtRisk = FALSE,
                                    riskWindowEnd = 365)

