library(SqlRender)
library(DatabaseConnector)
library(FeatureExtraction)
library(PatientLevelPrediction)
options(fftempdir = "/Users/Shared/tempff")

# Azure 
dbms <- "sql server"
user <- "prijnbeek@emif"
pw <- "pjotter1!"
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
pw <- "pjotter1!"
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

settings <- createCovariateSettings(useCovariateDemographics = TRUE,
                                    useCovariateDemographicsGender = TRUE,
                                    useCovariateDemographicsRace = TRUE,
                                    useCovariateDemographicsEthnicity = TRUE,
                                    useCovariateDemographicsAge = TRUE,
                                    useCovariateDemographicsYear = TRUE,
                                    useCovariateDemographicsMonth = TRUE,
                                    useCovariateConditionOccurrence = TRUE,
                                    useCovariateConditionOccurrence365d = TRUE,
                                    useCovariateConditionOccurrence30d = TRUE,
                                    useCovariateConditionOccurrenceInpt180d = TRUE,
                                    useCovariateConditionEra = TRUE,
                                    useCovariateConditionEraEver = TRUE,
                                    useCovariateConditionEraOverlap = TRUE,
                                    useCovariateConditionGroup = TRUE,
                                    useCovariateConditionGroupMeddra = TRUE,
                                    useCovariateConditionGroupSnomed = TRUE,
                                    useCovariateDrugExposure = TRUE,
                                    useCovariateDrugExposure365d = TRUE,
                                    useCovariateDrugExposure30d = TRUE,
                                    useCovariateDrugEra = TRUE,
                                    useCovariateDrugEra365d = TRUE,
                                    useCovariateDrugEra30d = TRUE,
                                    useCovariateDrugEraOverlap = TRUE,
                                    useCovariateDrugEraEver = TRUE,
                                    useCovariateDrugGroup = TRUE,
                                    useCovariateProcedureOccurrence = TRUE,
                                    useCovariateProcedureOccurrence365d = TRUE,
                                    useCovariateProcedureOccurrence30d = TRUE,
                                    useCovariateProcedureGroup = TRUE,
                                    useCovariateObservation = TRUE,
                                    useCovariateObservation365d = TRUE,
                                    useCovariateObservation30d = TRUE,
                                    useCovariateObservationCount365d = TRUE,
                                    useCovariateMeasurement = TRUE,
                                    useCovariateMeasurement365d = TRUE,
                                    useCovariateMeasurement30d = TRUE,
                                    useCovariateMeasurementCount365d = TRUE,
                                    useCovariateMeasurementBelow = TRUE,
                                    useCovariateMeasurementAbove = TRUE,
                                    useCovariateConceptCounts = TRUE,
                                    useCovariateRiskScores = TRUE,
                                    useCovariateRiskScoresCharlson = TRUE,
                                    useCovariateRiskScoresDCSI = TRUE,
                                    useCovariateRiskScoresCHADS2 = TRUE,
                                    useCovariateRiskScoresCHADS2VASc = TRUE,
                                    useCovariateInteractionYear = FALSE,
                                    useCovariateInteractionMonth = FALSE,
                                    excludedCovariateConceptIds = c(),
                                    includedCovariateConceptIds = c(),
                                    deleteCovariatesSmallCount = 100)


plpData <- getPlpData(connectionDetails = connectionDetails,
                      cdmDatabaseSchema = cdmDatabaseSchema,
                      cohortDatabaseSchema = resultsDatabaseSchema,
                      cohortTable = "coxibVsNonselVsGiBleed",
                      cohortId = 1,
                      covariateSettings = settings,
                      outcomeDatabaseSchema = resultsDatabaseSchema,
                      outcomeTable = "coxibVsNonselVsGiBleed",
                      outcomeIds = 3,
                      cdmVersion = 5)

savePlpData(plpData, "~/Documents/temp/plpData")

plpData <- loadPlpData("~/Documents/temp/plpData")

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

lrModel <- setLassoLogisticRegression()

learningCurve <- createLearningCurve(population, plpData,
                                modelSettings = lrModel,
                                testSplit = 'time', testFraction=0.25, trainFractions = c(0.25,0.50,0.75), splitSeed=NULL, nfold=3, indexes=NULL,
                                verbosity=futile.logger::INFO, timeStamp=FALSE, analysisId=NULL)

# Test plot
plot(log(learningCurve$trainAUC),type = "o",col = "red", xlab = "Training set size",
     ylab = "AUC", main = "Learning Curve")
lines(log(learningCurve$testAUC), type = "o", col = "blue")
legend('topright', c("Train error", "Test error"), lty = c(1,1), lwd = c(2.5, 2.5),
       col = c("red", "blue"))