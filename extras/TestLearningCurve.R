library(SqlRender)
library(DatabaseConnector)
library(FeatureExtraction)
library(PatientLevelPrediction)
options(fftempdir = "C:/Users/prijnbee/tempff")

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

cdmDatabaseSchema <- "dbo"
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
                      outcomeIds = 2,
                      cdmVersion = 5)

savePlpData(plpData, "~/Documents/temp/plpData")

plpData <- loadPlpData("h:/largeScalePredictionRun/data")
plpData <- loadPlpData("~/Documents/temp/plpData")

population <- createStudyPopulation(plpData,
                                    outcomeId = 2559,
                                    includeAllOutcomes = TRUE,
                                    firstExposureOnly = FALSE,
                                    washoutPeriod = 0,
                                    removeSubjectsWithPriorOutcome = FALSE,
                                    priorOutcomeLookback = 365,
                                    riskWindowStart = 1,
                                    requireTimeAtRisk = FALSE,
                                    riskWindowEnd = 365)

lrModel <- setLassoLogisticRegression()

learningCurve <- PatientLevelPrediction::createLearningCurve(population, plpData,
                                modelSettings = lrModel,
                                testSplit = 'person', testFraction=0.30, trainFractions = c(0.10,0.20,0.30,0.40,0.50,0.60,0.70), splitSeed=NULL, nfold=3, indexes=NULL,
                                verbosity=futile.logger::INFO, timeStamp=FALSE, analysisId=NULL)

# Plot Learning Curve 
ggplot(learningCurve, aes(x)) +
    geom_line(aes(y=trainError),
              colour="red") +
    geom_line(aes(y=testError),
              colour="green")+
    xlab("Training Size") +
    ylab("Error")


# Test
learningCurve <- data.frame(x = numeric(3),
                            trainError = integer(3),
                            testError = integer(3))
learningCurve$x[0] = 0.10
learningCurve$x[1] = 0.25
learningCurve$trainError[0] = 0.7
learningCurve$trainError[1] = 0.8
learningCurve$testError[0] = 0.5
learningCurve$testError[1] = 0.6
