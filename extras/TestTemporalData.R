library(SqlRender)
library(DatabaseConnector)
library(FeatureExtraction)
library(PatientLevelPrediction)
options(fftempdir = "/Users/Shared/tempff")
options(fftempdir = "~/tmp/tempff")

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
server <- "Res-Srv-Lin-01/SYNPUF"
port <- 5432
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)

connection <- DatabaseConnector::connect(connectionDetails)



# Postgres Server
dbms <- "postgresql"
user <- "postgres"
pw <- "secret"
server <- "Res-Srv-Lin-01/IPCI-EEYORE_20170309"
port <- 5432
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)

connection <- DatabaseConnector::connect(connectionDetails)

cdmDatabaseSchema <- "cdm"
workDatabaseSchema <- "xpan"
resultsDatabaseSchema <-"xpan"
#coxibVsNonselVsHypertensive.sql, coxibVsNonselVsGiBleed.sql


sql <- loadRenderTranslateSql("coxibVsNonselVsHypertensive.sql",
                              packageName = "CohortMethod",
                              dbms = dbms,
                              cdmDatabaseSchema = cdmDatabaseSchema,
                              resultsDatabaseSchema = resultsDatabaseSchema)
DatabaseConnector::executeSql(connection, sql)

# Check number of subjects per cohort:
sql <- "SELECT cohort_definition_id, COUNT(*) AS count FROM xpan.coxibVsNonselVshypertension GROUP BY cohort_definition_id"
#sql <- SqlRender::renderSql(sql, resultsDatabaseSchema = resultsDatabaseSchema)$sql
sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
DatabaseConnector::querySql(connection, sql)

settings <- FeatureExtraction::createCovariateSettings( useCovariateDemographics = TRUE, useCovariateMeasurement = TRUE, excludedCovariateConceptIds = c())

#the below for hypertensive disorder
settings <- FeatureExtraction::createCovariateSettings( useCovariateDemographics = TRUE,
                                                           useCovariateDemographicsGender = T,
                                                           useCovariateDemographicsAge = T,
                                                           useCovariateDemographicsMonth = T,
                                                           useCovariateConditionEra = T,
                                                           useCovariateConditionEraEver = T,
                                                           useCovariateDrugEra = T,
                                                           useCovariateDrugEraEver = T,
                                                           useCovariateProcedureOccurrence = T,
                                                           useCovariateProcedureOccurrenceLongTerm = T,
                                                           useCovariateMeasurement = T,
                                                           useCovariateMeasurementLongTerm = T,
                                                           useCovariateObservation = T,
                                                           useCovariateObservationLongTerm = T,
                                                           deleteCovariatesSmallCount = 50)

settings <- FeatureExtraction::createCovariateSettings( useCovariateDemographics = TRUE,
                                                        useCovariateDemographicsGender = T,
                                                        useCovariateConditionEra = T,
                                                        useCovariateConditionEraEver = T,
                                                        useCovariateDrugEra = T,
                                                        useCovariateDrugEraEver = T,
                                                        useCovariateProcedureOccurrence = T,
                                                        useCovariateMeasurement = T,
                                                        useCovariateObservation = T,
                                                        deleteCovariatesSmallCount = 50)

settings<- FeatureExtraction::createCovariateSettings(useCovariateDemographics = TRUE,
                                                      useCovariateDemographicsGender = TRUE,
                                                      useCovariateDemographicsRace = TRUE,
                                                      useCovariateDemographicsEthnicity = TRUE,
                                                      useCovariateDemographicsAge = TRUE, useCovariateDemographicsYear = TRUE,
                                                      useCovariateDemographicsMonth = TRUE,
                                                      useCovariateConditionOccurrence = TRUE,
                                                      useCovariateConditionOccurrence365d = TRUE,
                                                      useCovariateConditionOccurrence30d = TRUE,
                                                      useCovariateConditionOccurrenceInpt180d = TRUE,
                                                      useCovariateConditionEra = TRUE, useCovariateConditionEraEver = TRUE,
                                                      useCovariateConditionEraOverlap = TRUE,
                                                      useCovariateConditionGroup = TRUE,
                                                      useCovariateConditionGroupMeddra = TRUE,
                                                      useCovariateConditionGroupSnomed = TRUE,
                                                      useCovariateDrugExposure = TRUE, useCovariateDrugExposure365d = TRUE,
                                                      useCovariateDrugExposure30d = TRUE, useCovariateDrugEra = TRUE,
                                                      useCovariateDrugEra365d = TRUE, useCovariateDrugEra30d = TRUE,
                                                      useCovariateDrugEraOverlap = TRUE, useCovariateDrugEraEver = TRUE,
                                                      useCovariateDrugGroup = TRUE, useCovariateProcedureOccurrence = TRUE,
                                                      useCovariateProcedureOccurrence365d = TRUE,
                                                      useCovariateProcedureOccurrence30d = TRUE,
                                                      useCovariateProcedureGroup = TRUE, useCovariateObservation = TRUE,
                                                      useCovariateObservation365d = TRUE, useCovariateObservation30d = TRUE,
                                                      useCovariateObservationCount365d = TRUE, useCovariateMeasurement = TRUE,
                                                      useCovariateMeasurement365d = TRUE, useCovariateMeasurement30d = TRUE,
                                                      useCovariateMeasurementCount365d = TRUE,
                                                      useCovariateMeasurementBelow = TRUE,
                                                      useCovariateMeasurementAbove = TRUE, useCovariateConceptCounts = TRUE,
                                                      useCovariateRiskScores = TRUE, useCovariateRiskScoresCharlson = TRUE,
                                                      useCovariateRiskScoresDCSI = TRUE, useCovariateRiskScoresCHADS2 = TRUE,
                                                      useCovariateRiskScoresCHADS2VASc = TRUE,
                                                      useCovariateInteractionYear = TRUE, useCovariateInteractionMonth = TRUE,
                                                      excludedCovariateConceptIds = c(), includedCovariateConceptIds = c(),
                                                      deleteCovariatesSmallCount = 50)

settings <- createTemporalCovariateSettings(useCovariateConditionEraStart = FALSE,
                                            useCovariateConditionEraPresent = FALSE,
                                            useCovariateConditionGroup = FALSE,
                                            useCovariateConditionGroupMeddra = FALSE,
                                            useCovariateConditionGroupSnomed = FALSE,
                                            useCovariateDrugEraStart = FALSE,
                                            useCovariateDrugEraPresent = TRUE,
                                            useCovariateMeasurementValue = TRUE,
                                            useCovariateMeasurementAbove = FALSE,
                                            useCovariateMeasurementBelow = FALSE,
                                            useCovariateProcedureOccurence = FALSE,
                                            useCovariateProcedureGroup = FALSE,
                                            useCovariateObservationOccurence = FALSE,
                                            useCovariateVisitOccurence = FALSE,
                                            useCovariateConceptCounts = FALSE,
                                            startDays = seq(from = -365, to = -1, by = 12), 
                                            endDays = c(seq(from = -353, to = 0, by = 12), 0))

covarData <- getDbCovariateData(connectionDetails = connectionDetails,
                                cdmDatabaseSchema = cdmDatabaseSchema,
                                cdmVersion = 5,
                                cohortDatabaseSchema = resultsDatabaseSchema,
                                cohortTable = "coxibVsNonselVshypertension",
                                cohortIds = 1,
                                rowIdField = "subject_id",
                                cohortTableIsTemp = FALSE,
                                covariateSettings = settings,
                                normalize = FALSE)

covarData <- getDbTemporalCovariateData(connection = connection,cdmDatabaseSchema = cdmDatabaseSchema,
                                        cohortTempTable = "coxibVsNonselVshypertension",
                                        rowIdField = "subject_id",
                                        covariateSettings = settings
)


covarData$covariates
covarData$covariateRef
covarData$timePeriods


#coxibVsNonselVshypertension, coxibVsNonselVsGiBleed
plpData <- getPlpData(connectionDetails = connectionDetails,
                      cdmDatabaseSchema = cdmDatabaseSchema,
                      cohortDatabaseSchema = resultsDatabaseSchema,
                      cohortTable = "coxibVsNonselVshypertension",
                      cohortId = 1,
                      covariateSettings = settings,
                      outcomeDatabaseSchema = resultsDatabaseSchema,
                      outcomeTable = "coxibVsNonselVshypertension",
                      outcomeIds = 3,
                      cdmVersion = 5,
                      temporal = T)

plpData <- getPlpData(connectionDetails = connectionDetails,
                      cdmDatabaseSchema = cdmDatabaseSchema,
                      cohortDatabaseSchema = resultsDatabaseSchema,
                      cohortTable = "coxibVsNonselVshypertension",
                      cohortId = 1,
                      covariateSettings = settings,
                      outcomeDatabaseSchema = resultsDatabaseSchema,
                      outcomeTable = "coxibVsNonselVshypertension",
                      outcomeIds = 3,
                      cdmVersion = 5,
                      temporal = FALSE)

plpData = loadPlpData('/data/share/plp/plpData.rds')
population <- createStudyPopulation(plpData,
                                    outcomeId = 3,
                                    includeAllOutcomes = TRUE,
                                    firstExposureOnly = FALSE,
                                    washoutPeriod = 0,
                                    removeSubjectsWithPriorOutcome = FALSE,
                                    priorOutcomeLookback = 365,
                                    riskWindowStart = 1,
                                    requireTimeAtRisk = FALSE,
                                    riskWindowEnd = 365)

mlp_model <- PatientLevelPrediction::setMLPTorch()
mlp_model <- PatientLevelPrediction::setLassoLogisticRegression()
mlp_model <- PatientLevelPrediction::setCNNTorch()
mlp_results <- PatientLevelPrediction::RunPlp(population, plpData, 
                                              modelSettings = mlp_model,
                                              testSplit='person',
                                              testFraction=0.20,
                                              nfold=5) 
