
# Download the PostreSQL driver ---------------------------
# If DATABASECONNECTOR_JAR_FOLDER exists, assume driver has been downloaded
jarFolder <- Sys.getenv("DATABASECONNECTOR_JAR_FOLDER", unset = "")
if (jarFolder == "") {
  tempJarFolder <- tempfile("jdbcDrivers")
  dir.create(tempJarFolder)
  Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = tempJarFolder)
  downloadJdbcDrivers("postgresql")
  
  withr::defer({
    unlink(tempJarFolder, recursive = TRUE, force = TRUE)
    Sys.unsetenv("DATABASECONNECTOR_JAR_FOLDER")
  }, testthat::teardown_env())
}


# this files contains the objects used in the tests:
travis <- T

saveLoc <- tempfile("saveLoc")
dir.create(saveLoc)

# configure and activate python
PatientLevelPrediction::configurePython(envname = 'r-reticulate', envtype = "conda")
PatientLevelPrediction::setPythonEnvironment(envname = 'r-reticulate', envtype = "conda")

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# simulated data Tests
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

data(plpDataSimulationProfile, envir = environment())

# PLPDATA
sampleSize <- 2500+sample(1000,1)
plpData <- simulatePlpData(plpDataSimulationProfile, n = sampleSize)
#plpData$metaData$cohortId <- plpData$metaData$cohortIds

# POPULATION
populationSettings <- createStudyPopulationSettings(
  firstExposureOnly = FALSE,
  washoutPeriod = 0,
  removeSubjectsWithPriorOutcome = FALSE,
  priorOutcomeLookback = 99999,
  requireTimeAtRisk = T,
  minTimeAtRisk=10,
  riskWindowStart = 0,
  startAnchor = 'cohort start',
  riskWindowEnd = 365,
  endAnchor = 'cohort start')


# MODEL SETTINGS
lrSet <- setLassoLogisticRegression()

# RUNPLP - LASSO LR
plpResult <- runPlp(
  plpData = plpData, 
  outcomeId = 2, 
  analysisId = 'Test', 
  analysisName = 'Testing analysis',
  populationSettings = populationSettings, 
  splitSettings = createDefaultSplitSetting(),
  preprocessSettings = createPreprocessSettings(), 
  modelSettings = lrSet, 
  logSettings = createLogSettings(verbosity = 'TRACE'),
  executeSettings = createDefaultExecuteSettings(), 
  saveDirectory = 'D:/test/plp'
  )

#


population <- createStudyPopulation(
  plpData = plpData,
  outcomeId = 2, 
  populationSettings = populationSettings
  )

createTrainData <- function(plpData, population){
  trainData <- list()
  trainData$covariateData <- Andromeda::copyAndromeda(plpData$covariateData)
  trainData$labels <- population %>% dplyr::select(.data$rowId, .data$outcomeCount, .data$survivalTime)
  trainData$folds <- data.frame(
    rowId = population$rowId,
    index = sample(3, nrow(population), replace = T)
  )
  
  attr(trainData, "metaData")$outcomeId <- 2
  attr(trainData, "metaData")$cohortId <- 1

  class(trainData$covariateData) <- 'CovariateData'
  
  return(trainData)
}


sampleSize2 <- 1000+sample(1000,1)
plpData2 <- simulatePlpData(plpDataSimulationProfile, n = sampleSize2)

population2 <- createStudyPopulation(
  plpData = plpData2,
  outcomeId = 2, 
  populationSettings = populationSettings
)

sampleSizeBig <- 10000
plpDataBig <- simulatePlpData(plpDataSimulationProfile, n = sampleSizeBig)

populationBig <- createStudyPopulation(
  plpData = plpDataBig,
  outcomeId = 2, 
  populationSettings = populationSettings
)


# MODEL SETTINGS
lrSet <- setLassoLogisticRegression()
gbmSet <- setGradientBoostingMachine(ntrees = 50, maxDepth = 3, learnRate = 0.01, seed = 1)
knnSet <- setKNN(k=100, indexFolder = file.path(saveLoc,"knn"))
rfSet2 <- setRandomForest(mtries = -1,ntrees = 10, maxDepth = 2, varImp = F, seed=1)
surv <- PatientLevelPrediction::setCoxModel(variance = 0.01, seed = 1)



# RUNPLP - LASSO LR
plpResult <- runPlp(plpData = plpData, 
  outcomeId = 2, 
  analysisId = 'Test', 
  populationSettings = populationSettings, 
  splitSettings = createDefaultSplitSetting(), 
  preprocessSettings = createPreprocessSettings(), 
  modelSettings = lrSet, 
  logSettings = createLogSettings(),
  executeSettings = createDefaultExecuteSettings(), 
  saveDirectory = 'D:/test/plp')

# RUNPLP - survival Cox
plpResult2 <- runPlp(population = population,
                    plpData = plpData, 
                    modelSettings = surv, 
                    savePlpData = F, 
                    savePlpResult = F, 
                    saveEvaluation = F, 
                    savePlpPlots = F, 
                    analysisId = 'survTest',
                    saveDirectory =  saveLoc)



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# read data Tests
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#======== TEST-APPLY =============
cdmDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
ohdsiDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_OHDSI_SCHEMA")
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "postgresql",
                                                                user = Sys.getenv("CDM5_POSTGRESQL_USER"),
                                                                password = URLdecode(Sys.getenv("CDM5_POSTGRESQL_PASSWORD")),
                                                                server = Sys.getenv("CDM5_POSTGRESQL_SERVER"))

# create the cohorts
createCohort <- F
if(createCohort){
  conn <- DatabaseConnector::connect(connectionDetails)
  sql <- SqlRender::render(sql = 'select * into  @ohdsiDatabaseSchema.cohorts from (SELECT 1 as COHORT_DEFINITION_ID, PERSON_ID as SUBJECT_ID, min(CONDITION_START_DATE) as COHORT_START_DATE, min(CONDITION_START_DATE) as COHORT_END_DATE from @cdmDatabaseSchema.condition_occurrence where CONDITION_CONCEPT_ID=320128
                           group by PERSON_ID) temp limit 1000;',
                           cdmDatabaseSchema = cdmDatabaseSchema ,
                           ohdsiDatabaseSchema = ohdsiDatabaseSchema)
  DatabaseConnector::executeSql(conn, sql)
  
  sql <- SqlRender::render(sql = 'select * into  @ohdsiDatabaseSchema.outs_test from (select 2 as COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE, COHORT_END_DATE from cohort) temp  limit 100;',
                           ohdsiDatabaseSchema = ohdsiDatabaseSchema)
  DatabaseConnector::executeSql(conn, sql)
}

covSet <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T, useDemographicsAgeGroup = T)#  useConditionOccurrenceShortTerm = T)
plpDataReal <- getPlpData(connectionDetails = connectionDetails, 
                          cdmDatabaseSchema = cdmDatabaseSchema, 
                          cohortId = 1, outcomeIds = 2, 
                          cohortDatabaseSchema = ohdsiDatabaseSchema, 
                          cohortTable = 'cohorts', 
                          outcomeDatabaseSchema = ohdsiDatabaseSchema, 
                          outcomeTable = 'outs_test', 
                          sampleSize = 1000, 
                          covariateSettings = covSet)
plpDataReal$metaData$call$cdmDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")

populationReal <- createStudyPopulation(plpDataReal,
                                        outcomeId = 2,
                                        firstExposureOnly = FALSE,
                                        washoutPeriod = 0,
                                        removeSubjectsWithPriorOutcome = FALSE,
                                        priorOutcomeLookback = 99999,
                                        requireTimeAtRisk = F,
                                        minTimeAtRisk=1,
                                        riskWindowStart = 0, 
                                        startAnchor = 'cohort start',
                                        riskWindowEnd = 1000,
                                        endAnchor = 'cohort end')

plpResultReal <- runPlp(population = populationReal,
                        plpData = plpDataReal, 
                        modelSettings = rfSet2, 
                        splitSeed = 1,
                        savePlpData = F, 
                        savePlpResult = F, 
                        saveEvaluation = F, 
                        savePlpPlots = F, 
                        analysisId = 'gbmReal',
                        saveDirectory =  saveLoc)

