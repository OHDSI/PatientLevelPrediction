# example to test arrow
library(PatientLevelPrediction)
# 

connectionDetails <- Eunomia::getEunomiaConnectionDetails()
Eunomia::createCohorts(connectionDetails)

covSet <- FeatureExtraction::createCovariateSettings(useDemographicsGender = T,
                                                     useDemographicsAge = T,
                                                     useDemographicsRace = T,
                                                     useDemographicsEthnicity = T,
                                                     useDemographicsAgeGroup = T,
                                                     useConditionGroupEraLongTerm = T,
                                                     useDrugEraStartLongTerm  = T,
                                                     endDays = -1
)

databaseDetails <- PatientLevelPrediction::createDatabaseDetails(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = "main",
  cohortDatabaseSchema = "main",
  cohortTable = "cohort",
  targetId = 4,
  outcomeIds = 3,
  outcomeDatabaseSchema = "main",
  outcomeTable =  "cohort",
  cdmDatabaseName = 'eunomia'
)

restrictPlpDataSettings <- PatientLevelPrediction::createRestrictPlpDataSettings(
  firstExposureOnly = T,
  washoutPeriod = 365
)
plpData <- PatientLevelPrediction::getPlpData(
  databaseDetails = databaseDetails,
  restrictPlpDataSettings = restrictPlpDataSettings,
  covariateSettings = covSet
)


populationSettings <- PatientLevelPrediction::createStudyPopulationSettings(
  requireTimeAtRisk = F,
  riskWindowStart = 1,
  riskWindowEnd = 365)

modelSettings <- PatientLevelPrediction::setLassoLogisticRegression()
plpResults <- PatientLevelPrediction::runPlp(plpData = plpData, 
                                             outcomeId = 3,
                                             modelSettings = modelSettings,
                                             analysisId = 'Test',
                                             analysisName = 'Testing data table',
                                             populationSettings = populationSettings,
                                             splitSettings = createDefaultSplitSetting(),
                                             sampleSettings = createSampleSettings(),  # none
                                             featureEngineeringSettings = createFeatureEngineeringSettings(), # none
                                             preprocessSettings = createPreprocessSettings(minFraction=0,
                                                                                           normalize = T,
                                                                                           removeRedundancy = F),
                                             logSettings = createLogSettings(),
                                             executeSettings = createExecuteSettings(runSplitData = T,
                                                                                     runSampleData = T,
                                                                                     runfeatureEngineering = T,
                                                                                     runPreprocessData = T,
                                                                                     runModelDevelopment = T,
                                                                                     runCovariateSummary = T
                                             ))
viewPlp(plpResults)
