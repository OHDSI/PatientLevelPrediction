library(PatientLevelPrediction)
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


databaseDetails <- createDatabaseDetails(
  connectionDetails = connectionDetails, 
  cdmDatabaseSchema = "main", 
  cdmDatabaseName = "main",
  cohortDatabaseSchema = "main", 
  cohortTable = "cohort", 
  outcomeDatabaseSchema = "main", 
  outcomeTable =  "cohort",
  targetId = 1, 
  outcomeIds = 3, #make this ids
  cdmVersion = 5)

restrictPlpDataSettings <- createRestrictPlpDataSettings(  
  firstExposureOnly = T, 
  washoutPeriod = 365
)

plpDataEunomia <- PatientLevelPrediction::getPlpData(
  databaseDetails = databaseDetails, 
  restrictPlpDataSettings = restrictPlpDataSettings, 
  covariateSettings = covSet
)




plpResultEunomia <- PatientLevelPrediction::runPlp(
  plpData = plpDataEunomia, 
  outcomeId = 3, 
  analysisId = 'Eunomia', 
  analysisName = 'Testing with Eunomia', 
  populationSettings = createStudyPopulationSettings(), 
  splitSettings = createDefaultSplitSetting(), 
  sampleSettings = createSampleSettings(), 
  featureEngineeringSettings = createFeatureEngineeringSettings(), 
  preprocessSettings = createPreprocessSettings(), 
  modelSettings = setLassoLogisticRegression(), 
  logSettings = createLogSettings(), 
  executeSettings = createDefaultExecuteSettings(), 
  saveDirectory = file.path(tempdir(), 'EunomiaTest')
)

plpResultEunomia9 <- PatientLevelPrediction::runPlp(
  plpData = plpDataEunomia, 
  outcomeId = 3, 
  analysisId = 'Eunomia', 
  analysisName = 'Testing with Eunomia', 
  populationSettings = createStudyPopulationSettings(), 
  splitSettings = createDefaultSplitSetting(), 
  sampleSettings = createSampleSettings(), 
  featureEngineeringSettings = createFeatureEngineeringSettings(), 
  preprocessSettings = createPreprocessSettings(), 
  modelSettings = setKNN(), 
  logSettings = createLogSettings(), 
  executeSettings = createDefaultExecuteSettings(), 
  saveDirectory = file.path(tempdir(), 'EunomiaTest9')
)

plpResultEunomia8 <- PatientLevelPrediction::runPlp(
  plpData = plpDataEunomia, 
  outcomeId = 3, 
  analysisId = 'Eunomia', 
  analysisName = 'Testing with Eunomia', 
  populationSettings = createStudyPopulationSettings(), 
  splitSettings = createDefaultSplitSetting(), 
  sampleSettings = createSampleSettings(), 
  featureEngineeringSettings = createFeatureEngineeringSettings(), 
  preprocessSettings = createPreprocessSettings(), 
  modelSettings = setSVM(), 
  logSettings = createLogSettings(), 
  executeSettings = createDefaultExecuteSettings(), 
  saveDirectory = file.path(tempdir(), 'EunomiaTest8')
)
# issue with loading json - fixed by saving as pickle

plpResultEunomia7 <- PatientLevelPrediction::runPlp(
  plpData = plpDataEunomia, 
  outcomeId = 3, 
  analysisId = 'Eunomia', 
  analysisName = 'Testing with Eunomia', 
  populationSettings = createStudyPopulationSettings(), 
  splitSettings = createDefaultSplitSetting(), 
  sampleSettings = createSampleSettings(), 
  featureEngineeringSettings = createFeatureEngineeringSettings(), 
  preprocessSettings = createPreprocessSettings(), 
  modelSettings = setRandomForest(), 
  logSettings = createLogSettings(), 
  executeSettings = createDefaultExecuteSettings(), 
  saveDirectory = file.path(tempdir(), 'EunomiaTest7')
)

plpResultEunomia6 <- PatientLevelPrediction::runPlp(
  plpData = plpDataEunomia, 
  outcomeId = 3, 
  analysisId = 'Eunomia', 
  analysisName = 'Testing with Eunomia', 
  populationSettings = createStudyPopulationSettings(), 
  splitSettings = createDefaultSplitSetting(), 
  sampleSettings = createSampleSettings(), 
  featureEngineeringSettings = createFeatureEngineeringSettings(), 
  preprocessSettings = createPreprocessSettings(), 
  modelSettings = setMLP(hiddenLayerSizes = list(c(10))), 
  logSettings = createLogSettings(), 
  executeSettings = createDefaultExecuteSettings(), 
  saveDirectory = file.path(tempdir(), 'EunomiaTest6')
)
# invalid hiddenLayerSizes can cause error 

plpResultEunomia5 <- PatientLevelPrediction::runPlp(
  plpData = plpDataEunomia, 
  outcomeId = 3, 
  analysisId = 'Eunomia', 
  analysisName = 'Testing with Eunomia', 
  populationSettings = createStudyPopulationSettings(), 
  splitSettings = createDefaultSplitSetting(), 
  sampleSettings = createSampleSettings(), 
  featureEngineeringSettings = createFeatureEngineeringSettings(), 
  preprocessSettings = createPreprocessSettings(), 
  modelSettings = setNaiveBayes(), 
  logSettings = createLogSettings(), 
  executeSettings = createDefaultExecuteSettings(), 
  saveDirectory = file.path(tempdir(), 'EunomiaTest5')
)
# worked

plpResultEunomia3 <- PatientLevelPrediction::runPlp(
  plpData = plpDataEunomia, 
  outcomeId = 3, 
  analysisId = 'Eunomia', 
  analysisName = 'Testing with Eunomia', 
  populationSettings = createStudyPopulationSettings(), 
  splitSettings = createDefaultSplitSetting(), 
  sampleSettings = createSampleSettings(), 
  featureEngineeringSettings = createFeatureEngineeringSettings(), 
  preprocessSettings = createPreprocessSettings(), 
  modelSettings = setAdaBoost(), 
  logSettings = createLogSettings(), 
  executeSettings = createDefaultExecuteSettings(), 
  saveDirectory = file.path(tempdir(), 'EunomiaTest3')
)
# worked

plpResultEunomia4 <- PatientLevelPrediction::runPlp(
  plpData = plpDataEunomia, 
  outcomeId = 3, 
  analysisId = 'Eunomia', 
  analysisName = 'Testing with Eunomia', 
  populationSettings = createStudyPopulationSettings(), 
  splitSettings = createDefaultSplitSetting(), 
  sampleSettings = createSampleSettings(), 
  featureEngineeringSettings = createFeatureEngineeringSettings(), 
  preprocessSettings = createPreprocessSettings(), 
  modelSettings = setDecisionTree(maxFeatures = list(50,'sqrt', NULL)), 
  logSettings = createLogSettings(), 
  executeSettings = createDefaultExecuteSettings(), 
  saveDirectory = file.path(tempdir(), 'EunomiaTest4')
)
# DT error!


plpResultEunomia2 <- PatientLevelPrediction::runPlp(
  plpData = plpDataEunomia, 
  outcomeId = 3, 
  analysisId = 'Eunomia', 
  analysisName = 'Testing with Eunomia', 
  populationSettings = createStudyPopulationSettings(), 
  splitSettings = createDefaultSplitSetting(), 
  sampleSettings = createSampleSettings(), 
  featureEngineeringSettings = createFeatureEngineeringSettings(), 
  preprocessSettings = createPreprocessSettings(), 
  modelSettings = setGradientBoostingMachine(
    ntrees = c(500),
    nthread = c(10),
    earlyStopRound = c(25), 
    maxDepth = c(4), 
    learnRate = c(0.2) 
  ), 
  logSettings = createLogSettings(), 
  executeSettings = createDefaultExecuteSettings(), 
  saveDirectory = file.path(tempdir(), 'EunomiaTest2')
)
# worked