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
  cohortId = 1, 
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
  saveDirectory = file.path(tempdir(), 'EunomiaTest8')
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
  saveDirectory = file.path(tempdir(), 'EunomiaTest7')
)

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
  saveDirectory = file.path(tempdir(), 'EunomiaTest6')
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
  modelSettings = setMLP(), 
  logSettings = createLogSettings(), 
  executeSettings = createDefaultExecuteSettings(), 
  saveDirectory = file.path(tempdir(), 'EunomiaTest5')
)

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
  saveDirectory = file.path(tempdir(), 'EunomiaTest4')
)


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
  saveDirectory = file.path(tempdir(), 'EunomiaTest2')
)

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
  modelSettings = setDecisionTree(maxFeatures = list(50,'auto', NULL)), 
  logSettings = createLogSettings(), 
  executeSettings = createDefaultExecuteSettings(), 
  saveDirectory = file.path(tempdir(), 'EunomiaTest3')
)



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
    minRows = c(5), 
    learnRate = c(0.2) 
  ), 
  logSettings = createLogSettings(), 
  executeSettings = createDefaultExecuteSettings(), 
  saveDirectory = file.path(tempdir(), 'EunomiaTest')
)
