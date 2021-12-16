library(PatientLevelPrediction)

# This demo will generate a stacked ensemble consisting 
# of a Logistic Regression and Random Forest model.
# Dependent on your system it can take some time to run

# We first simulate some data
cat("Press a key to continue")
invisible(readline())

# Simulate plpData
data(plpDataSimulationProfile)
set.seed(1234)
sampleSize <- 2000
plpData <- simulatePlpData(
  plpDataSimulationProfile,
  n = sampleSize
)

# Generate the study population
populationSettings <- createStudyPopulationSettings(
  binary = TRUE,
  firstExposureOnly = FALSE,
  washoutPeriod = 0,
  removeSubjectsWithPriorOutcome = FALSE,
  priorOutcomeLookback = 99999,
  requireTimeAtRisk = TRUE,
  minTimeAtRisk = 0,
  riskWindowStart = 0,
  startAnchor = 'cohort start',
  riskWindowEnd = 365,
  endAnchor = 'cohort start'
)

# Let's set the models and model building parameters
cat("Press a key to continue")
invisible(readline())

# Use LASSO logistic regression and Random Forest as base predictors
model1 <- setLassoLogisticRegression()
model2 <- setRandomForest()

# Specify the spilt settings
splitSettings <- createDefaultSplitSetting(
  testFraction = 0.2, 
  nfold = 4, 
  splitSeed = 100 # this makes sure same split is done 
  )

# Specify the ensemble strategy
ensembleStrategy <- 'stacked'

# Now we build the stacked ensemble
cat("Press a key to continue")
invisible(readline())
ensembleResults <- runEnsembleModel(
  ensembleStrategy = ensembleStrategy,
  parallel = T,
  maxCores = 2,
  dataList = list(
    plpData, 
    plpData
    ), 
  outcomeIds = list(2,2),
  populationSettings = list(
    populationSettings, 
    populationSettings
    ),
  sampleSettings = list(
    createSampleSettings(),
    createSampleSettings()
  ),
  featureEngineeringSettings = list(
    createFeatureEngineeringSettings(),
    createFeatureEngineeringSettings()
  ),
  preprocessSettings = list(
    createPreprocessSettings(),
    createPreprocessSettings()
  ),
  modelList = list(
    model1, 
    model2
    ),
  splitSettings = splitSettings
) 

# You could now save the model and apply it on other data as described in more detail
# in the vignette.
