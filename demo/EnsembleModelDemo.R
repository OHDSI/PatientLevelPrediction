library(PatientLevelPrediction)

# We need to have a writable folder for the ff objects
checkffFolder()

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
population <- createStudyPopulation(
  plpData,
  outcomeId = 2,
  binary = TRUE,
  firstExposureOnly = FALSE,
  washoutPeriod = 0,
  removeSubjectsWithPriorOutcome = FALSE,
  priorOutcomeLookback = 99999,
  requireTimeAtRisk = FALSE,
  minTimeAtRisk = 0,
  riskWindowStart = 0,
  addExposureDaysToStart = FALSE,
  riskWindowEnd = 365,
  addExposureDaysToEnd = FALSE,
  verbosity = "INFO"
)

# Let's set the models and model building parameters
cat("Press a key to continue")
invisible(readline())

# Use LASSO logistic regression and Random Forest as base predictors
model1 <- setLassoLogisticRegression()
model2 <- setRandomForest()

# Specify a test fraction and a sequence of training set fractions
testFraction <- 0.2

# Specify the ensemble strategy
ensembleStrategy <- 'stacked'

# Specify the test split to be used
testSplit <- 'person'

# Now we build the stacked ensemble
cat("Press a key to continue")
invisible(readline())
ensembleResults <- PatientLevelPrediction::runEnsembleModel(population, 
                                                          dataList = list(plpData, plpData), 
                                                          modelList = list(model1, model2),
                                                          testSplit=testSplit,
                                                          testFraction=testFraction,
                                                          nfold=3, splitSeed=1000, 
                                                          ensembleStrategy = ensembleStrategy) 

# You could now save the model and apply it on other data as described in more detail
# in the vignette.
