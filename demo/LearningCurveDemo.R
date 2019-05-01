library(PatientLevelPrediction)

# We need to have a writable folder for the ff objects
checkffFolder()

# This demo will generate a learning curve using 8 training set sizes
# Dependent on your system it can take some time to run
# If you have multiple cores we suggest to use them

selection <- readline(prompt="Do you like to demo the parallel version (y/n):")

# Generate simulated plpData
data(plpDataSimulationProfile)
set.seed(1234)
sampleSize <- 12000
plpData <- simulatePlpData(
  plpDataSimulationProfile,
  n = sampleSize
)

# Create the study population
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

# Specify the prediction algorithm to be used
modelSettings <- setLassoLogisticRegression()

# Specify a test fraction and a sequence of training set fractions
testFraction <- 0.2
trainFractions <- seq(0.1, 0.8, 0.1)

# Specify the test split to be used
testSplit <- 'person'

# Create the learning curve object
if (selection != "y" &&
    selection != "Y") {
  learningCurve <- createLearningCurve(
    population,
    plpData = plpData,
    modelSettings = modelSettings,
    testFraction = testFraction,
    verbosity = "TRACE",
    trainFractions = trainFractions,
    splitSeed = 1000,
    saveModel = TRUE
  )
  
  # plot the learning curve by specify one of the available metrics: 
  # 'AUROC', 'AUPRC', 'sBrier'.
  plotLearningCurve(
    learningCurve,
    metric = "AUROC",
    plotTitle = "Learning Curve",
    plotSubtitle = "AUROC performance"
  )
  
} else {
  # create a learning curve object in parallel
  learningCurvePar <- createLearningCurvePar(
    population,
    plpData = plpData,
    modelSettings = modelSettings,
    testFraction = 0.2,
    trainFractions = trainFractions,
    splitSeed = 1000
  )
  
  # plot the learning curve
  plotLearningCurve(
    learningCurvePar,
    metric = "AUROC",
    plotTitle = "Learning Curve Parallel",
    plotSubtitle = "AUROC performance"
  )
}
