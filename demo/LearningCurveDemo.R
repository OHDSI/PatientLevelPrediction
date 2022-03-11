library(PatientLevelPrediction)

# This demo will generate a learning curve using 8 training set sizes
# Dependent on your system it can take some time to run
# If you have multiple cores we suggest to use them

selection <- readline(prompt="Would you like to demo the parallel version (y/n):")

# Generate simulated plpData
data(plpDataSimulationProfile)
set.seed(1234)
sampleSize <- 12000
plpData <- simulatePlpData(
  plpDataSimulationProfile,
  n = sampleSize
)

# Create the study population
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

# Specify the prediction algorithm to be used
modelSettings <- setLassoLogisticRegression()

# Specify a test fraction and a sequence of training set fractions
splitSettings <- createDefaultSplitSetting(
  testFraction = 0.2,
  type = 'stratified'
  )
trainEvents <- seq(100, 800, 100)


# Create the learning curve object
if (selection != "y" &&
    selection != "Y") {
  learningCurve <- createLearningCurve(
    plpData = plpData, 
    outcomeId = 2, 
    analysisId = 'learningCurveDemo', 
    parallel = F,
    cores = 4, 
    modelSettings = modelSettings, 
    populationSettings = populationSettings, 
    splitSettings = splitSettings, 
    trainEvents = trainEvents,
    saveDirectory = './learningCurve'
)
  
} else {
  # create a learning curve object in parallel
  learningCurve <- createLearningCurve(
    plpData = plpData, 
    outcomeId = 2, 
    analysisId = 'learningCurveDemo', 
    parallel = T,
    cores = 4, 
    modelSettings = modelSettings, 
    populationSettings = populationSettings, 
    splitSettings = splitSettings, 
    trainEvents = trainEvents,
    saveDirectory = './learningCurve'
  )

}

# plot the learning curve
plotLearningCurve(
  learningCurve,
  metric = "AUROC",
  abscissa = "events",
  plotTitle = "Learning Curve Parallel",
  plotSubtitle = "AUROC performance"
)
