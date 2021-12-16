# This demo will run a logistic regression model on simulated data and will show the Shiny App
library(PatientLevelPrediction)
devAskNewPage(ask = FALSE)

### Simulated data from a database profile
set.seed(1234)
data(plpDataSimulationProfile)
sampleSize <- 2000
plpData <- simulatePlpData(plpDataSimulationProfile, n = sampleSize)

### Define the study population
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

### Regularised logistic regression
lr_model <- setLassoLogisticRegression()
lr_results <- runPlp( 
  plpData = plpData, 
  outcomeId = 2,
  analysisId = 'demo', 
  analysisName = 'run plp demo', 
  populationSettings = populationSettings, 
  splitSettings = createDefaultSplitSetting(
    type = "time",
    testFraction = 0.25,
    nfold = 2
  ), 
  sampleSettings = createSampleSettings(),
  preprocessSettings = createPreprocessSettings(
    minFraction = 0, 
    normalize = T
  ), 
  modelSettings = lr_model, 
  executeSettings = createDefaultExecuteSettings(), 
  saveDirectory = "./plpdemo"
  )


### Have a look at the results object.

### You can start the Shiny App by using this command now:
### viewPlp(lr_results)
