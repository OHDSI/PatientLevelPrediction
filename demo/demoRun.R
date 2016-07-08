

### Simulated data from a database profile
set.seed(1234)
data(plpDataSimulationProfile)
sampleSize <- 2000
plpData <- PatientLevelPrediction::simulatePlpData(plpDataSimulationProfile, n = sampleSize)

### Define the study population 
population <- createStudyPopulation(plpData,
                                         outcomeId = 2,
                                         binary = TRUE,
                                         firstExposureOnly = FALSE,
                                         washoutPeriod = 0,
                                         removeSubjectsWithPriorOutcome = FALSE,
                                         priorOutcomeLookback = 99999,
                                         requireTimeAtRisk = FALSE,
                                         minTimeAtRisk=0,
                                         riskWindowStart = 0,
                                         addExposureDaysToStart = FALSE,
                                         riskWindowEnd = 365,
                                         addExposureDaysToEnd = FALSE,
                                         silent=F)

# simple regularised logistic regression:
lr_model <- PatientLevelPrediction::lassoLogisticRegression.set()
lr_results <- PatientLevelPrediction::RunPlp(population, plpData, 
                                                   modelSettings = lr_model,
                                                   testSplit='time',
                                                   testFraction=0.25,
                                                   nfold=2)

# To Do Add other results and plots

