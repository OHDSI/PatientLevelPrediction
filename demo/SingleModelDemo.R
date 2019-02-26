# This demo will run a logistic regression model on simulated data and will show the Shiny App
library(PatientLevelPrediction)
devAskNewPage(ask = FALSE)

# We need to have a writable folder for the ff objects
checkffFolder()

### Simulated data from a database profile
set.seed(1234)
data(plpDataSimulationProfile)
sampleSize <- 2000
plpData <- PatientLevelPrediction::simulatePlpData(plpDataSimulationProfile, n = sampleSize)

### Define the study population
population <- PatientLevelPrediction::createStudyPopulation(plpData,
                                                            outcomeId = 2,
                                                            binary = TRUE,
                                                            firstExposureOnly = FALSE,
                                                            washoutPeriod = 0,
                                                            removeSubjectsWithPriorOutcome = FALSE,
                                                            priorOutcomeLookback = 99999,
                                                            requireTimeAtRisk = TRUE,
                                                            minTimeAtRisk = 0,
                                                            riskWindowStart = 0,
                                                            addExposureDaysToStart = FALSE,
                                                            riskWindowEnd = 365,
                                                            addExposureDaysToEnd = FALSE,
                                                            verbosity = "INFO")

### Regularised logistic regression
lr_model <- PatientLevelPrediction::setLassoLogisticRegression()
lr_results <- PatientLevelPrediction::runPlp(population,
                                             plpData,
                                             modelSettings = lr_model,
                                             testSplit = "time",
                                             testFraction = 0.25,
                                             nfold = 2,
                                             verbosity = "INFO",
                                             savePlpPlots = F,
                                             saveDirectory = "./plpmodels")


### Have a look at the results object.

### You can start the Shiny App by using this command now:
### viewPlp(lr_results)
