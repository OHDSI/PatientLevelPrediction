library(PatientLevelPrediction)

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

### Example 1: Regularised logistic regression
lr_model <- PatientLevelPrediction::setLassoLogisticRegression()
lr_results <- PatientLevelPrediction::runPlp(population,
                                             plpData,
                                             modelSettings = lr_model,
                                             testSplit = "time",
                                             testFraction = 0.25,
                                             nfold = 2,
                                             verbosity = "INFO",
                                             saveDirectory =  "./plpmodels")

### Example 2: Naive bayes model using python
cat("Press a key to continue")
invisible(readline())

nb_model <- PatientLevelPrediction::setNaiveBayes()
nb_results <- PatientLevelPrediction::runPlp(population,
                                             plpData,
                                             modelSettings = nb_model,
                                             testSplit = "time",
                                             testFraction = 0.25,
                                             nfold = 2,
                                             verbosity = "INFO",
                                             saveDirectory =  "./plpmodels")

### Example 3: Gradient Boosting Machine with person split
cat("Press a key to continue")
invisible(readline())

gbm_model <- PatientLevelPrediction::setGradientBoostingMachine(ntrees = c(10, 50, 100),
                                                                maxDepth = c(4, 16),
                                                                minRows = 2)
gbm_results <- PatientLevelPrediction::runPlp(population,
                                              plpData,
                                              modelSettings = gbm_model,
                                              testSplit = "person",
                                              testFraction = 0.25,
                                              nfold = 2,
                                              verbosity = "INFO",
                                              saveDirectory =  "./plpmodels")

### Example 4: Viewing Results of Logistic Regression example
cat("Press a key to continue")
viewPlp(lr_results)