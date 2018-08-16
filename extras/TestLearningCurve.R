library(PatientLevelPrediction)
options(fftempdir = "D:/Users/hjohn/temp/tempff")

# simulated data from a database profile
set.seed(1234)
data(plpDataSimulationProfile)
sampleSize <- 12000
plpData <- simulatePlpData(plpDataSimulationProfile, n = sampleSize)

# uncomment the following two lines to save and load the data object savePlpData(plpData,
# '~/Documents/temp/plpData') plpData <- loadPlpData('~/Documents/temp/plpData')

# define a study population
population <- createStudyPopulation(plpData,
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
                                    verbosity = "INFO")

# define the prediction algorithm, for example LASSO logistic regression
modelSettings <- setLassoLogisticRegression()

# define a test fraction and a sequence of training set fractions
testFraction <- 0.2
trainFractions <- seq(0.1, 0.8, 0.1)

# Use a split by person, alterantively a time split is possible
testSplit <- "person"

# create a learning curve object
learningCurve <- createLearningCurve(population,
                                     plpData = plpData,
                                     modelSettings = modelSettings,
                                     testFraction = 0.2,
                                     verbosity = "TRACE",
                                     trainFractions = trainFractions,
                                     splitSeed = 1000,
                                     saveModel = TRUE)

# plot the learning curve by specify one of the available metrics: 'AUROC', 'AUPRC', 'sBrier'.
plotLearningCurve(learningCurve,
                  metric = "AUROC",
                  plotTitle = "Learning Curve",
                  plotSubtitle = "AUROC performance")

# create a learning curve object in parallel
learningCurvePar <- createLearningCurvePar(population,
                                           plpData = plpData,
                                           modelSettings = modelSettings,
                                           testFraction = 0.2,
                                           trainFractions = trainFractions,
                                           splitSeed = 1000)

# plot the learning curve
plotLearningCurve(learningCurvePar,
                  metric = "AUROC",
                  plotTitle = "Learning Curve Parallel",
                  plotSubtitle = "AUROC performance")
