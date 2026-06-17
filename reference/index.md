# Package index

## Extracting data from the OMOP CDM database

Functions for getting the necessary data from the database in Common
Data Model and saving/loading.

- [`createDatabaseDetails()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createDatabaseDetails.md)
  : Create a setting that holds the details about the cdmDatabase
  connection for data extraction
- [`createRestrictPlpDataSettings()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createRestrictPlpDataSettings.md)
  : createRestrictPlpDataSettings define extra restriction settings when
  calling getPlpData
- [`getPlpData()`](https://ohdsi.github.io/PatientLevelPrediction/reference/getPlpData.md)
  : Extract the patient level prediction data from the server
- [`getEunomiaPlpData()`](https://ohdsi.github.io/PatientLevelPrediction/reference/getEunomiaPlpData.md)
  : Create a plpData object from the Eunomia database'
- [`savePlpData()`](https://ohdsi.github.io/PatientLevelPrediction/reference/savePlpData.md)
  : Save the plpData to folder
- [`loadPlpData()`](https://ohdsi.github.io/PatientLevelPrediction/reference/loadPlpData.md)
  : Load the plpData from a folder
- [`getCohortCovariateData()`](https://ohdsi.github.io/PatientLevelPrediction/reference/getCohortCovariateData.md)
  : Extracts covariates based on cohorts
- [`print(`*`<plpData>`*`)`](https://ohdsi.github.io/PatientLevelPrediction/reference/print.plpData.md)
  : Print a plpData object
- [`print(`*`<summary.plpData>`*`)`](https://ohdsi.github.io/PatientLevelPrediction/reference/print.summary.plpData.md)
  : Print a summary.plpData object
- [`summary(`*`<plpData>`*`)`](https://ohdsi.github.io/PatientLevelPrediction/reference/summary.plpData.md)
  : Summarize a plpData object

## Settings for designing a prediction models

Design settings required when developing a model.

- [`createStudyPopulationSettings()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createStudyPopulationSettings.md)
  : create the study population settings

- [`createDefaultSplitSetting()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createDefaultSplitSetting.md)
  : Create the settings for defining how the plpData are split into
  test/validation/train sets using default splitting functions (either
  random stratified by outcome, time or subject splitting)

- [`createExistingSplitSettings()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createExistingSplitSettings.md)
  : Create the settings for defining how the plpData are split into
  test/validation/train sets using an existing split - good to use for
  reproducing results from a different run

- [`createSampleSettings()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createSampleSettings.md)
  :

  Create the settings for defining how the trainData from `splitData`
  are sampled using default sample functions.

- [`createFeatureEngineeringSettings()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createFeatureEngineeringSettings.md)
  : Create the settings for defining any feature engineering that will
  be done

- [`createPreprocessSettings()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createPreprocessSettings.md)
  : Create the settings for preprocessing the trainData.

- [`createHyperparameterSettings()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createHyperparameterSettings.md)
  : Create Hyperparameter Settings

- [`createTuningMetric()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createTuningMetric.md)
  : Create a tuning metric descriptor

## Optional design settings

Settings for optional steps that can be used in the PLP pipeline

- [`createCohortCovariateSettings()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createCohortCovariateSettings.md)
  : Extracts covariates based on cohorts
- [`createRandomForestFeatureSelection()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createRandomForestFeatureSelection.md)
  : Create the settings for random foreat based feature selection
- [`createUnivariateFeatureSelection()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createUnivariateFeatureSelection.md)
  : Create the settings for defining any feature selection that will be
  done
- [`createSplineSettings()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createSplineSettings.md)
  : Create the settings for adding a spline for continuous variables
- [`createStratifiedImputationSettings()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createStratifiedImputationSettings.md)
  : Create the settings for using stratified imputation.
- [`createNormalizer()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createNormalizer.md)
  : Create the settings for normalizing the data @param type The type of
  normalization to use, either "minmax" or "robust"
- [`createSimpleImputer()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createSimpleImputer.md)
  : Create Simple Imputer settings
- [`createIterativeImputer()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createIterativeImputer.md)
  : Create Iterative Imputer settings
- [`createSklearnIterativeImputer()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createSklearnIterativeImputer.md)
  : Create scikit-learn Iterative Imputer settings
- [`createRareFeatureRemover()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createRareFeatureRemover.md)
  : Create the settings for removing rare features

## External validation

- [`createValidationDesign()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createValidationDesign.md)
  : createValidationDesign - Define the validation design for external
  validation
- [`validateExternal()`](https://ohdsi.github.io/PatientLevelPrediction/reference/validateExternal.md)
  : validateExternal - Validate model performance on new data
- [`createValidationSettings()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createValidationSettings.md)
  : createValidationSettings define optional settings for performing
  external validation
- [`recalibratePlp()`](https://ohdsi.github.io/PatientLevelPrediction/reference/recalibratePlp.md)
  : recalibratePlp
- [`recalibratePlpRefit()`](https://ohdsi.github.io/PatientLevelPrediction/reference/recalibratePlpRefit.md)
  : recalibratePlpRefit

## Execution settings when developing a model

Execution settings required when developing a model.

- [`createLogSettings()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createLogSettings.md)
  : Create the settings for logging the progression of the analysis
- [`createExecuteSettings()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createExecuteSettings.md)
  : Creates list of settings specifying what parts of runPlp to execute
- [`createDefaultExecuteSettings()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createDefaultExecuteSettings.md)
  : Creates default list of settings specifying what parts of runPlp to
  execute

## Binary Classification Models

Functions for creating binary models

- [`setAdaBoost()`](https://ohdsi.github.io/PatientLevelPrediction/reference/setAdaBoost.md)
  : Create setting for AdaBoost with python DecisionTreeClassifier base
  estimator

- [`setDecisionTree()`](https://ohdsi.github.io/PatientLevelPrediction/reference/setDecisionTree.md)
  : Create setting for the scikit-learn DecisionTree with python

- [`setGradientBoostingMachine()`](https://ohdsi.github.io/PatientLevelPrediction/reference/setGradientBoostingMachine.md)
  : Create setting for gradient boosting machine model using gbm_xgboost
  implementation

- [`setLassoLogisticRegression()`](https://ohdsi.github.io/PatientLevelPrediction/reference/setLassoLogisticRegression.md)
  : Create modelSettings for lasso logistic regression

- [`setRidgeRegression()`](https://ohdsi.github.io/PatientLevelPrediction/reference/setRidgeRegression.md)
  : Create modelSettings for ridge logistic regression

- [`setMLP()`](https://ohdsi.github.io/PatientLevelPrediction/reference/setMLP.md)
  :

  Create setting for neural network model with python's scikit-learn.
  For bigger models, consider using `DeepPatientLevelPrediction`
  package.

- [`setNaiveBayes()`](https://ohdsi.github.io/PatientLevelPrediction/reference/setNaiveBayes.md)
  : Create setting for naive bayes model with python

- [`setRandomForest()`](https://ohdsi.github.io/PatientLevelPrediction/reference/setRandomForest.md)
  : Create setting for random forest model using sklearn

- [`setSVM()`](https://ohdsi.github.io/PatientLevelPrediction/reference/setSVM.md)
  : Create setting for the python sklearn SVM (SVC function)

- [`setIterativeHardThresholding()`](https://ohdsi.github.io/PatientLevelPrediction/reference/setIterativeHardThresholding.md)
  : Create setting for Iterative Hard Thresholding model

- [`setLightGBM()`](https://ohdsi.github.io/PatientLevelPrediction/reference/setLightGBM.md)
  : Create setting for gradient boosting machine model using lightGBM
  (https://github.com/microsoft/LightGBM/tree/master/R-package).

## Survival Models

Functions for creating survival models

- [`setCoxModel()`](https://ohdsi.github.io/PatientLevelPrediction/reference/setCoxModel.md)
  : Create setting for lasso Cox model

## Single Patient-Level Prediction Model

Functions for training/evaluating/applying a single
patient-level-prediction model

- [`runPlp()`](https://ohdsi.github.io/PatientLevelPrediction/reference/runPlp.md)
  : runPlp - Develop and internally evaluate a model using specified
  settings
- [`externalValidateDbPlp()`](https://ohdsi.github.io/PatientLevelPrediction/reference/externalValidateDbPlp.md)
  : externalValidateDbPlp - Validate a model on new databases
- [`savePlpModel()`](https://ohdsi.github.io/PatientLevelPrediction/reference/savePlpModel.md)
  : Saves the plp model
- [`loadPlpModel()`](https://ohdsi.github.io/PatientLevelPrediction/reference/loadPlpModel.md)
  : loads the plp model
- [`savePlpResult()`](https://ohdsi.github.io/PatientLevelPrediction/reference/savePlpResult.md)
  : Saves the result from runPlp into the location directory
- [`loadPlpResult()`](https://ohdsi.github.io/PatientLevelPrediction/reference/loadPlpResult.md)
  : Loads the evalaution dataframe
- [`diagnosePlp()`](https://ohdsi.github.io/PatientLevelPrediction/reference/diagnosePlp.md)
  : diagnostic - Investigates the prediction problem settings - use
  before training a model

## Multiple Patient-Level Prediction Models

Functions for training multiple patient-level-prediction model in an
efficient way.

- [`createModelDesign()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createModelDesign.md)
  : Specify settings for developing a single model
- [`runMultiplePlp()`](https://ohdsi.github.io/PatientLevelPrediction/reference/runMultiplePlp.md)
  : Run a list of predictions analyses
- [`validateMultiplePlp()`](https://ohdsi.github.io/PatientLevelPrediction/reference/validateMultiplePlp.md)
  : externally validate the multiple plp models across new datasets
- [`savePlpAnalysesJson()`](https://ohdsi.github.io/PatientLevelPrediction/reference/savePlpAnalysesJson.md)
  : Save the modelDesignList to a json file
- [`loadPlpAnalysesJson()`](https://ohdsi.github.io/PatientLevelPrediction/reference/loadPlpAnalysesJson.md)
  : Load the multiple prediction json settings from a file
- [`diagnoseMultiplePlp()`](https://ohdsi.github.io/PatientLevelPrediction/reference/diagnoseMultiplePlp.md)
  : Run a list of predictions diagnoses

## Individual pipeline functions

Functions for running parts of the PLP workflow

- [`createStudyPopulation()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createStudyPopulation.md)
  : Create a study population

- [`splitData()`](https://ohdsi.github.io/PatientLevelPrediction/reference/splitData.md)
  :

  Split the plpData into test/train sets using a splitting settings of
  class `splitSettings`

- [`preprocessData()`](https://ohdsi.github.io/PatientLevelPrediction/reference/preprocessData.md)
  : A function that wraps around FeatureExtraction::tidyCovariateData to
  normalise the data and remove rare or redundant features

- [`fitPlp()`](https://ohdsi.github.io/PatientLevelPrediction/reference/fitPlp.md)
  : fitPlp

- [`predictPlp()`](https://ohdsi.github.io/PatientLevelPrediction/reference/predictPlp.md)
  : predictPlp

- [`evaluatePlp()`](https://ohdsi.github.io/PatientLevelPrediction/reference/evaluatePlp.md)
  : evaluatePlp

- [`covariateSummary()`](https://ohdsi.github.io/PatientLevelPrediction/reference/covariateSummary.md)
  : covariateSummary

## Saving results into database

Functions for saving the prediction model and performances into a
database.

- [`insertResultsToSqlite()`](https://ohdsi.github.io/PatientLevelPrediction/reference/insertResultsToSqlite.md)
  : Create sqlite database with the results
- [`createPlpResultTables()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createPlpResultTables.md)
  : Create the results tables to store PatientLevelPrediction models and
  results into a database
- [`createDatabaseSchemaSettings()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createDatabaseSchemaSettings.md)
  : Create the PatientLevelPrediction database result schema settings
- [`extractDatabaseToCsv()`](https://ohdsi.github.io/PatientLevelPrediction/reference/extractDatabaseToCsv.md)
  : Exports all the results from a database into csv files
- [`insertCsvToDatabase()`](https://ohdsi.github.io/PatientLevelPrediction/reference/insertCsvToDatabase.md)
  : Function to insert results into a database from csvs
- [`migrateDataModel()`](https://ohdsi.github.io/PatientLevelPrediction/reference/migrateDataModel.md)
  : Migrate Data model

## Shiny Viewers

Functions for viewing results via a shiny app

- [`viewPlp()`](https://ohdsi.github.io/PatientLevelPrediction/reference/viewPlp.md)
  : viewPlp - Interactively view the performance and model settings
- [`viewMultiplePlp()`](https://ohdsi.github.io/PatientLevelPrediction/reference/viewMultiplePlp.md)
  : open a local shiny app for viewing the result of a multiple PLP
  analyses
- [`viewDatabaseResultPlp()`](https://ohdsi.github.io/PatientLevelPrediction/reference/viewDatabaseResultPlp.md)
  : open a local shiny app for viewing the result of a PLP analyses from
  a database

## Plotting

Functions for various performance plots

- [`plotPlp()`](https://ohdsi.github.io/PatientLevelPrediction/reference/plotPlp.md)
  : Plot all the PatientLevelPrediction plots
- [`plotSparseRoc()`](https://ohdsi.github.io/PatientLevelPrediction/reference/plotSparseRoc.md)
  : Plot the ROC curve using the sparse thresholdSummary data frame
- [`plotSmoothCalibration()`](https://ohdsi.github.io/PatientLevelPrediction/reference/plotSmoothCalibration.md)
  : Plot the smooth calibration as detailed in Calster et al. "A
  calibration heirarchy for risk models was defined: from utopia to
  empirical data" (2016)
- [`plotSparseCalibration()`](https://ohdsi.github.io/PatientLevelPrediction/reference/plotSparseCalibration.md)
  : Plot the calibration
- [`plotSparseCalibration2()`](https://ohdsi.github.io/PatientLevelPrediction/reference/plotSparseCalibration2.md)
  : Plot the conventional calibration
- [`plotNetBenefit()`](https://ohdsi.github.io/PatientLevelPrediction/reference/plotNetBenefit.md)
  : Plot the net benefit
- [`plotDemographicSummary()`](https://ohdsi.github.io/PatientLevelPrediction/reference/plotDemographicSummary.md)
  : Plot the Observed vs. expected incidence, by age and gender
- [`plotF1Measure()`](https://ohdsi.github.io/PatientLevelPrediction/reference/plotF1Measure.md)
  : Plot the F1 measure efficiency frontier using the sparse
  thresholdSummary data frame
- [`plotGeneralizability()`](https://ohdsi.github.io/PatientLevelPrediction/reference/plotGeneralizability.md)
  : Plot the train/test generalizability diagnostic
- [`plotPrecisionRecall()`](https://ohdsi.github.io/PatientLevelPrediction/reference/plotPrecisionRecall.md)
  : Plot the precision-recall curve using the sparse thresholdSummary
  data frame
- [`plotPredictedPDF()`](https://ohdsi.github.io/PatientLevelPrediction/reference/plotPredictedPDF.md)
  : Plot the Predicted probability density function, showing prediction
  overlap between true and false cases
- [`plotPreferencePDF()`](https://ohdsi.github.io/PatientLevelPrediction/reference/plotPreferencePDF.md)
  : Plot the preference score probability density function, showing
  prediction overlap between true and false cases \#'
- [`plotPredictionDistribution()`](https://ohdsi.github.io/PatientLevelPrediction/reference/plotPredictionDistribution.md)
  : Plot the side-by-side boxplots of prediction distribution, by class
- [`plotVariableScatterplot()`](https://ohdsi.github.io/PatientLevelPrediction/reference/plotVariableScatterplot.md)
  : Plot the variable importance scatterplot
- [`outcomeSurvivalPlot()`](https://ohdsi.github.io/PatientLevelPrediction/reference/outcomeSurvivalPlot.md)
  : Plot the outcome incidence over time

## Learning Curves

Functions for creating and plotting learning curves

- [`createLearningCurve()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createLearningCurve.md)
  : createLearningCurve
- [`plotLearningCurve()`](https://ohdsi.github.io/PatientLevelPrediction/reference/plotLearningCurve.md)
  : plotLearningCurve

## Simulation

Functions for simulating PLP data objects.

- [`simulatePlpData()`](https://ohdsi.github.io/PatientLevelPrediction/reference/simulatePlpData.md)
  : Generate simulated data
- [`simulatePlpBenchmarkData()`](https://ohdsi.github.io/PatientLevelPrediction/reference/simulatePlpBenchmarkData.md)
  : Generate benchmark PLP data with known outcome risk
- [`simulationProfile`](https://ohdsi.github.io/PatientLevelPrediction/reference/simulationProfile.md)
  : A simulation profile for generating synthetic patient level
  prediction data

## Data manipulation functions

Functions for manipulating data

- [`toSparseM()`](https://ohdsi.github.io/PatientLevelPrediction/reference/toSparseM.md)
  : Convert the plpData in COO format into a sparse R matrix
- [`MapIds()`](https://ohdsi.github.io/PatientLevelPrediction/reference/MapIds.md)
  : Map covariate and row Ids so they start from 1

## Helper/utility functions

- [`listAppend()`](https://ohdsi.github.io/PatientLevelPrediction/reference/listAppend.md)
  : join two lists
- [`listCartesian()`](https://ohdsi.github.io/PatientLevelPrediction/reference/listCartesian.md)
  : Cartesian product
- [`createTempModelLoc()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createTempModelLoc.md)
  : Create a temporary model location
- [`configurePython()`](https://ohdsi.github.io/PatientLevelPrediction/reference/configurePython.md)
  : Sets up a python environment to use for PLP (can be conda or venv)
- [`setPythonEnvironment()`](https://ohdsi.github.io/PatientLevelPrediction/reference/setPythonEnvironment.md)
  : Use the python environment created using configurePython()

## Evaluation measures

- [`averagePrecision()`](https://ohdsi.github.io/PatientLevelPrediction/reference/averagePrecision.md)
  : Calculate the average precision
- [`brierScore()`](https://ohdsi.github.io/PatientLevelPrediction/reference/brierScore.md)
  : brierScore
- [`calibrationLine()`](https://ohdsi.github.io/PatientLevelPrediction/reference/calibrationLine.md)
  : calibrationLine
- [`computeAuc()`](https://ohdsi.github.io/PatientLevelPrediction/reference/computeAuc.md)
  : Compute the area under the ROC curve
- [`computeAuprc()`](https://ohdsi.github.io/PatientLevelPrediction/reference/computeAuprc.md)
  : Compute the area under the Precision-Recall curve
- [`ici()`](https://ohdsi.github.io/PatientLevelPrediction/reference/ici.md)
  : Calculate the Integrated Calibration Index from Austin and
  Steyerberg https://onlinelibrary.wiley.com/doi/full/10.1002/sim.8281
- [`modelBasedConcordance()`](https://ohdsi.github.io/PatientLevelPrediction/reference/modelBasedConcordance.md)
  : Calculate the model-based concordance, which is a calculation of the
  expected discrimination performance of a model under the assumption
  the model predicts the "TRUE" outcome as detailed in van Klaveren et
  al. https://pubmed.ncbi.nlm.nih.gov/27251001/
- [`computeGridPerformance()`](https://ohdsi.github.io/PatientLevelPrediction/reference/computeGridPerformance.md)
  : Computes grid performance for a hyperparameter combination
  (backwards compatible)
- [`getCalibrationSummary()`](https://ohdsi.github.io/PatientLevelPrediction/reference/getCalibrationSummary.md)
  : Get a sparse summary of the calibration
- [`getDemographicSummary()`](https://ohdsi.github.io/PatientLevelPrediction/reference/getDemographicSummary.md)
  : Get a demographic summary
- [`getThresholdSummary()`](https://ohdsi.github.io/PatientLevelPrediction/reference/getThresholdSummary.md)
  : Calculate all measures for sparse ROC
- [`getPredictionDistribution()`](https://ohdsi.github.io/PatientLevelPrediction/reference/getPredictionDistribution.md)
  : Calculates the prediction distribution

## Saving/loading models as json

Functions for saving or loading models as json

- [`sklearnFromJson()`](https://ohdsi.github.io/PatientLevelPrediction/reference/sklearnFromJson.md)
  : Loads sklearn python model from json
- [`sklearnToJson()`](https://ohdsi.github.io/PatientLevelPrediction/reference/sklearnToJson.md)
  : Saves sklearn python model object to json in path

## Load/save for sharing

Functions for loading/saving objects for sharing

- [`savePlpShareable()`](https://ohdsi.github.io/PatientLevelPrediction/reference/savePlpShareable.md)
  : Save the plp result as json files and csv files for transparent
  sharing
- [`loadPlpShareable()`](https://ohdsi.github.io/PatientLevelPrediction/reference/loadPlpShareable.md)
  : Loads the plp result saved as json/csv files for transparent sharing
- [`loadPrediction()`](https://ohdsi.github.io/PatientLevelPrediction/reference/loadPrediction.md)
  : Loads the prediction dataframe to json
- [`savePrediction()`](https://ohdsi.github.io/PatientLevelPrediction/reference/savePrediction.md)
  : Saves the prediction dataframe to a json file

## Feature importance

- [`pfi()`](https://ohdsi.github.io/PatientLevelPrediction/reference/pfi.md)
  : Permutation Feature Importance

## Other functions

- [`predictCyclops()`](https://ohdsi.github.io/PatientLevelPrediction/reference/predictCyclops.md)
  : Create predictive probabilities
- [`predictGlm()`](https://ohdsi.github.io/PatientLevelPrediction/reference/predictGlm.md)
  : predict using a logistic regression model
- [`createGlmModel()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createGlmModel.md)
  : createGlmModel
- [`createSklearnModel()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createSklearnModel.md)
  : Plug an existing scikit learn python model into the PLP framework
