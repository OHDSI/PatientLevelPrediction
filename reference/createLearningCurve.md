# createLearningCurve

Creates a learning curve object, which can be plotted using the
[`plotLearningCurve()`](https://ohdsi.github.io/PatientLevelPrediction/reference/plotLearningCurve.md)
function.

## Usage

``` r
createLearningCurve(
  plpData,
  outcomeId,
  parallel = TRUE,
  cores = 4,
  modelSettings,
  saveDirectory = NULL,
  analysisId = "learningCurve",
  populationSettings = createStudyPopulationSettings(),
  splitSettings = createDefaultSplitSetting(),
  trainFractions = c(0.25, 0.5, 0.75),
  trainEvents = NULL,
  sampleSettings = createSampleSettings(),
  featureEngineeringSettings = createFeatureEngineeringSettings(),
  preprocessSettings = createPreprocessSettings(minFraction = 0.001, normalize = TRUE),
  logSettings = createLogSettings(),
  executeSettings = createExecuteSettings(runSplitData = TRUE, runSampleData = FALSE,
    runFeatureEngineering = FALSE, runPreprocessData = TRUE, runModelDevelopment = TRUE,
    runCovariateSummary = FALSE)
)
```

## Arguments

- plpData:

  An object of type `plpData` - the patient level prediction data
  extracted from the CDM.

- outcomeId:

  (integer) The ID of the outcome.

- parallel:

  Whether to run the code in parallel

- cores:

  The number of computer cores to use if running in parallel

- modelSettings:

  An object of class `modelSettings` created using one of the function:

  - [`setLassoLogisticRegression()`](https://ohdsi.github.io/PatientLevelPrediction/reference/setLassoLogisticRegression.md)
    A lasso logistic regression model

  - [`setGradientBoostingMachine()`](https://ohdsi.github.io/PatientLevelPrediction/reference/setGradientBoostingMachine.md)
    A gradient boosting machine

  - [`setAdaBoost()`](https://ohdsi.github.io/PatientLevelPrediction/reference/setAdaBoost.md)
    An ada boost model

  - [`setRandomForest()`](https://ohdsi.github.io/PatientLevelPrediction/reference/setRandomForest.md)
    A random forest model

  - [`setDecisionTree()`](https://ohdsi.github.io/PatientLevelPrediction/reference/setDecisionTree.md)
    A decision tree model

  - `setKNN()` A KNN model

- saveDirectory:

  The path to the directory where the results will be saved (if NULL
  uses working directory)

- analysisId:

  (integer) Identifier for the analysis. It is used to create, e.g., the
  result folder. Default is a timestamp.

- populationSettings:

  An object of type `populationSettings` created using
  `createStudyPopulationSettings` that specifies how the data class
  labels are defined and addition any exclusions to apply to the plpData
  cohort

- splitSettings:

  An object of type `splitSettings` that specifies how to split the data
  into train/validation/test. The default settings can be created using
  `createDefaultSplitSetting`.

- trainFractions:

  A list of training fractions to create models for. Note, providing
  `trainEvents` will override your input to `trainFractions`.

- trainEvents:

  Events have shown to be determinant of model performance. Therefore,
  it is recommended to provide `trainEvents` rather than
  `trainFractions`. Note, providing `trainEvents` will override your
  input to `trainFractions`. The format should be as follows:

  - `c(500, 1000, 1500) ` - a list of training events

- sampleSettings:

  An object of type `sampleSettings` that specifies any under/over
  sampling to be done. The default is none.

- featureEngineeringSettings:

  An object of `featureEngineeringSettings` specifying any feature
  engineering to be learned (using the train data)

- preprocessSettings:

  An object of `preprocessSettings`. This setting specifies the minimum
  fraction of target population who must have a covariate for it to be
  included in the model training and whether to normalise the covariates
  before training

- logSettings:

  An object of `logSettings` created using `createLogSettings`
  specifying how the logging is done

- executeSettings:

  An object of `executeSettings` specifying which parts of the analysis
  to run

## Value

A learning curve object containing the various performance measures
obtained by the model for each training set fraction. It can be plotted
using `plotLearningCurve`.

## Examples
