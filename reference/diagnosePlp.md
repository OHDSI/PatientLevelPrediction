# diagnostic - Investigates the prediction problem settings - use before training a model

This function runs a set of prediction diagnoses to help pick a suitable
T, O, TAR and determine whether the prediction problem is worth
executing.

## Usage

``` r
diagnosePlp(
  plpData = NULL,
  outcomeId,
  analysisId,
  populationSettings,
  splitSettings = createDefaultSplitSetting(),
  sampleSettings = createSampleSettings(),
  saveDirectory = NULL,
  featureEngineeringSettings = createFeatureEngineeringSettings(),
  modelSettings = setLassoLogisticRegression(),
  logSettings = createLogSettings(verbosity = "DEBUG", timeStamp = TRUE, logName =
    "diagnosePlp Log"),
  preprocessSettings = createPreprocessSettings()
)
```

## Arguments

- plpData:

  An object of type `plpData` - the patient level prediction data
  extracted from the CDM. Can also include an initial population as
  plpData\$popualtion.

- outcomeId:

  (integer) The ID of the outcome.

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

- sampleSettings:

  An object of type `sampleSettings` that specifies any under/over
  sampling to be done. The default is none.

- saveDirectory:

  The path to the directory where the results will be saved (if NULL
  uses working directory)

- featureEngineeringSettings:

  An object of `featureEngineeringSettings` specifying any feature
  engineering to be learned (using the train data)

- modelSettings:

  An object of class `modelSettings`, usually created using one of the
  model setting functions:

  - setLassoLogisticRegression() A lasso logistic regression model

  - setGradientBoostingMachine() A gradient boosting machine

  - setAdaBoost() An ada boost model

  - setRandomForest() A random forest model

  - setDecisionTree() A decision tree model

  - setSVM() A support vector machine model

  - setMLP() A neural network model

  - setNaiveBayes() A naive Bayes model

  - setLightGBM() A LightGBM model

- logSettings:

  An object of `logSettings` created using `createLogSettings`
  specifying how the logging is done

- preprocessSettings:

  An object of `preprocessSettings`. This setting specifies the minimum
  fraction of target population who must have a covariate for it to be
  included in the model training and whether to normalise the covariates
  before training

## Value

An object containing the model or location where the model is saved, the
data selection settings, the preprocessing and training settings as well
as various performance measures obtained by the model.

- `distribution`: List for each O of a data.frame containing: i) Time to
  observation end distribution, ii) Time from observation start
  distribution, iii) Time to event distribution and iv) Time from last
  prior event to index distribution (only for patients in T who have O
  before index)

- `incident`: List for each O of incidence of O in T during TAR

- `characterization`: List for each O of Characterization of T, TnO,
  Tn~O

## Details

Users can define set of Ts, Os, databases and population settings. A
list of data.frames containing details such as follow-up time
distribution, time-to-event information, characteriszation details, time
from last prior event, observation time distribution.

## Examples
