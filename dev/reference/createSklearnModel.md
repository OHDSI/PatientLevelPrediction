# Plug an existing scikit learn python model into the PLP framework

Plug an existing scikit learn python model into the PLP framework

## Usage

``` r
createSklearnModel(
  modelLocation = "/model",
  covariateMap = data.frame(columnId = 1:2, covariateId = c(1, 2), ),
  isPickle = TRUE,
  targetId = NULL,
  outcomeId = NULL,
  populationSettings = createStudyPopulationSettings(),
  restrictPlpDataSettings = createRestrictPlpDataSettings(),
  covariateSettings = FeatureExtraction::createDefaultCovariateSettings(),
  featureEngineering = NULL,
  tidyCovariates = NULL,
  requireDenseMatrix = FALSE,
  modelName = "existingSklearn"
)
```

## Arguments

- modelLocation:

  The location of the folder that contains the model as model.pkl

- covariateMap:

  A data.frame with the columns: columnId and covariateId. `covariateId`
  from FeatureExtraction is the standard OHDSI covariateId. `columnId`
  is the column location the model expects that covariate to be in. For
  example, if you had a column called 'age' in your model and this was
  the 3rd column when fitting the model, then the values for columnId
  would be 3, covariateId would be 1002 (the covariateId for age in
  years) and

- isPickle:

  If the model should be saved as a pickle set this to TRUE if it should
  be saved as json set this to FALSE.

- targetId:

  Add the development targetId here

- outcomeId:

  Add the development outcomeId here

- populationSettings:

  Add development population settings (this includes the time-at-risk
  settings).

- restrictPlpDataSettings:

  Add development restriction settings

- covariateSettings:

  Add the covariate settings here to specify how the model covariates
  are created from the OMOP CDM

- featureEngineering:

  Add any feature engineering here (e.g., if you need to modify the
  covariates before applying the model) This is a list of lists
  containing a string named funct specifying the engineering function to
  call and settings that are inputs to that function. funct must take as
  input trainData (a plpData object) and settings (a list).

- tidyCovariates:

  Add any tidyCovariates mappings here (e.g., if you need to normalize
  the covariates)

- requireDenseMatrix:

  Specify whether the model needs a dense matrix (TRUE or FALSE)

- modelName:

  A name that will show as the model type in the shiny app

## Value

An object of class plpModel, this is a list that contains: model (the
location of the model.pkl), preprocessing (settings for mapping the
covariateIds to the model column mames), modelDesign (specification of
the model design), trainDetails (information about the model fitting)
and covariateImportance.

You can use the output as an input in PatientLevelPrediction::predictPlp
to apply the model and calculate the risk for patients.

## Details

This function lets users add an existing scikit learn model that is
saved as model.pkl into PLP format. covariateMap is a mapping between
standard covariateIds and the model columns. The user also needs to
specify the covariate settings and population settings as these are used
to determine the standard PLP model design.
