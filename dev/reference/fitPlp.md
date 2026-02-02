# fitPlp

Train various models using a default parameter grid search or user
specified parameters

## Usage

``` r
fitPlp(
  trainData,
  modelSettings,
  hyperparameterSettings = createHyperparameterSettings(),
  analysisId,
  analysisPath
)
```

## Arguments

- trainData:

  An object of type `trainData` created using `splitData` data extracted
  from the CDM.

- modelSettings:

  An object of class `modelSettings` created using one of the
  `createModelSettings` functions

- hyperparameterSettings:

  An object of class `hyperparameterSettings`'

- analysisId:

  The id of the analysis

- analysisPath:

  The path of the analysis

## Value

An object of class `plpModel` containing:

- model:

  The trained prediction model

- preprocessing:

  The preprocessing required when applying the model

- prediction:

  The cohort data.frame with the predicted risk column added

- modelDesign:

  A list specifiying the modelDesign settings used to fit the model

- trainDetails:

  The model meta data

- covariateImportance:

  The covariate importance for the model

## Details

The user can define the machine learning model to train

## Examples
