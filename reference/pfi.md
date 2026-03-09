# Permutation Feature Importance

Calculate the permutation feature importance (pfi) for a PLP model.

## Usage

``` r
pfi(
  plpResult,
  population,
  plpData,
  repeats = 1,
  covariates = NULL,
  cores = NULL,
  log = NULL,
  logthreshold = "INFO"
)
```

## Arguments

- plpResult:

  An object of type `runPlp`

- population:

  The population created using createStudyPopulation() who will have
  their risks predicted

- plpData:

  An object of type `plpData` - the patient level prediction data
  extracted from the CDM.

- repeats:

  The number of times to permute each covariate

- covariates:

  A vector of covariates to calculate the pfi for. If NULL it uses all
  covariates included in the model.

- cores:

  Number of cores to use when running this (it runs in parallel)

- log:

  A location to save the log for running pfi

- logthreshold:

  The log threshold (e.g., INFO, TRACE, ...)

## Value

A dataframe with the covariateIds and the pfi (change in AUC caused by
permuting the covariate) value

## Details

The function permutes the each covariate/features `repeats` times and
calculates the mean AUC change caused by the permutation.

## Examples
