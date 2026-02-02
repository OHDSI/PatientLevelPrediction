# A simulation profile for generating synthetic patient level prediction data

A simulation profile for generating synthetic patient level prediction
data

## Usage

``` r
data(simulationProfile)
```

## Format

A data frame containing the following elements:

- covariatePrevalence:

  prevalence of all covariates

- outcomeModels:

  regression model parameters to simulate outcomes

- metaData:

  settings used to simulate the profile

- covariateRef:

  covariateIds and covariateNames

- timePrevalence:

  time window

- exclusionPrevalence:

  prevalence of exclusion of covariates
