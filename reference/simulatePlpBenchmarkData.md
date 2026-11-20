# Generate benchmark PLP data with known outcome risk

`simulatePlpBenchmarkData` creates semi-synthetic PLP data by resampling
rows from an existing `plpData` object and generating outcomes from a
known logistic outcome model. The source covariate patterns are
preserved, so this function is intended for benchmark simulations rather
than realistic synthetic patient generation. Rows without observable
time at `riskWindowStart` are excluded before resampling, and generated
event times are sampled within each row's observed risk window. When
`daysToCohortEnd` is present and contains positive follow-up, event
times are also kept within the target cohort end.

The attached truth is defined for each simulated row's observable risk
window. For benchmark evaluation against this truth, use population
settings with `requireTimeAtRisk = FALSE`; otherwise PLP can drop
short-follow-up non-outcome rows and change the estimand. For a strict
fixed-horizon benchmark, first restrict the source `plpData` to rows
with complete follow-up through `riskWindowEnd`.

## Usage

``` r
simulatePlpBenchmarkData(
  plpData,
  outcomeModel,
  n = nrow(plpData$cohorts),
  riskWindowStart = 1,
  riskWindowEnd = 365,
  outcomeId = NULL,
  targetOutcomeRate = NULL,
  seed = NULL,
  returnTruth = TRUE
)
```

## Arguments

- plpData:

  An object of type `plpData`.

- outcomeModel:

  A named numeric vector of logistic model coefficients. Use
  `"(Intercept)"` for the intercept and covariate IDs for covariate
  coefficients.

- n:

  The number of rows to generate.

- riskWindowStart:

  The earliest generated `daysToEvent` for outcome cases.

- riskWindowEnd:

  The latest generated `daysToEvent` for outcome cases.

- outcomeId:

  The outcome ID to use. If `NULL`, the first outcome ID in
  `plpData$metaData$databaseDetails$outcomeIds` is used.

- targetOutcomeRate:

  Optional target mean true risk. If provided, the intercept is shifted
  so the generated population has this mean risk.

- seed:

  An optional seed for the random number generator.

- returnTruth:

  If `TRUE`, a truth table with source row IDs, linear predictors, true
  risks, and generated outcome status is attached as the
  `simulationTruth` attribute.

## Value

An object of type `plpData`.

## Examples

``` r
data("simulationProfile")
plpData <- simulatePlpData(simulationProfile, n = 100, seed = 42)
#> Generating covariates
#> Generating cohorts
#> Generating outcomes
benchmarkData <- simulatePlpBenchmarkData(
  plpData = plpData,
  outcomeModel = c("(Intercept)" = -2, "1002" = 0.04),
  n = 100,
  riskWindowStart = 1,
  riskWindowEnd = 365,
  seed = 42
)
attr(benchmarkData, "simulationTruth")
#> NULL
```
