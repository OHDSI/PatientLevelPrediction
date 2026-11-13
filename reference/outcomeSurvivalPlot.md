# Plot the outcome incidence over time

Plot the outcome incidence over time

## Usage

``` r
outcomeSurvivalPlot(
  plpData,
  outcomeId,
  populationSettings = createStudyPopulationSettings(binary = TRUE, includeAllOutcomes =
    TRUE, firstExposureOnly = FALSE, washoutPeriod = 0, removeSubjectsWithPriorOutcome =
    TRUE, priorOutcomeLookback = 99999, requireTimeAtRisk = FALSE, riskWindowStart = 1,
    startAnchor = "cohort start", riskWindowEnd = 3650, endAnchor = "cohort start"),
  riskTable = TRUE,
  confInt = TRUE,
  yLabel = "Fraction of those who are outcome free in target population"
)
```

## Arguments

- plpData:

  The plpData object returned by running getPlpData()

- outcomeId:

  The cohort id corresponding to the outcome

- populationSettings:

  The population settings created using `createStudyPopulationSettings`

- riskTable:

  (binary) Whether to include a table at the bottom of the plot showing
  the number of people at risk over time

- confInt:

  (binary) Whether to include a confidence interval

- yLabel:

  (string) The label for the y-axis

## Value

A `ggsurvplot` object

## Details

This creates a survival plot that can be used to pick a suitable
time-at-risk period

## Examples

``` r
data("simulationProfile")
plpData <- simulatePlpData(simulationProfile, n = 999, seed = 42)
#> Generating covariates
#> Generating cohorts
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmpGx3hts/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.
#> Generating outcomes
plotObject <- outcomeSurvivalPlot(plpData, outcomeId = 3)
#> outcomeId: 3
#> binary: TRUE
#> includeAllOutcomes: TRUE
#> firstExposureOnly: FALSE
#> washoutPeriod: 0
#> removeSubjectsWithPriorOutcome: TRUE
#> priorOutcomeLookback: 99999
#> requireTimeAtRisk: FALSE
#> minTimeAtRisk: 364
#> restrictTarToCohortEnd: FALSE
#> riskWindowStart: 1
#> startAnchor: cohort start
#> riskWindowEnd: 3650
#> endAnchor: cohort start
#> restrictTarToCohortEnd: FALSE
#> Removing subjects with prior outcomes (if any)
#> Removing subjects with no time at risk (if any)
#> Outcome is 0 or 1
#> Population created with: 999 observations, 999 unique subjects and 486 outcomes
#> Population created in 0.0313 secs
#> Ignoring unknown labels:
#> • fill : "Strata"
print(plotObject)
#> Ignoring unknown labels:
#> • fill : "Strata"
#> Ignoring unknown labels:
#> • fill : "Strata"
#> Ignoring unknown labels:
#> • fill : "Strata"
#> Ignoring unknown labels:
#> • colour : "Strata"
```
