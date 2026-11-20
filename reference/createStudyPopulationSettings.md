# create the study population settings

create the study population settings

## Usage

``` r
createStudyPopulationSettings(
  binary = TRUE,
  includeAllOutcomes = TRUE,
  firstExposureOnly = FALSE,
  washoutPeriod = 0,
  removeSubjectsWithPriorOutcome = TRUE,
  priorOutcomeLookback = 99999,
  requireTimeAtRisk = TRUE,
  minTimeAtRisk = 364,
  riskWindowStart = 1,
  startAnchor = "cohort start",
  riskWindowEnd = 365,
  endAnchor = "cohort start",
  restrictTarToCohortEnd = FALSE
)
```

## Arguments

- binary:

  Forces the outcomeCount to be 0 or 1 (use for binary prediction
  problems)

- includeAllOutcomes:

  (binary) indicating whether to include people with outcomes who are
  not observed for the whole at risk period

- firstExposureOnly:

  Should only the first exposure per subject be included? Note that this
  is typically done in the `createStudyPopulation` function,

- washoutPeriod:

  The mininum required continuous observation time prior to index date
  for a person to be included in the cohort.

- removeSubjectsWithPriorOutcome:

  Remove subjects that have the outcome prior to the risk window start?

- priorOutcomeLookback:

  How many days should we look back when identifying prior outcomes?

- requireTimeAtRisk:

  Should subject without time at risk be removed?

- minTimeAtRisk:

  The minimum number of days at risk required to be included

- riskWindowStart:

  The start of the risk window (in days) relative to the index date (+
  days of exposure if the `addExposureDaysToStart` parameter is
  specified).

- startAnchor:

  The anchor point for the start of the risk window. Can be "cohort
  start" or "cohort end".

- riskWindowEnd:

  The end of the risk window (in days) relative to the index data (+
  days of exposure if the `addExposureDaysToEnd` parameter is
  specified).

- endAnchor:

  The anchor point for the end of the risk window. Can be "cohort start"
  or "cohort end".

- restrictTarToCohortEnd:

  If using a survival model and you want the time-at-risk to end at the
  cohort end date set this to T

## Value

An object of type populationSettings containing all the settings
required for creating the study population

## Examples

``` r
# Create study population settings with a washout period of 30 days and a 
# risk window of 1 to 90 days
populationSettings <- createStudyPopulationSettings(washoutPeriod = 30, 
                                                    riskWindowStart = 1,
                                                    riskWindowEnd = 90)
#> Warning: issue: minTimeAtRisk is greater than max possible time-at-risk
```
