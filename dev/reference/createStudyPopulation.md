# Create a study population

Create a study population

## Usage

``` r
createStudyPopulation(
  plpData,
  outcomeId = plpData$metaData$databaseDetails$outcomeIds[1],
  populationSettings = createStudyPopulationSettings(),
  population = NULL
)
```

## Arguments

- plpData:

  An object of type `plpData` as generated using `getplpData`.

- outcomeId:

  The ID of the outcome.

- populationSettings:

  An object of class populationSettings created using
  `createPopulationSettings`

- population:

  If specified, this population will be used as the starting point
  instead of the cohorts in the `plpData` object.

## Value

A data frame specifying the study population. This data frame will have
the following columns:

- rowId:

  A unique identifier for an exposure

- subjectId:

  The person ID of the subject

- cohortStartdate:

  The index date

- outcomeCount:

  The number of outcomes observed during the risk window

- timeAtRisk:

  The number of days in the risk window

- survivalTime:

  The number of days until either the outcome or the end of the risk
  window

## Details

Create a study population by enforcing certain inclusion and exclusion
criteria, defining a risk window, and determining which outcomes fall
inside the risk window.

## Examples
