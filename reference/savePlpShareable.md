# Save the plp result as json files and csv files for transparent sharing

Save the plp result as json files and csv files for transparent sharing

## Usage

``` r
savePlpShareable(result, saveDirectory, minCellCount = 10)
```

## Arguments

- result:

  An object of class runPlp with development or validation results

- saveDirectory:

  The directory the save the results as csv files

- minCellCount:

  Counts below this value, and the values that give them back, are left
  empty in the covariate summary and evaluation results. The rules are
  those of
  [`extractDatabaseToCsv()`](https://ohdsi.github.io/PatientLevelPrediction/reference/extractDatabaseToCsv.md).

## Value

                           The directory path where the results were saved

## Details

Saves the main results json/csv files (these files can be read by the
shiny app)

## Examples
