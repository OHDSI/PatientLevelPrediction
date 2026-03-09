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

  Minimum cell count for the covariateSummary and certain evaluation
  results

## Value

                           The directory path where the results were saved

## Details

Saves the main results json/csv files (these files can be read by the
shiny app)

## Examples
