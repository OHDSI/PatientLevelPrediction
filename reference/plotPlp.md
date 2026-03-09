# Plot all the PatientLevelPrediction plots

Plot all the PatientLevelPrediction plots

## Usage

``` r
plotPlp(plpResult, saveLocation = NULL, typeColumn = "evaluation")
```

## Arguments

- plpResult:

  Object returned by the runPlp() function

- saveLocation:

  Name of the directory where the plots should be saved (NULL means no
  saving)

- typeColumn:

  The name of the column specifying the evaluation type (to stratify the
  plots)

## Value

TRUE if it ran, plots are saved in the specified directory

## Details

Create a directory with all the plots

## Examples
