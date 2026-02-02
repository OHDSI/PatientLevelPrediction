# viewPlp - Interactively view the performance and model settings

This is a shiny app for viewing interactive plots of the performance and
the settings

## Usage

``` r
viewPlp(runPlp, validatePlp = NULL, diagnosePlp = NULL)
```

## Arguments

- runPlp:

  The output of runPlp() (an object of class 'runPlp')

- validatePlp:

  The output of externalValidatePlp (on object of class 'validatePlp')

- diagnosePlp:

  The output of diagnosePlp()

## Value

Opens a shiny app for interactively viewing the results

## Details

Either the result of runPlp and view the plots

## Examples
