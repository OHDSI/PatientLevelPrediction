# Plot the preference score probability density function, showing prediction overlap between true and false cases \#'

Plot the preference score probability density function, showing
prediction overlap between true and false cases \#'

## Usage

``` r
plotPreferencePDF(
  plpResult,
  typeColumn = "evaluation",
  saveLocation = NULL,
  fileName = "plotPreferencePDF.png"
)
```

## Arguments

- plpResult:

  A plp result object as generated using the
  [`runPlp`](https://ohdsi.github.io/PatientLevelPrediction/reference/runPlp.md)
  function.

- typeColumn:

  The name of the column specifying the evaluation type

- saveLocation:

  Directory to save plot (if NULL plot is not saved)

- fileName:

  Name of the file to save to plot, for example 'plot.png'. See the
  function `ggsave` in the ggplot2 package for supported file formats.

## Value

A ggplot object. Use the
[`ggsave`](https://ggplot2.tidyverse.org/reference/ggsave.html) function
to save to file in a different format.

## Details

Create a plot showing the preference score probability density function,
showing prediction overlap between true and false cases \#'

## Examples
