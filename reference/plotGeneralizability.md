# Plot the train/test generalizability diagnostic

Plot the train/test generalizability diagnostic

## Usage

``` r
plotGeneralizability(
  covariateSummary,
  saveLocation = NULL,
  fileName = "Generalizability.png"
)
```

## Arguments

- covariateSummary:

  A prediction object as generated using the
  [`runPlp`](https://ohdsi.github.io/PatientLevelPrediction/reference/runPlp.md)
  function.

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

Create a plot showing the train/test generalizability diagnostic \#'

## Examples
