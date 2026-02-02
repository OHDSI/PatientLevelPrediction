# Plot the net benefit

Plot the net benefit

## Usage

``` r
plotNetBenefit(
  plpResults,
  modelNames = NULL,
  typeColumn = "evaluation",
  saveLocation = NULL,
  showPlot = TRUE,
  fileName = "netBenefit.png",
  evalType = NULL,
  ylim = NULL,
  xlim = NULL
)
```

## Arguments

- plpResults:

  list of (named) plpResult objects or a single plpResult as generated
  using the
  [`runPlp`](https://ohdsi.github.io/PatientLevelPrediction/dev/reference/runPlp.md)
  function.

- modelNames:

  (optional) names of the models to be used in the plot. If NULL, the
  names of the plpResults are used. Must have the same length as
  plpResults.

- typeColumn:

  The name of the column specifying the evaluation type

- saveLocation:

  Directory to save plot (if NULL plot is not saved)

- showPlot:

  If TRUE, the plot is shown on the screen, if FALSE the plot object is
  returned without plotting.

- fileName:

  Name of the file to save to plot, for example 'plot.png'. See the
  function `ggsave` in the ggplot2 package for supported file formats.

- evalType:

  Which evaluation type to plot for. For example `Test`, `Train`. If
  NULL everything is plotted

- ylim:

  The y limits for the plot, if NULL the limits are calculated from the
  data

- xlim:

  The x limits for the plot, if NULL the limits are calculated from the
  data

## Value

A list of ggplot objects or a single ggplot object if only one
evaluation type is plotted

## Examples
