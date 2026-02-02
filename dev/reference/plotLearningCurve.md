# plotLearningCurve

Create a plot of the learning curve using the object returned from
`createLearningCurve`.

## Usage

``` r
plotLearningCurve(
  learningCurve,
  metric = "AUROC",
  abscissa = "events",
  plotTitle = "Learning Curve",
  plotSubtitle = NULL,
  fileName = NULL
)
```

## Arguments

- learningCurve:

  An object returned by
  [`createLearningCurve`](https://ohdsi.github.io/PatientLevelPrediction/dev/reference/createLearningCurve.md)
  function.

- metric:

  Specifies the metric to be plotted:

  - `'AUROC'` - use the area under the Receiver Operating Characteristic
    curve

  - `'AUPRC'` - use the area under the Precision-Recall curve

  - `'sBrier'` - use the scaled Brier score

- abscissa:

  Specify the abscissa metric to be plotted:

  - `'events'` - use number of events

  - `'observations'` - use number of observations

- plotTitle:

  Title of the learning curve plot.

- plotSubtitle:

  Subtitle of the learning curve plot.

- fileName:

  Filename of plot to be saved, for example `'plot.png'`. See the
  function `ggsave` in the ggplot2 package for supported file formats.

## Value

A ggplot object. Use the
[`ggsave`](https://ggplot2.tidyverse.org/reference/ggsave.html) function
to save to file in a different format.

## Examples
