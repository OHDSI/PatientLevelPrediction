# A function that wraps around FeatureExtraction::tidyCovariateData to normalise the data and remove rare or redundant features

A function that wraps around FeatureExtraction::tidyCovariateData to
normalise the data and remove rare or redundant features

## Usage

``` r
preprocessData(covariateData, preprocessSettings = createPreprocessSettings())
```

## Arguments

- covariateData:

  The covariate part of the training data created by `splitData` after
  being sampled and having any required feature engineering

- preprocessSettings:

  The settings for the preprocessing created by
  `createPreprocessSettings` The data processed

## Value

The covariateData object with the processed covariates

## Details

Returns an object of class `covariateData` that has been processed. This
includes normalising the data and removing rare or redundant features.
Redundant features are features that within an analysisId together cover
all obervations.

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
data("simulationProfile")
plpData <- simulatePlpData(simulationProfile, n = 1000, seed = 42)
#> Generating covariates
#> Generating cohorts
#> Generating outcomes
preProcessedData <- preprocessData(plpData$covariateData, createPreprocessSettings())
#> Removing 2 redundant covariates
#> Removing 0 infrequent covariates
#> Normalizing covariates
#> Tidying covariates took 1.66 secs
# check age is normalized by max value
preProcessedData$covariates %>% dplyr::filter(.data$covariateId == 1002)
#> # Source:   SQL [?? x 3]
#> # Database: DuckDB 1.5.2 [unknown@Linux 6.17.0-1018-azure:R 4.6.0//tmp/RtmpkKkjda/file1f1c7a8eac42.duckdb]
#>    rowId covariateId covariateValue
#>    <int>       <dbl>          <dbl>
#>  1     1        1002          0.851
#>  2     2        1002          0.936
#>  3     3        1002          0.851
#>  4     4        1002          0.872
#>  5     5        1002          0.872
#>  6     6        1002          0.787
#>  7     7        1002          0.809
#>  8     8        1002          0.872
#>  9     9        1002          0.872
#> 10    10        1002          0.787
#> # ℹ more rows
```
