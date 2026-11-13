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
#> Removing 3 redundant covariates
#> Removing 0 infrequent covariates
#> Normalizing covariates
#> Tidying covariates took 1.21 secs
# check age is normalized by max value
preProcessedData$covariates %>% dplyr::filter(.data$covariateId == 1002)
#> # A query:  ?? x 3
#> # Database: DuckDB 2.0.0 [unknown@Linux 7.0.0-1016-aws:R 4.6.2//tmp/Rtmpf7DTHg/file1cd07bb92ad5.duckdb]
#>    rowId covariateId covariateValue
#>    <int>       <dbl>          <dbl>
#>  1     1        1002          0.660
#>  2     2        1002          0.979
#>  3     3        1002          0.830
#>  4     4        1002          0.936
#>  5     5        1002          0.851
#>  6     6        1002          0.872
#>  7     7        1002          0.894
#>  8     8        1002          0.766
#>  9     9        1002          0.872
#> 10    10        1002          0.830
#> # ℹ more rows
```
