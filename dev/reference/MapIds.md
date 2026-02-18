# Map covariate and row Ids so they start from 1

this functions takes covariate data and a cohort/population and remaps
the covariate and row ids, restricts to pop and saves/creates mapping

## Usage

``` r
MapIds(covariateData, cohort = NULL, mapping = NULL)
```

## Arguments

- covariateData:

  a covariateData object

- cohort:

  if specified rowIds restricted to the ones in cohort

- mapping:

  A pre defined mapping to use

## Value

a new `covariateData` object with remapped covariate and row ids

## Examples

``` r
covariateData <- Andromeda::andromeda(
  covariates = data.frame(rowId = c(1, 3, 5, 7, 9), 
                          covariateId = c(10, 20, 10, 10, 20),
                          covariateValue = c(1, 1, 1, 1, 1)),
  covariateRef = data.frame(covariateId = c(10, 20), 
                              covariateNames = c("covariateA", 
                                                 "covariateB"),
                              analysisId = c(1, 1)))
mappedData <- MapIds(covariateData)
#> starting to map the columns and rows
#> finished MapCovariates
# columnId and rowId are now starting from 1 and are consecutive
mappedData$covariates
#> # Source:   table<covariates> [?? x 4]
#> # Database: DuckDB 1.4.4 [unknown@Linux 6.14.0-1017-azure:R 4.5.2//tmp/Rtmp6rBTNM/file1e9d4a525281.duckdb]
#>   covariateId covariateValue columnId rowId
#>         <dbl>          <dbl>    <int> <int>
#> 1          10              1        1     1
#> 2          20              1        2     4
#> 3          10              1        1     2
#> 4          10              1        1     3
#> 5          20              1        2     5
```
