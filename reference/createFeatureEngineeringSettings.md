# Create the settings for defining any feature engineering that will be done

Create the settings for defining any feature engineering that will be
done

## Usage

``` r
createFeatureEngineeringSettings(type = "none")
```

## Arguments

- type:

  (character) Choice of:

  - 'none' No feature engineering - this is the default

## Value

An object of class `featureEngineeringSettings`

## Details

Returns an object of class `featureEngineeringSettings` that specifies
the sampling function that will be called and the settings

## Examples

``` r
createFeatureEngineeringSettings(type = "none")
#> list()
#> attr(,"fun")
#> [1] "sameData"
#> attr(,"class")
#> [1] "featureEngineeringSettings"
```
