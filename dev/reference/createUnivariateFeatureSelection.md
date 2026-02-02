# Create the settings for defining any feature selection that will be done

Create the settings for defining any feature selection that will be done

## Usage

``` r
createUnivariateFeatureSelection(k = 100)
```

## Arguments

- k:

  This function returns the K features most associated (univariately) to
  the outcome

## Value

An object of class `featureEngineeringSettings`

## Details

Returns an object of class `featureEngineeringSettings` that specifies
the function that will be called and the settings. Uses the scikit-learn
SelectKBest function with chi2 for univariate feature selection.

## Examples
