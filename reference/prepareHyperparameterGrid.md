# Prepare Hyperparameter

Prepare Hyperparameter

## Usage

``` r
prepareHyperparameterGrid(paramDefinition, hyperSettings)
```

## Arguments

- paramDefinition:

  A list defining the hyperparameters and their possible values.

- hyperSettings:

  An object of class `hyperparameterSettings` created using
  `createHyperparameterSettings`.

## Value

An iterator object with methods to get the next hyperparameter
combination and finalize the search.
