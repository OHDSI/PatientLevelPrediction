# Create the settings for defining how the trainData from `splitData` are sampled using default sample functions.

Create the settings for defining how the trainData from `splitData` are
sampled using default sample functions.

## Usage

``` r
createSampleSettings(
  type = "none",
  numberOutcomestoNonOutcomes = 1,
  sampleSeed = sample(10000, 1)
)
```

## Arguments

- type:

  (character) Choice of:

  - 'none' No sampling is applied - this is the default

  - 'underSample' Undersample the non-outcome class to make the data
    more balanced

  - 'overSample' Oversample the outcome class by adding in each outcome
    multiple times

- numberOutcomestoNonOutcomes:

  (numeric) A numeric specifying the required number of outcomes per
  non-outcomes

- sampleSeed:

  (numeric) A seed to use when splitting the data for reproducibility
  (if not set a random number will be generated)

## Value

An object of class `sampleSettings`

## Details

Returns an object of class `sampleSettings` that specifies the sampling
function that will be called and the settings

## Examples

``` r
# \donttest{
# sample even rate of outcomes to non-outcomes
sampleSetting <- createSampleSettings(
  type = "underSample",
  numberOutcomestoNonOutcomes = 1,
  sampleSeed = 42
)
# }
```
