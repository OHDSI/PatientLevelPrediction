# Create the settings for defining how the plpData are split into test/validation/train sets using default splitting functions (either random stratified by outcome, time or subject splitting)

Create the settings for defining how the plpData are split into
test/validation/train sets using default splitting functions (either
random stratified by outcome, time or subject splitting)

## Usage

``` r
createDefaultSplitSetting(
  testFraction = 0.25,
  trainFraction = 0.75,
  splitSeed = sample(1e+05, 1),
  nfold = 3,
  type = "stratified"
)
```

## Arguments

- testFraction:

  (numeric) A real number between 0 and 1 indicating the test set
  fraction of the data

- trainFraction:

  (numeric) A real number between 0 and 1 indicating the train set
  fraction of the data. If not set train is equal to 1 - test

- splitSeed:

  (numeric) A seed to use when splitting the data for reproducibility
  (if not set a random number will be generated)

- nfold:

  (numeric) An integer \> 1 specifying the number of folds used in cross
  validation

- type:

  (character) Choice of:

  - 'stratified' Each data point is randomly assigned into the test or a
    train fold set but this is done stratified such that the outcome
    rate is consistent in each partition

  - 'time' Older data are assigned into the training set and newer data
    are assigned into the test set

  - 'subject' Data are partitioned by subject, if a subject is in the
    data more than once, all the data points for the subject are
    assigned either into the test data or into the train data (not
    both).

## Value

An object of class `splitSettings`

## Details

Returns an object of class `splitSettings` that specifies the splitting
function that will be called and the settings

## Examples

``` r
createDefaultSplitSetting(testFraction=0.25, trainFraction=0.75, nfold=3,
                          splitSeed=42)
#> $test
#> [1] 0.25
#> 
#> $train
#> [1] 0.75
#> 
#> $seed
#> [1] 42
#> 
#> $nfold
#> [1] 3
#> 
#> attr(,"fun")
#> [1] "randomSplitter"
#> attr(,"class")
#> [1] "splitSettings"
```
