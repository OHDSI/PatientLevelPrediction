# Split the plpData into test/train sets using a splitting settings of class `splitSettings`

Split the plpData into test/train sets using a splitting settings of
class `splitSettings`

## Usage

``` r
splitData(
  plpData = plpData,
  population = population,
  splitSettings = createDefaultSplitSetting(splitSeed = 42)
)
```

## Arguments

- plpData:

  An object of type `plpData` - the patient level prediction data
  extracted from the CDM.

- population:

  The population created using `createStudyPopulation` that define who
  will be used to develop the model

- splitSettings:

  An object of type `splitSettings` specifying the split - the default
  can be created using `createDefaultSplitSetting`

## Value

Returns a list containing the training data (Train) and optionally the
test data (Test). Train is an Andromeda object containing

- covariateRef: a table with the covariate information

- labels: a table (rowId, outcomeCount, ...) for each data point in the
  train data (outcomeCount is the class label)

- folds: a table (rowId, index) specifying which training fold each data
  point is in.

Test is an Andromeda object containing

- covariateRef: a table with the covariate information

- labels: a table (rowId, outcomeCount, ...) for each data point in the
  test data (outcomeCount is the class label)

## Examples

``` r
data("simulationProfile")
plpData <- simulatePlpData(simulationProfile, n = 1000, seed = 42)
#> Generating covariates
#> Generating cohorts
#> Generating outcomes
population <- createStudyPopulation(plpData)
#> Outcome is 0 or 1
#> Population created with: 961 observations, 961 unique subjects and 499 outcomes
#> Population created in 0.0471 secs
splitSettings <- createDefaultSplitSetting(testFraction = 0.50, 
                                           trainFraction = 0.50, nfold = 5)
data = splitData(plpData, population, splitSettings)
#> seed: 74735
#> Creating a 50% test and 50% train (into 5 folds) random stratified split by class
#> Data split into 480 test cases and 481 train cases (97, 96, 96, 96, 96)
#> Data split in 1.4 secs
# test data should be ~500 rows (changes because of study population)
nrow(data$Test$labels)
#> [1] 480
# train data should be ~500 rows
nrow(data$Train$labels)
#> [1] 481
# should be five fold in the train data
length(unique(data$Train$folds$index))
#> [1] 5
```
