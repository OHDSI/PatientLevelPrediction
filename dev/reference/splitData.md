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
#> Population created with: 954 observations, 954 unique subjects and 434 outcomes
#> Population created in 0.0483 secs
splitSettings <- createDefaultSplitSetting(testFraction = 0.50, 
                                           trainFraction = 0.50, nfold = 5)
data = splitData(plpData, population, splitSettings)
#> seed: 74735
#> Creating a 50% test and 50% train (into 5 folds) random stratified split by class
#> Data split into 477 test cases and 477 train cases (96, 96, 95, 95, 95)
#> Data split in 1.43 secs
# test data should be ~500 rows (changes because of study population)
nrow(data$Test$labels)
#> [1] 477
# train data should be ~500 rows
nrow(data$Train$labels)
#> [1] 477
# should be five fold in the train data
length(unique(data$Train$folds$index))
#> [1] 5
```
