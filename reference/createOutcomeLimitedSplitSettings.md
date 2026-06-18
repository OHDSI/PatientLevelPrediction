# Create outcome-limited split settings

Create split settings for large data sets where model training should
use at most a target number of outcome-positive rows. When the
population has no more outcome rows than the cap, ordinary 25 percent
test and 75 percent train splitting is used.

John et al. found that, for LASSO logistic regression models fit on
large observational health care data sets, learning curves often plateau
after a certain number of outcome events. When a study has many more
outcome events than are needed to approach the full-data model
performance, this split can be useful to reduce computational cost while
preserving a separate test set.

In row-level stratified mode, the cap can be applied exactly. In
subject-level mode, whole subjects are assigned to either training or
testing, so the training outcome count can be below the cap. If the
selected subjects cannot leave outcome-positive rows for testing,
splitting fails with an actionable error.

## Usage

``` r
createOutcomeLimitedSplitSettings(
  maxTrainingOutcomes,
  splitSeed = sample(1e+05, 1),
  nfold = 3,
  type = "stratified"
)
```

## Arguments

- maxTrainingOutcomes:

  Maximum number of outcome-positive rows to include in the training set
  when the cap is triggered. There is no universal default: the adequate
  number of outcomes depends on the prediction problem, database, model,
  and acceptable performance loss. In subject-level mode this is
  approximate because all rows for selected subjects are kept together.

- splitSeed:

  A seed to use when splitting the data for reproducibility.

- nfold:

  An integer \> 1 specifying the number of folds used in cross
  validation.

- type:

  Either `stratified` for row-level splitting or `subject` for
  subject-level splitting.

## Value

An object of class `splitSettings`.

## References

John LH, Kors JA, Reps JM, Ryan PB, Rijnbeek PR. Logistic regression
models for patient-level prediction based on massive observational data:
Do we need all data? International Journal of Medical Informatics.
2022;163:104762.
[doi:10.1016/j.ijmedinf.2022.104762](https://doi.org/10.1016/j.ijmedinf.2022.104762)

## Examples

``` r
createOutcomeLimitedSplitSettings(maxTrainingOutcomes = 1000, splitSeed = 42)
#> $maxTrainingOutcomes
#> [1] 1000
#> 
#> $seed
#> [1] 42
#> 
#> $nfold
#> [1] 3
#> 
#> $type
#> [1] "stratified"
#> 
#> attr(,"fun")
#> [1] "outcomeLimitedSplitter"
#> attr(,"class")
#> [1] "splitSettings"
```
