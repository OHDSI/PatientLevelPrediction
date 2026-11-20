# Generate benchmark PLP data with known outcome risk

`simulatePlpBenchmarkData` creates semi-synthetic PLP data by resampling
rows from an existing `plpData` object and generating outcomes from a
known logistic outcome model. The source covariate patterns are
preserved, so this function is intended for benchmark simulations rather
than realistic synthetic patient generation. Rows without observable
time at `riskWindowStart` are excluded before resampling, and generated
event times are sampled within each row's observed risk window. When
`daysToCohortEnd` is present and contains positive follow-up, event
times are also kept within the target cohort end.

The attached truth is defined for each simulated row's observable risk
window. For benchmark evaluation against this truth, use population
settings with `requireTimeAtRisk = FALSE`; otherwise PLP can drop
short-follow-up non-outcome rows and change the estimand. For a strict
fixed-horizon benchmark, first restrict the source `plpData` to rows
with complete follow-up through `riskWindowEnd`.

Rows are sampled with replacement, as in standard plasmode simulation. A
source row can therefore appear more than once with different simulated
`rowId` and `subjectId` values. The truth table records `sourceRowId`
and `sourceSubjectId` so duplicate source rows can be identified. This
is not a problem when using the simulated data to study known risks in
realistic covariate distributions, but ordinary row-level train/test
splitting can put the same source covariate pattern in both partitions
and can make held-out performance optimistic.

## Usage

``` r
simulatePlpBenchmarkData(
  plpData,
  outcomeModel,
  n = nrow(plpData$cohorts),
  riskWindowStart = 1,
  riskWindowEnd = 365,
  outcomeId = NULL,
  targetOutcomeRate = NULL,
  seed = NULL,
  returnTruth = TRUE
)
```

## Arguments

- plpData:

  An object of type `plpData`.

- outcomeModel:

  A PLP model, a
  [`runPlp()`](https://ohdsi.github.io/PatientLevelPrediction/reference/runPlp.md)
  result, or a named numeric vector of logistic model coefficients. When
  a PLP model or result is supplied, outcomes are generated from
  [`predictPlp()`](https://ohdsi.github.io/PatientLevelPrediction/reference/predictPlp.md),
  so the model's feature engineering and preprocessing are applied
  before calculating true risks. Named numeric vectors are used directly
  and must already match the scale and feature space of `plpData`; use
  `"(Intercept)"` for the intercept and covariate IDs for covariate
  coefficients.

- n:

  The number of rows to generate.

- riskWindowStart:

  The earliest generated `daysToEvent` for outcome cases.

- riskWindowEnd:

  The latest generated `daysToEvent` for outcome cases.

- outcomeId:

  The outcome ID to use. If `NULL`, the first outcome ID in
  `plpData$metaData$databaseDetails$outcomeIds` is used.

- targetOutcomeRate:

  Optional target mean true risk. If provided, the intercept is shifted
  so the generated population has this mean risk.

- seed:

  An optional seed for the random number generator.

- returnTruth:

  If `TRUE`, a truth table with source row and subject IDs, linear
  predictors, true risks, and generated outcome status is returned as
  `simulationTruth`.

## Value

An object of type `plpData`. The returned object includes
`simulationSettings`; when `returnTruth = TRUE`, it also includes
`simulationTruth`.

## Examples

``` r
data("simulationProfile")
plpData <- simulatePlpData(simulationProfile, n = 100, seed = 42)
#> Generating covariates
#> Generating cohorts
#> Generating outcomes
benchmarkData <- simulatePlpBenchmarkData(
  plpData = plpData,
  outcomeModel = c("(Intercept)" = -2, "1002" = 0.04),
  n = 100,
  riskWindowStart = 1,
  riskWindowEnd = 365,
  seed = 42
)
benchmarkData$simulationTruth
#>     rowId sourceRowId sourceSubjectId linearPredictor  trueRisk
#> 1       1          49           2e+10           -0.48 0.3822521
#> 2       2          65           2e+10           -0.64 0.3452465
#> 3       3          25           2e+10           -0.52 0.3728522
#> 4       4          74           2e+10           -0.48 0.3822521
#> 5       5         100           2e+10           -0.60 0.3543437
#> 6       6          18           2e+10           -0.32 0.4206757
#> 7       7          49           2e+10           -0.48 0.3822521
#> 8       8          47           2e+10           -0.52 0.3728522
#> 9       9          24           2e+10           -0.20 0.4501660
#> 10     10          71           2e+10           -0.48 0.3822521
#> 11     11         100           2e+10           -0.60 0.3543437
#> 12     12          89           2e+10           -0.36 0.4109596
#> 13     13          37           2e+10           -0.56 0.3635475
#> 14     14          20           2e+10           -0.52 0.3728522
#> 15     15          26           2e+10           -0.52 0.3728522
#> 16     16           3           2e+10           -0.72 0.3273930
#> 17     17          41           2e+10           -0.48 0.3822521
#> 18     18          89           2e+10           -0.36 0.4109596
#> 19     19          27           2e+10           -0.64 0.3452465
#> 20     20          36           2e+10           -0.28 0.4304538
#> 21     21          95           2e+10           -0.32 0.4206757
#> 22     22           5           2e+10           -0.48 0.3822521
#> 23     23          84           2e+10           -0.48 0.3822521
#> 24     24          34           2e+10           -0.28 0.4304538
#> 25     25          92           2e+10           -0.44 0.3917410
#> 26     26           3           2e+10           -0.72 0.3273930
#> 27     27          58           2e+10           -0.60 0.3543437
#> 28     28          97           2e+10           -0.44 0.3917410
#> 29     29          42           2e+10           -0.48 0.3822521
#> 30     30          24           2e+10           -0.20 0.4501660
#> 31     31          30           2e+10           -0.72 0.3273930
#> 32     32          43           2e+10           -0.40 0.4013123
#> 33     33          15           2e+10           -0.64 0.3452465
#> 34     34          22           2e+10           -0.44 0.3917410
#> 35     35          58           2e+10           -0.60 0.3543437
#> 36     36           8           2e+10           -0.56 0.3635475
#> 37     37          36           2e+10           -0.28 0.4304538
#> 38     38          68           2e+10           -0.44 0.3917410
#> 39     39          86           2e+10           -0.48 0.3822521
#> 40     40          18           2e+10           -0.32 0.4206757
#> 41     41          92           2e+10           -0.44 0.3917410
#> 42     42          69           2e+10           -0.52 0.3728522
#> 43     43           4           2e+10           -0.56 0.3635475
#> 44     44          98           2e+10           -0.56 0.3635475
#> 45     45          50           2e+10           -0.24 0.4402864
#> 46     46          99           2e+10           -0.48 0.3822521
#> 47     47          88           2e+10           -0.52 0.3728522
#> 48     48          87           2e+10           -0.28 0.4304538
#> 49     49          49           2e+10           -0.48 0.3822521
#> 50     50          26           2e+10           -0.52 0.3728522
#> 51     51           6           2e+10           -0.68 0.3362613
#> 52     52           6           2e+10           -0.68 0.3362613
#> 53     53           2           2e+10           -0.24 0.4402864
#> 54     54           3           2e+10           -0.72 0.3273930
#> 55     55          21           2e+10           -0.40 0.4013123
#> 56     56           2           2e+10           -0.24 0.4402864
#> 57     57          58           2e+10           -0.60 0.3543437
#> 58     58          10           2e+10           -0.76 0.3186463
#> 59     59          40           2e+10           -0.40 0.4013123
#> 60     60           5           2e+10           -0.48 0.3822521
#> 61     61          33           2e+10           -0.28 0.4304538
#> 62     62          49           2e+10           -0.48 0.3822521
#> 63     63         100           2e+10           -0.60 0.3543437
#> 64     64          73           2e+10           -0.40 0.4013123
#> 65     65          29           2e+10           -0.32 0.4206757
#> 66     66          76           2e+10           -0.64 0.3452465
#> 67     67          84           2e+10           -0.48 0.3822521
#> 68     68           9           2e+10           -0.56 0.3635475
#> 69     69          35           2e+10           -0.40 0.4013123
#> 70     70          93           2e+10           -0.44 0.3917410
#> 71     71          16           2e+10           -0.44 0.3917410
#> 72     72          92           2e+10           -0.44 0.3917410
#> 73     73          69           2e+10           -0.52 0.3728522
#> 74     74          92           2e+10           -0.44 0.3917410
#> 75     75           2           2e+10           -0.24 0.4402864
#> 76     76          82           2e+10           -0.52 0.3728522
#> 77     77          24           2e+10           -0.20 0.4501660
#> 78     78          18           2e+10           -0.32 0.4206757
#> 79     79          69           2e+10           -0.52 0.3728522
#> 80     80          55           2e+10           -0.16 0.4600851
#> 81     81          40           2e+10           -0.40 0.4013123
#> 82     82          21           2e+10           -0.40 0.4013123
#> 83     83         100           2e+10           -0.60 0.3543437
#> 84     84          57           2e+10           -0.60 0.3543437
#> 85     85         100           2e+10           -0.60 0.3543437
#> 86     86          42           2e+10           -0.48 0.3822521
#> 87     87          18           2e+10           -0.32 0.4206757
#> 88     88          91           2e+10           -0.60 0.3543437
#> 89     89          13           2e+10           -0.24 0.4402864
#> 90     90          53           2e+10           -0.56 0.3635475
#> 91     91          54           2e+10           -0.36 0.4109596
#> 92     92          83           2e+10           -0.36 0.4109596
#> 93     93          32           2e+10           -0.60 0.3543437
#> 94     94          80           2e+10           -0.40 0.4013123
#> 95     95          60           2e+10           -0.28 0.4304538
#> 96     96          29           2e+10           -0.32 0.4206757
#> 97     97          81           2e+10           -0.68 0.3362613
#> 98     98          73           2e+10           -0.40 0.4013123
#> 99     99          85           2e+10           -0.48 0.3822521
#> 100   100          43           2e+10           -0.40 0.4013123
#>     interceptAdjustment outcomeCount daysToEvent
#> 1                     0            1         124
#> 2                     0            0          NA
#> 3                     0            0          NA
#> 4                     0            0          NA
#> 5                     0            1          63
#> 6                     0            1          49
#> 7                     0            0          NA
#> 8                     0            0          NA
#> 9                     0            0          NA
#> 10                    0            0          NA
#> 11                    0            0          NA
#> 12                    0            0          NA
#> 13                    0            0          NA
#> 14                    0            0          NA
#> 15                    0            0          NA
#> 16                    0            1          61
#> 17                    0            0          NA
#> 18                    0            0          NA
#> 19                    0            0          NA
#> 20                    0            0          NA
#> 21                    0            0          NA
#> 22                    0            1           4
#> 23                    0            0          NA
#> 24                    0            1         257
#> 25                    0            1          28
#> 26                    0            1           2
#> 27                    0            1         159
#> 28                    0            0          NA
#> 29                    0            0          NA
#> 30                    0            1         136
#> 31                    0            1         127
#> 32                    0            1         208
#> 33                    0            1         259
#> 34                    0            0          NA
#> 35                    0            0          NA
#> 36                    0            0          NA
#> 37                    0            1         229
#> 38                    0            0          NA
#> 39                    0            0          NA
#> 40                    0            0          NA
#> 41                    0            0          NA
#> 42                    0            0          NA
#> 43                    0            0          NA
#> 44                    0            1          13
#> 45                    0            1          12
#> 46                    0            1         193
#> 47                    0            0          NA
#> 48                    0            1         158
#> 49                    0            0          NA
#> 50                    0            0          NA
#> 51                    0            0          NA
#> 52                    0            1          51
#> 53                    0            0          NA
#> 54                    0            0          NA
#> 55                    0            0          NA
#> 56                    0            0          NA
#> 57                    0            1         351
#> 58                    0            0          NA
#> 59                    0            0          NA
#> 60                    0            0          NA
#> 61                    0            1         165
#> 62                    0            0          NA
#> 63                    0            0          NA
#> 64                    0            0          NA
#> 65                    0            1          47
#> 66                    0            0          NA
#> 67                    0            0          NA
#> 68                    0            0          NA
#> 69                    0            1          56
#> 70                    0            0          NA
#> 71                    0            1          70
#> 72                    0            0          NA
#> 73                    0            0          NA
#> 74                    0            0          NA
#> 75                    0            0          NA
#> 76                    0            0          NA
#> 77                    0            0          NA
#> 78                    0            0          NA
#> 79                    0            0          NA
#> 80                    0            1          16
#> 81                    0            1          10
#> 82                    0            0          NA
#> 83                    0            1         255
#> 84                    0            0          NA
#> 85                    0            0          NA
#> 86                    0            0          NA
#> 87                    0            0          NA
#> 88                    0            0          NA
#> 89                    0            0          NA
#> 90                    0            0          NA
#> 91                    0            0          NA
#> 92                    0            0          NA
#> 93                    0            0          NA
#> 94                    0            0          NA
#> 95                    0            1         201
#> 96                    0            0          NA
#> 97                    0            0          NA
#> 98                    0            0          NA
#> 99                    0            0          NA
#> 100                   0            0          NA
```
