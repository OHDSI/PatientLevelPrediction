# createRestrictPlpDataSettings define extra restriction settings when calling getPlpData

This function creates the settings used to restrict the target cohort
when calling getPlpData

## Usage

``` r
createRestrictPlpDataSettings(
  studyStartDate = "",
  studyEndDate = "",
  firstExposureOnly = FALSE,
  washoutPeriod = 0,
  sampleSize = NULL
)
```

## Arguments

- studyStartDate:

  A calendar date specifying the minimum date that a cohort index date
  can appear. Date format is 'yyyymmdd'.

- studyEndDate:

  A calendar date specifying the maximum date that a cohort index date
  can appear. Date format is 'yyyymmdd'. Important: the study end data
  is also used to truncate risk windows, meaning no outcomes beyond the
  study end date will be considered.

- firstExposureOnly:

  Should only the first exposure per subject be included? Note that this
  is typically done in the `createStudyPopulation` function, but can
  already be done here for efficiency reasons.

- washoutPeriod:

  The mininum required continuous observation time prior to index date
  for a person to be included in the at risk cohort. Note that this is
  typically done in the `createStudyPopulation` function, but can
  already be done here for efficiency reasons.

- sampleSize:

  If not NULL, the number of people to sample from the target cohort

## Value

A setting object of class `restrictPlpDataSettings` containing a list of
the settings:

- `studyStartDate`: A calendar date specifying the minimum date that a
  cohort index date can appear

- `studyEndDate`: A calendar date specifying the maximum date that a
  cohort index date can appear

- `firstExposureOnly`: Should only the first exposure per subject be
  included

- `washoutPeriod`: The mininum required continuous observation time
  prior to index date for a person to be included in the at risk cohort

- `sampleSize`: If not NULL, the number of people to sample from the
  target cohort

## Details

Users need to specify the extra restrictions to apply when downloading
the target cohort

## Examples

``` r
# restrict to 2010, first exposure only, require washout period of 365 day
# and sample 1000 people
createRestrictPlpDataSettings(studyStartDate = "20100101", studyEndDate = "20101231", 
firstExposureOnly = TRUE, washoutPeriod = 365, sampleSize = 1000)
#> $studyStartDate
#> [1] "20100101"
#> 
#> $studyEndDate
#> [1] "20101231"
#> 
#> $firstExposureOnly
#> [1] TRUE
#> 
#> $washoutPeriod
#> [1] 365
#> 
#> $sampleSize
#> [1] 1000
#> 
#> attr(,"class")
#> [1] "restrictPlpDataSettings"
```
