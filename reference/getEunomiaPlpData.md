# Create a plpData object from the Eunomia database'

This function creates a plpData object from the Eunomia database. It
gets the connection details, creates the cohorts, and extracts the data.
The cohort is predicting GIbleed in new users of celecoxib.

## Usage

``` r
getEunomiaPlpData(covariateSettings = NULL)
```

## Arguments

- covariateSettings:

  A list of covariateSettings objects created using the
  `createCovariateSettings` function in the `FeatureExtraction` package.
  If nothing is specified covariates with age, gender, conditions and
  drug era are used.

## Value

An object of type `plpData`, containing information on the cohorts,
their outcomes, and baseline covariates. Information about multiple
outcomes can be captured at once for efficiency reasons. This object is
a list with the following components:

- outcomes:

  A data frame listing the outcomes per person, including the time to
  event, and the outcome id

- cohorts:

  A data frame listing the persons in each cohort, listing their
  exposure status as well as the time to the end of the observation
  period and time to the end of the cohort

- covariateData:

  An Andromeda object created with the `FeatureExtraction` package. This
  object contains the following items:

  covariates

  :   An Andromeda table listing the covariates per person in the two
      cohorts. This is done using a sparse representation: covariates
      with a value of 0 are omitted to save space. Usually has three
      columns, rowId, covariateId and covariateValue'.

  covariateRef

  :   An Andromeda table describing the covariates that have been
      extracted.

  AnalysisRef

  :   An Andromeda table with information about which analysisIds from
      'FeatureExtraction' were used.

## Examples
