# Convert the plpData in COO format into a sparse R matrix

Converts the standard plpData to a sparse matrix

## Usage

``` r
toSparseM(plpData, cohort = NULL, map = NULL)
```

## Arguments

- plpData:

  An object of type `plpData` with covariate in coo format - the patient
  level prediction data extracted from the CDM.

- cohort:

  If specified the plpData is restricted to the rowIds in the cohort
  (otherwise plpData\$labels is used)

- map:

  A covariate map (telling us the column number for covariates)

## Value

Returns a list, containing the data as a sparse matrix, the plpData
covariateRef and a data.frame named map that tells us what covariate
corresponds to each column This object is a list with the following
components:

- data:

  A sparse matrix with the rows corresponding to each person in the
  plpData and the columns corresponding to the covariates.

- covariateRef:

  The plpData covariateRef.

- map:

  A data.frame containing the data column ids and the corresponding
  covariateId from covariateRef.

## Details

This function converts the covariates `Andromeda` table in COO format
into a sparse matrix from the package Matrix

## Examples
