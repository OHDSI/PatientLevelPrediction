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

  A covariate map (telling us the column number for covariates), with
  columns covariateId and columnId, for example from a fitted model.
  Covariates that are not in the map are left out.

## Value

A list with the following components:

- dataMatrix:

  A sparse matrix with a row for each person in the cohort, in the order
  of the cohort, and a column for each covariate in the map.

- labels:

  The cohort, with rowId replaced by the matrix row and the original
  rowId in originalRowId.

- covariateRef:

  The covariateRef of the mapped covariates, with their columnId.

- covariateMap:

  A data.frame with the covariateId and columnId of each column.

## Details

This function converts the covariates `Andromeda` table in COO format
into a sparse matrix from the package Matrix. The matrix is built in one
query on the covariates, without copying them.

## Examples
