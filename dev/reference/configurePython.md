# Sets up a python environment to use for PLP (can be conda or venv)

Sets up a python environment to use for PLP (can be conda or venv)

## Usage

``` r
configurePython(envname = "PLP", envtype = NULL, condaPythonVersion = "3.11")
```

## Arguments

- envname:

  A string for the name of the virtual environment (default is 'PLP')

- envtype:

  An option for specifying the environment as'conda' or 'python'. If
  NULL then the default is 'conda' for windows users and 'python' for
  non-windows users

- condaPythonVersion:

  String, Python version to use when creating a conda environment

## Value

location of the created conda or virtual python environment

## Details

This function creates a python environment that can be used by
PatientLevelPrediction and installs all the required package
dependancies.

## Examples
