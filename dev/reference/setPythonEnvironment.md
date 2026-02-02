# Use the python environment created using configurePython()

Use the python environment created using configurePython()

## Usage

``` r
setPythonEnvironment(envname = "PLP", envtype = NULL)
```

## Arguments

- envname:

  A string for the name of the virtual environment (default is 'PLP')

- envtype:

  An option for specifying the environment as'conda' or 'python'. If
  NULL then the default is 'conda' for windows users and 'python' for
  non-windows users

## Value

A string indicating the which python environment will be used

## Details

This function sets PatientLevelPrediction to use a python environment

## Examples
