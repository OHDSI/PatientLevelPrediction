# Select a manually configured Python environment

Select a manually configured Python environment

## Usage

``` r
setPythonEnvironment(envname = "PLP", envtype = NULL)
```

## Arguments

- envname:

  A string for the name of the virtual environment (default is 'PLP')

- envtype:

  An option for specifying the environment as 'conda' or 'python'. If
  NULL then the default is 'conda' for Windows users and 'python' for
  non-Windows users

## Value

A string indicating which Python environment will be used

## Details

This function sets PatientLevelPrediction to use a Python environment
created using
[`configurePython()`](https://ohdsi.github.io/PatientLevelPrediction/reference/configurePython.md)
or another manual environment setup.

## Examples
