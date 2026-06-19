# Configure a Python environment manually

Configure a Python environment manually

## Usage

``` r
configurePython(envname = "PLP", envtype = NULL, condaPythonVersion = "3.11")
```

## Arguments

- envname:

  A string for the name of the virtual environment (default is 'PLP')

- envtype:

  An option for specifying the environment as 'conda' or 'python'. If
  NULL then the default is 'conda' for Windows users and 'python' for
  non-Windows users

- condaPythonVersion:

  String, Python version to use when creating a conda environment

## Value

Location of the created conda or virtual Python environment

## Details

PatientLevelPrediction normally lets `reticulate` manage Python
requirements automatically when Python-backed models are used. This
helper is retained for users who need to create a conda or virtualenv
environment manually, for example in offline or locked-down
environments.

## Examples
