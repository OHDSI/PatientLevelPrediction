# Adding Custom Patient-Level Prediction Algorithms

## Introduction

This vignette describes how you can add your own custom algorithms in
the Observational Health Data Sciences and Informatics (OHDSI)
[`PatientLevelPrediction`](https://github.com/OHDSI/PatientLevelPrediction)
package. This allows you to fully leverage the OHDSI
PatientLevelPrediction framework for model development and validation.
This vignette assumes you have read and are comfortable with building
single patient level prediction models as described in the
[`vignette('BuildingPredictiveModels')`](https://ohdsi.github.io/PatientLevelPrediction/articles/BuildingPredictiveModels.md).

**We invite you to share your new algorithms with the OHDSI community
through our [GitHub
repository](https://github.com/OHDSI/PatientLevelPrediction).**

## Algorithm Code Structure

A model is described by a set\<Name\> function that returns its
`modelSettings`. PatientLevelPrediction does the rest: `fitClassifier()`
prepares the data, runs the hyperparameter search with cross-validation,
fits the final model and builds the `plpModel`, and
[`predictPlp()`](https://ohdsi.github.io/PatientLevelPrediction/reference/predictPlp.md),
[`savePlpModel()`](https://ohdsi.github.io/PatientLevelPrediction/reference/savePlpModel.md)
and
[`loadPlpModel()`](https://ohdsi.github.io/PatientLevelPrediction/reference/loadPlpModel.md)
use the same settings later. The model itself supplies a few functions,
the hooks, which are named in its settings. The xgboost model in
`GradientBoostingMachine.R` is a complete example.

### Set

The set\<Name\> function takes the hyperparameter values to search and
returns a list of class `modelSettings` with:

- param - a named list of hyperparameter values;
  [`createHyperparameterSettings()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createHyperparameterSettings.md)
  decides how candidates are drawn from them (a grid by default)
- settings - the model’s settings and its hooks:
  - modelName, modelType (`"binary"`) and seed
  - prepareData - turns the training data into what train expects,
    usually `"toSparseM"` (a sparse matrix with one row per patient). If
    it has a `settings` argument, it also gets these settings.
  - train - fits the model for one set of hyperparameters
  - predict - predicts risks with a fitted model
  - variableImportance - gives the importance of each covariate
  - saveType - how the fitted model is saved: the name of a save handler
    (see below), `"RtoJson"` for a model that is an R list, or `"file"`
    for a model that is a folder of files

Each hook can be a function, the name of a function, or
`"pkg::function"`. A name is looked up in PatientLevelPrediction and
then on the search path, so plain names are meant for the package’s own
functions; `"pkg::function"` is an exported function of an installed
package, which is loaded but does not need to be attached. Use the
`"pkg::function"` form for hooks in your own package, so that saved
models still work in a new R session. All hooks are looked up before
fitting starts, so a misspelled one fails at once.

For example, a model called madeUp with two hyperparameters:

``` r

setMadeUp <- function(a = c(1, 4, 10), b = 2, seed = NULL) {
  # add input checks here...
  if (is.null(seed)) {
    seed <- sample(100000, 1)
  }

  param <- list(
    a = a,
    b = b
  )

  settings <- list(
    modelName = "Made Up",
    modelType = "binary",
    seed = seed,
    prepareData = "toSparseM",
    train = "myPackage::fitMadeUp",
    predict = "myPackage::predictMadeUp",
    variableImportance = "myPackage::varImpMadeUp",
    saveType = "myPackage::saveLoadMadeUp"
  )

  result <- list(
    param = param,
    settings = settings
  )
  class(result) <- "modelSettings"

  return(result)
}
```

### Train

The train hook fits the model for one candidate. It gets:

- dataMatrix - the prepared data, a sparse matrix with rows as patients
  and columns as features when prepareData is `"toSparseM"`
- labels - a data.frame where labels\$outcomeCount is the class label (0
  or 1) per patient
- hyperParameters - a named list with one value per hyperparameter
- settings - the settings from the set function, such as the seed

and returns the fitted model.

``` r

fitMadeUp <- function(dataMatrix, labels, hyperParameters, settings) {
  # set the seed for reproducibility
  set.seed(settings$seed)

  model <- madeUpModel(
    X = dataMatrix,
    Y = labels$outcomeCount,
    a = hyperParameters$a,
    b = hyperParameters$b
  )

  return(model)
}
```

An error in train or predict stops the search, so that problems in the
model’s code show up at once. When the model knows that a candidate
simply cannot be fitted, for example because it runs out of memory, it
can call
[`stopCandidate()`](https://ohdsi.github.io/PatientLevelPrediction/reference/stopCandidate.md)
instead. The search then warns, records the candidate with a missing
performance and the message, and goes on with the next one; it stops
only if every candidate is skipped.

``` r

fitMadeUp <- function(dataMatrix, labels, hyperParameters, settings) {
  model <- tryCatch(
    madeUpModel(X = dataMatrix, Y = labels$outcomeCount, a = hyperParameters$a),
    madeUpOutOfMemory = function(e) stopCandidate("out of memory")
  )
  return(model)
}
```

### Predict

The predict hook gets:

- plpModel - during the search the model returned by train, and after
  fitting the `plpModel`
- data - during the search a part of the prepared data, and when
  predicting on new data a `plpData` object
- cohort - the patients to predict for

It returns the cohort with a column `value` holding the predicted risks.
The rows must keep the original `rowId`s:
[`toSparseM()`](https://ohdsi.github.io/PatientLevelPrediction/reference/toSparseM.md)
numbers the matrix rows and keeps the original in `originalRowId`.

``` r

predictMadeUp <- function(plpModel, data, cohort) {
  if (inherits(data, "plpData")) {
    matrixObjects <- toSparseM(
      plpData = data,
      cohort = cohort,
      map = plpModel$covariateImportance %>%
        dplyr::select("columnId", "covariateId")
    )
    newData <- matrixObjects$dataMatrix
    cohort <- matrixObjects$labels
  } else {
    newData <- data
  }

  if (inherits(plpModel, "plpModel")) {
    model <- plpModel$model
  } else {
    model <- plpModel
  }

  prediction <- cohort
  prediction$value <- stats::predict(model, newData)

  # give back the original rowIds
  prediction <- prediction %>%
    dplyr::select(-"rowId") %>%
    dplyr::rename(rowId = "originalRowId")

  attr(prediction, "metaData") <- list(modelType = attr(plpModel, "modelType"))
  return(prediction)
}
```

As the madeUp model uses the standard R prediction, it could reuse
xgboost’s prediction function instead:
`predict = "PatientLevelPrediction::predictXgboost"`.

### Variable importance

The variableImportance hook gets the fitted model and a data.frame
called covariateMap with the covariateId and columnId of each column of
the feature matrix. It returns a data.frame with the columns
covariateId, covariateValue (the importance) and included (1 if the
model uses the covariate, otherwise 0). If it fails, the model is still
fitted, without importances.

``` r

varImpMadeUp <- function(model, covariateMap) {
  covariateMap$covariateValue <- madeUpImportance(model)[covariateMap$columnId]
  covariateMap$included <- as.numeric(covariateMap$covariateValue != 0)
  return(covariateMap[, c("covariateId", "covariateValue", "included")])
}
```

### Saving and loading

A save handler is a function that returns a list with:

- save - a function of the fitted model and a file path that writes the
  model
- load - a function of a file path that reads it back
- fileName - optionally, the name of the file (or folder) to write in
  the model folder, a plain name without a path; the default is
  `"model.json"`

``` r

saveLoadMadeUp <- function() {
  list(
    fileName = "model.rds",
    save = function(model, file) saveRDS(model, file),
    load = function(file) readRDS(file)
  )
}
```

[`savePlpModel()`](https://ohdsi.github.io/PatientLevelPrediction/reference/savePlpModel.md)
calls save with `file.path(modelFolder, fileName)` and
[`loadPlpModel()`](https://ohdsi.github.io/PatientLevelPrediction/reference/loadPlpModel.md)
calls load with the same path.

## Acknowledgments

Considerable work has been dedicated to provide the
`PatientLevelPrediction` package.

``` r

citation("PatientLevelPrediction")
```

    ## To cite PatientLevelPrediction in publications use:
    ## 
    ##   Reps JM, Schuemie MJ, Suchard MA, Ryan PB, Rijnbeek P (2018). "Design
    ##   and implementation of a standardized framework to generate and
    ##   evaluate patient-level prediction models using observational
    ##   healthcare data." _Journal of the American Medical Informatics
    ##   Association_, *25*(8), 969-975.
    ##   <https://doi.org/10.1093/jamia/ocy032>.
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Article{,
    ##     author = {J. M. Reps and M. J. Schuemie and M. A. Suchard and P. B. Ryan and P. Rijnbeek},
    ##     title = {Design and implementation of a standardized framework to generate and evaluate patient-level prediction models using observational healthcare data},
    ##     journal = {Journal of the American Medical Informatics Association},
    ##     volume = {25},
    ##     number = {8},
    ##     pages = {969-975},
    ##     year = {2018},
    ##     url = {https://doi.org/10.1093/jamia/ocy032},
    ##   }

**Please reference this paper if you use the PLP Package in your work:**

[Reps JM, Schuemie MJ, Suchard MA, Ryan PB, Rijnbeek PR. Design and
implementation of a standardized framework to generate and evaluate
patient-level prediction models using observational healthcare data. J
Am Med Inform Assoc.
2018;25(8):969-975.](https://dx.doi.org/10.1093/jamia/ocy032)

This work is supported in part through the National Science Foundation
grant IIS 1251151.
