# Contributing to PatientLevelPrediction

Thanks for helping improve PatientLevelPrediction (PLP). PLP is part of
[HADES](https://ohdsi.github.io/Hades/) and follows the
[HADES Developer Guidelines](https://ohdsi.github.io/Hades/developerGuidelines.html)
and the [OHDSI Code Style](https://ohdsi.github.io/Hades/codeStyle.html). This
document repeats the rules that matter most and adds what is specific to PLP.

## Before you write code

- **Start from an issue.** Find or open an issue and agree on the approach with
  a maintainer in that issue before opening a pull request. Typo and small
  documentation fixes are the exception.
- Say in the issue that you are working on it, so work is not duplicated.

## Pull request rules

- **Target the `develop` branch.** `main` only receives release merges.
  Pull requests against `main` will be retargeted or closed.
- **One issue per pull request.** Do not reformat, restyle or "improve" code
  unrelated to the issue, including whitespace. Open a separate issue instead.
- **Fix the problem everywhere it occurs**, not only where the issue reported
  it. Search the code base for other places that produce or consume the same
  object (see [Things that must stay in sync](#things-that-must-stay-in-sync)).
- **Add tests** that fail without your change and pass with it.
- **Add a `NEWS.md` entry** under the `PatientLevelPrediction development`
  heading, ending with the issue number, e.g. `(#571)`.
- **Pass R check with no errors, warnings or NOTEs** (`devtools::check()`,
  which runs with `--as-cran`). NOTEs block CRAN submission, and CI only fails
  on warnings, so check for NOTEs yourself.
- **Describe the change** in the pull request: what changed, why, and which
  tests you ran with their result. Link the issue with `Fixes #NNN`.
- To update a pull request, push to the same branch. Do not close it and open
  a new one.

## Use of AI tools

You may use AI coding tools, but you are responsible for the result:

- You must understand every line you submit and be able to answer review
  questions about it.
- Run the tests yourself and report the result. Do not submit code you have
  not run.
- Say in the pull request description that AI tools were used and for what.

The rules above apply in full to AI-assisted pull requests. Pull requests that
ignore them will be closed.

## Development setup

- **R version:** PLP supports R >= 4.1 and CI checks the minimum dependency
  versions on R 4.1.3. Do not use language features newer than R 4.1 (for
  example the `_` placeholder for the native pipe). Do not raise minimum
  dependency versions in `DESCRIPTION` without discussing it in the issue.
- **Dependencies:** `devtools::install_deps(dependencies = TRUE)`.
- **Python:** scikit-learn models run through `reticulate`. The test setup
  calls `reticulate::py_require("scikit-learn")`, which creates a Python
  environment automatically. Python tests only run when `NOT_CRAN=true`, which
  `devtools::test()` sets.
- **Test data:** most tests use the [Eunomia](https://github.com/OHDSI/Eunomia)
  example database, which is downloaded on first use and needs internet access.

## Running tests

```r
devtools::test()                       # all tests
devtools::test(filter = "extractData") # tests/testthat/test-extractData.R only
```

`tests/testthat/setup.R` extracts Eunomia data and fits a model before any test
runs, so even a single test file takes a few minutes. It creates shared objects
such as `plpData`, `plpResult`, `population`, `trainData`, `testData` and
`tinyPlpData`; reuse them instead of extracting new data. Helpers for building
small data sets are in `tests/testthat/helper-functions.R`.

Guard tests with `skip_if_offline()` when they need Eunomia or other downloads,
and with `skip_if_not_installed()` when they need a package from `Suggests`.

## Code style

- Follow the [OHDSI Code Style](https://ohdsi.github.io/Hades/codeStyle.html):
  camelCase names, `<-` for assignment, named arguments in function calls.
- Call functions from other packages with `package::function()`. Never call
  `library()` or `require()` in package code.
- Do not add packages to `Imports` without discussion. Optional packages go in
  `Suggests` and are checked with `rlang::check_installed()` before use.
- Log with `ParallelLogger::logInfo()`, `logWarn()`, `logTrace()` and so on,
  not `print()`, `cat()` or `message()`.

## Documentation

- Functions are documented with roxygen2 comments in `R/`. Run
  `devtools::document()` after changing them. Do not edit `man/` or `NAMESPACE`
  by hand.
- New exported functions need a runnable `@examples` section that works
  offline (for example with `simulatePlpData()`) and must be added to a section
  of `_pkgdown.yml`, or the website build fails.

## Things that must stay in sync

These parts of PLP are built or read in several places. A change to one place
usually needs the same change in the others.

- **`plpData` objects** are created by `getPlpData()` (`R/ExtractData.R`),
  `simulatePlpData()` and `simulatePlpBenchmarkData()` (`R/Simulation.R`) and
  `loadPlpData()` (`R/SaveLoadPlp.R`), and written by `savePlpData()`. A change
  to their structure needs all of these updated and a save/load round-trip test.
- **Saved models:** `savePlpModel()` and `loadPlpModel()` (`R/SaveLoadPlp.R`)
  must round-trip every model type. scikit-learn models are stored as JSON by
  `sklearnToJson()` and `sklearnFromJson()` (`R/SklearnToJson.R`), not pickle,
  so a new scikit-learn classifier or fitted attribute needs serialization code
  and a test that predictions match after loading.
- **Model settings:** `set*()` functions (`R/CyclopsSettings.R`,
  `R/SklearnClassifier.R`, `R/LightGBM.R`, ...) return a `modelSettings` object
  naming its fit function. Follow an existing model of the same kind rather than
  inventing a new structure.
- **Results data model:** tables are defined in
  `inst/settings/resultsDataModelSpecification.csv`. A schema change needs a new,
  sequentially numbered migration in each of
  `inst/sql/{sql_server,postgresql,sqlite}/migrations/`, plus matching changes to
  the upload code in `R/uploadToDatabase*.R`.
- **SQL** in `inst/sql/sql_server/` is written in SQL Server dialect and
  translated with SqlRender, so it must work on all supported database
  platforms.
