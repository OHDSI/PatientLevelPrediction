# AGENTS.md

Instructions for AI coding agents working on PatientLevelPrediction (PLP), an
R package in OHDSI HADES.

**Before changing any code, read `.github/CONTRIBUTING.md` in full.** It holds
the contribution rules, the development setup and the list of code paths that
must stay in sync. The rules below are repeated from it because they are the
ones most often broken.

## Hard rules

- Base your branch on `develop` and target pull requests at `develop`, never
  `main`.
- Work on one existing issue. If the approach has not been agreed with a
  maintainer in the issue, say so to the user instead of opening a pull request.
- Change only what the issue needs. No reformatting, restyling or whitespace
  changes in unrelated code.
- Fix the problem in every place it occurs. Before finishing, search `R/` for
  other functions that create, save, load or consume the same object or field,
  and check the "Things that must stay in sync" section of
  `.github/CONTRIBUTING.md`.
- Add a test that fails without the change, and a `NEWS.md` bullet under
  `PatientLevelPrediction development` ending with the issue number.
- Run the relevant tests and report the actual result. Never claim tests pass
  without running them.
- State in the pull request description that AI tools were used.

## Commands

```r
devtools::install_deps(dependencies = TRUE)
devtools::document()                   # after changing roxygen comments
devtools::test(filter = "extractData") # one test file; setup.R still runs first
devtools::test()                       # all tests
devtools::check()                      # must give 0 errors, 0 warnings, 0 NOTEs
```

`tests/testthat/setup.R` downloads the Eunomia database and fits a model before
any test runs, so even one test file takes a few minutes and needs internet
access. Reuse the shared objects it creates (`plpData`, `plpResult`,
`trainData`, `tinyPlpData`, ...) instead of extracting new data.

## Code conventions

- Must run on R 4.1. No `_` pipe placeholder or other features newer than R 4.1.
- OHDSI code style: camelCase, `<-`, named arguments, `package::function()`
  calls, no `library()` in package code.
- No new `Imports`. Optional packages go in `Suggests` and are checked with
  `rlang::check_installed()`.
- Log with `ParallelLogger::log*()`, not `print()`, `cat()` or `message()`.
- Never edit `man/` or `NAMESPACE` by hand. New exported functions must be
  added to `_pkgdown.yml`.
