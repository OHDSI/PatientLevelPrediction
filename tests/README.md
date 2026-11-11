# Running the tests

The tests run in parallel: `Config/testthat/parallel: true` in `DESCRIPTION`
makes testthat run the test files in several R processes. Each process loads the
package and runs `setup.R`, so each builds the Eunomia fixtures once. The
slowest files are listed in `Config/testthat/start-first`, so they start first.
The processes share the Eunomia dataset in `EUNOMIA_DATA_FOLDER` and
reticulate's Python setup. If either doesn't exist yet, create it once first
(`Eunomia::getEunomiaConnectionDetails()`; `reticulate::py_require("scikit-learn")`
then `reticulate::import("sklearn")`), so the processes don't all create it at
the same time. CI does both before the tests.

testthat takes the number of processes from `getOption("Ncpus")`, then the
`TESTTHAT_CPUS` environment variable, and uses 2 otherwise. The CI workflow sets
`TESTTHAT_CPUS` to the number of cores. Locally you can set it, for example in
`~/.Renviron`:

```
TESTTHAT_CPUS=6
```

In the parallel processes, OpenMP, BLAS and DuckDB (`andromedaThreads`) use one
thread each, so the processes do not compete for the cores (`setup.R`).

On CRAN (`NOT_CRAN` not set, as in `R CMD check` without it) `tests/testthat.R`
runs the tests in one process, as before. Setting `TESTTHAT_PARALLEL` overrides
this. `devtools::test()` and the CI workflow (through `r-lib/actions/setup-r`) set
`NOT_CRAN`, so they run in parallel.

## When a parallel run fails

A test that fails or errors is listed at the end with its file, line and name,
as in one process. A test process that dies (for example in DuckDB, Java or
Python code) stops the whole run, and testthat names only the file. Run the
tests in one process to see the test, with every test up to the failure in the
output:

```r
Sys.setenv(TESTTHAT_PARALLEL = "FALSE")
devtools::test(filter = "featureEngineering")
```

When the tests fail on CI, the workflow runs them again in one process, so the
job log has that output. Coverage (`covr`) runs the tests in one process, as
code run in parallel processes would not be counted.
