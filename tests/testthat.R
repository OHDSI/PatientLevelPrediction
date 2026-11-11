library(testthat)
library(PatientLevelPrediction)
# on CRAN (NOT_CRAN not set) the tests run in one process; set TESTTHAT_PARALLEL to choose
if (!identical(Sys.getenv("NOT_CRAN"), "true") && Sys.getenv("TESTTHAT_PARALLEL") == "") {
  Sys.setenv(TESTTHAT_PARALLEL = "FALSE")
}
test_check("PatientLevelPrediction")
