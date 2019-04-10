#Sys.setenv("R_TESTS" = "")
#options(fftempdir = file.path(getwd(),'fftemp'))
library(testthat)
test_check("PatientLevelPrediction")
