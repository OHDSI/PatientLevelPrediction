library("testthat")
library("pROC")

test_that("AUC", {
  prediction = runif(100)
  status = round(runif(100))
  rocobj <- roc(status, prediction, algorithm = 3)
  goldStandard <- as.numeric(ci(rocobj, method = "delong"))
  auc <- .Call("PatientLevelPrediction_auc",
               PACKAGE = "PatientLevelPrediction",
               prediction,
               status)
  
  aucWithCi <- .Call("PatientLevelPrediction_aucWithCi",
                     PACKAGE = "PatientLevelPrediction",
                     prediction,
                     status)
  if ((auc < 0.5) != (goldStandard[2] < 0.5)) {
    auc <- 1 - auc
    aucWithCi <- c(1 - aucWithCi[1], 1 - aucWithCi[3], 1 - aucWithCi[2])
  }
  tolerance <- 0.001
  expect_equal(goldStandard[2], auc, tolerance = tolerance)
  expect_equal(goldStandard[2], as.numeric(aucWithCi[1]), tolerance = tolerance)
  expect_equal(goldStandard[1], as.numeric(aucWithCi[2]), tolerance = tolerance)
  expect_equal(goldStandard[3], as.numeric(aucWithCi[3]), tolerance = tolerance)
})

