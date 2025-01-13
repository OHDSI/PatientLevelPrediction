test_that("createSimpleImputer works", {
  imputer <- createSimpleImputer()

  expect_equal(imputer$method, "mean")
  expect_equal(imputer$missingThreshold, 0.3)
  expect_equal(attr(imputer, "fun"), "simpleImpute")

  imputer <- createSimpleImputer(
    method = "median",
    missingThreshold = 0.5
  )
  expect_equal(imputer$method, "median")
  expect_equal(imputer$missingThreshold, 0.5)

  expect_error(createSimpleImputer(method = "mean", missingThreshold = -1))
  expect_error(createSimpleImputer(method = "mean", missingThreshold = "0.5"))
  expect_error(createSimpleImputer(method = "mean", missingThreshold = 1))
  expect_error(createSimpleImputer(method = "notMean"))
})

test_that("createIterativeImputer works", {
  imputer <- createIterativeImputer()

  expect_equal(imputer$method, "pmm")
  expect_error(createIterativeImputer(method = "notPmm"))
  expect_equal(attr(imputer, "fun"), "iterativeImpute")

  expect_error(createIterativeImputer(method = "pmm", missingThreshold = -1))
  expect_error(createIterativeImputer(method = "pmm", missingThreshold = "0.5"))
  expect_error(createIterativeImputer(method = "pmm", missingThreshold = 1))

  imputer <- createIterativeImputer(
    method = "pmm",
    missingThreshold = 0.5
  )
  expect_equal(imputer$missingThreshold, 0.5)

})
