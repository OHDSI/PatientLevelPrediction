
resultNames <- c('executionSummary','model','prediction', 'performanceEvaluation', 'covariateSummary', 'analysisRef')

# reduce data so test runs quicker, still with at least 10 outcomes in test
plpDataKNN <- plpData
plpData$population <- plpData$cohorts[sample(nrow(plpData$cohorts), 400),]

# will fit 100% on training data which produces a warning
suppressWarnings({
plpResultKNN <- runPlp(
  plpData = plpDataKNN, 
  outcomeId = 2, 
  analysisId = 'knnTest', 
  analysisName = 'Testing knn',
  populationSettings = populationSettings, 
  splitSettings = createDefaultSplitSetting(),
  preprocessSettings = createPreprocessSettings(), 
  modelSettings = setKNN(k=10), 
  logSettings = createLogSettings(verbosity = 'TRACE'),
  executeSettings = createDefaultExecuteSettings(), 
  saveDirectory = file.path(saveLoc, 'knn')
  )
})

test_that("covRef is correct size", {
  
  testthat::expect_true(nrow(plpData$covariateData$covariateRef %>% dplyr::collect()) >=  
    nrow(plpResultKNN$model$covariateImportance))
  
})


test_that("KNN results have correct structure", {
  
  
  # same output names for LR, KNN and GBM
  testthat::expect_equal(
    names(plpResultKNN),
    resultNames
    )
  
})

test_that("KNN settings", {

model_set <- setKNN(k=5)
testthat::expect_is(model_set, "modelSettings")
testthat::expect_length(model_set,2)
testthat::expect_error(setKNN(k = 0))
testthat::expect_error(setKNN(indexFolder = 2372))
})
