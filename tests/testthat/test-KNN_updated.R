
resultNames <- c('executionSummary','model','prediction', 'performanceEvaluation', 'covariateSummary', 'analysisRef')

plpResultKNN <- runPlp(
  plpData = plpData, 
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

test_that("covRef is correct size", {
  
  testthat::expect_true(nrow(as.data.frame(plpData$covariateData$covariateRef)) >=  
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
