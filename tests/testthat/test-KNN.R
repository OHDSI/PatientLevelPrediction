

test_that('KNN fit works', {
  modelSettings = setKNN(k=5)
  
  plpModel <- fitPlp(
    trainData = tinyTrainData, 
    modelSettings = modelSettings,
    analysisId = 'KNN'
  )
  
  expect_correct_fitPlp(plpModel, tinyTrainData) 
  
})


test_that("KNN settings", {

model_set <- setKNN(k=5)
testthat::expect_is(model_set, "modelSettings")
testthat::expect_length(model_set,2)
testthat::expect_error(setKNN(k = 0))
testthat::expect_error(setKNN(indexFolder = 2372))
})
