
resultNames <- c('prediction', '', ...)

plpResultKNN <- runPlp(population = population,
  plpData = plpData, 
  modelSettings = knnSet, 
  savePlpData = F, 
  savePlpResult = F, 
  saveEvaluation = F, 
  savePlpPlots = F, 
  analysisId = 'knnTest',
  saveDirectory =  saveLoc)


test_that("covRef is correct size", {
  
  testthat::expect_equal(nrow(as.data.frame(plpData$covariateData$covariateRef)), 
    nrow(plpResultKNN$model$varImp))
  
})


test_that("KNN results have correct structure", {
  
  
  # same output names for LR, KNN and GBM
  testthat::expect_equal(
    names(plpResultKNN),
    resultNames
    )
  
})

test_that("KNN settings", {

model_set <- setKNN()
testthat::expect_that(model_set, is_a("modelSettings"))
testthat::expect_length(model_set,3)
testthat::expect_error(setKNN(k = 0))
testthat::expect_error(setKNN(indexFolder = 2372))
})