

test_that('KNN fit works', {
  modelSettings = setKNN(k = 2)
  nanoTrainData <- reduceTrainData(tinyTrainData, n = 2)
  subjectToKeep <- nanoTrainData$labels[sample.int(nrow(nanoTrainData$labels), 50),"rowId"]
  nanoTrainData$labels <- nanoTrainData$labels[nanoTrainData$labels$rowId %in% subjectToKeep,]
  nanoTrainData$folds <- nanoTrainData$folds[nanoTrainData$folds$rowId %in% subjectToKeep,]
  nanoTrainData$covariateData$covariates <- nanoTrainData$covariateData$covariates %>% dplyr::filter(.data$rowId %in% subjectToKeep)
  plpModel <- fitPlp(
    trainData = nanoTrainData, 
    modelSettings = modelSettings,
    analysisId = 'KNN',
    analysisPath = tempdir()
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
