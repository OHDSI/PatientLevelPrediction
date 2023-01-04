# common tests that can be grouped together, such as the output from fitplp

expect_correct_fitPlp <- function(plpModel, trainData, outputRisk=TRUE) {
  
  # predictions are same amount as labels
  expect_equal(nrow(trainData$labels)*2, nrow(plpModel$prediction))
  
  # get predictions both for train and CV
  expect_equal(length(unique(plpModel$prediction$evaluationType)), 2)

  # predictions are all between 0 and 1
  if (outputRisk) {
    expect_true(all((plpModel$prediction$value>=0) & (plpModel$prediction$value<1)))
  }
  else {
    expect_true(all((plpModel$prediction$value==0) | (plpModel$prediction$value==1)))
  }
  
  expect_true(nrow(plpModel$covariateImportance) < 
              trainData$covariateData$covariateRef %>% 
              dplyr::tally() %>% 
              dplyr::pull())
  
  expect_true(dir.exists(plpModel$model))
  expect_equal(dir(plpModel$model),"model.pkl")
  
  expect_equal(plpModel$modelDesign$outcomeId,2)
  expect_equal(plpModel$modelDesign$targetId,1)
}
