# common tests that can be grouped together, such as testing the output from fitplp

expect_correct_fitPlp <- function(plpModel, trainData) {
  
  # predictions are same amount as labels
  multiplicativeFactor <- dplyr::n_distinct(plpModel$prediction %>% dplyr::pull(evaluationType))
  expect_equal(NROW(trainData$labels)*multiplicativeFactor, NROW(plpModel$prediction))
  
  # predictions are all between 0 and 1
  expect_true(all((plpModel$prediction$value>=0) & (plpModel$prediction$value<=1)))

  # model directory exists
  expect_true(dir.exists(plpModel$model))
  
  expect_equal(plpModel$modelDesign$outcomeId,2)
  expect_equal(plpModel$modelDesign$targetId,1)
  
  # structure of plpModel is correct
  expect_equal(names(plpModel), c("model", "preprocessing", "prediction",
                                  "modelDesign", "trainDetails", "covariateImportance"))
}
