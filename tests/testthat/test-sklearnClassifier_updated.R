

test_that("DecisionTree settings work checks", {
  
dtset <- setDecisionTree(
  criterion = list('gini'),
  splitter = list('best'),
  maxDepth = list(4, 10, NULL),
  minSamplesSplit = list(2, 10),
  minSamplesLeaf = list(10, 50),
  minWeightFractionLeaf = list(0),
  maxFeatures = list(100,'auto', NULL),
  maxLeafNodes = list(NULL),
  minImpurityDecrease = list(10^-7),
  classWeight = list(NULL, 'balanced'),
  seed = sample(1000000,1)
)

expect_equal(dtset$fitFunction, "fitSklearn")

expect_equal(length(dtset$param), 3*2*2*3*2)

expect_equal(unique(unlist(lapply(dtset$param, function(x) x[[1]]))), 'gini')
expect_equal(unique(unlist(lapply(dtset$param, function(x) x[[2]]))), 'best')
expect_equal(length(unique(lapply(dtset$param, function(x) x[[3]]))), 3)

expect_false(attr(dtset$param, 'settings')$requiresDenseMatrix)
expect_equal(attr(dtset$param, 'settings')$name, 'Decision Tree')
expect_equal(attr(dtset$param, 'settings')$pythonImport, 'sklearn')
expect_equal(attr(dtset$param, 'settings')$pythonImportSecond, 'tree')
expect_equal(attr(dtset$param, 'settings')$pythonClassifier, "DecisionTreeClassifier")


})


test_that("DecisionTree errors as expected", {
  
  expect_error(setDecisionTree(criterion = list('madeup')))
  
  expect_error(setDecisionTree(maxDepth =  list(-1)))
  expect_error(setDecisionTree(minSamplesSplit =  list(-1)))
  expect_error(setDecisionTree(minSamplesLeaf  =  list(-1)))
  
})


test_that("check fit of DecisionTree", {
  
  
  modelSettings <- setDecisionTree(
    criterion = list('gini'),
    splitter = list('best'),
    maxDepth = list(as.integer(4)),
    minSamplesSplit = list(2),
    minSamplesLeaf = list(10),
    minWeightFractionLeaf = list(0),
    maxFeatures = list(as.integer(100),'auto'),
    maxLeafNodes = list(NULL),
    minImpurityDecrease = list(10^-7),
    classWeight = list(NULL, 'balanced'),
    seed = sample(1000000,1)
  )
  trainData <- createTrainData(
    plpData = plpData, 
    population = population
    )
  
  plpModel <- fitPlp(
    trainData = trainData, 
    modelSettings = modelSettings,
    analysisId = 'DecisionTree'
    )
  
  expect_equal(nrow(trainData$labels)*2, nrow(plpModel$prediction))
  expect_equal(length(unique(plpModel$prediction$evaluationType)), 2)

  expect_true(nrow(plpModel$covariateImportance) < trainData$covariateData$covariateRef %>% dplyr::tally() %>% dplyr::pull())
  
  expect_true(dir.exists(plpModel$model))
  expect_equal(dir(plpModel$model),"model.json")
  
  expect_equal(plpModel$trainDetails$outcomeId,2)
  expect_equal(plpModel$trainDetails$cohortId,1)
  
})

# add tests for other classifiers

