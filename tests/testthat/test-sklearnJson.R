

sklearn <- reticulate::import('sklearn', convert=FALSE)
np <- reticulate::import('numpy', convert=FALSE)

data <- sklearn$datasets$make_classification(n_samples=500L, n_features=3L, 
                                            n_classes=2L, n_informative=3L, 
                                            n_redundant=0L, random_state=0L, 
                                            shuffle=FALSE)

X_unseen <- sklearn$datasets$make_classification(n_samples=100L, n_features=3L, 
                                                 n_classes=2L, n_informative=3L, 
                                                 n_redundant=0L, random_state=42L, 
                                                 shuffle=FALSE)[[0]]
X <- data[[0]]
y <- data[[1]]

test_that("Decision tree to json is correct", {
  classifier <- sklearn$tree$DecisionTreeClassifier(max_depth=3L)
  
  model <- classifier$fit(X,y)
  predictions <- reticulate::py_to_r(model$predict_proba(X_unseen))
  path <- file.path(tempdir(),"model.json")
  
  sklearnToJson(model, path)
  
  loadedModel <- sklearnFromJson(path)
  
  loadedPredictions  <- reticulate::py_to_r(loadedModel$predict_proba(X_unseen))
  
  expect_true(all.equal(predictions, loadedPredictions))
})

test_that("Random forest to json is correct", {
  classifier <- sklearn$ensemble$RandomForestClassifier(n_estimators=10L)
  
  model <- classifier$fit(X,y)
  predictions <- reticulate::py_to_r(model$predict_proba(X_unseen))
  path <- file.path(tempdir(),"model.json")
  
  sklearnToJson(model, path)
  
  loadedModel <- sklearnFromJson(path)
  
  loadedPredictions  <- reticulate::py_to_r(loadedModel$predict_proba(X_unseen))
  
  expect_true(all.equal(predictions, loadedPredictions))
})

test_that("Adaboost to json is correct", {
  classifier <- sklearn$ensemble$AdaBoostClassifier(n_estimators=10L)
  
  model <- classifier$fit(X,y)
  predictions <- reticulate::py_to_r(model$predict_proba(X_unseen))
  path <- file.path(tempdir(), "model.json")
  
  sklearnToJson(model, path)
  
  loadedModel <- sklearnFromJson(path)
  
  loadedPredictions  <- reticulate::py_to_r(loadedModel$predict_proba(X_unseen))
  
  expect_true(all.equal(predictions, loadedPredictions))
})

test_that("Naive Bayes to json is correct", {
  classifier <- sklearn$naive_bayes$GaussianNB()
  
  model <- classifier$fit(X,y)
  predictions <- reticulate::py_to_r(model$predict_proba(X_unseen))
  path <- file.path(tempdir(), "model.json")
  
  sklearnToJson(model, path)
  
  loadedModel <- sklearnFromJson(path)
  
  loadedPredictions  <- reticulate::py_to_r(loadedModel$predict_proba(X_unseen))
  
  expect_true(all.equal(predictions, loadedPredictions))
})

test_that("MLP to json is correct", {
  # lower tolerance to not get convergence warning
  classifier <- sklearn$neural_network$MLPClassifier(tol=1e-2)
  
  model <- classifier$fit(X,y)
  predictions <- reticulate::py_to_r(model$predict_proba(X_unseen))
  path <- file.path(tempdir(), "model.json")
  
  sklearnToJson(model, path)
  
  loadedModel <- sklearnFromJson(path)
  
  loadedPredictions  <- reticulate::py_to_r(loadedModel$predict_proba(X_unseen))
  
  expect_true(all.equal(predictions, loadedPredictions))
})

test_that("SVM to json is correct", {
  classifier <- sklearn$svm$SVC(probability=TRUE)
  
  # create sparse data because then some of the internal fields in the 
  # SVM will be sparse
  feature_hasher <- sklearn$feature_extraction$FeatureHasher(n_features=3L)
  random <- reticulate::import("random", convert=FALSE)
  features <- list()
  y_sparse <- np$empty(100L)
  for (i in 1:100) {
    row <- reticulate::dict(a=random$randint(0,2),
                            b=random$randint(3,5),
                            c=random$randint(6,8))
    features <- c(features, row)
    reticulate::py_set_item(y_sparse, i - 1L, random$randint(0, 2))
  } 
  X_sparse <- feature_hasher$transform(features)
  
  model <- classifier$fit(X_sparse,y_sparse)
  predictions <- reticulate::py_to_r(model$predict_proba(X_unseen))
  path <- file.path(tempdir(), "model.json")
  
  sklearnToJson(model, path)
  
  loadedModel <- sklearnFromJson(path)
  
  loadedPredictions  <- reticulate::py_to_r(loadedModel$predict_proba(X_unseen))
  
  expect_true(all.equal(predictions, loadedPredictions))
})



