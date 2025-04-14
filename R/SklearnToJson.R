# @file SklearnToJson.R
#
# Copyright 2023 Observational Health Data Sciences and Informatics
#
# This file is part of PatientLevelPrediction
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitatons under the License.
#
#' Saves sklearn python model object to json in path
#' @param     model a fitted sklearn python model object
#' @param     path  path to the saved model file
#' @return    nothing, saves the model to the path as json
#' @examplesIf rlang::is_installed("reticulate") && reticulate::py_module_available("sklearn")
#' \donttest{ 
#' sklearn <- reticulate::import("sklearn", convert = FALSE)
#' model <- sklearn$tree$DecisionTreeClassifier()
#' model$fit(sklearn$datasets$load_iris()$data, sklearn$datasets$load_iris()$target)
#' saveLoc <- file.path(tempdir(), "model.json")
#' sklearnToJson(model, saveLoc)
#' # the model.json is saved in the tempdir
#' dir(tempdir())
#' # clean up
#' unlink(saveLoc)
#' }
#' @export
sklearnToJson <- function(model, path) {
  py <- reticulate::import_builtins(convert = FALSE)
  json <- reticulate::import("json", convert = FALSE)
  if (inherits(model, "sklearn.tree._classes.DecisionTreeClassifier")) {
    serializedModel <- serializeDecisionTree(model)
  } else if (inherits(model, "sklearn.ensemble._forest.RandomForestClassifier")) {
    serializedModel <- serializeRandomForest(model)
  } else if (inherits(model, "sklearn.ensemble._weight_boosting.AdaBoostClassifier")) {
    serializedModel <- serializeAdaboost(model)
  } else if (inherits(model, "sklearn.naive_bayes.GaussianNB")) {
    serializedModel <- serializeNaiveBayes(model)
  } else if (inherits(model, "sklearn.neural_network._multilayer_perceptron.MLPClassifier")) {
    serializedModel <- serializeMLP(model)
  } else if (inherits(model, "sklearn.svm._classes.SVC")) {
    serializedModel <- serializeSVM(model)
  } else {
    stop("Unsupported model")
  }

  with(py$open(path, "w"), as = file, {
    json$dump(serializedModel, fp = file)
  })
  return(invisible())
}

#' Loads sklearn python model from json
#' @param     path  path to the model json file
#' @return    a sklearn python model object
#' @export
#' @examplesIf rlang::is_installed("reticulate") && reticulate::py_module_available("sklearn")
#' plpData <- getEunomiaPlpData()
#' modelSettings <- setDecisionTree(maxDepth = list(3), minSamplesSplit = list(2),
#'                                   minSamplesLeaf = list(1), maxFeatures = list(100))
#' saveLocation <- file.path(tempdir(), "sklearnFromJson")
#' results <- runPlp(plpData, modelSettings = modelSettings, saveDirectory = saveLocation)
#' # view save model
#' dir(results$model$model, full.names = TRUE)
#' # load into a sklearn object
#' model <- sklearnFromJson(file.path(results$model$model, "model.json"))
#' # max depth is 3 as we set in beginning
#' model$max_depth
#' # clean up
#' unlink(saveLocation, recursive = TRUE)
sklearnFromJson <- function(path) {
  py <- reticulate::import_builtins(convert = FALSE)
  json <- reticulate::import("json", convert = FALSE)
  with(py$open(path, "r"), as = file, {
    model <- json$load(fp = file)
  })
  if (reticulate::py_bool(model["meta"] == "decision-tree")) {
    model <- deSerializeDecisionTree(model)
  } else if (reticulate::py_bool(model["meta"] == "rf")) {
    model <- deSerializeRandomForest(model)
  } else if (reticulate::py_bool(model["meta"] == "adaboost")) {
    model <- deSerializeAdaboost(model)
  } else if (reticulate::py_bool(model["meta"] == "naive-bayes")) {
    model <- deSerializeNaiveBayes(model)
  } else if (reticulate::py_bool(model["meta"] == "mlp")) {
    model <- deSerializeMlp(model)
  } else if (reticulate::py_bool(model["meta"] == "svm")) {
    model <- deSerializeSVM(model)
  } else {
    stop("Unsupported model")
  }
  return(model)
}

serializeTree <- function(tree) {
  serializedTree <- tree$`__getstate__`()
  dtypes <- serializedTree["nodes"]$dtype

  serializedTree["nodes"] <- serializedTree["nodes"]$tolist()
  serializedTree["values"] <- serializedTree["values"]$tolist()

  return(list(serializedTree, dtypes))
}

deSerializeTree <- function(tree_dict, nFeatures, nClasses, nOutputs) {
  # TODO the below only works for tree_dict loaded from json, if not it
  for (i in 0:(length(tree_dict["nodes"]) - 1)) {
    reticulate::py_set_item(
      tree_dict["nodes"], i,
      reticulate::tuple(reticulate::py_to_r(tree_dict["nodes"][i]))
    )
  }

  names <- list("left_child", "right_child", "feature", "threshold", "impurity", "n_node_samples", "weighted_n_node_samples")
  if (length(tree_dict["nodes"][0]) == 8) {
    # model used sklearn>=1.3 which added a parameter
    names[[8]] <- "missing_go_to_left"
  }

  sklearn <- reticulate::import("sklearn")
  np <- reticulate::import("numpy", convert = FALSE)

  tree_dict["nodes"] <- np$array(tree_dict["nodes"],
    dtype = np$dtype(reticulate::dict(
      names = names,
      formats = tree_dict["nodes_dtype"]
    ))
  )
  tree_dict["values"] <- np$array(tree_dict["values"])

  Tree <- sklearn$tree$`_tree`$Tree(
    nFeatures,
    np$array(reticulate::tuple(nClasses),
      dtype = np$intp
    ),
    nOutputs
  )

  Tree$`__setstate__`(tree_dict)

  return(Tree)
}

serializeDecisionTree <- function(model) {
  tree <- serializeTree(model$tree_)
  dtypes <- tree[[2]]
  tree <- tree[[1]]
  py <- reticulate::import_builtins(convert = FALSE)
  serialized_model <- reticulate::dict(
    "meta" = "decision-tree",
    "feature_importances_" = model$feature_importances_$tolist(),
    "max_features_" = model$max_features_,
    "n_classes_" = py$int(model$n_classes_),
    "n_features_in_" = model$n_features_in_,
    "n_outputs_" = model$n_outputs_,
    "tree_" = tree,
    "classes_" = model$classes_$tolist(),
    "params" = model$get_params()
  )

  tree_dtypes <- list()
  for (i in 0:(length(dtypes) - 1)) {
    tree_dtypes <- c(tree_dtypes, dtypes[[i]]$str)
  }

  serialized_model["tree_"]["nodes_dtype"] <- tree_dtypes
  return(serialized_model)
}

deSerializeDecisionTree <- function(model_dict) {
  np <- reticulate::import("numpy", convert = FALSE)
  sklearn <- reticulate::import("sklearn", convert = FALSE)
  deserialized_model <- do.call(
    sklearn$tree$DecisionTreeClassifier,
    reticulate::py_to_r(model_dict["params"])
  )

  deserialized_model$classes_ <- np$array(model_dict["classes_"])
  deserialized_model$max_features_ <- model_dict["max_features_"]
  deserialized_model$n_classes_ <- model_dict["n_classes_"]
  deserialized_model$n_features_in <- model_dict["n_features_in_"]
  deserialized_model$n_outputs_ <- model_dict["n_outputs_"]

  tree <- deSerializeTree(
    model_dict["tree_"],
    model_dict["n_features_in_"],
    model_dict["n_classes_"],
    model_dict["n_outputs_"]
  )
  deserialized_model$tree_ <- tree

  return(deserialized_model)
}

serializeRandomForest <- function(model) {
  estimators <- list()
  for (i in 1:length(model$estimators_)) {
    estimators <- c(estimators, serializeDecisionTree(model$estimators_[i - 1]))
  }

  serialized_model <- reticulate::dict(
    "meta" = "rf",
    "max_depth" = model$max_depth,
    "min_samples_split" = model$min_samples_split,
    "min_samples_leaf" = model$min_samples_leaf,
    "min_weight_fraction_leaf" = model$min_weight_fraction_leaf,
    "max_features" = model$max_features,
    "max_leaf_nodes" = model$max_leaf_nodes,
    "min_impurity_decrease" = model$min_impurity_decrease,
    "min_impurity_split" = model$min_samples_split,
    "n_features_in_" = model$n_features_in_,
    "n_outputs_" = model$n_outputs_,
    "classes_" = model$classes_$tolist(),
    "estimators_" = reticulate::r_to_py(estimators),
    "params" = model$get_params(),
    "n_classes_" = model$n_classes_
  )

  if (reticulate::py_bool(model$`__dict__`["oob_score_"] != reticulate::py_none())) {
    serialized_model["oob_score_"] <- model$oob_score_
    serialized_model["oob_decision_function_"] <- model$oob_decision_function_$tolist()
  }

  return(serialized_model)
}

deSerializeRandomForest <- function(model_dict) {
  np <- reticulate::import("numpy", convert = FALSE)
  sklearn <- reticulate::import("sklearn", convert = FALSE)
  model <- do.call(
    sklearn$ensemble$RandomForestClassifier,
    reticulate::py_to_r(model_dict["params"])
  )

  estimators <- list()
  for (i in 1:length(model_dict$estimators_)) {
    estimators <- c(estimators, deSerializeDecisionTree(model_dict["estimators_"][i - 1]))
  }

  model$estimators_ <- np$array(estimators)

  model$classes_ <- np$array(model_dict["classes_"])
  model$n_features_in_ <- model_dict["n_features_in_"]
  model$n_outputs_ <- model_dict["n_outputs_"]
  model$max_depth <- model_dict["max_depth"]
  model$min_samples_split <- model_dict["min_samples_split"]
  model$min_samples_leaf <- model_dict["min_samples_leaf"]
  model$min_weight_fraction_leaf <- model_dict["min_weight_fraction_leaf"]
  model$max_features <- model_dict["max_features"]
  model$max_leaf_nodes <- model_dict["max_leaf_nodes"]
  model$min_impurity_decrease <- model_dict["min_impurity_decrease"]
  model$min_impurity_split <- model_dict["min_impurity_split"]
  model$n_classes_ <- model_dict["n_classes_"]

  if (reticulate::py_bool(model_dict$oob_score_ != reticulate::py_none())) {
    model$oob_score_ <- model_dict["oob_score_"]
    model$oob_decision_function_ <- model_dict["oob_decision_function_"]
  }
  return(model)
}

serializeAdaboost <- function(model) {
  estimators <- list()
  for (i in 1:length(model$estimators_)) {
    estimators <- c(estimators, serializeDecisionTree(model$estimators_[i - 1]))
  }
  serialized_model <- reticulate::dict(
    "meta" = "adaboost",
    "estimators_" = reticulate::r_to_py(estimators),
    "n_features_in_" = model$n_features_in_,
    "n_classes_" = model$n_classes_,
    "params" = model$get_params(),
    "classes_" = model$classes_$tolist(),
    "estimator_weights_" = model$estimator_weights_$tolist()
  )

  return(serialized_model)
}

deSerializeAdaboost <- function(model_dict) {
  np <- reticulate::import("numpy", convert = FALSE)
  sklearn <- reticulate::import("sklearn", convert = FALSE)
  model <- do.call(
    sklearn$ensemble$AdaBoostClassifier,
    reticulate::py_to_r(model_dict["params"])
  )
  estimators <- list()
  for (i in 1:length(model_dict$estimators_)) {
    estimators <- c(estimators, deSerializeDecisionTree(model_dict["estimators_"][i - 1]))
  }

  model$estimators_ <- np$array(estimators)
  model$classes_ <- np$array(model_dict["classes_"])
  model$n_features_in_ <- model_dict["n_features_in_"]
  model$n_classes_ <- model_dict["n_classes_"]
  model$estimator_weights_ <- np$array(model_dict["estimator_weights_"])

  return(model)
}

serializeNaiveBayes <- function(model) {
  serialized_model <- reticulate::dict(
    "meta" = "naive-bayes",
    "classes_" = model$classes_$tolist(),
    "class_count_" = model$class_count_$tolist(),
    "class_prior_" = model$class_prior_$tolist(),
    "theta_" = model$theta_$tolist(),
    "epsilon_" = model$epsilon_,
    "params" = model$get_params(),
    "var_" = model$var_$tolist()
  )
  return(serialized_model)
}

deSerializeNaiveBayes <- function(model_dict) {
  sklearn <- reticulate::import("sklearn", convert = FALSE)
  np <- reticulate::import("numpy", convert = FALSE)
  model <- do.call(
    sklearn$naive_bayes$GaussianNB,
    reticulate::py_to_r(model_dict["params"])
  )

  model$classes_ <- np$array(model_dict["classes_"])
  model$class_count_ <- np$array(model_dict["class_count_"])
  model$class_prior_ <- np$array(model_dict["class_prior_"])
  model$theta_ <- np$array(model_dict["theta_"])
  model$epsilon_ <- model_dict["epsilon_"]
  model$var_ <- np$array(model_dict["var_"])

  return(model)
}

serializeMLP <- function(model) {
  # TODO Check if length(intercepts_) is ever different from length(coefs_)
  for (i in 0:(length(model$coefs_) - 1)) {
    reticulate::py_set_item(
      model$coefs_, i,
      model$coefs_[i]$tolist()
    )
    reticulate::py_set_item(
      model$intercepts_, i,
      model$intercepts_[i]$tolist()
    )
  }
  serialized_model <- reticulate::dict(
    "meta" = "mlp",
    "coefs_" = model$coefs_,
    "loss_" = model$loss_,
    "intercepts_" = model$intercepts_,
    "n_iter_" = model$n_iter_,
    "n_layers_" = model$n_layers_,
    "n_outputs_" = model$n_outputs_,
    "out_activation_" = model$out_activation_,
    "params" = model$get_params(),
    "classes_" = model$classes_$tolist()
  )
  return(serialized_model)
}

deSerializeMlp <- function(model_dict) {
  sklearn <- reticulate::import("sklearn", convert = FALSE)
  np <- reticulate::import("numpy", convert = FALSE)

  model <- do.call(
    sklearn$neural_network$MLPClassifier,
    reticulate::py_to_r(model_dict["params"])
  )
  for (i in 0:(length(model_dict["coefs_"]) - 1)) {
    reticulate::py_set_item(
      model_dict["coefs_"], i,
      np$array(model_dict["coefs_"][i])
    )
    reticulate::py_set_item(
      model_dict["intercepts_"], i,
      np$array(model_dict["intercepts_"][i])
    )
  }
  model$coefs_ <- model_dict["coefs_"]
  model$loss_ <- model_dict["loss_"]
  model$intercepts_ <- model_dict["intercepts_"]
  model$n_iter_ <- model_dict["n_iter_"]
  model$n_layers_ <- model_dict["n_layers_"]
  model$n_outputs_ <- model_dict["n_outputs_"]
  model$out_activation_ <- model_dict["out_activation_"]
  model$classes_ <- np$array(model_dict["classes_"])

  return(model)
}

serializeSVM <- function(model) {
  serialized_model <- reticulate::dict(
    "meta" = "svm",
    "class_weight_" = model$class_weight_$tolist(),
    "classes_" = model$classes_$tolist(),
    "support_" = model$support_$tolist(),
    "n_support_" = model$n_support_$tolist(),
    "intercept_" = model$intercept_$tolist(),
    "probA_" = model$probA_$tolist(),
    "probB_" = model$probB_$tolist(),
    "_intercept_" = model$`_intercept_`$tolist(),
    "shape_fit_" = model$shape_fit_,
    "_gamma" = model$`_gamma`,
    "params" = model$get_params()
  )
  if (inherits(model$support_vectors_, "numpy.ndarray")) {
    serialized_model["support_vectors_"] <- model$support_vectors_$tolist()
  } else {
    serialized_model["support_vectors_"] <- serializeCsrMatrix(model$support_vectors_)
  }

  if (inherits(model$dual_coef_, "numpy.ndarray")) {
    serialized_model["dual_coef_"] <- model$dual_coef_$tolist()
  } else {
    serialized_model["dual_coef_"] <- serializeCsrMatrix(model$dual_coef_)
  }

  if (inherits(model$`_dual_coef_`, "numpy.ndarray")) {
    serialized_model["_dual_coef_"] <- model$`_dual_coef_`$tolist()
  } else {
    serialized_model["_dual_coef_"] <- serializeCsrMatrix(model$`_dual_coef_`)
  }
  return(serialized_model)
}

deSerializeSVM <- function(model_dict) {
  sklearn <- reticulate::import("sklearn", convert = FALSE)
  np <- reticulate::import("numpy", convert = FALSE)
  model <- do.call(
    sklearn$svm$SVC,
    reticulate::py_to_r(model_dict["params"])
  )
  model$shape_fit_ <- model_dict$shape_fit_
  model$`_gamma` <- model_dict["_gamma"]
  model$class_weight_ <- np$array(model_dict$class_weight_)$astype(np$float64)
  model$classes_ <- np$array(model_dict["classes_"])
  model$support_ <- np$array(model_dict["support_"])$astype(np$int32)
  model$`_n_support` <- np$array(model_dict["n_support_"])$astype(np$int32)
  model$intercept_ <- np$array(model_dict["intercept_"])$astype(np$float64)
  model$`_probA` <- np$array(model_dict["probA_"])$astype(np$float64)
  model$`_probB` <- np$array(model_dict["probB_"])$astype(np$float64)
  model$`_intercept_` <- np$array(model_dict["_intercept_"])$astype(np$float64)

  if (reticulate::py_bool((model_dict$support_vectors_["meta"] != reticulate::py_none())) &
    (reticulate::py_bool(model_dict$support_vectors_["meta"] == "csr"))) {
    model$support_vectors_ <- deSerializeCsrMatrix(model_dict$support_vectors_)
    model$`_sparse` <- TRUE
  } else {
    model$support_vectors_ <- np$array(model_dict$support_vectors_)$astype(np$float64)
    model$`_sparse` <- FALSE
  }
  if (reticulate::py_bool((model_dict$dual_coef_["meta"] != reticulate::py_none())) &
    (reticulate::py_bool(model_dict$dual_coef_["meta"] == "csr"))) {
    model$dual_coef_ <- deSerializeCsrMatrix(model_dict$dual_coef_)
  } else {
    model$dual_coef_ <- np$array(model_dict$dual_coef_)$astype(np$float64)
  }

  if (reticulate::py_bool((model_dict$`_dual_coef_`["meta"] != reticulate::py_none())) &
    (reticulate::py_bool(model_dict$`_dual_coef_`["meta"] == "csr"))) {
    model$`_dual_coef_` <- deSerializeCsrMatrix(model_dict$`dual_coef_`)
  } else {
    model$`_dual_coef_` <- np$array(model_dict$`_dual_coef_`)$astype(np$float64)
  }
  return(model)
}

serializeCsrMatrix <- function(csr_matrix) {
  serialized_csr_matrix <- reticulate::dict(
    "meta" = "csr",
    "indices" = csr_matrix$indices$tolist(),
    "indptr" = csr_matrix$indptr$tolist(),
    "_shape" = csr_matrix$`_shape`
  )
  serialized_csr_matrix["data"] <- csr_matrix$data$tolist()
  return(serialized_csr_matrix)
}

deSerializeCsrMatrix <- function(csr_dict,
                                 data_type = np$float64,
                                 indices_type = np$int32,
                                 indptr_type = np$int32) {
  sp <- reticulate::import("scipy", convert = FALSE)
  np <- reticulate::import("numpy", convert = FALSE)
  csr_matrix <- sp$sparse$csr_matrix(
    reticulate::tuple(list(
      np$array(csr_dict["data"])$astype(data_type),
      np$array(csr_dict["indices"])$astype(indices_type),
      np$array(csr_dict["indptr"])$astype(indptr_type)
    )),
    shape = csr_dict["shape"]
  )
  return(csr_matrix)
}
