serializeTree <- function(tree) {
  serializedTree <- tree$`__getstate__`()
  dtypes <- serializedTree["nodes"]$dtype
  
  serializedTree["nodes"] <- serializedTree["nodes"]$tolist()
  serializedTree["values"] <- serializedTree["values"]$tolist()
  
  return(list(serializedTree, dtypes)) 
}

deSerializeTree <- function(tree_dict, nFeatures, nClasses, nOutputs) {
  for (i in 0:(length(tree_dict['nodes']) - 1)) {
    # that's a monstrosity.. 
    reticulate::py_set_item(tree_dict['nodes'],i,reticulate::tuple(reticulate::py_to_r(tree_dict['nodes'][i])))
    }
  
  names = list('left_child', 'right_child', 'feature', 'threshold', 'impurity', 'n_node_samples', 'weighted_n_node_samples')
  
  sklearn <- reticulate::import('sklearn')
  np <- reticulate::import('numpy', convert = FALSE)
  
  tree_dict['nodes'] <- np$array(tree_dict['nodes'], 
                                 dtype=np$dtype(reticulate::dict(
                                   names = names,
                                   formats = tree_dict['nodes_dtype']
                                   )))
  tree_dict['values'] <- np$array(tree_dict['values'])
  
  Tree <- sklearn$tree$`_tree`$Tree(nFeatures, 
                                    np$array(reticulate::tuple(nClasses), 
                                             dtype=np$intp),
                               nOutputs)
  
  Tree$`__setstate__`(tree_dict)
  
  return(Tree)
}

serializeDecisionTree <- function(model) {
   tree <- serializeTree(model$tree_)
   dtypes <- tree[[2]]
   tree <- tree[[1]]
   py <- reticulate::import_builtins(convert=FALSE)
   serialized_model <- reticulate::dict(
     "meta" = 'decision-tree',
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
   for (i in 0:(length(dtypes)-1)) {
     tree_dtypes <- c(tree_dtypes, dtypes[[i]]$str)
   }
   
   serialized_model["tree_"]["nodes_dtype"] <- tree_dtypes
   return(serialized_model)
}

deSerializeDecisionTree <- function(model_dict) {
  np <- reticulate::import("numpy", convert=FALSE)
  sklearn <- reticulate::import("sklearn", convert=FALSE)
  deserialized_model <- do.call(sklearn$tree$DecisionTreeClassifier,
                                reticulate::py_to_r(model_dict["params"]))
  
  deserialized_model$classes_ <- np$array(model_dict["classes_"])
  deserialized_model$max_features_ <- model_dict["max_features_"]
  deserialized_model$n_classes_ <- model_dict["n_classes_"]
  deserialized_model$n_features_in <- model_dict["n_features_in_"]
  deserialized_model$n_outputs_ <- model_dict["n_outputs_"]
  
  tree <- deSerializeTree(model_dict["tree_"],
                          model_dict["n_features_in_"],
                          model_dict["n_classes_"],
                          model_dict["n_outputs_"])
  deserialized_model$tree_ <- tree
  
  return(deserialized_model)
}