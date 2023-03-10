# @file LightGBM.R

#' Create setting for gradient boosting machine model using gbm_xgboost implementation # nolint
#'
#' @param numLeaves   This hyperparameter sets the maximum number of leaves. Increasing this parameter can lead to higher model complexity and potential overfitting.
#' @param maxDepth   This hyperparameter sets the maximum depth . Increasing this parameter can also lead to higher model complexity and potential overfitting.
#' @param minDataInLeaf   This hyperparameter sets the minimum number of data points that must be present in a leaf node. Increasing this parameter can help to reduce overfitting
#' @param learningRate This hyperparameter controls the step size at each iteration of the gradient descent algorithm. Lower values can lead to slower convergence but may result in better performance.
#' @param lambdaL1  This hyperparameter controls L1 regularization, which can help to reduce overfitting by encouraging sparse models.
#' @param lambdaL2 This hyperparameter controls L2 regularization, which can also help to reduce overfitting by discouraging large weights in the model.
#' @param minSplitGain This hyperparameter sets the minimum gain required to split a node in a decision tree. Increasing this parameter can help to reduce overfitting.
#' @param scalePosWeight    Controls weight of positive class in loss - useful for imbalanced classes
#'
#' @examples
#' model.lightgbm <- setightGBM(
#'     numLeaves = c(20, 31, 50), maxDepth = c(-1, 5, 10),
#'     minDataInLeaf = c(10, 20, 30), learningRate = c(0.05, 0.1, 0.3)
#' )
#'
#' @export
setLightGBM <- function(nthread = 20, earlyStopRound = 25, numLeaves = c(20, 31, 50),
                        maxDepth = c(-1, 5, 10),
                        minDataInLeaf = c(10, 20, 30),
                        learningRate = c(0.05, 0.1, 0.3),
                        lambdaL1 = c(0, 0.1, 1),
                        lambdaL2 = c(0, 0.1, 1),
                        minSplitGain = c(0, 0.1, 1),
                        scalePosWeight = 1, seed = sample(10000000, 1)) {
    ensure_installed("xgboost")

    checkIsClass(seed, c("numeric", "integer"))
    ensure_installed("lightgbm")

    checkIsClass(seed, c("numeric", "integer"))

    if (length(nthread) > 1) {
        stop("nthread must be length 1")
    }
    if (!inherits(x = seed, what = c("numeric", "NULL", "integer"))) {
        stop("Invalid seed")
    }

    paramGrid <- list(
        numLeaves = numLeaves,
        maxDepth = maxDepth,
        minDataInLeaf = minDataInLeaf,
        learningRate = learningRate,
        lambdaL1 = lambdaL1,
        lambdaL2 = lambdaL2,
        minSplitGain = minSplitGain,
        scalePosWeight = scalePosWeight
    )

    param <- listCartesian(paramGrid)

    attr(param, "settings") <- list(
        modelType = "LightGBM",
        seed = seed[[1]],
        modelName = "LightGBM",
        threads = nthread[1],
        varImpRFunction = "varImpLightGBM",
        trainRFunction = "fitLightGBM",
        predictRFunction = "predictLightGBM"
    )

    attr(param, "saveType") <- "lightgbm"

    result <- list(
        fitFunction = "fitRclassifier",
        param = param
    )

    class(result) <- "modelSettings"

    return(result)
}



varImpLightGBM <- function(model,
                           covariateMap) {
    varImp <- lightgbm::lightgbm.importance(model, importance_type = "gain")

    varImp <- data.frame(
        covariateId = names(varImp),
        covariateValue = varImp,
        included = 1
    )

    varImp <- merge(covariateMap, varImp, by.x = "columnId", by.y = "covariateId")
    varImp <- varImp %>%
        dplyr::select(covariateId, covariateValue, included)

    return(varImp)
}

predictLightGBM <- function(plpModel,
                            data,
                            cohort) {
    if (inherits(data, "plpData")) {
        # convert
        matrixObjects <- toSparseM(
            plpData = data,
            cohort = cohort,
            map = plpModel$covariateImportance %>%
                dplyr::select("columnId", "covariateId")
        )

        # use the include??

        newData <- matrixObjects$dataMatrix
        cohort <- matrixObjects$labels
    } else {
        newData <- data
    }

    if (inherits(plpModel, "plpModel")) {
        model <- plpModel$model
    } else {
        model <- plpModel
    }

    pred <- data.frame(value = predict(model, newData))
    prediction <- cohort
    prediction$value <- pred$value

    prediction <- prediction %>%
        dplyr::select(-"rowId") %>%
        dplyr::rename(rowId = "originalRowId")

    attr(prediction, "metaData") <- list(modelType = attr(plpModel, "modelType"))

    return(prediction)
}

fitLightGBM <- function(dataMatrix,
                        labels,
                        hyperParameters,
                        settings) {
    if (!is.null(hyperParameters$earlyStopRound)) {
        trainInd <- sample(nrow(dataMatrix), nrow(dataMatrix) * 0.9)
        train <- lightgbm::lgb.Dataset(
            data = dataMatrix[trainInd, , drop = F],
            label = labels$outcomeCount[trainInd]
        )
        test <- lightgbm::lgb.Dataset(
            data = dataMatrix[-trainInd, , drop = F],
            label = labels$outcomeCount[-trainInd]
        )
        watchlist <- list(train = train, test = test)
    } else {
        train <- lightgbm::lgb.Dataset(
            data = dataMatrix,
            label = labels$outcomeCount
        )
        watchlist <- list()
    }

    outcomes <- sum(labels$outcomeCount > 0)
    N <- nrow(labels)
    outcomeProportion <- outcomes / N
    set.seed(settings$seed)
    model <- lightgbm::lgb.train(
        data = train,
        params = list(
            objective = "binary",
            boost = "gbtree",
            metric = "auc",
            num_leaves = hyperParameters$numLeaves,
            max_depth = hyperParameters$maxDepth,
            learning_rate = hyperParameters$learnRate,
            min_data_in_leaf = hyperParameters$minDataInLeaf,
            scale_pos_weight = hyperParameters$scalePosWeight,
            lambda_l2 = hyperParameters$lambdaL1,
            lambda_l1 = hyperParameters$lambdaL2,
            seed = settings$seed,
            is_unbalance = T,
            max_bin = 255,
            num_threads = settings$threads
        ),
        nthread = settings$threads,
        early_stopping_rounds = hyperParameters$earlyStopRound,
        valids = watchlist,
        verbose_eval = 10,
        categorical_feature = "auto"
    )

    return(model)
}
