fitClassifier <- function(
  trainData,
  modelSettings,
  hyperparameterSettings = createHyperparameterSettings(),
  search = NULL, # redundant input - add warning if not null?
  analysisId,
  ...
) {
  if (!FeatureExtraction::isCovariateData(trainData$covariateData)) {
    stop("Needs correct covariateData")
  }

  # add folds to labels if present:
  if (!is.null(trainData$folds)) {
    trainData$labels <- merge(trainData$labels, trainData$folds, by = "rowId")
  }

  # get the param
  param <- modelSettings$param

  # get the settings
  settings <- modelSettings$settings
  # backwards compatible
  if (is.null(settings)) {
    settings <- attr(param, "settings")
  }

  # TODO add a check for the settings?
  # like the old checkPySettings()

  ParallelLogger::logInfo(paste0("Training ", settings$modelName))
  start <- Sys.time()
  set.seed(settings$seed)

  # make sure the inputs are valid
  #TODO: make this generic
  #checkPySettings(settings)

  # prepare data for training (if needed) 
  if (!is.null(settings$prepareData)) {
    preparedData <- do.call(
      settings$prepareData,
      args = list(
        trainData,
        map = NULL
      )
    )
    covariates <- preparedData$dataMatrix
  } else {
    preparedData <- trainData
    covariates <- preparedData$covariateData
  }

  labels <- preparedData$labels
  covariateRef <- preparedData$covariateRef

  # Hyperparameter optimization
  # input: matrixData, labels, settings, hyperparamSettings
  # output: model, prediction, finalParam, hyperSummary
  tuneResult <- tuneHyperparameters(
    covariates,
    labels,
    param = param,
    settings = settings,
    hyperparamSettings = hyperparameterSettings
  )

  # add code for var imp here - model and settings

  variableImportance <- tryCatch(
    {
      do.call(
        settings$variableImportance,
        list(model = tuneResult$model, covariateMap = covariateRef)
      )
    },
    error = function(e) {
      ParallelLogger::logInfo("Error calculating variableImportance")
      ParallelLogger::logInfo(e)
      return(NULL)
    }
  )

  if (!is.null(variableImportance)) {
    covariateRef <- merge(
      covariateRef,
      variableImportance,
      all.x = TRUE,
      by = "covariateId"
    )
    covariateRef$covariateValue[is.na(covariateRef$covariateValue)] <- 0
    covariateRef$included[is.na(covariateRef$included)] <- 0
  } else {
    covariateRef$covariateValue <- 0
    covariateRef$included <- 1
  }

  comp <- start - Sys.time()

  result <- list(
    model = tuneResult$model,
    preprocessing = list(
      featureEngineering = attr(
        trainData$covariateData,
        "metaData"
      )$featureEngineering,
      tidyCovariates = attr(
        trainData$covariateData,
        "metaData"
      )$tidyCovariateDataSettings,
      requiresDenseMatrix = settings$requiresDenseMatrix
    ),
    prediction = tuneResult$prediction,
    modelDesign = PatientLevelPrediction::createModelDesign(
      targetId = attr(trainData, "metaData")$targetId,
      outcomeId = attr(trainData, "metaData")$outcomeId,
      restrictPlpDataSettings = attr(
        trainData,
        "metaData"
      )$restrictPlpDataSettings,
      hyperparameterSettings = hyperparameterSettings,
      covariateSettings = attr(trainData, "metaData")$covariateSettings,
      populationSettings = attr(trainData, "metaData")$populationSettings,
      featureEngineeringSettings = attr(
        trainData$covariateData,
        "metaData"
      )$featureEngineeringSettings,
      preprocessSettings = attr(
        trainData$covariateData,
        "metaData"
      )$preprocessSettings,
      modelSettings = modelSettings,
      splitSettings = attr(trainData, "metaData")$splitSettings,
      sampleSettings = attr(trainData, "metaData")$sampleSettings
    ),
    trainDetails = list(
      analysisId = analysisId,
      analysisSource = "", # TODO add from model
      developmentDatabase = attr(trainData, "metaData")$cdmDatabaseName,
      developmentDatabaseSchema = attr(trainData, "metaData")$cdmDatabaseSchema,
      attrition = attr(trainData, "metaData")$attrition,
      trainingTime = paste(as.character(abs(comp)), attr(comp, "units")),
      trainingDate = Sys.Date(),
      modelName = modelSettings$modelName,
      finalModelParameters = tuneResult$finalParam,
      hyperParamSearch = tuneResult$paramSearch
    ),
    covariateImportance = covariateRef
  )

  class(result) <- "plpModel"
  return(result)
}


tuneHyperparameters <- function(
  data,
  labels,
  param,
  settings,
  hyperparamSettings
) {
  iterator <- prepareHyperparameterGrid(
    paramDefinition = param,
    hyperSettings = hyperparamSettings
  )
  history <- list()
  metric <- hyperparamSettings$tuningMetric
  bestPerformance <- if (metric$maximize) -Inf else Inf
  bestCvPrediction <- c()

  repeat {
    candidate <- iterator$getNext(history)
    if (is.null(candidate)) {
      break
    }
    cvPrediction <- c()
    cvPerformance <- c()

    for (i in unique(labels$index)) {
      ind <- labels$index != i
      model <- do.call(
        settings$train,
        list(
          dataMatrix = data[ind, , drop = FALSE],
          labels = labels[ind, ],
          hyperParameters = candidate,
          settings = settings
        )
      )
      prediction <- do.call(
        settings$predict,
        list(
          plpModel = model,
          data = data[!ind, , drop = FALSE],
          cohort = labels[!ind, ]
        )
      )
      performance <- metric$fun(prediction)
      cvPrediction <- rbind(cvPrediction, prediction)
      cvPerformance <- c(performance, cvPerformance)
    }
    meanCvPerformance <- mean(cvPerformance, na.rm = TRUE)
    history[[length(history) + 1]] <- list(
      metric = metric$name,
      param = candidate,
      cvPerformance = meanCvPerformance,
      cvPerformancePerFold = cvPerformance,
      hyperSummary = makeHyperSummary(
        metric,
        meanCvPerformance,
        cvPerformance,
        candidate
      )
    )
    bestPerformance <- if (metric$maximize) {
      max(bestPerformance, meanCvPerformance, na.rm = TRUE)
    } else {
      min(bestPerformance, meanCvPerformance, na.rm = TRUE)
    }
    if (identical(bestPerformance, meanCvPerformance)) {
      ParallelLogger::logInfo(paste0(
        "New best performance ",
        round(bestPerformance, 4),
        " with param: ",
        paste(names(candidate), candidate, sep = "=", collapse = ", ")
      ))
      bestCvPrediction <- cvPrediction
    } else {
      ParallelLogger::logInfo(paste0(
        "Performance ",
        round(meanCvPerformance, 4),
        " with param: ",
        paste(names(candidate), candidate, sep = "=", collapse = ", ")
      ))
    }
  }
  iterator$finalize(history)
  best <- selectBest(history, metric)

  finalParam <- history[[best]]$param

  cvPrediction <- bestCvPrediction
  cvPrediction$evaluationType <- "CV"

  # fit final model
  ParallelLogger::logInfo("Fit best model on whole training set")
  finalModel <- do.call(
    settings$train,
    list(
      dataMatrix = data,
      labels = labels,
      hyperParameters = finalParam,
      settings = settings
    )
  )

  prediction <- do.call(
    settings$predict,
    list(
      plpModel = finalModel,
      data = data,
      cohort = labels
    )
  )

  prediction$evaluationType <- "Train"

  prediction <- rbind(
    prediction,
    cvPrediction
  )

  list(
    model = finalModel,
    prediction = prediction,
    finalParam = finalParam,
    paramSearch = history
  )
}

selectBest <- function(history, metric) {
  performances <- sapply(history, function(x) x$cvPerformance)
  if (metric$maximize) {
    best <- which.max(performances)
  } else {
    best <- which.min(performances)
  }
  best
}

makeHyperSummary <- function(metric, meanScore, foldScores, param) {
  perfRows <- data.frame(
    metric = metric$name,
    fold = c("CV", paste0("Fold", seq_along(foldScores))),
    value = c(meanScore, foldScores),
    stringsAsFactors = FALSE
  )
  paramValues <- lapply(param, function(x) {
    if (is.null(x)) {
      "null"
    } else if (length(x) == 1) {
      x
    } else {
      paste(x, collapse = ",")
    }
  })
  paramDf <- as.data.frame(paramValues, stringsAsFactors = FALSE)
  paramDf <- paramDf[rep(1L, nrow(perfRows)), , drop = FALSE]

  cbind(perfRows, paramDf, row.names = NULL)
}
