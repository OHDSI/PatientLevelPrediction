# Specify settings for developing a single model

Specify settings for developing a single model

## Usage

``` r
createModelDesign(
  targetId = NULL,
  outcomeId = NULL,
  restrictPlpDataSettings = createRestrictPlpDataSettings(),
  populationSettings = createStudyPopulationSettings(),
  covariateSettings = FeatureExtraction::createDefaultCovariateSettings(),
  featureEngineeringSettings = NULL,
  sampleSettings = NULL,
  preprocessSettings = NULL,
  modelSettings = NULL,
  splitSettings = createDefaultSplitSetting(),
  hyperparameterSettings = createHyperparameterSettings(),
  runCovariateSummary = TRUE
)
```

## Arguments

- targetId:

  The id of the target cohort that will be used for data extraction
  (e.g., the ATLAS id)

- outcomeId:

  The id of the outcome that will be used for data extraction (e.g., the
  ATLAS id)

- restrictPlpDataSettings:

  The settings specifying the extra restriction settings when extracting
  the data created using
  [`createRestrictPlpDataSettings()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createRestrictPlpDataSettings.md).

- populationSettings:

  The population settings specified by
  [`createStudyPopulationSettings()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createStudyPopulationSettings.md)

- covariateSettings:

  The covariate settings, this can be a list or a single
  `'covariateSetting'` object.

- featureEngineeringSettings:

  Either NULL or an object of class `featureEngineeringSettings`
  specifying any feature engineering used during model development

- sampleSettings:

  Either NULL or an object of class `sampleSettings` with the over/under
  sampling settings used for model development

- preprocessSettings:

  Either NULL or an object of class `preprocessSettings` created using
  `createPreprocessingSettings()`

- modelSettings:

  The model settings such as
  [`setLassoLogisticRegression()`](https://ohdsi.github.io/PatientLevelPrediction/reference/setLassoLogisticRegression.md)

- splitSettings:

  The train/validation/test splitting used by all analyses created using
  [`createDefaultSplitSetting()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createDefaultSplitSetting.md)

- hyperparameterSettings:

  The hyperparameter settings created using
  [`createHyperparameterSettings()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createHyperparameterSettings.md)

- runCovariateSummary:

  Whether to run the covariateSummary

## Value

A list with analysis settings used to develop a single prediction model

## Details

This specifies a single analysis for developing as single model

## Examples

``` r
# L1 logistic regression model to predict the outcomeId 2 using the targetId 2
# with with default population, restrictPlp, split, and covariate settings
createModelDesign(
  targetId = 1,
  outcomeId = 2,
  modelSettings = setLassoLogisticRegression(seed=42),
  populationSettings = createStudyPopulationSettings(),
  restrictPlpDataSettings = createRestrictPlpDataSettings(),
  covariateSettings = FeatureExtraction::createDefaultCovariateSettings(),
  splitSettings = createDefaultSplitSetting(splitSeed = 42),
  runCovariateSummary = TRUE
)
#> $targetId
#> [1] 1
#> 
#> $outcomeId
#> [1] 2
#> 
#> $restrictPlpDataSettings
#> $studyStartDate
#> [1] ""
#> 
#> $studyEndDate
#> [1] ""
#> 
#> $firstExposureOnly
#> [1] FALSE
#> 
#> $washoutPeriod
#> [1] 0
#> 
#> $sampleSize
#> NULL
#> 
#> attr(,"class")
#> [1] "restrictPlpDataSettings"
#> 
#> $covariateSettings
#> $temporal
#> [1] FALSE
#> 
#> $temporalSequence
#> [1] FALSE
#> 
#> $DemographicsGender
#> [1] TRUE
#> 
#> $DemographicsAgeGroup
#> [1] TRUE
#> 
#> $DemographicsRace
#> [1] TRUE
#> 
#> $DemographicsEthnicity
#> [1] TRUE
#> 
#> $DemographicsIndexYear
#> [1] TRUE
#> 
#> $DemographicsIndexMonth
#> [1] TRUE
#> 
#> $ConditionGroupEraLongTerm
#> [1] TRUE
#> 
#> $ConditionGroupEraShortTerm
#> [1] TRUE
#> 
#> $DrugGroupEraLongTerm
#> [1] TRUE
#> 
#> $DrugGroupEraShortTerm
#> [1] TRUE
#> 
#> $DrugGroupEraOverlapping
#> [1] TRUE
#> 
#> $ProcedureOccurrenceLongTerm
#> [1] TRUE
#> 
#> $ProcedureOccurrenceShortTerm
#> [1] TRUE
#> 
#> $DeviceExposureLongTerm
#> [1] TRUE
#> 
#> $DeviceExposureShortTerm
#> [1] TRUE
#> 
#> $MeasurementLongTerm
#> [1] TRUE
#> 
#> $MeasurementShortTerm
#> [1] TRUE
#> 
#> $MeasurementRangeGroupLongTerm
#> [1] TRUE
#> 
#> $MeasurementRangeGroupShortTerm
#> [1] TRUE
#> 
#> $MeasurementValueAsConceptLongTerm
#> [1] TRUE
#> 
#> $MeasurementValueAsConceptShortTerm
#> [1] TRUE
#> 
#> $ObservationLongTerm
#> [1] TRUE
#> 
#> $ObservationShortTerm
#> [1] TRUE
#> 
#> $ObservationValueAsConceptLongTerm
#> [1] TRUE
#> 
#> $ObservationValueAsConceptShortTerm
#> [1] TRUE
#> 
#> $CharlsonIndex
#> [1] TRUE
#> 
#> $Dcsi
#> [1] TRUE
#> 
#> $Chads2
#> [1] TRUE
#> 
#> $Chads2Vasc
#> [1] TRUE
#> 
#> $includedCovariateConceptIds
#> logical(0)
#> 
#> $includedCovariateIds
#> logical(0)
#> 
#> $addDescendantsToInclude
#> [1] FALSE
#> 
#> $excludedCovariateConceptIds
#> logical(0)
#> 
#> $addDescendantsToExclude
#> [1] FALSE
#> 
#> $shortTermStartDays
#> [1] -30
#> 
#> $mediumTermStartDays
#> [1] -180
#> 
#> $endDays
#> [1] 0
#> 
#> $longTermStartDays
#> [1] -365
#> 
#> attr(,"fun")
#> [1] "getDbDefaultCovariateData"
#> attr(,"class")
#> [1] "covariateSettings"
#> 
#> $populationSettings
#> $binary
#> [1] TRUE
#> 
#> $includeAllOutcomes
#> [1] TRUE
#> 
#> $firstExposureOnly
#> [1] FALSE
#> 
#> $washoutPeriod
#> [1] 0
#> 
#> $removeSubjectsWithPriorOutcome
#> [1] TRUE
#> 
#> $priorOutcomeLookback
#> [1] 99999
#> 
#> $requireTimeAtRisk
#> [1] TRUE
#> 
#> $minTimeAtRisk
#> [1] 364
#> 
#> $riskWindowStart
#> [1] 1
#> 
#> $startAnchor
#> [1] "cohort start"
#> 
#> $riskWindowEnd
#> [1] 365
#> 
#> $endAnchor
#> [1] "cohort start"
#> 
#> $restrictTarToCohortEnd
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "populationSettings"
#> 
#> $sampleSettings
#> $sampleSettings[[1]]
#> $numberOutcomestoNonOutcomes
#> [1] 1
#> 
#> $sampleSeed
#> [1] 1
#> 
#> attr(,"fun")
#> [1] "sameData"
#> attr(,"class")
#> [1] "sampleSettings"
#> 
#> 
#> $featureEngineeringSettings
#> $featureEngineeringSettings[[1]]
#> list()
#> attr(,"fun")
#> [1] "sameData"
#> attr(,"class")
#> [1] "featureEngineeringSettings"
#> 
#> 
#> $preprocessSettings
#> $minFraction
#> [1] 0.001
#> 
#> $normalize
#> [1] TRUE
#> 
#> $removeRedundancy
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "preprocessSettings"
#> 
#> $modelSettings
#> $fitFunction
#> [1] "fitCyclopsModel"
#> 
#> $param
#> $param$priorParams
#> $param$priorParams$priorType
#> [1] "laplace"
#> 
#> $param$priorParams$forceIntercept
#> [1] FALSE
#> 
#> $param$priorParams$variance
#> [1] 0.01
#> 
#> $param$priorParams$exclude
#> [1] 0
#> 
#> 
#> $param$includeCovariateIds
#> NULL
#> 
#> $param$upperLimit
#> [1] 20
#> 
#> $param$lowerLimit
#> [1] 0.01
#> 
#> $param$priorCoefs
#> NULL
#> 
#> 
#> $settings
#> $settings$modelName
#> [1] "lassoLogisticRegression"
#> 
#> $settings$modelType
#> [1] "binary"
#> 
#> $settings$cyclopsModelType
#> [1] "logistic"
#> 
#> $settings$priorfunction
#> [1] "Cyclops::createPrior"
#> 
#> $settings$selectorType
#> [1] "byPid"
#> 
#> $settings$crossValidationInPrior
#> [1] TRUE
#> 
#> $settings$addIntercept
#> [1] TRUE
#> 
#> $settings$useControl
#> [1] TRUE
#> 
#> $settings$seed
#> [1] 42
#> 
#> $settings$threads
#> [1] -1
#> 
#> $settings$tolerance
#> [1] 2e-06
#> 
#> $settings$cvRepetitions
#> [1] 1
#> 
#> $settings$maxIterations
#> [1] 3000
#> 
#> $settings$saveType
#> [1] "RtoJson"
#> 
#> $settings$predict
#> [1] "predictCyclops"
#> 
#> 
#> attr(,"class")
#> [1] "modelSettings"
#> 
#> $splitSettings
#> $test
#> [1] 0.25
#> 
#> $train
#> [1] 0.75
#> 
#> $seed
#> [1] 42
#> 
#> $nfold
#> [1] 3
#> 
#> attr(,"fun")
#> [1] "randomSplitter"
#> attr(,"class")
#> [1] "splitSettings"
#> 
#> $hyperparameterSettings
#> $search
#> [1] "grid"
#> 
#> $tuningMetric
#> $tuningMetric$fun
#> function (prediction) 
#> {
#>     result <- do.call(fun, c(list(prediction = prediction), funArgs))
#>     if (!is.numeric(result) || length(result) != 1 || !is.finite(result)) {
#>         stop("Metric function must return one finite numeric value.", 
#>             call. = FALSE)
#>     }
#>     result
#> }
#> <bytecode: 0x56427ca11120>
#> <environment: 0x56427ca0d468>
#> 
#> $tuningMetric$maximize
#> [1] TRUE
#> 
#> $tuningMetric$name
#> [1] "AUC"
#> 
#> $tuningMetric$funArgs
#> list()
#> 
#> 
#> $sampleSize
#> NULL
#> 
#> $randomSeed
#> NULL
#> 
#> $generator
#> NULL
#> 
#> attr(,"class")
#> [1] "hyperparameterSettings"
#> 
#> $executeSettings
#> $runSplitData
#> [1] TRUE
#> 
#> $runSampleData
#> [1] FALSE
#> 
#> $runFeatureEngineering
#> [1] FALSE
#> 
#> $runPreprocessData
#> [1] FALSE
#> 
#> $runModelDevelopment
#> [1] TRUE
#> 
#> $runCovariateSummary
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "executeSettings"
#> 
#> attr(,"class")
#> [1] "modelDesign"
```
