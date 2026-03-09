# Load the multiple prediction json settings from a file

Load the multiple prediction json settings from a file

## Usage

``` r
loadPlpAnalysesJson(jsonFileLocation)
```

## Arguments

- jsonFileLocation:

  The location of the file 'predictionAnalysisList.json' with the
  modelDesignList

## Value

A list with the modelDesignList and cohortDefinitions

## Details

This function interprets a json with the multiple prediction settings
and creates a list that can be combined with connection settings to run
a multiple prediction study

## Examples

``` r
modelDesign <- createModelDesign(targetId = 1, outcomeId = 2, 
                                 modelSettings = setLassoLogisticRegression())
saveLoc <- file.path(tempdir(), "loadPlpAnalysesJson")
savePlpAnalysesJson(modelDesignList = modelDesign, saveDirectory = saveLoc)
#> [1] "/tmp/RtmpWSyFtM/loadPlpAnalysesJson/predictionAnalysisList.json"
loadPlpAnalysesJson(file.path(saveLoc, "predictionAnalysisList.json"))
#> $plpVersion
#> [1] "6.5.1.9999"
#> 
#> $analyses
#> $analyses[[1]]
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
#> list()
#> 
#> $includedCovariateIds
#> list()
#> 
#> $addDescendantsToInclude
#> [1] FALSE
#> 
#> $excludedCovariateConceptIds
#> list()
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
#> attr(,"class")
#> [1] "covariateSettings"
#> attr(,"fun")
#> [1] "getDbDefaultCovariateData"
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
#> attr(,"class")
#> [1] "sampleSettings"
#> attr(,"fun")
#> [1] "sameData"
#> 
#> 
#> $featureEngineeringSettings
#> $featureEngineeringSettings[[1]]
#> named list()
#> attr(,"class")
#> [1] "featureEngineeringSettings"
#> attr(,"fun")
#> [1] "sameData"
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
#> [1] 22590968
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
#> [1] 50549
#> 
#> $nfold
#> [1] 3
#> 
#> attr(,"class")
#> [1] "splitSettings"
#> attr(,"fun")
#> [1] "randomSplitter"
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
#> <bytecode: 0x55a95288b3f8>
#> <environment: 0x55a9527fc648>
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
#> 
#> 
#> $cohortDefinitions
#> NULL
#> 
# clean use
unlink(saveLoc, recursive = TRUE)
```
