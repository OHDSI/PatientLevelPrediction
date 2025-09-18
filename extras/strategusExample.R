library(PatientLevelPrediction)
library(Strategus)

# COHORT GENERATION ------------------------------------------------------
targetFile <- "~/github/DeepLearningComparison/cohorts/11932-lungcancer-target-v4.json"
cohortDefinitions <- CohortGenerator::createEmptyCohortDefinitionSet()
cohortDefinitions[1, ] <- list(
  cohortId = 1,
  cohortName = "Target",
  sql = "",
  json = readChar(targetFile, file.info(targetFile)$size)
)

outcomeFile <- "~/github/DeepLearningComparison/cohorts/298-lungcancer-outcome-v1.json"
cohortDefinitions[2, ] <- list(
  cohortId = 2,
  cohortName = "Outcome",
  sql = "empty",
  json = readChar(outcomeFile, file.info(outcomeFile)$size)
)

cohortGeneratorModule <- CohortGeneratorModule$new()
cohortDefShared <- 
  cohortGeneratorModule$createCohortSharedResourceSpecifications(cohortDefinitions)

cohortGeneratorModuleSpecifications <- 
  cohortGeneratorModule$createModuleSpecifications(
    generateStats = TRUE
  )

# PatientLevelPrediction ------------------------------------------------------
plpModule <- PatientLevelPredictionModule$new()

covariateSettings <- FeatureExtraction::createCovariateSettings(
  useDemographicsGender = TRUE,
  useDemographicsAgeGroup = TRUE,
  useDrugGroupEraLongTerm = TRUE,
  useDrugGroupEraShortTerm  = TRUE,
  useConditionGroupEraLongTerm = TRUE,
  useConditionGroupEraShortTerm = TRUE,
  endDays = -1)


populationSettings <- createStudyPopulationSettings(binary = TRUE,
                                                    removeSubjectsWithPriorOutcome = TRUE,
                                                    priorOutcomeLookback = 99999,
                                                    requireTimeAtRisk = FALSE,
                                                    minTimeAtRisk = 1,
                                                    riskWindowStart = 0,
                                                    startAnchor = "cohort start",
                                                    riskWindowEnd = 365,
                                                    endAnchor = "cohort start")

preprocessSettings <- createPreprocessSettings(
  minFraction = 0.001,
  normalize = TRUE,
  removeRedundancy = TRUE
)

splitSettings <- createDefaultSplitSetting(splitSeed = 42)

modelDesign <- PatientLevelPrediction::createModelDesign(
    targetId = 1, 
    outcomeId = 2,
    populationSettings = populationSettings,
    covariateSettings = covariateSettings,
    preprocessSettings = preprocessSettings,
    modelSettings = PatientLevelPrediction::setLassoLogisticRegression(seed = 42),
    splitSettings = splitSettings,
    runCovariateSummary = FALSE
  )
  
plpModuleSpecs <- plpModule$createModuleSpecifications(modelDesignList = modelDesign)

# ANALYSIS SPECIFICATIONS FOR STRATEGUS ------------------------------------------
analysisSpecifications <- createEmptyAnalysisSpecificiations() |>
  addSharedResources(cohortDefShared) |>
  addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  addModuleSpecifications(plpModuleSpecs)

# Save a sharable JSON file with the analysis specifications
# ParallelLogger::saveSettingsToJson(analysisSpecifications, file.path("extras", "development.json"))



# EXECUTE STUDY WITH STRATEGUS ------------------------------------------

# Information about where to store stuff in database and locally
executionSettings <- Strategus::createCdmExecutionSettings(
  workDatabaseSchema = "cohorts",
  cdmDatabaseSchema = "main",
  cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "strategus_cohort"),
  workFolder = "./results/strategus/work",
  resultsFolder = "./results/strategus/output"
)

# How to connect to the database
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "duckdb",
  server = "~/database/database-1M_filtered.duckdb"
)

# Execute the study
Strategus::execute(analysisSpecifications,
                   executionSettings = executionSettings,
                   connectionDetails = connectionDetails)
