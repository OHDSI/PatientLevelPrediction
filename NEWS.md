PatientLevelPrediction 6.3.4
======================
- added spline feature engineering 
- added age/sex stratified imputation feature engineering
- changed result table execution date types to varchar
- updated covariateSummary to use feature engineering

PatientLevelPrediction 6.3.3
======================
- fixed bug introduced with new reticulate update in model saving to json tests


PatientLevelPrediction 6.3.2
======================
- fixed bug with database insert if result is incomplete
- updated/fixed documentation (Egill)
- added model path to models (Henrik)
- updated hyper-parameter saving to data.frame and made consistent 

PatientLevelPrediction 6.3.1
======================
- fixed bug with multiple covariate settings in diagnose plp
- added min cell count when exporting database results to csv files
- light GBM added (thanks Jin Choi and Chungsoo Kim)
- fixed minor bugs when uploading results to database

PatientLevelPrediction 6.2.1
======================
- added ensure_installed("ResultModelManager") to getDataMigrator()

PatientLevelPrediction 6.1.0
======================
- shiny app is now using ShinyAppBuilder with a config saved in the /inst folder

PatientLevelPrediction 6.0.11
======================
- fixed bugs introduced when sklearn inputs changed
- added sklearn model being saved as jsons
- made changes around the DatabaseConnection get table names function to make it work for the updated DatabaseConnection
- removed check RAM stop (now it just warns)

PatientLevelPrediction 6.0.10
======================
- Updated test to skip test for FE setting if the model does not fit (this was causing occasional test fail)
- replaced .data$ with "" for all dplyr::select to remove warnings

PatientLevelPrediction 6.0.9
======================
- Fix bug with python type being required to be int

PatientLevelPrediction 6.0.8
======================
- Allow priorType to be passed down to getCV function in case prior is not 'laplace'
- Seed specified in Cyclops model wasn't passed to Cyclops

PatientLevelPrediction 6.0.7
======================
- fixed issue with shiny viewer converting connection details to large json

PatientLevelPrediction 6.0.6
======================
- added check for cdmDatabaseId into createDatabaseDetails
- added test for check for cdmDatabaseId into createDatabaseDetails to error when NULL
- removed session$onSessionEnded(shiny::stopApp)  from shiny server

PatientLevelPrediction 6.0.5
======================
- fixing cox predictions

PatientLevelPrediction 6.0.4
======================
- forcing cdmDatabaseId to be a string if integer is input

PatientLevelPrediction 6.0.3
======================
- replaced utils::read.csv with readr::read_csv when inserting results from csv

PatientLevelPrediction 6.0.2
======================
- replaced gsub with sub when inserting csvs to database

PatientLevelPrediction 6.0.1
======================
- saved result specification csv in windows to fix odd formating issue

PatientLevelPrediction 6.0.0
======================
- fixed sample data bugs
- updated to use v1.0.0 of OhdsiShinyModules
- updated plp database result tables to use the same structure for cohort and database as other HADES packages
- added function to insert csv results into plp database result tables
- added input for databaseId (database and version) when extracting data to be consistent with other HADES packages.  This is saved in plp objects.

PatientLevelPrediction 5.4.4
======================
- fixed issue with 'preprocess' vs 'preprocessing' inconsistently used across models
- added metaData tracking for feature engineering or preprocessing when predicting
- fixed issue with FE using trainData$covariateData metaData rather than trainData
- fixed bug when using sameData for FE

PatientLevelPrediction 5.4.3
======================
- pulled in multiple bug fixes and test improvements from Egill
- pulled in fix for learning curves from Henrik
- Pulled in fix for feature engineering from Solomon
- Cleaned check messages about comparing class(x) with a string by changing to inherits()

PatientLevelPrediction 5.4.2
======================
- removed json saving for sklearn models since sklearn-json is no longer working for the latest sklearn


PatientLevelPrediction 5.4.1
======================
- renamed the input corresponding to the string that gets appended to the results table names to tablePrefix
- fixed issues with system.file() from SqlRender code breaking the tests
- added an input fileAppend to the function that exports the database tables to csv files
- moved the plp model (including preprocessing details) outside of the result database (into a specified folder) due to the size of the objects (too large to insert into the database). 

PatientLevelPrediction 5.4.0
======================
- added saving of plp models into the result database 
- added default cohortDefinitions in runMultiplePlp

PatientLevelPrediction 5.3.3
======================
- added modelType to all models for database upload

PatientLevelPrediction 5.3.2
======================
- moved FeatureExtraction to depends 
- fixed using inherits()

PatientLevelPrediction 5.3.1
======================
- moved most of the shiny app code into OhdsiShinyModules
- removed shiny dependencies and added OhdsiShinyModules to suggests
- fixed bug with linux sklearn saving

PatientLevelPrediction 5.1.1
======================
- replaced cohortId to targetId for consistency throughout code

PatientLevelPrediction 5.1.0
======================
- replaced targetId in model design to cohortId for consistency throughout code
- replaced plpDataSettings to restrictPlpDataSettings to improve naming consistency 
- added ability to use initial population in runPlp by adding the population to plpData$population
- added splitSettings into modelDesign
- replaced saving json settings with ParallelLogger function
- updated database result schema (removed researcher_id from tables - if desired a new table with the setting_ids and researcher_id could be added, removed study tables and revised results table to performances table with a reference to model_design_id and development_database_id to enable validation results without a model to be inserted)
- added diagnostic code based on PROBAST
- added diagnostic shiny module
- added code to create sqlite database and populate in uploadToDatabase
- add code to convert runPlp+val to sqlite database when viewing shiny
- added code to extract database results into csv files: extractDatabaseToCsv()


PatientLevelPrediction 5.0.5
======================
- pulled in GBM update (default hyper-parameters and variable importance fix) work done by Egill (egillax)

PatientLevelPrediction 5.0.4
======================
- updated installation documents
- added tryCatch around plots to prevent code stopping

PatientLevelPrediction 5.0.3
======================
- updated result schema (added model_design table with settings and added attrition table)
- updated shiny app for new database result schema
- removed C++ code for AUC and Rcpp dependency, now using pROC instead as faster
- made covariate summary optional when externally validating 

PatientLevelPrediction 5.0.2
======================
- updated json structure for specifying study design (made it friendlier to read)
- includes smooth calibration plot fix - work done by Alex (rekkasa)
- fixed bug with multiple sample methods or feature engineering settings causing invalid error

PatientLevelPrediction 5.0.0
======================
- plpModel now saved as json files when possible
- Updated runPlp to make more modular
- now possible to customise data splitting, feature engineering, sampling (over/under) and learning algorithm
- added function for extracting cohort covariates
- updated evalaution to evaluate per strata (evaluation column)
- updated plpModel structure
- updated runPlp structure
- updated shiny and package to use tidyr and not reshape2
- sklearn learning algorithms share the same fit function
- r learning algorithms share the same fit function
- interface to cyclops code revised 
- ensemble learning removed (will be in separate package)
- deep learning removed (will be in DeepPatientLevelPrediction package)


PatientLevelPrediction 4.4.2
======================
- revised toSparseM() to do conversion in one go but check RAM availablility beforehand.
- removed temporal plpData conversion in toSparseM (this will be done in DeepPatientLevelPrediction)

PatientLevelPrediction 4.4.1
======================
- shiny can now read csv results
- objects loaded via loadPlpFromCsv() can be saved using savePlpResult()

PatientLevelPrediction 4.4.0
======================
- added database result storage 
- added interface to database results in shiny
- merged in shinyRepo that changed the shiny app to make it modular and added new features
- removed deep learning as this is being added into new OHDSI package DeepPatientLevelPrediction

PatientLevelPrediction 4.3.10
======================
- save xgboost model as json file for transparency
- set connectionDetails to NULL in getPlpData


PatientLevelPrediction 4.3.9
======================
- updated andromeda functions - restrict to pop and tidy covs for speed
- quick fix for GBM survival predicting negative values
- fixed occasional demoSum error for survival models
- updated index creation to use Andromeda function


PatientLevelPrediction 4.3.8
======================
- fixed bug when normalize data is false
- fixed bugs when single feature (gbm + python)
- updated GBM 

PatientLevelPrediction 4.3.7
======================
- updated calibration slope
- fixed missing age/gender in prediction
- fixed shiny intercept bug
- fixed diagnostic
- fixed missing covariateSettings in  load cvs plp

PatientLevelPrediction 4.3.6
======================
- Removed plpData from evaluation 
- Added recalibration into externalVal
- Updated shiny app for recalibration
- Added population creation setting to use cohortEndDate as timeAtRisk end 
- fixed tests

PatientLevelPrediction 4.3.3
======================
- Reduced imports by adding code to install some dependencies when used

PatientLevelPrediction 4.3.2
======================
- fixed csv result saving bug when no model param

PatientLevelPrediction 4.3.1
======================
- fixed r check vignette issues 
- added conda install to test 

PatientLevelPrediction 4.3.0
======================
- finalised permutation feature importance

PatientLevelPrediction 4.2.10
======================
- fixed deepNN index issue (reported on github - thanks dapritchard)
- add compression to python pickles
- removed requirement to have outcomeCount for prediction with python models

PatientLevelPrediction 4.2.9
======================
- cleaned all checks 
- fixed bug in python toSparseMatrix
- fixed warning in studyPop

PatientLevelPrediction 4.2.8
======================
- fixed bug (identified by Chungsoo) in covariateSummary
- fixed bug with thresholdSummary
- edited threshold summary function to make it cleaner
- added to ensemble where you can combine multiple models into an ensemble
- cleaned up the notes and tests
- updated simulated data covariateId in tests to use integer64
- fixed description imports (and sorted them)

PatientLevelPrediction 4.2.7
======================
- fixed Cox model calibration plots
- fixed int64 conversion bug

PatientLevelPrediction 4.2.6
======================
- added baseline risk to Cox model 

PatientLevelPrediction 4.2.3
======================
- updated shiny: added attrition and hyper-parameter grid search into settings

PatientLevelPrediction 4.2.2
======================
- updated shiny app added 95% CI to AUC in summary, size is now complete data size and there is a column valPercent that tells what percentage of the data were used for validation 

PatientLevelPrediction 4.2.1
======================
- updated GBMsurvival to use survival metrics and c-stat

PatientLevelPrediction 4.2.0
======================
- added survival metrics 

PatientLevelPrediction 4.1.0
======================
- added updates and fixes into master from development branch

PatientLevelPrediction 4.0.6
======================
- fixed bug with pdw data extraction due to multiple person_id columns
- fixed bug in shiny app converting covariate values due to tibble

PatientLevelPrediction 4.0.5
======================
- added calibration updates: cal-in-large, weak cal 
- updated smooth cal plot (sample for speed in big data)
- defaulted to 100 values in calibrationSummary + updated cal plot

PatientLevelPrediction 4.0.4
======================
- fixed backwards compat with normalization
- fixed python joblib dependancy 


PatientLevelPrediction 4.0.2
======================
- fixed bug in preprocessing 
- added cross validation aucs to LR, GBM, RF and MLP
- added more settings into MLP
- added threads option in LR


PatientLevelPrediction 4.0.1
======================
- fixed minor bug with shiny dependency 
- fixed some tests
- added standardizedMeanDiff to covariatesummary
- updated createStudyPopulation to make it cleaner to read and count outcome per TAR

PatientLevelPrediction 4.0.0
======================
- Andromeda replaced ff data objects
- added age/gender into cohort
- fixed python warnings
- updated shiny plp viewer

PatientLevelPrediction 3.0.16
======================
- Fixed bug when running multiple analyses using a data extraction sample with multiple covariate settings

PatientLevelPrediction 3.0.15
======================
- improved shiny PLP viewer
- added diagnostic shiny viewer

PatientLevelPrediction 3.0.14
======================
- updated external validate code to enable custom covariates using ATLAS cohorts
- fixed issues with startAnchor and endAnchor


PatientLevelPrediction 3.0.13
======================
- Deprecating addExposureDaysToStart and addExposureDaysToEnd arguments in createStudyPopulation, adding new arguments called startAnchor and endAnchor. The hope is this is less confusing.
- fixed transfer learning code (can now transfer or fine-tune model)
- made view plp shiny apps work when some results are missing

PatientLevelPrediction 3.0.12
======================
- set up testing 
- fixed build warnings

PatientLevelPrediction 3.0.11
======================
- added tests to get >70% coverage (keras tests too slow for travis)
- Fixed minor bugs
- Fixed deep learning code and removed pythonInR dependancy
- combined shiny into one file with one interface

PatientLevelPrediction 3.0.10
======================
- added recalibration using 25% sample in existing models
- added option to provide score to probabilities for existing models
- fixed warnings with some plots


PatientLevelPrediction 3.0.9
======================
Small bug fixes:
- added analysisId into model saving/loading
- made external validation saving recursive 
- added removal of patients with negative TAR when creating population 
- added option to apply model without preprocessing settings (make them NULL)
- updated create study population to remove patients with negative time-at-risk

PatientLevelPrediction 3.0.8
======================
Changes:
- merged in bug fix from Martijn - fixed AUC bug causing crash with big data
- update SQL code to be compatible with v6.0 OMOP CDM
- added save option to external validate PLP


PatientLevelPrediction 3.0.7
======================
Changes:
- Updated splitting functions to include a splitby subject and renamed personSplitter to randomSplitter
- Cast indices to integer in python functions to fix bug with non integer sparse matrix indices

PatientLevelPrediction 3.0.5
======================

Changes:
- Added GLM status to log (will now inform about any fitting issue in log)
- Added GBM survival model (still under development)
- Added RF quantile regression (still under development)
- Updated viewMultiplePlp() to match PLP skeleton package app
- Updated single plp vignette with additional example 
- Merge in deep learning updates from Chan

PatientLevelPrediction 3.0.4
======================

Changes:
- Updated website

PatientLevelPrediction 3.0.3
======================

Changes:
- Added more tests
- test files now match R files

PatientLevelPrediction 3.0.2
======================

Changes:
- Fixed ensemble stacker 

PatientLevelPrediction 3.0.1
======================

Changes:
- Using reticulate for python interface 
- Speed improvements
- Bug fixes
