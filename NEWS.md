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