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