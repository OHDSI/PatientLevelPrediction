PatientLevelPrediction
======================

[![Build Status](https://github.com/OHDSI/PatientLevelPrediction/workflows/R-CMD-check/badge.svg)](https://github.com/OHDSI/PatientLevelPrediction/actions?query=workflow%3AR-CMD-check)

[![codecov.io](https://codecov.io/github/OHDSI/PatientLevelPrediction/coverage.svg?branch=main)](https://codecov.io/github/OHDSI/PatientLevelPrediction?branch=main)


PatientLevelPrediction is part of [HADES](https://ohdsi.github.io/Hades).


Introduction
============

PatientLevelPrediction is an R package for building and validating patient-level predictive models using data in the OMOP Common Data Model format.  

Reps JM, Schuemie MJ, Suchard MA, Ryan PB, Rijnbeek PR. [Design and implementation of a standardized framework to generate and evaluate patient-level prediction models using observational healthcare data.](https://academic.oup.com/jamia/article/25/8/969/4989437) J Am Med Inform Assoc. 2018;25(8):969-975.

The figure below illustrates the prediction problem we address. Among a population at risk, we aim to predict which patients at a defined moment in time (t = 0) will experience some outcome during a time-at-risk. Prediction is done using only information about the patients in an observation window prior to that moment in time.

![](vignettes/Figure1.png)

To define a prediction problem we have to define t=0 by a Target Cohort (T), the outcome we like to predict by an outcome cohort (O), and the time-at-risk (TAR). Furthermore, we  have to make design choices for the model we like to develop, and determine the observational datasets to perform internal and external validation. This conceptual framework works for all type of prediction problems, for example those presented below (T=green, O=red).

![](vignettes/problems.png)

Features
========
- Takes one or more target cohorts (Ts) and one or more outcome cohorts (Os) and develops and validates models for all T and O combinations.
- Allows for multiple prediction design options.
- Extracts the necessary data from a database in OMOP Common Data Model format for multiple covariate settings.
- Uses a large set of covariates including for example all drugs, diagnoses, procedures, as well as age, comorbidity indexes, and custom covariates.
- Allows you to add custom covariates or cohort covariates.
- Includes a large number of state-of-the-art machine learning algorithms that can be used to develop predictive models, including Regularized logistic regression, Random forest, Gradient boosting machines, Decision tree, Naive Bayes, K-nearest neighbours, Neural network, AdaBoost and Support vector machines.
- Allows you to add custom algorithms.
- Allows you to add custom feature engineering
- Allows you to add custom under/over sampling (or any other sampling) [note: based on existing research this is not recommended]
- Contains functionality to externally validate models.
- Includes functions to plot and explore model performance (ROC + Calibration).
- Build ensemble models using EnsemblePatientLevelPrediction.
- Build Deep Learning models using DeepPatientLevelPrediction.
- Generates learning curves.
- Includes a shiny app to interactively view and explore results.
- In the shiny app you can create a html file document (report or protocol) containing all the study results.


Screenshots
===========

<table>
<tr valign="bottom">
<td width = 50%>

<img src="https://github.com/OHDSI/PatientLevelPrediction/raw/main/vignettes/sparseCalibration.png" alt="Calibration plot" title="Calibration plot" />

</td>
<td width = 50%>
<img src="https://github.com/OHDSI/PatientLevelPrediction/raw/main/vignettes/sparseROC.png" alt="ROC plot" title="ROC plot" />

</td>
</tr><tr>
<td> Calibration Plot</td><td> ROC Plot</td>
</tr>
</table>


Demo of the Shiny Apps can be found here:

- [Multiple Prediction Viewer Shiny App](http://data.ohdsi.org/smokingPhenotypeExplorer/)


Technology
==========
PatientLevelPrediction is an R package, with some functions using python through reticulate.

System Requirements
===================
Requires R (version 4.0 or higher). Installation on Windows requires [RTools](http://cran.r-project.org/bin/windows/Rtools/). Libraries used in PatientLevelPrediction require Java and Python.

The python installation is required for some of the machine learning algorithms. We advise to
install Python 3.7 using Anaconda (https://www.continuum.io/downloads). 

Getting Started
===============

- To install the package please read the [Package Installation guide](https://github.com/OHDSI/PatientLevelPrediction/blob/main/inst/doc/InstallationGuide.pdf)

- Have a look at the video below for an extensive demo of the package.

<a href="http://www.youtube.com/watch?feature=player_embedded&v=BEukCbT8UoA
" target="_blank"><img src="http://img.youtube.com/vi/BEukCbT8UoA/0.jpg" 
alt="Video Vignette PLP Package" width="240" height="180" border="10" /></a>

Please read the main vignette for the package:

- [Building Single Patient-Level Predictive Models](https://github.com/OHDSI/PatientLevelPrediction/blob/main/inst/doc/BuildingPredictiveModels.pdf)

In addition we have created vignettes that describe advanced functionality in more detail:

- [Building Multiple Patient-Level Predictive Models](https://github.com/OHDSI/PatientLevelPrediction/blob/main/inst/doc/BuildingMultiplePredictiveModels.pdf)
- [Implementing Existing Patient-Level Predictive Models](https://github.com/OHDSI/PatientLevelPrediction/blob/main/inst/doc/ImplementingExistingModels.pdf)
- [Adding Custom Machine Learning Algorithms](https://github.com/OHDSI/PatientLevelPrediction/blob/main/inst/doc/AddingCustomAlgorithms.pdf)
- [Building Deep Learning Models](https://github.com/OHDSI/DeepPatientLevelPrediction)
- [Building Ensemble Models](https://github.com/OHDSI/EnsemblePatientLevelPrediction)
- [Creating Learning Curves](https://github.com/OHDSI/PatientLevelPrediction/blob/main/inst/doc/CreatingLearningCurves.pdf)

Package manual: [PatientLevelPrediction.pdf](https://github.com/OHDSI/PatientLevelPrediction/blob/main/extras/PatientLevelPrediction.pdf)

User Documentation
==================
Documentation can be found on the [package website](https://ohdsi.github.io/PatientLevelPrediction).

PDF versions of the documentation are also available, as mentioned above.

Support
=======
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="https://github.com/OHDSI/PatientLevelPrediction/issues">GitHub issue tracker</a> for all bugs/issues/enhancements

Contributing
============
Read [here](https://ohdsi.github.io/Hades/contribute.html) how you can contribute to this package. 
 
License
=======
PatientLevelPrediction is licensed under Apache License 2.0

Development
===========
PatientLevelPrediction is being developed in R Studio.


# Acknowledgements

- The package is maintained by Jenna Reps and Peter Rijnbeek and has been developed with major contributions from Martijn Schuemie, Patrick Ryan, and Marc Suchard.
- We like to thank the following persons for their contributions to the package: Seng Chan You, Ross Williams, Henrik John, Xiaoyong Pan, James Wiggins, Egill Fridgeirsson, Alex Rekkas
- This project is supported in part through the National Science Foundation grant IIS 1251151.
