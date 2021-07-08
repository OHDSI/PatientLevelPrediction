
### This is a alpha version of the OHDSI/EHDEN Prediction model Library

This contains a demo version of the Patient-Level Prediction Model Library

This library will contain every model developed or validated within the OHDSI PLP Framework. It will be a centralised, versioned repository that will give users the ability to explore the models, interactively examine the results, download and validate different models. Including the cohorts used for each setting. 

The library will also provide a place to upload results of validations or model developments to then be included in the library itself.

### Aims
The motivation for this project is to increase the trust, reproducibility and ease of understanding of PLP research.

This will be done by centralising all published models to improve accessibility. The library will provide a single location to find the models, rather than the current method of each study having its own unique shiny application.
The models will be version controlled, so if i model is updated after publication of a journal paper, both the original evidence and the newer version will remain accessible.

Further for methods researchers it will provide an easy way to find models, that were developed on similar data to their own,  to test out new model development methods and metrics.

### Usage instructions ###
To use the app navigate to the Library tab which will show all the models that have been developed within the OHDSI PLP framework. The table displays information about the model setting, performance and number of external validations. Once you have found the model you are interested in select the model and then you can then select the model by clicking it, then navigating to the tabs labelled: 
 - Development Settings 
 - Summary 
 - Discrimination 
 - Calibration

will provide you with the ability to interactively explore the internal validation of the model and provide info on various features of the model development.
 
Under the validation tab, there is information available on the different external validations that have been performed for your chosen model. These can be selected and the metrics and plots will update to allow for easy performance comparison.

### Proposed Developments
- create database to store model information to increase speed of application (in process)
- add ability to upload development/validation (in process)
- add ability to download validation packages (in process)
- add versioning to support research reproducibility (completed, github/zenodo dependant)
- add fingerprinting to models to check the correct model/cohorts have been used
- add user accounts with different access levels
