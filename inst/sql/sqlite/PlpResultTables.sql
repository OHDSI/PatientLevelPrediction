-- this links the PLP cohort_definition_id to the COHORT_DEFINITION
CREATE TABLE @my_schema.@string_to_appendcohorts ( 
    cohort_id INTEGER PRIMARY KEY AUTOINCREMENT, -- 
    cohort_definition_id int NOT NULL, -- the atlas id check type
    cohort_name VARCHAR(MAX) NOT NULL
);

-- NEW - needs to match cohort generator COHORT_DEFINITION
CREATE TABLE @my_schema.@string_to_appendCOHORT_DEFINITION (
    cohort_definition_id int NOT NULL, -- check type
    cohort_name VARCHAR(MAX) NOT NULL,
    description VARCHAR(MAX),
    json VARCHAR(MAX),
    sql_command VARCHAR(MAX)
);

-- link the database_id in the results with the database_meta_data_id
CREATE TABLE @my_schema.@string_to_appenddatabase_details ( -- DATABASE_META_DATA
    database_id INTEGER PRIMARY KEY AUTOINCREMENT,
    database_meta_data_id varchar(MAX) -- databaseId strategus 
);

-- NEW - needs to match stragegus DATABASE_META_DATA
CREATE TABLE @my_schema.@string_to_appendDATABASE_META_DATA (
    database_id varchar(MAX) PRIMARY KEY,
    cdm_source_name varchar(MAX) NOT NULL,
    cdm_source_abbreviation varchar(MAX) NOT NULL,
    CDM_HOLDER varchar(MAX), 
    SOURCE_DESCRIPTION varchar(MAX),
    SOURCE_DOCUMENTATION_REFERENCE varchar(MAX),
    CDM_ETL_REFERENCE varchar(MAX), 
    SOURCE_RELEASE_DATE varchar(MAX), -- not date due to sqlite and consistency
    CDM_RELEASE_DATE varchar(MAX), -- not date due to sqlite and consistency
    CDM_VERSION varchar(MAX),
    VOCABULARY_VERSION varchar(MAX),
    MAX_OBS_PERIOD_END_DATE varchar(MAX) -- not date due to sqlite and consistency
);


CREATE TABLE @my_schema.@string_to_appendtars (
    tar_id INTEGER PRIMARY KEY AUTOINCREMENT,
    tar_start_day int NOT NULL,
	  tar_start_anchor VARCHAR(20) NOT NULL,
    tar_end_day int NOT NULL,
	  tar_end_anchor VARCHAR(20) NOT NULL
);

CREATE TABLE @my_schema.@string_to_appendpopulation_settings(
    population_setting_id INTEGER PRIMARY KEY AUTOINCREMENT,
    population_settings_json VARCHAR(MAX) NOT NULL
);

CREATE TABLE @my_schema.@string_to_appendcovariate_settings(
    covariate_setting_id INTEGER PRIMARY KEY AUTOINCREMENT,
    covariate_settings_json VARCHAR(MAX) NOT NULL
);

CREATE TABLE @my_schema.@string_to_appendmodel_settings(
    model_setting_id INTEGER PRIMARY KEY AUTOINCREMENT,
    model_type VARCHAR(50),
    model_settings_json VARCHAR(MAX)
);

CREATE TABLE @my_schema.@string_to_appendsplit_settings( -- was training_settings
    split_setting_id INTEGER PRIMARY KEY AUTOINCREMENT,
    split_settings_json VARCHAR(MAX)
);

CREATE TABLE @my_schema.@string_to_appendplp_data_settings( -- new
    plp_data_setting_id INTEGER PRIMARY KEY AUTOINCREMENT,
    plp_data_settings_json VARCHAR(MAX)
);

CREATE TABLE @my_schema.@string_to_appendfeature_engineering_settings( -- new
    feature_engineering_setting_id INTEGER PRIMARY KEY AUTOINCREMENT,
    feature_engineering_settings_json VARCHAR(MAX)
);

CREATE TABLE @my_schema.@string_to_appendtidy_covariates_settings( -- new
    tidy_covariates_setting_id INTEGER PRIMARY KEY AUTOINCREMENT,
    tidy_covariates_settings_json VARCHAR(MAX)
);

CREATE TABLE @my_schema.@string_to_appendsample_settings( -- new
    sample_setting_id INTEGER PRIMARY KEY AUTOINCREMENT,
    sample_settings_json VARCHAR(MAX)
);

CREATE TABLE  @my_schema.@string_to_appendmodel_designs (
    model_design_id INTEGER PRIMARY KEY AUTOINCREMENT,
    --model_name CHAR(50) NOT NULL,
    target_id int NOT NULL,
    outcome_id int NOT NULL,
    tar_id int NOT NULL,
    plp_data_setting_id int NOT NULL, -- new
	  population_setting_id int NOT NULL,
	  model_setting_id int NOT NULL,
	  covariate_setting_id int NOT NULL,
	  sample_setting_id int NOT NULL, -- new
	  split_setting_id int NOT NULL, -- new
	  feature_engineering_setting_id int NOT NULL, -- new
	  tidy_covariates_setting_id int NOT NULL, -- new
    FOREIGN KEY (target_id) REFERENCES @string_to_appendcohorts(cohort_id),
    FOREIGN KEY (outcome_id) REFERENCES @string_to_appendcohorts(cohort_id),
    FOREIGN KEY (tar_id) REFERENCES @string_to_appendtars(tar_id),
	  FOREIGN KEY (population_setting_id) REFERENCES @string_to_appendpopulation_settings(population_setting_id),
    FOREIGN KEY (model_setting_id) REFERENCES @string_to_appendmodel_settings(model_setting_id),
	  FOREIGN KEY (covariate_setting_id) REFERENCES @string_to_appendcovariate_settings(covariate_setting_id),
	  FOREIGN KEY (sample_setting_id) REFERENCES @string_to_appendsample_settings(sample_setting_id),  -- new
	  FOREIGN KEY (split_setting_id) REFERENCES @string_to_appendsplit_settings(split_setting_id),  -- new
	  FOREIGN KEY (plp_data_setting_id) REFERENCES @string_to_appendplp_data_settings(plp_data_setting_id), -- new
	  FOREIGN KEY (feature_engineering_setting_id) REFERENCES @string_to_appendfeature_engineering_settings(feature_engineering_setting_id), -- new
	  FOREIGN KEY (tidy_covariates_setting_id) REFERENCES @string_to_appendtidy_covariates_settings(tidy_covariates_setting_id) -- new
);

-- diagnostics holder
CREATE TABLE  @my_schema.@string_to_appenddiagnostics(
   diagnostic_id INTEGER PRIMARY KEY AUTOINCREMENT,
   model_design_id int,
   database_id int NOT NULL,
   execution_date_time VARCHAR(100),
   FOREIGN KEY (model_design_id) REFERENCES @string_to_appendmodel_designs(model_design_id),
   FOREIGN KEY (database_id) REFERENCES @string_to_appenddatabase_details(database_id)
);
CREATE TABLE  @my_schema.@string_to_appenddiagnostic_summary(
   diagnostic_id int NOT NULL,
   probast_id varchar(50),
   result_value varchar(50),
   FOREIGN KEY (diagnostic_id) REFERENCES @string_to_appenddiagnostics(diagnostic_id)
);
CREATE TABLE  @my_schema.@string_to_appenddiagnostic_predictors( -- call this kmplot
   diagnostic_id int NOT NULL,
   days_to_event int,
   outcome_at_time int,
   observed_at_start_of_day bigint,
   probast_id varchar(50),
   input_type varchar(50),
   FOREIGN KEY (diagnostic_id) REFERENCES @string_to_appenddiagnostics(diagnostic_id)
);
CREATE TABLE  @my_schema.@string_to_appenddiagnostic_participants(
   diagnostic_id int NOT NULL,
   design varchar(50),
   metric varchar(50),
   value float,
   probast_id varchar(50),
   FOREIGN KEY (diagnostic_id) REFERENCES @string_to_appenddiagnostics(diagnostic_id)
);
CREATE TABLE  @my_schema.@string_to_appenddiagnostic_outcomes(
   diagnostic_id int NOT NULL,
   xvalue int,
   outcome_percent float,
   aggregation varchar(50),
   probast_id varchar(50),
   input_type varchar(50),
   FOREIGN KEY (diagnostic_id) REFERENCES @string_to_appenddiagnostics(diagnostic_id)
);
CREATE TABLE  @my_schema.@string_to_appenddiagnostic_designs(
   diagnostic_id int NOT NULL,
   probast_id varchar(50),
   value varchar(50),
   FOREIGN KEY (diagnostic_id) REFERENCES @string_to_appenddiagnostics(diagnostic_id)
);
-- end diagnostics

-- results
CREATE TABLE  @my_schema.@string_to_appendmodels(
    model_id INTEGER PRIMARY KEY AUTOINCREMENT,
    analysis_id varchar(50),
    model_design_id int,
    database_id int NOT NULL,
    model_type VARCHAR(50),
    plp_model_file VARCHAR(MAX) NOT NULL, -- reference to saved model location
	  train_details VARCHAR(MAX), -- new this contains all the trainDetails
	  preprocessing VARCHAR(MAX), -- new this contains the preprocessing required
	  execution_date_time VARCHAR(100),
	  training_time VARCHAR(100), -- previously new
	  intercept float,
	  FOREIGN KEY (model_design_id) REFERENCES @string_to_appendmodel_designs(model_design_id),
    FOREIGN KEY (database_id) REFERENCES @string_to_appenddatabase_details(database_id)
);

-- make this relcaibration specific?
CREATE TABLE  @my_schema.@string_to_appendrecalibrations (
    recalibration_id INTEGER PRIMARY KEY AUTOINCREMENT,
	  original_model_id int NOT NULL,
	  recalibrated_model_id int NOT NULL,
	  recalibration_type varchar(15),
	  recalibration_json varchar(MAX),
	  FOREIGN KEY (original_model_id) REFERENCES @string_to_appendmodels(model_id),
	  FOREIGN KEY (recalibrated_model_id) REFERENCES @string_to_appendmodels(model_id)
);

CREATE TABLE  @my_schema.@string_to_appendperformances (
    performance_id INTEGER PRIMARY KEY AUTOINCREMENT,
    model_design_id int NOT NULL,
    development_database_id int NOT NULL,
    validation_database_id int NOT NULL,
    target_id int NOT NULL,
    outcome_id int NOT NULL,
    tar_id int NOT NULL,
    plp_data_setting_id int NOT NULL, -- added
	  population_setting_id int NOT NULL,
	  model_development int NOT NULL, -- added
    execution_date_time VARCHAR(100),
    plp_version char(10),
    FOREIGN KEY (model_design_id) REFERENCES @string_to_appendmodels_designs(model_design_id),
    FOREIGN KEY (development_database_id) REFERENCES @string_to_appenddatabase_details(database_id),
    FOREIGN KEY (validation_database_id) REFERENCES @string_to_appenddatabase_details(database_id),
    FOREIGN KEY (target_id) REFERENCES @string_to_appendcohorts(cohort_id),
    FOREIGN KEY (outcome_id) REFERENCES @string_to_appendcohorts(cohort_id),
    FOREIGN KEY (tar_id) REFERENCES @string_to_appendtars(tar_id),
    FOREIGN KEY (plp_data_setting_id) REFERENCES @string_to_appendplp_data_settings(plp_data_setting_id), -- new
	  FOREIGN KEY (population_setting_id) REFERENCES @string_to_appendpopulation_settings(population_setting_id)
);

-- new
CREATE TABLE @my_schema.@string_to_appendattrition (
	  performance_id int NOT NULL,
	  outcome_id int,
	  description varchar(1000),
	  target_count int, -- is this still target?
	  unique_people int,
	  outcomes int,
    FOREIGN KEY (performance_id) REFERENCES @string_to_appendperformances(performance_id)
);

CREATE TABLE @my_schema.@string_to_appendprediction_distribution (
    --distribution_id int GENERATED ALWAYS AS IDENTITY NOT NULL PRIMARY KEY,
	  performance_id int NOT NULL,
    evaluation VARCHAR(10),
    class_label int,
    person_count int,
    average_predicted_probability float,
    st_dev_predicted_probability float,
    min_predicted_probability float,
    p_05_predicted_probability float,
    p_25_predicted_probability float,
    median_predicted_probability float,
    p_75_predicted_probability float,
    p_95_predicted_probability float,
    max_predicted_probability float,
    FOREIGN KEY (performance_id) REFERENCES @string_to_appendperformances(performance_id)
);

CREATE TABLE @my_schema.@string_to_appendcovariate_summary(
    --cov_sum_id int GENERATED ALWAYS AS IDENTITY NOT NULL PRIMARY KEY,
	performance_id int NOT NULL,
    covariate_id bigint NOT NULL,
	covariate_name VARCHAR(1000) NOT NULL,
    concept_id int NOT NULL,
    covariate_value float NOT NULL,
    covariate_count int NOT NULL,
	covariate_mean float NOT NULL,
	covariate_st_dev float NOT NULL,
	with_no_outcome_covariate_count int NOT NULL,
    with_no_outcome_covariate_mean float NOT NULL,
	with_no_outcome_covariate_st_dev float NOT NULL,
    with_outcome_covariate_count int NOT NULL,
    with_outcome_covariate_mean float NOT NULL,
	with_outcome_covariate_st_dev float NOT NULL,
    standardized_mean_diff float NOT NULL,
    FOREIGN KEY (performance_id) REFERENCES @string_to_appendperformances(performance_id)
);
CREATE TABLE @my_schema.@string_to_appendthreshold_summary(
    --threshold_summary_id int GENERATED ALWAYS AS IDENTITY NOT NULL PRIMARY KEY,
    performance_id int NOT NULL,
    evaluation VARCHAR(10),
    prediction_threshold float,
    preference_threshold float,
    positive_count int,
    negative_count int,
    true_count int,
    false_count int,
    true_positive_count int,
    true_negative_count int,
    false_positive_count int,
    false_negative_count int,
    f_1_score float,
    accuracy float,
    sensitivity float,
    false_negative_rate float,
    false_positive_rate float,
    specificity float,
    positive_predictive_value float,
    false_discovery_rate float,
    negative_predictive_value float,
    false_omission_rate float,
    positive_likelihood_ratio float,
    negative_likelihood_ratio float,
    diagnostic_odds_ratio float,
    FOREIGN KEY (performance_id) REFERENCES @string_to_appendperformances(performance_id)
);

CREATE TABLE @my_schema.@string_to_appendcalibration_summary(
    --calibration_summary_id int GENERATED ALWAYS AS IDENTITY NOT NULL PRIMARY KEY,
    performance_id int NOT NULL,
    evaluation VARCHAR(10),
    prediction_threshold float,
    person_count_at_risk int,
    person_count_with_outcome int,
    average_predicted_probability float,
    st_dev_predicted_probability float,
    min_predicted_probability float,
    p_25_predicted_probability float,
    median_predicted_probability float,
    p_75_predicted_probability float,
    max_predicted_probability float,
    observed_incidence float,
    FOREIGN KEY (performance_id) REFERENCES @string_to_appendperformances(performance_id)
);

CREATE TABLE @my_schema.@string_to_appendevaluation_statistics (
    --evaluation_stat_id int GENERATED ALWAYS AS IDENTITY NOT NULL PRIMARY KEY,
	performance_id int NOT NULL,
	evaluation VARCHAR(10),
  metric varchar(50),
	value float,
    FOREIGN KEY (performance_id) REFERENCES @string_to_appendperformances(performance_id)
);

CREATE TABLE @my_schema.@string_to_appenddemographic_summary(
    --demographic_summary_id int GENERATED ALWAYS AS IDENTITY NOT NULL PRIMARY KEY,
    performance_id int NOT NULL,
    evaluation VARCHAR(10),
    age_group VARCHAR(20),
    gen_group VARCHAR(20),
    person_count_at_risk int,
    person_count_with_outcome int,
    average_predicted_probability float,
    st_dev_predicted_probability float,
    min_predicted_probability float,
    p_25_predicted_probability float,
    p_50_predicted_probability float,
    p_75_predicted_probability float,
    max_predicted_probability float,
    FOREIGN KEY (performance_id) REFERENCES @string_to_appendperformances(performance_id)
);

