-- do we need this
CREATE TABLE @my_schema.@string_to_appendstudies (
    study_id int GENERATED ALWAYS AS IDENTITY NOT NULL PRIMARY KEY,
	  study_name varchar(100),
    study_description varchar(1000)
);

CREATE TABLE @my_schema.@string_to_appendcohorts (
    cohort_id int GENERATED ALWAYS AS IDENTITY NOT NULL PRIMARY KEY,
	atlas_id bigint,
    cohort_name char(100) NOT NULL,
    cohort_json VARCHAR(MAX) NOT NULL
);

CREATE TABLE @my_schema.@string_to_appendresearchers (
    researcher_id int GENERATED ALWAYS AS IDENTITY NOT NULL PRIMARY KEY,
    researcher_name char(100) NOT NULL,
    researcher_email char(100) NOT NULL,
    researcher_affiliation char(250) NOT NULL
);

CREATE TABLE @my_schema.@string_to_appenddatabase_details (
    database_id int GENERATED ALWAYS AS IDENTITY NOT NULL PRIMARY KEY,
    database_name char(100) NOT NULL,
    database_acronym char(20) NOT NULL,
	  database_version int NOT NULL,
    database_description char(1000) NOT NULL,
    database_type char(20) NOT NULL
);

CREATE TABLE @my_schema.@string_to_appendtars (
    tar_id int GENERATED ALWAYS AS IDENTITY NOT NULL PRIMARY KEY,
    tar_start_day int NOT NULL,
	  tar_start_anchor char(20) NOT NULL,
    tar_end_day int NOT NULL,
	  tar_end_anchor char(20) NOT NULL
);

CREATE TABLE @my_schema.@string_to_appendpopulation_settings(
    population_setting_id int GENERATED ALWAYS AS IDENTITY NOT NULL PRIMARY KEY,
    population_settings_json VARCHAR(MAX) NOT NULL
);

CREATE TABLE @my_schema.@string_to_appendcovariate_settings(
    covariate_setting_id int GENERATED ALWAYS AS IDENTITY NOT NULL PRIMARY KEY,
    covariate_settings_json VARCHAR(MAX) NOT NULL
);

CREATE TABLE @my_schema.@string_to_appendmodel_settings(
    model_setting_id int GENERATED ALWAYS AS IDENTITY NOT NULL PRIMARY KEY,
    model_type VARCHAR(50),
    model_settings_json VARCHAR(MAX)
);

CREATE TABLE @my_schema.@string_to_appendsplit_settings( -- was training_settings
    split_setting_id int GENERATED ALWAYS AS IDENTITY NOT NULL PRIMARY KEY,
    split_settings_json VARCHAR(MAX)
);

CREATE TABLE @my_schema.@string_to_appendplp_data_settings( -- new
    plp_data_setting_id int GENERATED ALWAYS AS IDENTITY NOT NULL PRIMARY KEY,
    plp_data_settings_json VARCHAR(MAX)
);

CREATE TABLE @my_schema.@string_to_appendfeature_engineering_settings( -- new
    feature_engineering_setting_id int GENERATED ALWAYS AS IDENTITY NOT NULL PRIMARY KEY,
    feature_engineering_settings_json VARCHAR(MAX)
);

CREATE TABLE @my_schema.@string_to_appendtidy_covariates_settings( -- new
    tidy_covariates_setting_id int GENERATED ALWAYS AS IDENTITY NOT NULL PRIMARY KEY,
    tidy_covariates_settings_json VARCHAR(MAX)
);

CREATE TABLE @my_schema.@string_to_appendsample_settings( -- new
    sample_setting_id int GENERATED ALWAYS AS IDENTITY NOT NULL PRIMARY KEY,
    sample_settings_json VARCHAR(MAX)
);

CREATE TABLE  @my_schema.@string_to_appendmodel_designs (
    model_design_id int GENERATED ALWAYS AS IDENTITY NOT NULL PRIMARY KEY,
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
    researcher_id int NOT NULL,
    FOREIGN KEY (target_id) REFERENCES @my_schema.@string_to_appendcohorts(cohort_id),
    FOREIGN KEY (outcome_id) REFERENCES @my_schema.@string_to_appendcohorts(cohort_id),
    FOREIGN KEY (researcher_id) REFERENCES @my_schema.@string_to_appendresearchers(researcher_id),
	  FOREIGN KEY (tar_id) REFERENCES @my_schema.@string_to_appendtars(tar_id),
	  FOREIGN KEY (population_setting_id) REFERENCES @my_schema.@string_to_appendpopulation_settings(population_setting_id),
    FOREIGN KEY (model_setting_id) REFERENCES @my_schema.@string_to_appendmodel_settings(model_setting_id),
	  FOREIGN KEY (covariate_setting_id) REFERENCES @my_schema.@string_to_appendcovariate_settings(covariate_setting_id),
	  FOREIGN KEY (sample_setting_id) REFERENCES @my_schema.@string_to_appendsample_settings(sample_setting_id),  -- new
	  FOREIGN KEY (split_setting_id) REFERENCES @my_schema.@string_to_appendsplit_settings(split_setting_id),  -- new
	  FOREIGN KEY (plp_data_setting_id) REFERENCES @my_schema.@string_to_appendplp_data_settings(plp_data_setting_id), -- new
	  FOREIGN KEY (feature_engineering_setting_id) REFERENCES @my_schema.@string_to_appendfeature_engineering_settings(feature_engineering_setting_id), -- new
	  FOREIGN KEY (tidy_covariates_setting_id) REFERENCES @my_schema.@string_to_appendtidy_covariates_settings(tidy_covariates_setting_id) -- new
);

-- diagnostics holder (will add more tables)
CREATE TABLE  @my_schema.@string_to_appenddiagnostics(
   diagnostic_id int GENERATED ALWAYS AS IDENTITY NOT NULL PRIMARY KEY,
   model_design_id int,
   researcher_id int,
   database_id int NOT NULL,
   execution_date_time DATETIME2,
   FOREIGN KEY (researcher_id) REFERENCES @my_schema.@string_to_appendresearchers(researcher_id),
   FOREIGN KEY (model_design_id) REFERENCES @my_schema.@string_to_appendmodel_designs(model_design_id),
   FOREIGN KEY (database_id) REFERENCES @my_schema.@string_to_appenddatabase_details(database_id)
);


-- results
CREATE TABLE  @my_schema.@string_to_appendmodels(
    model_id int GENERATED ALWAYS AS IDENTITY NOT NULL PRIMARY KEY,
    researcher_id int,
    analysis_id varchar(50),
    model_design_id int,
    database_id int NOT NULL,
	  hyper_param_search VARCHAR(MAX), -- new this contains the hyperparameter performances
    plp_model_file char(50) NOT NULL,
	  execution_date_time DATETIME2,
	  training_time VARCHAR(100), -- previously new
	  intercept float,
	  require_dense_matrix char(1), -- new
	  FOREIGN KEY (researcher_id) REFERENCES @my_schema.@string_to_appendresearchers(researcher_id),
	  FOREIGN KEY (model_design_id) REFERENCES @my_schema.@string_to_appendmodel_designs(model_design_id),
    FOREIGN KEY (database_id) REFERENCES @my_schema.@string_to_appenddatabase_details(database_id)
);

-- do we need this?
CREATE TABLE @my_schema.@string_to_appendstudy_models (
    study_id int,
	  model_id int,
	  FOREIGN KEY (study_id) REFERENCES @my_schema.@string_to_appendstudies(study_id),
    FOREIGN KEY (model_id) REFERENCES @my_schema.@string_to_appendmodels(model_id)
);

-- make this relcaibration specific?
CREATE TABLE  @my_schema.@string_to_appendrecalibrations (
    recalibration_id int GENERATED ALWAYS AS IDENTITY NOT NULL PRIMARY KEY,
	  original_model_id int NOT NULL,
	  recalibrated_model_id int NOT NULL,
	  recalibration_type varchar(15),
	  recalibration_json varchar(MAX),
	  FOREIGN KEY (original_model_id) REFERENCES @my_schema.@string_to_appendmodels(model_id),
	  FOREIGN KEY (recalibrated_model_id) REFERENCES @my_schema.@string_to_appendmodels(model_id)
);

CREATE TABLE  @my_schema.@string_to_appendresults (
    result_id int GENERATED ALWAYS AS IDENTITY NOT NULL PRIMARY KEY,
    model_id int NOT NULL,
    researcher_id int NOT NULL,
    database_id int NOT NULL,
    target_id int NOT NULL,
    outcome_id int NOT NULL,
    tar_id int NOT NULL,
    plp_data_setting_id int NOT NULL, -- added
	  population_setting_id int NOT NULL,
    execution_date_time DATETIME2,
    plp_version char(10),
    FOREIGN KEY (model_id) REFERENCES @my_schema.@string_to_appendmodels(model_id),
    FOREIGN KEY (researcher_id) REFERENCES @my_schema.@string_to_appendresearchers(researcher_id),
    FOREIGN KEY (database_id) REFERENCES @my_schema.@string_to_appenddatabase_details(database_id),
    FOREIGN KEY (target_id) REFERENCES @my_schema.@string_to_appendcohorts(cohort_id),
    FOREIGN KEY (outcome_id) REFERENCES @my_schema.@string_to_appendcohorts(cohort_id),
    FOREIGN KEY (tar_id) REFERENCES @my_schema.@string_to_appendtars(tar_id),
    FOREIGN KEY (plp_data_setting_id) REFERENCES @my_schema.@string_to_appendplp_data_settings(plp_data_setting_id), -- new
	  FOREIGN KEY (population_setting_id) REFERENCES @my_schema.@string_to_appendpopulation_settings(population_setting_id)
);

-- new
CREATE TABLE @my_schema.@string_to_appendattrition (
	  result_id int NOT NULL,
	  outcome_id int,
	  description varchar(1000),
	  target_count int,
	  unique_people int,
	  outcomes int,
    FOREIGN KEY (result_id) REFERENCES @my_schema.@string_to_appendresults(result_id)
);

CREATE TABLE @my_schema.@string_to_appendprediction_distribution (
    --distribution_id int GENERATED ALWAYS AS IDENTITY NOT NULL PRIMARY KEY,
	  result_id int NOT NULL,
    evaluation char(10),
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
    FOREIGN KEY (result_id) REFERENCES @my_schema.@string_to_appendresults(result_id)
);

CREATE TABLE @my_schema.@string_to_appendcovariate_summary(
    --cov_sum_id int GENERATED ALWAYS AS IDENTITY NOT NULL PRIMARY KEY,
	result_id int NOT NULL,
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
    FOREIGN KEY (result_id) REFERENCES @my_schema.@string_to_appendresults(result_id)
);
CREATE TABLE @my_schema.@string_to_appendthreshold_summary(
    --threshold_summary_id int GENERATED ALWAYS AS IDENTITY NOT NULL PRIMARY KEY,
    result_id int NOT NULL,
    evaluation char(10),
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
    FOREIGN KEY (result_id) REFERENCES @my_schema.@string_to_appendresults(result_id)
);

CREATE TABLE @my_schema.@string_to_appendcalibration_summary(
    --calibration_summary_id int GENERATED ALWAYS AS IDENTITY NOT NULL PRIMARY KEY,
    result_id int NOT NULL,
    evaluation char(10),
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
    FOREIGN KEY (result_id) REFERENCES @my_schema.@string_to_appendresults(result_id)
);

CREATE TABLE @my_schema.@string_to_appendevaluation_statistics (
    --evaluation_stat_id int GENERATED ALWAYS AS IDENTITY NOT NULL PRIMARY KEY,
	result_id int NOT NULL,
	evaluation char(10),
  metric varchar(50),
	value float,
    FOREIGN KEY (result_id) REFERENCES @my_schema.@string_to_appendresults(result_id)
);

CREATE TABLE @my_schema.@string_to_appenddemographic_summary(
    --demographic_summary_id int GENERATED ALWAYS AS IDENTITY NOT NULL PRIMARY KEY,
    result_id int NOT NULL,
    evaluation char(10),
    age_group char(20),
    gen_group char(20),
    person_count_at_risk int,
    person_count_with_outcome int,
    average_predicted_probability float,
    st_dev_predicted_probability float,
    min_predicted_probability float,
    p_25_predicted_probability float,
    p_50_predicted_probability float,
    p_75_predicted_probability float,
    max_predicted_probability float,
    FOREIGN KEY (result_id) REFERENCES @my_schema.@string_to_appendresults(result_id)
);
