/************************************************************************
@file GetHdpsCovariates.sql

Copyright 2015 Observational Health Data Sciences and Informatics

This file is part of CohortMethod

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
************************************************************************/

{DEFAULT @cdm_database = 'CDM4_SIM' } 
{DEFAULT @cdm_version == '4'}
{DEFAULT @cohort_temp_table = '@cohort_person'}
{DEFAULT @row_id_field = 'person_id'}
{DEFAULT @cohort_definition_id = 'cohort_concept_id'} 
{DEFAULT @concept_class_id = 'concept_class'} 
{DEFAULT @measurement = 'observation'} 
{DEFAULT @use_covariate_cohort_id_is_1 = FALSE}
{DEFAULT @use_covariate_demographics = TRUE} 
{DEFAULT @use_covariate_demographics_age = TRUE} 
{DEFAULT @use_covariate_demographics_gender = TRUE} 
{DEFAULT @use_covariate_demographics_race = TRUE} 
{DEFAULT @use_covariate_demographics_ethnicity = TRUE} 
{DEFAULT @use_covariate_demographics_year = TRUE} 
{DEFAULT @use_covariate_demographics_month = TRUE} 
{DEFAULT @use_covariate_condition_occurrence = TRUE} 
{DEFAULT @use_covariate_3_digit_icd_9_inpatient_180d = TRUE}
{DEFAULT @use_covariate_3_digit_icd_9_inpatient_180d_med_f = TRUE}
{DEFAULT @use_covariate_3_digit_icd_9_inpatient_180d_75_f = TRUE}
{DEFAULT @use_covariate_3_digit_icd_9_ambulatory_180d = TRUE}
{DEFAULT @use_covariate_3_digit_icd_9_ambulatory_180d_med_f = TRUE}
{DEFAULT @use_covariate_3_digit_icd_9_ambulatory_180d_75_f = TRUE}
{DEFAULT @use_covariate_drug_exposure = FALSE} 
{DEFAULT @use_covariate_ingredient_exposure_180d = TRUE}
{DEFAULT @use_covariate_ingredient_exposure_180d_med_f = TRUE}
{DEFAULT @use_covariate_ingredient_exposure_180d_75_f = TRUE}
{DEFAULT @use_covariate_procedure_occurrence = FALSE} 
{DEFAULT @use_covariate_inpatient_procedure_occurrence_180d = TRUE}
{DEFAULT @use_covariate_inpatient_procedure_occurrence_180d_med_f = TRUE}
{DEFAULT @use_covariate_inpatient_procedure_occurrence_180d_75_f = TRUE}
{DEFAULT @use_covariate_ambulatory_procedure_occurrence_180d = TRUE}
{DEFAULT @use_covariate_ambulatory_procedure_occurrence_180d_med_f = TRUE}
{DEFAULT @use_covariate_ambulatory_procedure_occurrence_180d_75_f = TRUE}
{DEFAULT @has_excluded_covariate_concept_ids} 
{DEFAULT @has_included_covariate_concept_ids} 
{DEFAULT @delete_covariates_small_count = 100}

USE @cdm_database;

IF OBJECT_ID('tempdb..#cov', 'U') IS NOT NULL
	DROP TABLE #cov;

IF OBJECT_ID('tempdb..#cov_ref', 'U') IS NOT NULL
	DROP TABLE #cov_ref;

CREATE TABLE #cov_ref (
	covariate_id BIGINT,
	covariate_name VARCHAR(512),
	analysis_id INT,
	concept_id INT
	);
	
IF OBJECT_ID('tempdb..#dummy', 'U') IS NOT NULL
	DROP TABLE #dummy;

CREATE TABLE #dummy (
	row_id BIGINT,
	covariate_id BIGINT,
	covariate_value INT
	);
	
{@use_covariate_cohort_id_is_1} ? {
--covariate for exposure status, determining which patients are in which treatment group (only those in cohort 1 will get recorded)
INSERT INTO #cov_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
VALUES (
	1,
	'Cohort definition ID',
	1,
	0
	);


SELECT 
	@row_id_field AS row_id,
	1 AS covariate_id,
	@cohort_definition_id AS covariate_value
INTO #cov_exposure
FROM @cohort_temp_table
WHERE @cohort_definition_id = 1;
}

/**************************
***************************
DEMOGRAPHICS
***************************
**************************/
{@use_covariate_demographics} ? {



{@use_covariate_demographics_gender} ? {
--gender
SELECT cp1.@row_id_field AS row_id,
	gender_concept_id AS covariate_id,
	1 AS covariate_value
INTO #cov_gender
FROM @cohort_temp_table cp1
INNER JOIN person p1
	ON cp1.subject_id = p1.person_id
WHERE p1.gender_concept_id IN (
		SELECT concept_id
		FROM concept
		WHERE LOWER(@concept_class_id) = 'gender'
		);


INSERT INTO #cov_ref (
  covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
SELECT p1.covariate_id,
	'Gender = ' +
    CASE WHEN v1.concept_name IS NOT NULL
			THEN v1.concept_name
		ELSE 'Unknown invalid concept'
		END AS covariate_name,
	2 AS analysis_id,
	p1.covariate_id AS concept_id
FROM (SELECT distinct covariate_id FROM #cov_gender) p1
LEFT JOIN (
	SELECT concept_id,
		concept_name
	FROM concept
	WHERE LOWER(@concept_class_id) = 'gender'
	) v1
	ON p1.covariate_id = v1.concept_id;

}


{@use_covariate_demographics_race} ? {
--race
SELECT cp1.@row_id_field AS row_id,
	race_concept_id AS covariate_id,
	1 AS covariate_value
  INTO #cov_race
FROM @cohort_temp_table cp1
INNER JOIN person p1
	ON cp1.subject_id = p1.person_id
WHERE p1.race_concept_id IN (
		SELECT concept_id
		FROM concept
		WHERE LOWER(@concept_class_id) = 'race'
		);


INSERT INTO #cov_ref (
  covariate_id,
  covariate_name,
	analysis_id,
	concept_id
	)
SELECT p1.covariate_id,
	'Race = ' + CASE WHEN v1.concept_name IS NOT NULL
  		THEN v1.concept_name
		ELSE 'Unknown invalid concept'
		END  AS covariate_name,
	3 AS analysis_id,
	p1.covariate_id AS concept_id
FROM (SELECT distinct covariate_id FROM #cov_race) p1
LEFT JOIN (
	SELECT concept_id,
		concept_name
	FROM concept
	WHERE LOWER(@concept_class_id) = 'race'
	) v1
	ON p1.covariate_id = v1.concept_id;


}

{@use_covariate_demographics_ethnicity} ? {
--ethnicity
SELECT cp1.@row_id_field AS row_id,
	ethnicity_concept_id AS covariate_id,
	1 AS covariate_value
  INTO #cov_ethnicity
FROM @cohort_temp_table cp1
INNER JOIN person p1
	ON cp1.subject_id = p1.person_id
WHERE p1.ethnicity_concept_id IN (
		SELECT concept_id
		FROM concept
		WHERE LOWER(@concept_class_id) = 'ethnicity'
		);



INSERT INTO #cov_ref (
  covariate_id,
  covariate_name,
  analysis_id,
	concept_id
	)
SELECT p1.covariate_id,
	'Ethnicity = ' + CASE WHEN v1.concept_name IS NOT NULL
  		THEN v1.concept_name
		ELSE 'Unknown invalid concept'
		END  AS covariate_name,
	4 AS analysis_id,
	p1.covariate_id AS concept_id
FROM (SELECT distinct covariate_id FROM #cov_ethnicity) p1
LEFT JOIN (
	SELECT concept_id,
		concept_name
	FROM concept
	WHERE LOWER(@concept_class_id) = 'ethnicity'
	) v1
	ON p1.covariate_id = v1.concept_id;


}


{@use_covariate_demographics_age} ? {
--age group
SELECT cp1.@row_id_field AS row_id,
	FLOOR((YEAR(cp1.cohort_start_date) - p1.YEAR_OF_BIRTH) / 5) + 10 AS covariate_id,
	1 AS covariate_value
    INTO #cov_age
FROM @cohort_temp_table cp1
INNER JOIN person p1
	ON cp1.subject_id = p1.person_id
WHERE (YEAR(cp1.cohort_start_date) - p1.YEAR_OF_BIRTH) >= 0
	AND (YEAR(cp1.cohort_start_date) - p1.YEAR_OF_BIRTH) < 100;




INSERT INTO #cov_ref (
  covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
SELECT p1.covariate_id,
	'Age group: ' + CAST((covariate_id-10)*5 AS VARCHAR) + '-' + CAST((covariate_id-10+1)*5-1 AS VARCHAR)  AS covariate_name,
	4 AS analysis_id,
	0 AS concept_id
FROM (select distinct covariate_id FROM #cov_age) p1
;



}


{@use_covariate_demographics_year} ? {
--index year
SELECT cp1.@row_id_field AS row_id,
	YEAR(cohort_start_date) AS covariate_id,
	1 AS covariate_value
    INTO #cov_year
FROM @cohort_temp_table cp1;


INSERT INTO #cov_ref (
  covariate_id,
  covariate_name,
	analysis_id,
	concept_id
	)
SELECT p1.covariate_id,
	'Index year: ' + CAST(covariate_id AS VARCHAR)  AS covariate_name,
	5 AS analysis_id,
	0 AS concept_id
FROM (select distinct covariate_id FROM #cov_year) p1
;

}


{@use_covariate_demographics_month} ? {

--index month

SELECT cp1.@row_id_field AS row_id,
	MONTH(cohort_start_date) + 40 AS covariate_id,
	1 AS covariate_value
    INTO #cov_month
FROM @cohort_temp_table cp1;


INSERT INTO #cov_ref (
  covariate_id,
  covariate_name,
  analysis_id,
	concept_id
	)
SELECT p1.covariate_id,
	'Index month: ' + CAST(covariate_id-40 AS VARCHAR)  AS covariate_name,
	6 AS analysis_id,
	0 AS concept_id
FROM (select distinct covariate_id FROM #cov_month) p1
;

}

}



/**************************
***************************
CONDITION OCCURRENCE
***************************
**************************/
{@use_covariate_condition_occurrence} ? { 
	
	
{@use_covariate_3_digit_icd_9_inpatient_180d | @use_covariate_3_digit_icd_9_ambulatory_180d | @use_covariate_3_digit_icd_9_inpatient_180d_med_f | @use_covariate_3_digit_icd_9_ambulatory_180d_med_f | @use_covariate_3_digit_icd_9_inpatient_180d_75_f | @use_covariate_3_digit_icd_9_ambulatory_180d_75_f } ? {
IF OBJECT_ID('tempdb..#condition_id_to_icd9', 'U') IS NOT NULL
	DROP TABLE #condition_id_to_icd9;
	
-- Create unique numeric identifiers for 3 digit ICD9 codes, and map condition occurrence codes	
{@cdm_version == '4'} ? {
SELECT DISTINCT LEFT(source_to_concept_map.source_code, 3) AS icd9,
	CASE 
	  WHEN LEFT(source_to_concept_map.source_code,1) = 'E' THEN 1000 + CAST(RIGHT(LEFT(source_to_concept_map.source_code,3),2) AS INT)
	  WHEN LEFT(source_to_concept_map.source_code,1) = 'V' THEN 1100 + CAST(RIGHT(LEFT(source_to_concept_map.source_code,3),2) AS INT)
	  ELSE CAST(LEFT(source_to_concept_map.source_code,3) AS INT) END AS icd9_concept_id,
	ISNULL(icd9_concept_name, '') AS icd9_concept_name,
	target_concept_id AS condition_concept_id
INTO #temp
FROM source_to_concept_map
LEFT JOIN (
	SELECT source_code AS icd9,
		source_code_description AS icd9_concept_name
	FROM source_to_concept_map
	WHERE source_vocabulary_id = 2
		AND target_vocabulary_id = 1
		AND LEN(source_code) = 3
	) icd9_to_new_concept_id
	ON icd9_to_new_concept_id.icd9 = LEFT(source_to_concept_map.source_code, 3)
WHERE source_vocabulary_id = 2
	AND target_vocabulary_id = 1
	AND source_to_concept_map.invalid_reason IS NULL;
} : {
SELECT DISTINCT LEFT(icd9.concept_code, 3) AS icd9,
   CASE 
	  WHEN LEFT(icd9.concept_code,1) = 'E' THEN 1000 + CAST(RIGHT(LEFT(icd9.concept_code,3),2) AS INT)
	  WHEN LEFT(icd9.concept_code,1) = 'V' THEN 1100 + CAST(RIGHT(LEFT(icd9.concept_code,3),2) AS INT)
	  ELSE CAST(LEFT(icd9.concept_code,3) AS INT) END AS icd9_concept_id,
	ISNULL(icd9_concept_name, '') AS icd9_concept_name,
	condition.concept_id AS condition_concept_id
INTO #temp
FROM concept_relationship
INNER JOIN concept icd9
	ON concept_id_1 = icd9.concept_id
INNER JOIN concept condition
	ON concept_id_2 = condition.concept_id
LEFT JOIN (
	SELECT concept_code AS icd9,
		concept_id AS icd9_concept_id,
		concept_name AS icd9_concept_name
	FROM concept
	WHERE vocabulary_id = 'ICD9CM'
		AND LEN(concept_code) = 3
	) icd9_to_new_concept_id
	ON icd9 = LEFT(icd9.concept_code, 3)
WHERE condition.standard_concept = 'S'
	AND relationship_id = 'Maps to'
	AND icd9.vocabulary_id = 'ICD9CM'
	AND icd9.invalid_reason IS NULL
	AND concept_relationship.invalid_reason IS NULL;
}

-- If condition_concept_id maps to more than one ICD9 code, just pick one:
SELECT icd9,
	icd9_concept_id,
	icd9_concept_name,
	condition_concept_id
INTO #condition_id_to_icd9
FROM (
	SELECT icd9,
		icd9_concept_id,
		icd9_concept_name,
		condition_concept_id,
		ROW_NUMBER() OVER (
			PARTITION BY condition_concept_id ORDER BY icd9
			) AS rn
	FROM #temp
	) tmp
WHERE rn = 1;

TRUNCATE TABLE #temp;
DROP TABLE #temp;
}

{@use_covariate_3_digit_icd_9_inpatient_180d | @use_covariate_3_digit_icd_9_inpatient_180d_med_f | @use_covariate_3_digit_icd_9_inpatient_180d_75_f} ? {
SELECT cp1.@row_id_field AS row_id,
	icd9_concept_id,
	COUNT_BIG(*) AS frequency
INTO #freq
FROM @cohort_temp_table cp1
INNER JOIN condition_occurrence co1
	ON cp1.subject_id = co1.person_id
INNER JOIN visit_occurrence vo1
    ON co1.visit_occurrence_id = vo1.visit_occurrence_id
INNER JOIN #condition_id_to_icd9 condition_id_to_icd9
	ON co1.condition_concept_id = condition_id_to_icd9.condition_concept_id 
WHERE co1.condition_concept_id != 0
{@has_excluded_covariate_concept_ids} ? {	AND co1.condition_concept_id NOT IN (SELECT concept_id FROM #excluded_cov)}
{@has_included_covariate_concept_ids} ? {	AND co1.condition_concept_id IN (SELECT concept_id FROM #included_cov)}
{@cdm_version == '4'} ? {
	AND vo1.place_of_service_concept_id = 9201
} : {
	AND vo1.visit_concept_id = 9201
}
	AND co1.condition_start_date <= cp1.cohort_start_date
	AND co1.condition_start_date >= dateadd(dd, - 180, cp1.cohort_start_date)
GROUP BY cp1.@row_id_field,
	icd9_concept_id;
}

{@use_covariate_3_digit_icd_9_inpatient_180d_med_f | @use_covariate_3_digit_icd_9_inpatient_180d_75_f} ? {	
SELECT icd9_concept_id,
    MAX(CASE WHEN quantile <= 0.50 THEN frequency ELSE -9999 END) AS median_value,
	MAX(CASE WHEN quantile <= 0.75 THEN frequency ELSE -9999 END) AS q75_value
INTO #thresholds  
FROM (
	SELECT icd9_concept_id,
	  frequency,
	  1.0 * ROW_NUMBER() OVER (PARTITION BY icd9_concept_id ORDER BY frequency) / COUNT_BIG(*) OVER (PARTITION BY icd9_concept_id) AS quantile
	FROM #freq
   ) #temp	
GROUP BY icd9_concept_id;
}

{@use_covariate_3_digit_icd_9_inpatient_180d} ? {
--conditions:  inpatient diagnoses in last 180d at 3-digit ICD9 code level
SELECT DISTINCT row_id,
	CAST(icd9_concept_id AS BIGINT) * 1000 + 104 AS covariate_id,
	1 AS covariate_value
INTO #cov_co_i_icd_180d
FROM #freq;

INSERT INTO #cov_ref (
  covariate_id,
  covariate_name,
  analysis_id,
  concept_id
	)
SELECT p1.covariate_id,
	'3-digit ICD-9 occurrence record of inpatient diagnosis observed during 180d on or prior to cohort index: ' + icd9 + '-' + c1.icd9_concept_name AS covariate_name,
	104 AS analysis_id,
	0 AS concept_id
FROM (SELECT DISTINCT covariate_id FROM #cov_co_i_icd_180d) p1
INNER JOIN (SELECT DISTINCT icd9_concept_id, icd9, icd9_concept_name FROM #condition_id_to_icd9) c1
	ON (p1.covariate_id-104)/1000 = c1.icd9_concept_id;
} 

{@use_covariate_3_digit_icd_9_inpatient_180d_med_f} ? {
SELECT DISTINCT row_id,
	CAST(freq.icd9_concept_id AS BIGINT) * 1000 + 105 AS covariate_id,
	1 AS covariate_value
INTO #cov_co_i_icd_180d_m
FROM #freq freq
INNER JOIN #thresholds thresholds
ON freq.icd9_concept_id = thresholds.icd9_concept_id
WHERE frequency >= median_value
  AND median_value > 1;

INSERT INTO #cov_ref (
  covariate_id,
  covariate_name,
  analysis_id,
  concept_id
	)
SELECT p1.covariate_id,
	'3-digit ICD-9 occurrence record of inpatient diagnosis observed during 180d on or prior to cohort index with freq >= median: ' + icd9 + '-' + c1.icd9_concept_name AS covariate_name,
	105 AS analysis_id,
	0 AS concept_id
FROM (SELECT DISTINCT covariate_id FROM #cov_co_i_icd_180d_m) p1
INNER JOIN (SELECT DISTINCT icd9_concept_id, icd9, icd9_concept_name FROM #condition_id_to_icd9) c1
	ON (p1.covariate_id-105)/1000 = c1.icd9_concept_id;
}

{@use_covariate_3_digit_icd_9_inpatient_180d_75_f} ? {
SELECT DISTINCT row_id,
	CAST(freq.icd9_concept_id AS BIGINT) * 1000 + 106 AS covariate_id,
	1 AS covariate_value
INTO #cov_co_i_icd_180d_75
FROM #freq freq
INNER JOIN #thresholds thresholds
ON freq.icd9_concept_id = thresholds.icd9_concept_id
WHERE frequency >= q75_value
  AND q75_value > median_value;

INSERT INTO #cov_ref (
  covariate_id,
  covariate_name,
  analysis_id,
  concept_id
	)
SELECT p1.covariate_id,
	'3-digit ICD-9 occurrence record of inpatient diagnosis observed during 180d on or prior to cohort index  with freq >= q75: ' + icd9 + '-' + c1.icd9_concept_name AS covariate_name,
	106 AS analysis_id,
	0 AS concept_id
FROM (SELECT DISTINCT covariate_id FROM #cov_co_i_icd_180d_75) p1
INNER JOIN (SELECT DISTINCT icd9_concept_id, icd9, icd9_concept_name FROM #condition_id_to_icd9) c1
	ON (p1.covariate_id-106)/1000 = c1.icd9_concept_id;
}

{@use_covariate_3_digit_icd_9_inpatient_180d | @use_covariate_3_digit_icd_9_inpatient_180d_med_f | @use_covariate_3_digit_icd_9_inpatient_180d_75_f } ? {
TRUNCATE TABLE #freq;
DROP TABLE #freq;
}

{@use_covariate_3_digit_icd_9_inpatient_180d_med_f | @use_covariate_3_digit_icd_9_inpatient_180d_75_f } ? {
TRUNCATE TABLE #thresholds;
DROP TABLE #thresholds;
}

{@use_covariate_3_digit_icd_9_ambulatory_180d | @use_covariate_3_digit_icd_9_ambulatory_180d_med_f | @use_covariate_3_digit_icd_9_ambulatory_180d_75_f} ? {
SELECT @row_id_field AS row_id,
	icd9_concept_id,
	COUNT_BIG(*) AS frequency
INTO #freq
FROM @cohort_temp_table cp1
INNER JOIN condition_occurrence co1
	ON cp1.subject_id = co1.person_id
INNER JOIN visit_occurrence vo1
    ON co1.visit_occurrence_id = vo1.visit_occurrence_id
INNER JOIN #condition_id_to_icd9 condition_id_to_icd9
	ON co1.condition_concept_id = condition_id_to_icd9.condition_concept_id 
WHERE co1.condition_concept_id != 0
{@has_excluded_covariate_concept_ids} ? {	AND co1.condition_concept_id NOT IN (SELECT concept_id FROM #excluded_cov)}
{@has_included_covariate_concept_ids} ? {	AND co1.condition_concept_id IN (SELECT concept_id FROM #included_cov)}
{@cdm_version == '4'} ? {
	AND vo1.place_of_service_concept_id != 9201
} : {
	AND vo1.visit_concept_id != 9201
}
	AND co1.condition_start_date <= cp1.cohort_start_date
	AND co1.condition_start_date >= dateadd(dd, - 180, cp1.cohort_start_date)
GROUP BY cp1.@row_id_field,
	icd9_concept_id;
}

{@use_covariate_3_digit_icd_9_ambulatory_180d_med_f | @use_covariate_3_digit_icd_9_ambulatory_180d_75_f} ? {	
SELECT icd9_concept_id,
    MAX(CASE WHEN quantile <= 0.50 THEN frequency ELSE -9999 END) AS median_value,
	MAX(CASE WHEN quantile <= 0.75 THEN frequency ELSE -9999 END) AS q75_value
INTO #thresholds  
FROM (
	SELECT icd9_concept_id,
	  frequency,
	  1.0 * ROW_NUMBER() OVER (PARTITION BY icd9_concept_id ORDER BY frequency) / COUNT_BIG(*) OVER (PARTITION BY icd9_concept_id) AS quantile
	FROM #freq
   ) #temp	
GROUP BY icd9_concept_id;
}

{@use_covariate_3_digit_icd_9_ambulatory_180d} ? {
--conditions:  ambulatory diagnoses in last 180d at 3-digit ICD9 code level
SELECT DISTINCT row_id,
	CAST(icd9_concept_id AS BIGINT) * 1000 + 107 AS covariate_id,
	1 AS covariate_value
INTO #cov_co_a_icd_180d
FROM #freq;

INSERT INTO #cov_ref (
  covariate_id,
  covariate_name,
  analysis_id,
  concept_id
	)
SELECT p1.covariate_id,
	'3-digit ICD-9 occurrence record of ambulatory diagnosis observed during 180d on or prior to cohort index: ' + icd9 + '-' + c1.icd9_concept_name AS covariate_name,
	107 AS analysis_id,
	0 AS concept_id
FROM (SELECT DISTINCT covariate_id FROM #cov_co_a_icd_180d) p1
INNER JOIN (SELECT DISTINCT icd9_concept_id, icd9, icd9_concept_name FROM #condition_id_to_icd9) c1
	ON (p1.covariate_id-107)/1000 = c1.icd9_concept_id;
} 

{@use_covariate_3_digit_icd_9_ambulatory_180d_med_f} ? {
SELECT DISTINCT row_id,
	CAST(freq.icd9_concept_id AS BIGINT) * 1000 + 108 AS covariate_id,
	1 AS covariate_value
INTO #cov_co_a_icd_180d_m
FROM #freq freq
INNER JOIN #thresholds thresholds
ON freq.icd9_concept_id = thresholds.icd9_concept_id
WHERE frequency >= median_value
  AND median_value > 1;

INSERT INTO #cov_ref (
  covariate_id,
  covariate_name,
  analysis_id,
  concept_id
	)
SELECT p1.covariate_id,
	'3-digit ICD-9 occurrence record of ambulatory diagnosis observed during 180d on or prior to cohort index with freq >= median: ' + icd9 + '-' + c1.icd9_concept_name AS covariate_name,
	108 AS analysis_id,
	0 AS concept_id
FROM (SELECT DISTINCT covariate_id FROM #cov_co_a_icd_180d_m) p1
INNER JOIN (SELECT DISTINCT icd9_concept_id, icd9, icd9_concept_name FROM #condition_id_to_icd9) c1
	ON (p1.covariate_id-108)/1000 = c1.icd9_concept_id;
}

{@use_covariate_3_digit_icd_9_ambulatory_180d_75_f} ? {
SELECT DISTINCT row_id,
	CAST(freq.icd9_concept_id AS BIGINT) * 1000 + 109 AS covariate_id,
	1 AS covariate_value
INTO #cov_co_a_icd_180d_75
FROM #freq freq
INNER JOIN #thresholds thresholds
ON freq.icd9_concept_id = thresholds.icd9_concept_id
WHERE frequency >= q75_value
  AND q75_value > median_value;

INSERT INTO #cov_ref (
  covariate_id,
  covariate_name,
  analysis_id,
  concept_id
	)
SELECT p1.covariate_id,
	'3-digit ICD-9 occurrence record of ambulatory diagnosis observed during 180d on or prior to cohort index  with freq >= q75: ' + icd9 + '-' + c1.icd9_concept_name AS covariate_name,
	109 AS analysis_id,
	0 AS concept_id
FROM (SELECT DISTINCT covariate_id FROM #cov_co_a_icd_180d_75) p1
INNER JOIN (SELECT DISTINCT icd9_concept_id, icd9, icd9_concept_name FROM #condition_id_to_icd9) c1
	ON (p1.covariate_id-109)/1000 = c1.icd9_concept_id;
}

{@use_covariate_3_digit_icd_9_ambulatory_180d | @use_covariate_3_digit_icd_9_ambulatory_180d_med_f | @use_covariate_3_digit_icd_9_ambulatory_180d_75_f } ? {
TRUNCATE TABLE #freq;
DROP TABLE #freq;
}

{@use_covariate_3_digit_icd_9_ambulatory_180d_med_f | @use_covariate_3_digit_icd_9_ambulatory_180d_75_f } ? {
TRUNCATE TABLE #thresholds;
DROP TABLE #thresholds;
}

{@use_covariate_3_digit_icd_9_inpatient_180d | @use_covariate_3_digit_icd_9_ambulatory_180d | @use_covariate_3_digit_icd_9_inpatient_180d_med_f | @use_covariate_3_digit_icd_9_ambulatory_180d_med_f | @use_covariate_3_digit_icd_9_inpatient_180d_75_f | @use_covariate_3_digit_icd_9_ambulatory_180d_75_f } ? {
TRUNCATE TABLE #condition_id_to_icd9;
DROP TABLE #condition_id_to_icd9;
}

	
}


/**************************
***************************
DRUG EXPOSURE
***************************
**************************/
{@use_covariate_drug_exposure} ? { 


{@use_covariate_ingredient_exposure_180d | @use_covariate_ingredient_exposure_180d_med_f  | @use_covariate_ingredient_exposure_180d_75_f} ? {
SELECT cp1.@row_id_field AS row_id,
	ingredient.concept_id,
	COUNT_BIG(*) AS frequency
INTO #freq
FROM @cohort_temp_table cp1
INNER JOIN drug_exposure de1
	ON cp1.subject_id = de1.person_id
INNER JOIN concept_ancestor ca1
	ON de1.drug_concept_id = ca1.descendant_concept_id
INNER JOIN concept ingredient
	ON ca1.ancestor_concept_id = ingredient.concept_id
WHERE de1.drug_concept_id != 0
{@has_excluded_covariate_concept_ids} ? {	AND de1.drug_concept_id NOT IN (SELECT concept_id FROM #excluded_cov)}
{@has_included_covariate_concept_ids} ? {	AND de1.drug_concept_id IN (SELECT concept_id FROM #included_cov)}
	AND de1.drug_exposure_start_date <= cp1.cohort_start_date
	AND de1.drug_exposure_start_date >= dateadd(dd, - 180, cp1.cohort_start_date)
{@cdm_version == '4'} ? {
	AND ingredient.vocabulary_id = 8
	AND ingredient.concept_class = 'Ingredient'
} : {
	AND ingredient.vocabulary_id = 'RxNorm'
	AND ingredient.concept_class_id = 'Ingredient'
}
GROUP BY cp1.@row_id_field,
	ingredient.concept_id;
}

{@use_covariate_ingredient_exposure_180d_med_f  | @use_covariate_ingredient_exposure_180d_75_f} ? {	
SELECT concept_id,
    MAX(CASE WHEN quantile <= 0.50 THEN frequency ELSE -9999 END) AS median_value,
	MAX(CASE WHEN quantile <= 0.75 THEN frequency ELSE -9999 END) AS q75_value
INTO #thresholds  
FROM (
	SELECT concept_id,
	  frequency,
	  1.0 * ROW_NUMBER() OVER (PARTITION BY concept_id ORDER BY frequency) / COUNT_BIG(*) OVER (PARTITION BY concept_id) AS quantile
	FROM #freq
   ) #temp	
GROUP BY concept_id;
}

{@use_covariate_ingredient_exposure_180d} ? {
--drug exist:  ingredient prescription in last 180d prior
SELECT DISTINCT row_id,
	CAST(concept_id AS BIGINT) * 1000 + 403 AS covariate_id,
	1 AS covariate_value
INTO #cov_ie_180d
FROM #freq;

INSERT INTO #cov_ref (
  covariate_id,
  covariate_name,
  analysis_id,
  concept_id
	)
SELECT p1.covariate_id,
	'Ingredient prescription record observed during 180d on or prior to cohort index:  ' + CAST((p1.covariate_id-403)/1000 AS VARCHAR) + '-' + CASE
		WHEN c1.concept_name IS NOT NULL
			THEN c1.concept_name
		ELSE 'Unknown invalid concept'
		END AS covariate_name,
	403 AS analysis_id,
	(p1.covariate_id-403)/1000 AS concept_id
FROM (SELECT DISTINCT covariate_id FROM #cov_ie_180d) p1
LEFT JOIN concept c1
	ON (p1.covariate_id-403)/1000 = c1.concept_id;
}

{@use_covariate_ingredient_exposure_180d_med_f} ? {
SELECT DISTINCT row_id,
	CAST(freq.concept_id AS BIGINT) * 1000 + 404 AS covariate_id,
	1 AS covariate_value
INTO #cov_ie_180d_m
FROM #freq freq
INNER JOIN #thresholds thresholds
ON freq.concept_id = thresholds.concept_id
WHERE frequency >= median_value
  AND median_value > 1;

INSERT INTO #cov_ref (
  covariate_id,
  covariate_name,
  analysis_id,
  concept_id
	)
SELECT p1.covariate_id,
	'Ingredient prescription record observed during 180d on or prior to cohort index with freq > median:  ' + CAST((p1.covariate_id-404)/1000 AS VARCHAR) + '-' + CASE
		WHEN c1.concept_name IS NOT NULL
			THEN c1.concept_name
		ELSE 'Unknown invalid concept'
		END AS covariate_name,
	404 AS analysis_id,
	(p1.covariate_id-404)/1000 AS concept_id
FROM (SELECT DISTINCT covariate_id FROM #cov_ie_180d_m) p1
LEFT JOIN concept c1
	ON (p1.covariate_id-404)/1000 = c1.concept_id;
}

{@use_covariate_ingredient_exposure_180d_75_f} ? {
SELECT DISTINCT row_id,
	CAST(freq.concept_id AS BIGINT) * 1000 + 405 AS covariate_id,
	1 AS covariate_value
INTO #cov_ie_180d_75
FROM #freq freq
INNER JOIN #thresholds thresholds
ON freq.concept_id = thresholds.concept_id
WHERE frequency >= q75_value
  AND q75_value > median_value;

INSERT INTO #cov_ref (
  covariate_id,
  covariate_name,
  analysis_id,
  concept_id
	)
SELECT p1.covariate_id,
	'Ingredient prescription record observed during 180d on or prior to cohort index with freq > q75:  ' + CAST((p1.covariate_id-405)/1000 AS VARCHAR) + '-' + CASE
		WHEN c1.concept_name IS NOT NULL
			THEN c1.concept_name
		ELSE 'Unknown invalid concept'
		END AS covariate_name,
	405 AS analysis_id,
	(p1.covariate_id-405)/1000 AS concept_id
FROM (SELECT DISTINCT covariate_id FROM #cov_ie_180d_75) p1
LEFT JOIN concept c1
	ON (p1.covariate_id-405)/1000 = c1.concept_id;
}

{@use_covariate_ingredient_exposure_180d | @use_covariate_ingredient_exposure_180d_med_f  | @use_covariate_ingredient_exposure_180d_75_f} ? {
TRUNCATE TABLE #freq;
DROP TABLE #freq;
}

{@use_covariate_ingredient_exposure_180d_med_f  | @use_covariate_ingredient_exposure_180d_75_f} ? {
TRUNCATE TABLE #thresholds;
DROP TABLE #thresholds;
}


}

/**************************
***************************
PROCEDURE OCCURRENCE
***************************
**************************/
{@use_covariate_procedure_occurrence} ? { 


{@use_covariate_inpatient_procedure_occurrence_180d | @use_covariate_inpatient_procedure_occurrence_180d_med_f | @use_covariate_inpatient_procedure_occurrence_180d_75_f} ? {
SELECT @row_id_field AS row_id,
	po1.procedure_concept_id,
	COUNT_BIG(*) AS frequency
INTO #freq
FROM @cohort_temp_table cp1
INNER JOIN procedure_occurrence po1
	ON cp1.subject_id = po1.person_id
INNER JOIN visit_occurrence vo1
  ON po1.visit_occurrence_id = vo1.visit_occurrence_id
WHERE po1.procedure_concept_id  != 0
{@has_excluded_covariate_concept_ids} ? {	AND po1.procedure_concept_id  NOT IN (SELECT concept_id FROM #excluded_cov)}
{@has_included_covariate_concept_ids} ? {	AND po1.procedure_concept_id  IN (SELECT concept_id FROM #included_cov)}
{@cdm_version == '4'} ? {
  AND vo1.place_of_service_concept_id = 9201
} : {
  AND vo1.visit_concept_id = 9201
}
	AND po1.procedure_date <= cp1.cohort_start_date
	AND po1.procedure_date >= dateadd(dd, - 180, cp1.cohort_start_date)
GROUP BY cp1.@row_id_field,
	po1.procedure_concept_id;
}

{@use_covariate_inpatient_procedure_occurrence_180d_med_f | @use_covariate_inpatient_procedure_occurrence_180d_75_f} ? {	
SELECT procedure_concept_id,
    MAX(CASE WHEN quantile <= 0.50 THEN frequency ELSE -9999 END) AS median_value,
	MAX(CASE WHEN quantile <= 0.75 THEN frequency ELSE -9999 END) AS q75_value
INTO #thresholds  
FROM (
	SELECT procedure_concept_id,
	  frequency,
	  1.0 * ROW_NUMBER() OVER (PARTITION BY procedure_concept_id ORDER BY frequency) / COUNT_BIG(*) OVER (PARTITION BY procedure_concept_id) AS quantile
	FROM #freq
   ) #temp	
GROUP BY procedure_concept_id;
}

{@use_covariate_inpatient_procedure_occurrence_180d} ? {
--procedures exist:  episode in last 180d prior
SELECT DISTINCT row_id,
	CAST(procedure_concept_id AS BIGINT) * 1000 + 703 AS covariate_id,
	1 AS covariate_value
INTO #cov_po_i_180d
FROM #freq;

INSERT INTO #cov_ref (
  covariate_id,
  covariate_name,
  analysis_id,
  concept_id
	)
SELECT p1.covariate_id,
	'Inpatient procedure occurrence record observed during 180d on or prior to cohort index:  ' + CAST((p1.covariate_id-703)/1000 AS VARCHAR) + '-' + CASE
		WHEN c1.concept_name IS NOT NULL
			THEN c1.concept_name
		ELSE 'Unknown invalid concept'
		END AS covariate_name,
	703 AS analysis_id,
	(p1.covariate_id-703)/1000 AS concept_id
FROM (SELECT DISTINCT covariate_id FROM #cov_po_i_180d) p1
LEFT JOIN concept c1
	ON (p1.covariate_id-703)/1000 = c1.concept_id;
} 

{@use_covariate_inpatient_procedure_occurrence_180d_med_f} ? {
SELECT DISTINCT row_id,
	CAST(freq.procedure_concept_id AS BIGINT) * 1000 + 704 AS covariate_id,
	1 AS covariate_value
INTO #cov_po_i_180d_m
FROM #freq freq
INNER JOIN #thresholds thresholds
ON freq.procedure_concept_id = thresholds.procedure_concept_id
WHERE frequency >= median_value
  AND median_value > 1;

INSERT INTO #cov_ref (
  covariate_id,
  covariate_name,
  analysis_id,
	concept_id
	)
SELECT p1.covariate_id,
	'Inpatient procedure occurrence record observed during 180d on or prior to cohort index with freq >= median:  ' + CAST((p1.covariate_id-704)/1000 AS VARCHAR) + '-' + CASE
		WHEN c1.concept_name IS NOT NULL
			THEN c1.concept_name
		ELSE 'Unknown invalid concept'
		END AS covariate_name,
	704 AS analysis_id,
	(p1.covariate_id-704)/1000 AS concept_id
FROM (SELECT DISTINCT covariate_id FROM #cov_po_i_180d_m) p1
LEFT JOIN concept c1
	ON (p1.covariate_id-704)/1000 = c1.concept_id;
}

{@use_covariate_inpatient_procedure_occurrence_180d_75_f} ? {
SELECT DISTINCT row_id,
	CAST(freq.procedure_concept_id AS BIGINT) * 1000 + 705 AS covariate_id,
	1 AS covariate_value
INTO #cov_po_i_180d_75
FROM #freq freq
INNER JOIN #thresholds thresholds
ON freq.procedure_concept_id = thresholds.procedure_concept_id
WHERE frequency >= q75_value
  AND q75_value > median_value;

INSERT INTO #cov_ref (
  covariate_id,
  covariate_name,
  analysis_id,
	concept_id
	)
SELECT p1.covariate_id,
	'Inpatient procedure occurrence record observed during 180d on or prior to cohort index with freq >= q75:  ' + CAST((p1.covariate_id-705)/1000 AS VARCHAR) + '-' + CASE
		WHEN c1.concept_name IS NOT NULL
			THEN c1.concept_name
		ELSE 'Unknown invalid concept'
		END AS covariate_name,
	705 AS analysis_id,
	(p1.covariate_id-705)/1000 AS concept_id
FROM (SELECT DISTINCT covariate_id FROM #cov_po_i_180d_75) p1
LEFT JOIN concept c1
	ON (p1.covariate_id-705)/1000 = c1.concept_id;
}

{@use_covariate_inpatient_procedure_occurrence_180d | @use_covariate_inpatient_procedure_occurrence_180d_med_f | @use_covariate_inpatient_procedure_occurrence_180d_75_f} ? {
TRUNCATE TABLE #freq;
DROP TABLE #freq;
}

{@use_covariate_inpatient_procedure_occurrence_180d_med_f | @use_covariate_inpatient_procedure_occurrence_180d_75_f} ? {
TRUNCATE TABLE #thresholds;
DROP TABLE #thresholds;
}

{@use_covariate_ambulatory_procedure_occurrence_180d | @use_covariate_ambulatory_procedure_occurrence_180d_med_f | @use_covariate_ambulatory_procedure_occurrence_180d_75_f} ? {
SELECT @row_id_field AS row_id,
	po1.procedure_concept_id,
	COUNT_BIG(*) AS frequency
INTO #freq
FROM @cohort_temp_table cp1
INNER JOIN procedure_occurrence po1
	ON cp1.subject_id = po1.person_id
INNER JOIN visit_occurrence vo1
  ON po1.visit_occurrence_id = vo1.visit_occurrence_id
WHERE po1.procedure_concept_id  != 0
{@has_excluded_covariate_concept_ids} ? {	AND po1.procedure_concept_id  NOT IN (SELECT concept_id FROM #excluded_cov)}
{@has_included_covariate_concept_ids} ? {	AND po1.procedure_concept_id  IN (SELECT concept_id FROM #included_cov)}
{@cdm_version == '4'} ? {
  AND vo1.place_of_service_concept_id != 9201
} : {
  AND vo1.visit_concept_id != 9201
}
	AND po1.procedure_date <= cp1.cohort_start_date
	AND po1.procedure_date >= dateadd(dd, - 180, cp1.cohort_start_date)
GROUP BY cp1.@row_id_field,
	po1.procedure_concept_id;
}

{@use_covariate_ambulatory_procedure_occurrence_180d_med_f | @use_covariate_ambulatory_procedure_occurrence_180d_75_f} ? {	
SELECT procedure_concept_id,
    MAX(CASE WHEN quantile <= 0.50 THEN frequency ELSE -9999 END) AS median_value,
	MAX(CASE WHEN quantile <= 0.75 THEN frequency ELSE -9999 END) AS q75_value
INTO #thresholds  
FROM (
	SELECT procedure_concept_id,
	  frequency,
	  1.0 * ROW_NUMBER() OVER (PARTITION BY procedure_concept_id ORDER BY frequency) / COUNT_BIG(*) OVER (PARTITION BY procedure_concept_id) AS quantile
	FROM #freq
   ) #temp	
GROUP BY procedure_concept_id;
}

{@use_covariate_ambulatory_procedure_occurrence_180d} ? {
--procedures exist:  episode in last 180d prior
SELECT DISTINCT row_id,
	CAST(procedure_concept_id AS BIGINT) * 1000 + 706 AS covariate_id,
	1 AS covariate_value
INTO #cov_po_a_180d
FROM #freq;

INSERT INTO #cov_ref (
  covariate_id,
  covariate_name,
  analysis_id,
  concept_id
	)
SELECT p1.covariate_id,
	'ambulatory procedure occurrence record observed during 180d on or prior to cohort index:  ' + CAST((p1.covariate_id-706)/1000 AS VARCHAR) + '-' + CASE
		WHEN c1.concept_name IS NOT NULL
			THEN c1.concept_name
		ELSE 'Unknown invalid concept'
		END AS covariate_name,
	706 AS analysis_id,
	(p1.covariate_id-706)/1000 AS concept_id
FROM (SELECT DISTINCT covariate_id FROM #cov_po_a_180d) p1
LEFT JOIN concept c1
	ON (p1.covariate_id-706)/1000 = c1.concept_id;
} 

{@use_covariate_ambulatory_procedure_occurrence_180d_med_f} ? {
SELECT DISTINCT row_id,
	CAST(freq.procedure_concept_id AS BIGINT) * 1000 + 707 AS covariate_id,
	1 AS covariate_value
INTO #cov_po_a_180d_m
FROM #freq freq
INNER JOIN #thresholds thresholds
ON freq.procedure_concept_id = thresholds.procedure_concept_id
WHERE frequency >= median_value
  AND median_value > 1;

INSERT INTO #cov_ref (
  covariate_id,
  covariate_name,
  analysis_id,
	concept_id
	)
SELECT p1.covariate_id,
	'ambulatory procedure occurrence record observed during 180d on or prior to cohort index with freq >= median:  ' + CAST((p1.covariate_id-707)/1000 AS VARCHAR) + '-' + CASE
		WHEN c1.concept_name IS NOT NULL
			THEN c1.concept_name
		ELSE 'Unknown invalid concept'
		END AS covariate_name,
	707 AS analysis_id,
	(p1.covariate_id-707)/1000 AS concept_id
FROM (SELECT DISTINCT covariate_id FROM #cov_po_a_180d_m) p1
LEFT JOIN concept c1
	ON (p1.covariate_id-707)/1000 = c1.concept_id;
}

{@use_covariate_ambulatory_procedure_occurrence_180d_75_f} ? {
SELECT DISTINCT row_id,
	CAST(freq.procedure_concept_id AS BIGINT) * 1000 + 708 AS covariate_id,
	1 AS covariate_value
INTO #cov_po_a_180d_75
FROM #freq freq
INNER JOIN #thresholds thresholds
ON freq.procedure_concept_id = thresholds.procedure_concept_id
WHERE frequency >= q75_value
  AND q75_value > median_value;

INSERT INTO #cov_ref (
  covariate_id,
  covariate_name,
  analysis_id,
	concept_id
	)
SELECT p1.covariate_id,
	'ambulatory procedure occurrence record observed during 180d on or prior to cohort index with freq >= q75:  ' + CAST((p1.covariate_id-708)/1000 AS VARCHAR) + '-' + CASE
		WHEN c1.concept_name IS NOT NULL
			THEN c1.concept_name
		ELSE 'Unknown invalid concept'
		END AS covariate_name,
	708 AS analysis_id,
	(p1.covariate_id-708)/1000 AS concept_id
FROM (SELECT DISTINCT covariate_id FROM #cov_po_a_180d_75) p1
LEFT JOIN concept c1
	ON (p1.covariate_id-708)/1000 = c1.concept_id;
}

{@use_covariate_ambulatory_procedure_occurrence_180d | @use_covariate_ambulatory_procedure_occurrence_180d_med_f | @use_covariate_ambulatory_procedure_occurrence_180d_75_f} ? {
TRUNCATE TABLE #freq;
DROP TABLE #freq;
}

{@use_covariate_ambulatory_procedure_occurrence_180d_med_f | @use_covariate_ambulatory_procedure_occurrence_180d_75_f} ? {
TRUNCATE TABLE #thresholds;
DROP TABLE #thresholds;
}


}

/**************************
***************************
COMBINE INTO ONE TABLE
***************************
**************************/

SELECT row_id, covariate_id, covariate_value
INTO #cov_all
FROM
(

SELECT row_id, covariate_id, covariate_value FROM #dummy

{@use_covariate_cohort_id_is_1} ? {
UNION

SELECT row_id, covariate_id, covariate_value
FROM #cov_exposure
}


{@use_covariate_demographics} ? {

{@use_covariate_demographics_gender} ? {
UNION

SELECT row_id,covariate_id, covariate_value
FROM #cov_gender

}

{@use_covariate_demographics_race} ? {
UNION

SELECT row_id,covariate_id, covariate_value
FROM #cov_race

}

{@use_covariate_demographics_ethnicity} ? {
UNION

SELECT row_id,covariate_id, covariate_value
FROM #cov_ethnicity

}


{@use_covariate_demographics_age} ? {
UNION

SELECT row_id,covariate_id, covariate_value
FROM #cov_age

}

{@use_covariate_demographics_year} ? {
UNION

SELECT row_id,covariate_id, covariate_value
FROM #cov_year

}

{@use_covariate_demographics_month} ? {
UNION

SELECT row_id,covariate_id, covariate_value
FROM #cov_month

}

}

{@use_covariate_condition_occurrence} ? {

{@use_covariate_3_digit_icd_9_inpatient_180d} ? {

UNION

SELECT row_id,covariate_id, covariate_value
FROM #cov_co_i_icd_180d

}

{@use_covariate_3_digit_icd_9_inpatient_180d_med_f} ? {

UNION

SELECT row_id,covariate_id, covariate_value
FROM #cov_co_i_icd_180d_m

}

{@use_covariate_3_digit_icd_9_inpatient_180d_75_f} ? {

UNION

SELECT row_id,covariate_id, covariate_value
FROM #cov_co_i_icd_180d_75

}

{@use_covariate_3_digit_icd_9_ambulatory_180d} ? {

UNION

SELECT row_id,covariate_id, covariate_value
FROM #cov_co_a_icd_180d

}

{@use_covariate_3_digit_icd_9_ambulatory_180d_med_f} ? {

UNION

SELECT row_id,covariate_id, covariate_value
FROM #cov_co_a_icd_180d_m

}

{@use_covariate_3_digit_icd_9_ambulatory_180d_75_f} ? {

UNION

SELECT row_id,covariate_id, covariate_value
FROM #cov_co_a_icd_180d_75

}

}


{@use_covariate_drug_exposure} ? {

{@use_covariate_ingredient_exposure_180d} ? {

UNION

SELECT row_id,covariate_id, covariate_value
FROM #cov_ie_180d

}

{@use_covariate_ingredient_exposure_180d_med_f} ? {

UNION

SELECT row_id,covariate_id, covariate_value
FROM #cov_ie_180d_m

}

{@use_covariate_ingredient_exposure_180d_75_f} ? {

UNION

SELECT row_id,covariate_id, covariate_value
FROM #cov_ie_180d_75

}

}


{@use_covariate_procedure_occurrence} ? {


{@use_covariate_inpatient_procedure_occurrence_180d} ? {

UNION

SELECT row_id,covariate_id, covariate_value
FROM #cov_po_i_180d

}

{@use_covariate_inpatient_procedure_occurrence_180d_med_f} ? {

UNION

SELECT row_id,covariate_id, covariate_value
FROM #cov_po_i_180d_m

}

{@use_covariate_inpatient_procedure_occurrence_180d_75_f} ? {

UNION

SELECT row_id,covariate_id, covariate_value
FROM #cov_po_i_180d_75

}

{@use_covariate_ambulatory_procedure_occurrence_180d} ? {

UNION

SELECT row_id,covariate_id, covariate_value
FROM #cov_po_a_180d

}

{@use_covariate_ambulatory_procedure_occurrence_180d_med_f} ? {

UNION

SELECT row_id,covariate_id, covariate_value
FROM #cov_po_a_180d_m

}

{@use_covariate_ambulatory_procedure_occurrence_180d_75_f} ? {

UNION

SELECT row_id,covariate_id, covariate_value
FROM #cov_po_a_180d_75

}

}


) all_covariates;


/**************************
***************************
DELETE SMALL COUNTS
***************************
**************************/

{@delete_covariates_small_count != 0 } ? {

DELETE
FROM #cov_ref
WHERE covariate_id IN (
  	SELECT covariate_id
		FROM #cov_all
		GROUP BY covariate_id
		HAVING COUNT(row_id) <= @delete_covariates_small_count

);
}

SELECT row_id, covariate_id, covariate_value
  INTO #cov
FROM (
	SELECT row_id, covariate_id, covariate_value
	FROM #cov_all
	WHERE covariate_id IN (
		SELECT covariate_id
			FROM #cov_ref
		)

) t1
;

IF OBJECT_ID('tempdb..#cov_exposure', 'U') IS NOT NULL
  DROP TABLE #cov_exposure;
IF OBJECT_ID('tempdb..#cov_gender', 'U') IS NOT NULL
  DROP TABLE #cov_gender;
IF OBJECT_ID('tempdb..#cov_race', 'U') IS NOT NULL
  DROP TABLE #cov_race;
IF OBJECT_ID('tempdb..#cov_ethnicity', 'U') IS NOT NULL
  DROP TABLE #cov_ethnicity;
IF OBJECT_ID('tempdb..#cov_age', 'U') IS NOT NULL
  DROP TABLE #cov_age;
IF OBJECT_ID('tempdb..#cov_year', 'U') IS NOT NULL
  DROP TABLE #cov_year;
IF OBJECT_ID('tempdb..#cov_month', 'U') IS NOT NULL
  DROP TABLE #cov_month;
TRUNCATE TABLE #cov_all;
  DROP TABLE #cov_all;
TRUNCATE TABLE #dummy;
  DROP TABLE #dummy;

{@has_excluded_covariate_concept_ids} ? {
TRUNCATE TABLE #excluded_cov;

DROP TABLE #excluded_cov;
}

{@has_included_covariate_concept_ids} ? {
TRUNCATE TABLE #included_cov;

DROP TABLE #included_cov;
}
