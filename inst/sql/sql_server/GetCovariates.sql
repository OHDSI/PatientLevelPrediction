/************************************************************************
@file GetCovariates.sql

Copyright 2015 Observational Health Data Sciences and Informatics

This file is part of PatientLevelPrediction

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

{DEFAULT @cdm_database = 'CDM4_SIM' } /*cdm_database: @cdm_database*/
{DEFAULT @use_existing_cohort_person = TRUE } /*use_existing_cohort_person: @ use_existing_cohort_person*/
{DEFAULT @cohort_database_schema = 'CDM4_SIM' } /*cohort_database_schema: @cohort_database_schema*/
{DEFAULT @cohort_table = 'cohort' } /*cohort_table: @cohort_table*/
{DEFAULT @cohort_concept_ids = '0,1' } /*cohort_concept_ids: @cohort_concept_ids*/
{DEFAULT @use_covariate_demographics = TRUE} /*use_covariate_demographics: @use_covariate_demographics*/
{DEFAULT @use_covariate_condition_occurrence = TRUE} /*use_covariate_condition_occurrence: @use_covariate_condition_occurrence*/
{DEFAULT @use_covariate_condition_occurrence_365d = TRUE} /*use_covariate_condition_occurrence_365d: @use_covariate_condition_occurrence_365d*/
{DEFAULT @use_covariate_condition_occurrence_30d = FALSE} /*use_covariate_condition_occurrence_30d: @use_covariate_condition_occurrence_30d*/
{DEFAULT @use_covariate_condition_occurrence_inpt180d = FALSE} /*use_covariate_condition_occurrence_inpt180d: @use_covariate_condition_occurrence_inpt180d*/
{DEFAULT @use_covariate_condition_era = FALSE} /*use_covariate_condition_era: @use_covariate_condition_era*/
{DEFAULT @use_covariate_condition_era_ever = FALSE} /*use_covariate_condition_era_ever: @use_covariate_condition_era_ever*/
{DEFAULT @use_covariate_condition_era_overlap = FALSE} /*use_covariate_condition_era_overlap: @use_covariate_condition_era_overlap*/
{DEFAULT @use_covariate_condition_group = FALSE} /*use_covariate_condition_group: @use_covariate_condition_group*/
{DEFAULT @use_covariate_drug_exposure = FALSE} /*use_covariate_drug_exposure: @use_covariate_drug_exposure*/
{DEFAULT @use_covariate_drug_exposure_365d = FALSE} /*use_covariate_drug_exposure_365d: @use_covariate_drug_exposure_365d*/
{DEFAULT @use_covariate_drug_exposure_30d = FALSE} /*use_covariate_drug_exposure_30d: @use_covariate_drug_exposure_30d*/
{DEFAULT @use_covariate_drug_era = FALSE} /*use_covariate_drug_era: @use_covariate_drug_era*/
{DEFAULT @use_covariate_drug_era_365d = FALSE} /*use_covariate_drug_era_365d: @use_covariate_drug_era_365d*/
{DEFAULT @use_covariate_drug_era_30d = FALSE} /*use_covariate_drug_era_30d: @use_covariate_drug_era_30d*/
{DEFAULT @use_covariate_drug_era_overlap = FALSE} /*use_covariate_drug_era_overlap: @use_covariate_drug_era_overlap*/
{DEFAULT @use_covariate_drug_era_ever = FALSE} /*use_covariate_drug_era_ever: @use_covariate_drug_era_ever*/
{DEFAULT @use_covariate_drug_group = FALSE} /*use_covariate_drug_group: @use_covariate_drug_group*/
{DEFAULT @use_covariate_procedure_occurrence = FALSE} /*use_covariate_procedure_occurrence: @use_covariate_procedure_occurrence*/
{DEFAULT @use_covariate_procedure_occurrence_365d = FALSE} /*use_covariate_procedure_occurrence_365d: @use_covariate_procedure_occurrence_365d*/
{DEFAULT @use_covariate_procedure_occurrence_30d = FALSE} /*use_covariate_procedure_occurrence_30d: @use_covariate_procedure_occurrence_30d*/
{DEFAULT @use_covariate_procedure_group = FALSE} /*use_covariate_procedure_group: @use_covariate_procedure_group*/
{DEFAULT @use_covariate_observation = FALSE} /*use_covariate_observation: @use_covariate_observation*/
{DEFAULT @use_covariate_observation_365d = FALSE} /*use_covariate_observation_365d: @use_covariate_observation_365d*/
{DEFAULT @use_covariate_observation_30d = FALSE} /*use_covariate_observation_30d: @use_covariate_observation_30d*/
{DEFAULT @use_covariate_observation_below = FALSE} /*use_covariate_observation_below: @use_covariate_observation_below*/
{DEFAULT @use_covariate_observation_above = FALSE} /*use_covariate_observation_above: @use_covariate_observation_after*/
{DEFAULT @use_covariate_observation_count365d = FALSE} /*use_covariate_observation_count365d: @use_covariate_observation_count365d*/
{DEFAULT @use_covariate_concept_counts = FALSE} /*use_covariate_concept_counts: @use_covariate_concept_counts*/
{DEFAULT @use_covariate_risk_scores = FALSE} /*use_covariate_risk_scores: @use_covariate_risk_scores*/
{DEFAULT @use_covariate_interaction_year = FALSE} /*use_covariate_interaction_year: @use_covariate_interaction_year*/
{DEFAULT @use_covariate_interaction_month = FALSE} /*use_covariate_interaction_month: @use_covariate_interaction_month*/
{DEFAULT @excluded_covariate_concept_ids = '' } /*excluded_covariate_concept_ids: @excluded_covariate_concept_ids*/
{DEFAULT @delete_covariates_small_count = 100 } /*delete_covariates_small_count: @delete_covariates_small_count*/

USE @cdm_database;

{!@use_existing_cohort_person} ? {
IF OBJECT_ID('tempdb..#cohort_person', 'U') IS NOT NULL
	DROP TABLE #cohort_person;
	
SELECT cohort_concept_id,
	subject_id,
	cohort_start_date,
	cohort_end_date
INTO #cohort_person
FROM @cohort_database_schema.@cohort_table
{@cohort_concept_ids != ''} ? {
WHERE cohort_concept_id IN (@cohort_concept_ids);
}
}

IF OBJECT_ID('tempdb..#cohort_covariate', 'U') IS NOT NULL
	DROP TABLE #cohort_covariate;

CREATE TABLE #cohort_covariate (
	cohort_start_date DATE,
	cohort_concept_id INT,
	person_id BIGINT,
	covariate_id BIGINT,
	covariate_value FLOAT
	);

IF OBJECT_ID('tempdb..#cohort_covariate_ref', 'U') IS NOT NULL
	DROP TABLE #cohort_covariate_ref;

CREATE TABLE #cohort_covariate_ref (
	covariate_id BIGINT,
	covariate_name VARCHAR(max),
	analysis_id INT,
	concept_id INT
	);

--covariate for exposure status, determining which patients are in which treatment group (only those in cohort 1 will get recorded)
INSERT INTO #cohort_covariate_ref (
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

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT cohort_start_date,
	cohort_concept_id,
	subject_id AS person_id,
	1 AS covariate_id,
	cohort_concept_id AS covariate_value
FROM #cohort_person
WHERE cohort_concept_id = 1;

/**************************
***************************
DEMOGRAPHICS
***************************
**************************/
{@use_covariate_demographics} ? {

--gender
INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
SELECT DISTINCT p1.gender_concept_id AS covariate_id,
	'Gender = ' + v1.concept_name AS covariate_name,
	2 AS analysis_id,
	p1.gender_concept_id AS concept_id
FROM #cohort_person cp1
INNER JOIN person p1
	ON cp1.subject_id = p1.person_id
INNER JOIN (
	SELECT concept_id,
		concept_name
	FROM concept
	WHERE LOWER(concept_class) = 'gender'
	) v1
	ON p1.gender_concept_id = v1.concept_id;

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id,
	gender_concept_id AS covariate_id,
	1 AS covariate_value
FROM #cohort_person cp1
INNER JOIN person p1
	ON cp1.subject_id = p1.person_id
WHERE p1.gender_concept_id IN (
		SELECT concept_id
		FROM concept
		WHERE LOWER(concept_class) = 'gender'
		);

--race
INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
SELECT DISTINCT p1.race_concept_id AS covariate_id,
	'Race = ' + v1.concept_name AS covariate_name,
	3 AS analysis_id,
	p1.race_concept_id AS concept_id
FROM #cohort_person cp1
INNER JOIN person p1
	ON cp1.subject_id = p1.person_id
INNER JOIN (
	SELECT concept_id,
		concept_name
	FROM concept
	WHERE LOWER(concept_class) = 'race'
	) v1
	ON p1.race_concept_id = v1.concept_id;

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id,
	race_concept_id AS covariate_id,
	1 AS covariate_value
FROM #cohort_person cp1
INNER JOIN person p1
	ON cp1.subject_id = p1.person_id
WHERE p1.race_concept_id IN (
		SELECT concept_id
		FROM concept
		WHERE LOWER(concept_class) = 'race'
		);

--ethnicity
INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
SELECT DISTINCT p1.ethnicity_concept_id AS covariate_id,
	'Ethnicity = ' + v1.concept_name AS covariate_name,
	4 AS analysis_id,
	p1.ethnicity_concept_id AS concept_id
FROM #cohort_person cp1
INNER JOIN person p1
	ON cp1.subject_id = p1.person_id
INNER JOIN (
	SELECT concept_id,
		concept_name
	FROM concept
	WHERE LOWER(concept_class) = 'ethnicity'
	) v1
	ON p1.ethnicity_concept_id = v1.concept_id;

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id AS person_id,
	race_concept_id AS covariate_id,
	1 AS covariate_value
FROM #cohort_person cp1
INNER JOIN person p1
	ON cp1.subject_id = p1.person_id
WHERE p1.ethnicity_concept_id IN (
		SELECT concept_id
		FROM concept
		WHERE LOWER(concept_class) = 'ethnicity'
		);

--age group
INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
SELECT DISTINCT floor((year(cp1.cohort_start_date) - p1.YEAR_OF_BIRTH) / 5) + 10 AS covariate_id,
	'Age group: ' + CAST(floor((year(cp1.cohort_start_date) - p1.YEAR_OF_BIRTH) / 5) * 5 AS VARCHAR) + '-' + CAST((floor((year(cp1.cohort_start_date) - p1.YEAR_OF_BIRTH) / 5) + 1) * 5 - 1 AS VARCHAR) AS covariate_name,
	4 AS analysis_id,
	0 AS concept_id
FROM #cohort_person cp1
INNER JOIN person p1
	ON cp1.subject_id = p1.person_id
WHERE (year(cp1.cohort_start_date) - p1.YEAR_OF_BIRTH) >= 0
	AND (year(cp1.cohort_start_date) - p1.YEAR_OF_BIRTH) < 100;

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id AS person_id,
	FLOOR((YEAR(cp1.cohort_start_date) - p1.YEAR_OF_BIRTH) / 5) + 10 AS covariate_id,
	1 AS covariate_value
FROM #cohort_person cp1
INNER JOIN person p1
	ON cp1.subject_id = p1.person_id
WHERE (YEAR(cp1.cohort_start_date) - p1.YEAR_OF_BIRTH) >= 0
	AND (YEAR(cp1.cohort_start_date) - p1.YEAR_OF_BIRTH) < 100;

--index year
INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
SELECT DISTINCT YEAR(cohort_start_date) AS covariate_id,
	'Index year: ' + CAST(YEAR(cohort_start_date) AS VARCHAR) AS covariate_name,
	5 AS analysis_id,
	0 AS concept_id
FROM #cohort_person cp1;

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id AS person_id,
	YEAR(cohort_start_date) AS covariate_id,
	1 AS covariate_value
FROM #cohort_person cp1;

--index month
INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
SELECT DISTINCT MONTH(cohort_start_date) + 40 AS covariate_id,
	'Index month: ' + CAST(MONTH(cohort_start_date) AS VARCHAR) AS covariate_name,
	6 AS analysis_id,
	0 AS concept_id
FROM #cohort_person cp1;

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id,
	MONTH(cohort_start_date) + 40 AS covariate_id,
	1 AS covariate_value
FROM #cohort_person cp1;

}
/**************************
***************************
CONDITION OCCURRENCE
***************************
**************************/
	{@use_covariate_condition_occurrence} ? { {@use_covariate_condition_occurrence_365d} ? {

--conditions exist:  episode in last 365d prior
INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
SELECT DISTINCT CAST(co1.condition_concept_id AS BIGINT) * 1000 + 101 AS covariate_id,
	'Condition occurrence record observed during 365d on or prior to cohort index:  ' + CAST(co1.condition_concept_id AS VARCHAR) + '-' + CASE 
		WHEN c1.concept_name IS NOT NULL
			THEN c1.concept_name
		ELSE 'Unknown invalid concept'
		END AS covariate_name,
	101 AS analysis_id,
	co1.condition_concept_id AS concept_id
FROM #cohort_person cp1
INNER JOIN condition_occurrence co1
	ON cp1.subject_id = co1.person_id
LEFT JOIN concept c1
	ON co1.condition_concept_id = c1.concept_id
WHERE co1.condition_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
	AND co1.condition_start_date <= cp1.cohort_start_date
	AND co1.condition_start_date >= dateadd(dd, - 365, cp1.cohort_start_date);

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT DISTINCT cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id AS person_id,
	CAST(co1.condition_concept_id AS BIGINT) * 1000 + 101 AS covariate_id,
	1 AS covariate_value
FROM #cohort_person cp1
INNER JOIN condition_occurrence co1
	ON cp1.subject_id = co1.person_id
WHERE co1.condition_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
	AND co1.condition_start_date <= cp1.cohort_start_date
	AND co1.condition_start_date >= dateadd(dd, - 365, cp1.cohort_start_date);

} {@use_covariate_condition_occurrence_30d} ? {

--conditions:  episode in last 30d prior
INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
SELECT DISTINCT CAST(co1.condition_concept_id AS BIGINT) * 1000 + 102 AS covariate_id,
	'Condition occurrence record observed during 30d on or prior to cohort index:  ' + CAST(co1.condition_concept_id AS VARCHAR) + '-' + CASE 
		WHEN c1.concept_name IS NOT NULL
			THEN c1.concept_name
		ELSE 'Unknown invalid concept'
		END AS covariate_name,
	102 AS analysis_id,
	co1.condition_concept_id AS concept_id
FROM #cohort_person cp1
INNER JOIN condition_occurrence co1
	ON cp1.subject_id = co1.person_id
LEFT JOIN concept c1
	ON co1.condition_concept_id = c1.concept_id
WHERE co1.condition_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
	AND co1.condition_start_date <= cp1.cohort_start_date
	AND co1.condition_start_date >= dateadd(dd, - 30, cp1.cohort_start_date);

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT DISTINCT cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id AS person_id,
	CAST(co1.condition_concept_id AS BIGINT) * 1000 + 102 AS covariate_id,
	1 AS covariate_value
FROM #cohort_person cp1
INNER JOIN condition_occurrence co1
	ON cp1.subject_id = co1.person_id
WHERE co1.condition_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
	AND co1.condition_start_date <= cp1.cohort_start_date
	AND co1.condition_start_date >= dateadd(dd, - 30, cp1.cohort_start_date);

} {@use_covariate_condition_occurrence_inpt180d} ? {

--conditions:  primary inpatient diagnosis in last 180d
INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
SELECT DISTINCT CAST(co1.condition_concept_id AS BIGINT) * 1000 + 103 AS covariate_id,
	'Condition occurrence record of primary inpatient diagnosis observed during 180d on or prior to cohort index:  ' + CAST(co1.condition_concept_id AS VARCHAR) + '-' + CASE 
		WHEN c1.concept_name IS NOT NULL
			THEN c1.concept_name
		ELSE 'Unknown invalid concept'
		END AS covariate_name,
	103 AS analysis_id,
	co1.condition_concept_id AS concept_id
FROM #cohort_person cp1
INNER JOIN condition_occurrence co1
	ON cp1.subject_id = co1.person_id
LEFT JOIN concept c1
	ON co1.condition_concept_id = c1.concept_id
WHERE co1.condition_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
	AND co1.condition_type_concept_id IN (38000183, 38000184, 38000199, 38000200)
	AND co1.condition_start_date <= cp1.cohort_start_date
	AND co1.condition_start_date >= dateadd(dd, - 180, cp1.cohort_start_date);

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT DISTINCT cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id,
	CAST(co1.condition_concept_id AS BIGINT) * 1000 + 103 AS covariate_id,
	1 AS covariate_value
FROM #cohort_person cp1
INNER JOIN condition_occurrence co1
	ON cp1.subject_id = co1.person_id
WHERE co1.condition_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
	AND co1.condition_type_concept_id IN (38000183, 38000184, 38000199, 38000200)
	AND co1.condition_start_date <= cp1.cohort_start_date
	AND co1.condition_start_date >= dateadd(dd, - 180, cp1.cohort_start_date);

} }
/**************************
***************************
CONDITION ERA
***************************
**************************/
	{@use_covariate_condition_era} ? { {@use_covariate_condition_era_ever} ? {

--condition:  exist any time prior
INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
SELECT DISTINCT CAST(ce1.condition_concept_id AS BIGINT) * 1000 + 201 AS covariate_id,
	'Condition era record observed during anytime on or prior to cohort index:  ' + CAST(ce1.condition_concept_id AS VARCHAR) + '-' + CASE 
		WHEN c1.concept_name IS NOT NULL
			THEN c1.concept_name
		ELSE 'Unknown invalid concept'
		END AS covariate_name,
	201 AS analysis_id,
	ce1.condition_concept_id AS concept_id
FROM #cohort_person cp1
INNER JOIN condition_era ce1
	ON cp1.subject_id = ce1.person_id
LEFT JOIN concept c1
	ON ce1.condition_concept_id = c1.concept_id
WHERE ce1.condition_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
	AND ce1.condition_era_start_date <= cp1.cohort_start_date;

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT DISTINCT cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id,
	CAST(ce1.condition_concept_id AS BIGINT) * 1000 + 201 AS covariate_id,
	1 AS covariate_value
FROM #cohort_person cp1
INNER JOIN condition_era ce1
	ON cp1.subject_id = ce1.person_id
LEFT JOIN concept c1
	ON ce1.condition_concept_id = c1.concept_id
WHERE ce1.condition_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
	AND ce1.condition_era_start_date <= cp1.cohort_start_date;

} {@use_covariate_condition_era_overlap} ? {

--concurrent on index date (era overlapping)
INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
SELECT DISTINCT CAST(ce1.condition_concept_id AS BIGINT) * 1000 + 202 AS covariate_id,
	'Condition era record observed concurrent (overlapping) with cohort index:  ' + CAST(ce1.condition_concept_id AS VARCHAR) + '-' + CASE 
		WHEN c1.concept_name IS NOT NULL
			THEN c1.concept_name
		ELSE 'Unknown invalid concept'
		END AS covariate_name,
	202 AS analysis_id,
	ce1.condition_concept_id AS concept_id
FROM #cohort_person cp1
INNER JOIN condition_era ce1
	ON cp1.subject_id = ce1.person_id
LEFT JOIN concept c1
	ON ce1.condition_concept_id = c1.concept_id
WHERE ce1.condition_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
	AND ce1.condition_era_start_date <= cp1.cohort_start_date
	AND ce1.condition_era_end_date >= cp1.cohort_start_date;

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT DISTINCT cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id,
	CAST(ce1.condition_concept_id AS BIGINT) * 1000 + 202 AS covariate_id,
	1 AS covariate_value
FROM #cohort_person cp1
INNER JOIN condition_era ce1
	ON cp1.subject_id = ce1.person_id
LEFT JOIN concept c1
	ON ce1.condition_concept_id = c1.concept_id
WHERE ce1.condition_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
	AND ce1.condition_era_start_date <= cp1.cohort_start_date
	AND ce1.condition_era_end_date >= cp1.cohort_start_date;

} }
/**************************
***************************
CONDITION GROUP
***************************
**************************/
	{@use_covariate_condition_group} ? {


IF OBJECT_ID('tempdb..#condition_group', 'U') IS NOT NULL
	DROP TABLE #condition_group;

CREATE TABLE #condition_group (
	descendant_concept_id INT,
	ancestor_concept_id INT
	);

--currently using MedDRA hierarchy
--future extension:  expand to SNOMED classes as well
INSERT INTO #condition_group (
	descendant_concept_id,
	ancestor_concept_id
	)
SELECT DISTINCT ca1.descendant_concept_id,
	ca1.ancestor_concept_id
FROM (
	SELECT covariate_id,
		covariate_name,
		analysis_id,
		concept_id
	FROM #cohort_covariate_ref
	WHERE analysis_id > 100
		AND analysis_id < 300
	) ccr1
INNER JOIN concept_ancestor ca1
	ON ccr1.concept_id = ca1.descendant_concept_id
INNER JOIN concept c1
	ON ca1.ancestor_concept_id = c1.concept_id
WHERE c1.vocabulary_id = 15
	AND c1.concept_class <> 'System Organ Class'
	AND c1.concept_id NOT IN (36302170, 36303153, 36313966 /*Investigation concepts, too broad*/ {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids });

INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
SELECT DISTINCT CAST(cg1.ancestor_concept_id AS BIGINT) * 1000 + ccr1.analysis_id AS covariate_id,
	CASE 
		WHEN analysis_id = 101
			THEN 'Condition occurrence record observed during 365d on or prior to cohort index within condition group:  '
		WHEN analysis_id = 102
			THEN 'Condition occurrence record observed during 30d on or prior to cohort index within condition group:  '
		WHEN analysis_id = 103
			THEN 'Condition occurrence record of primary inpatient diagnosis observed during 180d on or prior to cohort index within condition group:  '
		WHEN analysis_id = 201
			THEN 'Condition era record observed during anytime on or prior to cohort index within condition group:  '
		WHEN analysis_id = 202
			THEN 'Condition era record observed concurrent (overlapping) with cohort index within condition group:  '
		ELSE 'Other condition group analysis'
		END + CAST(cg1.ancestor_concept_id AS VARCHAR) + '-' + c1.concept_name AS covariate_name,
	ccr1.analysis_id,
	cg1.ancestor_concept_id AS concept_id
FROM #cohort_covariate cc1
INNER JOIN (
	SELECT covariate_id,
		covariate_name,
		analysis_id,
		concept_id
	FROM #cohort_covariate_ref
	WHERE analysis_id > 100
		AND analysis_id < 300
	) ccr1
	ON cc1.covariate_id = ccr1.covariate_id
INNER JOIN #condition_group cg1
	ON ccr1.concept_id = cg1.descendant_concept_id
INNER JOIN concept c1
	ON cg1.ancestor_concept_id = c1.concept_id;

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT DISTINCT cc1.cohort_start_date,
	cc1.cohort_concept_id,
	cc1.person_id,
	CAST(cg1.ancestor_concept_id AS BIGINT) * 1000 + ccr1.analysis_id AS covariate_id,
	1 AS covariate_value
FROM #cohort_covariate cc1
INNER JOIN (
	SELECT covariate_id,
		covariate_name,
		analysis_id,
		concept_id
	FROM #cohort_covariate_ref
	WHERE analysis_id > 100
		AND analysis_id < 300
	) ccr1
	ON cc1.covariate_id = ccr1.covariate_id
INNER JOIN #condition_group cg1
	ON ccr1.concept_id = cg1.descendant_concept_id;

TRUNCATE TABLE #condition_group;

DROP TABLE #condition_group;

}
/**************************
***************************
DRUG EXPOSURE
***************************
**************************/
	{@use_covariate_drug_exposure} ? { {@use_covariate_drug_exposure_365d} ? {

--drug exist:  episode in last 365d prior
INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
SELECT DISTINCT CAST(de1.drug_concept_id AS BIGINT) * 1000 + 401 AS covariate_id,
	'Drug exposure record observed during 365d on or prior to cohort index:  ' + CAST(de1.drug_concept_id AS VARCHAR) + '-' + CASE 
		WHEN c1.concept_name IS NOT NULL
			THEN c1.concept_name
		ELSE 'Unknown invalid concept'
		END AS covariate_name,
	401 AS analysis_id,
	de1.drug_concept_id AS concept_id
FROM #cohort_person cp1
INNER JOIN drug_exposure de1
	ON cp1.subject_id = de1.person_id
LEFT JOIN concept c1
	ON de1.drug_concept_id = c1.concept_id
WHERE de1.drug_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
	AND de1.drug_exposure_start_date <= cp1.cohort_start_date
	AND de1.drug_exposure_start_date >= dateadd(dd, - 365, cp1.cohort_start_date);

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT DISTINCT cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id,
	CAST(de1.drug_concept_id AS BIGINT) * 1000 + 401 AS covariate_id,
	1 AS covariate_value
FROM #cohort_person cp1
INNER JOIN drug_exposure de1
	ON cp1.subject_id = de1.person_id
WHERE de1.drug_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
	AND de1.drug_exposure_start_date <= cp1.cohort_start_date
	AND de1.drug_exposure_start_date >= dateadd(dd, - 365, cp1.cohort_start_date);

} {@use_covariate_drug_exposure_30d} ? {

--drug exist:  episode in last 30d prior
INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
SELECT DISTINCT CAST(de1.drug_concept_id AS BIGINT) * 1000 + 402 AS covariate_id,
	'Drug exposure record observed during 30d on or prior to cohort index:  ' + CAST(de1.drug_concept_id AS VARCHAR) + '-' + CASE 
		WHEN c1.concept_name IS NOT NULL
			THEN c1.concept_name
		ELSE 'Unknown invalid concept'
		END AS covariate_name,
	402 AS analysis_id,
	de1.drug_concept_id AS concept_id
FROM #cohort_person cp1
INNER JOIN drug_exposure de1
	ON cp1.subject_id = de1.person_id
LEFT JOIN concept c1
	ON de1.drug_concept_id = c1.concept_id
WHERE de1.drug_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
	AND de1.drug_exposure_start_date <= cp1.cohort_start_date
	AND de1.drug_exposure_start_date >= dateadd(dd, - 30, cp1.cohort_start_date);

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT DISTINCT cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id,
	CAST(de1.drug_concept_id AS BIGINT) * 1000 + 402 AS covariate_id,
	1 AS covariate_value
FROM #cohort_person cp1
INNER JOIN drug_exposure de1
	ON cp1.subject_id = de1.person_id
WHERE de1.drug_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
	AND de1.drug_exposure_start_date <= cp1.cohort_start_date
	AND de1.drug_exposure_start_date >= dateadd(dd, - 30, cp1.cohort_start_date);

} }
/**************************
***************************
DRUG ERA
***************************
**************************/
	{@use_covariate_drug_era} ? { {@use_covariate_drug_era_365d} ? {

--drug exist:  episode in last 365d prior
INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
SELECT DISTINCT CAST(de1.drug_concept_id AS BIGINT) * 1000 + 501 AS covariate_id,
	'Drug era record observed during 365d on or prior to cohort index:  ' + CAST(de1.drug_concept_id AS VARCHAR) + '-' + CASE 
		WHEN c1.concept_name IS NOT NULL
			THEN c1.concept_name
		ELSE 'Unknown invalid concept'
		END AS covariate_name,
	501 AS analysis_id,
	de1.drug_concept_id AS concept_id
FROM #cohort_person cp1
INNER JOIN drug_era de1
	ON cp1.subject_id = de1.person_id
LEFT JOIN concept c1
	ON de1.drug_concept_id = c1.concept_id
WHERE de1.drug_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
	AND de1.drug_era_start_date <= cp1.cohort_start_date
	AND de1.drug_era_end_date >= dateadd(dd, - 365, cp1.cohort_start_date);

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT DISTINCT cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id,
	CAST(de1.drug_concept_id AS BIGINT) * 1000 + 501 AS covariate_id,
	1 AS covariate_value
FROM #cohort_person cp1
INNER JOIN drug_era de1
	ON cp1.subject_id = de1.person_id
WHERE de1.drug_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
	AND de1.drug_era_start_date <= cp1.cohort_start_date
	AND de1.drug_era_end_date >= dateadd(dd, - 365, cp1.cohort_start_date);

} {@use_covariate_drug_era_30d} ? {

--drug exist:  episode in last 30d prior
INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
SELECT DISTINCT CAST(de1.drug_concept_id AS BIGINT) * 1000 + 502 AS covariate_id,
	'Drug era record observed during 30d on or prior to cohort index:  ' + CAST(de1.drug_concept_id AS VARCHAR) + '-' + CASE 
		WHEN c1.concept_name IS NOT NULL
			THEN c1.concept_name
		ELSE 'Unknown invalid concept'
		END AS covariate_name,
	502 AS analysis_id,
	de1.drug_concept_id AS concept_id
FROM #cohort_person cp1
INNER JOIN drug_era de1
	ON cp1.subject_id = de1.person_id
LEFT JOIN concept c1
	ON de1.drug_concept_id = c1.concept_id
WHERE de1.drug_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
	AND de1.drug_era_start_date <= cp1.cohort_start_date
	AND de1.drug_era_end_date >= dateadd(dd, - 30, cp1.cohort_start_date);

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT DISTINCT cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id,
	CAST(de1.drug_concept_id AS BIGINT) * 1000 + 502 AS covariate_id,
	1 AS covariate_value
FROM #cohort_person cp1
INNER JOIN drug_era de1
	ON cp1.subject_id = de1.person_id
WHERE de1.drug_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
	AND de1.drug_era_start_date <= cp1.cohort_start_date
	AND de1.drug_era_end_date >= dateadd(dd, - 30, cp1.cohort_start_date);

} {@use_covariate_drug_era_overlap} ? {

--concurrent on index date (era overlapping)
INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
SELECT DISTINCT CAST(de1.drug_concept_id AS BIGINT) * 1000 + 503 AS covariate_id,
	'Drug era record observed concurrent (overlapping) with cohort index:  ' + CAST(de1.drug_concept_id AS VARCHAR) + '-' + CASE 
		WHEN c1.concept_name IS NOT NULL
			THEN c1.concept_name
		ELSE 'Unknown invalid concept'
		END AS covariate_name,
	503 AS analysis_id,
	de1.drug_concept_id AS concept_id
FROM #cohort_person cp1
INNER JOIN drug_era de1
	ON cp1.subject_id = de1.person_id
LEFT JOIN concept c1
	ON de1.drug_concept_id = c1.concept_id
WHERE de1.drug_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
	AND de1.drug_era_start_date <= cp1.cohort_start_date
	AND de1.drug_era_end_date >= cp1.cohort_start_date;

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT DISTINCT cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id,
	CAST(de1.drug_concept_id AS BIGINT) * 1000 + 503 AS covariate_id,
	1 AS covariate_value
FROM #cohort_person cp1
INNER JOIN drug_era de1
	ON cp1.subject_id = de1.person_id
WHERE de1.drug_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
	AND de1.drug_era_start_date <= cp1.cohort_start_date
	AND de1.drug_era_end_date >= cp1.cohort_start_date;

} {@use_covariate_drug_era_ever} ? {

--drug exist:  episode in all time prior
INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
SELECT DISTINCT CAST(de1.drug_concept_id AS BIGINT) * 1000 + 504 AS covariate_id,
	'Drug era record observed during anytime on or prior to cohort index:  ' + CAST(de1.drug_concept_id AS VARCHAR) + '-' + CASE 
		WHEN c1.concept_name IS NOT NULL
			THEN c1.concept_name
		ELSE 'Unknown invalid concept'
		END AS covariate_name,
	504 AS analysis_id,
	de1.drug_concept_id AS concept_id
FROM #cohort_person cp1
INNER JOIN drug_era de1
	ON cp1.subject_id = de1.person_id
LEFT JOIN concept c1
	ON de1.drug_concept_id = c1.concept_id
WHERE de1.drug_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
	AND de1.drug_era_start_date <= cp1.cohort_start_date;

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT DISTINCT cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id,
	CAST(de1.drug_concept_id AS BIGINT) * 1000 + 504 AS covariate_id,
	1 AS covariate_value
FROM #cohort_person cp1
INNER JOIN drug_era de1
	ON cp1.subject_id = de1.person_id
WHERE de1.drug_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
	AND de1.drug_era_start_date <= cp1.cohort_start_date;

} }
/**************************
***************************
DRUG GROUP
***************************
**************************/
	{@use_covariate_drug_group} ? {


IF OBJECT_ID('tempdb..#drug_group', 'U') IS NOT NULL
	DROP TABLE #drug_group;

CREATE TABLE #drug_group (
	descendant_concept_id INT,
	ancestor_concept_id INT
	);

--ATC
INSERT INTO #drug_group (
	descendant_concept_id,
	ancestor_concept_id
	)
SELECT DISTINCT ca1.descendant_concept_id,
	ca1.ancestor_concept_id
FROM (
	SELECT covariate_id,
		covariate_name,
		analysis_id,
		concept_id
	FROM #cohort_covariate_ref
	WHERE analysis_id > 500
		AND analysis_id < 600
	) ccr1
INNER JOIN concept_ancestor ca1
	ON ccr1.concept_id = ca1.descendant_concept_id
INNER JOIN concept c1
	ON ca1.ancestor_concept_id = c1.concept_id
WHERE c1.vocabulary_id = 21
	AND len(c1.concept_code) IN (1, 3, 5)
	AND c1.concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids });

INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
SELECT DISTINCT CAST(cg1.ancestor_concept_id AS BIGINT) * 1000 + ccr1.analysis_id AS covariate_id,
	CASE 
		WHEN analysis_id = 501
			THEN 'Drug era record observed during 365d on or prior to cohort index within drug group:  '
		WHEN analysis_id = 502
			THEN 'Drug era record observed during 30d on or prior to cohort index within drug group:  '
		WHEN analysis_id = 503
			THEN 'Drug era record observed concurrent (overlapping) with cohort index within drug group:  '
		ELSE 'Other drug group analysis'
		END + CAST(cg1.ancestor_concept_id AS VARCHAR) + '-' + c1.concept_name AS covariate_name,
	ccr1.analysis_id,
	cg1.ancestor_concept_id AS concept_id
FROM #cohort_covariate cc1
INNER JOIN (
	SELECT covariate_id,
		covariate_name,
		analysis_id,
		concept_id
	FROM #cohort_covariate_ref
	WHERE analysis_id > 500
		AND analysis_id < 600
	) ccr1
	ON cc1.covariate_id = ccr1.covariate_id
INNER JOIN #drug_group cg1
	ON ccr1.concept_id = cg1.descendant_concept_id
INNER JOIN concept c1
	ON cg1.ancestor_concept_id = c1.concept_id;

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT DISTINCT cc1.cohort_start_date,
	cc1.cohort_concept_id,
	cc1.person_id,
	CAST(cg1.ancestor_concept_id AS BIGINT) * 1000 + ccr1.analysis_id AS covariate_id,
	1 AS covariate_value
FROM #cohort_covariate cc1
INNER JOIN (
	SELECT covariate_id,
		covariate_name,
		analysis_id,
		concept_id
	FROM #cohort_covariate_ref
	WHERE analysis_id > 500
		AND analysis_id < 600
	) ccr1
	ON cc1.covariate_id = ccr1.covariate_id
INNER JOIN #drug_group cg1
	ON ccr1.concept_id = cg1.descendant_concept_id;

--number of drugs within each ATC3 groupings all time prior
INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
SELECT DISTINCT CAST(cg1.ancestor_concept_id AS BIGINT) * 1000 + 601 AS covariate_id,
	'Number of ingredients within the drug group observed all time on or prior to cohort index: ' + CAST(cg1.ancestor_concept_id AS VARCHAR) + '-' + c1.concept_name AS covariate_name,
	601 AS analysis_id,
	cg1.ancestor_concept_id AS concept_id
FROM #cohort_covariate cc1
INNER JOIN (
	SELECT covariate_id,
		covariate_name,
		analysis_id,
		concept_id
	FROM #cohort_covariate_ref
	WHERE analysis_id = 504
	) ccr1
	ON cc1.covariate_id = ccr1.covariate_id
INNER JOIN #drug_group cg1
	ON ccr1.concept_id = cg1.descendant_concept_id
INNER JOIN concept c1
	ON cg1.ancestor_concept_id = c1.concept_id
WHERE len(c1.concept_code) = 3;

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT cc1.cohort_start_date,
	cc1.cohort_concept_id,
	cc1.person_id,
	CAST(cg1.ancestor_concept_id AS BIGINT) * 1000 + 601 AS covariate_id,
	COUNT(DISTINCT ccr1.concept_id) AS covariate_value
FROM #cohort_covariate cc1
INNER JOIN (
	SELECT covariate_id,
		covariate_name,
		analysis_id,
		concept_id
	FROM #cohort_covariate_ref
	WHERE analysis_id = 504
	) ccr1
	ON cc1.covariate_id = ccr1.covariate_id
INNER JOIN #drug_group cg1
	ON ccr1.concept_id = cg1.descendant_concept_id
INNER JOIN concept c1
	ON cg1.ancestor_concept_id = c1.concept_id
WHERE len(c1.concept_code) = 3
GROUP BY cc1.cohort_start_date,
	cc1.cohort_concept_id,
	cc1.person_id,
	CAST(cg1.ancestor_concept_id AS BIGINT) * 1000 + 601;

TRUNCATE TABLE #drug_group;

DROP TABLE #drug_group;

}
/**************************
***************************
PROCEDURE OCCURRENCE
***************************
**************************/
	{@use_covariate_procedure_occurrence} ? { {@use_covariate_procedure_occurrence_365d} ? {

--procedures exist:  episode in last 365d prior
INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
SELECT DISTINCT CAST(po1.procedure_concept_id AS BIGINT) * 1000 + 701 AS covariate_id,
	'Procedure occurrence record observed during 365d on or prior to cohort index:  ' + CAST(po1.procedure_concept_id AS VARCHAR) + '-' + CASE 
		WHEN c1.concept_name IS NOT NULL
			THEN c1.concept_name
		ELSE 'Unknown invalid concept'
		END AS covariate_name,
	701 AS analysis_id,
	po1.procedure_concept_id AS concept_id
FROM #cohort_person cp1
INNER JOIN procedure_occurrence po1
	ON cp1.subject_id = po1.person_id
LEFT JOIN concept c1
	ON po1.procedure_concept_id = c1.concept_id
WHERE po1.procedure_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
	AND po1.procedure_date <= cp1.cohort_start_date
	AND po1.procedure_date >= dateadd(dd, - 365, cp1.cohort_start_date);

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT DISTINCT cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id,
	CAST(po1.procedure_concept_id AS BIGINT) * 1000 + 701 AS covariate_id,
	1 AS covariate_value
FROM #cohort_person cp1
INNER JOIN procedure_occurrence po1
	ON cp1.subject_id = po1.person_id
WHERE po1.procedure_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
	AND po1.procedure_date <= cp1.cohort_start_date
	AND po1.procedure_date >= dateadd(dd, - 365, cp1.cohort_start_date);

} {@use_covariate_procedure_occurrence_30d} ? {

--procedures exist:  episode in last 30d prior
INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
SELECT DISTINCT CAST(po1.procedure_concept_id AS BIGINT) * 1000 + 702 AS covariate_id,
	'Procedure occurrence record observed during 30d on or prior to cohort index:  ' + CAST(po1.procedure_concept_id AS VARCHAR) + '-' + CASE 
		WHEN c1.concept_name IS NOT NULL
			THEN c1.concept_name
		ELSE 'Unknown invalid concept'
		END AS covariate_name,
	702 AS analysis_id,
	po1.procedure_concept_id AS concept_id
FROM #cohort_person cp1
INNER JOIN procedure_occurrence po1
	ON cp1.subject_id = po1.person_id
LEFT JOIN concept c1
	ON po1.procedure_concept_id = c1.concept_id
WHERE po1.procedure_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
	AND po1.procedure_date <= cp1.cohort_start_date
	AND po1.procedure_date >= dateadd(dd, - 30, cp1.cohort_start_date);

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT DISTINCT cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id,
	CAST(po1.procedure_concept_id AS BIGINT) * 1000 + 702 AS covariate_id,
	1 AS covariate_value
FROM #cohort_person cp1
INNER JOIN procedure_occurrence po1
	ON cp1.subject_id = po1.person_id
WHERE po1.procedure_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
	AND po1.procedure_date <= cp1.cohort_start_date
	AND po1.procedure_date >= dateadd(dd, - 30, cp1.cohort_start_date);

} }
/**************************
***************************
PROCEDURE GROUP
***************************
**************************/
	{@use_covariate_procedure_group} ? {


IF OBJECT_ID('tempdb..#procedure_group', 'U') IS NOT NULL
	DROP TABLE #procedure_group;

CREATE TABLE #procedure_group (
	descendant_concept_id INT,
	ancestor_concept_id INT
	);

--SNOMED
INSERT INTO #procedure_group (
	descendant_concept_id,
	ancestor_concept_id
	)
SELECT DISTINCT ca1.descendant_concept_id,
	ca1.ancestor_concept_id
FROM (
	SELECT covariate_id,
		covariate_name,
		analysis_id,
		concept_id
	FROM #cohort_covariate_ref
	WHERE analysis_id > 700
		AND analysis_id < 800
	) ccr1
INNER JOIN concept_ancestor ca1
	ON ccr1.concept_id = ca1.descendant_concept_id
INNER JOIN concept c1
	ON ca1.ancestor_concept_id = c1.concept_id
WHERE c1.vocabulary_id = 1
	AND c1.concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids });

INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
SELECT DISTINCT CAST(cg1.ancestor_concept_id AS BIGINT) * 1000 + ccr1.analysis_id AS covariate_id,
	CASE 
		WHEN analysis_id = 701
			THEN 'Procedure occurrence record observed during 365d on or prior to cohort index within procedure group:  '
		WHEN analysis_id = 702
			THEN 'Procedure occurrence record observed during 30d on or prior to cohort index within procedure group:  '
		ELSE 'Other procedure group analysis'
		END + CAST(cg1.ancestor_concept_id AS VARCHAR) + '-' + c1.concept_name AS covariate_name,
	ccr1.analysis_id,
	cg1.ancestor_concept_id AS concept_id
FROM #cohort_covariate cc1
INNER JOIN (
	SELECT covariate_id,
		covariate_name,
		analysis_id,
		concept_id
	FROM #cohort_covariate_ref
	WHERE analysis_id > 700
		AND analysis_id < 800
	) ccr1
	ON cc1.covariate_id = ccr1.covariate_id
INNER JOIN #procedure_group cg1
	ON ccr1.concept_id = cg1.descendant_concept_id
INNER JOIN concept c1
	ON cg1.ancestor_concept_id = c1.concept_id;

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT DISTINCT cc1.cohort_start_date,
	cc1.cohort_concept_id,
	cc1.person_id,
	CAST(cg1.ancestor_concept_id AS BIGINT) * 1000 + ccr1.analysis_id AS covariate_id,
	1 AS covariate_value
FROM #cohort_covariate cc1
INNER JOIN (
	SELECT covariate_id,
		covariate_name,
		analysis_id,
		concept_id
	FROM #cohort_covariate_ref
	WHERE analysis_id > 700
		AND analysis_id < 800
	) ccr1
	ON cc1.covariate_id = ccr1.covariate_id
INNER JOIN #procedure_group cg1
	ON ccr1.concept_id = cg1.descendant_concept_id;

TRUNCATE TABLE #procedure_group;

DROP TABLE #procedure_group;

}
/**************************
***************************
OBSERVATION
***************************
**************************/
	{@use_covariate_observation} ? { {@use_covariate_observation_365d} ? {

--observation exist:  episode in last 365d prior
INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
SELECT DISTINCT CAST(o1.observation_concept_id AS BIGINT) * 1000 + 901 AS covariate_id,
	'Observation record observed during 365d on or prior to cohort index:  ' + CAST(o1.observation_concept_id AS VARCHAR) + '-' + CASE 
		WHEN c1.concept_name IS NOT NULL
			THEN c1.concept_name
		ELSE 'Unknown invalid concept'
		END AS covariate_name,
	901 AS analysis_id,
	o1.observation_concept_id AS concept_id
FROM #cohort_person cp1
INNER JOIN observation o1
	ON cp1.subject_id = o1.person_id
LEFT JOIN concept c1
	ON o1.observation_concept_id = c1.concept_id
WHERE o1.observation_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
	AND o1.observation_date <= cp1.cohort_start_date
	AND o1.observation_date >= dateadd(dd, - 365, cp1.cohort_start_date);

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT DISTINCT cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id AS person_id,
	CAST(o1.observation_concept_id AS BIGINT) * 1000 + 901 AS covariate_id,
	1 AS covariate_value
FROM #cohort_person cp1
INNER JOIN observation o1
	ON cp1.subject_id = o1.person_id
WHERE o1.observation_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
	AND o1.observation_date <= cp1.cohort_start_date
	AND o1.observation_date >= dateadd(dd, - 365, cp1.cohort_start_date);

} {@use_covariate_observation_30d} ? {

--observation exist:  episode in last 30d prior
INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
SELECT DISTINCT CAST(o1.observation_concept_id AS BIGINT) * 1000 + 902 AS covariate_id,
	'Observation record observed during 30d on or prior to cohort index:  ' + CAST(o1.observation_concept_id AS VARCHAR) + '-' + CASE 
		WHEN c1.concept_name IS NOT NULL
			THEN c1.concept_name
		ELSE 'Unknown invalid concept'
		END AS covariate_name,
	902 AS analysis_id,
	o1.observation_concept_id AS concept_id
FROM #cohort_person cp1
INNER JOIN observation o1
	ON cp1.subject_id = o1.person_id
LEFT JOIN concept c1
	ON o1.observation_concept_id = c1.concept_id
WHERE o1.observation_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
	AND o1.observation_date <= cp1.cohort_start_date
	AND o1.observation_date >= dateadd(dd, - 30, cp1.cohort_start_date);

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT DISTINCT cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id AS person_id,
	CAST(o1.observation_concept_id AS BIGINT) * 1000 + 902 AS covariate_id,
	1 AS covariate_value
FROM #cohort_person cp1
INNER JOIN observation o1
	ON cp1.subject_id = o1.person_id
WHERE o1.observation_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
	AND o1.observation_date <= cp1.cohort_start_date
	AND o1.observation_date >= dateadd(dd, - 30, cp1.cohort_start_date);

} {@use_covariate_observation_below} ? {

--for numeric values with valid range, latest value within 180 below low
INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
SELECT DISTINCT CAST(t1.observation_concept_id AS BIGINT) * 1000 + 903 AS covariate_id,
	'Observation numeric value below normal range for latest value within 180d of cohort index:  ' + CAST(t1.observation_concept_id AS VARCHAR) + '-' + CASE 
		WHEN c1.concept_name IS NOT NULL
			THEN c1.concept_name
		ELSE 'Unknown invalid concept'
		END AS covariate_name,
	903 AS analysis_id,
	t1.observation_concept_id AS concept_id
FROM (
	SELECT cp1.cohort_concept_id,
		cp1.subject_id,
		o1.observation_concept_id,
		o1.value_as_number,
		o1.range_low,
		o1.range_high,
		ROW_NUMBER() OVER (
			PARTITION BY cp1.cohort_concept_id,
			cp1.subject_id,
			o1.observation_concept_id ORDER BY o1.observation_date DESC
			) AS rn1
	FROM #cohort_person cp1
	INNER JOIN observation o1
		ON cp1.subject_id = o1.person_id
	WHERE o1.observation_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
		AND o1.observation_date <= cp1.cohort_start_date
		AND o1.observation_date >= dateadd(dd, - 180, cp1.cohort_start_date)
		AND o1.value_as_number >= 0
		AND o1.range_low >= 0
		AND o1.range_high >= 0
	) t1
LEFT JOIN concept c1
	ON t1.observation_concept_id = c1.concept_id
WHERE RN1 = 1
	AND VALUE_AS_NUMBER < RANGE_LOW;

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT DISTINCT cohort_start_date,
	cohort_concept_id,
	subject_id AS person_id,
	CAST(observation_concept_id AS BIGINT) * 1000 + 903 AS covariate_id,
	1 AS covariate_value
FROM (
	SELECT cp1.cohort_start_date,
		cp1.cohort_concept_id,
		cp1.subject_id,
		o1.observation_concept_id,
		o1.value_as_number,
		o1.range_low,
		o1.range_high,
		ROW_NUMBER() OVER (
			PARTITION BY cp1.cohort_concept_id,
			cp1.subject_id,
			o1.observation_concept_id ORDER BY o1.observation_date DESC
			) AS rn1
	FROM #cohort_person cp1
	INNER JOIN observation o1
		ON cp1.subject_id = o1.person_id
	WHERE o1.observation_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
		AND o1.observation_date <= cp1.cohort_start_date
		AND o1.observation_date >= dateadd(dd, - 180, cp1.cohort_start_date)
		AND o1.value_as_number >= 0
		AND o1.range_low >= 0
		AND o1.range_high >= 0
	) t1
WHERE RN1 = 1
	AND VALUE_AS_NUMBER < RANGE_LOW;

} {@use_covariate_observation_above} ? {

--for numeric values with valid range, latest value above high
INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
SELECT DISTINCT CAST(t1.observation_concept_id AS BIGINT) * 1000 + 904 AS covariate_id,
	'Observation numeric value above normal range for latest value within 180d of cohort index:  ' + CAST(t1.observation_concept_id AS VARCHAR) + '-' + CASE 
		WHEN c1.concept_name IS NOT NULL
			THEN c1.concept_name
		ELSE 'Unknown invalid concept'
		END AS covariate_name,
	904 AS analysis_id,
	t1.observation_concept_id AS concept_id
FROM (
	SELECT cp1.cohort_concept_id,
		cp1.subject_id,
		o1.observation_concept_id,
		o1.value_as_number,
		o1.range_low,
		o1.range_high,
		ROW_NUMBER() OVER (
			PARTITION BY cp1.cohort_concept_id,
			cp1.subject_id,
			o1.observation_concept_id ORDER BY o1.observation_date DESC
			) AS rn1
	FROM #cohort_person cp1
	INNER JOIN observation o1
		ON cp1.subject_id = o1.person_id
	WHERE o1.observation_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
		AND o1.observation_date <= cp1.cohort_start_date
		AND o1.observation_date >= dateadd(dd, - 180, cp1.cohort_start_date)
		AND o1.value_as_number >= 0
		AND o1.range_low >= 0
		AND o1.range_high >= 0
	) t1
LEFT JOIN concept c1
	ON t1.observation_concept_id = c1.concept_id
WHERE RN1 = 1
	AND VALUE_AS_NUMBER > RANGE_HIGH;

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT DISTINCT cohort_start_date,
	cohort_concept_id,
	subject_id AS person_id,
	CAST(observation_concept_id AS BIGINT) * 1000 + 904 AS covariate_id,
	1 AS covariate_value
FROM (
	SELECT cp1.cohort_start_date,
		cp1.cohort_concept_id,
		cp1.subject_id,
		o1.observation_concept_id,
		o1.value_as_number,
		o1.range_low,
		o1.range_high,
		ROW_NUMBER() OVER (
			PARTITION BY cp1.cohort_concept_id,
			cp1.subject_id,
			o1.observation_concept_id ORDER BY o1.observation_date DESC
			) AS rn1
	FROM #cohort_person cp1
	INNER JOIN observation o1
		ON cp1.subject_id = o1.person_id
	WHERE o1.observation_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
		AND o1.observation_date <= cp1.cohort_start_date
		AND o1.observation_date >= dateadd(dd, - 180, cp1.cohort_start_date)
		AND o1.value_as_number >= 0
		AND o1.range_low >= 0
		AND o1.range_high >= 0
	) t1
WHERE RN1 = 1
	AND VALUE_AS_NUMBER > RANGE_HIGH;

} {@use_covariate_observation_count365d} ? {

--number of observations:  episode in last 365d prior
INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
SELECT DISTINCT CAST(o1.observation_concept_id AS BIGINT) * 1000 + 905 AS covariate_id,
	'Number of observations observed during 365d on or prior to cohort index:  ' + CAST(o1.observation_concept_id AS VARCHAR) + '-' + CASE 
		WHEN c1.concept_name IS NOT NULL
			THEN c1.concept_name
		ELSE 'Unknown invalid concept'
		END AS covariate_name,
	905 AS analysis_id,
	o1.observation_concept_id AS concept_id
FROM #cohort_person cp1
INNER JOIN observation o1
	ON cp1.subject_id = o1.person_id
LEFT JOIN concept c1
	ON o1.observation_concept_id = c1.concept_id
WHERE o1.observation_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
	AND o1.observation_date <= cp1.cohort_start_date
	AND o1.observation_date >= dateadd(dd, - 365, cp1.cohort_start_date);

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id AS person_id,
	CAST(o1.observation_concept_id AS BIGINT) * 1000 + 905 AS covariate_id,
	count(observation_id) AS covariate_value
FROM #cohort_person cp1
INNER JOIN observation o1
	ON cp1.subject_id = o1.person_id
WHERE o1.observation_concept_id NOT IN (0 {@excluded_covariate_concept_ids != '' } ? {, @excluded_covariate_concept_ids })
	AND o1.observation_date <= cp1.cohort_start_date
	AND o1.observation_date >= dateadd(dd, - 365, cp1.cohort_start_date)
GROUP BY cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id,
	CAST(o1.observation_concept_id AS BIGINT) * 1000 + 905;

} }
/**************************
***************************
DATA DENSITY CONCEPT COUNTS
***************************
**************************/
	{@use_covariate_concept_counts} ? {

--Number of distinct conditions observed in 365d on or prior to cohort index
INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
VALUES (
	1000,
	'Number of distinct conditions observed in 365d on or prior to cohort index',
	1000,
	0
	);

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id AS person_id,
	1000 AS covariate_id,
	COUNT(DISTINCT ce1.condition_concept_id) AS covariate_value
FROM #cohort_person cp1
INNER JOIN condition_era ce1
	ON cp1.subject_id = ce1.person_id
WHERE ce1.condition_era_start_date <= cp1.cohort_start_date
	AND ce1.condition_era_end_date >= dateadd(dd, - 365, cp1.cohort_start_date)
GROUP BY cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id;

--Number of distinct drug ingredients observed in 365d on or prior to cohort index
INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
VALUES (
	1001,
	'Number of distinct drug ingredients observed in 365d on or prior to cohort index',
	1001,
	0
	);

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id AS person_id,
	1001 AS covariate_id,
	COUNT(DISTINCT de1.drug_concept_id) AS covariate_value
FROM #cohort_person cp1
INNER JOIN drug_era de1
	ON cp1.subject_id = de1.person_id
WHERE de1.drug_era_start_date <= cp1.cohort_start_date
	AND de1.drug_era_start_date >= dateadd(dd, - 365, cp1.cohort_start_date)
GROUP BY cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id;

--Number of distinct procedures observed in 365d on or prior to cohort index
INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
VALUES (
	1002,
	'Number of distinct procedures observed in 365d on or prior to cohort index',
	1002,
	0
	);

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id AS person_id,
	1002 AS covariate_id,
	COUNT(DISTINCT po1.procedure_concept_id) AS covariate_value
FROM #cohort_person cp1
INNER JOIN procedure_occurrence po1
	ON cp1.subject_id = po1.person_id
WHERE po1.procedure_date <= cp1.cohort_start_date
	AND po1.procedure_date >= dateadd(dd, - 365, cp1.cohort_start_date)
GROUP BY cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id;

--Number of distinct observations observed in 365d on or prior to cohort index
INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
VALUES (
	1003,
	'Number of distinct observations observed in 365d on or prior to cohort index',
	1003,
	0
	);

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id AS person_id,
	1003 AS covariate_id,
	COUNT(DISTINCT o1.observation_concept_id) AS covariate_value
FROM #cohort_person cp1
INNER JOIN observation o1
	ON cp1.subject_id = o1.person_id
WHERE o1.observation_date <= cp1.cohort_start_date
	AND o1.observation_date >= dateadd(dd, - 365, cp1.cohort_start_date)
GROUP BY cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id;

--Number of visits observed in 365d on or prior to cohort index
INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
VALUES (
	1004,
	'Number of visits observed in 365d on or prior to cohort index',
	1004,
	0
	);

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id AS person_id,
	1004 AS covariate_id,
	COUNT(vo1.visit_occurrence_id) AS covariate_value
FROM #cohort_person cp1
INNER JOIN visit_occurrence vo1
	ON cp1.subject_id = vo1.person_id
WHERE vo1.visit_start_date <= cp1.cohort_start_date
	AND vo1.visit_start_date >= dateadd(dd, - 365, cp1.cohort_start_date)
GROUP BY cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id;

--Number of inpatient visits observed in 365d on or prior to cohort index
INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
VALUES (
	1005,
	'Number of inpatient visits observed in 365d on or prior to cohort index',
	1005,
	0
	);

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id AS person_id,
	1005 AS covariate_id,
	COUNT(vo1.visit_occurrence_id) AS covariate_value
FROM #cohort_person cp1
INNER JOIN visit_occurrence vo1
	ON cp1.subject_id = vo1.person_id
WHERE vo1.visit_start_date <= cp1.cohort_start_date
	AND vo1.visit_start_date >= dateadd(dd, - 365, cp1.cohort_start_date)
	AND vo1.place_of_service_concept_id = 9201
GROUP BY cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id;

--Number of ER visits observed in 365d on or prior to cohort index
INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
VALUES (
	1006,
	'Number of ER visits observed in 365d on or prior to cohort index',
	1006,
	0
	);

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id AS person_id,
	1006 AS covariate_id,
	COUNT(vo1.visit_occurrence_id) AS covariate_value
FROM #cohort_person cp1
INNER JOIN visit_occurrence vo1
	ON cp1.subject_id = vo1.person_id
WHERE vo1.visit_start_date <= cp1.cohort_start_date
	AND vo1.visit_start_date >= dateadd(dd, - 365, cp1.cohort_start_date)
	AND vo1.place_of_service_concept_id = 9203
GROUP BY cp1.cohort_start_date,
	cp1.cohort_concept_id,
	cp1.subject_id;

}
/**************************
***************************
RISK SCORES
***************************
**************************/
	{@use_covariate_risk_scores} ? {

--CHARLSON

IF OBJECT_ID('tempdb..#Charlson_codes', 'U') IS NOT NULL
	DROP TABLE #Charlson_codes;

CREATE TABLE #Charlson_codes (
	diag_category_id INT,
	icd9 VARCHAR(10)
	);


IF OBJECT_ID('tempdb..#Charlson_scoring', 'U') IS NOT NULL
	DROP TABLE #Charlson_scoring;

CREATE TABLE #Charlson_scoring (
	diag_category_id INT,
	diag_category_name VARCHAR(255),
	weight INT
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.00'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.01'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.02'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.10'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.11'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.12'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.20'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.21'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.22'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.30'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.31'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.32'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.40'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.41'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.42'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.50'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.51'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.52'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.6'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.60'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.61'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.62'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.7'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.70'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.71'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.72'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.80'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.81'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.82'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.90'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.91'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'410.92'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	1,
	'412'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'402.01'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'402.11'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'402.91'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'425'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'425.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'425.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'425.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'425.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'425.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'425.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'425.7'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'425.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'425.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'428'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'428.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'428.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'428.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'428.20'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'428.21'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'428.22'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'428.23'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'428.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'428.30'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'428.31'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'428.32'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'428.33'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'428.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'428.40'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'428.41'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'428.42'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'428.43'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'428.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	2,
	'429.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'440'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'440.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'440.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'440.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'440.20'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'440.21'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'440.22'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'440.23'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'440.24'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'440.29'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'440.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'440.30'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'440.31'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'440.32'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'440.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'440.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'440.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'441'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'441.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'441.00'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'441.01'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'441.02'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'441.03'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'441.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'441.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'441.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'441.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'441.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'441.6'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'441.7'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'441.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'442'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'442.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'442.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'442.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'442.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'442.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'442.81'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'442.82'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'442.83'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'442.84'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'442.89'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'442.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'443.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'443.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'443.21'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'443.22'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'443.23'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'443.24'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'443.29'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'443.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'443.81'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'443.82'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'443.89'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'443.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'447.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	3,
	'785.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'362.34'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'430'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'431'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'432'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'432.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'432.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'432.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'433'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'433.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'433.00'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'433.01'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'433.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'433.10'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'433.11'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'433.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'433.20'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'433.21'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'433.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'433.30'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'433.31'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'433.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'433.80'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'433.81'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'433.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'433.90'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'433.91'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'434'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'434.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'434.00'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'434.01'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'434.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'434.10'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'434.11'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'434.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'434.90'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'434.91'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'435'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'435.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'435.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'435.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'435.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'435.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'435.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'436'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'437'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'437.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'437.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'437.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.10'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.11'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.12'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.19'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.20'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.21'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.22'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.30'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.31'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.32'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.40'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.41'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.42'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.50'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.51'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.52'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.53'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.6'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.7'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.81'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.82'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.83'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.84'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.85'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.89'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'438.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'781.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'784.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'997.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'997.00'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'997.01'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'997.02'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	4,
	'997.09'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	5,
	'290'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	5,
	'290.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	5,
	'290.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	5,
	'290.10'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	5,
	'290.11'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	5,
	'290.12'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	5,
	'290.13'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	5,
	'290.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	5,
	'290.20'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	5,
	'290.21'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	5,
	'290.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	5,
	'290.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	5,
	'290.40'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	5,
	'290.41'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	5,
	'290.42'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	5,
	'290.43'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	5,
	'290.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	5,
	'290.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	5,
	'331'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	5,
	'331.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	5,
	'331.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	5,
	'331.11'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	5,
	'331.19'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	5,
	'331.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'415.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'416.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'416.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'491'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'491.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'491.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'491.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'491.20'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'491.21'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'491.22'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'491.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'491.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'492'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'492.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'492.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'493'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'493.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'493.00'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'493.01'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'493.02'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'493.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'493.10'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'493.11'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'493.12'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'493.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'493.20'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'493.21'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'493.22'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'493.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'493.81'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'493.82'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'493.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'493.90'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'493.91'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'493.92'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	6,
	'494'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	7,
	'710'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	7,
	'710.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	7,
	'710.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	7,
	'710.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	7,
	'710.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	7,
	'710.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	7,
	'710.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	7,
	'710.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	7,
	'710.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	7,
	'714'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	7,
	'714.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	7,
	'714.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	7,
	'714.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	7,
	'714.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	7,
	'714.30'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	7,
	'714.31'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	7,
	'714.32'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	7,
	'714.33'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	7,
	'714.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	7,
	'714.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	7,
	'714.81'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	7,
	'714.89'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	7,
	'714.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'531'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'531.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'531.00'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'531.01'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'531.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'531.10'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'531.11'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'531.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'531.20'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'531.21'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'531.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'531.30'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'531.31'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'531.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'531.40'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'531.41'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'531.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'531.50'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'531.51'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'531.6'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'531.60'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'531.61'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'531.7'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'531.70'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'531.71'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'531.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'531.90'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'531.91'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'532'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'532.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'532.00'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'532.01'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'532.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'532.10'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'532.11'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'532.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'532.20'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'532.21'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'532.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'532.30'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'532.31'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'532.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'532.40'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'532.41'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'532.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'532.50'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'532.51'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'532.6'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'532.60'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'532.61'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'532.7'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'532.70'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'532.71'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'532.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'532.90'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'532.91'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'533'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'533.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'533.00'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'533.01'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'533.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'533.10'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'533.11'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'533.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'533.20'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'533.21'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'533.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'533.30'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'533.31'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'533.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'533.40'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'533.41'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'533.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'533.50'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'533.51'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'533.6'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'533.60'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'533.61'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'533.7'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'533.70'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'533.71'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'533.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'533.90'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'533.91'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'534'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'534.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'534.00'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'534.01'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'534.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'534.10'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'534.11'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'534.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'534.20'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'534.21'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'534.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'534.30'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'534.31'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'534.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'534.40'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'534.41'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'534.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'534.50'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'534.51'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'534.6'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'534.60'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'534.61'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'534.7'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'534.70'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'534.71'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'534.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'534.90'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	8,
	'534.91'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	9,
	'571.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	9,
	'571.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	9,
	'571.6'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	9,
	'571.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	9,
	'571.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	10,
	'250'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	10,
	'250.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	10,
	'250.00'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	10,
	'250.01'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	10,
	'250.02'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	10,
	'250.03'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	10,
	'250.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	10,
	'250.10'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	10,
	'250.11'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	10,
	'250.12'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	10,
	'250.13'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	10,
	'250.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	10,
	'250.20'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	10,
	'250.21'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	10,
	'250.22'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	10,
	'250.23'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	10,
	'250.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	10,
	'250.30'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	10,
	'250.31'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	10,
	'250.32'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	10,
	'250.33'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	11,
	'250.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	11,
	'250.40'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	11,
	'250.41'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	11,
	'250.42'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	11,
	'250.43'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	11,
	'250.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	11,
	'250.50'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	11,
	'250.51'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	11,
	'250.52'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	11,
	'250.53'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	11,
	'250.6'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	11,
	'250.60'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	11,
	'250.61'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	11,
	'250.62'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	11,
	'250.63'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	11,
	'250.7'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	11,
	'250.70'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	11,
	'250.71'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	11,
	'250.72'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	11,
	'250.73'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	11,
	'250.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	11,
	'250.80'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	11,
	'250.81'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	11,
	'250.82'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	11,
	'250.83'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	11,
	'250.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	11,
	'250.90'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	11,
	'250.91'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	11,
	'250.92'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	11,
	'250.93'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'342'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'342.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'342.00'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'342.01'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'342.02'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'342.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'342.10'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'342.11'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'342.12'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'342.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'342.80'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'342.81'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'342.82'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'342.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'342.90'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'342.91'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'342.92'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'344'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'344.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'344.00'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'344.01'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'344.02'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'344.03'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'344.04'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'344.09'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'344.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'344.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'344.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'344.30'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'344.31'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'344.32'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'344.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'344.40'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'344.41'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'344.42'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'344.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'344.6'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'344.60'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'344.61'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'344.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'344.81'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'344.89'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	12,
	'344.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	13,
	'585'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	13,
	'585.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	13,
	'585.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	13,
	'585.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	13,
	'585.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	13,
	'585.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	13,
	'585.6'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	13,
	'585.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	13,
	'586'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	13,
	'V42.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	13,
	'V45.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	13,
	'V45.11'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	13,
	'V45.12'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	13,
	'V56'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	13,
	'V56.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	13,
	'V56.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	13,
	'V56.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	13,
	'V56.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	13,
	'V56.31'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	13,
	'V56.32'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	13,
	'V56.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'140'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'140.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'140.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'140.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'140.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'140.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'140.6'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'140.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'140.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'141'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'141.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'141.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'141.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'141.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'141.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'141.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'141.6'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'141.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'141.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'142'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'142.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'142.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'142.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'142.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'142.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'143'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'143.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'143.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'143.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'143.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'144'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'144.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'144.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'144.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'144.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'145'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'145.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'145.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'145.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'145.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'145.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'145.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'145.6'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'145.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'145.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'146'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'146.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'146.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'146.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'146.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'146.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'146.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'146.6'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'146.7'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'146.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'146.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'147'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'147.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'147.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'147.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'147.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'147.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'147.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'148'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'148.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'148.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'148.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'148.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'148.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'148.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'149'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'149.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'149.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'149.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'149.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'150'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'150.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'150.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'150.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'150.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'150.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'150.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'150.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'150.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'151'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'151.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'151.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'151.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'151.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'151.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'151.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'151.6'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'151.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'151.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'152'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'152.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'152.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'152.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'152.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'152.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'152.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'153'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'153.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'153.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'153.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'153.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'153.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'153.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'153.6'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'153.7'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'153.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'153.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'154'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'154.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'154.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'154.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'154.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'154.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'155'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'155.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'155.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'155.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'156'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'156.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'156.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'156.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'156.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'156.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'157'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'157.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'157.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'157.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'157.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'157.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'157.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'157.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'158'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'158.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'158.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'158.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'159'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'159.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'159.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'159.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'159.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'160'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'160.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'160.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'160.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'160.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'160.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'160.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'160.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'160.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'161'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'161.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'161.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'161.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'161.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'161.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'161.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'162'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'162.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'162.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'162.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'162.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'162.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'162.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'162.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'163'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'163.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'163.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'163.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'163.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'164'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'164.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'164.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'164.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'164.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'164.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'164.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'165'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'165.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'165.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'165.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'170'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'170.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'170.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'170.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'170.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'170.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'170.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'170.6'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'170.7'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'170.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'170.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'171'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'171.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'171.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'171.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'171.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'171.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'171.6'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'171.7'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'171.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'171.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'174'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'174.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'174.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'174.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'174.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'174.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'174.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'174.6'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'174.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'174.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'175'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'175.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'175.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'176'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'176.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'176.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'176.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'176.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'176.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'176.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'176.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'176.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'179'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'180'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'180.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'180.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'180.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'180.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'181'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'182'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'182.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'182.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'182.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'183'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'183.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'183.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'183.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'183.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'183.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'183.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'183.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'184'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'184.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'184.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'184.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'184.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'184.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'184.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'184.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'185'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'186'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'186.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'186.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'187'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'187.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'187.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'187.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'187.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'187.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'187.6'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'187.7'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'187.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'187.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'188'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'188.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'188.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'188.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'188.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'188.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'188.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'188.6'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'188.7'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'188.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'188.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'189'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'189.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'189.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'189.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'189.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'189.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'189.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'189.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'190'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'190.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'190.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'190.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'190.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'190.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'190.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'190.6'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'190.7'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'190.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'190.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'191'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'191.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'191.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'191.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'191.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'191.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'191.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'191.6'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'191.7'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'191.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'191.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'192'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'192.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'192.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'192.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'192.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'192.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'192.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'193'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'194'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'194.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'194.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'194.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'194.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'194.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'194.6'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'194.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'194.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'195'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'195.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'195.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'195.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'195.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'195.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'195.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'195.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.00'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.01'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.02'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.03'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.04'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.05'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.06'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.07'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.08'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.10'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.11'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.12'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.13'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.14'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.15'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.16'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.17'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.18'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.20'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.21'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.22'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.23'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.24'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.25'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.26'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.27'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.28'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.30'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.31'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.32'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.33'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.34'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.35'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.36'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.37'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.38'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.40'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.41'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.42'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.43'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.44'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.45'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.46'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.47'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.48'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.50'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.51'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.52'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.54'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.55'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.56'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.58'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.60'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.61'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.62'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.63'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.64'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.65'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.66'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.67'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.68'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.7'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.70'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.71'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.72'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.73'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.74'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.75'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.76'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.77'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.78'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.80'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.81'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.82'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.83'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.84'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.85'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.86'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.87'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'200.88'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.00'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.01'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.02'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.03'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.04'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.05'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.06'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.07'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.08'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.10'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.11'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.12'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.13'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.14'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.15'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.16'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.17'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.18'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.20'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.21'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.22'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.23'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.24'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.25'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.26'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.27'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.28'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.40'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.41'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.42'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.43'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.44'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.45'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.46'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.47'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.48'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.50'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.51'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.52'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.53'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.54'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.55'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.56'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.57'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.58'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.6'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.60'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.61'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.62'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.63'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.64'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.65'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.66'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.67'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.68'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.7'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.70'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.71'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.72'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.73'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.74'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.75'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.76'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.77'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.78'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.90'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.91'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.92'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.93'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.94'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.95'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.96'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.97'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'201.98'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.00'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.01'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.02'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.03'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.04'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.05'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.06'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.07'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.08'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.10'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.11'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.12'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.13'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.14'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.15'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.16'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.17'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.18'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.20'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.21'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.22'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.23'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.24'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.25'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.26'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.27'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.28'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.30'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.31'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.32'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.33'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.34'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.35'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.36'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.37'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.38'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.40'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.41'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.42'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.43'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.44'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.45'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.46'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.47'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.48'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.50'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.51'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.52'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.53'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.54'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.55'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.56'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.57'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.58'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.6'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.60'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.61'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.62'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.63'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.64'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.65'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.66'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.67'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.68'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.7'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.70'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.71'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.72'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.73'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.74'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.75'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.76'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.77'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.78'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.80'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.81'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.82'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.83'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.84'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.85'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.86'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.87'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.88'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.90'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.91'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.92'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.93'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.94'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.95'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.96'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.97'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'202.98'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'203'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'203.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'203.00'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'203.01'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'203.02'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'203.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'203.10'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'203.11'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'203.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'203.80'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'203.81'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'204'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'204.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'204.00'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'204.01'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'204.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'204.10'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'204.11'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'204.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'204.20'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'204.21'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'204.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'204.80'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'204.81'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'204.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'204.90'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'204.91'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'205'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'205.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'205.00'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'205.01'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'205.02'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'205.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'205.10'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'205.11'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'205.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'205.20'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'205.21'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'205.22'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'205.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'205.30'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'205.31'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'205.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'205.80'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'205.81'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'205.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'205.90'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'205.91'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'206'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'206.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'206.00'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'206.01'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'206.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'206.10'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'206.11'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'206.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'206.20'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'206.21'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'206.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'206.80'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'206.81'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'206.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'206.90'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'206.91'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'207'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'207.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'207.00'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'207.01'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'207.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'207.10'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'207.11'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'207.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'207.20'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'207.21'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'207.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'207.80'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'207.81'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'208'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'208.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'208.00'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'208.01'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'208.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'208.10'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'208.11'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'208.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'208.20'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'208.21'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'208.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'208.80'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'208.81'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'208.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'208.90'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'208.91'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'208.92'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'273.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'273.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	14,
	'V10.46'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	15,
	'456.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	15,
	'456.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	15,
	'456.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	15,
	'456.20'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	15,
	'456.21'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	15,
	'572.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	15,
	'572.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	15,
	'572.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'196'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'196.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'196.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'196.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'196.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'196.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'196.6'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'196.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'196.9'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'197'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'197.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'197.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'197.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'197.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'197.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'197.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'197.6'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'197.7'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'197.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'198'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'198.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'198.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'198.2'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'198.3'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'198.4'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'198.5'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'198.6'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'198.7'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'198.8'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'198.81'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'198.82'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'198.89'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'199'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'199.0'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	16,
	'199.1'
	);

INSERT INTO #Charlson_codes (
	diag_category_id,
	icd9
	)
VALUES (
	17,
	'042'
	);

INSERT INTO #Charlson_scoring (
	diag_category_id,
	diag_category_name,
	weight
	)
VALUES (
	1,
	'Myocardial infarction',
	1
	);

INSERT INTO #Charlson_scoring (
	diag_category_id,
	diag_category_name,
	weight
	)
VALUES (
	2,
	'Congestive heart failure',
	1
	);

INSERT INTO #Charlson_scoring (
	diag_category_id,
	diag_category_name,
	weight
	)
VALUES (
	3,
	'Peripheral vascular disease',
	1
	);

INSERT INTO #Charlson_scoring (
	diag_category_id,
	diag_category_name,
	weight
	)
VALUES (
	4,
	'Cerebrovascular disease',
	1
	);

INSERT INTO #Charlson_scoring (
	diag_category_id,
	diag_category_name,
	weight
	)
VALUES (
	5,
	'Dementia',
	1
	);

INSERT INTO #Charlson_scoring (
	diag_category_id,
	diag_category_name,
	weight
	)
VALUES (
	6,
	'Chronic pulmonary disease',
	1
	);

INSERT INTO #Charlson_scoring (
	diag_category_id,
	diag_category_name,
	weight
	)
VALUES (
	7,
	'Rheumatologic disease',
	1
	);

INSERT INTO #Charlson_scoring (
	diag_category_id,
	diag_category_name,
	weight
	)
VALUES (
	8,
	'Peptic ulcer disease',
	1
	);

INSERT INTO #Charlson_scoring (
	diag_category_id,
	diag_category_name,
	weight
	)
VALUES (
	9,
	'Mild liver disease',
	1
	);

INSERT INTO #Charlson_scoring (
	diag_category_id,
	diag_category_name,
	weight
	)
VALUES (
	10,
	'Diabetes (mild to moderate)',
	1
	);

INSERT INTO #Charlson_scoring (
	diag_category_id,
	diag_category_name,
	weight
	)
VALUES (
	11,
	'Diabetes with chronic complications',
	2
	);

INSERT INTO #Charlson_scoring (
	diag_category_id,
	diag_category_name,
	weight
	)
VALUES (
	12,
	'Hemoplegia or paralegia',
	2
	);

INSERT INTO #Charlson_scoring (
	diag_category_id,
	diag_category_name,
	weight
	)
VALUES (
	13,
	'Renal disease',
	2
	);

INSERT INTO #Charlson_scoring (
	diag_category_id,
	diag_category_name,
	weight
	)
VALUES (
	14,
	'Any malignancy',
	2
	);

INSERT INTO #Charlson_scoring (
	diag_category_id,
	diag_category_name,
	weight
	)
VALUES (
	15,
	'Moderate to severe liver disease',
	3
	);

INSERT INTO #Charlson_scoring (
	diag_category_id,
	diag_category_name,
	weight
	)
VALUES (
	16,
	'Metastatic solid tumor',
	6
	);

INSERT INTO #Charlson_scoring (
	diag_category_id,
	diag_category_name,
	weight
	)
VALUES (
	17,
	'AIDS',
	6
	);

INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
VALUES (
	1100,
	'Charlson Index - Romano adaptation, using conditions all time on or prior to cohort index',
	1100,
	0
	);

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT cohort_start_date,
	cohort_concept_id,
	subject_id AS person_id,
	1100 AS covariate_id,
	SUM(weight) AS covariate_value
FROM (
	SELECT DISTINCT cp1.cohort_start_date,
		cp1.cohort_concept_id,
		cp1.subject_id,
		cs1.diag_category_id,
		cs1.weight
	FROM #cohort_person cp1
	INNER JOIN condition_era ce1
		ON cp1.subject_id = ce1.person_id
	INNER JOIN (
		SELECT diag_category_id,
			stcm1.target_concept_id
		FROM #Charlson_codes cc1
		INNER JOIN source_to_concept_map stcm1
			ON cc1.icd9 = stcm1.source_code
		WHERE stcm1.source_vocabulary_id = 2
			AND stcm1.target_vocabulary_id = 1
		) c1
		ON ce1.condition_concept_id = c1.target_concept_id
	INNER JOIN #Charlson_scoring cs1
		ON c1.diag_category_id = cs1.diag_category_id
	WHERE ce1.condition_era_start_date <= cp1.cohort_start_date
	) t1
GROUP BY cohort_start_date,
	cohort_concept_id,
	subject_id;

TRUNCATE TABLE #Charlson_codes;

DROP TABLE #Charlson_codes;

TRUNCATE TABLE #Charlson_scoring;

DROP TABLE #Charlson_scoring;

--DCSI

IF OBJECT_ID('tempdb..#DCSI_scoring', 'U') IS NOT NULL
	DROP TABLE #DCSI_scoring;

CREATE TABLE #DCSI_scoring (
	DCSI_category VARCHAR(255),
	DCSI_ICD9_code VARCHAR(255),
	DCSI_concept_id INT,
	DCSI_score INT
	);

INSERT INTO #DCSI_scoring (
	DCSI_category,
	DCSI_ICD9_code,
	DCSI_concept_id,
	DCSI_score
	)
SELECT 'Retinopathy' AS DCSI_category,
	source_code,
	target_concept_id,
	1 AS DCSI_score
FROM SOURCE_TO_CONCEPT_MAP
WHERE SOURCE_VOCABULARY_ID = 2
	AND target_vocabulary_id = 1
	AND (
		source_code LIKE '250.5%'
		OR source_code IN ('362.01', '362.1', '362.83', '362.53', '362.81', '362.82')
		)
ORDER BY source_code;

INSERT INTO #DCSI_scoring (
	DCSI_category,
	DCSI_ICD9_code,
	DCSI_concept_id,
	DCSI_score
	)
SELECT 'Retinopathy' AS DCSI_category,
	source_code,
	target_concept_id,
	2 AS DCSI_score
FROM SOURCE_TO_CONCEPT_MAP
WHERE SOURCE_VOCABULARY_ID = 2
	AND target_vocabulary_id = 1
	AND (
		source_code LIKE '361%'
		OR source_code LIKE '369%'
		OR source_code IN ('362.02', '379.23')
		)
ORDER BY source_code;

INSERT INTO #DCSI_scoring (
	DCSI_category,
	DCSI_ICD9_code,
	DCSI_concept_id,
	DCSI_score
	)
SELECT 'Nephropathy' AS DCSI_category,
	source_code,
	target_concept_id,
	1 AS DCSI_score
FROM SOURCE_TO_CONCEPT_MAP
WHERE SOURCE_VOCABULARY_ID = 2
	AND target_vocabulary_id = 1
	AND (
		source_code IN ('250.4', '580', '581', '581.81', '582', '583')
		OR source_code LIKE '580%'
		OR source_code LIKE '581%'
		OR source_code LIKE '582%'
		OR source_code LIKE '583%'
		)
ORDER BY source_code;

INSERT INTO #DCSI_scoring (
	DCSI_category,
	DCSI_ICD9_code,
	DCSI_concept_id,
	DCSI_score
	)
SELECT 'Nephropathy' AS DCSI_category,
	source_code,
	target_concept_id,
	2 AS DCSI_score
FROM SOURCE_TO_CONCEPT_MAP
WHERE SOURCE_VOCABULARY_ID = 2
	AND target_vocabulary_id = 1
	AND (
		source_code IN ('585', '586', '593.9')
		OR source_code LIKE '585%'
		OR source_code LIKE '586%'
		OR source_code LIKE '593.9%'
		)
ORDER BY source_code;

INSERT INTO #DCSI_scoring (
	DCSI_category,
	DCSI_ICD9_code,
	DCSI_concept_id,
	DCSI_score
	)
SELECT 'Neuropathy' AS DCSI_category,
	source_code,
	target_concept_id,
	1 AS DCSI_score
FROM SOURCE_TO_CONCEPT_MAP
WHERE SOURCE_VOCABULARY_ID = 2
	AND target_vocabulary_id = 1
	AND (
		source_code IN ('356.9', '250.6', '358.1', '951.0', '951.1', '951.3', '713.5', '357.2', '596.54', '337.0', '337.1', '564.5', '536.3', '458.0')
		OR (
			source_code >= '354.0'
			AND source_code <= '355.99'
			)
		OR source_code LIKE '356.9%'
		OR source_code LIKE '250.6%'
		OR source_code LIKE '358.1%'
		OR source_code LIKE '951.0%'
		OR source_code LIKE '951.1%'
		OR source_code LIKE '951.3%'
		OR source_code LIKE '713.5%'
		OR source_code LIKE '357.2%'
		OR source_code LIKE '337.0%'
		OR source_code LIKE '337.1%'
		OR source_code LIKE '564.5%'
		OR source_code LIKE '536.3%'
		OR source_code LIKE '458.0%'
		)
ORDER BY source_code;

INSERT INTO #DCSI_scoring (
	DCSI_category,
	DCSI_ICD9_code,
	DCSI_concept_id,
	DCSI_score
	)
SELECT 'Cerebrovascular' AS DCSI_category,
	source_code,
	target_concept_id,
	1 AS DCSI_score
FROM SOURCE_TO_CONCEPT_MAP
WHERE SOURCE_VOCABULARY_ID = 2
	AND target_vocabulary_id = 1
	AND (source_code LIKE '435%')
ORDER BY source_code;

INSERT INTO #DCSI_scoring (
	DCSI_category,
	DCSI_ICD9_code,
	DCSI_concept_id,
	DCSI_score
	)
SELECT 'Cerebrovascular' AS DCSI_category,
	source_code,
	target_concept_id,
	2 AS DCSI_score
FROM SOURCE_TO_CONCEPT_MAP
WHERE SOURCE_VOCABULARY_ID = 2
	AND target_vocabulary_id = 1
	AND (
		source_code IN ('431', '433', '434', '436')
		OR source_code LIKE '431%'
		OR source_code LIKE '433%'
		OR source_code LIKE '434%'
		OR source_code LIKE '436%'
		)
ORDER BY source_code;

INSERT INTO #DCSI_scoring (
	DCSI_category,
	DCSI_ICD9_code,
	DCSI_concept_id,
	DCSI_score
	)
SELECT 'Cardiovascular' AS DCSI_category,
	source_code,
	target_concept_id,
	1 AS DCSI_score
FROM SOURCE_TO_CONCEPT_MAP
WHERE SOURCE_VOCABULARY_ID = 2
	AND target_vocabulary_id = 1
	AND (
		source_code LIKE '440%'
		OR source_code LIKE '411%'
		OR source_code LIKE '413%'
		OR source_code LIKE '414%'
		OR source_code LIKE '429.2%'
		)
ORDER BY source_code;

INSERT INTO #DCSI_scoring (
	DCSI_category,
	DCSI_ICD9_code,
	DCSI_concept_id,
	DCSI_score
	)
SELECT 'Cardiovascular' AS DCSI_category,
	source_code,
	target_concept_id,
	2 AS DCSI_score
FROM SOURCE_TO_CONCEPT_MAP
WHERE SOURCE_VOCABULARY_ID = 2
	AND target_vocabulary_id = 1
	AND (
		source_code LIKE '410%'
		OR source_code LIKE '427.1%'
		OR source_code LIKE '427.3%'
		OR source_code LIKE '427.4%'
		OR source_code LIKE '427.5%'
		OR source_code LIKE '412%'
		OR source_code LIKE '428%'
		OR source_code LIKE '441%'
		OR source_code IN ('440.23', '440.24')
		)
ORDER BY source_code;

INSERT INTO #DCSI_scoring (
	DCSI_category,
	DCSI_ICD9_code,
	DCSI_concept_id,
	DCSI_score
	)
SELECT 'Peripheral vascular disease' AS DCSI_category,
	source_code,
	target_concept_id,
	1 AS DCSI_score
FROM SOURCE_TO_CONCEPT_MAP
WHERE SOURCE_VOCABULARY_ID = 2
	AND target_vocabulary_id = 1
	AND (
		source_code LIKE '250.7%'
		OR source_code LIKE '442.3%'
		OR source_code LIKE '892.1%'
		OR source_code LIKE '443.9%'
		OR source_code IN ('443.81')
		)
ORDER BY source_code;

INSERT INTO #DCSI_scoring (
	DCSI_category,
	DCSI_ICD9_code,
	DCSI_concept_id,
	DCSI_score
	)
SELECT 'Peripheral vascular disease' AS DCSI_category,
	source_code,
	target_concept_id,
	2 AS DCSI_score
FROM SOURCE_TO_CONCEPT_MAP
WHERE SOURCE_VOCABULARY_ID = 2
	AND target_vocabulary_id = 1
	AND (
		source_code LIKE '785.4%'
		OR source_code LIKE '707.1%'
		OR source_code LIKE '040.0%'
		OR source_code IN ('444.22')
		)
ORDER BY source_code;

INSERT INTO #DCSI_scoring (
	DCSI_category,
	DCSI_ICD9_code,
	DCSI_concept_id,
	DCSI_score
	)
SELECT 'Metabolic' AS DCSI_category,
	source_code,
	target_concept_id,
	2 AS DCSI_score
FROM SOURCE_TO_CONCEPT_MAP
WHERE SOURCE_VOCABULARY_ID = 2
	AND target_vocabulary_id = 1
	AND (
		source_code LIKE '250.1%'
		OR source_code LIKE '250.2%'
		OR source_code LIKE '250.3%'
		)
ORDER BY source_code;

INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
VALUES (
	1101,
	'Diabetes Comorbidity Severity Index (DCSI), using conditions all time on or prior to cohort index',
	1101,
	0
	);

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT cohort_start_date,
	cohort_concept_id,
	subject_id AS person_id,
	1101 AS covariate_id,
	SUM(max_score) AS covariate_value
FROM (
	SELECT cp1.cohort_start_date,
		cp1.cohort_concept_id,
		cp1.subject_id,
		ds1.dcsi_category,
		max(ds1.DCSI_score) AS max_score
	FROM #cohort_person cp1
	INNER JOIN condition_era ce1
		ON cp1.subject_id = ce1.person_id
	INNER JOIN #DCSI_scoring ds1
		ON ce1.condition_concept_id = ds1.DCSI_concept_id
	WHERE ce1.condition_era_start_date <= cp1.cohort_start_date
	GROUP BY cp1.cohort_start_date,
		cp1.cohort_concept_id,
		cp1.subject_id,
		ds1.dcsi_category
	) t1
GROUP BY cohort_start_date,
	cohort_concept_id,
	subject_id;

TRUNCATE TABLE #DCSI_scoring;

DROP TABLE #DCSI_scoring;

/*************

other risk scores to consider adding:

CHADS2 for stroke
HAS_BLED

**************/
}
/**************************
***************************
INTERACTION YEAR
***************************
**************************/
	{@use_covariate_interaction_year} ? {

INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
SELECT DISTINCT CAST(ccr1.covariate_id AS BIGINT) * 10000 + YEAR(cp1.cohort_start_date) AS covariate_id,
	ccr1.covariate_name + ' * interaction term with index year: ' + CAST(YEAR(cp1.cohort_start_date) AS VARCHAR) AS covariate_name,
	ccr1.analysis_id,
	ccr1.concept_id
FROM #cohort_person cp1
INNER JOIN #cohort_covariate cc1
	ON cp1.subject_id = cc1.person_id
		AND cp1.cohort_concept_id = cc1.cohort_concept_id
INNER JOIN #cohort_covariate_ref ccr1
	ON cc1.covariate_id = ccr1.covariate_id
WHERE ccr1.analysis_id NOT IN (5)
	AND ccr1.covariate_id > 1;

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT DISTINCT cc1.cohort_start_date,
	cc1.cohort_concept_id,
	cc1.person_id,
	CAST(cc1.covariate_id AS BIGINT) * 10000 + CAST(YEAR(cp1.cohort_start_date) AS BIGINT) AS covariate_id,
	cc1.covariate_value AS covariate_value
FROM #cohort_person cp1
INNER JOIN #cohort_covariate cc1
	ON cp1.subject_id = cc1.person_id
		AND cp1.cohort_concept_id = cc1.cohort_concept_id
INNER JOIN #cohort_covariate_ref ccr1
	ON cc1.covariate_id = ccr1.covariate_id
WHERE ccr1.analysis_id NOT IN (5)
	AND ccr1.covariate_id > 1;

}
/**************************
***************************
INTERACTION MONTH
***************************
**************************/
	{@use_covariate_interaction_month} ? {

INSERT INTO #cohort_covariate_ref (
	covariate_id,
	covariate_name,
	analysis_id,
	concept_id
	)
SELECT DISTINCT CAST(ccr1.covariate_id AS BIGINT) * 10000 + CAST(MONTH(cp1.cohort_start_date) AS BIGINT) AS covariate_id,
	ccr1.covariate_name + ' * interaction term with index month: ' + CAST(MONTH(cp1.cohort_start_date) AS VARCHAR) AS covariate_name,
	ccr1.analysis_id,
	ccr1.concept_id
FROM #cohort_person cp1
INNER JOIN #cohort_covariate cc1
	ON cp1.subject_id = cc1.person_id
		AND cp1.cohort_concept_id = cc1.cohort_concept_id
INNER JOIN #cohort_covariate_ref ccr1
	ON cc1.covariate_id = ccr1.covariate_id
WHERE ccr1.analysis_id NOT IN (6)
	AND ccr1.covariate_id > 1;

INSERT INTO #cohort_covariate (
	cohort_start_date,
	cohort_concept_id,
	person_id,
	covariate_id,
	covariate_value
	)
SELECT DISTINCT cc1.cohort_start_date,
	cc1.cohort_concept_id,
	cc1.person_id,
	CAST(cc1.covariate_id AS BIGINT) * 10000 + CAST(MONTH(cp1.cohort_start_date) AS BIGINT) AS covariate_id,
	cc1.covariate_value AS covariate_value
FROM #cohort_person cp1
INNER JOIN #cohort_covariate cc1
	ON cp1.subject_id = cc1.person_id
		AND cp1.cohort_concept_id = cc1.cohort_concept_id
INNER JOIN #cohort_covariate_ref ccr1
	ON cc1.covariate_id = ccr1.covariate_id
WHERE ccr1.analysis_id NOT IN (6)
	AND ccr1.covariate_id > 1;

} {@delete_covariates_small_count != 0 } ? {

DELETE
FROM #cohort_covariate_ref
WHERE covariate_id IN (
		SELECT covariate_id
		FROM #cohort_covariate
		GROUP BY covariate_id
		HAVING COUNT(person_id) < @delete_covariates_small_count
		);

DELETE
FROM #cohort_covariate
WHERE covariate_id IN (
		SELECT covariate_id
		FROM #cohort_covariate
		GROUP BY covariate_id
		HAVING COUNT(person_id) < @delete_covariates_small_count
		);

}
