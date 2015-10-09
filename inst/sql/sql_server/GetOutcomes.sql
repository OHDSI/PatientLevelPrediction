/************************************************************************
@file GetOutcomes.sql

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

{DEFAULT @cdm_database = 'CDM4_SIM' } 
{DEFAULT @outcome_database_schema = 'CDM4_SIM' } 
{DEFAULT @outcome_table = 'condition_occurrence' }
{DEFAULT @outcome_ids = '' }
{DEFAULT @outcome_condition_type_concept_ids = '' }
{DEFAULT @first_outcome_only = FALSE }
{DEFAULT @cdm_version == '4'}
{DEFAULT @cohort_definition_id = 'cohort_concept_id'} 

USE @cdm_database;

IF OBJECT_ID('tempdb..#cohort_outcome', 'U') IS NOT NULL
	DROP TABLE #cohort_outcome;
	
IF OBJECT_ID('tempdb..#cohort_excluded_person', 'U') IS NOT NULL
	DROP TABLE #cohort_excluded_person;

SELECT exposure.row_id,
	exposure.subject_id AS person_id,
	exposure.cohort_start_date,
	exposure.@cohort_definition_id AS @cohort_definition_id,
	outcome.outcome_id,
	COUNT(DISTINCT outcome_date) AS outcome_count,
	MIN(DATEDIFF(DAY, exposure.cohort_start_date, outcome_date)) AS time_to_event
INTO #cohort_outcome
FROM #cohort_person exposure
INNER JOIN (
{@first_outcome_only} ? {
{@outcome_table == 'condition_occurrence' } ? {
	SELECT condition_concept_id AS outcome_id,
		person_id,
		MIN(condition_start_date) AS outcome_date
	FROM condition_occurrence
	WHERE condition_concept_id IN (@outcome_ids)
	GROUP BY condition_concept_id,
		person_id
	  {@outcome_condition_type_concept_ids} ? {AND condition_type_concept_id IN (@outcome_condition_type_concept_ids}
} : { {@outcome_table == 'condition_era' } ? {
	SELECT condition_concept_id AS outcome_id,
	  person_id,
	  MIN(condition_era_start_date) AS outcome_date
	FROM condition_era
	WHERE condition_concept_id IN (@outcome_ids)
	GROUP BY condition_concept_id,
		person_id
} : {
	SELECT @cohort_definition_id AS outcome_id,
	  subject_id AS person_id,
	  MIN(cohort_start_date) AS outcome_date
	FROM @outcome_database_schema.@outcome_table co1
	WHERE @cohort_definition_id IN (@outcome_ids)
	GROUP BY @cohort_definition_id,
		subject_id
}}
} : {
{@outcome_table == 'condition_occurrence' } ? {
	SELECT condition_concept_id AS outcome_id,
	  person_id,
	  condition_start_date AS outcome_date
	FROM condition_occurrence
	WHERE condition_concept_id IN (@outcome_ids)
	  {@outcome_condition_type_concept_ids} ? {AND condition_type_concept_id IN (@outcome_condition_type_concept_ids}
} : { {@outcome_table == 'condition_era' } ? {
	SELECT condition_concept_id AS outcome_id,
	  person_id,
	  condition_era_start_date AS outcome_date
	FROM condition_era
	WHERE condition_concept_id IN (@outcome_ids)
} : {
	SELECT @cohort_definition_id AS outcome_id,
	  subject_id AS person_id,
	  cohort_start_date AS outcome_date
	FROM @outcome_database_schema.@outcome_table co1
	WHERE @cohort_definition_id IN (@outcome_ids)
}}
}
) outcome
ON outcome.person_id = exposure.subject_id
	AND outcome_date >= exposure.cohort_start_date
	AND outcome_date <= exposure.cohort_end_date
GROUP BY exposure.row_id,
	exposure.subject_id,
	exposure.cohort_start_date,
	exposure.@cohort_definition_id,
	outcome.outcome_id;


---find people to exclude from each analysis (if outcome occurs prior to index)	
{@first_outcome_only} ? {

{@outcome_table == 'condition_occurrence' } ? {
SELECT DISTINCT	cp1.row_id,
	cp1.subject_id AS person_id,
	cp1.cohort_start_date,
	cp1.@cohort_definition_id,
	ca1.ancestor_concept_id AS outcome_id
INTO #cohort_excluded_person
FROM #cohort_person cp1
INNER JOIN condition_occurrence co1
	ON cp1.subject_id = co1.person_id
INNER JOIN (
	SELECT descendant_concept_id,
		ancestor_concept_id
	FROM concept_ancestor
	WHERE ancestor_concept_id IN (@outcome_ids)
	) ca1
	ON co1.condition_concept_id = descendant_concept_id
WHERE {@outcome_condition_type_concept_ids != '' } ? { co1.condition_type_concept_id IN (@outcome_condition_type_concept_ids)
	AND } co1.condition_start_date < cp1.cohort_start_date;
} : { {@outcome_table == 'condition_era' } ? {

SELECT DISTINCT cp1.row_id,
	cp1.subject_id AS person_id,
	cp1.cohort_start_date,
	cp1.@cohort_definition_id,
	ca1.ancestor_concept_id AS outcome_id
INTO #cohort_excluded_person
FROM #cohort_person cp1
INNER JOIN condition_era co1
	ON cp1.subject_id = co1.person_id
INNER JOIN (
	SELECT descendant_concept_id,
		ancestor_concept_id
	FROM concept_ancestor
	WHERE ancestor_concept_id IN (@outcome_ids)
	) ca1
	ON co1.condition_concept_id = descendant_concept_id
WHERE {@outcome_condition_type_concept_ids != '' } ? { co1.condition_type_concept_id IN (@outcome_condition_type_concept_ids)
	AND } co1.condition_era_start_date < cp1.cohort_start_date;

} : {

SELECT DISTINCT cp1.row_id,
	cp1.subject_id AS person_id,
	cp1.cohort_start_date,
	cp1.@cohort_definition_id,
	co1.@cohort_definition_id AS outcome_id
INTO #cohort_excluded_person
FROM #cohort_person cp1
INNER JOIN @outcome_database_schema.@outcome_table co1
	ON cp1.subject_id = co1.subject_id
WHERE co1.@cohort_definition_id IN (@outcome_ids)
	AND co1.cohort_start_date < cp1.cohort_start_date; 
} }
}