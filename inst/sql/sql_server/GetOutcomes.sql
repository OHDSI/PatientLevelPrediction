/************************************************************************
@file GetOutcomes.sql
Copyright 2021 Observational Health Data Sciences and Informatics
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

{DEFAULT @cdm_database_schema = 'CDM_SIM' }
{DEFAULT @outcome_database_schema = 'CDM_SIM' } 
{DEFAULT @outcome_table = 'condition_era' }
{DEFAULT @outcome_ids = ''} 
{DEFAULT @cdm_version = '5'}

{@outcome_table == 'condition_era' } ? {

{@cdm_version == '6' } ? {
SELECT DISTINCT cast(row_id as int) row_id,
	ancestor_concept_id AS outcome_id,
	DATEDIFF(DAY, cohort_start_date, condition_era_start_datetime) AS days_to_event
FROM #cohort_person cohort_person
INNER JOIN @cdm_database_schema.condition_era
	ON subject_id = person_id
INNER JOIN (
	SELECT descendant_concept_id,
		ancestor_concept_id
	FROM @cdm_database_schema.concept_ancestor
	WHERE ancestor_concept_id IN (@outcome_ids)
	) concept_ancestor
	ON condition_concept_id = descendant_concept_id
WHERE DATEDIFF(DAY, condition_era_start_datetime, cohort_start_date) <= days_from_obs_start
	AND DATEDIFF(DAY, cohort_start_date, condition_era_start_datetime) <= days_to_obs_end} : {
	
	SELECT DISTINCT cast(row_id as int) row_id,
	ancestor_concept_id AS outcome_id,
	DATEDIFF(DAY, cohort_start_date, condition_era_start_date) AS days_to_event
FROM #cohort_person cohort_person
INNER JOIN @cdm_database_schema.condition_era
	ON subject_id = person_id
INNER JOIN (
	SELECT descendant_concept_id,
		ancestor_concept_id
	FROM @cdm_database_schema.concept_ancestor
	WHERE ancestor_concept_id IN (@outcome_ids)
	) concept_ancestor
	ON condition_concept_id = descendant_concept_id
WHERE DATEDIFF(DAY, condition_era_start_date, cohort_start_date) <= days_from_obs_start
	AND DATEDIFF(DAY, cohort_start_date, condition_era_start_date) <= days_to_obs_end
	
	}
	
	
	
} : {
SELECT DISTINCT cast(row_id as int) row_id,
{@cdm_version == "4"} ? {	
	outcome.cohort_concept_id AS outcome_id,
} : {
	outcome.cohort_definition_id AS outcome_id,
}	
	DATEDIFF(DAY, cohort_person.cohort_start_date, outcome.cohort_start_date) AS days_to_event
FROM #cohort_person cohort_person
INNER JOIN @outcome_database_schema.@outcome_table outcome
	ON cohort_person.subject_id = outcome.subject_id
WHERE DATEDIFF(DAY, outcome.cohort_start_date, cohort_person.cohort_start_date) <= days_from_obs_start
	AND DATEDIFF(DAY, cohort_person.cohort_start_date, outcome.cohort_start_date) <= days_to_obs_end
{@cdm_version == "4"} ? {	
	AND outcome.cohort_concept_id IN (@outcome_ids)
} : {
	AND outcome.cohort_definition_id IN (@outcome_ids)
}		
}
