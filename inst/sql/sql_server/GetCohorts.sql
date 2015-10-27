/************************************************************************
@file GetCohorts.sql

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
{DEFAULT @cohort_database_schema = 'CDM4_SIM' } 
{DEFAULT @cohort_table = 'cohort' } 
{DEFAULT @cohort_ids = '0,1' }
{DEFAULT @washout_window = 183 }
{DEFAULT @cdm_version == '4'}
{DEFAULT @cohort_definition_id = 'cohort_concept_id'} 
{DEFAULT @use_cohort_end_date = TRUE}
{DEFAULT @window_persistence = 0}

USE @cdm_database;

IF OBJECT_ID('tempdb..#cohort_person', 'U') IS NOT NULL
	DROP TABLE #cohort_person;
	
SELECT ROW_NUMBER() OVER (ORDER BY subject_id, cohort_start_date) AS row_id,
  	@cohort_definition_id,
	subject_id,
	cohort_start_date,
	cohort_end_date
INTO #cohort_person
FROM (
	SELECT DISTINCT @cohort_definition_id,
		subject_id,
		cohort_start_date,
{@use_cohort_end_date} ? {
		CASE 
			WHEN DATEADD(DAY, @window_persistence, cohort_end_date) > observation_period_end_date THEN observation_period_end_date 
			ELSE DATEADD(DAY, @window_persistence, cohort_end_date)
		END AS cohort_end_date
} : {
		CASE 
			WHEN DATEADD(DAY, @window_persistence, cohort_start_date) > observation_period_end_date THEN observation_period_end_date 
			ELSE DATEADD(DAY, @window_persistence, cohort_start_date)
		END AS cohort_end_date
}	
	FROM @cohort_database_schema.@cohort_table cohort
	INNER JOIN observation_period
	ON cohort.subject_id = observation_period.person_id
	WHERE cohort_start_date >= observation_period_start_date 
	AND cohort_start_date <= observation_period_end_date 
{@cohort_concept_ids != ''} ? {
	AND @cohort_definition_id IN (@cohort_ids)
} 
	AND cohort_start_date >= DATEADD(DAY, @washout_window, observation_period_start_date) 
) unique_periods
;
