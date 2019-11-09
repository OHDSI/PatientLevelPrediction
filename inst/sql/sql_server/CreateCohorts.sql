/************************************************************************
@file GetCohorts.sql
Copyright 2019 Observational Health Data Sciences and Informatics
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
{DEFAULT @cohort_database_schema = 'CDM_SIM' }
{DEFAULT @cohort_table = 'drug_era' }
{DEFAULT @cdm_version = '5'}
{DEFAULT @cohort_id = '' }
{DEFAULT @study_start_date = '' }
{DEFAULT @study_end_date = '' }
{DEFAULT @first_only = FALSE}
{DEFAULT @washout_period = 0}
{DEFAULT @use_sample = FALSE}
{DEFAULT @sample_number = 100000}

IF OBJECT_ID('tempdb..#cohort_person', 'U') IS NOT NULL
	DROP TABLE #cohort_person;
	
SELECT ROW_NUMBER() OVER (ORDER BY person_id, cohort_start_date) AS row_id,
	subject_id,
{@cdm_version == "4"} ? {	
	cohort_definition_id AS cohort_concept_id,
} : {
	cohort_definition_id,
}
	cohort_start_date, cohort_end_date,
	DATEDIFF(DAY, observation_period_start_date, cohort_start_date) AS days_from_obs_start,
	{@study_end_date != '' } ? { 
	    CASE 
			WHEN cohort_end_date <= CAST('@study_end_date' AS DATE)
				THEN DATEDIFF(DAY, cohort_start_date, cohort_end_date) 
			ELSE 
				DATEDIFF(DAY, cohort_start_date, CAST('@study_end_date' AS DATE))
		END 
	} : {
		DATEDIFF(DAY, cohort_start_date, cohort_end_date)
	} AS days_to_cohort_end,
	{@study_end_date != '' } ? { 
	    CASE 
			WHEN observation_period_end_date <= CAST('@study_end_date' AS DATE)
				THEN DATEDIFF(DAY, cohort_start_date, observation_period_end_date) 
			ELSE 
				DATEDIFF(DAY, cohort_start_date, CAST('@study_end_date' AS DATE))
		END 
	} : {
		DATEDIFF(DAY, cohort_start_date, observation_period_end_date) 
	} AS days_to_obs_end
INTO #cohort_person

{@use_sample}?{From ( select * 
                from ( select *, 
                row_number() over (order by (CAST(person_id*month(cohort_start_date) AS BIGINT) % 123)*(CAST(year(cohort_start_date)*day(cohort_start_date) AS BIGINT) % 123)) rn
  } 

FROM (
{@first_only} ? {
	SELECT subject_id,
		cohort_definition_id,
		MIN(cohort_start_date) AS cohort_start_date,
		MIN(cohort_end_date) AS cohort_end_date
	FROM (
}
{@cohort_table == 'drug_era' } ? { 


{@cdm_version == '6' } ? { 
	SELECT person_id AS subject_id,
@cohort_id AS cohort_definition_id,
		drug_era_start_datetime AS cohort_start_date,
		drug_era_end_datetime AS cohort_end_date
	FROM  @cohort_database_schema.@cohort_table exposure_table
	WHERE drug_concept_id IN (@cohort_id)} : {
	SELECT person_id AS subject_id,
@cohort_id AS cohort_definition_id,
		drug_era_start_date AS cohort_start_date,
		drug_era_end_date AS cohort_end_date
	FROM  @cohort_database_schema.@cohort_table exposure_table
	WHERE drug_concept_id IN (@cohort_id)
	}
	
	
} : {
	SELECT subject_id,
			@cohort_id AS cohort_definition_id,
		cohort_start_date,
		cohort_end_date
	FROM @cohort_database_schema.@cohort_table cohort_table
{@cdm_version == "4"} ? {	
	WHERE cohort_concept_id IN (@cohort_id)
} : {
	WHERE cohort_definition_id IN (@cohort_id)
}
}
	) raw_cohorts
{@first_only} ? {
  GROUP BY subject_id,
	cohort_definition_id
	) first_only
}
INNER JOIN @cdm_database_schema.observation_period
	ON subject_id = person_id
WHERE cohort_start_date <= observation_period_end_date
	AND cohort_start_date >= observation_period_start_date
{@study_start_date != '' } ? {AND cohort_start_date >= CAST('@study_start_date' AS DATE) } 
{@study_end_date != '' } ? {AND cohort_start_date < CAST('@study_end_date' AS DATE) }
{@washout_period != 0} ? {AND DATEDIFF(DAY, observation_period_start_date, cohort_start_date) >= @washout_period}

{@use_sample}?{) alldata where rn <= @sample_number) sampdata } 
;
