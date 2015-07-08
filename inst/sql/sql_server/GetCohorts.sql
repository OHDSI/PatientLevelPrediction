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
{DEFAULT @use_existing_cohort_person = TRUE } 
{DEFAULT @cohort_database_schema = 'CDM4_SIM' } 
{DEFAULT @cohort_table = 'cohort' } 
{DEFAULT @cohort_ids = '0,1' }
{DEFAULT @cdm_version == '4'}
{DEFAULT @cohort_definition_id = 'cohort_concept_id'} 

USE @cdm_database;

{!@use_existing_cohort_person} ? {
IF OBJECT_ID('tempdb..#cohort_person', 'U') IS NOT NULL
	DROP TABLE #cohort_person;
	
SELECT @cohort_definition_id,
	subject_id,
	cohort_start_date,
	cohort_end_date
INTO #cohort_person
FROM @cohort_database_schema.@cohort_table
{@cohort_concept_ids != ''} ? {
WHERE @cohort_definition_id IN (@cohort_ids);
}
}
