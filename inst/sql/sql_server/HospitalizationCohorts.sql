/***********************************
  File HospitalizationCohorts.sql 
***********************************/
{DEFAULT @cdm_version = '5'}


  IF OBJECT_ID('@resultsDatabaseSchema.rehospitalization', 'U') IS NOT NULL
DROP TABLE @resultsDatabaseSchema.rehospitalization;


{@cdm_version == "6"} ? {	
SELECT visit_occurrence.person_id AS subject_id,
MIN(visit_start_datetime) AS cohort_start_date,
DATEADD(DAY, @post_time, MIN(visit_start_datetime)) AS cohort_end_date,
1 AS cohort_definition_id
INTO @resultsDatabaseSchema.rehospitalization
FROM @cdmDatabaseSchema.visit_occurrence
INNER JOIN @cdmDatabaseSchema.observation_period
ON visit_occurrence.person_id = observation_period.person_id
INNER JOIN @cdmDatabaseSchema.condition_occurrence
ON condition_occurrence.person_id = visit_occurrence.person_id 
WHERE visit_concept_id IN (9201, 9203)
AND DATEDIFF(DAY, observation_period_start_date, visit_start_datetime) > @pre_time
AND visit_start_datetime > observation_period_start_date
AND DATEDIFF(DAY, visit_start_datetime, observation_period_end_date) > @post_time
AND visit_start_datetime < observation_period_end_date
AND DATEDIFF(DAY, condition_start_datetime, visit_start_datetime) > @pre_time
AND condition_start_datetime <= visit_start_datetime
AND condition_concept_id IN (
  SELECT descendant_concept_id 
  FROM @cdmDatabaseSchema.concept_ancestor 
  WHERE ancestor_concept_id = 201826) /* Type 2 DM */
  GROUP BY visit_occurrence.person_id;

INSERT INTO @resultsDatabaseSchema.rehospitalization
SELECT visit_occurrence.person_id AS subject_id,
visit_start_datetime AS cohort_start_date,
visit_end_datetime AS cohort_end_date,
2 AS cohort_definition_id
FROM @resultsDatabaseSchema.rehospitalization
INNER JOIN @cdmDatabaseSchema.visit_occurrence
ON visit_occurrence.person_id = rehospitalization.subject_id
WHERE visit_concept_id IN (9201, 9203)
AND visit_start_datetime > cohort_start_date
AND visit_start_datetime <= cohort_end_date
AND cohort_definition_id = 1; } : {

SELECT visit_occurrence.person_id AS subject_id,
MIN(visit_start_date) AS cohort_start_date,
DATEADD(DAY, @post_time, MIN(visit_start_date)) AS cohort_end_date,
1 AS cohort_definition_id
INTO @resultsDatabaseSchema.rehospitalization
FROM @cdmDatabaseSchema.visit_occurrence
INNER JOIN @cdmDatabaseSchema.observation_period
ON visit_occurrence.person_id = observation_period.person_id
INNER JOIN @cdmDatabaseSchema.condition_occurrence
ON condition_occurrence.person_id = visit_occurrence.person_id 
WHERE visit_concept_id IN (9201, 9203)
AND DATEDIFF(DAY, observation_period_start_date, visit_start_date) > @pre_time
AND visit_start_date > observation_period_start_date
AND DATEDIFF(DAY, visit_start_date, observation_period_end_date) > @post_time
AND visit_start_date < observation_period_end_date
AND DATEDIFF(DAY, condition_start_date, visit_start_date) > @pre_time
AND condition_start_date <= visit_start_date
AND condition_concept_id IN (
  SELECT descendant_concept_id 
  FROM @cdmDatabaseSchema.concept_ancestor 
  WHERE ancestor_concept_id = 201826) /* Type 2 DM */
  GROUP BY visit_occurrence.person_id;

INSERT INTO @resultsDatabaseSchema.rehospitalization
SELECT visit_occurrence.person_id AS subject_id,
visit_start_date AS cohort_start_date,
visit_end_date AS cohort_end_date,
2 AS cohort_definition_id
FROM @resultsDatabaseSchema.rehospitalization
INNER JOIN @cdmDatabaseSchema.visit_occurrence
ON visit_occurrence.person_id = rehospitalization.subject_id
WHERE visit_concept_id IN (9201, 9203)
AND visit_start_date > cohort_start_date
AND visit_start_date <= cohort_end_date
AND cohort_definition_id = 1;

}
