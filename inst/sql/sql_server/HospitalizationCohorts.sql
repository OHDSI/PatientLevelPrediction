/***********************************
File HospitalizationCohort.sql 
***********************************/
IF OBJECT_ID('@resultsDatabaseSchema.first_hospitalization', 'U') IS NOT NULL
	DROP TABLE @resultsDatabaseSchema.first_hospitalization;

IF OBJECT_ID('@resultsDatabaseSchema.rehospitalization', 'U') IS NOT NULL
	DROP TABLE @resultsDatabaseSchema.rehospitalization;
	
SELECT visit_occurrence.person_id AS subject_id,
	MIN(visit_start_date) AS cohort_start_date,
	DATEADD(DAY, @post_time, MIN(visit_start_date)) AS cohort_end_date,
	1 AS cohort_concept_id
INTO @resultsDatabaseSchema.first_hospitalization
FROM visit_occurrence
INNER JOIN observation_period
	ON visit_occurrence.person_id = observation_period.person_id
WHERE place_of_service_concept_id IN (9201, 9203)
	AND DATEDIFF(DAY, observation_period_start_date, visit_start_date) > @pre_time
	AND visit_start_date > observation_period_start_date
	AND DATEDIFF(DAY, visit_start_date, observation_period_end_date) > @post_time
	AND visit_start_date < observation_period_end_date
GROUP BY visit_occurrence.person_id;

SELECT visit_occurrence.person_id AS subject_id,
	visit_start_date AS cohort_start_date,
	visit_end_date AS cohort_end_date,
	1 AS cohort_concept_id
INTO @resultsDatabaseSchema.rehospitalization
FROM @resultsDatabaseSchema.first_hospitalization
INNER JOIN visit_occurrence
	ON visit_occurrence.person_id = first_hospitalization.subject_id
WHERE place_of_service_concept_id IN (9201, 9203)
	AND visit_start_date > cohort_start_date
	AND visit_start_date <= cohort_end_date;
