/***********************************
File LengthOfObsCohortAttr.sql 
***********************************/
IF OBJECT_ID('@cohort_database_schema.@cohort_attribute_table', 'U') IS NOT NULL
	DROP TABLE @cohort_database_schema.@cohort_attribute_table;
	
IF OBJECT_ID('@cohort_database_schema.@attribute_definition_table', 'U') IS NOT NULL
	DROP TABLE @cohort_database_schema.@attribute_definition_table;
	
SELECT cohort_definition_id,
    subject_id,
	cohort_start_date,
	1 AS attribute_definition_id,
	DATEDIFF(DAY, observation_period_start_date, cohort_start_date) AS value_as_number
INTO @cohort_database_schema.@cohort_attribute_table
FROM @cohort_database_schema.@cohort_table cohort
INNER JOIN @cdm_database_schema.observation_period op
	ON op.person_id = cohort.subject_id
WHERE cohort_start_date >= observation_period_start_date
	AND cohort_start_date <= observation_period_end_date
{@cohort_definition_ids != ''} ? {
	AND cohort_definition_id IN (@cohort_definition_ids)
}
;
	
SELECT 1 AS attribute_definition_id,
  'Length of observation in days' AS attribute_name
INTO @cohort_database_schema.@attribute_definition_table;


	
