{DEFAULT @cdm_version = '5'}

SELECT row_id,
	subject_id,
{@cdm_version == "4"} ? {	
	cohort_concept_id AS cohort_id,
} : {
	cohort_definition_id AS cohort_id,
}
	cohort_start_date,
	days_from_obs_start,
	days_to_cohort_end,
	days_to_obs_end
FROM #cohort_person cohort
ORDER BY subject_id
