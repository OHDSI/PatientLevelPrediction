{DEFAULT @use_existing_cohort_person = TRUE } 

TRUNCATE TABLE #cohort_covariate;

DROP TABLE #cohort_covariate;

TRUNCATE TABLE #cohort_covariate_ref;

DROP TABLE #cohort_covariate_ref;

{!@use_existing_cohort_person} ? {
TRUNCATE TABLE #cohort_person;

DROP TABLE #cohort_person;
}
