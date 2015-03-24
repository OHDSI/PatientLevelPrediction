{DEFAULT @use_existing_cohort_person = TRUE } 

{!@use_existing_cohort_person} ? {
TRUNCATE TABLE #cohort_person;

DROP TABLE #cohort_person;
}