{DEFAULT @first_outcome_only = FALSE }

TRUNCATE TABLE #cohort_outcome;

DROP TABLE #cohort_outcome;

{@first_outcome_only} ? {
TRUNCATE TABLE #cohort_excluded_person;

DROP TABLE #cohort_excluded_person;
}
