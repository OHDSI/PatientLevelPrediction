TRUNCATE TABLE #cov;

DROP TABLE #cov;

TRUNCATE TABLE #cov_ref;

DROP TABLE #cov_ref;

{@has_excluded_covariate_concept_ids} ? {
TRUNCATE TABLE #excluded_cov;

DROP TABLE #excluded_cov;
}

{@has_included_covariate_concept_ids} ? {
TRUNCATE TABLE #included_cov;

DROP TABLE #included_cov;
}

