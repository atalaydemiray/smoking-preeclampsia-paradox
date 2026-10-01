# Age-by-smoking omnibus interaction tests, 1 October 2026

Written before the fits below. The owner approved execution of Sarah's interaction-test and reporting revisions on 1 October 2026. This amendment adds tests and reporting summaries without replacing any released full model, cohort, exposure definition, covariate, spline basis, standardized estimate, or crossover result.

## Models and hypotheses

Use the released joint binary logistic GH/PE model in `outputs/supplementary/joint_3group` (SS, SN, NN) and the released binary prepregnancy model in `outputs/models/prepregnancy_main` (S, N), each for 2016–2024, ages 15–45. Use exactly the corresponding complete-case records, fixed age and BMI bases, factor levels, and adjustment set. No imputation or outcome-dependent knot selection. The joint model remains binary logistic, not multinomial logistic.

For each full model, construct a nested reduced model by removing only all smoking-by-age spline interactions while retaining smoking main effects, all four age basis columns, and every adjustment term. The global null is that all smoking-by-age interaction coefficients equal zero: 8 degrees of freedom in the three-group model and 4 in the binary model. Record and verify these ranks from the fitted designs. Compute the unweighted binomial likelihood-ratio statistic as twice the full-minus-reduced log likelihood and compare with the corresponding chi-squared distribution. These are model-based tests under the working Bernoulli likelihood, not robust likelihood-ratio tests.

Also compute HC0 sandwich-covariance omnibus Wald tests of exactly the same coefficient blocks, and exploratory four-degree-of-freedom contrast-specific Wald tests for SN versus SS, NN versus SS, and SN versus NN. Reference reversal cannot change these test statistics. The example paper by Hrywna, Bover Manderski, and Delnevo used survey-design-adjusted Wald tests; its weighted survey design is not transferred to these vital-record cohorts. The requested nested likelihood comparison is additional to, not attributed to, that paper.

Interaction tests concern variation on the model log-odds scale. They are not tests of the crossover location, reversal itself, or equality of standardized risk ratios/risk differences. Retain the existing standardized relative/absolute risks, local root intervals, and simultaneous sign inference to support those separate questions. Do not use significance as a gate to select or discard the full interaction model. Report p<0.001 for very small p values; preserve finite log-tail probabilities in machine-readable output rather than writing an underflowed zero as evidence.

## Provenance, validation, and bounded execution

Run only new `scripts/40_*.R` code, writing to `outputs/supplementary/interaction_revision_20261001/`. Verify frozen code, model, annual input, and receipt hashes before reuse. Reconstruct the original full-model input signature, then independently reevaluate its likelihood, score, model covariance, and HC0 covariance on all original records. Do not refit the full model unless a verified discrepancy requires a new, separately recorded correction.

Before the reduced national fit, compare full and reduced streaming fits against ordinary `glm` on a deterministic, evenly spaced 120,000-record subset of that same cohort. Compare coefficients, deviance, the LRT, and HC0 Wald statistics using independently accumulated model matrices. On the national records verify nested model columns, fitted ranks, nonnegative likelihood improvement, convergence, and final score. Save the reduced model, fit trace, validation results, test tables, exact commands, session information, and SHA-256 receipt. Use one heavy R process at a time, primary then secondary, with separate processes to release memory.

## Missingness and population summaries

On each model's original exposure-eligible population, count unknown values for each required adjustment field separately, using the same eligible denominator for every field. These counts overlap and are not sequential exclusions or additive. Report eligible N, at least one unknown adjustment field N, and final complete-case N explicitly. The joint population first requires membership in SS, SN, or NN; the binary population allows known prepregnancy status regardless of T1. They therefore have different valid denominators. Derive group-level N, event counts, and descriptive summaries from the exact complete-case fit records for manuscript tables.

No original manuscript, released result, old package, or source dataset will be overwritten. The final publication package will identify this as an additional analysis developed after inspection of the original results.
