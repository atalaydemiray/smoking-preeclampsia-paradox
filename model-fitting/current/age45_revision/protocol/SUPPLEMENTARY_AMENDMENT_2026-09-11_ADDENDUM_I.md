# Addendum I, 19 September 2026 (about 01:45 CEST): diagnostic decomposition of the flagged SS vs SN movement. Not a reported result.

Parent: `SUPPLEMENTARY_AMENDMENT_2026-09-11.md`; addendum H (joint three-group model and its
pre-specified validation). Written and dated before the one diagnostic fit it describes.

## Why

`33b_derive_ss_vs_sn.R` (01:20) compared the joint model's SS vs SN contrast with the released within-group
fit `primary_main` and fired the addendum H flag on the crossover: 33.30 years (local 95% CI 32.08 to
34.52) against 31.84 (30.97 to 32.72), a movement of 1.46 years over the 1.0-year threshold. Overall RR
(0.9408 vs 0.9443) and RD (-5.28 vs -4.94 per 1,000) are within their thresholds and the reversal is
established in both. Addendum H requires the discrepancy to be investigated before either design is used.

Two designs differ in exactly two ways: (1) `primary_main` adjusts for prepregnancy cigarettes per day
(`prepreg_dose5`), which the joint model cannot carry because dose is zero for every NN record and the
indicator sum of its levels would be collinear with the SN and SS indicators; (2) `primary_main` estimates
the covariate function among women who smoked before pregnancy only (1,980,559 records), whereas the joint
model estimates one covariate function for all 30,076,524 records, 93 percent of them NN. Reading the two
fitted models shows the dose coefficients are at most 0.022 in absolute value, and the shared covariate
coefficients differ by 0.1 to 0.3 (race and ethnicity, BMI spline, prior preterm birth, prepregnancy
diabetes). The conditional log-odds contrast h_SS(a) - h_SN(a) is lower in the joint model by 0.009 at age
15 to 0.034 at age 40; because it crosses zero with a slope near 0.02 per year, that offset moves the root
by about 1.5 years. The standardization population plays no part (the root of the conditional contrast
does not depend on it; addendum H).

## The one diagnostic fit

To separate (1) from (2) with numbers rather than reasoning: one streaming logistic fit of SS vs SN on the
released within-group population (the SS and SN complete cases of the extended prepregnancy inputs, which
are exactly the 1,480,543 SS and 500,016 SN records of `primary_main` and of the joint model) with the
nine natality-main covariates and NO prepregnancy dose, standardized to that population like
`primary_main`. Its crossover differs from `primary_main` only through (1) and from the joint model only
through (2). Script `age45_revision/scripts/36_diagnostic_within_s_no_dose.R`; outputs under
`age45_revision/outputs/supplementary/joint_3group/diagnostics/within_s_no_dose/` with a receipt and a
`decomposition.csv`.

**Status of the numbers.** Diagnostic only. They enter the verification log and the owner's decision
memo; they do not enter any table, figure or sentence of the manuscript or supplement. No released result
is touched; `21_fit_supplementary.R` and `33_fit_joint_three_group.R` are not edited.
