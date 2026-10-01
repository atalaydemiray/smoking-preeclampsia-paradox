# Addendum J, 19 September 2026: the first-trimester-marginal model (T1 smoking versus none in T1, irrespective of before-pregnancy smoking)

Parent: `SUPPLEMENTARY_AMENDMENT_2026-09-11.md`; addenda G (three-group presentation and exposure codes),
H (the joint model and its pre-specified validation) and I (diagnostic decomposition). **Written and dated
before the fit it authorises; the fit's receipt records its start time.**

## What prompted this

The manuscript reports one joint model of the two early windows (SS versus SN and SS versus NN) and one
window-marginal model, before pregnancy (S versus N, irrespective of the first trimester). The owner asked
on 19 September for the remaining cell of that grid: the first-trimester-marginal model, smoking in the
first trimester versus none in the first trimester, irrespective of before-pregnancy smoking. Reporting the
before-pregnancy margin without the first-trimester margin is an asymmetry a reviewer may query, and the
first-trimester margin is the more clinically immediate question (whether a woman is smoking now).

## The model

One streaming logistic fit, the same engine, age basis, covariates, covariance and standardization as every
other model in the paper:

    logit P(Y = 1 | A, a, X) = g(a, X) + A h(a),   A = 1 if any cigarettes reported in the first trimester

`g(a, X)` is the locked natural cubic spline of age (knots 21, 27, 35; boundaries 15, 45) plus the nine
natality-main covariates (year, race and ethnicity, prepregnancy diabetes, prior living children, education,
BMI, prior preterm birth, prior cesarean delivery, nativity). `h(a)` is the smoking-by-age spline on the same
basis. HC0 covariance, chunk size 25,000, age-specific empirical standardization over the model's own
complete cases, crossover by exact root isolation with a local implicit-delta 95% CI, whole 95% null-age set
and simultaneous sign regions, exactly as for `prepregnancy_main`.

**By symmetry with the before-pregnancy model, before-pregnancy smoking is not adjusted for.** The
before-pregnancy model does not adjust for the first trimester; adjusting here would make the two margins
non-comparable and would condition on the other window rather than marginalise over it.

**Population.** All eligible complete cases whose first-trimester cigarette field is known. Records with an
unknown first-trimester value are excluded, because the exposure would be undefined; that is 4,370 complete
cases. The resulting population is the union SS + NS + SN + NN. This differs from the before-pregnancy
population (30,097,165), which retains records with an unknown first-trimester value, so the flow table and
Figure 1 gain a row rather than reusing an existing count.

**Exposed and reference groups, counted before the fit from the receipted inputs:**

| Arm | Composition | Records | Recorded GH/PE | Crude per 1,000 |
|---|---|---|---|---|
| Exposed, T1 positive | SS 1,480,543 and NS 16,271 | 1,496,814 | 121,542 | 81.20 |
| Reference, T1 negative | SN 500,016 and NN 28,095,965 | 28,595,981 | 2,376,159 | 83.09 |

`NS` (no before-pregnancy smoking, smoking recorded in the first trimester) is 1.09 percent of the exposed
arm; `SN` is 1.75 percent of the reference arm.

## Pre-specified expectation, recorded before the fit

Because the exposed arm is 98.9 percent SS and the reference arm is 98.25 percent NN, this model is expected
to lie close to SS versus NN rather than to provide independent evidence. The crude risk ratios are 0.977
here against 0.980 for SS versus NN. **Expected: an adjusted risk ratio within about 0.02 of the SS versus NN
value and a crossover within about 1 year of 29.4.** A larger divergence would mean the two arms' minority
groups (NS, SN) carry more weight than their size suggests and must be reported and investigated, not
smoothed over. This is a comparison for interpretation, not an acceptance gate: the model estimates a
different estimand from SS versus NN and is not required to agree with it.

**The reference arm pools SN with NN**, two groups the three-group model exists to separate and whose crude
risks differ (97.3 against 82.8 per 1,000). Any presentation of this model must say so, because it is the
one respect in which a marginal first-trimester contrast is weaker than the joint model.

## Placement

**Owner's ruling, 19 September 2026, taken while the fit was running and before any number was seen:
supplementary, with a brief mention in the main text.** The model therefore takes:

- **Table S4e**, its age-specific associations, extending the S4 series that already holds SS vs SN (S4a),
  SS vs NN (S4b), S vs N (S4c) and the within-group SS vs SN sensitivity fit (S4d). The supplement stays at
  22 numbered tables; S4 gains a part, as S6 has parts a and b.
- **A row in Table S5**, the crossover table that lists every GH/PE specification, and **a row in Figure 3**,
  the crossover forest.
- **One sentence in Results**, reporting the contrast, its crossover and its relation to SS vs NN, and
  stating that its reference arm pools SN with NN.
- **A paragraph in Supplementary Methods**, section 3, defining the margin and why before-pregnancy smoking
  is not adjusted for.

Main Tables 1 to 3, Figure 1 and Figure 2 and the abstract are unchanged: the main analysis remains the joint
model's two contrasts plus the before-pregnancy margin. Because the main text is at its 3,000-word cap, the
added sentence is offset by an equivalent cut recorded in the verification log.

## Outputs

`age45_revision/outputs/supplementary/t1_only/` with `model.rds`, the receipt (columnwise SHA-256 of the fit
data and identity keys, formula, knots, code manifest), the engine's standard file set
(`HC0_age_standardized.csv`, `HC0_overall_standardized.csv`, `HC0_crossover.rds`, `HC0_roots.csv`, the two
null-age sets, the model-based equivalents) and `age_arm_support.csv`, plus
`comparison_with_ss_vs_nn.csv`.

Script: `age45_revision/scripts/38_fit_t1_only.R`, one fit per R process. No existing script is edited; no
released result is touched; complete cases only, no imputation.
