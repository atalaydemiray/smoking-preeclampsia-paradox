# Addendum H, 19 September 2026: one joint three-group model (SS, SN, NN) as the source of the main three-group results; the common-reference pair becomes its confirmatory check

Parent: `SUPPLEMENTARY_AMENDMENT_2026-09-11.md`; addendum G (19 September 2026, presentation as a
three-group comparison and a before-pregnancy comparison). This addendum is written and dated before
the fit it authorises; the fit's receipt records its start time.

## What prompted this

Addendum G sources the three-group main results from two two-arm fits that share the NN records and
one standardization population (`supp_3lvl_continued_vs_none`, `supp_3lvl_stopped_vs_none`). That
design estimates the NN reference curve twice (the two estimates differ by at most 0.046 per 1,000).
The owner asked for the most rigorous design. One joint model gives one NN curve and one covariance for
all three groups; the already-verified pair then serves as an independent numerical check of the new
fit rather than as the reported result.

## The model

One streaming logistic fit on the pooled SS, SN and NN complete cases, the same 30,076,524 records
that form the common reference of the existing pair (complete on the nine natality-main covariates;
records with no smoking before pregnancy but smoking in the first trimester, or with an unknown
first-trimester field, are excluded exactly as before):

    logit P(Y = 1 | G, a, X) = g(a, X) + 1[G = SS] h_SS(a) + 1[G = SN] h_SN(a)

`g(a, X)` is the locked natural cubic spline of age (knots 21, 27, 35; boundaries 15, 45) plus the
nine covariates of the natality-main tier (year, race and ethnicity, prepregnancy diabetes, prior
living children, education, BMI, prior preterm birth, prior cesarean, nativity). `h_SS` and `h_SN`
are natural cubic splines of age on the same locked basis, one for each smoking group, with NN the
reference for both. No smoking-by-covariate interaction is included, as in every other model.

**Implementation.** The validated fitting engine (`fit_streaming_logit`, `R/11_streaming_standardization.R`)
is not modified. `A` is 1 for SS and 0 otherwise, as the engine requires; the SN block enters as the
formula terms `SN + SN:splines::ns(age, knots, Boundary.knots, intercept = FALSE)`, which the engine's
formula inspector accepts. HC0 covariance, chunk size 25,000, the same convergence tolerances.
Script: `age45_revision/scripts/33_fit_joint_three_group.R`, one fit per R process.
`21_fit_supplementary.R` is not edited (its hash is in every supplementary receipt).

**Estimands.** At each age 15 to 45, the standardized risks R_NN(a), R_SN(a), R_SS(a) over the
covariate distribution of the pooled population at that age; the adjusted RR and RD of SN vs NN and
of SS vs NN; overall standardized risks, RR and RD; and for each contrast the crossover age with its
local implicit-delta 95% CI, the whole 95% null-age set and the simultaneous-band sign regions, from
`analyze_age_crossover` applied to that contrast's coefficient block.

**Standardization.** SS vs NN: the engine's `standardize_streaming_logit` with the target's SN column
fixed at 0, so toggling `A` gives the NN and SS patterns. SN vs NN: a two-pattern g-computation that
reuses the engine's design builder and delta-method exactly and toggles the SN column with `A` fixed
at 0. **Self-check:** the two-pattern routine, asked for SS vs NN, must reproduce the engine's output
to machine precision before its SN vs NN output is written (`stopifnot`).

## Estimands revised by the owner at about 01:00 on 19 September 2026, before any post-processing ran

The fit above is unchanged (it was running when the clarification arrived and estimates all three group
curves regardless). **The contrasts presented are SS vs SN and SS vs NN**: SS, women who smoked before
pregnancy and in the first trimester, is the exposed group of interest, compared with the women who
stopped (SN) and with the women who reported no early smoking (NN). SN vs NN is not presented anywhere;
it is computed only as part of the numerical check against the two-arm pair.

SS vs SN from the joint model: conditional contrast h_SS(a) − h_SN(a), a spline on the same locked
basis, so the crossover engine is applied to the difference of the two coefficient blocks with covariance
V_AA + V_SS − V_AS − V_SA; standardized risks by the two-pattern routine with SN and SS as the two
patterns over the pooled population. Script: `age45_revision/scripts/33b_derive_ss_vs_sn.R`, which reads
`model.rds`, rebuilds the identical population (its input signature must equal the fit receipt's) and
refits nothing.

**Expected movement of SS vs SN relative to the released comparison 1 (`primary_main`, 31.8 years) is
larger than for SS vs NN**, for two reasons that are recorded here so that they are not mistaken for an
error: (1) the joint model standardizes to the pooled SS, SN and NN population, whereas `primary_main`
standardizes to women who smoked before pregnancy; a marginal RR depends on that population; (2) the joint
model estimates one covariate function for all three groups, whereas `primary_main` estimated it among
women who smoked before pregnancy only (7 percent of the pooled records). Flag thresholds for review:
overall RR 0.02, overall RD 2 per 1,000, crossover age 1.0 year. Larger movement is reported and
discussed before use, not hidden. The crossover age itself does not depend on the standardization
population (the sign of the standardized RD at each age equals the sign of the conditional contrast).

## Pre-specified validation against the existing pair

The joint model is adopted as the source of the main three-group results only if, for each
contrast, it agrees with the corresponding two-arm fit within:

| Quantity | Tolerance |
|---|---|
| Overall adjusted RR | 0.003 |
| Overall RD per 1,000 | 0.3 |
| Age-specific standardized risk, every age, every group | 0.2 per 1,000 |
| Crossover age | 0.2 years |
| Simultaneous-reversal classification | identical |

The expected differences are in the third decimal: NN is 93 percent of the fitted records, and the
two designs differ only in whether the covariate function is estimated once across three groups or
once per pair. If any tolerance is exceeded, the discrepancy is reported in the verification log and
investigated before either design is used; the joint model is not adopted silently. If all are met,
the agreement is recorded as the validation of the new code path and the comparison table is deposited
with the outputs.

## Consequences if validated

Figure 2 row 1, the three-group block of Table 2, the SS vs SN and SS vs NN rows of Table 3, and the
abstract read from the joint model. Supplementary Methods describe the joint model as the main
analysis and the two common-reference two-arm fits as the confirmatory check, with the agreement
table. The two-arm fits stay in the supplementary specification tables.

**Outputs:** `age45_revision/outputs/supplementary/joint_3group/` (its own folder: the assembler scans `outputs/supplementary/models/` for the 31 supplementary fits, and this model is not one of them) with `model.rds`, the fit
receipt (columnwise SHA-256 of the fit data and identity keys, formula, knots, code manifest), and one
subfolder per contrast (`ss_vs_nn`, `sn_vs_nn`) in the engine's file layout (`HC0_age_standardized.csv`,
`HC0_overall_standardized.csv`, `HC0_crossover.rds`, `HC0_roots.csv`, the two null-age sets,
`age_arm_support.csv`), plus `validation_against_pair.csv`.

**No released result is touched.** `outputs/models`, `submission_staging` and the existing
`outputs/supplementary/models/*` are unchanged. The new fit is the only new estimate this addendum
authorises. No imputation; complete cases exactly as in every other model.
