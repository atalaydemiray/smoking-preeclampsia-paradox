# Supplementary analysis amendment, addendum C — 12 September 2026, 13:05 CEST

Recorded after the event it documents, because the event was a failure discovered in the data,
not a fit being authorised. This addendum amends addendum B. It withdraws the three fits that
addendum B section 1 authorised and changes nothing else.

## 1. S-I chronic hypertension: withdrawn

Addendum B authorised `supp_chtn_primary`, `supp_chtn_broad` and `supp_chtn_prepregnancy`: the
three main contrasts with eligibility opened from chronic hypertension explicitly "No" to
explicitly "No" or "Yes", and chronic hypertension entered as a covariate.

The first of these, `supp_chtn_primary`, was run six times between 03:xx and 12:28 CEST on
12 September by the automatic supervisor (`RUN_SUPPLEMENTARY_WATCHDOG.command`, log
`outputs/supplementary/WATCHDOG_20260912_012851.txt`). Every attempt ended identically:

    Error in fit_streaming_logit(d, spec$formula, "Y", "A", "age", spec$age_spec, :
      Logistic IRLS did not converge under all explicit tolerances.

This is not a memory or runtime failure. Read directly from the nine prepared input files
(`derived/chronic_hypertension/<year>_primary_chtn.rds`, 2016 to 2024), the outcome is
distributed as follows:

| chronic_hypertension | records | recorded GH/PE events | event rate |
|---|---|---|---|
| Yes | 70,549 | 0 | 0.0% |
| No | 2,044,364 | 174,591 | 8.5% |

Not one of the 70,549 births with chronic hypertension explicitly recorded also carries a
recorded GH/PE diagnosis, in any year. The covariate perfectly predicts the outcome to be
zero (complete separation). The maximum-likelihood estimate of its coefficient does not exist,
so no IRLS setting can make the model converge; the failure is deterministic and would recur
on `supp_chtn_broad` and `supp_chtn_prepregnancy`, which share the outcome and the covariate.

The most likely reason is the structure of the birth-certificate hypertension field, on which
chronic hypertension and gestational hypertension or preeclampsia appear to be recorded as
mutually exclusive categories rather than co-occurring conditions. That is a property of the
source data, not of the analysis. It should have been checked before addendum B was written.

## 2. Owner decision, 12 September 2026

Presented with three options (report the separation as a descriptive finding; fit a penalised
estimator for these three models only; drop the analysis), the owner chose to **drop the
chronic-hypertension sensitivity analysis from the supplement entirely**: no fitted model and
no descriptive table. The manuscript therefore reports 31 supplementary fits, not 34, and the
supplementary text, tables (S23, S24) and package builder have been changed to match.

## 3. What this does and does not change

- `21_fit_supplementary.R` is not edited. Its `chtn` branch remains as code but is no longer
  invoked by `RUN_SUPPLEMENTARY_PERFIT.command`, which now gates on 31 receipts. Editing 21
  would change the hash carried in every receipt's signature and force a refit of all 31
  verified models for no analytical reason.
- The re-imported chronic-hypertension inputs under `derived/chronic_hypertension/` are left in
  place as provenance. They are not read by any reported analysis.
- Nothing else in addendum B changes. Sections 2, 3 and 4 (relapse-fit reclassification, the
  timing-comparator footnote, the Methods sentence) stand.
- The main comparisons' exclusion of women with recorded chronic hypertension is unchanged and
  is still stated as a limitation in the manuscript.
