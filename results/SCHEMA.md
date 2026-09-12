# What is in results/

These are the estimates the manuscript reports. Every file is an aggregate: a count, an estimate, a
curve or a scenario. No file has one row per birth.

`run_all.R` reads only this directory. The code that wrote it is in `../model-fitting/`.

Two notes on reading these files directly. Label columns such as `label` in `main_overall.csv` and
`display_label` in `crossover_labels.csv` carry the short internal name of each model, which names the
behaviour rather than the person. `relabel()` in `R/labels.R` maps those to the wording the paper uses,
and `check_people_first()` refuses to write any table that still carries the short form, so every
published label is people-first even though the stored one is not. `exhibit_checksums.csv` is not an
estimate: it holds one checksum per manuscript table so that `scripts/check_outputs_match.R` can prove
the rebuilt tables still match the published ones.

Conventions used throughout. `age` is maternal age in whole years from 15 to 45. `A` is the smoking
contrast, 0 for the reference group and 1 for the exposed group, defined for each comparison in
`METHODS.md`. `risk0` and `risk1` are standardized probabilities, and the columns ending `_per1000`
are the same quantities per 1,000 births. `rr` is a risk ratio and `rd` a risk difference, each with
`_lower` and `_upper` confidence limits. `covariance` is `HC0` for the sandwich estimator that the
paper reports and `model` for the model-based sensitivity analysis. `model_id` names a fitted
specification; `contrast` is one of `primary`, `broad` and `prepregnancy`, which are comparisons 1, 2
and 3 in the paper.

## main/ the three main comparisons and the specifications around them

| File | Rows | What it holds |
| --- | --- | --- |
| `main_overall.csv` | 3 | One row per comparison: eligible and complete-case counts, events, arm sizes, standardized risks, risk ratio and risk difference. |
| `main_age_estimates.csv` | 186 | Standardized risk, risk ratio and risk difference at each age from 15 to 45, for each comparison and each covariance. |
| `root_reference.csv` | 46 | Every fitted root of the smoking by age contrast: its age, the slope there, whether it is a regular crossing, and its local confidence limits. |
| `crossover_summary.csv` | 42 | One row per fitted specification: counts, the fitted null ages, the whole null-age confidence set, the ages where the simultaneous band supports each direction, and the omnibus test. |
| `crossover_labels.csv` | 21 | The display name of each specification. |
| `crossover_full_null_age_sets.csv` | 214 | The complete 95 percent null-age set of each model, as intervals. |
| `crossover_simultaneous_sign_regions.csv` | 190 | The age intervals where the simultaneous band lies entirely below or entirely above the null. |
| `interaction_curves.csv` | 186 | Age-specific estimates when smoking is also allowed to interact with body mass index and calendar year. |
| `interaction_age_all_models.csv` | 248 | The same, for each interaction specification separately. |
| `interaction_overall.csv` | 32 | Overall estimates for those specifications. |
| `source_specific_endpoints.csv` | 112 | Observed infant, neonatal and early neonatal mortality, and the exploratory fetal-source ratios. |
| `paired_GH_overall_differences.csv` | 16 | The change in the association when eligible fetal-death records are added to the same live-birth reference. |
| `crossover_simulation_summary.csv` | 56 | Coverage of the crossover procedure under eight known-truth scenarios, 1,000 replications each. |

## descriptive/ who was studied

| File | Rows | What it holds |
| --- | --- | --- |
| `common_clinical_flow_by_year.csv` | 144 | Records entering, retained and excluded at each eligibility stage, by year and gestational-age threshold. |
| `numeric_cc_exclusions_overall.csv` | 3 | Eligible, included and excluded counts per comparison, with the recorded outcome frequency in each. |
| `baseline_all_three_contrasts.csv` | 214 | Characteristics of both arms of all three comparisons, as counts and percentages or means and standard deviations. |
| `missingness_by_variable_year.csv` | 369 | Records whose value was unknown, by covariate field, comparison and year. |

## models/ per specification

Four files may exist for a specification: `_HC0_age_standardized.csv` is its curve across ages,
`_age_arm_support.csv` is the births and recorded events in each arm at each age, which is what makes
the crude risks in Figure 1 possible, `_HC0_overall_standardized.csv` is its overall estimate, and
`_counts.csv` is its fitted and reference record counts.

The specifications shipped here are the three main comparisons, the 2014 to 2015 historical
comparison, and the four adjustment and fitting-population sensitivities that Table S6b reports.

## sensitivity/ the 31 supplementary fits

| File | Rows | What it holds |
| --- | --- | --- |
| `supp_summary.csv` | 31 | One row per supplementary fit: counts, overall estimates, fitted crossover with its local interval, and the simultaneous sign regions. |
| `supp_age_estimates.csv` | 961 | The age curve of each supplementary fit. |
| `supp_roots.csv` | 31 | The fitted roots of each supplementary fit. |

## bias/ what a bias would have to do

These are fixed-assumption calculations on the main model outputs or on the crude age-specific tables.
They state what an unmeasured factor, a selection mechanism or a reporting error would have to do to
produce the observed pattern. They are not corrections and they do not estimate a bias.

| File | Rows | What it holds |
| --- | --- | --- |
| `S_evalue_by_age_all_contrasts.csv` | 96 | The minimum strength an unmeasured factor would need, at each age, for each comparison. |
| `S_confounding_benchmarks_primary.csv` | 1,152 | The same calculation over a grid of assumed factor strengths. |
| `S_selection_left_truncation_scenarios.csv` | 8,928 | The full grid for live-birth selection: assumed loss curve, smoking loss ratio, prevalence and risk ratio of a hidden hypertension-prone group, and the loss differential each scenario would require. |
| `S_selection_left_truncation_compact.csv` | 144 | The subset of that grid the supplement reports. |
| `S_exposure_misclassification_nondifferential.csv` | 1,116 | Crude risk ratios corrected for misreporting that does not depend on the outcome. |
| `S_exposure_misclassification_crossover.csv` | 36 | How far that correction moves the age at which the crude ratio crosses one. |
| `S_exposure_misclassification_differential_tipping.csv` | 93 | The reporting sensitivity ratio that would make the association null, by age. |
| `S_outcome_sensitivity_tipping_by_age.csv` | 96 | The outcome recording sensitivity ratio that would equalize the latent risks, by age and validation anchor. |
