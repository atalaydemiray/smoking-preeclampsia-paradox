# Data and aggregate dictionary

No record-level datasets or fitted record-level objects are distributed. CSV inputs are UTF-8
with a header row; publication layouts and reference tables are display matrices whose first row
contains column labels. Counts describe births or source records, not unique women. Public CSVs
contain grouped counts, fitted summaries or validation diagnostics, not one row per birth.

## Record-level definitions for authorized refitting

The full source coding and annual reporting gates are implemented in the bundled source adapters;
this summary does not replace the annual NCHS manuals. See METHODS.md for eligibility and adjustment.

| Field/group | Meaning and units |
| --- | --- |
| `source_type`, `year`, `source_row` | Joint record key within prepared source files; not a maternal identifier. Repeated pregnancies cannot be clustered by mother. |
| `Y` | Binary recorded GH/PE outcome, 0 absent and 1 present. Separate severity or onset is unavailable. Other endpoints use their own explicitly identified preparation. |
| `age` | Maternal age in completed years; main fitted domain 15–45 inclusive. |
| `bmi` | Prepregnancy BMI, kg/m²; explicit source boundaries 13–69.9. |
| `SS`, `SN`, `NN` | Smoking before pregnancy and in trimester 1; before pregnancy but none in trimester 1; none in either period. SS is the primary reporting reference. |
| `S`, `N` | Any versus no prepregnancy smoking, irrespective of trimester 1; S is the secondary reporting reference. |
| `A`, `SN` in the joint design | `A=1` identifies SS; `SN=1` identifies SN; both zero identifies the NN coding baseline. Coding baseline and reporting reference differ. |
| `year_factor` | Calendar year, modeled categorically; main years 2016–2024. |
| `race_ethnicity` | Source-edited race/ethnicity categories; not a biological classification or a claim of complete unedited self-report. |
| `education4` | Less than high school; high school/GED; some college/associate; bachelor's or higher. |
| `prior_living4` | 0, 1, 2, or at least 3 previous living children; not total parity. |
| `prepreg_diabetes`, `prior_preterm`, `prior_cesarean` | Recorded prepregnancy diabetes, previous preterm birth, and previous cesarean; known binary covariates. |
| `nativity` | Born in the 50 US states versus elsewhere, including territories. |
| Cigarette source counts | 00–97 are counts; 98 is a positive top code; 99 is unknown. Reporting flags govern whether a value is usable. |

No investigator imputation is performed. Missing covariates lead to complete-case exclusion.
Source editing may already include agency imputation. Earlier losses are unavailable. A raw
trimester-3 entry does not make trimester-3 exposure clinically defined before 28 weeks.

## Main aggregate schemas

Paths below are relative to `publication/Code/` unless stated otherwise. A key identifies a row
within the named file, not across different model releases.

| Input | Row key/grain | Main fields |
| --- | --- | --- |
| `supplementary/aggregate_outputs/joint_three_group/summary_main_summary.csv` | One row per `model_id` | Fitted/reference N, risks, RR/RD, local crossover CI, null-age sets and sign regions. |
| Same folder, `summary_main_age_estimates.csv` | `model_id`, `age` | Same-age standardization N and age-specific risks/RRs/RDs with pointwise intervals. |
| Same folder, `age_group_support.csv` | `age`, `group` | Observed `n` and `events` for SS/SN/NN. |
| `supplementary/aggregate_outputs/main_models/prepregnancy_main_age_arm_support.csv` | `age`, `A` | Observed counts for secondary-model arms. |
| `interaction_tests/population_overall.csv` | `population` | Exposure-eligible N, complete-case N, covariate exclusions and total fitted events. |
| `interaction_tests/missingness_overall.csv` | `population`, `variable` | `unknown_n`, `eligible_n`, `unknown_percent`; all variables use the original exposure-eligible denominator. Missingness overlaps. |
| `interaction_tests/global_interaction_tests.csv` | `population` | LRT and HC0 Wald statistics/df, ordinary and log-tail P values, and two-hypothesis Holm adjustments. |
| `interaction_tests/interaction_tests.csv` | `population`, `test`, `hypothesis` | Global and exploratory contrast tests, P displays and multiplicity scope. |
| `interaction_tests/{joint,prepregnancy}/interaction_tests.csv` | `test`, `hypothesis` | Full/reduced log-likelihoods and parameter counts for independent LRT/df checks. |
| Same population folders, `validation.csv` | `check` | Observed absolute difference, declared tolerance and pass indicator. |
| `table_inputs/table1_by_outcome_long.csv` | `column`, `variable`, `level` | Outcome-stratified descriptive cells for the secondary complete-case population. |
| `table_inputs/covariance/*.csv` | `measure` within comparison/estimator file | Model-based and HC0 overall estimates. Preserve file-specific orientation; the table builder applies the documented inversions. |
| `../results/main/root_reference.csv` | Model/covariance/root within file | Full-precision retained sensitivity roots and local uncertainty. |
| `../results/main/crossover_summary.csv` | Model and covariance | Retained sensitivity confidence sets and simultaneous sign inference. |
| `../results/main/crossover_simulation_summary.csv` | `scenario`, `metric` | Attempts, applicable denominators, rates and Wilson Monte Carlo intervals. Synthetic evidence, not clinical observations. |

Joint summary `events` counts events in the contrasted pair, not necessarily all three fitted
groups. Use `interaction_tests/population_overall.csv` for full-model events. Historical identifiers
`primary`, `broad`, `prepregnancy` in retained sensitivities are not interchangeable with the
current joint-model contrasts. Source-specific mortality ratios are not automatically population
mortality risk ratios; see the table notes and METHODS.md.

## Effect scales and missing values

| Field/convention | Interpretation |
| --- | --- |
| `risk0`, `risk1` | Standardized probabilities for the file's reference and comparator. |
| `_per1000` | Risk or risk difference multiplied by 1,000; never a ratio multiplied by 1,000. |
| `rr` | Comparator risk divided by reference risk; unitless. |
| `rd` | Comparator risk minus reference risk. |
| `_lower`, `_upper` | Interval bounds for the specified covariance estimator and inference method. |
| `target_n`, `reference_n`, `fit_n` | Standardization-target size, reference size and fitted population size; they need not be identical. |
| `crossover_age`, `delta_lower`, `delta_upper` | Root and local implicit-delta interval, in years. |
| `full_95_fixed_null_age_set` | Pointwise-inversion set; components joined by `U`. Not a whole-set simultaneous confidence guarantee. |
| `simultaneous_strict_negative`, `simultaneous_strict_positive` | Supported lower/higher comparator risk across age within the named contrast. |
| Parentheses / square brackets | Excluded / included interval endpoints. |
| `HC0`, `model` | Sandwich covariance / inverse-information covariance. |
| `*_log_p` | Natural log of a tail probability. Finite values retain information when ordinary P values underflow to zero. Publish the `<.001` display, never `P=0`. |
| `NA` / empty numeric cell | Missing or unavailable numerical result, not a zero. |
| `Empty` or `none` confidence set | No ages in that computed set; not missing data. Display wording is generated by the table formatter. |
| `Not established` | The specified inference did not establish the claimed pattern; not evidence of absence. |
| `Not applicable` | The metric is undefined for that scenario; distinct from a zero rate. |
| `0`, `TRUE`, `FALSE` | Genuine zero count or explicit logical value, not a suppression token. |

Table 3 reports only the null-age component containing the main fitted crossover. The additional
older-age component for SN versus SS is retained in Table S19. No fitted ages or birth records
were removed by that display decision.
