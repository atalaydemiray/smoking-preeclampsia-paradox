# Retained supplementary aggregates

These files are the inputs used by the current publication tables for within-group, model-
specification, mortality and simulation sensitivity analyses. No file contains one row per birth.
Current main joint-model estimates and formal interaction tests are in `../publication/Code/`.

- `descriptive/common_clinical_flow_by_year.csv`: sequential eligibility counts for Table S1.
- `main/main_age_estimates.csv`: pairwise age curves, including the within-group sensitivity in
  Table S4d. These are not the current joint-model main estimates.
- `main/root_reference.csv`, `crossover_summary.csv`, `crossover_labels.csv`: roots, uncertainty,
  sign regions and model labels for retained specification comparisons.
- `main/interaction_age_all_models.csv`, `interaction_overall.csv`: additional smoking-by-BMI
  and calendar-year interactions.
- `main/paired_GH_overall_differences.csv`: paired changes after eligible fetal-death inclusion.
- `main/source_specific_endpoints.csv`: source-specific mortality comparisons; fetal-source
  ratios are not population fetal-mortality risk ratios.
- `main/crossover_simulation_summary.csv`: eight known-truth scenarios, 1,000 attempts each.
- `models/`: counts and HC0 overall estimates for the five adjustment/reference-population rows.

`risk0` and `risk1` are standardized probabilities, `rr` is their ratio and `rd` their difference.
Columns ending `_per1000` use the per-1,000 scale. `HC0` denotes sandwich covariance and `model`
denotes model-based covariance. The `primary`, `broad` and `prepregnancy` identifiers describe
specific pairwise sensitivities; use the current table labels and `METHODS.md` when interpreting them.

`run_all.R` verifies every reconstructed table against the reference files under
`publication/reference_tables/`.
