# Supplementary analysis amendment, 11 September 2026

Recorded before any of the fits below were run. All analyses here are post-results
supplementary analyses added after the age-15-to-45 main results were known. They are
labelled that way in Methods, in the wording the manuscript already uses for the
smoking-by-BMI and calendar-year models. None changes the three main comparisons, their
populations, covariates, knots, standardization or inference. Released outputs under
`age45_revision/outputs/models` and the 36 validated receipts are not touched; everything
new writes to `age45_revision/outputs/supplementary/` and
`age45_revision/derived/supplementary_inputs/`.

Owner decisions that motivated this amendment (10 to 11 September): keep the three main
comparisons; add the exposure-definition work proposed in
`EXPOSURE_DEFINITION_SUPPLEMENT_PROPOSAL_2026-09-10.md`; make the supplement answer the
epidemiological threats a reviewer will raise (live-birth selection, exposure misreporting,
the chronic-hypertension exclusion, outcome specificity, dose, race/ethnicity), and report
the quantitative bias analysis that was already computed but never tabulated.

## Rule for exposure definitions

A group used to estimate an association with recorded GH/PE may be defined only by the
before-pregnancy and first-trimester fields. Second- and third-trimester fields are used
only (a) with missing treated as not positive, so that no group requires reaching a given
week, (b) to remove inconsistent records, or (c) in analyses whose stated purpose is to show
the bias. Sizing on the 2020 clinical input (all clinically eligible records with known
GH/PE, n = 3,398,303): among women who smoked before pregnancy, the third-trimester field
was unknown for 78.2% of births at 20 to 27 weeks and 0.2% at term; births to women who
"stopped in the third trimester" were preterm 19.0% of the time versus 12.7% for women who
smoked throughout. The 2018 NCHS edit to the third-trimester field must be read before any
third-trimester value is interpreted.

## Analyses (identifiers are the model folder names)

Population sets, prepared once per year from the validated clinical inputs with the same
covariate construction as the main models (`prepare_sep_model_data`):

- `prepregnancy_ext`: the prepregnancy-only population (before-pregnancy field known,
  GH/PE known, clinical eligibility) with added flags: first-, second- and third-trimester
  status, before-pregnancy dose category, any later positive report, all four fields known.
- `within_ext`: the population of women who smoked before pregnancy (smoked before pregnancy, first trimester
  known) with the same flags plus first-trimester dose category and dose change.

Fits (all natality_main covariates, same age spline, HC0, age-specific empirical
standardization, complete case):

| Group | Model ids | Population | Exposed vs reference | Reference for standardization |
|---|---|---|---|---|
| S-A three-level early smoking | supp_3lvl_stopped_vs_none, supp_3lvl_continued_vs_none | prepregnancy_ext, T1 known, T1-only smoking dropped | stopped by T1 vs none; continued vs none | all three groups together |
| S-B first-trimester dose | supp_t1dose_1_5, _6_10, _11_20, _21plus | within_ext | each dose category vs 0 cigarettes in T1 | all within_ext complete cases |
| S-B dose change | supp_t1change_reduced, supp_t1change_same_or_more | within_ext with known before-pregnancy dose | reduced (0 < T1 < P) vs stopped; same or more vs stopped | all |
| S-B before-pregnancy dose | supp_pdose_1_5, _6_10, _11_20, _21plus | prepregnancy_ext | each category vs 0 | all prepregnancy_ext complete cases |
| S-C strict stopping | supp_strict_continued_vs_stopped, supp_strict_relapse_vs_stopped | within_ext | continued vs stopped with no later positive report; T1 zero then later positive vs strict stopped | all within_ext |
| S-D clean reference | supp_clean_broad, supp_clean_prepregnancy | prepregnancy_ext | M2 and M3 with the reference restricted to zero in every recorded window | fit rows |
| S-E bias illustration | supp_t3_through_vs_none, supp_timing_stopped_by_T1, supp_timing_stopped_in_T2, supp_timing_stopped_in_T3 | prepregnancy_ext with all four fields known; within_ext | positive in all four windows vs zero in all four; each stopping time vs smoked throughout | fit rows; all within_ext |
| S-F control outcome | supp_preterm_primary, supp_preterm_broad, supp_preterm_prepregnancy | existing main model inputs | same three contrasts, outcome preterm birth (obstetric estimate < 37 weeks) | fit rows |
| S-G race and ethnicity | supp_race_primary_{nhw,nhb,hisp,other}, supp_race_broad_{nhw,nhb,hisp,other} | existing main model inputs, one stratum each | M1 and M2 within stratum, race removed from covariates | stratum rows |
| S-H quantitative bias | aggregate only | main model outputs | E-values and confounding benchmarks for all three contrasts by age; outcome misclassification tipping; live-birth selection bound and left-truncation model; exposure misclassification scenarios | not applicable |
| S-I chronic hypertension | not run | requires re-import of excluded records | M1 to M3 with chronic hypertension retained and adjusted | owner decision (disk, time) |

Preterm birth as a control outcome uses the same populations as the GH/PE models
(GH/PE status known) so that any age pattern is compared on identical records.

## Anchors for the bias analyses

- Miscarriage by maternal age: Magnus MC et al., BMJ 2019;364:l869 (421,201 Norwegian
  pregnancies, 2009 to 2013): risk lowest at 25 to 29 years (10%), rising rapidly after 30
  to 53% at 45 and over. Intermediate ages are handled as scenarios, not as quoted values.
- Smoking and miscarriage: Pineles BL, Park E, Samet JM, Am J Epidemiol 2014;179(7):807-823:
  pooled RR 1.23 (95% CI 1.16 to 1.30) for any active smoking, about 1% per cigarette per day.
- Birth-certificate smoking ascertainment: PRAMS Working Group (Am J Public Health 1998):
  70.6% to 82.0% of smoking captured on certificates versus 86.2% to 90.3% on confidential
  questionnaires. Used as sensitivity scenarios 0.7, 0.8, 0.9.
- Outcome validation anchors: unchanged from `config/outcome_validation_candidates.json`.

## What this amendment does not do

No imputation. No change to eligibility. No claim that any supplementary result identifies a
causal effect. Numbers from these fits enter the manuscript only after the final refit and
the verification log (`PRE_SUBMISSION_NUMERICAL_VERIFICATION_LOG.md`) are complete.
