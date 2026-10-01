# Current analysis methods

This specification describes the October 2026 joint-model presentation. The superseded
three-pairwise-model presentation is not the current main analysis.

## Population and outcome

US-resident singleton live births, maternal ages 15–45, known obstetric gestational age 20–47 weeks,
recorded absence of chronic hypertension and known combined gestational hypertension/preeclampsia
(GH/PE) status. Main years are 2016–2024, after national implementation of the revised certificate;
2014–2015 are historical sensitivity years. Counts are births, not unique women.

GH/PE is a combined recorded checkbox, not adjudicated preeclampsia. Severity and onset are unavailable.
Preterm birth is a comparison outcome, not a negative-control proof against bias.

## Smoking definitions and reference groups

Source cigarette fields cover the three months before pregnancy and each trimester. Codes 00–97
are counts, 98 is a positive top code, and 99 is unknown. Reporting flags are respected.
The primary joint model contains SS (smoking in both early periods), SN (before pregnancy but none
reported in the first trimester), and NN (none in either period). **SS is the reporting reference.**
The secondary model compares no prepregnancy smoking (N) with prepregnancy smoking (S), regardless
of the first trimester. **S is its reference.** Both models use complete cases without investigator
imputation. Source-edited fields may contain agency editing or imputation.

Later-trimester reports do not define main-model groups. Third-trimester exposure is not clinically
defined for births before 28 weeks, although the raw field may contain reported values. Later-report
sensitivity definitions can introduce gestational-duration selection and do not quantify bias.
The first-trimester-only sensitivity ignores the value of prepregnancy smoking but still requires
that field to be known in its prepared input.

## Models and standardized effects

The binary logistic joint model has a three-level smoking predictor and smoking-by-age spline
interactions. NN is the coding baseline, but contrasts are re-expressed against SS for reporting.
Other covariate coefficients are shared across the three groups. The secondary model has a binary
prepregnancy predictor with an age interaction. Natural cubic spline interior knots are 21, 27 and
35 years for age (boundaries 15,45), and 19.6,26.1,37.8 for BMI (boundaries 13,69.9).

Covariates are year, recorded race/ethnicity, BMI, education, prepregnancy diabetes, prior living
children, prior preterm birth, prior cesarean delivery and nativity. The separate within-group
SS-versus-SN sensitivity also adjusts for prepregnancy dose and estimates covariate coefficients
within that population. Its 31.8-year crossover is distinct from the joint-model 33.3-year estimate.

At each age, probabilities for every comparison group are averaged over the same empirical covariate
distribution for that model and age. Overall estimates use that model's full complete-case reference.
Risks and risk differences are expressed per 1,000; risk ratios are unitless. HC0 covariance is
primary and model-based covariance is a sensitivity. Uncertainty conditions on the empirical
reference and assumes independent records; repeated pregnancies cannot be clustered by mother.

## Formal interaction tests added in October

Two reduced logistic models omit only the smoking-by-age terms while retaining all main effects,
covariates, factor levels, spline bases and complete-case records. Likelihood-ratio statistics use
twice the maximized log-likelihood difference; verified rank differences are 8 and 4. Joint HC0
Wald tests examine the same coefficient blocks. Holm adjustment covers the two global hypotheses
separately for each test method. Three exploratory primary-model contrast Wald tests form a
separate Holm family. Log-tail probabilities are retained when ordinary probabilities underflow.

These additions followed coauthor review after the main findings were available, as stated in the
dated protocol amendments. They test conditional log-odds interaction, not directly constancy of
standardized RRs/RDs and not reversal by themselves.

## Crossovers and sensitivity analyses

For the age-only smoking interactions, the zero of the conditional contrast also gives standardized
RR=1 and RD=0. Local intervals use the implicit delta method and assume a regular root. Coefficient-
ellipsoid simultaneous bands test support for opposite directions across age within each contrast;
coverage is not simultaneous across all three contrasts. Table S19 retains complete pointwise
null-age confidence sets, including boundary uncertainty that is not another fitted crossover.

Additional smoking-by-BMI/year models use integer-age standardized contrasts and model-specific
Bonferroni intervals; their sign brackets are not continuous-root intervals. Other retained
sensitivities address age bases, covariate sets, periods, exposure definitions, dose, race/ethnicity,
fetal-death inclusion and observed linked infant death. The populations overlap and are not
independent replications. Mortality analyses do not eliminate live-birth selection or recover early
losses. Withdrawn quantitative-bias scenarios are excluded from the current publication driver.

## Reproduction boundary

`run_all.R` reconstructs publication displays from aggregate results. `model-fitting/run_current.R`
runs the statistical plan in an isolated workspace with verified prepared records. Original import
and preprocessing code is retained, but the source-only route also needs historical reconciliation
inputs and source dictionaries. The October extension and aggregate rebuild were executed; an
independent end-to-end raw-download rerun is not claimed.
