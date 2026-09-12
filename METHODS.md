# Methods

The operational definitions behind every estimate in `results/`. The manuscript states these in prose;
this file states them in the form the code implements, so a reader can check one against the other.

## Population

United States natality public-use files, 2016 to 2024. A birth record is eligible when all of the
following hold. Each is a recorded value, not an inference.

| Requirement | Rule |
| --- | --- |
| Residency | mother resident in the United States |
| Plurality | singleton live birth |
| Maternal age | 15 to 45 years |
| Gestational age | obstetric estimate of 20 to 47 weeks, known |
| Chronic hypertension | recorded absent, and not merely unreported |
| Outcome | gestational hypertension or preeclampsia status known |

2014 and 2015 are analysed separately as a historical sensitivity analysis and are not part of the main
period, because national adoption of the revised birth certificate was complete only in 2016.

The unit is a birth record, not a mother. Repeated pregnancies to the same woman cannot be linked in
these files, so the records are not clustered.

## Outcome

Gestational hypertension or preeclampsia, written GH/PE, is the combined birth-certificate checkbox. It
is not clinically adjudicated preeclampsia, and it carries no onset date and no severity. The control
outcome used to test specificity is preterm birth, an obstetric estimate below 37 weeks, on the same
records.

## Exposure

Four cigarette fields are recorded: average cigarettes per day in the three months before pregnancy
(P) and in each trimester (T1, T2, T3). Values 00 to 97 are stated counts, 98 is a positive top code
meaning 98 or more, and 99 is unknown. A recorded value is not used when its reporting flag says the
item was not supported that year.

**The exposure rule.** A group whose association with GH/PE is estimated is defined only by the P and
T1 fields. GH/PE begins at or after 20 weeks, the third-trimester field does not exist for a birth
before 28 weeks, and GH/PE shortens gestation, so the later fields both follow the outcome and depend
on how long the pregnancy lasted. They are used in three ways only: to narrow a reference group, with a
missing value counted as not positive so that no group requires the pregnancy to reach a given week; to
remove inconsistent records; or in analyses whose stated purpose is to show the bias the later fields
introduce.

The three comparisons, written as the required pattern of P/T1/T2/T3 where `+` is smoking, `-` is zero
cigarettes and `any` is unrestricted:

| Comparison | Exposed | Reference | Population |
| --- | --- | --- | --- |
| 1 | `+/+/any/any` | `+/-/any/any` | women who smoked before pregnancy with known T1 |
| 2 | `+/+/any/any` | `-/-/any/any` | P and T1 both known |
| 3 | `+/any/any/any` | `-/any/any/any` | P known |

A zero report does not identify a woman who has never smoked, and a zero first-trimester report does
not establish that she stopped for good.

## Model

A logistic probability model for the recorded outcome:

    logit Pr(Y = 1 | A, age, X) = g(age, X) + A h(age)

with `A` the binary smoking contrast and `h` a natural cubic spline, so smoking interacts with age and
with nothing else. Age spline: interior knots 21, 27 and 35, boundaries 15 and 45. Prepregnancy body
mass index spline: interior knots 19.6, 26.1 and 37.8, boundaries 13 and 69.9. The knots were taken
from empirical quantiles without using outcome status and are then held fixed, so the reported
uncertainty does not include knot selection.

Adjustment: year, maternal race and ethnicity as a recorded social variable, prepregnancy diabetes,
prior living children, education, prior preterm birth, prior cesarean delivery, and nativity. The
comparison-1 model additionally adjusts for prepregnancy cigarettes per day.

Complete cases for each model's own covariates. No imputation, by an explicit recorded decision.

## Standardization

At each integer age from 15 to 45, predicted probabilities under both smoking categories are averaged
over the complete-case covariate distribution at that age. Overall estimates average over the whole
complete-case population. Reported quantities are standardized risks, risk ratios and risk differences
per 1,000 records. HC0 sandwich covariance is primary; model-based covariance is a sensitivity
analysis. Both condition on the empirical reference distribution and assume independent records.

## Crossover age

Because the logistic link is increasing and `h` has no covariate interaction, the sign of the
within-covariate risk contrast is the sign of `h(age)`. The standardized risk ratio therefore crosses
one exactly where `h(age) = 0`, and that root is the crossover age.

The root is found by exact polynomial root isolation on each piece of the spline, not by searching
integer ages. Its local 95% confidence interval is the implicit delta interval, the root plus or minus
1.96 times `sqrt(b(a)' V b(a)) / |h'(a)|` evaluated at the root with HC0 covariance. That interval
assumes an isolated root with non-zero slope. It is not a confidence set for every null age and it does
not establish that the root is unique.

Whether the direction genuinely reverses is a separate question, answered by a simultaneous confidence
band built from the coefficient ellipsoid with the critical value from the chi-square distribution on
the smoking spline dimension. Ages where that band lies entirely below one and ages where it lies
entirely above one are the ages at which a lower and a higher recorded risk are supported. A band that
touches the null is not an additional crossover.

## What the estimates are

Associations in recorded data. Birth-certificate records cannot separate a biological mechanism from
differences in smoking history, in which pregnancies reach a registered birth, or in how the outcome is
recorded. The bias analyses in `results/bias/` state what each of those would have to do to produce the
observed pattern; they are fixed-assumption calculations, not corrections, and they do not exclude a
smaller true association combined with ordinary pregnancy loss.
