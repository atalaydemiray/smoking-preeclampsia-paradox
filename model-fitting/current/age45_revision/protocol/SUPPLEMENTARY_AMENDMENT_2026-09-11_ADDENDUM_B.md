# Supplementary analysis amendment, addendum B — 11 September 2026, 21:02 CEST

Recorded before any fit it authorises. At the time of writing, the resume driver launched at
20:33:56 is inside stage 1 (`21_fit_supplementary.R large`); it has completed one fit,
`supp_3lvl_stopped_vs_none`. The chronic-hypertension stages authorised below are stages 2
and 3 of that driver and have not started: `grep -c "26 chtn prepare START"` over
`outputs/supplementary/RUN_LOG_resume_20260911_203356.txt` returns 0.

This addendum amends `SUPPLEMENTARY_AMENDMENT_2026-09-11.md`. It does not replace it. It
changes nothing about the three main comparisons, their populations, covariates, knots,
standardization or inference, and touches nothing under `age45_revision/outputs/models`.

## 1. S-I chronic hypertension: authorised

The parent amendment's S-I row reads `| S-I chronic hypertension | not run | requires
re-import of excluded records | M1 to M3 with chronic hypertension retained and adjusted |
owner decision (disk, time) |`. Counting model ids across the parent amendment's table gives
31 authorised fits (S-A 2, S-B 4 + 2 + 4, S-C 2, S-D 2, S-E 4, S-F 3, S-G 8), none of them a
chronic-hypertension fit.

`21_fit_supplementary.R:190-209` nevertheless defines three, and
`26_prepare_chronic_hypertension.R:1` cites its authority as "S-I" — the one row that
declines to grant it. The owner decision the parent row was waiting on was taken on 11
September and is recorded in `PRE_SUBMISSION_NUMERICAL_VERIFICATION_LOG.md`
("Chronic-hypertension sensitivity … approved 11 Sep; import complete for all nine years"),
and the re-import it was conditional on is complete for 2016 through 2024. The parent
amendment was simply never updated to match, so the fits would otherwise have run
unauthorised, against the hard rule "a protocol amendment is dated before any fit it
authorises".

Authorised here, prospectively:

| Group | Model ids | Population | Exposed vs reference | Reference for standardization |
|---|---|---|---|---|
| S-I chronic hypertension | supp_chtn_primary, supp_chtn_broad, supp_chtn_prepregnancy | the three main model populations with eligibility opened from `chtn %in% "N"` to `chtn %in% c("N", "Y")`, prepared by `26_prepare_chronic_hypertension.R` | the same three contrasts as M1, M2, M3, with recorded chronic hypertension retained and adjusted | fit rows |

Eligibility is opened by a single documented token substitution on the project's own frozen
`prepare_sep_model_data` source (`26_prepare_chronic_hypertension.R:101-107`), so the
covariate construction is identical to the main models everywhere else. Records whose chronic
hypertension status is unknown remain excluded; only explicit "yes" records are added to the
explicit "no" records already in the main populations. This is a sensitivity analysis of an
eligibility criterion, not a new exposure definition, and it is subject to the same exposure
rule as everything else: the contrasts are defined by the before-pregnancy and
first-trimester fields only.

Authorised total after this addendum: **34 fits** (31 in the parent amendment, 3 here). The
"16 of 34" in the project notes and the verification log refers to this total. The parent amendment's
31 was correct for the parent amendment and is superseded.

## 2. S-C: `supp_strict_relapse_vs_stopped` is reclassified to S-E, bias illustration

The parent amendment files `supp_strict_relapse_vs_stopped` under **S-C strict stopping**
(line 52), alongside `supp_strict_continued_vs_stopped`. That classification is wrong and is
corrected here.

The exposed arm is `relapse <- id$t1_smoking %in% FALSE & id$later_positive`
(`21_fit_supplementary.R:111`), and `later_positive` is
`(t2$smoking %in% TRUE) | (t3$smoking %in% TRUE)` (`20_prepare_supplementary_inputs.R:33`).
Membership in the exposed arm therefore **requires a positive second- or third-trimester
report**. This is not use (a) of the exposure rule. Under (a), a missing later field counts
as not positive so that no group requires reaching a given week; here a missing later field
does not merely fail to place a woman in the exposed arm, it places her in the **reference**
arm (strict stopped). Because the third-trimester field is unknown for 78.2% of births at 20
to 27 weeks and 0.2% at term — the parent amendment's own sizing, lines 26 to 28 — the
reference arm is systematically enriched with early deliveries, which carry early-onset
GH/PE. The comparison is therefore built on post-onset information in the direction of the
observed result, RR 0.821 (0.782, 0.862).

The fit stays in the supplement. What changes:

- It is a **bias illustration (S-E)**, not a strict-stopping estimate (S-C).
- It moves from Table S21 to Table S22 in `25_supplementary_tables.py`. Table S21's footnote
  ("Missing later-trimester fields count as not positive, so no group requires the pregnancy
  to reach a given week") is true of the other rows in that table and false of this one; an
  affirmative misstatement in a supplementary table is worse than an unlabelled estimate.
- The manuscript may not read it mechanistically. The current wording — "the signature of
  continuing smoking misreported at the first trimester"
  (`SUPPLEMENTARY_REVISION_TEXT_2026-09-11.md:142-144`) — gives a causal reading to a
  quantity with a sufficient structural explanation, and is replaced.

No refit is required: the estimate is unchanged, only its classification, placement and
interpretation.

## 3. S-E `supp_timing_*`: the comparator, not only the exposed arm, conditions on reaching 28 weeks

The three `supp_timing_*` fits are already classified as bias illustrations, so this is a
correction to the **stated mechanism**, not to their authorisation.

Their shared comparator is `timing == "continued_through"`, defined as
`t1$smoking %in% TRUE & t2$smoking %in% TRUE & t3$smoking %in% TRUE`
(`20_prepare_supplementary_inputs.R:42`). `%in% TRUE` is FALSE for NA, so a missing
third-trimester value disqualifies a woman from the comparator. That comparator is the A = 0
arm of all three fits and is numerically identical in all three (A0 = 1,169,300). The exposed
arms are built with missing counted as not positive and so retain very preterm births.

The reference arm is therefore depleted of exactly the early deliveries that carry
early-onset GH/PE, while every exposed arm keeps them. This pushes all three risk ratios
above 1 mechanically, independent of any effect of smoking or of its timing — and the three
estimates are in fact near-identical, 1.101, 1.118 and 1.098, across three very different
"stopping times", which is what a shared structural artefact looks like and is not what a
timing gradient looks like.

Table S22's footnote currently attributes the reaching-third-trimester conditioning only to
the all-four-windows rows, which tells the reader the three timing rows are free of it. That
qualifier is removed. The Supplementary Methods text is corrected to state that the
comparator is unobtainable for a birth before 28 weeks.

## 4. The main-text Methods sentence on third-trimester definitions is corrected

`SUPPLEMENTARY_REVISION_TEXT_2026-09-11.md:32-33` states: "Definitions that use the
third-trimester field are shown only to illustrate the bias they introduce." This is false as
written. `supp_strict_continued_vs_stopped`, `supp_clean_prepregnancy` and `supp_clean_broad`
all reach the third-trimester field through `later_positive`
(`21_fit_supplementary.R:111, 154`) and all three are reported as ordinary estimates; the
first is quoted in Results as confirming comparison 1.

Those three uses are legitimate under (a): the later-trimester fields only **narrow a
reference** and missing counts as not positive, so no group requires reaching a given week.
The sentence is replaced by an accurate one that distinguishes narrowing a reference from
defining an exposed group, rather than by a claim the fits do not support.

## What this addendum does not do

No imputation. No change to eligibility in the three main comparisons. No refit of any
completed model. No claim that any supplementary result identifies a causal effect. Numbers
from the S-I fits enter the manuscript only after the verification log records them.
